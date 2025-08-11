#' This script is used to train, test, and compare our models.
#' 
#' 1. Three simple statistical models.
#' 2. The air2stream model.
#' 2. The global futureStreams model. (predictions directly extracted)




library(tidyverse)
library(nlme)
library(drc) #for nonlinear starting parameter
library(deSolve)
library(pso)




#### Evaluation functions ####
rmse_m <- function(obs, pred) {sqrt(mean((obs - pred)^2, na.rm = TRUE))}
bias_m <- function(obs, pred) {mean(pred - obs, na.rm = TRUE)}
nsc_m <- function(obs, pred) {
  numerator = sum((obs - pred)^2, na.rm = TRUE)
  denominator = sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  1 - numerator / denominator
}
aic_m <- function(obs, pred, k) {
  MSE = mean((obs - pred)^2)
  length(obs) * log(MSE) + 2 * k
}




#### Import data ####
## Master temp
master.subtemp <- read.csv("subsampled final combined temperature.csv")
master.subtemp <- master.subtemp %>% dplyr::select(2:14)

master.subtemp$date <- as.Date(master.subtemp$date)
master.subtemp$location <- as.factor(master.subtemp$location)
master.subtemp$week <- week(master.subtemp$date)

loc_seq=levels(master.subtemp$location) # get location sequence
northern_sites <- c("salmontrout", "stlouis")
southern_sites <- setdiff(loc_seq, northern_sites)


## Subsampled years
sub.years <- read.csv("subsampled_years.csv")


## FutureStreams modeled temp
futureS <- read.csv("futureS modeled water temperature.csv")
futureS.sub <- futureS %>%
  semi_join(sub.years, by = c("location", "year")) %>% #get 5 years for each loc
  filter((location %in% northern_sites & week >= 18 & week <= 29) |
         (location %in% southern_sites & week >= 14 & week <= 29)) #760 in total




#### Linear lag6 regression ####
form.linear <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + dmean_6
ctrl = lmeControl(opt='optim')
results_list <- list()


## Loops starts
for (loc in loc_seq) { # loop through locations
  
  master.loc <- master.subtemp %>% filter(location == loc)
  years <- sort(unique(master.loc$year))
  
  # one df per location
  loc_results <- data.frame()
  
  for (i in 1:5) { # loop through folds
    test_year <- years[i]
    train_years <- years[-i]
    
    train <- master.loc %>% filter(year %in% train_years)
    test <- master.loc %>% filter(year == test_year)
    
    # --- Fit ---
    model.ar <- gls(form.linear, control = ctrl, na.action = na.omit,
                    data = train, correlation=corAR1())
    k_para <- length(coef(model.ar))
    
    # --- Predict ---
    test$preds <- predict(model.ar, newdata = test)
    
    # --- Aggregate daily obs and preds to weekly means ---
    test_weekly <- test %>%
      group_by(week) %>%
      summarise(obs = mean(water, na.rm = TRUE),
                preds = mean(preds, na.rm = TRUE),.groups = "drop")

    # --- Compute metrics ---
    rmse_val = rmse_m(test_weekly$obs, test_weekly$preds)
    bias_val = bias_m(test_weekly$obs, test_weekly$preds)
    nsc_val = nsc_m(test_weekly$obs, test_weekly$preds)
    aic_val = aic_m(test_weekly$obs, test_weekly$preds, k_para)
    
    # Store in list
    spec_row <- data.frame(
      location = loc, fold = i, test_year = test_year,
      rmse = rmse_val, bias = bias_val, nsc = nsc_val, aic = aic_val
    )
    
    # Append to location-specific df
    loc_results <- rbind(loc_results, spec_row)
  }
  
  results_list[[loc]] <- loc_results
}


# Combine all locations into one big df
linear_results <- bind_rows(results_list)




#### Nonlinear regression ####
ctrl = lmeControl(opt='optim')
results_list <- list()


## Loops starts
for (loc in loc_seq) { # loop through locations
  
  master.loc <- master.subtemp %>% filter(location == loc)
  years <- sort(unique(master.loc$year))
  
  # one df per location
  loc_results <- data.frame()
  
  for (i in 1:5) { # loop through folds
    test_year <- years[i]
    train_years <- years[-i]
    
    train <- master.loc %>% filter(year %in% train_years)
    test <- master.loc %>% filter(year == test_year)
    
    # coefs
    modelco <- drm(water ~ air, fct = L.3(), data = train)
    co = c(alpha=as.numeric(coef(modelco)[2]),
           beta=as.numeric(coef(modelco)[3]),
           gamma=as.numeric(-coef(modelco)[1]))
    
    # --- Fit ---
    model.new <- gnls(water ~ alpha / (1 + exp(gamma * (beta - cair))),
                      data = train, na.action = na.omit,
                      start = co,
                      correlation = corAR1(),
                      control=gnlsControl(nlsTol=10, maxIter=1000))
    k_para <- length(coef(model.new))
    
    # --- Predict ---
    test$preds <- predict(model.new, newdata = test)
    
    # --- Aggregate daily obs and preds to weekly means ---
    test_weekly <- test %>%
      group_by(week) %>%
      summarise(obs = mean(water, na.rm = TRUE),
                preds = mean(preds, na.rm = TRUE),.groups = "drop")
    
    # --- Compute metrics ---
    rmse_val = rmse_m(test_weekly$obs, test_weekly$preds)
    bias_val = bias_m(test_weekly$obs, test_weekly$preds)
    nsc_val = nsc_m(test_weekly$obs, test_weekly$preds)
    aic_val = aic_m(test_weekly$obs, test_weekly$preds, k_para)
    
    # Store in list
    spec_row <- data.frame(
      location = loc, fold = i, test_year = test_year,
      rmse = rmse_val, bias = bias_val, nsc = nsc_val, aic = aic_val
    )
    
    # Append to location-specific df
    loc_results <- rbind(loc_results, spec_row)
  }
  
  results_list[[loc]] <- loc_results
}


# Combine all locations into one big df
nonlinear_results <- bind_rows(results_list)




#### Seasonal residual model ####
ctrl <- glsControl(opt = 'optim')
results_list <- list()

for (loc in loc_seq) {
  
  master.loc <- master.subtemp %>% filter(location == loc)
  years <- sort(unique(master.loc$year))
  
  loc_results <- data.frame()
  
  for (i in 1:5) {
    test_year <- years[i]
    train_years <- years[-i]
    
    train <- master.loc %>% filter(year %in% train_years)
    test <- master.loc %>% filter(year == test_year)
    
    # --- Fit ---
    # fit seasonal annual model on training air temperature
    annual.comp <- nls(air ~ a + b * sin(2 * pi / 365 * (yday(date) + t0)),
                       start = list(a = 0.05, b = 5, t0 = -26),
                       data = train)
    
    # calculate air residuals for training data
    train_res <- train %>% 
      mutate(res_t = residuals(annual.comp)) %>%
      arrange(date) %>%
      mutate(res_t1 = lag(res_t, 1), res_t2 = lag(res_t, 2))
    
    # calculate water residuals for training data (seasonal component)
    water.seasonal <- nls(water ~ a + b * sin(2 * pi / 365 * (yday(date) + t0)),
                          start = list(a = 0.05, b = 5, t0 = -26),
                          data = train)
    train_res$res_w <- residuals(water.seasonal)
    
    # fit residual AR(1) model on water residuals
    residual.comp.ar <- gls(res_w ~ res_t + res_t1 + res_t2,
                            correlation = corAR1(), data = train_res,
                            control = ctrl, na.action = na.omit)
    
    # --- predict ---
    # predict annual air component on test data
    preds_annual <- predict(annual.comp, newdata = test)
    
    # calculate residuals on test air data
    test_res <- test %>%
      mutate(pred_air = preds_annual, res_t = air - pred_air) %>%
      arrange(date) %>%
      mutate(res_t1 = lag(res_t, 1), res_t2 = lag(res_t, 2))
    
    # predict residual component on test data
    preds_residuals <- predict(residual.comp.ar, newdata = test_res, na.action = na.omit)
    
    # combine predictions and residuals, align lengths by removing NA rows
    valid_idx <- which(!is.na(test_res$res_t1) & !is.na(test_res$res_t2))
    preds_combined <- preds_annual[valid_idx] + preds_residuals
    test$preds_combined[valid_idx] <- preds_combined
    
    # --- Aggregate daily obs and preds to weekly means ---
    test_weekly <- test %>%
      filter(!is.na(preds_combined)) %>%      # remove rows with NA predictions
      group_by(week) %>%
      summarise(obs = mean(water, na.rm = TRUE),
                preds = mean(preds_combined, na.rm = TRUE),
                .groups = "drop")
    
    # --- Compute metrics ---
    rmse_val = rmse_m(test_weekly$obs, test_weekly$preds)
    bias_val = bias_m(test_weekly$obs, test_weekly$preds)
    nsc_val = nsc_m(test_weekly$obs, test_weekly$preds)
    aic_val = aic_m(test_weekly$obs, test_weekly$preds, 9)
    
    # Store in list
    spec_row <- data.frame(
      location = loc, fold = i, test_year = test_year,
      rmse = rmse_val, bias = bias_val, nsc = nsc_val, aic = aic_val
    )
    
    # Append to location-specific df
    loc_results <- rbind(loc_results, spec_row)
  }
  
  results_list[[loc]] <- loc_results
}


# Combine all location results
seasonal_results <- bind_rows(results_list)









#### FutureStreams model ####
futureS.merged <- master.subtemp %>% 
  group_by(location, year, week) %>% 
  summarise(obs = mean(water, na.rm = TRUE), .groups = "drop") %>% 
  left_join(futureS.sub, by = c("location", "year","week"))


results_list <- list()


## Loops starts
for (loc in loc_seq) { # loop through locations
  
  master.loc <- futureS.merged %>% filter(location == loc)
  years <- sort(unique(master.loc$year))
  
  # one df per location
  loc_results <- data.frame()
  
  # Compute metrics
  spec_row <- master.loc %>% 
    group_by(year) %>% 
    summarise(rmse = rmse_m(obs, preds.futureS), bias = bias_m(obs, preds.futureS),
              nsc = nsc_m(obs, preds.futureS), aic = aic_m(obs, preds.futureS, 0),
              .groups = "drop") %>% 
    mutate(location = loc) %>% 
    rename(test_year = year)
  
  results_list[[loc]] <- spec_row
}


# Combine all the rows
futureS_results <- bind_rows(results_list) %>% 
  left_join(linear_results %>% dplyr::select(location, test_year, fold),
            by = c("location", "test_year"))




#### Air2Stream model ####
# --- Air2Stream ODE Function (5-parameter version) ---
air2stream_model <- function(t, state, parms, Ta, doy) {
  with(as.list(c(state, parms)), {
    Ta_t <- Ta[t]
    doy_t <- doy[t]
    ty <- 365
    dTw <- a1 + a2 * Ta_t - a3 * Tw + a6 * cos(2 * pi * doy_t / ty - a7)
    list(c(dTw))
  })
}


# --- Calibration Function: Returns RMSE ---
calibrate_air2stream_rmse <- function(params, Ta, Tw_obs, doy) {
  state <- c(Tw = Tw_obs[1])
  times <- 1:length(Ta)
  
  out <- tryCatch({
    ode(y = state, times = times, func = air2stream_model,
        parms = list(a1=params[1], a2=params[2], a3=params[3],
                     a6=params[4], a7=params[5]),
        method = "rk4", Ta = Ta, doy = doy)
  }, error = function(e) return(NULL))
  
  if (is.null(out)) return(Inf)
  
  Tw_sim <- out[, "Tw"]
  return(rmse_m(Tw_obs, Tw_sim))
}



# --- Plot the best RMSE convergence for a specific location and fold ---
plot_best_convergence <- function(loc, fold) {
  key <- paste(loc, fold, sep = "_")
  if (!is.null(best_rmse_list[[key]])) {
    df <- data.frame(iteration = 1:length(best_rmse_list[[key]]),
                     best_rmse = best_rmse_list[[key]])
    ggplot(df, aes(x = iteration, y = best_rmse)) +
      geom_line(color = "blue") +
      ggtitle(paste("Best RMSE Convergence: Location", loc, "Fold", fold)) +
      xlab("Iteration") + ylab("Best RMSE (So Far)") +
      scale_y_log10() +
      theme_minimal()
  } else {
    message("No convergence data for ", key)
  }
}


results_list <- list()
best_params_list <- list()
best_rmse_list <- list()
predictions_list <- list()


## Loop starts
for (loc in loc_seq) {
  master.loc <- master.subtemp %>% filter(location == loc)
  years <- sort(unique(master.loc$year))
  loc_results <- data.frame()
  
  for (i in 1:5) {  # 5-fold cross-validation
    test_year <- years[i]
    train_years <- years[-i]
    
    train <- master.loc %>% filter(year %in% train_years) %>% arrange(date)
    test  <- master.loc %>% filter(year == test_year) %>% arrange(date)
    
    # Parameter Calibration with PSO
    trace_vec <- c()
    best_trace <- c()
    
    fn_with_trace <- function(p) {
      val <- calibrate_air2stream_rmse(p, train$air, train$water, train$julian)
      trace_vec <<- c(trace_vec, val)
      best_val <- if (length(trace_vec) == 1) val else min(val, min(trace_vec[-length(trace_vec)]))
      best_trace <<- c(best_trace, best_val)
      return(val)
    }
    
    pso_result <- psoptim(
      par = rep(0, 5),
      fn = fn_with_trace,
      lower = rep(-10, 5),
      upper = rep(10, 5),
      control = list(maxit = 200, s = 50, trace = 1)
    )
    
    best_params <- pso_result$par
    
    # Save best params and convergence trace
    best_params_list[[paste(loc, i, sep = "_")]] <- best_params
    best_rmse_list[[paste(loc, i, sep = "_")]] <- best_trace
    
    # Prediction on Test Set
    sim_out <- ode(y = c(Tw = test$water[1]), times = 1:nrow(test),
                   func = air2stream_model,
                   parms = list(a1=best_params[1], a2=best_params[2],
                                a3=best_params[3], a6=best_params[4],
                                a7=best_params[5]),
                   method = "rk4",
                   Ta = test$air, doy = test$julian)
    
    test$preds <- sim_out[, "Tw"]
    
    # Aggregate to Weekly Means
    test_weekly <- test %>%
      group_by(week) %>%
      summarise(location = loc, fold = i, test_year = unique(year),
                obs = mean(water, na.rm = TRUE),
                preds = mean(preds, na.rm = TRUE), .groups = "drop")
    predictions_list[[paste(loc, i, sep = "_")]] <- test_weekly
    
    # Compute metrics
    rmse_val = rmse_m(test_weekly$obs, test_weekly$preds)
    bias_val = bias_m(test_weekly$obs, test_weekly$preds)
    nsc_val = nsc_m(test_weekly$obs, test_weekly$preds)
    aic_val = aic_m(test_weekly$obs, test_weekly$preds, 5)
    
    # store results
    spec_row <- data.frame(
      location = loc, fold = i, test_year = test_year,
      rmse = rmse_val, bias = bias_val, nsc = nsc_val, aic = aic_val
    )
    
    loc_results <- rbind(loc_results, spec_row)
  }
  
  results_list[[loc]] <- loc_results
}


air2stream_results <- bind_rows(results_list)



### Check convergence ###
#' check whether the model would converge successfully on the data

pdf("air2stream model convergence check.pdf", width = 10, height = 6)

for (loc in loc_seq) {
  for (i in 1:5) {
    pp <- plot_best_convergence(loc,i)
    print(pp)
  }
}

dev.off()




#### Compare results ####
all_models <- bind_rows(
  linear_results %>% mutate(model = "linear"),
  nonlinear_results %>% mutate(model = "nonlinear"),
  seasonal_results %>% mutate(model = "seasonal"),
  futureS_results %>% mutate(model = "futureS"),
  air2stream_results %>% mutate(model = "air2stream")
)



### === RMSE ===
rmse_results <- all_models %>%
  dplyr::select(location, fold, test_year, model, rmse) %>%
  pivot_wider(names_from = model, values_from = rmse)

rmse_results %>%
  group_by(location) %>%
  summarise(linear = round(mean(linear, na.rm = TRUE),2),
            nonlinear = round(mean(nonlinear, na.rm = TRUE),2),
            seasonal = round(mean(seasonal, na.rm = TRUE),2),
            futureS = round(mean(futureS, na.rm = TRUE),2),
            air2stream = round(mean(air2stream, na.rm = TRUE),2))

# mean rmse
colMeans(rmse_results[,-c(1,2,3)])



### === BIAS ===
bias_results <- all_models %>%
  dplyr::select(location, fold, test_year, model, bias) %>%
  pivot_wider(names_from = model, values_from = bias)

bias_results %>%
  group_by(location) %>%
  summarise(linear = round(mean(linear, na.rm = TRUE),2),
            nonlinear = round(mean(nonlinear, na.rm = TRUE),2),
            seasonal = round(mean(seasonal, na.rm = TRUE),2),
            futureS = round(mean(futureS, na.rm = TRUE),2),
            air2stream = round(mean(air2stream, na.rm = TRUE),2))

# mean bias
colMeans(bias_results[,-c(1,2,3)])



### === NSC ===
nsc_results <- all_models %>%
  dplyr::select(location, fold, test_year, model, nsc) %>%
  pivot_wider(names_from = model, values_from = nsc)

nsc_results %>%
  group_by(location) %>%
  summarise(linear = round(mean(linear, na.rm = TRUE),2),
            nonlinear = round(mean(nonlinear, na.rm = TRUE),2),
            seasonal = round(mean(seasonal, na.rm = TRUE),2),
            futureS = round(mean(futureS, na.rm = TRUE),2),
            air2stream = round(mean(air2stream, na.rm = TRUE),2))

# mean bias
colMeans(nsc_results[,-c(1,2,3)])



### === AIC ===
aic_results <- all_models %>%
  dplyr::select(location, fold, test_year, model, aic) %>%
  pivot_wider(names_from = model, values_from = aic)

aic_results %>%
  group_by(location) %>%
  summarise(linear = round(mean(linear, na.rm = TRUE),2),
            nonlinear = round(mean(nonlinear, na.rm = TRUE),2),
            seasonal = round(mean(seasonal, na.rm = TRUE),2),
            futureS = round(mean(futureS, na.rm = TRUE),2),
            air2stream = round(mean(air2stream, na.rm = TRUE),2))

# mean aic
colMeans(aic_results[,-c(1,2,3)])



### === RMSE: large and small rivers ===
small <- c("bigcreek","humber","rifle","salmontrout","yellowr")
large <- c("stlouis","stjoseph","saginaw","allegheny","genesee")

xlarge <- rmse_results %>% 
  filter(location %in% large) %>% 
  group_by(location) %>% 
  summarise(linear = mean(linear, na.rm = TRUE),
            nonlinear = mean(nonlinear, na.rm = TRUE),
            seasonal = mean(seasonal, na.rm = TRUE),
            futureS = mean(futureS, na.rm = TRUE))

colMeans(xlarge[,-c(1,2,3)])


xsmall <- rmse_results %>% 
  filter(location %in% small) %>% 
  group_by(location) %>% 
  summarise(linear = mean(linear, na.rm = TRUE),
            nonlinear = mean(nonlinear, na.rm = TRUE),
            seasonal = mean(seasonal, na.rm = TRUE),
            futureS = mean(futureS, na.rm = TRUE))

colMeans(xsmall[,-c(1,2,3)])




#### Data for example plot ####
#' We use Allegheny River 2015, and St.Joseph River 2016 as an exampl.

### === Allegheny River 2015 === ###
test_year <- 2015
train_years <- c(2012,2013,2016,2017)

train <- master.subtemp %>% filter(location == "allegheny" & year %in% train_years)
test <- master.subtemp %>% filter(location == "allegheny" & year == test_year)

# coefs
modelco <- drm(water ~ air, fct = L.3(), data = train)
co = c(alpha=as.numeric(coef(modelco)[2]),
       beta=as.numeric(coef(modelco)[3]),
       gamma=as.numeric(-coef(modelco)[1]))

# --- Fit ---
ctrl = lmeControl(opt='optim')
model.new <- gnls(water ~ alpha / (1 + exp(gamma * (beta - cair))),
                  data = train, na.action = na.omit,
                  start = co,
                  correlation = corAR1(),
                  control=gnlsControl(nlsTol=10, maxIter=1000))

test$preds <- predict(model.new, newdata = test)

# --- Aggregate daily obs and preds to weekly means ---
test_allegheny <- test %>%
  group_by(week) %>%
  summarise(obs = mean(water, na.rm = TRUE),
            preds = mean(preds, na.rm = TRUE),.groups = "drop") %>% 
  mutate(location = "allegheny", year = test_year)

x <- futureS.merged %>% filter(location == "allegheny"& year == 2015)
test_allegheny <- cbind(test_allegheny, x[,"preds.futureS"])



### === St.Joseph River 2016 === ###
test_year <- 2016
train_years <- c(2018,2013,2019,2017)

train <- master.subtemp %>% filter(location == "stjoseph" & year %in% train_years)
test <- master.subtemp %>% filter(location == "stjoseph" & year == test_year)

# coefs
modelco <- drm(water ~ air, fct = L.3(), data = train)
co = c(alpha=as.numeric(coef(modelco)[2]),
       beta=as.numeric(coef(modelco)[3]),
       gamma=as.numeric(-coef(modelco)[1]))

# --- Fit ---
ctrl = lmeControl(opt='optim')
model.new <- gnls(water ~ alpha / (1 + exp(gamma * (beta - cair))),
                  data = train, na.action = na.omit,
                  start = co,
                  correlation = corAR1(),
                  control=gnlsControl(nlsTol=10, maxIter=1000))

test$preds <- predict(model.new, newdata = test)

# --- Aggregate daily obs and preds to weekly means ---
test_stjoesph <- test %>%
  group_by(week) %>%
  summarise(obs = mean(water, na.rm = TRUE),
            preds = mean(preds, na.rm = TRUE),.groups = "drop") %>% 
  mutate(location = "stjoseph", year = test_year)

x <- futureS.merged %>% filter(location == "stjoseph"& year == 2016)
test_stjoesph <- cbind(test_stjoesph, x[,"preds.futureS"])


## merge together
example.test <- rbind(test_allegheny, test_stjoesph)




#### Save results ####
write.csv(rmse_results, "RMSE for plot.csv")
write.csv(bias_results, "BIAS for plot.csv")
write.csv(nsc_results, "NSC for plot.csv")
write.csv(aic_results, "AIC for plot.csv")
write.csv(example.test, "data for example plot.csv")



