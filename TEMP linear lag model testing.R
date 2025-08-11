#' This script is used to test linear mixed effected models with different
#' time lags, to determine which one is the most appropriate to use.




library(tidyverse)
library(nlme)
library(flextable)
library(officer)




#### Evaluation functions ####
rmse_m <- function(obs, pred) {sqrt(mean((obs - pred)^2, na.rm = TRUE))}
aic_m <- function(obs, pred, k) {
  MSE = mean((obs - pred)^2)
  length(obs) * log(MSE) + 2 * k
}




#### Import data ####
master.subtemp <- read.csv("subsampled final combined temperature.csv")
master.subtemp <- master.subtemp %>% dplyr::select(2:14)

master.subtemp$date <- as.Date(master.subtemp$date)
master.subtemp$location <- as.factor(master.subtemp$location)

loc_seq=levels(master.subtemp$location) # get location sequence




#### Linear regression ####

## Models
ctrl = lmeControl(opt='optim')
form1 <- water ~ air + dmean_1
form2 <- water ~ air + dmean_1 + dmean_2
form3 <- water ~ air + dmean_1 + dmean_2 + dmean_3
form4 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4
form5 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5
form6 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + dmean_6


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
    
    # Fit models
    models <- list(
      gls(form1, control = ctrl, na.action = na.omit, data = train, correlation = corAR1()),
      gls(form2, control = ctrl, na.action = na.omit, data = train, correlation = corAR1()),
      gls(form3, control = ctrl, na.action = na.omit, data = train, correlation = corAR1()),
      gls(form4, control = ctrl, na.action = na.omit, data = train, correlation = corAR1()),
      gls(form5, control = ctrl, na.action = na.omit, data = train, correlation = corAR1()),
      gls(form6, control = ctrl, na.action = na.omit, data = train, correlation = corAR1())
    )
    
    # Loop through the 6 models
    for (m in 1:6) {
      
      model <- models[[m]]
      k_para <- length(coef(model))
      
      # Predict on test set
      test$preds <- predict(model, newdata = test)
      
      # Aggregate to weekly means
      test_weekly <- test %>%
        group_by(week(date)) %>%
        summarise(obs = mean(water, na.rm = TRUE),
                  preds = mean(preds, na.rm = TRUE),.groups = "drop")

      # Compute metrics
      rmse_val <- rmse_m(test$water, test$preds)
      aic_val <- aic_m(test$water, test$preds, k_para)
      
      # Store in list
      spec_row <- data.frame(
        location = loc, fold = i, test_year = test_year, model_id = m,
        formula = paste(deparse(formula(model)), collapse = ""),
        rmse = rmse_val, aic = aic_val
      )
      
      # Append to location-specific df
      loc_results <- rbind(loc_results, spec_row)
    }
  }
  # Store each location result as a list element
  results_list[[loc]] <- loc_results
}


## Final combined df
final_results_df <- do.call(rbind, results_list)




#### Results and Export ####
## Extract metrics
summary_rmse <- final_results_df %>% 
  group_by(location, model_id) %>% 
  summarise(mean_rmse = round(mean(rmse, na.rm = TRUE),2), .group = "drop") %>% 
  pivot_wider(names_from = model_id,
              values_from = mean_rmse,
              names_prefix = "Model_") %>% 
  dplyr::select(location, Model_1:Model_6)

colnames(summary_rmse) <- c("Location","Lag1","Lag2","Lag3","Lag4","Lag5","Lag6")


summary_aic <- final_results_df %>% 
  group_by(location, model_id) %>% 
  summarise(mean_aic = round(mean(aic, na.rm = TRUE),2), .group = "drop") %>% 
  pivot_wider(names_from = model_id,
              values_from = mean_aic,
              names_prefix = "Model_") %>% 
  dplyr::select(location, Model_1:Model_6)

colnames(summary_aic) <- c("Location","Lag1","Lag2","Lag3","Lag4","Lag5","Lag6")


## Table results
# Make a flextable object
ft.rmse <- flextable(summary_rmse)
ft.rmse <- autofit(ft.rmse)

ft.aic <- flextable(summary_aic)
ft.aic <- autofit(ft.aic)

# Read out
read_docx() %>%
  body_add_par("A first table") %>%
  body_add_flextable(ft.rmse) %>%
  body_add_par("A second table") %>% 
  body_add_flextable(ft.aic) %>%
  print(target = "linear_model_summary_table.docx")

