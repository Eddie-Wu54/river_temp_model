#' This script is used to compare simple regression models with flow. There are
#' three different model configurations that could include flow:
#' 
#' 1. Simple lag5 + flow
#' 2. Simple lag5 + relative flow (for previous day)
#' 3. Simple lag5 + flow lag3
#' 
#' All three models are compared to the simple lag5 multiple linear regression
#' model without flow to see if including flow would improve model performance...
#' 
#' Also, only the mixed effect model is used for comparison...


{
  library(dplyr)
  library(tidyverse)
  library(lme4)
  library(nlme)
  library(xts)
  library(ModelMetrics)
  library(lmerTest)
  library(zoo)
  library(lubridate)
  library(plotrix) #for zooming in plots
}



#### Data importing and cleaning ####

## Data import
air <- read.csv("tributary air temperatures clean.csv", stringsAsFactors=F)
water <- read.csv("tributary water temperature/water_temperature_d.csv",
                  stringsAsFactors=F)
flow <- read.csv("tributary discharge.csv", stringsAsFactors=F)
avg15 <- read.csv("15-year average river discharge.csv", stringsAsFactors=F)

# Convert to date format
air$date <- as.Date(air$date, "%m/%d/%Y")
water$date <- as.Date(water$date, "%m/%d/%Y")
flow$date <- as.Date(flow$date, "%m/%d/%Y")


## Calculate the MEAN airT for US locations
for (i in which(is.na(air$mean_temp))) {
  air$mean_temp <- (air$max_temp + air$min_temp) / 2
}


## Combine water temperature and discharge
fl <- merge(flow, avg15, by = "location")
aw <- merge(air, water, by = c("station_name","date"))
awf <- merge(aw, fl, by = c("location","date"))
awf <- awf %>% arrange(location, date)

## Change into factor
awf$location <- as.factor(awf$location)
awf$station_name <- as.factor(awf$station_name)

# Get location sequence
loc_seq=levels(awf$location)


## Check if there are any duplicates
duplicates <- awf %>%
  group_by(location, date) %>%
  filter(n() > 1) # Duplicates should be NA...


## Print the result
table(awf$location)


# make sure that no initial NA values in awf water temperature
which(is.na(awf$temp))

# Need to calculate the rolling mean for each location separately...
for(loc in loc_seq) {
  sub <- awf[awf$location == loc,]
  sub$rolling_mean <- rollmean(sub$temp, k = 7, fill = NA, align = "right")
  awf[awf$location == loc, "rolling_mean"] <- sub$rolling_mean
}

# Calculate variance
awf$variance <- (awf$temp - awf$rolling_mean)^2

# Assign NA when variance is very small
awf$temp[which(awf$variance < 1e-10)] <- NA

# Check results - how many imputed values are in each location
awf %>%
  group_by(location) %>%
  summarise(na_count = sum(is.na(temp)))




#### Calculate different flow-related metrics

## Get the time lag day variables
awf <- awf %>%
  group_by(location) %>%
  mutate(dmean_1 = lag(mean_temp, 1),
         dmean_2 = lag(mean_temp, 2),
         dmean_3 = lag(mean_temp, 3),
         dmean_4 = lag(mean_temp, 4),
         dmean_5 = lag(mean_temp, 5),
         dflow_1 = lag(flow, 1),
         dflow_2 = lag(flow, 2),
         dflow_3 = lag(flow, 3),
         dflow_4 = lag(flow, 4),
         dflow_5 = lag(flow, 5))


## Get relative flow, cumulative and inverse flow
awf <- awf %>% 
  mutate(rqc = (flow - dflow_1)/flow)

awf <- awf %>% 
  mutate(cumflow = dflow_1 + dflow_2 + dflow_3)

awf <- awf %>% 
  mutate(inv = 1/flow)


## Get cumulative air temp for past five days
awf <- awf %>% 
  rowwise() %>% 
  mutate(cair = (mean_temp+dmean_1+dmean_2+dmean_3+dmean_4+dmean_5)/6)



## Get final master dataset
master.temp <- awf[complete.cases(cbind(awf$mean_temp,awf$temp)),] %>% 
  select(location, date, station_name, country,
         year, month = month.x, day = day.x,
         water = temp, air = mean_temp, dmean_1, dmean_2, dmean_3,
         dmean_4, dmean_5, flow, dflow_1, dflow_2, dflow_3, dflow_4,
         dflow_5, rqc, inv, cumflow, X15.year.average, cair)

master.temp <- master.temp[complete.cases(master.temp[,8:25]),]


#### Get training and testing ####

#' This step is the same as the other .Rmd file. Please run it from the
#' other scripts to get the training and testing data.



## Create a list to store all the models (for AIC comparison later)
model.list <- vector("list", 5)


#### Run 7 different models with different flow input ####
## Model forms
formnull <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5
form1 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + flow
form2 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + inv
form3 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + rqc
form4 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + cumflow
form5 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + dflow_1 + dflow_2 + dflow_3
form6 <- water ~ air + dmean_1 + dmean_2 + dmean_3 + dmean_4 + dmean_5 + X15.year.average


## Create list to store output preds
spring.r <- vector("list", fold)
summer.r <- vector("list", fold)
fall.r <- vector("list", fold)
winter.r <- vector("list", fold)
annual.r <- vector("list", fold)

grand.lag5 <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.flow <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.inv <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.rqc <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.cum <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.flowlag3 <- list(spring.r, summer.r, fall.r, winter.r, annual.r)
grand.15year <- list(spring.r, summer.r, fall.r, winter.r, annual.r)


## Model iteration starts here
#' Two loops: outer loop for season, inner loop for 10 replicates
for (season in 1:5) {
  
  ## Get current season and its correspnding training/testing
  train <- grand_training[[season]]
  test <- grand_testing[[season]]
  
  ## 10 fold iteration starts here
  for (i in 1:fold){
    
    compare <- NA
    # select current dataset, and all unique location levels
    current.training <- train[[i]]
    current.testing <- test[[i]]
    
    # model training
    model.null <- lme(fixed = formnull, random = ~1 | location,
                     correlation=corAR1(form=~1|location, value = 0.85, fixed=T),
                     na.action = na.omit, data = current.training)
    model.flow <- lme(fixed = form1, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.85, fixed=T),
                      na.action = na.omit, data = current.training)
    model.inv <- lme(fixed = form2, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.85, fixed=T),
                      na.action = na.omit, data = current.training)
    model.rqc <- lme(fixed = form3, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.8, fixed=T),
                      na.action = na.omit, data = current.training)
    model.cum <- lme(fixed = form4, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.8, fixed=T),
                      na.action = na.omit, data = current.training)
    model.flowlag3 <- lme(fixed = form5, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.8, fixed=T),
                      na.action = na.omit, data = current.training)
    model.15year <- lme(fixed = form6, random = ~1 | location,
                      correlation=corAR1(form=~1|location, value = 0.8, fixed=T),
                      na.action = na.omit, data = current.training)
    
    # model predictions
    preds.null <- as.data.frame(predict(model.null, newdata = current.testing, re.form=~(1|location)))
    preds.flow <- as.data.frame(predict(model.flow, newdata = current.testing, re.form=~(1|location)))
    preds.inv <- as.data.frame(predict(model.inv, newdata = current.testing, re.form=~(1|location)))
    preds.rqc <- as.data.frame(predict(model.rqc, newdata = current.testing, re.form=~(1|location)))
    preds.cum <- as.data.frame(predict(model.cum, newdata = current.testing, re.form=~(1|location)))
    preds.flowlag3 <- as.data.frame(predict(model.flowlag3, newdata = current.testing, re.form=~(1|location)))
    preds.15year <- as.data.frame(predict(model.15year, newdata = current.testing, re.form=~(1|location)))
    
    # combine into a dataframe
    grand.lag5[[season]][[i]] <- cbind(current.testing, preds = preds.null[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.flow[[season]][[i]] <- cbind(current.testing, preds = preds.flow[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.inv[[season]][[i]] <- cbind(current.testing, preds = preds.inv[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.rqc[[season]][[i]] <- cbind(current.testing, preds = preds.rqc[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.cum[[season]][[i]] <- cbind(current.testing, preds = preds.cum[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.flowlag3[[season]][[i]] <- cbind(current.testing, preds = preds.flowlag3[,1]) %>% 
      select(location, date, obs = water, preds)
    grand.15year[[season]][[i]] <- cbind(current.testing, preds = preds.15year[,1]) %>% 
      select(location, date, obs = water, preds)
  }
  
  # store models in a model dataframe
  model.list[[season]][["lag5"]] <- model.null
  model.list[[season]][["flow"]] <- model.flow
  model.list[[season]][["inv"]] <- model.inv
  model.list[[season]][["rqc"]] <- model.rqc
  model.list[[season]][["cum"]] <- model.cum
  model.list[[season]][["flowlag3"]] <- model.flowlag3
  model.list[[season]][["15year"]] <- model.15year
}





#### Compare model AIC separately (for each season) ####
aic.results <- vector("list", 5)

for (season in 1:5){
  aic.results[[season]] <- anova(model.list[[season]][["lag5"]],
                                 model.list[[season]][["flow"]],
                                 model.list[[season]][["inv"]],
                                 model.list[[season]][["rqc"]],
                                 model.list[[season]][["cum"]],
                                 model.list[[season]][["flowlag3"]],
                                 model.list[[season]][["15year"]])
  print(aic.results[[season]])
  }

# In general, the model with inverse flow has the lowest AIC,let's check the model summary
for (season in 1:5) {
  print(summary(model.list[[season]][["inv"]]))
}



#### Check model preformance (RMSE and NSC) ####

## RMSE
rmse.results <- vector("list", 5)

for (season in 1:5) {
  rmse.results[[season]][[1]] <- colMeans(cal.rmse(grand.lag5[[season]],10))
  rmse.results[[season]][[2]] <- colMeans(cal.rmse(grand.flow[[season]],10))
  rmse.results[[season]][[3]] <- colMeans(cal.rmse(grand.inv[[season]],10))
  rmse.results[[season]][[4]] <- colMeans(cal.rmse(grand.rqc[[season]],10))
  rmse.results[[season]][[5]] <- colMeans(cal.rmse(grand.cum[[season]],10))
  rmse.results[[season]][[6]] <- colMeans(cal.rmse(grand.flowlag3[[season]],10))
  rmse.results[[season]][[7]] <- colMeans(cal.rmse(grand.15year[[season]],10))
}


# Overall RMSE for each model
for (season in 1:5) {
  c <- data.frame(lag5 = rmse.results[[season]][[1]],
                  flow = rmse.results[[season]][[2]],
                  inv = rmse.results[[season]][[3]],
                  rqc = rmse.results[[season]][[4]],
                  cum = rmse.results[[season]][[5]],
                  flowlag3 = rmse.results[[season]][[6]],
                  year15 = rmse.results[[season]][[7]])
  print(colMeans(c))
}



## NSC
ncs.results <- vector("list", 5)

for (season in 1:5) {
  ncs.results[[season]][[1]] <- colMeans(cal.nsc(grand.lag5[[season]],10))
  ncs.results[[season]][[2]] <- colMeans(cal.nsc(grand.flow[[season]],10))
  ncs.results[[season]][[3]] <- colMeans(cal.nsc(grand.inv[[season]],10))
  ncs.results[[season]][[4]] <- colMeans(cal.nsc(grand.rqc[[season]],10))
  ncs.results[[season]][[5]] <- colMeans(cal.nsc(grand.cum[[season]],10))
  ncs.results[[season]][[6]] <- colMeans(cal.nsc(grand.flowlag3[[season]],10))
  ncs.results[[season]][[7]] <- colMeans(cal.nsc(grand.15year[[season]],10))
}


# Overall RMSE for each model
for (season in 1:5) {
  c <- data.frame(lag5 = ncs.results[[season]][[1]],
                  flow = ncs.results[[season]][[2]],
                  inv = ncs.results[[season]][[3]],
                  rqc = ncs.results[[season]][[4]],
                  cum = ncs.results[[season]][[5]],
                  flowlag3 = ncs.results[[season]][[6]],
                  year15 = ncs.results[[season]][[7]])
  print(colMeans(c))
}




#### Get correlation between flow and water temperature ####
cor.test(master.spring$inv, master.spring$waterT)
ggplot(aes(x=sqrt(inv), y=waterT), data=master.spring)+
  geom_point()



#### Plotting ####

season = 5

plot.df <- grand.lag5[[season]][[1]] %>% filter(location == "bigotter")
plot.df2 <- grand.inv[[season]][[1]] %>% filter(location == "bigotter")

ggplot(data=plot.df, aes(x=date))+
  geom_line(aes(x=date, y=obs), color = "black")+
  geom_line(aes(x=date, y=preds), color = "red")+
  geom_line(data = plot.df2, aes(x=date, y=preds), color = "blue")+
  labs(x="date", y="temperature (°C)")+
  theme_bw()

