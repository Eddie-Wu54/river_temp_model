#' This script is used to compare simple regression models with flow. There are
#' three different model configurations that could include flow:
#' 
#' 1. Simple lag5 + flow
#' 2. Simple lag5 + relative flow (for previous day)
#' 3. Simple lag5 + flow lag5
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
  library(zoo)
  library(lubridate)
}


## Functions
cal.rmse <- function(out.list, fold) {
  
  # data frame to store the results
  r <- matrix(NA, nrow=fold, ncol=length(unique(out.list[[1]]$location)))
  colnames(r) <- unique(out.list[[1]]$location)
  
  # 10 iterations
  for (i in 1:fold){
    compare <- out.list[[i]]
    
    # calculate rmse
    for (loc in unique(compare$location)) {
      compare.now <- subset(compare, location == loc) %>% na.omit()
      r[i,loc] <- rmse(compare.now$obs, compare.now$preds)
    }
  }
  return(r)
}
cal.nsc <- function(out.list, fold) {
  
  # dataframe to store the results
  r <- matrix(NA, nrow=fold, ncol=length(unique(out.list[[1]]$location)))
  colnames(r) <- unique(out.list[[1]]$location)
  
  # 10 iterations
  for (i in 1:fold){
    compare <- out.list[[i]]
    
    # calculate nsc
    for (loc in unique(compare$location)) {
      compare.now <- subset(compare, location == loc) %>% na.omit()
      nsc <- 1 - sum((compare.now$obs - compare.now$preds)^2) / sum((compare.now$obs - mean(compare.now$obs))^2)
      r[i,loc] <- nsc
    }
  }
  return(r)
}




#### Data import and cleaning ####

### Data import
airtemp <- read.csv("tributary air temperatures clean.csv")
watertemp <- read.csv("tributary water temperature/water_temperature_d.csv")
flow <- read.csv("tributary discharge.csv")

# Convert to date format
airtemp$date <- as.Date(airtemp$date, format = "%m/%d/%Y")
watertemp$date <- as.Date(watertemp$date, format = "%m/%d/%Y")
flow$date <- as.Date(flow$date, format = "%m/%d/%Y")


table(airtemp$station_name)
table(watertemp$station_name)
table(watertemp$location)
table(flow$location)


## Calculate the MEAN airT for US locations
for (i in which(is.na(airtemp$mean_temp))) {
  airtemp$mean_temp <- (airtemp$max_temp + airtemp$min_temp) / 2
}


### Check imputed values
# make sure that no initial NA values in watertemp
which(is.na(watertemp$location))

# Need to calculate the rolling mean for each location separately...
watertemp$location <- as.factor(watertemp$location)
unique_locations <- unique(watertemp$location)

for(loc in unique_locations) {
  sub <- watertemp[watertemp$location == loc,]
  sub$rolling_mean <- rollmean(sub$temp, k = 7, fill = NA, align = "right")
  watertemp[watertemp$location == loc, "rolling_mean"] <- sub$rolling_mean
}

# Calculate variance
watertemp$variance <- (watertemp$temp - watertemp$rolling_mean)^2
# Assign NA when variance is very small
watertemp$temp[which(watertemp$variance < 1e-10)] <- NA


### Combine into one master dataframe
water <- left_join(watertemp, flow, by = c("location","date"))
master.temp <- left_join(airtemp, water, by = c("station_name", "date")) %>% 
  select(location, country, station_name, date, year, month = month.x,
         day = day.x, airT = mean_temp, waterT = temp, flow) %>% 
  na.omit() %>% 
  arrange(location, date) # arrange by location and date



### Get the time lag air temperature and flow
master.temp <- master.temp %>%
  group_by(location) %>%
  mutate(airT.lag1 = lag(airT, 1),
         airT.lag2 = lag(airT, 2),
         airT.lag3 = lag(airT, 3),
         airT.lag4 = lag(airT, 4),
         airT.lag5 = lag(airT, 5)) %>% 
  na.omit()

master.temp <- master.temp %>% 
  group_by(location) %>% 
  mutate(flow.lag1 = lag(flow, 1),
         flow.lag2 = lag(flow, 2),
         flow.lag3 = lag(flow, 3),
         flow.lag4 = lag(flow, 4),
         flow.lag5 = lag(flow, 5)) %>% 
  na.omit()


### Get relative flow and cumulative flow
master.temp <- master.temp %>% 
  mutate(rqc = (flow - flow.lag1)/flow)

master.temp <- master.temp %>% 
  mutate(cumflow = flow.lag1 + flow.lag2 + flow.lag3)


# Change the location into factors
master.temp$country <- as.factor(master.temp$country)
master.temp$location <- as.factor(master.temp$location)
table(master.temp$location)




#### Get training and testing data ####

## Subsetting
df_by_location <- split(master.temp, master.temp$location)
df_by_location <- df_by_location[sapply(df_by_location, function(x) !is.null(x) && nrow(x) > 0)]

fold = 10 #run 10 folds
combined_training_list <- vector("list",fold)
combined_testing_list <- vector("list",fold)

## Loop
for (f in 1:fold) {
  
  for (loc in 1:length(df_by_location)) {
    
    # Subset the current location data, and get train/test
    current <- df_by_location[[loc]]
    current_training <- current[current$year == sample(current$year, 1),]
    current_testing <- current[current$year == sample(current$year, 1),]
    
    # Add to the combined list
    combined_training_list[[f]] <- rbind(
      combined_training_list[[f]], current_training)
    combined_testing_list[[f]] <- rbind(
      combined_testing_list[[f]], current_testing)
  }
  
  combined_training_list[[f]]$year <- as.factor(combined_training_list[[f]]$year)
  combined_testing_list[[f]]$year <- as.factor(combined_testing_list[[f]]$year)
}




#### Simple lag5 linear ####
## Forms
form1 <- waterT ~ airT.lag1 + airT.lag2 + airT.lag3 + airT.lag4 + airT.lag5 + (1|location)
results.lag5 <- vector("list", fold)


## Iteration starts here
for (i in 1:fold){
  
  compare <- NA
  # select current dataset, and all unique location levels
  current.training <- combined_training_list[[i]]
  current.testing <- combined_testing_list[[i]]
  loc.levels <- unique(current.training$location)
  
  # model training and predicting
  model <- lmer(form1, data = current.training)
  preds <- predict(model, newdata = current.testing, re.form=(~1|location))
  
  p <- as.data.frame(preds)
  
  # calculate RMSE
  compare <- cbind(current.testing, preds = p$preds) %>% 
    select(location, date, obs = waterT, preds)
  
  results.lag5[[i]] <- compare
}


## Get the results
rmse.lag5 <- cal.rmse(results.lag5, fold)
colMeans(rmse.lag5)

nsc.lag5 <- cal.nsc(results.lag5, fold)
colMeans(nsc.lag5)




#### Simple lag5 linear with flow ####
## Forms
form2 <- waterT ~ airT.lag1 + airT.lag2 + airT.lag3 + airT.lag4 + airT.lag5 + flow + (1|location)
results.flow <- vector("list", fold)


## Iteration starts here
for (i in 1:fold){
  
  compare <- NA
  # select current dataset, and all unique location levels
  current.training <- combined_training_list[[i]]
  current.testing <- combined_testing_list[[i]]
  loc.levels <- unique(current.training$location)
  
  # model training and predicting
  model <- lmer(form2, data = current.training)
  preds <- predict(model, newdata = current.testing, re.form=(~1|location))
  
  p <- as.data.frame(preds)
  
  # calculate RMSE
  compare <- cbind(current.testing, preds = p$preds) %>% 
    select(location, date, obs = waterT, preds)
  
  results.flow[[i]] <- compare
}


## Get the results
rmse.flow <- cal.rmse(results.flow, fold)
colMeans(rmse.flow)

nsc.flow <- cal.nsc(results.flow, fold)
colMeans(nsc.flow)




#### Simple lag5 linear with lag5 flow ####
## Forms
form3 <- waterT ~ airT.lag1 + airT.lag2 + airT.lag3 + airT.lag4 + airT.lag5 +
  flow.lag1 + flow.lag2 + flow.lag3 + flow.lag4 + flow.lag5 + (1|location)
results.5flow <- vector("list", fold)


## Iteration starts here
for (i in 1:fold){
  
  compare <- NA
  # select current dataset, and all unique location levels
  current.training <- combined_training_list[[i]]
  current.testing <- combined_testing_list[[i]]
  loc.levels <- unique(current.training$location)
  
  # model training and predicting
  model <- lmer(form3, data = current.training)
  preds <- predict(model, newdata = current.testing, re.form=(~1|location))
  
  p <- as.data.frame(preds)
  
  # calculate RMSE
  compare <- cbind(current.testing, preds = p$preds) %>% 
    select(location, date, obs = waterT, preds)
  
  results.5flow[[i]] <- compare
}


## Get the results
rmse.5flow <- cal.rmse(results.5flow, fold)
colMeans(rmse.5flow)

nsc.5flow <- cal.nsc(results.5flow, fold)
colMeans(nsc.5flow)



#### Simple lag5 linear with relative flow ####
## Forms
form4 <- waterT ~ airT.lag1 + airT.lag2 + airT.lag3 + airT.lag4 + airT.lag5 +
  cumflow + (1|location)
results.rflow <- vector("list", fold)


## Iteration starts here
for (i in 1:fold){
  
  compare <- NA
  # select current dataset, and all unique location levels
  current.training <- combined_training_list[[i]]
  current.testing <- combined_testing_list[[i]]
  loc.levels <- unique(current.training$location)
  
  # model training and predicting
  model <- lmer(form4, data = current.training)
  preds <- predict(model, newdata = current.testing, re.form=(~1|location))
  
  p <- as.data.frame(preds)
  
  # calculate RMSE
  compare <- cbind(current.testing, preds = p$preds) %>% 
    select(location, date, obs = waterT, preds)
  
  results.rflow[[i]] <- compare
}


## Get the results
rmse.rflow <- cal.rmse(results.rflow, fold)
colMeans(rmse.rflow)

nsc.rflow <- cal.nsc(results.rflow, fold)
colMeans(nsc.rflow)




#### Simple linear no lag with flow? ####
## Forms
form5 <- waterT ~ airT + flow + (1|location)
results.sf <- vector("list", fold)


## Iteration starts here
for (i in 1:fold){
  
  compare <- NA
  # select current dataset, and all unique location levels
  current.training <- combined_training_list[[i]]
  current.testing <- combined_testing_list[[i]]
  loc.levels <- unique(current.training$location)
  
  # model training and predicting
  model <- lmer(form5, data = current.training)
  preds <- predict(model, newdata = current.testing, re.form=(~1|location))
  
  p <- as.data.frame(preds)
  
  # calculate RMSE
  compare <- cbind(current.testing, preds = p$preds) %>% 
    select(location, date, obs = waterT, preds)
  
  results.sf[[i]] <- compare
}


## Get the results
rmse.sf <- cal.rmse(results.sf, fold)
colMeans(rmse.sf)

nsc.sf <- cal.nsc(results.sf, fold)
colMeans(nsc.sf)












