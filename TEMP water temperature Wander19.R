#' Use the Wander et al. 2019 model water temperature



#### Get spatial points ####
sp <- brick("futureStream/waterTemperature_monthly_1981-2014.nc", varname = "waterTemp")


#### Import our specific locations ####
## Import location latitudes and longitudes
loc <- read.csv("tributary locations.csv")
str(loc)

lat<-loc$latitude
lon<-loc$longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations.spatial <- SpatialPoints(coords, proj4string = sp@crs)


#### Extract water temperature data ####

new.weeklyWater <- extract(sp, locations.spatial)

new.M.weeklyWater <- new.weeklyWater - 273.15



### Compare the two temperature models

## Wanders: Select only 1986 - 2005 (240 months)
new.M.weeklyWater <- new.M.weeklyWater[,c(61:300)]

## Start the subseting from here
data <- as.data.frame(new.M.weeklyWater[2,])
colnames(data) <- "temps"

# Add the years
for (y in 1:20){ data$year[(12*y-11):(12*y)] <- 1985+y }
# Add the months
data$month <- c(1:12)


## Create a dataframe to store the results (for one location)
results <- as.data.frame(matrix(NA, 12, 7))
colnames(results) <- c("months","temp.avg","temp.me","max","min","lower.CI","upper.CI")

# Fill in the values
confidence_level <- 0.95

for (m in 1:12) { # m index what month it is
  subset <- data %>% filter(month == m)
  results[m,"months"] <- m
  results[m,"temp.avg"] <- sum(subset$temps)/20 # average temp
  results[m,"max"] <- max(subset$temps) # max temp
  results[m,"min"] <- min(subset$temps) # min temp
  
  standard_error <- sd(subset$temps) / sqrt(20) # se
  
  # calculate the margin of error
  results[m,"temp.me"] <- abs(qnorm((1 - confidence_level) / 2)) * standard_error
  # calculate the CIs
  results[m,"lower.CI"] <- results[m,"temp.avg"] - results[m,"temp.me"]
  results[m,"upper.CI"] <- results[m,"temp.avg"] + results[m,"temp.me"]
}


write.csv(results, "water temperature clean/saginaw_model_wanders.csv")




