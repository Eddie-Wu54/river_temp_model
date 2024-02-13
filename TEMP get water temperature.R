#' This script:
#' 
#' 1. Extracts the average weekly water temperature data (for 27 years) from the
#' global water temperature model for all 12 GREAT LAKES tributary locations.
#' 
#' 2. Calculate the average throughout the Years.
#' 
#' 3. Export the weekly average water temperature for each location separately.


library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(dplyr)


#### Get spatial points ####
sp <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")


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
wt1 <- brick("futureStream/E2O_hist_1979_1985.nc", varname = "waterTemperature")
wt2 <- brick("futureStream/E2O_hist_1986_1995.nc", varname = "waterTemperature")
wt3 <- brick("futureStream/E2O_hist_1996_2005.nc", varname = "waterTemperature")

# Include all the temperature data in one data frame
weeklyWater <- extract(wt2, locations.spatial)
weeklyWater <- cbind(weeklyWater, extract(wt2, locations.spatial))
weeklyWater <- cbind(weeklyWater, extract(wt3, locations.spatial))

# Change from Kelvin to Celsius
M.weeklyWater <- weeklyWater - 273.15 # should have 1040 columns of data



#### Get the average week temperature throughout the year ####

## Create two data frames to store the average water temperature data, 
# standard deviation, max, and min for each week of the year (1979-2005)
week.avg <- matrix(NA, 14, 52)
week.me <- matrix(NA, 14, 52)
week.max <- matrix(NA, 14, 52)
week.min <- matrix(NA, 14, 52)
confidence_level <- 0.95

# Use two for loops to calculate the average for each week over 20 years
for (i in 1:52) { # i index which week
  
  # sub setting that week for each year
  subset <- data.frame(M.weeklyWater[,i])
  
  for (j in 1:19) { # j index which year
    subset <- cbind(subset, M.weeklyWater[,i+52*j])
  } # now "subset" is a data frame that contains the 20-year temperature data for week i
  
  # Calculate the mean
  week.avg[,i] <- rowSums(subset)/20
  # Calculate the margin of error (for confidence interval)
  for (y in 1:14) { # y index each location
    standard_error <- sd(subset[y,]) / sqrt(20)
    week.me[y,i] <- abs(qnorm((1 - confidence_level) / 2)) * standard_error
  }
  # Calculate the max and min
  week.max[,i] <- apply(subset, 1, max, na.rm=TRUE)
  week.min[,i] <- apply(subset, 1, min, na.rm=TRUE)
}



#### Save each location separately ####
# Clean the data so that we have a data frame for each river location


## St Louis River ##
time <- seq(from=1, to=52)
st.louis.mod <- as.data.frame(t(rbind(time, week.avg[1,], week.me[1,],
                                      week.max[1,], week.min[1,])))

# Add the lower and upper confidence intervals
st.louis.mod <- cbind(st.louis.mod, st.louis.mod$V2 - st.louis.mod$V3)
st.louis.mod <- cbind(st.louis.mod, st.louis.mod$V2 + st.louis.mod$V3)

# Rename the data frame
colnames(st.louis.mod) <- c("weeks", "temperature.avg", "temperature.me",
                            "max", "min", "lower.CI", "upper.CI")

write.csv(st.louis.mod, "water temperature clean/st_louis_model.csv")


## Saginaw River ##
saginaw.mod <- as.data.frame(t(rbind(time, week.avg[2,], week.me[2,],
                                     week.max[2,], week.min[2,])))

saginaw.mod <- cbind(saginaw.mod, saginaw.mod$V2 - saginaw.mod$V3)
saginaw.mod <- cbind(saginaw.mod, saginaw.mod$V2 + saginaw.mod$V3)
colnames(saginaw.mod) <- c("weeks", "temperature.avg", "temperature.me",
                           "max", "min", "lower.CI", "upper.CI")

write.csv(saginaw.mod, "water temperature clean/saginaw_model.csv")


## Fox River ##
fox.mod <- as.data.frame(t(rbind(time, week.avg[3,], week.me[3,],
                                 week.max[3,], week.min[3,])))

fox.mod <- cbind(fox.mod, fox.mod$V2 - fox.mod$V3)
fox.mod <- cbind(fox.mod, fox.mod$V2 + fox.mod$V3)
colnames(fox.mod) <- c("weeks", "temperature.avg", "temperature.me",
                       "max", "min", "lower.CI", "upper.CI")

write.csv(fox.mod, "water temperature clean/fox_model.csv")


## Portage-Burns Waterways ##
pb.mod <- as.data.frame(t(rbind(time, week.avg[4,], week.me[4,],
                                week.max[4,], week.min[4,])))

pb.mod <- cbind(pb.mod, pb.mod$V2 - pb.mod$V3)
pb.mod <- cbind(pb.mod, pb.mod$V2 + pb.mod$V3)
colnames(pb.mod) <- c("weeks", "temperature.avg", "temperature.me",
                      "max", "min", "lower.CI", "upper.CI")

write.csv(pb.mod, "water temperature clean/pb_model.csv")


## Vermilion River ##
vermilion.mod <- as.data.frame(t(rbind(time, week.avg[5,], week.me[5,],
                                       week.max[5,], week.min[5,])))

vermilion.mod <- cbind(vermilion.mod, vermilion.mod$V2 - vermilion.mod$V3)
vermilion.mod <- cbind(vermilion.mod, vermilion.mod$V2 + vermilion.mod$V3)
colnames(vermilion.mod) <- c("weeks", "temperature.avg", "temperature.me",
                             "max", "min", "lower.CI", "upper.CI")

write.csv(vermilion.mod, "water temperature clean/vermilion_model.csv")


## Genesee River ##
genesee.mod <- as.data.frame(t(rbind(time, week.avg[6,], week.me[6,],
                                     week.max[6,], week.min[6,])))

genesee.mod <- cbind(genesee.mod, genesee.mod$V2 - genesee.mod$V3)
genesee.mod <- cbind(genesee.mod, genesee.mod$V2 + genesee.mod$V3)
colnames(genesee.mod) <- c("weeks", "temperature.avg", "temperature.me",
                           "max", "min", "lower.CI", "upper.CI")

write.csv(genesee.mod, "water temperature clean/genesee_model.csv")


## Big Breek River ##
big.creek.mod <- as.data.frame(t(rbind(time, week.avg[7,], week.me[7,],
                                       week.max[7,], week.min[7,])))

big.creek.mod <- cbind(big.creek.mod, big.creek.mod$V2 - big.creek.mod$V3)
big.creek.mod <- cbind(big.creek.mod, big.creek.mod$V2 + big.creek.mod$V3)
colnames(big.creek.mod) <- c("weeks", "temperature.avg", "temperature.me",
                             "max", "min", "lower.CI", "upper.CI")

write.csv(big.creek.mod, "water temperature clean/bigcreek_model.csv")


## Big Otter Creek ##
big.otter.mod <- as.data.frame(t(rbind(time, week.avg[8,], week.me[8,],
                                       week.max[8,], week.min[8,])))

big.otter.mod <- cbind(big.otter.mod, big.otter.mod$V2 - big.otter.mod$V3)
big.otter.mod <- cbind(big.otter.mod, big.otter.mod$V2 + big.otter.mod$V3)
colnames(big.otter.mod) <- c("weeks", "temperature.avg", "temperature.me",
                             "max", "min", "lower.CI", "upper.CI")

write.csv(big.otter.mod, "water temperature clean/bigotter_model.csv")


## Still River ##
still.mod <- as.data.frame(t(rbind(time, week.avg[9,], week.me[9,],
                                   week.max[9,], week.min[9,])))

still.mod <- cbind(still.mod, still.mod$V2 - still.mod$V3)
still.mod <- cbind(still.mod, still.mod$V2 + still.mod$V3)
colnames(still.mod) <- c("weeks", "temperature.avg", "temperature.me",
                         "max", "min", "lower.CI", "upper.CI")

write.csv(still.mod, "water temperature clean/still_model.csv")


## Mississagi River ##
mississagi.mod <- as.data.frame(t(rbind(time, week.avg[10,], week.me[10,],
                                        week.max[10,], week.min[10,])))

mississagi.mod <- cbind(mississagi.mod, mississagi.mod$V2 - mississagi.mod$V3)
mississagi.mod <- cbind(mississagi.mod, mississagi.mod$V2 + mississagi.mod$V3)
colnames(mississagi.mod) <- c("weeks", "temperature.avg", "temperature.me",
                              "max", "min", "lower.CI", "upper.CI")

write.csv(mississagi.mod, "water temperature clean/mississagi_model.csv")


## Nipigon River ##
nipigon.mod <- as.data.frame(t(rbind(time, week.avg[11,], week.me[11,],
                                     week.max[11,], week.min[11,])))

nipigon.mod <- cbind(nipigon.mod, nipigon.mod$V2 - nipigon.mod$V3)
nipigon.mod <- cbind(nipigon.mod, nipigon.mod$V2 + nipigon.mod$V3)
colnames(nipigon.mod) <- c("weeks", "temperature.avg", "temperature.me",
                           "max", "min", "lower.CI", "upper.CI")

write.csv(nipigon.mod, "water temperature clean/nipigon_model.csv")


## Humber River ##
humber.mod <- as.data.frame(t(rbind(time, week.avg[12,], week.me[12,],
                                    week.max[12,], week.min[12,])))

humber.mod <- cbind(humber.mod, humber.mod$V2 - humber.mod$V3)
humber.mod <- cbind(humber.mod, humber.mod$V2 + humber.mod$V3)
colnames(humber.mod) <- c("weeks", "temperature.avg", "temperature.me",
                          "max", "min", "lower.CI", "upper.CI")

write.csv(humber.mod, "water temperature clean/humber_model.csv")


## Long Point (Inner Bay) ##
lp.mod <- as.data.frame(t(rbind(time, week.avg[13,], week.me[13,],
                                    week.max[13,], week.min[13,])))

lp.mod <- cbind(lp.mod, lp.mod$V2 - lp.mod$V3)
lp.mod <- cbind(lp.mod, lp.mod$V2 + lp.mod$V3)
colnames(lp.mod) <- c("weeks", "temperature.avg", "temperature.me",
                          "max", "min", "lower.CI", "upper.CI")

write.csv(lp.mod, "water temperature clean/lp_model.csv")


## Port Dover ##
portdover.mod <- as.data.frame(t(rbind(time, week.avg[14,], week.me[14,],
                                    week.max[14,], week.min[14,])))

portdover.mod <- cbind(portdover.mod, portdover.mod$V2 - portdover.mod$V3)
portdover.mod <- cbind(portdover.mod, portdover.mod$V2 + portdover.mod$V3)
colnames(portdover.mod) <- c("weeks", "temperature.avg", "temperature.me",
                          "max", "min", "lower.CI", "upper.CI")

write.csv(portdover.mod, "water temperature clean/portdover_model.csv")







## Notes for the for loops

subset <- data.frame(M.weeklyWater[,1])
subset <- cbind(subset, M.weeklyWater[,1+52*1])
subset <- cbind(subset, M.weeklyWater[,1+52*2])
subset

confidence_level <- 0.95
standard_error <- sd(subset[1,]) / sqrt(3)
week.me[y,i] <- qnorm((1 - confidence_level) / 2) * standard_error

qnorm((1 - confidence_level) / 2) * standard_error

qnorm(0.05/2)
qnorm((1 - 0.95)/2)

M.weeklyWater[,52] # j = 0
M.weeklyWater[,52 + 52*(1)] # j = 1
M.weeklyWater[,52 + 52*(2)] # j = 2
M.weeklyWater[,52 + 52*(25)] # j = 3






## Calculate annual temperature (optional)
annual <- rowSums(weeklyWater.C, na.rm = TRUE)/364
annual <- data.frame(annual)
write.csv(annual, file = "waterT_test.csv")
