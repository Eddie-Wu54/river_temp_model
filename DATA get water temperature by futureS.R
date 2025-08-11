#' This script:
#' 
#' 1. Extracts the average weekly water temperature data from the global water
#' temperature model for all 10 GREAT LAKES tributary locations (year 1996-2019).
#' 
#' 2. Get the water temperature for the entire period for each location.



library(raster)
library(ncdf4)
library(sp)
library(ggplot2)
library(dplyr)




#### Get spatial points ####
sp <- brick("gfdl_rcp8p5_2006-01-07_to_2019-12-30.nc", varname = "waterTemperature")




#### Import our specific locations ####
## Import location latitudes and longitudes
loc <- read.csv("tributary locations.csv", stringsAsFactors = TRUE)
tri.name=levels(loc$tributary.name)
str(loc)

lat<-loc$latitude
lon<-loc$longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations.spatial <- SpatialPoints(coords, proj4string = sp@crs)


## Get the loc_seq name from another file
air <- read.csv("tributary air temperature clean.csv", stringsAsFactors = TRUE)
loc_seq=levels(air$location)
#' IMPORTANT: Need to distinguish loc_seq and tri.name!




#### Extract water temperature data ####
wt <- brick("gfdl_hist_1996-01-07_to_2005-12-30.nc", varname = "waterTemperature")
wt1 <- brick("gfdl_rcp8p5_2006-01-07_to_2019-12-30.nc", varname = "waterTemperature")
weeklyWater <- cbind(extract(wt, locations.spatial),
                     extract(wt1, locations.spatial))


# Change from Kelvin to Celsius
M.weeklyWater <- weeklyWater - 273.15 # should have 1248 columns of data




#### Get RWT for each location ####

## Make a list to store all the results
modeledWT.list <- vector("list", length = 10)
names(modeledWT.list) <- tri.name


## Use a loop
for (i in 1:10){
  # get the data
  x <- as.data.frame(M.weeklyWater[i,])
  colnames(x) <- "preds.futureS"
  
  x$week <- rep(1:52, length.out = nrow(x))
  years <- 1996:2019
  x$year <- rep(years, each = 52, length.out = nrow(x))
  
  # select only the years needed
  x <- x %>% filter(year >= loc[loc$tributary.name == tri.name[i],]$start &
                    year <= loc[loc$tributary.name == tri.name[i],]$end) %>% 
             mutate(location = loc_seq[i]) #distinguish loc_seq and tri.name
  
  # store in the resulting list
  modeledWT.list[[i]] <- x
}


## Combine and output
modeledWT.outdf <- do.call(rbind, modeledWT.list)
modeledWT.outdf$week <- modeledWT.outdf$week + 1
#' IMPORTANT: Since the model defines the first week of the year starting from
#' Jan 7th, we need to add 1 to the week count, so that it represents the actual
#' calendar week number!

write.csv(modeledWT.outdf, file="futureS modeled water temperature.csv")
