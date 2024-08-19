#' This script:
#' 
#' 1. Extracts the average weekly water temperature data from the global water
#' temperature model for all 12 GREAT LAKES tributary locations (year 1996-2019).
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
loc <- read.csv("tributary locations.csv")
str(loc)

lat<-loc$latitude
lon<-loc$longitude

# Turn them to spatial points
coords <- data.frame(longitude=lon, latitude=lat)
locations.spatial <- SpatialPoints(coords, proj4string = sp@crs)


#### Extract water temperature data ####
wt <- brick("gfdl_hist_1996-01-07_to_2005-12-30.nc", varname = "waterTemperature")
wt1 <- brick("gfdl_rcp8p5_2006-01-07_to_2019-12-30.nc", varname = "waterTemperature")
weeklyWater <- cbind(extract(wt, locations.spatial),
                     extract(wt1, locations.spatial))


# Change from Kelvin to Celsius
M.weeklyWater <- weeklyWater - 273.15 # should have 1248 columns of data



#### Get the week temperature for each loc each year ####



#### Save each location separately ####
# Clean the data so that we have a data frame for each river location


## St Louis River ##
x <- as.data.frame(M.weeklyWater[1,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
stlouis.mod <- x %>% filter(year >= loc[loc$tributary.name == "St.Louis River",]$start &
                            year <= loc[loc$tributary.name == "St.Louis River",]$end) %>% 
  mutate(location = "stlouis")


## Saginaw River ##
x <- as.data.frame(M.weeklyWater[2,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
saginaw.mod <- x %>% filter(year >= loc[loc$tributary.name == "Saginaw River",]$start &
                            year <= loc[loc$tributary.name == "Saginaw River",]$end) %>% 
  mutate(location = "saginaw")


## Fox River ##
x <- as.data.frame(M.weeklyWater[3,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
fox.mod <- x %>% filter(year >= loc[loc$tributary.name == "Fox River",]$start &
                        year <= loc[loc$tributary.name == "Fox River",]$end) %>% 
  mutate(location = "fox")


## Portage-Burns Waterways ##
x <- as.data.frame(M.weeklyWater[4,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
portage.mod <- x %>% filter(year >= loc[loc$tributary.name == "Portage-Burns Waterway",]$start &
                            year <= loc[loc$tributary.name == "Portage-Burns Waterway",]$end) %>% 
  mutate(location = "portage")


## Vermilion River ##
x <- as.data.frame(M.weeklyWater[5,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
vermilion.mod <- x %>% filter(year >= loc[loc$tributary.name == "Vermilion River",]$start &
                              year <= loc[loc$tributary.name == "Vermilion River",]$end) %>% 
  mutate(location = "vermilion")


## Genesee River ##
x <- as.data.frame(M.weeklyWater[6,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
genesee.mod <- x %>% filter(year >= loc[loc$tributary.name == "Genesee River",]$start &
                            year <= loc[loc$tributary.name == "Genesee River",]$end) %>% 
  mutate(location = "genesee")


## Big Breek River ##
x <- as.data.frame(M.weeklyWater[7,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
bigcreek.mod <- x %>% filter(year >= loc[loc$tributary.name == "Big Creek River",]$start &
                             year <= loc[loc$tributary.name == "Big Creek River",]$end) %>% 
  mutate(location = "bigcreek")


## Big Otter Creek ##
x <- as.data.frame(M.weeklyWater[8,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
bigotter.mod <- x %>% filter(year >= loc[loc$tributary.name == "Big Otter River",]$start &
                             year <= loc[loc$tributary.name == "Big Otter River",]$end) %>% 
  mutate(location = "bigotter")


## Still River ##
x <- as.data.frame(M.weeklyWater[9,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
still.mod <- x %>% filter(year >= loc[loc$tributary.name == "Still River",]$start &
                          year <= loc[loc$tributary.name == "Still River",]$end) %>% 
  mutate(location = "still")


## Mississagi River ##
x <- as.data.frame(M.weeklyWater[10,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
mississagi.mod <- x %>% filter(year >= loc[loc$tributary.name == "Mississagi River",]$start &
                               year <= loc[loc$tributary.name == "Mississagi River",]$end) %>% 
  mutate(location = "mississagi")


## Nipigon River ##
x <- as.data.frame(M.weeklyWater[11,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
nipigon.mod <- x %>% filter(year >= loc[loc$tributary.name == "Nipigon River",]$start &
                            year <= loc[loc$tributary.name == "Nipigon River",]$end) %>% 
  mutate(location = "nipigon")


## Humber River ##
x <- as.data.frame(M.weeklyWater[12,])
colnames(x) <- "modeled temp"

x$week <- rep(1:52, length.out = nrow(x))
years <- 1996:2019
x$year <- rep(years, each = 52, length.out = nrow(x))

# select only the year needed
humber.mod <- x %>% filter(year >= loc[loc$tributary.name == "Humber River",]$start &
                           year <= loc[loc$tributary.name == "Humber River",]$end) %>% 
  mutate(location = "humber")


## Combine and output
modeledWT.out <- rbind(stlouis.mod, saginaw.mod, fox.mod, portage.mod,
                       vermilion.mod, genesee.mod, bigcreek.mod, bigotter.mod,
                       still.mod, mississagi.mod, nipigon.mod, humber.mod)

write.csv(modeledWT.out, file="futureS modeled water temperature.csv")
