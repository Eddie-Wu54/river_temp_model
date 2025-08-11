#' This script is used to obtain the air temperature data from
#' Environmental and Climate Change Canada (ECCC) for Canadian locations,
#' and NOAA from US locations.


{
  library(weathercan)
  library(rnoaa)
  library(geosphere)
  library(dplyr)
  library(sf)
  library(lubridate)
}


tributary.loc <- read.csv("tributary locations.csv")



#### Find ECCC weather stations (2 sites) ####
tri.ca <- tributary.loc[tributary.loc$country == "ca",]

# EXAMPLE:
A.example <- tri.ca[1,]

A <- stations_search(coords = c(A.example$latitude, A.example$longitude),
                     interval = "day", dist = 50,
                     starts_latest = 2009, ends_earliest = 2014)


## Store station ID for each location
#stations_dl() #update station information if needed
eccc.list <- vector("list", 2)
names(eccc.list) <- tri.ca$tributary.name


# Use a loop
for (i in 1:nrow(tri.ca)) {
  # Latitude and longitude for the current location
  lat <- tri.ca[i,]$latitude
  lon <- tri.ca[i,]$longitude
  
  # Start and end data
  start <- tri.ca[i,]$start
  end <- tri.ca[i,]$end
  
  # Get the station ID
  info <- stations_search(coords = c(lat, lon), interval = "day", dist = 50,
                           starts_latest = start, ends_earliest = end)
  
  # Store the temperature data in a list
  eccc.list[[i]] <- info
}


View(eccc.list[[1]])
View(eccc.list[[2]])
#' These two locations can use ECCC air temperature:
#' 1. Humber: TORONTO LESTER B. PEARSON INT'L A; 5097; 11.24
#' 2. Big creek: DELHI CS; 27528; 12.92



#### Extract air temperature from ECCC stations ####

## Humber (1998-2013, ect 2004,2010)
humber <- weather_dl(station_ids = 5097, interval = "day", quiet = T,
                     start = "1998-01-01", end = "2013-12-31") %>% 
  select(station_id, date, max_temp, min_temp, mean_temp) %>% 
  rename(max_air = max_temp, min_air = min_temp, mean_air = mean_temp) %>% 
  mutate(location = "humber")


## Big creek (2000-2009, 2012-2014)
#' 1. Big creek: DELHI CS; 27528; 2000-2014 (ect 2010,2011)
bigcreek <- weather_dl(station_ids = 27528, interval = "day", quiet = T,
                       start = "2000-01-01", end = "2014-12-31") %>% 
  select(station_id, date, max_temp, min_temp, mean_temp) %>% 
  rename(max_air = max_temp, min_air = min_temp, mean_air = mean_temp) %>% 
  mutate(location = "bigcreek")




#### Find NOAA weather stations (6 sites) ####

# Select the 6 tributary locations
tri.us <- tributary.loc[tributary.loc$country == "us",]

## GHCNd
ghcnd.list <- vector("list", 8)
names(ghcnd.list) <- tri.us$tributary.name


# Retrieve station metadata
stations <- ghcnd_stations() %>% 
  filter(state == "MN" | state == "WI" | state == "MI" | state == "IL"
         | state == "IN" | state == "OH" | state == "NY") %>% 
  filter(element == "TMAX" | element == "TMIN" | element == "TAVG")


## Get closest weather station
for (i in 1:nrow(tri.us)) {
  # new lc dataframe
  lc <- tri.us[i,c("tributary.name", "latitude", "longitude")]
  lc<- rename(lc, id = tributary.name)
  
  # Start and end data
  start <- tri.us[i,]$start
  end <- tri.us[i,]$end
  
  # get station
  info <- meteo_nearby_stations(
    lat_lon_df = lc, lat_colname = "latitude", lon_colname = "longitude",
    station_data = ghcnd_stations(),
    var = "TMAX",
    year_min = start,
    year_max = end,
    limit = 5
  )
  
  info <- as.data.frame(info[[1]])
  
  # Store the temperature data in a list
  ghcnd.list[[i]] <- info
}


View(ghcnd.list[[1]])
View(ghcnd.list[[2]])
View(ghcnd.list[[3]])
View(ghcnd.list[[4]])
View(ghcnd.list[[5]])
View(ghcnd.list[[6]])
View(ghcnd.list[[7]])
View(ghcnd.list[[8]])


#' These locations can use NOAA air temperature: 
#' 1. St.Louis River: SAGINAW MINNESOTA; USR0000MSAG; 15.94
#' 2. Salmon Trout River: BIG BAY 1NW; USC00200770; 11.19
#' 3. Saginaw River: SAGINAW MBS INTL AP; USW00014845; 15.76
#' 4. Rifle River: W BRANCH 3SE; USC00208800; 24.84
#' 5. St.Joseph River: NILES; USC00205892; 1.36
#' 6. Yellow River: KNOX WWTP; USC00124657; 0.73
#' 7. Genesee River: ROCHESTER GTR INTL; USW00014768; 5.51
#' 8. Allegheny River: WARREN; USC00369298; 11.40

ghcnd.df <- rbind(ghcnd.list[[1]][2,],ghcnd.list[[2]][2,],
                  ghcnd.list[[3]][3,],ghcnd.list[[4]][2,],
                  ghcnd.list[[5]][1,],ghcnd.list[[6]][2,],
                  ghcnd.list[[7]][1,],ghcnd.list[[8]][1,])
ghcnd.df <- cbind(tri.us$tributary.name, ghcnd.df)




#### Extract air temperature from NOAA stations ####

## List to store all the RWT dataframe
ATlist <- vector("list", length = 8)
names(ATlist) <- tri.us$tributary.name


## Use a loop to extract air temperature fpr all locations
for (i in 1:8) {
  airData <- meteo_tidy_ghcnd(stationid = ghcnd.df[i,]$id,
                              var = c("tmax","tmin"),
                              date_min = paste0(tri.us$start[i], "-01-01"),
                              date_max = paste0(tri.us$end[i], "-12-31")) %>% 
    as.data.frame()
  
  ATlist[[i]] <- airData
}



## St.Louis River
stlouis <- ATlist[[1]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "stlouis")


## Salmon Trout River
salmon.trout <- ATlist[[2]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "salmontrout")


## Saginaw River
saginaw <- ATlist[[3]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "saginaw")


## Rifle River
rifle <- ATlist[[4]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "rifle")


## St.Joseph River
stjoseph <- ATlist[[5]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "stjoseph")


## Yellow River
yellow <- ATlist[[6]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "yellowr")


## Genesee River
genesee <- ATlist[[7]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "genesee")


## Allegheny River
allegheny <- ATlist[[8]] %>% 
  rename(station_id = id, max_air = tmax, min_air = tmin) %>% 
  mutate(max_air = max_air/10, min_air = min_air/10,
         mean_air = (max_air+min_air)/2, location = "allegheny")



#### Combine all temperature output ####

## Combine outputs from all locations
combined_AT <- rbind(humber, bigcreek, stlouis, salmon.trout, saginaw,
                     rifle, stjoseph, yellow, genesee, allegheny)



## Export as csv
write.csv(combined_AT, "tributary air temperature clean.csv")



