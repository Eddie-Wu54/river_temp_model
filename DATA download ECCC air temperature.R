#' This script is used to obtain the air temperature data from
#' Environmental and Climate Change Canada (ECCC).



library(weathercan)
library(rnoaa)
library(geosphere)
library(dplyr)


#### Find the ECCC weather stations (8 sites) ####

tributary.loc <- read.csv("tributary locations.csv")

# EXAMPLE:
A.example <- tributary.loc[10,]

A <- stations_search(coords = c(A.example$latitude, A.example$longitude),
                     interval = "day", dist = 50,
                     starts_latest = 2009, ends_earliest = 2014)


## Store station ID for each location
station.info <- list()

# Use a loop
for (i in 1:nrow(tributary.loc)) {
  # Latitude and longitude for the current location
  lat <- tributary.loc[i,]$latitude
  lon <- tributary.loc[i,]$longitude
  
  # Start and end data
  start <- tributary.loc[i,]$start
  end <- tributary.loc[i,]$end
  
  # Get the station ID
  info <- stations_search(coords = c(lat, lon), interval = "day", dist = 50,
                           starts_latest = start, ends_earliest = end)
  
  # Store the temperature data in a list
  station.info[[i]] <- info
}


## Check information for each of the 14 locations

for (i in 1:nrow(tributary.loc)){
  
  # Extarct name as character
  name <- as.character(tributary.loc[i, 1])
  assign(name, station.info[[i]])
}


#' These locations can use ECCC air temperature: 
#' 1. Big creek: DELHI CS; 27528; 2000-2014 (ect 2010,2011)
#' 2. Big otter: TILLSONBURG NORTH; 27488; 2012-2014
#' 3. Humber: TORONTO LESTER B. PEARSON INT'L A; 5097; 1998-2013 (ect 2004,2010)
#' 4. Long point: LONG POINT(AUT) (do not have 04-05 data); 9026; 2004-2009
#' 5. Port dover: DELHI CS; 27528; 2006-2012 (ect 09,10)
#' 6. Nipigon: CAMERON FALLS (AUT); 27674; 1998-2010(ect 1999)
#' 7. Still: MONETVILLE; 4125; 2002-2008 (ect 03,06,07)
#' 8. Mississagi: GORE BAY CLIMATE (only 2010-2014); 48788; 2007-2014 (ect 2008,09)

#' Locations that do not have corresponding weather stations:
#' St.louis; Fox; Vermilion; PB; Saginaw; Genesee




#### Extract air temperature from ECCC stations ####


## Big creek (2000-2009, 2012-2014)
#' 1. Big creek: DELHI CS; 27528; 2000-2014 (ect 2010,2011)
bigcreek <- weather_dl(station_ids = 27528, interval = "day", quiet = T,
                       start = "2000-01-01", end = "2014-12-31") %>% 
  select(station_name, station_id, date, year, month, day,
         max_temp, mean_temp, min_temp) %>% 
  filter(year != 2010 & year != 2011)

## Big otter (2012-2014)
#' 2. Big otter: TILLSONBURG NORTH; 27488; 2012-2014
bigotter <- weather_dl(station_ids = 27488, interval = "day", quiet = T,
                        start = "2012-01-01", end = "2014-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)


## Humber (1998-2013, ect 2004,2010)
#' 3. Humber: TORONTO LESTER B. PEARSON INT'L A; 5097; 1998-2013 (ect 2004,2010)
humber <- weather_dl(station_ids = 5097, interval = "day", quiet = T,
                     start = "1998-01-01", end = "2013-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp) %>% 
  filter(year != 2004 & year != 2010)


## Long point
#' 4. Long point: LONG POINT(AUT) (do not have 04-05 data); 9026; 2004-2009
longpoint <- weather_dl(station_ids = 9026, interval = "day", quiet = T,
                        start = "2006-01-01", end = "2009-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)


## Port Dover (2006-2012, ect 09,10)
portdover <- weather_dl(station_ids = 27674, interval = "day", quiet = T,
                        start = "2006-01-01", end = "2012-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp) %>% 
  filter(year != 2009 & year != 2010)


## Nipigon (1998-2010, ect 1999)
#' 6. Nipigon: CAMERON FALLS (AUT); 27674; 1998-2010(ect 1999)
nipigon <- weather_dl(station_ids = 27528, interval = "day", quiet = T,
                      start = "1998-01-01", end = "2010-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp) %>% 
  filter(year != 1999)


## Still
#' 7. Still: MONETVILLE; 4125; 2002-2008 (ect 03,06,07)
still <- weather_dl(station_ids = 4125, interval = "day", quiet = T,
                    start = "2002-01-01", end = "2008-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp) %>% 
  filter(year != 2003 & year != 2006 & year != 2007)


## Mississagi
#' 8. Mississagi: GORE BAY CLIMATE (only 2010-2014); 48788; 2007-2014 (ect 2008,09)
mississagi <- weather_dl(station_ids = 48788, interval = "day", quiet = T,
                    start = "2010-01-01", end = "2014-12-31") %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)




#### Find NOAA weather stations (6 sites) ####

# Select the 6 tributary locations
tributary.loc.noaa <- tributary.loc[tributary.loc$tributary.name == 
                                      c("St.Louis River","Saginaw River",
                                        "Fox River","Portage-Burns Waterway",
                                        "Vermilion River","Genesee River"),]

## GHCNd

ghcnd.list <- data.frame()

# Retrieve station metadata
stations <- ghcnd_stations() %>% 
  filter(state == "MN" | state == "WI" | state == "MI" | state == "IL"
         | state == "IN" | state == "OH" | state == "NY") %>% 
  filter(element == "TMAX" | element == "TMIN" | element == "TAVG")


# Loop for each location

for (i in 1:nrow(tributary.loc.noaa)){
  
  current <- tributary.loc.noaa[i,]
  
  # subset the stations with our data time frame
  stations.current <- stations %>% 
    filter(first_year <= current$start & last_year >= current$end )
  
  # calculate distance
  distances <- geosphere::distHaversine(
    cbind(current$longitude, current$latitude),
    stations.current[, c("longitude", "latitude")])
  
  # find the cloest station
  closest_station_index <- which.min(distances)
  closest_station <- stations.current[closest_station_index, ]
  closest_station <- cbind(closest_station, distance = min(distances)/1000)
  
  ghcnd.list <- rbind(ghcnd.list, closest_station)
}

View(ghcnd.list)


## ANOTHER WAY TO EXTRACT WEATHER STATION INFORMATION: SAME RESULTS

lc <- tributary.loc.noaa %>% 
  select(tributary.name, latitude, longitude) %>% 
  rename(id = tributary.name)
View(lc)

meteo_nearby_stations(
  lat_lon_df = lc, lat_colname = "latitude", lon_colname = "longitude",
  station_data = ghcnd_stations(),
  var = "TMAX",
  year_min = 2011,
  year_max = 2014,
  limit = 1
)


#' These locations can use NOAA air temperature: 
#' 1. St.Louis: CLOQUET; USC00211630; 2011-2014
#' 2. Saginaw: SAGINAW #3; USC00207222; 2012-2014
#' 3. Fox: GREEN BAY BOTANICAL; USC00473271; 2011-2014
#' 4. Portage-Burns Waterway: INDIANA DUNES NP; USC00124244; 2011-2012
#' 5. Vermilion: ELYRIA LORAIN CO AP; USW00004849; 2012-2014
#' 6. Genesee: ROCHESTER GTR INTL; USW00014768; 2011-2013




#### Extract air temperature from NOAA stations ####

## St.Louis (2011-2014)
stlouis <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[1],
                               var = c("tmax","tmin"),
                               date_min = "2011-01-01", date_max = "2014-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[1]) %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)

View(stlouis)


## Saginaw (2012-2014)
saginaw <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[2],
                            var = c("tmax","tmin"),
                            date_min = "2012-01-01", date_max = "2014-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[2]) %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)

View(saginaw)


## Fox (2011-2014)
fox <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[3],
                        var = c("tmax","tmin"),
                        date_min = "2011-01-01", date_max = "2014-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[3]) %>% 
  select(station_name, station_id, year, month, day,
         max_temp, mean_temp, min_temp)

View(fox)


## Portage-Burns Waterway (2011-2012)
pb <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[4],
                       var = c("tmax","tmin"),
                       date_min = "2011-01-01", date_max = "2012-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[4]) %>% 
  select(station_name, station_id, date, year, month, day,
         max_temp, mean_temp, min_temp)

View(pb)


## vermilion (2012-2014)
vermilion <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[5],
                              var = c("tmax","tmin"),
                              date_min = "2012-01-01", date_max = "2014-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[5]) %>% 
  select(station_name, station_id, date, year, month, day,
         max_temp, mean_temp, min_temp)

View(vermilion)


## Genesee (2011-2013)
genesee <- meteo_tidy_ghcnd(stationid = ghcnd.list$id[6],
                            var = c("tmax","tmin"),
                            date_min = "2011-01-01", date_max = "2013-12-31") %>% 
  as.data.frame() %>% 
  mutate(station_id = id, year = format(date, "%Y"), month = format(date, "%m"),
         day = format(date,"%d"), max_temp = tmax/10, min_temp = tmin/10,
         mean_temp = NA, station_name = ghcnd.list$name[6]) %>% 
  select(station_name, station_id, date, year, month, day,
         max_temp, mean_temp, min_temp)

View(genesee)




#### Combine all temperature output ####

## Combine outputs from all locations
results.df <- rbind(stlouis, saginaw, fox, pb, vermilion, genesee, bigcreek,
                    bigotter, still, mississagi, humber, nipigon, longpoint, portdover)

View(results.df)
str(results.df)


## Export as csv
write.csv(results.df, "tributary air temperatures clean.csv")



