#' This script is used to obtain river discharge data from the 14 tributary sites.
#' Data is extracted from ECCC(WSA) for Canadian sites, and USGS stationsfor where
#' the river temperatures were orginally collected.


library(tidyhydat) #for WSC date
library(dataRetrieval) #for USGS data
library(geosphere)
library(dplyr)
library(lubridate)

conv <- 0.028316847 #convert from cubic feet per second to cubic meter per second



#### Find WSC stations ####
#' We use the "tidyhydat" package to find stations and extract data from the
#' Water Survey of Canada data sources.

download_hydat(dl_hydat_here = NULL, ask = TRUE)

tributary.loc <- read.csv("tributary locations.csv")
tributary.loc.wsc <- tributary.loc[tributary.loc$country == "ca",]


## WSC

wsc.list <- data.frame()

# Retrieve station metadata
stations <- hy_stations()

# Loop for each location
for (i in 1:nrow(tributary.loc.wsc)){
  
  current <- tributary.loc.wsc[i,]
  
  # calculate distance
  distances <- geosphere::distHaversine(
    cbind(current$longitude, current$latitude),
    stations[, c("LONGITUDE", "LATITUDE")])
  
  # find the 5 closest stations
  df <- cbind(stations, dis = distances/1000)
  top5 <- head(df[order(df$dis), ], 5)
  top5$location <- current$tributary.name
  
  wsc.list <- rbind(wsc.list, top5)
}

View(wsc.list)


## Check data records
#' Then we need to manually check these 8 sights on the WSC website to ensure:
#' 
#' 1. They all have complete data records for these years.
#' 2. They are in the correct tributary location.

wsc.list.updated <- wsc.list %>% 
  select(tributary = location, id = STATION_NUMBER, name = STATION_NAME,
         lat = LATITUDE, long = LONGITUDE, dis)

# Check each station individually by its station number
check <- hy_stn_data_coll(station_number = "02GC008")
sa <- check[check$DATA_TYPE == "Flow",]


#' These locations can use ECCC air temperature: 
#'' 1. Big creek: BIG CREEK NEAR WALSINGHAM; 02GC007; 2000-2014 (ect 2010,2011)
#'' 2. Big otter: BIG OTTER CREEK NEAR CALTON; 02GC026; 2012-2014
#'' 3. Still: MAGNETAWAN RIVER NEAR BRITT; 02EA011; 2002-2008 (ect 03,06,07)
#'' 4. Mississagi: LITTLE WHITE RIVER NEAR BELLINGHAM; 02CC005; 2007-2014 (ect 2008,09)
#'' 5. Nipigon: NIPIGON RIVER BELOW ALEXANDER GENERATING STATION (only have 2008-2010 data); 02AD012; 1998-2010(ect 1999)
#'' 6. Humber: HUMBER RIVER AT WESTO; 02HC003; 1998-2013 (ect 2004,2010)
#' 7. Long point: VENISON CREEK NEAR WALSINGHAM; 02GC021; 2004-2009
#' 8. Port dover: LYNN RIVER AT SIMCOE; 02GC008; 2006-2012 (ect 09,10)


#' NOTES:
#' 
#' 1. Mississagi and Nipigon are at Fall locations, the discharges might not be that
#' accurate...
#' 2. Long point and Port dover are not tributary locations, so probably should
#' not be included...

final.ids <- c("02GC007","02GC026","02EA011","02CC005","02AD012","02HC003")
wsc.list.final <- wsc.list.updated[wsc.list.updated$id %in% final.ids, ]



#### Extract river discharge from WSC for CA locations ####

## Big creek
bigcreek <- hy_daily_flows(station_number = "02GC007",
                           start_date = "2000-01-01", end_date = "2014-12-31") %>% 
  filter(year(Date) != 2010 & year(Date) != 2011) %>% 
  mutate(location = "bigcreek") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)


## Big otter
bigotter <- hy_daily_flows(station_number = "02GC026",
                           start_date = "2012-01-01", end_date = "2014-12-31") %>%
  mutate(location = "bigotter") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)


## Still
still <- hy_daily_flows(station_number = "02EA011",
                        start_date = "2002-01-01", end_date = "2008-12-31") %>% 
  filter(year(Date) != 2003 & year(Date) != 2006 & year(Date) != 2007) %>%
  mutate(location = "still") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)


## Mississagi
mississagi <- hy_daily_flows(station_number = "02CC005",
                         start_date = "2010-01-01", end_date = "2014-12-31") %>% 
  mutate(location = "mississagi") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)


## Nipigon
nipigon <- hy_daily_flows(station_number = "02AD012",
                      start = "2008-01-01", end = "2010-12-31") %>% 
  mutate(location = "nipigon") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)


## Humber (1998-2013, ect 2004,2010)
humber <- hy_daily_flows(station_number = "02HC003",
                         start_date = "1998-01-01", end_date = "2013-12-31") %>% 
  filter(year(Date) != 2004 & year(Date) != 2010) %>%
  mutate(location = "humber") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)




#### Extract river discharge from USGS for US locations ####

#' These are the USGS gauge locations:
#' 1. St.Louis: 04024000; 2011-2014
#' 2. Saginaw: 04157005; 2012-2014
#' 3. Fox: 040851385; 2011-2014
#' 4. Portage-Burns Waterway: 04095090; 2011-2012
#' 5. Vermilion: 04199500; 2012-2014
#' 6. Genesee: 04231600; 2011-2013


## Get the tributary locations
tributary.loc <- read.csv("tributary locations.csv")
tributary.loc.usgs <- tributary.loc[tributary.loc$tributary.name == 
                                      c("St.Louis River","Saginaw River",
                                        "Fox River","Portage-Burns Waterway",
                                        "Vermilion River","Genesee River"),]


## St.Louis (2011-2014)
stlouis <- readNWISdv(siteNumbers = "04024000", parameterCd = "00060",
                      startDate = "2011-01-01", endDate = "2014-12-31") %>%
  mutate(flow = X_00060_00003*conv, location = "stlouis") %>% 
  select(location, station_id = site_no, Date, flow)


## Saginaw (2012-2014)
saginaw <- readNWISdv(siteNumbers = "04157005", parameterCd = "00060",
                      startDate = "2012-01-01", endDate = "2014-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "saginaw") %>% 
  select(location, station_id = site_no, Date, flow)


## Fox (2011-2014)
fox <- readNWISdv(siteNumbers = "040851385", parameterCd = "00060",
                  startDate = "2011-01-01", endDate = "2014-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "fox") %>% 
  select(location, station_id = site_no, Date, flow)


## Portage-Burns Waterway (2011-2012)
pb <- readNWISdv(siteNumbers = "04095090", parameterCd = "00060",
                 startDate = "2011-01-01", endDate = "2012-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "portage") %>% 
  select(location, station_id = site_no, Date, flow)


## Vermilion (2012-2014)
vermilion <- readNWISdv(siteNumbers = "04199500", parameterCd = "00060",
                        startDate = "2012-01-01", endDate = "2014-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "vermilion") %>% 
  select(location, station_id = site_no, Date, flow)


## Genesee (2011-2013)
genesee <- readNWISdv(siteNumbers = "04199500", parameterCd = "00060",
                      startDate = "2011-01-01", endDate = "2013-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "genesee") %>% 
  select(location, station_id = site_no, Date, flow)



#### Combine all discharge outputs ####
results.df <- rbind(stlouis, saginaw, fox, pb, vermilion, genesee, bigcreek,
                    bigotter, still, mississagi, humber, nipigon)

View(results.df)
str(results.df)


## Export as csv
write.csv(results.df, "tributary discharge.csv")

