#' This script is used to obtain river discharge data from the 10 tributary sites.
#' Data is extracted from ECCC(WSA) for Canadian sites, and USGS stations for where
#' the river temperatures were originally collected.

#' The duration of the study is from 1998 - 2019, in total 22 years.



library(tidyhydat) #for WSC date
library(dataRetrieval) #for USGS data
library(geosphere)
library(dplyr)
library(lubridate)

conv <- 0.028316847 #convert from cubic feet per second to cubic meter per second




#### Extract river discharge from WSC for CA locations ####

download_hydat()

#' These locations can use WSC discharge data: 
#'' 1. Big creek: BIG CREEK NEAR WALSINGHAM; 02GC007; 2000-2014 (ect 2010,2011)
#'' 2. Humber: HUMBER RIVER AT WESTO; 02HC003; 1998-2013 (ect 2004,2010)


## Big creek
bigcreek <- hy_daily_flows(station_number = "02GC007",
                           start_date = "1998-01-01", end_date = "2019-12-31") %>% 
  mutate(location = "bigcreek") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)

mean(bigcreek$flow)


## Humber
humber <- hy_daily_flows(station_number = "02HC003",
                         start_date = "1998-01-01", end_date = "2019-12-31") %>% 
  mutate(location = "humber") %>% 
  select(location, station_id = STATION_NUMBER, Date, flow = Value)

mean(humber$flow)




#### Extract river discharge from USGS for US locations ####

#' These are the USGS gauge locations:
#' 1. St.Louis: 04024000
#' 2. Salmon trout: 04043238
#' 3. Saginaw: 04157005
#' 4. Refile: 04142000
#' 5. St.Joseph River: 04101500
#' 6. Yellow: 05517000
#' 7. Genesee: 04231600
#' 8. Vermilion: 04199500


## St.Louis
stlouis <- readNWISdv(siteNumbers = "04024000", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>%
  mutate(flow = X_00060_00003*conv, location = "stlouis") %>% 
  select(location, station_id = site_no, Date, flow)

mean(stlouis$flow)


## Salmon trout
salmontrout <- readNWISdv(siteNumbers = "04043238", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>%
  mutate(flow = X_00060_00003*conv, location = "salmontrout") %>% 
  select(location, station_id = site_no, Date, flow)

mean(salmontrout$flow)


## Saginaw
saginaw <- readNWISdv(siteNumbers = "04157005", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "saginaw") %>% 
  select(location, station_id = site_no, Date, flow)

mean(saginaw$flow)


## Refile
rifle <- readNWISdv(siteNumbers = "04142000", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "rifle") %>% 
  select(location, station_id = site_no, Date, flow)

mean(rifle$flow)


## St.Joseph
stjoseph <- readNWISdv(siteNumbers = "04101500", parameterCd = "00060",
                  startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "stjoseph") %>% 
  select(location, station_id = site_no, Date, flow)

mean(stjoseph$flow)


## Yellow
yellowr <- readNWISdv(siteNumbers = "05517000", parameterCd = "00060",
                 startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "yellowr") %>% 
  select(location, station_id = site_no, Date, flow)

mean(yellowr$flow)


## Genesee
genesee <- readNWISdv(siteNumbers = "04231600", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "genesee") %>% 
  select(location, station_id = site_no, Date, flow)

mean(genesee$flow)


# ## Vermilion
# vermilion <- readNWISdv(siteNumbers = "04199500", parameterCd = "00060",
#                         startDate = "1998-01-01", endDate = "2019-12-31") %>% 
#   mutate(flow = X_00060_00003*conv, location = "vermilion") %>% 
#   select(location, station_id = site_no, Date, flow)
# 
# mean(vermilion$flow)


## Allegheny River
allegheny <- readNWISdv(siteNumbers = "03012550", parameterCd = "00060",
                      startDate = "1998-01-01", endDate = "2019-12-31") %>% 
  mutate(flow = X_00060_00003*conv, location = "allegheny") %>% 
  select(location, station_id = site_no, Date, flow)

mean(allegheny$flow)




#### Combine and output ####
results.df <- rbind(stlouis, salmontrout, saginaw, rifle, stjoseph,
                    yellowr, genesee, humber, allegheny, bigcreek)

meanflow <- results.df %>% group_by(location) %>% summarise(mf = mean(flow))
View(meanflow)

write.csv(meanflow, "21-year average river discharge.csv")

