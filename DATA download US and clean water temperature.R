#' This script is used to
#' 1. download the US water temperature data from the USGS website.
#' 2. clean and combine CA and US water temperature records.


library(dataRetrieval) #for USGS data
library(tidyverse)




## Importing
loc <- read.csv("tributary locations.csv", stringsAsFactors = FALSE)
loc.us <- loc %>% filter(country == "us")




#### Download US water temp data ####
## St.Louis
stlouis <- readNWISdv(siteNumbers = "04024000", parameterCd = "00010",
                      startDate = paste0(loc.us$start[1], "-01-01"),
                      endDate = paste0(loc.us$end[1], "-12-31")) %>% 
  mutate(location = "stlouis") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Salmon trout
salmontrout <- readNWISdv(siteNumbers = "04043238", parameterCd = "00010",
                          startDate = paste0(loc.us$start[2], "-01-01"),
                          endDate = paste0(loc.us$end[2], "-12-31")) %>%
  mutate(location = "salmontrout") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Saginaw
saginaw <- readNWISdv(siteNumbers = "04157005", parameterCd = "00010",
                          startDate = paste0(loc.us$start[3], "-01-01"),
                          endDate = paste0(loc.us$end[3], "-12-31")) %>%
  mutate(location = "saginaw") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Refile
rifle <- readNWISdv(siteNumbers = "04142000", parameterCd = "00010",
                      startDate = paste0(loc.us$start[4], "-01-01"),
                      endDate = paste0(loc.us$end[4], "-12-31")) %>%
  mutate(location = "rifle") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## St.Joseph
stjoseph <- readNWISdv(siteNumbers = "04101500", parameterCd = "00010",
                     startDate = paste0(loc.us$start[5], "-01-01"),
                     endDate = paste0(loc.us$end[5], "-12-31")) %>%
  mutate(location = "stjoseph") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Yellow
yellowr <- readNWISdv(siteNumbers = "05517000", parameterCd = "00010",
                       startDate = paste0(loc.us$start[6], "-01-01"),
                       endDate = paste0(loc.us$end[6], "-12-31")) %>%
  mutate(location = "yellowr") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Genesee
genesee <- readNWISdv(siteNumbers = "04231600", parameterCd = "00010",
                      startDate = paste0(loc.us$start[7], "-01-01"),
                      endDate = paste0(loc.us$end[7], "-12-31")) %>%
  mutate(location = "genesee") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)


## Allegheny River
allegheny <- readNWISdv(siteNumbers = "03012550", parameterCd = "00010",
                        startDate = paste0(loc.us$start[8], "-01-01"),
                        endDate = paste0(loc.us$end[8], "-12-31")) %>%
  mutate(location = "allegheny") %>% 
  dplyr::select(location, station_id = site_no, Date, temp = X_00010_00003)



rwt.us <- results.df <- rbind(stlouis, salmontrout, saginaw, rifle, stjoseph,
                              yellowr, genesee, allegheny)
names(rwt.us)[names(rwt.us) == "Date"] <- "date"




#### Combine US and CA RWT
rwt.ca <- read.csv("water_temperature_d.csv")
rwt.ca$date <- as.Date(rwt.ca$date, format = "%m/%d/%Y")

rwt.ca <- rwt.ca %>% 
  filter(location == "bigcreek" | location == "humber") %>% 
  mutate(station_id = 999, tempr = round(temp, digits = 2)) %>% 
  dplyr::select(location, station_id, date, temp=tempr)

rwt.gl <- rbind(rwt.ca, rwt.us)
write.csv(rwt.gl, "tributary water temperature clean.csv")





