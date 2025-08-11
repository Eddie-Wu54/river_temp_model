#' This script is used to combine air and water temperature together. It is also used to determine
#' the complete years, and randomly subsample 5 years from all the available years for each location.


library(zoo)
library(tidyverse)




#### Import and combine ####

## Import
air <- read.csv("tributary air temperature clean.csv", stringsAsFactors=F)
water <- read.csv("tributary water temperature clean.csv", stringsAsFactors=F)

# Convert to date format
air$date <- as.Date(air$date)
water$date <- as.Date(water$date)


## Merge air and water
aw <- merge(air, water, by = c("location","date"))
aw$location <- as.factor(aw$location)
aw <- aw %>% mutate(year = year(date), julian = yday(date))
loc_seq=levels(aw$location) # get location sequence


## Check if there are any duplicates
duplicates <- aw %>%
  group_by(location, date) %>%
  filter(n() > 1) # should be NA...




#### Expand to full times series data ####
aw.full <- aw %>%
  group_by(location, year) %>%
  summarise(date = seq(min(date), max(date), by = "day"), .groups = "drop") %>%
  left_join(aw, by = c("location", "year", "date"))




#### Data cleaning ####
###=== Outlier check ===
daily_stats <- aw.full %>%
  group_by(location, julian) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            .groups = "drop")

aw.full <- aw.full %>%
  left_join(daily_stats, by = c("location", "julian")) 

# Check and remove outlier beyond 3sd
aw.full <- aw.full %>%
  mutate(is_outlier = !is.na(temp) & (temp < mean_temp - 3 * sd_temp | temp > mean_temp + 3 * sd_temp),
         temp = ifelse(is_outlier, NA, temp))

# Check how many are removed
aw.full %>%summarise(outliers = sum(is_outlier, na.rm = TRUE))


##=== Imput Check ===
# Need to calculate the rolling mean for each location separately...
aw.full <- aw.full %>%
  group_by(location, year) %>%
  mutate(rolling_sd = rollapply(temp, width = 7, FUN = sd, fill = NA, align = "center"),
         temp = ifelse(!is.na(rolling_sd) & rolling_sd < 0.01, NA, temp)) %>%
  ungroup()

# Check results - how many imputed values are in each location
aw.full %>% group_by(location) %>% summarise(na_count = sum(is.na(temp)))


###=== Calculate lagged air temp and mean cumulative ===
aw.full <- aw.full %>%
  arrange(location, year, date) %>% 
  group_by(location, year) %>%
  mutate(dmean_1 = lag(mean_air, 1),
         dmean_2 = lag(mean_air, 2),
         dmean_3 = lag(mean_air, 3),
         dmean_4 = lag(mean_air, 4),
         dmean_5 = lag(mean_air, 5),
         dmean_6 = lag(mean_air, 6),
         cair = rowMeans(cbind(mean_air, dmean_1, dmean_2, dmean_3,
                               dmean_4, dmean_5, dmean_6),
                         na.rm = FALSE)) %>% 
  ungroup()


###=== Get final master temp ===
master.temp <- aw.full[complete.cases(aw.full[, c("mean_air", "temp", "dmean_1",
                                                  "dmean_2", "dmean_3", "dmean_4",
                                                  "dmean_5", "dmean_6", "cair")]), ] %>%
  dplyr::select(location, year, date, julian, water = temp, air = mean_air,
                dmean_1, dmean_2, dmean_3, dmean_4, dmean_5, dmean_6, cair)




#### Select complete years ####
northern_sites <- c("salmontrout", "stlouis")
southern_sites <- setdiff(loc_seq, northern_sites)


## Filter for selected period
master.temp.grow <- master.temp %>%
  filter((location %in% southern_sites & julian >= 92 & julian <= 203) |
         (location %in% northern_sites & julian >= 120 & julian <= 203))


## Get records count and complete years
record_counts <- master.temp.grow %>%
  group_by(location, year) %>%
  summarise(n_days = n(), .groups = "drop") %>%
  mutate(expected_days = case_when(
    location %in% southern_sites ~ 203 - 92 + 1,    # 112 days
    location %in% northern_sites ~ 203 - 120 + 1    # 84 days
  ),
  complete = n_days == expected_days)

complete_years <- record_counts %>% filter(complete == TRUE)


sampled_years <- read.csv("subsampled_years.csv")

#### Subsample 5 years ####
## Randonly subsample 5 years for each location year group
sampled_years <- complete_years %>%
  group_by(location) %>%
  sample_n(5) %>%
  ungroup()


master.temp.subset <- master.temp.grow %>%
  inner_join(sampled_years, by = c("location", "year"))
# here we have 5320 records, which is (8 * 112 + 2 * 84) * 5.


## Export
write.csv(sampled_years, "subsampled_years.csv")
write.csv(master.temp.subset, "subsampled final combined temperature.csv")

