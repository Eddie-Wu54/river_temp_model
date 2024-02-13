#' This script
#' 
#' 1. cleans the water temperature data from different river tributaries.
#' 2. Superimpose them on the plot obtained from the global water temperature model.
#' 3. Calculate the Root Mean Square Error (RMSE) between the predicted value and
#' the observed value.


library(ggplot2)
library(dplyr)


WT <- read.csv("water temperature clean/water_temperature_final_clean.csv",
               stringsAsFactors = TRUE)

WT$year <- as.factor(WT$year)
str(WT)


#### Define the fuctions ####

## Plot the modeled data and field observations
plot_temp <- function(water.pred, water.field) {
  
  ggplot(water.pred, aes(x = weeks, y = temperature.avg))+
    geom_line(size = 1.5)+
    geom_line(aes(x = weeks, y = lower.CI), size = 1, alpha = 0.5)+
    geom_line(aes(x = weeks, y = upper.CI), size = 1, alpha = 0.5)+
    geom_line(aes(x = weeks, y = max), size = 1, alpha = 0.4, color = "blue")+
    geom_line(aes(x = weeks, y = min), size = 1, alpha = 0.4, color = "blue")+
    geom_line(water.field, mapping = aes(x = weeks, y = temp, color = year))+
    scale_colour_hue()+
    theme_bw()
}

## Calculate the RMSE
calculate_rmse <- function(water.pred, water.field) {
  
  # Remove the observations from non-complete years
  water.field.c <- water.field[water.field$complete == 1,]
  
  # Get the unique levels of the "year" variable
  unique_year <- unique(water.field.c$year)

  # Create a vector list to store the results
  rmse_list <- vector("numeric", 0)
  
  # Loop through each level and calculate the RMSE
  for (year.number in unique_year) {
    sub <- subset(water.field.c, year == year.number) # create subsets
    value <- sqrt(mean((sub$temp - water.pred$temperature.avg)^2))
    rmse_list[year.number] <- value
  }
  
  return(rmse_list)
}


#### St Louis River (2011-2014) ####

# observed values
st.louis.field <- WT %>% filter(river == "stlouis")
str(st.louis.field)

# predicted values
st.louis.pred <- read.csv("water temperature clean/st_louis_model.csv")

# Plotting
plot_temp(st.louis.pred, st.louis.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(st.louis.pred, st.louis.field)


#### Saginaw River ####

# observed values
saginaw.field <- WT %>% filter(river == "saginaw")

# predicted values
saginaw.pred <- read.csv("water temperature clean/saginaw_model.csv")

# Plotting
plot_temp(saginaw.pred, saginaw.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(saginaw.pred, saginaw.field)



#### Fox River ####

# observed values
fox.field <- WT %>% filter(river == "fox")

# predicted values
fox.pred <- read.csv("water temperature clean/fox_model.csv")

# Plotting
plot_temp(fox.pred, fox.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(fox.pred, fox.field)



#### Portage-Burns Waterways ####

# observed values
pb.field <- WT %>% filter(river == "portage")
str(pb.field)

# predicted values
pb.pred <- read.csv("water temperature clean/pb_model.csv")

# Plotting
plot_temp(pb.pred, pb.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(pb.pred, pb.field)



#### Vermilion River (2012 - 2014) ####

# observed values
vermilion.field <- WT %>% filter(river == "vermilion")
str(vermilion.field)

# predicted values
vermilion.pred <- read.csv("water temperature clean/vermilion_model.csv")

# Plotting
plot_temp(vermilion.pred, vermilion.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(vermilion.pred, vermilion.field)



#### Genesee River ####

# observed values
genesee.field <- WT %>% filter(river == "genesee")
str(genesee.field)

# predicted values
genesee.pred <- read.csv("water temperature clean/genesee_model.csv")

# Plotting
plot_temp(genesee.pred, genesee.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(genesee.pred, genesee.field)



#### Big Creek River ####

# observed values
big.creek.field <- WT %>% filter(river == "big creek")
str(big.creek.field)

# predicted values
big.creek.pred <- read.csv("water temperature clean/bigcreek_model.csv")

# Plotting
plot_temp(big.creek.pred, big.creek.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(big.creek.pred, big.creek.field)



#### Big Otter River ####

# observed values
big.otter.field <- WT %>% filter(river == "big otter")
str(big.otter.field)

# predicted values
big.otter.pred <- read.csv("water temperature clean/bigotter_model.csv")

# Plotting
plot_temp(big.otter.pred, big.otter.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(big.otter.pred, big.otter.field)



#### Still River ####

# observed values
still.field <- WT %>% filter(river == "still")
str(still.field)

# predicted values
still.pred <- read.csv("water temperature clean/still_model.csv")

# Plotting
plot_temp(still.pred, still.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(still.pred, still.field)



#### Mississagi River ####

# observed values
mississagi.field <- WT %>% filter(river == "mississagi")
str(mississagi.field)

# predicted values
mississagi.pred <- read.csv("water temperature clean/mississagi_model.csv")

# Plotting
plot_temp(mississagi.pred, mississagi.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(mississagi.pred, mississagi.field)



#### Nipigon River ####

# observed values
nipigon.field <- WT %>% filter(river == "nipigon")
str(nipigon.field)

# predicted values
nipigon.pred <- read.csv("water temperature clean/nipigon_model.csv")

# Plotting
plot_temp(nipigon.pred, nipigon.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(nipigon.pred, nipigon.field)



#### Humber River ####

# observed values
humber.field <- WT %>% filter(river == "humber")
str(humber.field)

# predicted values
humber.pred <- read.csv("water temperature clean/humber_model.csv")

# Plotting
plot_temp(humber.pred, humber.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(humber.pred, humber.field)



#### Port Dover ####

# observed values
portdover.field <- WT %>% filter(river == "portdover")
str(portdover.field)

# predicted values
portdover.pred <- read.csv("water temperature clean/portdover_model.csv")

# Plotting
plot_temp(portdover.pred, portdover.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(portdover.pred, portdover.field)



#### Long Point (Inner Bay) ####

# observed values
lp.field <- WT %>% filter(river == "longpoint")
str(lp.field)

# predicted values
lp.pred <- read.csv("water temperature clean/lp_model.csv")

# Plotting
plot_temp(lp.pred, lp.field)

# Calculate the Root Mean Square Error (RMSE)
calculate_rmse(lp.pred, lp.field)


