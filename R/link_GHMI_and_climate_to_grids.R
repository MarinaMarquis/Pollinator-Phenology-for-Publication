#Marina Marquis
#Making a data frame with mean GHMI value for each grid in the data set  

#############################################################################################################

# Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Read in data 
GHMI <- read.csv("Data/Spatial Data/GHMI/mean_gHM.csv")
climate <- read.csv("Data/Spatial Data/Climate_Data/climate.csv")
filtered_grids_with_observations <- readRDS("Data/filtered_5_up.rds")


#############################################################################################################

### We only want the unique grids from filtered_5 (no repeats) so that we can use this 
#   data frame in the future to simply match observations to grids to landsat variables 

# Combine these data frames by grid id

filtered_grids_with_observations <- filtered_grids_with_observations %>%
  select(grid_id) %>% 
  distinct() 

merged_df <- merge(filtered_grids_with_observations, GHMI[, c("grid_id", "mean")], by = "grid_id", all.x = TRUE)

# get average climatic values before merging data
climate_summarized <- climate %>%
  group_by(grid_id) %>%
  summarise(
    prcp = mean(prcp, na.rm = TRUE),
    temp_min = mean(tmin, na.rm = TRUE),
    temp_max = mean(tmax, na.rm = TRUE),
    temp = mean(c(tmin, tmax), na.rm = TRUE)
  ) %>%
  select(grid_id, temp, prcp)

write.csv(climate_summarized, "Data/Spatial Data/Climate Data/climate_summarized.csv")

merged_df_final <- merge(merged_df, climate_summarized, by = "grid_id", all.x = TRUE)

# Export
write.csv(merged_df_final, "Data/filtered_5_with_GHMI.csv")


