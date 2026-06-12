# Supplemental Analysis: Spatial resampling sensitivity analysis

# In this script, we assess if the spatial bias towards human-modified areas is affecting the results,
# by subsampling so that grids have an even sample of GMHI values

# Load Packages 
library(readr)
library(tidyverse)
library(mgcv)
library(broom)
library(sf)
library(MuMIn)
library(patchwork)
library(gratia)
set.seed(120)

# Read in data 
fp_data <- readRDS("Data/phenology_estimates_data_for_analysis.rds") #phenology estimate data
climate <- read.csv("Data/Spatial Data/Climate_Data/climate_summarized.csv")
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #geoJSON of 

# add climate data to fp_data
fp_data <- left_join(fp_data, climate %>% select(grid_id, temp, prcp), by=c("grid"="grid_id"))
#NA24
filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data, reading this in 
#so we can see how many observations we used for analysis after all of the filtering 

# Now get the mean latitude and longitude for each grid
grids_centroids <- five_km_grids %>%
  st_centroid() %>%                                 
  mutate(lon = st_coordinates(.)[, 1],             
         lat = st_coordinates(.)[, 2]) %>%        
  st_drop_geometry() %>%                            
  select(grid_id, lon, lat) %>%
  group_by(grid_id) %>%
  summarise(lon = first(lon),
            lat = first(lat),
            .groups = "drop")

# Now add the mean lon and lat to our fp_data
fp_data <- left_join(fp_data, grids_centroids, by=c("grid"="grid_id"))%>%
  mutate(species = as.factor(species))

# Subsample to even GHMI values -------------------------------------------

# examine the distribution of GMHI values
hist(fp_data$mean_GHMI)

quantile(fp_data$mean_GHMI, p=c(0.05, 0.95))

# resample so there is an even number of data per 0.2 inerval of GHMI index

# create GHMI bins
fp_data_binned <- fp_data %>%
  mutate(GHMI_bin = cut(mean_GHMI,
                        breaks = seq(0, 1, by = 0.2),
                        include.lowest = TRUE,
                        right = FALSE))

# find minimum bin size
min_n <- fp_data_binned %>%
  count(GHMI_bin) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

# resample
fp_data_balanced <- fp_data_binned %>%
  group_by(GHMI_bin) %>%
  slice_sample(n = min_n) %>%
  ungroup()

hist(fp_data_balanced$mean_GHMI)


# Run the full GAM models without resampling -------------------------------------------------

# duration
gam_1 <- gam(duration ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
               s(lat, lon, k = 170, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1) #GHMI is a sig. predictor of duration (p=0.0147), as is species (<2e-16) and lat/long(<2e-16)
gam.check(gam_1)
gam.check(gam_1)$k.check

# offset
gam_1_off <- gam(offset ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
                   s(lat, lon, k = 170, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data)
summary(gam_1_off) 
gam.check(gam_1_off)

# onset
gam_1_on <- gam(onset ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_1_on)
gam.check(gam_1_on)

# Run the full GAM models with resampling -------------------------------------------------

# duration
gam_1_rs <- gam(duration ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
               s(lat, lon, k = 100, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data_balanced)
summary(gam_1_rs) #GHMI is a sig. predictor of duration (p=0.0147), as is species (<2e-16) and lat/long(<2e-16)
gam.check(gam_1_rs)

# offset
gam_1_off_rs <- gam(offset ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
                   s(lat, lon, k = 100, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data_balanced)
summary(gam_1_off_rs) 
gam.check(gam_1_off_rs)

# onset
gam_1_on_rs <- gam(onset ~ mean_GHMI + s(temp, k=20) + s(prcp, k=20) +
                  s(lat, lon, k = 100, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data_balanced)
summary(gam_1_on_rs)
gam.check(gam_1_on_rs)

