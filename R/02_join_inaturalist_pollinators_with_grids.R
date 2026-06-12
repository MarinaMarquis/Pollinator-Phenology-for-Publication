# Link Observations to Grids 

#   R script to join iNaturalist pollinators with 5 x 5 km grids


############################################################################################################

# Load Packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Read in data 

# Read in geoJSON of eco-region NA24
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson")

# Read in iNat data
inat_pollinators <- readRDS("Data/iNaturalist_pollinator_observations.rds")


############################################################################################################


# Convert pollinators to SF object
pollinators_sf <- inat_pollinators %>%
  st_as_sf(coords=c("decimalLongitude", "decimalLatitude"), crs=4326)

# Filter points that fall inside the NA24 eco-region polygon. We're doing this because the GBIF download 
# was filtered by an NA24 bounding box that exceeds the size of the NA24 polygon. Our data frame therefore
# includes observations that fall outside of our area of interest. 
pollinators_sf <- pollinators_sf %>%
  st_filter(five_km_grids, .predicate = st_within)

# Join iNaturalist data with each grid cell
pollinators_grids <- st_join(pollinators_sf, five_km_grids, join = st_within)

# Test to make sure it worked
random_grid <- five_km_grids %>%
  dplyr::filter(grid_id==3010)

plot(random_grid)

random_grid_points <- pollinators_grids %>%
  dplyr::filter(grid_id==3010)

ggplot()+
  geom_sf(data=random_grid)+
  geom_sf(data=random_grid_points)

# Save it as an rds file 
saveRDS(pollinators_grids, "Data/pollinators_joined_with_grids_5.rds")
