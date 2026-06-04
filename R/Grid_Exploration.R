### Summarize pollinator observations in grid cells across Bioregion NA24
### Marina Marquis

########################################################################################################### 

# Load packages
library(tidyverse)
library(sf)
library(ggplot2)

# Read in data  
filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map 
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of region (no grids)

########################################################################################################### 


# How many species? 1060
length(unique(filtered_5$species))

# How many species per grid cell?
species_num <- filtered_5 %>%
  group_by (grid_id)%>%
  summarise(species_n = n_distinct(species))
species_num

# Average species per grid cell: 7.233656
mean(species_num$species_n)


# Max/min species in a single grid cell: 
max(species_num$species_n) #237
min(species_num$species_n) #1

# Number of grids: 826
length(unique(species_num$grid_id))


# How many grid cells per species?  
grid_per_spec <- filtered_5 %>%
  group_by (species)%>%
  summarise(grid_per_spec = n_distinct(grid_id))
grid_per_spec



########################################################################################################### 
### Map Data

# Join data with grids, then make it an sf object
map_grids <- species_num %>%
  left_join(., grids_5, by="grid_id") %>%  #grids_5 is an st 
  st_as_sf() #turn st into sf 


# Plot it 
ggplot()+
  geom_sf(data=map_grids, aes(fill=log10(species_n)))+ #add grids 
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) + 
  theme_bw()+
  labs(x="Number of Species per Grid Cell")

# Save as png
ggsave("Figures/number_species_per_grid.png", width=5, height=6, units="in")


