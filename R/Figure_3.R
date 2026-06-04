# Figure 3: Map of Data
# 7 Oct 2025

library(tidyverse)
library(sf)
library(biscale)
library(cowplot)
library(viridis)
library(RStoolbox)
library(terra)
library(prettymapr)
library(ggspatial)

# read in data 
observations_with_landsat_variables <- readRDS("Data/observations_with_landsat_variables.rds") 
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map

#Summarize number of species in each grid cell 
species_num <- observations_with_landsat_variables %>%
  group_by (grid_id)%>%
  summarise(species_n = n_distinct(species))

#Join with grids_5 to obtain the geometry column associated with unique grids 
grid_with_species <- grids_5 %>%
  left_join(species_num, by = "grid_id")

# get statellite map to add to the plot
bbox_orig <- st_bbox(grid_with_species)

# Expand by 1 degrees
buffer <- 1
bbox_expanded <- bbox_orig
bbox_expanded["xmin"] <- bbox_expanded["xmin"] - buffer
bbox_expanded["ymin"] <- bbox_expanded["ymin"] - buffer
bbox_expanded["xmax"] <- bbox_expanded["xmax"] + buffer
bbox_expanded["ymax"] <- bbox_expanded["ymax"] + buffer

# Convert numeric bbox to sfc polygon 
bbox_sfc <- st_as_sfc(bbox_expanded)

sat_map <- get_tiles(bbox_sfc, zoom=7, provider = "Esri.WorldImagery", crop = TRUE)

ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data = NA_24, color = "black", fill = "white", linewidth = 0.8, alpha=0.5) + 
  geom_sf(data=grid_with_species, aes(fill=species_n), color = NA)+ #add grids without grid outline 
  scale_fill_viridis_c(option = "plasma", na.value = NA, trans="log10") +  # leave cells with no species number white 
  annotation_scale(location="bl", width_hint=0.1,
                   pad_x=unit(6.2,"in"), pad_y=unit(0.45,"in")) +
  annotation_north_arrow(location="bl", which_north="true",
                         pad_x=unit(6.3, "in"), pad_y=unit(0.6,"in"),
                         style=north_arrow_fancy_orienteering) +
  theme_bw()+
  labs(
    title = "Number of Species per Grid Cell",
    fill = "Species Count",
    x = NULL,
    y = NULL
  )+
  theme_minimal(base_size = 18) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

ggsave("Figures/map_of_species_per_grid_cell.png", width=6.27, height=6.27, units="in")

############################################## Example Species 

library(maptiles)

# create bounding box to use as an example
bbox <- c(xmin = -84.8, ymin = 33.02802, xmax = -83.5, ymax = 34.60635)

#Look at species with most observations from the data frame that was created earlier in this script 
obs

#We will use the top 4 species with the most observations as example species. Listing them for filtering here:
example_species <- c("Bombus impatiens", "Papilio glaucus", "Xylocopa virginica", "Apis mellifera")

#Summarize for each species, how many observations are found in each grid
obs_per_spec <- observations_with_landsat_variables %>%
  filter(species %in% example_species) %>%
  group_by(species, grid_id) %>%
  summarise(obs_n = n(), .groups = "drop")
obs_per_spec

#Join with grids_5 to obtain the geometry column associated with unique grids 
grid_with_obs_count_per_species <- grids_5 %>%
  left_join(obs_per_spec, by = "grid_id")%>%
  filter(species %in% example_species & !is.na(obs_n))  # filter out NA rows

#Plot observations count (across Bioregion NA24 grids) of each of the following species: Bombus impatiens, Papilio glaucus,
#Xylocopa virginica, and Apis mellifera


#Bombus impatiens
Bombus_impatiens <- grid_with_obs_count_per_species %>%
  filter(species == "Bombus impatiens")

# Get extent 
bbox_sfc <- st_as_sfc(st_bbox(bbox, crs = st_crs(Bombus_impatiens)))
Bombus_impatiens_cropped <- st_crop(Bombus_impatiens, bbox_sfc)

sat_map <- get_tiles(bbox_sfc, zoom=10, provider = "Esri.WorldImagery", crop = TRUE)

ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data=Bombus_impatiens_cropped, aes(fill = log10(obs_n)), color = NA) +
  
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Bombus impatiens\nObservations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
ggsave("Figures/Bombus_impatiens_observations_across_grids_sat.png", width = 2.07, height = 2.07, units = "in", bg = "transparent")



#Papilio glaucus,
Papilio_glaucus <- grid_with_obs_count_per_species %>%
  filter(species == "Papilio glaucus")

# Get extent 
bbox_sfc <- st_as_sfc(st_bbox(bbox, crs = st_crs(Papilio_glaucus)))
Papilio_glaucus_cropped <- st_crop(Papilio_glaucus, bbox_sfc)

ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data= Papilio_glaucus_cropped, aes(fill = log10(obs_n)), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Papilio glaucus\nObservations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
ggsave("Figures/Papilio_glaucus_observations_across_grids.png", width = 2.07, height = 2.07, units = "in", bg = "transparent")


#Xylocopa virginica
Xylocopa_virginica <- grid_with_obs_count_per_species %>%
  filter(species == "Xylocopa virginica")

# Get extent 
bbox_sfc <- st_as_sfc(st_bbox(bbox, crs = st_crs(Xylocopa_virginica)))
Xylocopa_virginica_cropped <- st_crop(Xylocopa_virginica, bbox_sfc)

ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data= Xylocopa_virginica_cropped, aes(fill = log10(obs_n)), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Xylocopa virginica\nObservations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
ggsave("Figures/Xylocopa_virginica_observations_across_grids_sat.png", width = 2.07, height = 2.07, units = "in", bg = "transparent")

#Apis mellifera
Apis_mellifera <- grid_with_obs_count_per_species %>%
  filter(species == "Apis mellifera")

# Get extent 
bbox_sfc <- st_as_sfc(st_bbox(bbox, crs = st_crs(Apis_mellifera)))
Apis_mellifera_cropped <- st_crop(Apis_mellifera, bbox_sfc)

ggRGB(sat_map, r = 1, g = 2, b = 3) +
  geom_sf(data= Apis_mellifera_cropped, aes(fill = log10(obs_n)), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Apis mellifera\nObservations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )
ggsave("Figures/Apis_mellifera_observations_across_grids_sat.png", width = 2.07, height = 2.07, units = "in", bg = "transparent")

