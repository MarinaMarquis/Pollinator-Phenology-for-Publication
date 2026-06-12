# Create figures of empirical data

#Packages: 
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(extrafont)
library(sf)
library(ggspatial)
library(maptiles)
library(terra)
library(biscale)
library(cowplot)
library(viridis)
library(RStoolbox)
library(terra)
library(prettymapr)
library(ggspatial)

#Read in data: 
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI for each grid 
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of bioregion NA24 (no grids)
fp_data <- readRDS("Data/final_phenology_df_for_analysis.RDS")
GHMI <- read.csv("Data/Spatial Data/GHMI/mean_gHM.csv")
climate <- read.csv("Data/Spatial Data/Climate_Data/climate_summarized.csv")
pop_den <- read.csv("Data/Spatial Data/Population_Density/mean_pop_density.csv")
fp_data <- readRDS("Data/final_phenology_df_for_analysis.RDS")

#Merge them into one data set with observations and GHMI: 
observations_with_landsat_variables <- filtered_5 %>%
  left_join(filtered_5_with_landsat %>%
              select(grid_id, mean_GHMI = mean), by = "grid_id")

#Only retain necessary columns 
observations_with_landsat_variables <- observations_with_landsat_variables %>%
  select(grid_id, mean_GHMI, species, order, family, genus, verbatimScientificName, 
         eventDate, day, month, year)

#Valid species x grid combinations that were used for analysis
valid_combos <- fp_data %>%
  select(species, grid) %>%
  distinct()

#Filter our data set with raw observation data to only have species x grid combos used for analysis
observations_with_landsat_variables <- observations_with_landsat_variables %>%
  filter(species %in% valid_combos$species) %>%
  inner_join(valid_combos, by = c("species", "grid_id" = "grid"))

#Make sure it worked
nrow(distinct(observations_with_landsat_variables, species, grid_id))
nrow(valid_combos) # looks good 
setdiff(unique(observations_with_landsat_variables$species), unique(fp_data$species)) # should be empty


#Save it: 
saveRDS(observations_with_landsat_variables, "Data/observations_with_landsat_variables.rds") 
#raw data of observations, GHMI, and grid cell. Filtered to only include 
#observations of species that will be used for analysis 

############################################# Data Summary

# To get an idea of which species to look at, we're first looking at how many species in each grid 
spec_per_grid <- observations_with_landsat_variables %>%
  group_by(species)%>%
  summarise(grid_per_spec = n_distinct(grid_id))%>%
  arrange(desc(grid_per_spec))
spec_per_grid 

# We're also looking at amount of observations of each species in each grid 
obs_per_grid_each_spec <- observations_with_landsat_variables %>%
  group_by(species, grid_id)%>%
  summarise(obs_per_grid_each_spec = n())%>%
  arrange(desc(obs_per_grid_each_spec))
obs_per_grid_each_spec

# And how many observations of each species in the data set 
obs <- observations_with_landsat_variables %>%
  group_by(species) %>%
  summarise(obs = n()) %>%
  arrange(desc(obs))
obs

# Frequency of observations across the months 
obs_over_months <- observations_with_landsat_variables %>%
  group_by(month)%>%
  summarise (obs = n()) %>%
  arrange(desc(obs))%>%
  print()
#most observations from July, August, September

# Frequency of observations across the months, grouped by order 
obs_over_months_order <- observations_with_landsat_variables %>%
  group_by(month, order)%>%
  summarise (obs = n()) %>%
  arrange(desc(obs))%>%
  print(n=48)
#most Lepidopterans were observed in July, August, and September
#most Hymenopterans were observed in September 
#most Coleopterans were observed in May and June 
#most Dipterans were observed in June 


############################################## Figure 1: Map of Bioregion NA24 with number of species in each grid 
###############                                cell and example species maps

#Summarize number of species in each grid cell 
species_num <- observations_with_landsat_variables %>%
  group_by (grid_id)%>%
  summarise(species_n = n_distinct(species))

#Join with grids_5 to obtain the geometry column associated with unique grids 
grid_with_species <- grids_5 %>%
  left_join(species_num, by = "grid_id")

#Plot it 
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

# Example species 

# create bounding box to use as an example
bbox <- c(xmin = -84.8, ymin = 33.02802, xmax = -83.5, ymax = 34.60635)

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

# map of United States with Bioregion NA24 for spatial context 

# Bounding box of the United States
us_bbox <- st_as_sfc(st_bbox(c(
  xmin = -125, xmax = -66,
  ymin = 24, ymax = 50
), crs = 4326))


# Download satellite imagery
sat_map <- get_tiles(us_bbox,
                     provider = "Esri.WorldImagery",
                     zoom = 5,
                     crop = TRUE)


# Make sure the CRS of satellite imagery matches NA24
NA_24 <- st_transform(NA_24, st_crs(sat_map))

# Plot it 
ggplot() +
  layer_spatial(sat_map) +
  geom_sf(data = NA_24,
          fill = "lightgreen",
          color = "lightgreen",
          linewidth = 1.2) +
  theme_void()

# Save it 
ggsave(
  "Figures/map_of_US_and_BioregionNA24.png",
  width = 6.35,  
  height = 10.59,  
  dpi = 600
)


############################################## Map of Bioregion NA24 with number of observations in 
###############                                each grid cell 



#Summarize number of observations in each grid cell 
obs_num <- observations_with_landsat_variables %>%
  count(grid_id, name = "obs_n")

#Join with grids_5 to obtain the geometry column associated with unique grids 
grid_with_obs_count <- grids_5 %>%
  left_join(obs_num, by = "grid_id")

#Plot it 
ggplot()+
  geom_sf(data=grid_with_obs_count, aes(fill=log10(obs_n)), color = NA)+ #add grids without grid outline 
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  coord_sf(expand = FALSE) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +  # leave cells with no species number white 
  labs(
    title = "Number of Observations per Grid Cell",
    fill = "log10(Pollinator Observation Count)",
    x = NULL,
    y = NULL
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 10)
  )


ggsave("Figures/map_of_observations_per_grid_cell.png", width=6, height=6, units="in")




############################################## Map of GHMI across grid cells of Bioregion NA24 
#Make GHMI dataframe into an sf object
GHMI_merged <- grids_5 %>%
  left_join(GHMI, by = "grid_id")

#Plot it
(ghmi_plot <- ggplot(GHMI_merged) +
    geom_sf(aes(fill = mean), color = NA) +
    geom_sf(data = NA_24, color = NA, fill = NA) +
    scale_fill_viridis_c(name = "Mean GHMI") +
    theme_bw() +
    labs(title = "Anthropogenic Change Across\nBioregion NA24 (GHMI)")+
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    ))

ggsave("Figures/GHMI_map_of_Bioregion_NA24.png", width=6, height=6, units="in")


############################################## Figure 2: Map of Bioregion NA24 with number of species in each grid 
###############                                cell

#Summarize number of species in each grid cell 
species_num <- observations_with_landsat_variables %>%
  group_by (grid_id)%>%
  summarise(species_n = n_distinct(species))

#Join with grids_5 to obtain the geometry column associated with unique grids 
grid_with_species <- grids_5 %>%
  left_join(species_num, by = "grid_id")

#Plot it 
ggplot()+
  geom_sf(data=grid_with_species, aes(fill=log10(species_n)), color = NA)+ #add grids without grid outline 
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) + 
  scale_fill_viridis_c(option = "plasma", na.value = NA) +  # leave cells with no species number white 
  labs(
    title = "Number of Species per Grid Cell",
    fill = "log10(Species Count)",
    x = NULL,
    y = NULL
  )+
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 10)
  )

ggsave("Figures/map_of_species_per_grid_cell.png", width=6, height=6, units="in")


# alternative log transformation
ggplot()+
  geom_sf(data=grid_with_species, aes(fill=species_n), color = NA)+ #add grids without grid outline 
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) + 
  scale_fill_viridis_c(option = "plasma", na.value = NA, trans="log10") +  # leave cells with no species number white 
  theme_bw()+
  labs(
    title = "Number of Species per Grid Cell",
    fill = "log10(Species Count)",
    x = NULL,
    y = NULL
  )+
  theme_minimal(base_size = 14) +
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

grid_points <- grid_with_species %>%
  st_centroid()

ggplot() +
  geom_sf(data = grid_points, aes(color = log10(species_n)), size = 2) +  # Use color instead of fill
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) + 
  scale_color_viridis_c(option = "plasma", na.value = NA) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Number of Species per Grid Cell (Centroids)",
    color = "log10(Species Count)",
    x = NULL,
    y = NULL
  ) +
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

ggsave("Figures/map_of_species_per_grid_cell_centroids.png", width=6, height=6, units="in")





############################################## Figure S4: Examine change in population over time by grid cell

pop_den_long <- pop_den %>%
  pivot_longer(
    cols = starts_with("pop_den_"),
    names_to = "year",
    values_to = "pop_den"
  ) %>%
  mutate(
    year = as.numeric(gsub("pop_den_", "", year))
  ) %>%
  filter(grid_id %in% filtered_5$grid_id) # filter to only the grids used in analysis

plot_dat <- pop_den_long %>%
  filter(year %in% c(2010, 2015, 2020))

ggplot(plot_dat, aes(x = year, y = pop_den, group = grid_id)) +
  
  # grid-level lines
  geom_line(alpha = 0.15, color = "steelblue") +
  
  # mean line
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1.8,
    color = "black"
  ) +
  
  # mean points (optional but nice)
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "point",
    size = 2,
    color = "black"
  ) +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  
  theme_minimal() +
  labs(x = "Year", y = "Population Density")

ggsave("Figures/Change_in_Pop_Den_by_Year.jpeg", height=4, width=6, units="in")

summary_stats <- pop_den_long %>%
  filter(year %in% c(2010, 2015, 2020)) %>%
  group_by(year) %>%
  summarise(
    mean_pop_den = mean(pop_den, na.rm = TRUE)
  )

lm_mean <- lm(mean_pop_den ~ year, data = summary_stats)

summary(lm_mean)





############################################## Figure S7: histogram of the available GHMI values
#                                              available in Bioregion NA24

# Quick visualization 
ggplot(GHMI, aes(x = mean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size = 14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in Bioregion NA24")
# Save it 
ggsave("Figures/distribution_of_GHMI_values_in_Bioregion_NA24.png", width=6, height=6, units="in")

# Quick visualization 
ggplot(fp_data, aes(x = mean_GHMI)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size=14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in the Data Set After Filtering")

# Save it 
ggsave("Figures/distribution_of_GHMI_values_in_GAM_dataset.png", width=6, height=6, units="in")



############################################# Figure S8: Observation Frequency of Lepidoptera
############################################## in Low versus High GHMI areas throughout the Year

lepidoptera_data <- observations_with_landsat_variables %>%
  filter(order == "Lepidoptera") %>%
  filter((mean_GHMI >= 0 & mean_GHMI <= 0.3) | (mean_GHMI >= 0.7 & mean_GHMI <= 1)) 

lepidoptera_data <- lepidoptera_data %>%
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    GHMI_range = case_when(
      mean_GHMI >= 0 & mean_GHMI <= 0.3 ~ "Range 0-0.3",  # Group for mean in [0, 0.3]
      mean_GHMI >= 0.7 & mean_GHMI <= 1 ~ "Range 0.7-1",  # Group for mean in [0.7, 1]
      TRUE ~ "Other"  # Optionally, classify other values if any (though they shouldn't be in this case)
    )
  )


# Plotting the data
ggplot(lepidoptera_data, aes(x = day_of_year, fill = GHMI_range)) +
  geom_bar(stat = "count", position = "dodge") +  # Position bars side by side for GHMI ranges
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal(base_size=16) +
  labs(
    # title = "Frequency of Lepidoptera Observations in Low and High Urban Areas (GHMI) Throughout the Year",
    x = "Day of Year",
    y = "Frequency",
    fill = "GHMI Range"
  ) +
  theme(axis.text.x = element_text(hjust = 1, family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"), 
        plot.title = element_text(size = 10, family = "Times New Roman"), 
        legend.title = element_text(family = "Times New Roman"), 
        legend.text = element_text(family = "Times New Roman")
  )

ggsave("Figures/Lepidoptera_Observations_in_Low_and_High_GHMI.png", width=5.28, height=5.28, units="in")







############################################## Figure S8: Observation Frequency of Hymenoptera
############################################## in Low versus High GHMI areas throughout the Year

hymenoptera_data <- observations_with_landsat_variables %>%
  filter(order == "Hymenoptera") %>%
  filter((mean_GHMI >= 0 & mean_GHMI <= 0.3) | (mean_GHMI >= 0.7 & mean_GHMI <= 1)) 

hymenoptera_data <- hymenoptera_data %>%
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    GHMI_range = case_when(
      mean_GHMI >= 0 & mean_GHMI <= 0.3 ~ "Range 0-0.3",  # Group for mean in [0, 0.3]
      mean_GHMI >= 0.7 & mean_GHMI <= 1 ~ "Range 0.7-1",  # Group for mean in [0.7, 1]
      TRUE ~ "Other"  # Optionally, classify other values if any (though they shouldn't be in this case)
    )
  )


# Plotting the data
ggplot(hymenoptera_data, aes(x = day_of_year, fill = GHMI_range)) +
  geom_bar(stat = "count", position = "dodge") +  # Position bars side by side for GHMI ranges
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal(base_size=16) +
  labs(
    # title = "Frequency of Hymenoptera Observations in Low and High Urban Areas (GHMI) Throughout the Year",
    x = "Day of Year",
    y = "Frequency",
    fill = "GHMI Range"
  ) +
  theme(axis.text.x = element_text(hjust = 1, family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"), 
        plot.title = element_text(size = 10, family = "Times New Roman"), 
        legend.title = element_text(family = "Times New Roman"), 
        legend.text = element_text(family = "Times New Roman")
  )

ggsave("Figures/Hymenoptera_Observations_in_Low_and_High_GHMI.png", width=5.28, height=5.28, units="in")







############################################## Figure S8: Observation Frequency of Coleoptera
############################################## in Low versus High GHMI areas throughout the Year

coleoptera_data <- observations_with_landsat_variables %>%
  filter(order == "Coleoptera") %>%
  filter((mean_GHMI >= 0 & mean_GHMI <= 0.3) | (mean_GHMI >= 0.7 & mean_GHMI <= 1)) 

coleoptera_data <- coleoptera_data %>%
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    GHMI_range = case_when(
      mean_GHMI >= 0 & mean_GHMI <= 0.3 ~ "Range 0-0.3",  # Group for mean in [0, 0.3]
      mean_GHMI >= 0.7 & mean_GHMI <= 1 ~ "Range 0.7-1",  # Group for mean in [0.7, 1]
      TRUE ~ "Other"  # Optionally, classify other values if any (though they shouldn't be in this case)
    )
  )


# Plotting the data
ggplot(coleoptera_data, aes(x = day_of_year, fill = GHMI_range)) +
  geom_bar(stat = "count", position = "dodge") +  # Position bars side by side for GHMI ranges
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal(base_size=16) +
  labs(
    # title = "Frequency of Coleoptera Observations in Low and High Urban Areas (GHMI) Throughout the Year",
    x = "Day of Year",
    y = "Frequency",
    fill = "GHMI Range"
  ) +
  theme(axis.text.x = element_text(hjust = 1, family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"), 
        plot.title = element_text(size = 10, family = "Times New Roman"), 
        legend.title = element_text(family = "Times New Roman"), 
        legend.text = element_text(family = "Times New Roman")
  )

ggsave("Figures/Coleoptera_Observations_in_Low_and_High_GHMI.png", width=5.28, height=5.28, units="in")






############################################## Figure S8: Observation Frequency of Diptera
############################################## in Low versus High GHMI areas throughout the Year

diptera_data <- observations_with_landsat_variables %>%
  filter(order == "Diptera") %>%
  filter((mean_GHMI >= 0 & mean_GHMI <= 0.3) | (mean_GHMI >= 0.7 & mean_GHMI <= 1)) 

diptera_data <- diptera_data %>%
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    GHMI_range = case_when(
      mean_GHMI >= 0 & mean_GHMI <= 0.3 ~ "Range 0-0.3",  # Group for mean in [0, 0.3]
      mean_GHMI >= 0.7 & mean_GHMI <= 1 ~ "Range 0.7-1",  # Group for mean in [0.7, 1]
      TRUE ~ "Other"  # Optionally, classify other values if any (though they shouldn't be in this case)
    )
  )


# Plotting the data
ggplot(diptera_data, aes(x = day_of_year, fill = GHMI_range)) +
  geom_bar(stat = "count", position = "dodge") +  # Position bars side by side for GHMI ranges
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal(base_size=16) +
  labs(
    # title = "Frequency of Diptera Observations in Low and High Urban Areas (GHMI) Throughout the Year",
    x = "Day of Year",
    y = "Frequency",
    fill = "GHMI Range"
  ) +
  theme(axis.text.x = element_text(hjust = 1, family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"), 
        plot.title = element_text(size = 10, family = "Times New Roman"), 
        legend.title = element_text(family = "Times New Roman"), 
        legend.text = element_text(family = "Times New Roman")
  )

ggsave("Figures/Diptera_Observations_in_Low_and_High_GHMI.png", width=5.28, height=5.28, units="in")



############################################## Figure of temperature and precipitation

temp_prcp <- grids_5 %>%
  left_join(climate %>% select(grid_id, temp, prcp), by = "grid_id") 

(temp_plot <- ggplot(temp_prcp) +
    geom_sf(aes(fill = temp), color = NA) +
    geom_sf(data = NA_24, color = NA, fill = NA) +
    scale_fill_viridis_c(name = "Mean Daily\nTemperature (°C)") +
    theme_bw() +
    labs(title = "Mean Daily Temperature\nAcross Bioregion NA24")+
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    ))

(prcp_plot <- ggplot(temp_prcp) +
    geom_sf(aes(fill = prcp), color = NA) +
    geom_sf(data = NA_24, color = NA, fill = NA) +
    scale_fill_viridis_c(name = "Mean Daily\nPrecipitation (mm)") +
    theme_bw() +
    labs(title = "Mean Daily Precipitation\nAcross Bioregion NA24")+
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    ))

temp_and_prcp_plot <- temp_plot + prcp_plot
temp_and_prcp_plot

ggsave(
  "Figures/Temperature_and_Precipitation_Plot.png",
  plot = temp_and_prcp_plot,
  width = 8,
  height = 6,
  units = "in"
)

############################################## Figure of population density

GHMI_pop <- grids_5 %>%
  left_join(pop_den %>% select(grid_id, pop_den_2010, pop_den_2015, pop_den_2020), by = "grid_id") %>%
  left_join(GHMI %>% select(grid_id, mean), by = "grid_id")

cor(GHMI_pop$mean, GHMI_pop$pop_den_2020, use = "complete.obs")

(pop_den_plot <- ggplot(GHMI_pop) +
    geom_sf(aes(fill = pop_den_2020), color = NA) +
    geom_sf(data = NA_24, color = NA, fill = NA) +
    scale_fill_viridis_c(name = "Mean Population\nDensity") +
    theme_bw() +
    labs(title = "Population Density\nAcross Bioregion NA24")+
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    ))

combined_plot <- ghmi_plot + pop_den_plot

ggsave(
  "Figures/GHMI_vs_Pop_Density_Bioregion_NA24.png",
  plot = combined_plot,
  width = 8,
  height = 6,
  units = "in"
)
