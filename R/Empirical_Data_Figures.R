######Figures for Pollinator Phenology Project 
#### Marina Marquis 

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


#Read in data: 
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI for each grid 
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of bioregion NA24 (no grids)
fp_data <- readRDS("Data/final_phenology_df_for_analysis.RDS")
#final data frame that was used for in the GAMs so that our raw data can be matched to the
#species x grid combinations that were actually used for analysis



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



##########################################################################################################################

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



##########################################################################################################################




############################################## Figure 1: Observation Frequency of Lepidoptera
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







############################################## Figure 2: Observation Frequency of Hymenoptera
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







############################################## Figure 3: Observation Frequency of Coleoptera
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






############################################## Figure 4: Observation Frequency of Diptera
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






############################################## Figure 5-6: Map of Bioregion NA24 with number of species in each grid 
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






############################################## Figure 7: Map of Bioregion NA24 with number of observations in 
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






############################################## Figure 8: Map of GHMI across grid cells of Bioregion NA24 
#Make GHMI dataframe into an sf object
GHMI_merged <- grids_5 %>%
  left_join(GHMI, by = "grid_id")

#Plot it
ggplot(GHMI_merged) +
  geom_sf(aes(fill = mean)) +
  geom_sf(data = NA_24, color = NA, fill = NA) +
  scale_fill_viridis_c(name = "Mean GHMI") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24 (GHMI)")+
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

ggsave("Figures/GHMI_map_of_Bioregion_NA24.png", width=6, height=6, units="in")









############################################## Figure 9: quick histogram of the available GHMI values
#                                              available in Bioregion NA24

# Quick visualization 
ggplot(GHMI, aes(x = mean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size = 14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in Bioregion NA24")
# Save it 
ggsave("Figures/distribution_of_GHMI_values_in_Bioregion_NA24.png", width=6, height=6, units="in")













########################################################################################################################
### Figure 10: map of United States with Bioregion NA24 for spatial context 

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

########################################################################################################################









########################################################################################################################
### Figure 11: Satellite map of Bioregion NA24


# Set CRS
NA_24 <- st_transform(NA_24, 4326)

# Add buffer 
NA_24_buf <- st_buffer(NA_24, dist = 0.01)

# Download satellite imagery
sat_map <- get_tiles(
  NA_24_buf,
  provider = "Esri.WorldImagery",
  zoom = 7,
  crop = TRUE
)

# Convert to terra + mask 
sat_masked <- terra::mask(
  sat_map,
  terra::vect(NA_24_buf)
)

# Convert to data frame 
sat_df <- as.data.frame(sat_masked, xy = TRUE, na.rm = TRUE)
names(sat_df)[3:5] <- c("R", "G", "B")

# Plot it
p <- ggplot() +
  
  geom_raster(
    data = sat_df,
    aes(x = x, y = y, fill = rgb(R, G, B, maxColorValue = 255))
  ) +
  scale_fill_identity() +
  
  # NA24 outline (optional but useful for checking)
  geom_sf(
    data = st_transform(NA_24, st_crs(sat_masked)),
    fill = NA,
    color = NA
  ) +
  
  coord_sf(expand = FALSE) +
  theme_void()

p

# Save it 
ggsave(
  "Figures/NA24_satellite_cutout.png",
  plot = p,
  width = 7,
  height = 7,
  dpi = 900,
  bg = "white"
)

########################################################################################################################