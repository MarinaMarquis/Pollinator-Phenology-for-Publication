######Figures for Pollinator Phenology Project 
#### Marina Marquis 

#Packages: 
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(sf)
library(patchwork)

select <- dplyr::select

#Read in data: 
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI for each grid 
climate <- read.csv("Data/Spatial Data/Climate_Data/climate_summarized.csv")
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of bioregion NA24 (no grids)
GHMI <- read.csv("Data/Spatial Data/GHMI/mean_gHM.csv") #mean GHMI per grid of bioregion NA24
species_gam <- read.csv("Data/GAM_results/gam_results_by_species.csv")
pop_den <- read.csv("Data/Spatial Data/Population_Density/mean_pop_density.csv")
#data frame of GAM outputs so that we have a final species list that was used for analysis  

#Merge them into one data set with observations and landsat variables: 
observations_with_landsat_variables <- filtered_5 %>%
  left_join(filtered_5_with_landsat %>%
              select(grid_id, mean_GHMI = mean), by = "grid_id")

#only retain necessary columns 
observations_with_landsat_variables <- observations_with_landsat_variables %>%
  select(grid_id, mean_GHMI, species, order, family, genus, verbatimScientificName, 
         eventDate, day, month, year)

#Filter data frame to only include species that will be used in final analysis: 
species_list <- unique(species_gam$species)

observations_with_landsat_variables <- observations_with_landsat_variables %>%
  filter(species %in% species_list)

#Check that it worked
unique(observations_with_landsat_variables$species)

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
obs_over_months <- observations_with_landsat_variables %>%
  group_by(month, order)%>%
  summarise (obs = n()) %>%
  arrange(desc(obs))%>%
  print(n=48)
#most Lepidopterans were observed in July, August, and September
#most Hymenopterans were observed in September 
#most Coleopterans were observed in May and June 
#most Dipterans were observed in June 

##########################################################################################################################


############################################## Figure 1: Frequency of Dione vanillae 
############################################## Occurrences Over Time (2008-2024) in grid 1656

#Subset grid 1656
grid1656 <- observations_with_landsat_variables%>%
  filter(grid_id==1656)
unique(filtered_5$species)


# Group counts by species, months, and years 
counts <- grid1656 %>%
  group_by(species, year, month) %>%
  filter(species=="Dione vanillae") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(counts, aes(x = month, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Dione vanillae Occurrences Over Time (2008-2024) in grid 1656",
       x = "Month",
       y = "Frequency",
       fill = "Species") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()

ggsave("Figures/frequency_Dione_vanillae_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 2: Frequency of Bombus impatiens
############################################## Occurrences Over Time (2008-2024) in grid 1656


B_impatiens_1656 <- grid1656 %>%
  group_by(species, year, month) %>%
  filter(species=="Bombus impatiens") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(B_impatiens_1656, aes(x = month, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Bombus impatiens Occurrences Over Time (2008-2024) in grid 1656",
       x = "Month",
       y = "Frequency",
       fill = "Species") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()

ggsave("Figures/frequency_Bombus_impatiens_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 3: Frequency of Phoebis sennae
############################################## Occurrences Over Time (2008-2024) in grid 1656

P_sennae_1656 <- grid1656 %>%
  group_by(species, year, month) %>%
  filter(species=="Phoebis sennae") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(P_sennae_1656, aes(x = month, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Phoebis sennae Occurrences Over Time (2008-2024) in grid 1656",
       x = "Month",
       y = "Frequency",
       fill = "Species") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()
ggsave("Figures/frequency_Phoebis_sennae_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 4: Frequency of Epargyreus clarus
############################################## Occurrences Over Time (2008-2024) in grid 1656

E_clarus_1656 <- grid1656 %>%
  group_by(species, year, month) %>%
  filter(species=="Epargyreus clarus") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(E_clarus_1656, aes(x = month, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Epargyreus clarus Occurrences Over Time (2008-2024) in grid 1656",
       x = "Month",
       y = "Frequency",
       fill = "Species") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()
ggsave("Figures/frequency_Epargyreus_clarus_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 5: Frequency of Hylephila phyleus
############################################## Occurrences Over Time (2008-2024) in grid 7

H_phyleus_1656 <- grid1656 %>%
  group_by(species, year, month) %>%
  filter(species=="Hylephila phyleus") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(H_phyleus_1656, aes(x = month, y = frequency, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Hylephila phyleus Occurrences Over Time (2008-2024) in grid 1656",
       x = "Month",
       y = "Frequency",
       fill = "Species") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()
ggsave("Figures/frequency_Hylephila_phyleus_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 6: Frequency of all species
############################################## Occurrences Over Time (2008-2024) in grid 1656

#Let's look at all species in grid 4145

#plot it 
grid1656 %>%
  filter(!is.na(eventDate)) %>%  # Remove rows with missing eventDate
  mutate(day_of_year = as.integer(yday(eventDate))) %>%  # Calculate day_of_year
  ggplot(aes(x = day_of_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Observation Frequency Throughout the Year in Grid 1656",
       x = "Day of Year",
       y = "Frequency") +
  facet_wrap(~species, scales = "fixed") + # Facet by species with fixed y scales
  ylim(0, 8)
ggsave("Figures/frequency_all_species_observations_over_time_grid_1656.png", width=6, height=6, units="in")


############################################## Figure 7: Frequency of Bombus impatiens
############################################## Occurrences Over Time (2008-2024), across all grids

#Bombus impatiens
Bombus_impatiens <- observations_with_landsat_variables %>%
  group_by(species, year, month) %>%
  filter(species=="Bombus impatiens") %>%
  summarize(frequency = n(), .groups = 'drop')

# Plot it
ggplot(Bombus_impatiens, aes(x = month, y = frequency)) +
  geom_bar(stat = "identity", position = "dodge") +  # Positioning bars side by side
  labs(title = "Frequency of Bombus impatiens Occurrences Over Time (2008-2024)",
       x = "Months",
       y = "Frequency") +
  scale_x_discrete(limits = month.abb) +  # Ensure month order
  theme_minimal()
ggsave("Figures/frequency_Bombus_impatiens_observations_over_time_all_grids.png", width=6, height=6, units="in")




############################################## Figure 8: Frequency of Urbanus proteus
############################################## Occurrences Over Time (2008-2024), across all grids

observations_with_landsat_variables %>%
  filter(!is.na(eventDate)) %>%  # Remove rows with missing eventDate
  filter(species == "Urbanus proteus")%>%
  mutate(day_of_year = as.integer(yday(eventDate))) %>%  # Calculate day_of_year
  ggplot(aes(x = day_of_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Frequency of Urbanus proteus Occurrences Over Time (2008-2024)",
       x = "Day of Year",
       y = "Frequency") +
  theme(
    axis.line.y = element_line(color = "black", linewidth = 0.5),
    axis.line.x = element_line(color = "black", linewidth = 0.5) 
  )


ggsave("Figures/frequency_Urbanus_proteus_observations_over_time_all_grids.png", width=6, height=6, units="in")




############################################## Figure 9: Observation Frequency Throughout 
############################################## the Year by family

observations_with_landsat_variables %>%
  filter(!is.na(eventDate)) %>%  # Remove rows with missing eventDate
  mutate(day_of_year = as.integer(yday(eventDate))) %>%  # Calculate day_of_year
  ggplot(aes(x = day_of_year)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Observation Frequency Throughout the Year by Family",
       x = "Day of Year",
       y = "Frequency") +
  facet_wrap(~family, scales = "free_y")

ggsave("Figures/observation_frequency_over_time_by_family.png", width=6, height=6, units="in")


############################################## Figure 10: Observation Frequency Throughout 
############################################## the Year by order

#stacking it by order (family too big)
observations_with_landsat_variables %>%
  filter(!is.na(eventDate)) %>%  # Remove rows with missing eventDate
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    order = factor(order)  # Ensure 'order is a factor
  ) %>%
  ggplot(aes(x = day_of_year, fill = order)) +  # Fill by 'family'
  geom_bar(stat = "count", position = "stack") +  # Stacked bars
  scale_fill_brewer(palette = "Set3") +  # Set color palette for families
  theme_minimal() +
  labs(
    title = "Observation Frequency Throughout the Year by Order",
    x = "Day of Year",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggsave("Figures/observation_frequency_over_time_by_order.png", width=6, height=6, units="in")




############################################## Figure 11: Observation Frequency of Lepidoptera
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
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 10)
  )

ggsave("Figures/Lepodoptera_Observations_in_Low_and_High_GHMI.png", width=6, height=6, units="in")







############################################## Figure 12: Observation Frequency of Hymenoptera
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
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 10)
  )

ggsave("Figures/Hymenoptera_Observations_in_Low_and_High_GHMI.png", width=6, height=6, units="in")







############################################## Figure 13: Observation Frequency of Coleoptera
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
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 10)
  )

ggsave("Figures/Coleoptera_Observations_in_Low_and_High_GHMI.png", width=6, height=6, units="in")






############################################## Figure 14: Observation Frequency of Diptera
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
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size = 10)
  )

ggsave("Figures/Diptera_Observations_in_Low_and_High_GHMI.png", width=6, height=6, units="in")







############################################## Figure 15: Observation Frequency of Dione vanillae
############################################## in Low versus High GHMI areas throughout the Year

D.v <- observations_with_landsat_variables %>%
  filter(species == "Dione vanillae") %>%
  filter((mean_GHMI >= 0 & mean_GHMI <= 0.3) | (mean_GHMI >= 0.7 & mean_GHMI <= 1)) 

D.v <- D.v %>%
  mutate(
    day_of_year = as.integer(yday(eventDate)),  # Calculate day_of_year
    GHMI_range = case_when(
      mean_GHMI >= 0 & mean_GHMI <= 0.3 ~ "Range 0-0.3",  # Group for mean in [0, 0.3]
      mean_GHMI >= 0.7 & mean_GHMI <= 1 ~ "Range 0.7-1",  # Group for mean in [0.7, 1]
      TRUE ~ "Other"  # Optionally, classify other values if any (though they shouldn't be in this case)
    )
  )

ggplot(D.v, aes(x = day_of_year, fill = GHMI_range)) +
  geom_bar(stat = "count", position = "dodge") +  # Position bars side by side for GHMI ranges
  scale_fill_brewer(palette = "Set1") +  
  theme_minimal(base_size=16) +
  labs(
    # title = "Dione vanillae Observations by GHMI Value (Day of Year)",
    x = "Day of Year",
    y = "Frequency",
    fill = "GHMI Range"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#peak of activity shifted earlier in the year in more urbanized areas 

ggsave("Figures/Dione_vanillae_Observations_in_Low_and_High_GHMI.png", width=6, height=6, units="in")





############################################## Figure 16: Compare figure 11 and 12

#look at them side-by-side
ggplot(D.v, aes(x = day_of_year)) +  
  geom_bar(stat = "count", color = "black", fill = "skyblue", alpha = 0.7) +  # Simple filled bars
  facet_wrap(~ GHMI_range, scales = "free_y") +  # Separate plots for each GHMI range
  theme_minimal() +
  labs(
    title = "Dione vanillae Observations by GHMI Range (Day of Year)",
    x = "Day of Year",
    y = "Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("Figures/Dione_vanillae_Observations_in_Low_and_High_GHMI_two_figures.png", width=6, height=6, units="in")




############################################## Figure 17: Map of Bioregion NA24 with number of species in each grid 
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






############################################## Figure 18: Map of Bioregion NA24 with number of observations in 
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






############################################## Figure 19: Map of GHMI across grid cells of Bioregion NA24 
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

############################################## Figure 20-23: Number of observations in gridded map of 
#                                              NA24 of 4 sample species 

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

ggplot(Bombus_impatiens) +
  geom_sf(aes(fill = log10(obs_n)), color = NA) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Bombus impatiens Observations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
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
ggsave("Figures/Bombus_impatiens_observations_across_grids.png", width = 6, height = 6, units = "in", bg = "transparent")



#Papilio glaucus,
Papilio_glaucus <- grid_with_obs_count_per_species %>%
  filter(species == "Papilio glaucus")

ggplot(Papilio_glaucus) +
  geom_sf(aes(fill = log10(obs_n)), color = NA) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Papilio glaucus, Observations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
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
ggsave("Figures/Papilio_glaucus_observations_across_grids.png", width = 6, height = 6, units = "in", bg = "transparent")


#Xylocopa virginica
Xylocopa_virginica <- grid_with_obs_count_per_species %>%
  filter(species == "Xylocopa virginica")

ggplot(Xylocopa_virginica) +
  geom_sf(aes(fill = log10(obs_n)), color = NA) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Xylocopa virginica Observations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
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
ggsave("Figures/Xylocopa_virginica_observations_across_grids.png", width = 6, height = 6, units = "in", bg = "transparent")





#Apis mellifera
Apis_mellifera <- grid_with_obs_count_per_species %>%
  filter(species == "Apis mellifera")

ggplot(Apis_mellifera) +
  geom_sf(aes(fill = log10(obs_n)), color = NA) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(option = "plasma", na.value = NA) +
  labs(
    title = "Number of Apis mellifera Observations per Grid Cell",
    fill = "log10(Count)",
    x = NULL,
    y = NULL
  ) +
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
ggsave("Figures/Apis_mellifera_observations_across_grids.png", width = 6, height = 6, units = "in", bg = "transparent")





############################################## Figure 24: quick histogram of the available GHMI values
#                                              available in Bioregion NA24

# Quick visualization 
ggplot(GHMI, aes(x = mean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size = 14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in Bioregion NA24")
# Save it 
ggsave("Figures/distribution_of_GHMI_values_in_Bioregion_NA24.png", width=6, height=6, units="in")

############################################## Figure 25: examine change in population over time by grid cell

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
