###### Phenology-Specific Figures 
#### Marina Marquis
#### This script produces figures using the phenology data from the 
#   "Phenological_Estimates_by_grid_by_species.R" R script

#Packages: 
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(patchwork)
library(gridExtra)
library(grid)
library(broom)
library(purrr)
library(extrafont)

phenology_estimates_all_species_each_grid_with_landsat <- readRDS("Data/final_phenology_df_for_analysis.RDS") #phenology data used in GAMs with climate, lat/long, and GHMI
grids_5 <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #gridded map
NA_24 <- st_read("Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson") #map of bioregion NA24 (no grids)
species_gam <- read.csv("Data/GAM_results/gam_results_by_species_w_climate.csv") #individual species GAM results, select model outputs (p values, estimates, etc)
species_gam_full <- readRDS("Data/GAM_results/species_gam_full_w_climate.rds") # full GAM results (not just select model outputs)


##########################################################################################################################


############# Figure 1: looking at onset, offset, and duration for each species in a singular 
#grid cell 

# pick a grid cell
grid_ex <- phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::filter(grid==8756) %>%
  dplyr::select(1, 3:6) %>%
  pivot_longer(!species, names_to="metric", values_to="day_of_year") %>%
  dplyr::filter(metric != "median")

ggplot(grid_ex, aes(x=species, y=day_of_year, color=metric))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  ggtitle("Phenological estimates for all species in Grid #8756")

ggsave("Figures/phenology_estimates_example_for_grid_8756.png", width=6, height=6, units="in")


############# Figure 2: looking at onset, offset, and duration for each species in all 
#grid cells 

# Reshape the df to long format
phenology_long <- phenology_estimates_all_species_each_grid_with_landsat %>%
  pivot_longer(cols = c(onset, duration, offset), 
               names_to = "metric", 
               values_to = "day_of_year") %>%
  mutate(grid = factor(grid))
str(phenology_long)

# y-axis break intervals
unique_grids <- unique(phenology_long$grid)  
breaks <- unique_grids[seq(1, length(unique_grids), by = 10)]

# Plot it
ggplot(phenology_long, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("duration", "offset", "onset")
  ) +
  labs(
    title = "Onset, Offset, and Duration by Grid and Species",
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  )+
  scale_y_discrete(breaks = breaks) 

ggsave("Figures/phenology_estimates_all_species_across_all_grids.png", width=6, height=6, units="in")

#Note: grids move from south to north. So the larger the number, in general, the 
#more north the location.

 


############# Figure 3: looking at onset, offset, and duration for each species in all 
#grid cells, with each being its own graph  
ggplot(phenology_long, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("duration", "offset", "onset")
  ) +
  labs(
    title = "Onset, Offset, and Duration by Grid and Species",
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  )+
  scale_y_discrete(breaks = breaks) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) 


ggsave("Figures/phenology_estimates_all_species_across_all_grids_separate_graphs.png", width=6, height=6, units="in")




########### Figure 4: the same as figure 1, but for only Apis mellifera across all grids

#Apis mellifera 
species_name <- "Apis mellifera"  
#Only phenology columns (except for median)
metric_cols <- c("onset", "duration", "offset") 

# Apis melifera phenological estimates by grid 
phenology_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == species_name) %>%  
  select(grid, all_of(metric_cols)) %>%  # Keep only grid and metric columns
  pivot_longer(cols = all_of(metric_cols), names_to = "metric", values_to = "day_of_year")

# unique grid IDs for Apis melifera 
unique_grids <- unique(phenology_species$grid)

#intervals for the y-axis breaks: 10 
breaks <- unique_grids[seq(1, length(unique_grids), by = 10)]  

# Plot it
ggplot(phenology_species, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("Duration", "Offset", "Onset")
  ) +
  labs(
    title = paste("Onset, Offset, and Duration for", species_name, "by Grid"),
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  ) +
  scale_y_discrete(breaks = breaks)  # set breaks 

ggsave("Figures/phenology_estimates_Apis_mellifera_all_grids.png", width=6, height=6, units="in")



############## Figure 5: same thing but with Bombus impatiens

#set the name Bombus impatiens
Bombus_impatiens <- "Bombus impatiens"  

# Bombus impatiens phenological estimates by grid 
B_impatiens_estimates_by_grid <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == Bombus_impatiens) %>%  
  select(-species) %>%  # Remove the species column
  pivot_longer(cols = all_of(metric_cols), names_to = "metric", values_to = "day_of_year")


# Plot it
ggplot(B_impatiens_estimates_by_grid, aes(x = day_of_year, y = factor(grid), color = metric)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(
    values = c("onset" = "blue", "duration" = "green", "offset" = "red"),
    labels = c("Duration", "Offset", "Onset")
  ) +
  labs(
    title = paste("Onset, Offset, and Duration for", Bombus_impatiens, "by Grid"),
    x = "Day of Year",
    y = "Grid ID",
    color = "Phenological Estimates"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "gray80"),
    plot.margin = margin(10, 10, 10, 10) 
  ) 

ggsave("Figures/phenology_estimates_Bombus_impatiens_all_grids.png", width=6, height=6, units="in")


############## Figure 6: showing offset of Bombus impatiens across mapped grids 


#want only offset for Bombus impatiens
B_impatiens_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  select(species, grid, offset, mean_GHMI) %>%
  filter(species=="Bombus impatiens")
head(B_impatiens_offset) #check that it worked

#join data with grids, then make it an sf object
B_impatiens_offset_map <- B_impatiens_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# First plot  
p1 <- ggplot(B_impatiens_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Bombus impatiens")


# Second plot 
p2 <- ggplot(B_impatiens_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- p1 + p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Bombus_impatiens_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




############## Figure 7: showing offset of Papilio glaucus across mapped grids with 
#comparison to GHMI

#want only offset for Papilio glaucus
P_glaucus_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species == "Papilio glaucus", !is.na(grid), !is.na(offset)) %>%
  select(species, grid, offset, mean_GHMI)
head(P_glaucus_offset) #check that it worked


#join data with grids, then make it an sf object
P_glaucus_offset_map <- P_glaucus_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
P_glaucus_p1 <- ggplot(P_glaucus_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Median Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Papilio glaucus")

# Second plot 
P_glaucus_p2 <- ggplot(P_glaucus_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- P_glaucus_p1 + P_glaucus_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Papilio_glaucuse_Offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 8: showing offset of Lepidoptera across mapped grids with 
#comparison to GHMI


#Want only offset for Lepidoptera
Lepidoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Lepidoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Lepidoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Lepidoptera_offset_map <- Lepidoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Lepidoptera_p1 <- ggplot(Lepidoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Lepidopterans")

# Second plot 
Lepidoptera_p2 <- ggplot(Lepidoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Lepidoptera_p1 + Lepidoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Lepidoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 9: showing offset of Hymenoptera across mapped grids with 
#comparison to GHMI


#want only medians for Hymenoptera
Hymenoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Hymenoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Hymenoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Hymenoptera_offset_map <- Hymenoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Hymenoptera_p1 <- ggplot(Hymenoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Hymenopterans")

# Second plot 
Hymenoptera_p2 <- ggplot(Hymenoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Hymenoptera_p1 + Hymenoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Hymenoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




#############Figure 10: showing offset of Diptera across mapped grids with 
#comparison to GHMI


#want only offset for Diptera
Diptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Diptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Diptera_offset) #check that it worked


#join data with grids, then make it an sf object
Diptera_offset_map <- Diptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Diptera_p1 <- ggplot(Diptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Dipterans")

# Second plot 
Diptera_p2 <- ggplot(Diptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Diptera_p1 + Diptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Diptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")




#############Figure 11: showing offset of Coleoptera across mapped grids with 
#comparison to GHMI


#want only offset for Coleoptera
Coleoptera_offset <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order == "Coleoptera", !is.na(grid), !is.na(offset)) %>%
  select(order, grid, offset, mean_GHMI)
head(Coleoptera_offset) #check that it worked


#join data with grids, then make it an sf object
Coleoptera_offset_map <- Coleoptera_offset %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
Coleoptera_p1 <- ggplot(Coleoptera_offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of Coleopterans")

# Second plot 
Coleoptera_p2 <- ggplot(Coleoptera_offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- Coleoptera_p1 + Coleoptera_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/Coleoptera_offset_Bioregion_NA24_map.png", width=6, height=6, units="in")



#############Figure 12: showing offset of all pollinators across mapped grids with 
#comparison to GHMI


#join data with grids, then make it an sf object
offset_map <- phenology_estimates_all_species_each_grid_with_landsat %>%
  left_join(., grids_5, by = c("grid" = "grid_id")) %>% #grids_5 is an st 
  st_as_sf() #turn st into sf 

# first plot
offset_p1 <- ggplot(offset_map) +
  geom_sf(aes(fill = offset)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Offset of Flight Period") +
  theme_bw() +
  labs(title = "Offset of Flight Period of all Pollinators")

# Second plot 
offset_p2 <- ggplot(offset_map) +
  geom_sf(aes(fill = mean_GHMI)) +
  geom_sf(data = NA_24, color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_viridis_c(name = "Mean GHMI (Anthropogenic Change)") +
  theme_bw() +
  labs(title = "Anthropogenic Change Across Bioregion NA24")

# Combine the two maps side by side
final_plot <- offset_p1 + offset_p2 + plot_layout(ncol = 2)
final_plot

#save as png: 
ggsave("Figures/offset_Bioregion_NA24_map.png", width=6, height=6, units="in")






################ Figure 13: Onset, Offset, and Duration In Low Versus High GHMI areas

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1, 3, 5:7) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  facet_wrap(~name, ncol=1, scales="free_y")

#save as png: 
ggsave("Figures/mean_offset_duration_in_low_versus_high_ghmi.png", width=6, height=6, units="in")





############## Figure 14: Total Duration of flight period in low versus high GHMI areas for 10 random species
# get example species
example_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(species) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>10) %>%
  sample_n(10)

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_species$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="duration") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Total Duration of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration of Flight Period (Days)")+
  theme(legend.position = "none")
  
#Save it 
ggsave("Figures/total_duration_low_versus_high_urban_for_10_random_species.png", width=6, height=6, units="in")







############## Figure 15: Onset of flight period in low versus high GHMI areas for 10 random species
############### Plotting this for onset
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_species$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="onset") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Onset of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Onset (Days)")+
  theme(legend.position = "none")

#Save it 
ggsave("Figures/onset_low_versus_high_urban_for_10_random_species.png", width=6, height=6, units="in")






################ Figure 16: Average Onset, Offset, and Duration In Low Versus High GHMI areas
 
average_per_GHMI <- phenology_estimates_all_species_each_grid_with_landsat %>%
  group_by(mean_GHMI) %>%
  summarise(
    avg_onset = mean(onset, na.rm = TRUE),
    avg_offset = mean(offset, na.rm = TRUE),
    avg_duration = mean(offset, na.rm = TRUE)
  )
average_per_GHMI

#plot 1: onset 
p1 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_onset)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Onset") +
  theme(plot.title = element_text(hjust = 0.5))

#plot 2: duration
p2 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_duration)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Total Duration") +
  theme(plot.title = element_text(hjust = 0.5))

# plot 3: offset
p3 <- ggplot(average_per_GHMI, aes(x = mean_GHMI, y = avg_offset)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Mean GHMI", y = "Offset") +
  theme(plot.title = element_text(hjust = 0.5))


# Figure title
main_title <- textGrob("Average Phenology Estimates Across an Urban Gradient (GHMI)", gp = gpar(fontsize = 16, fontface = "bold"))

#Plot them together
grid.arrange(main_title, p1, p2, p3, ncol = 1, heights = c(0.1, 1, 1, 1))  #3 rows, 1 column
grid.arrange(main_title, p1, p2, p3, 
             ncol = 3, layout_matrix = rbind(c(1, 1, 1), c(2, 3, 4)), 
             widths = c(1, 1, 1), heights = c(0.1, 1)) 
#1 row, 3 columns

#Save it 
ggsave("Figures/onset_offset_duration_in_low_versus_high_urban.png", width=6, height=6, units="in")







############## Figure 17: Total Duration of flight period in low versus high GHMI areas for 10 random Lepidopterans

example_leps <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera") %>%
  group_by(species) %>%
  summarize(N=n()) %>%
  dplyr::filter(N>10) %>%
  sample_n(10)

phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% example_leps$species) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name=="duration") %>%
  ggplot(., aes(x=mean_GHMI, y=value, color=name))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  facet_wrap(~species, ncol=2, scales="free_y")+
  labs(
    title = "Total Duration of Phenology of 10 Random Species in Low Verus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration (Days)")+
  theme(legend.position = "none")+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)  # adjust size as needed
  )

#Save it 
ggsave("Figures/total_duration_in_low_versus_high_urban_for_10_random_leps.png", width=6, height=6, units="in")












################# Figure 18: comparing total duration of 10 pre-selected species 


#Look at all the species in each Order (want a variety)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Hymenoptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Diptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Coleoptera")%>%
  distinct(species)
phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(order=="Lepidoptera")%>%
  distinct(species)


#filter for only these species 
ten_selected_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species=="Apis mellifera"|species=="Bombus impatiens"|species=="Vespula maculifrons"|species=="Xylocopa virginica"|
           species=="Toxomerus geminatus"|species=="Chauliognathus pensylvanicus"|species=="Toxomerus marginatus"|
           species=="Tetraopes tetrophthalmus"|species=="Papilio troilus"|species=="Urbanus proteus")




#Plot it
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%  
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "duration") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Total Duration of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Total Duration (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )


#Save it 
ggsave("Figures/total_duration_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")







################# Figure 19: comparing onset of 10 pre-selected species 

#Plot it with onset
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "onset") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Onset of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Flight Period Onset (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )

#Save it 
ggsave("Figures/onset_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")








################# Figure 20: comparing offset of 10 pre-selected species 


#Plot it with offset
phenology_estimates_all_species_each_grid_with_landsat %>%
  dplyr::select(1:7) %>%
  dplyr::filter(species %in% unique(ten_selected_species$species)) %>%
  dplyr::select(-grid) %>%
  pivot_longer(!c(species, mean_GHMI)) %>%
  dplyr::filter(name == "offset") %>%
  ggplot(aes(x = mean_GHMI, y = value, color = name)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm") +
  facet_wrap(~species, ncol = 2, scales = "free_y") +
  labs(
    title = "Offset of Flight Period of 10 Species in Low Versus High GHMI Areas",
    x = "Mean GHMI",
    y = "Flight Period Offset (Days)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )

#Save it 
ggsave("Figures/offset_in_low_versus_high_urban_for_10_pre-selected_species.png", width=6, height=6, units="in")








######## Figure 21: Plotting slopes (estimates) of species' change in total duration across range of 
#        GHMI for 20 randomly selected species 

# Subset for duration values 
duration_gam <- species_gam %>%
  filter(model == "duration" & !is.na(GHMI_estimate))

# Randomly sample 20 species
set.seed(123)  
random_species <- sample(unique(duration_gam$species), 20)
duration_gam_random <- duration_gam %>%
  filter(species %in% random_species)



#Plot the slopes with the species names on the y-axis
ggplot(duration_gam_random, aes(x = GHMI_estimate, y = species)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    title = "Slope of Duration (GAM GHMI Effect) for 20 Randomly Selected Species",
    x = "GHMI Estimate (Slope)",
    y = "Species"
  ) +
  theme(axis.text.y = element_text(size = 8))


#Save it 
ggsave("Figures/slope_of_species_duration_plot_20_random_species.png", width=6, height=6, units="in", bg = "transparent")








######## Figure 22: Plotting slopes of species' change in total duration across range of GHMI for 10
#        example species  


# Randomly sample 10 species
set.seed(123)  
ten_random_species <- sample(unique(duration_gam$species), 10)
duration_gam_random_ten <- duration_gam %>%
  filter(species %in% ten_random_species)



#Plot the slopes with the species names on the y-axis
ggplot(duration_gam_random_ten, aes(x = GHMI_estimate, y = species)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    title = "Slope of Duration for 10 Randomly Selected Species",
    x = "GHMI Estimate (Slope)",
    y = "Species"
  ) +
  theme(axis.text.y = element_text(size = 8))

#Save it 
ggsave("Figures/slope_of_ten_selected_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")







######## Figure 23: Plotting slopes of species' change in onset across range of GHMI for 10
#        example species  

# Subset for onset values 
onset_gam <- species_gam %>%
  filter(model == "onset" & !is.na(GHMI_estimate))

# Randomly sample 10 species
onset_gam_random <- onset_gam %>%
  filter(species %in% ten_random_species)



#Plot the slopes with the species names on the y-axis
ggplot(onset_gam_random, aes(x = GHMI_estimate, y = species)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    title = "Slope of Onset for 10 Randomly Selected Species",
    x = "GHMI Estimate (Slope)",
    y = "Species"
  ) +
  theme(axis.text.y = element_text(size = 8))

#Save it 
ggsave("Figures/slope_of_ten_selected_species_onset_plot.png", width=6, height=6, units="in", bg = "transparent")





######## Figure 24: Plotting slopes of species' change in offset across range of GHMI for 10
#        example species  

# Subset for offset values 
offset_gam <- species_gam %>%
  filter(model == "offset" & !is.na(GHMI_estimate))

# Randomly sample 10 species
offset_gam_random <- offset_gam %>%
  filter(species %in% ten_random_species)



#Plot the slopes with the species names on the y-axis
ggplot(offset_gam_random, aes(x = GHMI_estimate, y = species)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    title = "Slope of Offset for 10 Randomly Selected Species",
    x = "GHMI Estimate (Slope)",
    y = "Species"
  ) +
  theme(axis.text.y = element_text(size = 8))

#Save it 
ggsave("Figures/slope_of_ten_selected_species_offset_plot.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 25: Plotting slopes of species' change in total duration across range of GHMI for all 
#        Lepidopterans 


# Subset Lepidopterans
lep_gam <- species_gam %>%
  filter(order == "Lepidoptera" & !is.na(GHMI_estimate))


# Convert species names to numeric indices
lep_gam$species <- as.numeric(factor(lep_gam$species))


# Plot the slopes 
ggplot(lep_gam, aes(x = species, y = GHMI_estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.6) +  # red horizontal line (becomes vertical after coord_flip)
  theme_minimal() +
  labs(
    title = "Total Flight Duration Across a Range of GHMI Values for All Lepidoptera Species",
    x = "Species Identification Number",
    y = "Slope of Duration versus GHMI"
  ) +
  coord_flip() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(color = "black")
  )



#Save it 
ggsave("Figures/slope_of_all_Lepidoptera_species_duration_plot.png", width=6, height=6, units="in", bg = "transparent")








######## Figure 26: Plotting slopes of species' change in total duration across range of GHMI for all species


# Subset duration estimates and reorder species by slope
species_gam_duration <- species_gam %>%
  filter(model == "duration") %>% #filter for only duration model outputs
  arrange(GHMI_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())

#Plot it 
p_duration <- ggplot(species_gam_duration, aes(x = GHMI_estimate, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se, xmax = GHMI_estimate + GHMI_se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Total Duration vs GHMI",
    y = "Species Identification Number",
    title = "Total Duration of Activity Period Across a Range of GHMI Values for All Species"
  ) +
  xlim(-200, 250) +
  theme(plot.title = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.text.y = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 14), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        axis.line = element_line(color = "black"))
p_duration

#Save it 
ggsave("Figures/slope_of_species_duration_plot_w_climate.png", width=6, height=6, units="in", bg = "transparent")






######## Figure 27: Plotting slopes of species' onset across range of GHMI for all species

# Subset onset estimates and reorder species by slope
species_gam_onset <- species_gam %>%
  filter(model == "onset") %>% #filter for only onset model outputs
  arrange(GHMI_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())

#Plot it 
p_onset <- ggplot(species_gam_onset, aes(x = GHMI_estimate, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se, xmax = GHMI_estimate + GHMI_se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Onset vs GHMI",
    y = "Species Identification Number",
    title = "Onset of Activity Period Across a Range of GHM Values for All Species"
  ) +
  xlim(-200, 250) +
  theme(plot.title = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.text.y = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 14), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        axis.line = element_line(color = "black"))


p_onset

#Save it 
ggsave("Figures/slope_of_species_onset_plot_w_climate.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 28: Plotting slopes of species' offset across range of GHMI for all species

# Subset offset estimates and reorder species by slope
species_gam_offset <- species_gam %>%
  filter(model == "offset") %>% #filter for only offset model outputs
  arrange(GHMI_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())

#Plot it 
p_offset <- ggplot(species_gam_offset, aes(x = GHMI_estimate, y = species_id)) +
  geom_point() +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se, xmax = GHMI_estimate + GHMI_se), height = 0.2) +
  geom_vline(xintercept = 0, color = "red") +
  theme_minimal() +
  labs(
    x = "Slope of Offset vs GHMI",
    y = "Species Identification Number",
    title = "Offset of Activity Period Across a Range of GHM Values for All Species"
  ) +
  xlim(-200, 250) +
  theme(plot.title = element_text(family = "Times New Roman", size = 14),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.text.y = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 14),
        axis.title.y = element_text(family = "Times New Roman", size = 14), 
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.6), 
        axis.line = element_line(color = "black"))

p_offset

#Save it 
ggsave("Figures/slope_of_species_offset_plot_w_climate.png", width=6, height=6, units="in", bg = "transparent")









######## Figure 29: Plotting slopes of species' onset, offset, and duration across range of GHMI 
#       for all species (a combined plot of the past 3 figures)



species_gam_all <- bind_rows(
  species_gam_duration %>% mutate(variable = "Duration"),
  species_gam_onset     %>% mutate(variable = "Onset"),
  species_gam_offset    %>% mutate(variable = "Offset")
)


ggplot(species_gam_all, aes(x = GHMI_estimate, y = species_id)) +
  geom_point(size=0.6) +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se,
                     xmax = GHMI_estimate + GHMI_se), height = 0.1, linewidth=0.3) +
  geom_vline(xintercept = 0, color = "red", size = 0.4) +
  facet_wrap(~ variable, nrow = 1) +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                     
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.text = element_text(family = "Times New Roman", size = 10),
    axis.title = element_text(family = "Times New Roman", size = 10),
    strip.text = element_text(family = "Times New Roman", size = 12, face="bold"),
    panel.spacing = unit(1.5, "lines")               
  ) +
  labs(x = "Effect of Anthropogenic Modification on Phenology", y = "Species Identification Number")


# Save it as a full figure 
ggsave("Figures/combined_plot_phenology_slopes_of_all_species_w_climate.png",
       width = 6.5, height = 3.41, units = "in", bg = "transparent")

# Then save a second one with solid background 
ggplot(species_gam_all, aes(x = GHMI_estimate, y = species_id)) +
  geom_point(size=0.6) +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se,
                     xmax = GHMI_estimate + GHMI_se), height = 0.1, linewidth=0.3) +
  geom_vline(xintercept = 0, color = "red", size = 0.4) +
  facet_wrap(~ variable, nrow = 1) +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                     
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.text = element_text(family = "Times New Roman", size = 10),
    axis.title = element_text(family = "Times New Roman", size = 10),
    strip.text = element_text(family = "Times New Roman", size = 12, face="bold"),
    panel.spacing = unit(1.5, "lines")               
  ) +
  labs(x = "Effect of Anthropogenic Modification on Phenology", y = "Species Identification Number")

ggsave("Figures/combined_plot_phenology_slopes_of_all_species_w_climate_solid_background.png",
       width = 6.5, height = 3.41, units = "in", bg = "white")


# Then save a third one with PowerPoint dimensions 
ggplot(species_gam_all, aes(x = GHMI_estimate, y = species_id)) +
  geom_point(size=2.3) +
  geom_errorbarh(aes(xmin = GHMI_estimate - GHMI_se,
                     xmax = GHMI_estimate + GHMI_se), height = 0.1, linewidth=0.3) +
  geom_vline(xintercept = 0, color = "red", size = 0.7) +
  facet_wrap(~ variable, nrow = 1) +  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),                     
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.text = element_text(family = "Times New Roman", size = 24),
    axis.title = element_text(family = "Times New Roman", size = 28),
    strip.text = element_text(family = "Times New Roman", size = 34, face="bold"),
    panel.spacing = unit(1.5, "lines")               
  ) +
  labs(x = "Effect of Anthropogenic Modification on Phenology", y = "Species Identification Number")

ggsave("Figures/combined_plot_phenology_slopes_of_all_species_with_climate_PowerPoint_dimensions.png",
       width = 19.26, height = 10.1,  units = "in", bg = "transparent")




######## Figure 30: Total Duration values across a range of GHMI values for 
#        6 species 


# Filter for the 6 species we want to look at
selected_species <- c("Bombus impatiens",
                      "Xylocopa virginica",
                      "Apis mellifera", 
                      "Papilio troilus", 
                      "Abaeis nicippe", 
                      "Chauliognathus marginatus"
)

six_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species %in% selected_species)


# Make a data frame of predictions using the GAM models stored in species_gam_full
prediction_df <- map_dfr(selected_species, function(sp) {
  
  # extract GAM
  model <- species_gam_full[[sp]]$models$duration
  
  # observed GHMI range
  ghmi_seq <- seq(
    min(six_species$mean_GHMI[six_species$species == sp], na.rm = TRUE),
    max(six_species$mean_GHMI[six_species$species == sp], na.rm = TRUE),
    length.out = 200
  )
  
  # average lat/lon for this species and average temp/precip
  sp_avg <- six_species %>%
    filter(species == sp) %>%
    summarise(
      lat = mean(lat, na.rm = TRUE),
      lon = mean(lon, na.rm = TRUE),
      prcp = mean(prcp, na.rm = TRUE),
      temp = mean(temp, na.rm = TRUE)
    )
  
  # newdata for prediction
  newdata <- data.frame(
    mean_GHMI = ghmi_seq,
    lat = sp_avg$lat,
    lon = sp_avg$lon,
    temp = sp_avg$temp,
    prcp = sp_avg$prcp
  )
  
  # predict with SE
  preds <- predict(model, newdata = newdata, se.fit = TRUE)
  
  newdata %>%
    mutate(
      species = sp,
      fit = preds$fit,
      se = preds$se.fit,
      lower = fit - 2 * se,
      upper = fit + 2 * se
    )
})

# This uses the GAMs (stored in species_gam_full) to predict the duration values we'd expect at each
#GHMI point. This is so we can create a slope (red line) based on our GAMs. The confidence intervals
#were calculated using the SE of predicted values from the GAMs output. 



# Plot it 
ggplot() +
  geom_point(
    data = six_species,
    aes(x = mean_GHMI, y = duration),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 2, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size=18)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Duration (days)"
  )


# Save it
ggsave("Figures/duration_across_ghmi_for_6_species_w_climate.png",  width = 6.2, height = 7.37, units = "in", bg = "transparent")

# Now save it with solid white background
ggplot() +
  geom_point(
    data = six_species,
    aes(x = mean_GHMI, y = duration),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 2, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size=18)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Duration (days)"
  )

ggsave("Figures/duration_across_ghmi_for_6_species_w_climate_solid_white_background.png",  width = 6.2, height = 7.37, units = "in", bg = "white")


# Now save it with PowerPoint dimensions
ggplot() +
  geom_point(
    data = six_species,
    aes(x = mean_GHMI, y = duration),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 2, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 24),
    axis.text = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size=26)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Duration (days)"
  )
ggsave("Figures/duration_across_ghmi_for_6_species_w_climate_PowerPoint_dimensions.png",  
       width = 9.33, height = 11.1, units = "in", bg = "transparent")












######## Figure 31: Onset values across a range of GHMI values for 
#        10 species 


# Species lists by phenology estimate 
species_onset <- c("Papilio glaucus",
                   "Xylocopa virginica",
                   "Danaus plexippus",
                   "Apis mellifera",
                   "Battus philenor",
                   "Hylephila phyleus", 
                   "Bombus griseocollis",
                   "Papilio troilus",
                   "Bombus pensylvanicus", 
                   "Chauliognathus marginatus")

ten_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species %in% species_onset)


# Make a data frame of predictions using the GAM models stored in species_gam_full
prediction_df <- map_dfr(species_onset, function(sp) {
  
  # extract GAM
  model <- species_gam_full[[sp]]$models$onset
  
  # observed GHMI range
  ghmi_seq <- seq(
    min(ten_species$mean_GHMI[ten_species$species == sp], na.rm = TRUE),
    max(ten_species$mean_GHMI[ten_species$species == sp], na.rm = TRUE),
    length.out = 200
  )
  
  # average lat/lon for this species and average temp/precip
  sp_avg <- ten_species %>%
    filter(species == sp) %>%
    summarise(
      lat = mean(lat, na.rm = TRUE),
      lon = mean(lon, na.rm = TRUE),
      prcp = mean(prcp, na.rm = TRUE),
      temp = mean(temp, na.rm = TRUE)
    )
  
  # newdata for prediction
  newdata <- data.frame(
    mean_GHMI = ghmi_seq,
    lat = sp_avg$lat,
    lon = sp_avg$lon,
    temp = sp_avg$temp,
    prcp = sp_avg$prcp
  )
  
  # predict with SE
  preds <- predict(model, newdata = newdata, se.fit = TRUE)
  
  newdata %>%
    mutate(
      species = sp,
      fit = preds$fit,
      se = preds$se.fit,
      lower = fit - 2 * se,
      upper = fit + 2 * se
    )
})


# This uses the GAMs (stored in species_gam_full) to predict the onset values we'd expect at each
#GHMI point. This is so we can create a slope (red line) based on our GAMs. The confidence intervals
#were calculated using the SE of predicted values from the GAMs output. 



# Plot it 
ggplot() +
  geom_point(
    data = ten_species,
    aes(x = mean_GHMI, y = onset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 9),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size=16)
  )  +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Onset (days)"
  )

# Save it
ggsave("Figures/onset_across_ghmi_for_10_species_w_climate.png", width = 6.2, height = 7.37, units = "in", bg = "transparent")

# Now save it with a white background
ggplot() +
  geom_point(
    data = ten_species,
    aes(x = mean_GHMI, y = onset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 9),
    axis.text = element_text(family = "Times New Roman", size = 14),
    axis.title = element_text(family = "Times New Roman", size=16)
  )  +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Onset (days)"
  )

# Save it
ggsave("Figures/onset_across_ghmi_for_10_species_w_climate_solid_white_background.png", width = 6.2, height = 7.37, units = "in", bg = "white")


# Now save it with PowerPoint dimensions
ggplot() +
  geom_point(
    data = ten_species,
    aes(x = mean_GHMI, y = onset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 16),
    axis.text = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size=26)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Onset (days)"
  )

ggsave("Figures/onset_across_ghmi_for_10_species_w_climate_PowerPoint_dimensions.png", 
       width = 9.46, height = 11.25, units = "in", bg = "transparent")












######## Figure 32: Offset values across a range of GHMI values for 
#        12 species 



species_offset <- c("Papilio glaucus",
                    "Bombus impatiens",
                    "Danaus plexippus",
                    "Epargyreus clarus",
                    "Battus philenor",
                    "Phyciodes tharos",
                    "Hylephila phyleus",
                    "Pieris rapae",
                    "Pyrrharctia isabella",
                    "Scopula limboundata",
                    "Pleuroprucha insulsaria",
                    "Marimatha nigrofimbria")

twelve_species <- phenology_estimates_all_species_each_grid_with_landsat %>%
  filter(species %in% species_offset)


# Make a data frame of predictions using the GAM models stored in species_gam_full
prediction_df <- map_dfr(species_offset, function(sp) {
  
  # extract GAM
  model <- species_gam_full[[sp]]$models$offset
  
  # observed GHMI range
  ghmi_seq <- seq(
    min(twelve_species$mean_GHMI[twelve_species$species == sp], na.rm = TRUE),
    max(twelve_species$mean_GHMI[twelve_species$species == sp], na.rm = TRUE),
    length.out = 200
  )
  
  # average lat/lon for this species and average temp/precip
  sp_avg <- twelve_species %>%
    filter(species == sp) %>%
    summarise(
      lat = mean(lat, na.rm = TRUE),
      lon = mean(lon, na.rm = TRUE),
      prcp = mean(prcp, na.rm = TRUE),
      temp = mean(temp, na.rm = TRUE)
    )
  
  # newdata for prediction
  newdata <- data.frame(
    mean_GHMI = ghmi_seq,
    lat = sp_avg$lat,
    lon = sp_avg$lon,
    temp = sp_avg$temp,
    prcp = sp_avg$prcp
  )
  
  # predict with SE
  preds <- predict(model, newdata = newdata, se.fit = TRUE)
  
  newdata %>%
    mutate(
      species = sp,
      fit = preds$fit,
      se = preds$se.fit,
      lower = fit - 2 * se,
      upper = fit + 2 * se
    )
})

# This uses the GAMs (stored in species_gam_full) to predict the offset values we'd expect at each
#GHMI point. This is so we can create a slope (red line) based on our GAMs. The confidence intervals
#were calculated using the SE of predicted values from the GAMs output. 



# Plot it 
ggplot() +
  geom_point(
    data = twelve_species,
    aes(x = mean_GHMI, y = offset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 9),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size=14)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Offset (days)"
  )

# Save it
ggsave("Figures/offset_across_ghmi_for_12_species_w_climate.png", width = 6.2, height = 7.37, units = "in", bg = "transparent")


#Now save it with white background
ggplot() +
  geom_point(
    data = twelve_species,
    aes(x = mean_GHMI, y = offset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 9),
    axis.text = element_text(family = "Times New Roman", size = 12),
    axis.title = element_text(family = "Times New Roman", size=14)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Offset (days)"
  )

# Save it
ggsave("Figures/offset_across_ghmi_for_12_species_w_climate_solid_white_background.png", width = 6.2, height = 7.37, units = "in", bg = "white")


# Now save it with PowerPoint dimensions
ggplot() +
  geom_point(
    data = twelve_species,
    aes(x = mean_GHMI, y = offset),
    alpha = 0.5
  ) +
  geom_line(
    data = prediction_df,
    aes(x = mean_GHMI, y = fit),
    color = "red",
    linewidth = 1
  ) +
  geom_ribbon(
    data = prediction_df,
    aes(x = mean_GHMI, ymin = lower, ymax = upper),
    fill = "red",
    alpha = 0.15
  ) +
  facet_wrap(~ species, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.spacing = unit(1, "lines"),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Times New Roman", face = "italic", size = 16),
    axis.text = element_text(family = "Times New Roman", size = 16),
    axis.title = element_text(family = "Times New Roman", size=26)
  ) +
  labs(
    x = "Global Human Modification Index (GHMI)",
    y = "Offset (days)"
  )

# Save it
ggsave("Figures/offset_across_ghmi_for_12_species_w_climate_PowePoint_dimensions.png", 
       width = 9.5, height = 11.29, units = "in", bg = "transparent")


