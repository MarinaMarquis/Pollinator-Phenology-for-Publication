# Phenology-Specific Figures 

#   This script produces figures using the phenology data from the 
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
library(mgcv)

phenology_estimates_all_species_each_grid_with_landsat <- readRDS("Data/final_phenology_df_for_analysis.RDS") #phenology data used in GAMs with climate, lat/long, and GHMI
species_gam <- read.csv("Data/GAM_results/gam_results_by_species_w_climate.csv") #individual species GAM results, select model outputs (p values, estimates, etc)
species_gam_full <- readRDS("Data/GAM_results/species_gam_full_w_climate.rds") # full GAM results (not just select model outputs)


##########################################################################################################################


######## Plotting slopes of species' change in total duration, onset, and offset across range of GHMI for all species


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




######## Onset

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









######## Offset

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





######## Figure 3: Plotting slopes of species' onset, offset, and duration across range of GHMI 
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




######## Figure S10: Total Duration values across a range of GHMI values for 6 species 


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












######## Figure 3: Onset values across a range of GHMI values for 10 species 


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












######## Figure S9: Offset values across a range of GHMI values for 12 species 



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


