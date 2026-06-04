# Compare GHMI and Population Density Estimates
# March 31, 2026

library(tidyverse)
library(patchwork)

# read in data
species_gam <- read.csv("Data/GAM_results/gam_results_by_species.csv") #individual species GAM results, select model outputs 
species_gam_ghmi <- read.csv("Data/GAM_results/gam_results_by_species_w_climate.csv") #individual species GAM results, select model outputs 
species_gam_pop <- read.csv("Data/GAM_results/gam_results_by_species_pop_den.csv") #individual species GAM results, select model outputs 

colnames(species_gam_ghmi)

# plot with and without climte
# combine the data frames to compare estimates
estimates <- left_join(species_gam %>% select(model, species, GHMI_estimate), 
                       species_gam_ghmi %>% select(model, species, GHMI_estimate) %>% rename(GHMI_estimate_climate = GHMI_estimate), 
                       by=c("model", "species"))

# plot the data
ggplot(estimates %>% filter(!is.na(GHMI_estimate)),
       aes(x = GHMI_estimate, y = GHMI_estimate_climate)) +
  
  geom_point(alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  
  theme_minimal() +
  labs(
    x = "GHMI Estimate Without Climate Variables",
    y = "GHMI Estimate with Climatic Variables"
  )

lm_estimates <- lm(GHMI_estimate_climate ~ GHMI_estimate, data= estimates)
summary(lm_estimates)


# combine the data frames to compare estimates
estimates <- left_join(species_gam %>% select(model, species, GHMI_estimate), 
                       species_gam_ghmi %>% select(model, species, GHMI_estimate) %>% rename(GHMI_estimate_climate = GHMI_estimate), 
                       by=c("model", "species"))

# Duration
# get correlation
estimates_complete_dur <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="duration")

r_val_dur <- cor(estimates_complete_dur$GHMI_estimate_climate,
                 estimates_complete_dur$GHMI_estimate,
                 use = "complete.obs")

# plot the data
(dur <- ggplot(estimates_complete_dur,
               aes(x = GHMI_estimate, y = GHMI_estimate_climate)) +
    
    geom_point(alpha = 0.7) +
    
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Cor = ", round(r_val_dur, 3)),
             hjust = 2, vjust = 1.5,
             size = 5) +
    
    theme_minimal() +
    
    labs(
      title = "A) Duration",
      x = "GHMI Estimate Without Climate Variables",
      y = "GHMI Estimate with Climatic Variables"
    ))

# onset
# get correlation
estimates_complete_onset <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="onset")

r_val_onset <- cor(estimates_complete_onset$GHMI_estimate_climate,
                   estimates_complete_onset$GHMI_estimate,
                   use = "complete.obs")

# plot the data
(onset <- ggplot(estimates_complete_onset,
                 aes(x = GHMI_estimate, y = GHMI_estimate_climate)) +
    
    geom_point(alpha = 0.7) +
    
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Cor = ", round(r_val_onset, 3)),
             hjust = 2, vjust = 1.5,
             size = 5) +
    
    theme_minimal() +
    
    labs(
      title = "B) Onset",
      x = "GHMI Estimate Without Climate Variables",
      y = "GHMI Estimate with Climatic Variables"
    ))

# offset
# get correlation
estimates_complete_offset <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="offset")

r_val_offset <- cor(estimates_complete_offset$GHMI_estimate_climate,
                    estimates_complete_offset$GHMI_estimate,
                    use = "complete.obs")

# plot the data
(offset <- ggplot(estimates_complete_offset,
                  aes(x = GHMI_estimate, y = GHMI_estimate_climate)) +
    
    geom_point(alpha = 0.7) +
    
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Cor = ", round(r_val_offset, 3)),
             hjust = 2, vjust = 1.5,
             size = 5) +
    
    theme_minimal() +
    
    labs(
      title = "C) Offset",
      x = "GHMI Estimate Without Climate Variables",
      y = "GHMI Estimate with Climatic Variables"
    ))

dur + onset + offset

ggsave("Figures/ghmi_vs_ghmi_climate_slope.jpeg", height = 5, width = 9, units="in")

# GHMI vs Pop Density -----------------------------------------------------

# combine the data frames to compare estimates
estimates <- left_join(species_gam_pop %>% select(model, species, pop_den_estimate, pop_den_se), 
                       species_gam_ghmi %>% select(model, species, GHMI_estimate, GHMI_se), by=c("model", "species"))

# Duration
# get correlation
estimates_complete_dur <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="duration")

r_val_dur <- cor(estimates_complete_dur$pop_den_estimate,
             estimates_complete_dur$GHMI_estimate,
             use = "complete.obs")

# plot the data
(dur <- ggplot(estimates_complete_dur,
       aes(x = GHMI_estimate, y = pop_den_estimate)) +
  
  geom_point(alpha = 0.7) +
  
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  
  annotate("text",
           x = Inf, y = Inf,
           label = paste0("Cor = ", round(r_val_dur, 3)),
           hjust = 2, vjust = 1.5,
           size = 5) +
  
  theme_minimal() +
  
  labs(
    title = "A) Duration",
    x = "GHMI Estimate",
    y = "Population Density Estimate"
  ))

# onset
# get correlation
estimates_complete_onset <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="onset")

r_val_onset <- cor(estimates_complete_onset$pop_den_estimate,
                 estimates_complete_onset$GHMI_estimate,
                 use = "complete.obs")

# plot the data
(onset <- ggplot(estimates_complete_onset,
               aes(x = GHMI_estimate, y = pop_den_estimate)) +
    
    geom_point(alpha = 0.7) +
    
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Cor = ", round(r_val_onset, 3)),
             hjust = 2, vjust = 1.5,
             size = 5) +
    
    theme_minimal() +
    
    labs(
      title = "B) Onset",
      x = "GHMI Estimate",
      y = "Population Density Estimate"
    ))

# offset
# get correlation
estimates_complete_offset <- estimates %>% 
  filter(!is.na(GHMI_estimate),
         model=="offset")

r_val_offset <- cor(estimates_complete_offset$pop_den_estimate,
                   estimates_complete_offset$GHMI_estimate,
                   use = "complete.obs")

# plot the data
(offset <- ggplot(estimates_complete_offset,
               aes(x = GHMI_estimate, y = pop_den_estimate)) +
    
    geom_point(alpha = 0.7) +
    
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("Cor = ", round(r_val_offset, 3)),
             hjust = 2, vjust = 1.5,
             size = 5) +
    
    theme_minimal() +
    
    labs(
      title = "C) Offset",
      x = "GHMI Estimate",
      y = "Population Density Estimate"
    ))

dur + onset + offset

ggsave("Figures/pop_den_vs_ghmi_slope.jpeg", height = 5, width = 9, units="in")


# Compare results of fig 4 ------------------------------------------------

estimates <- left_join(species_gam_pop %>% select(model, species, pop_den_estimate, pop_den_se), 
                       species_gam_ghmi %>% select(model, species, GHMI_estimate, GHMI_se), by=c("model", "species"))


# Subset duration estimates and reorder species by slope
species_gam_duration <- species_gam_pop %>%
  filter(model == "duration") %>% #filter for only duration model outputs
  arrange(pop_den_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())


# Subset onset estimates and reorder species by slope
species_gam_onset <- species_gam_pop %>%
  filter(model == "onset") %>% #filter for only onset model outputs
  arrange(pop_den_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())

# Subset offset estimates and reorder species by slope
species_gam_offset <- species_gam_pop %>%
  filter(model == "offset") %>% #filter for only offset model outputs
  arrange(pop_den_estimate) %>%   #reorder species by slope
  mutate(species = factor(species, levels = unique(species)),
         species_id = row_number())

species_gam_all <- bind_rows(
  species_gam_duration %>% mutate(variable = "Duration"),
  species_gam_onset     %>% mutate(variable = "Onset"),
  species_gam_offset    %>% mutate(variable = "Offset")
)


ggplot(species_gam_all, aes(x = pop_den_estimate, y = species_id)) +
  
  geom_point(size = 3) +
  
  geom_errorbarh(
    aes(xmin = pop_den_estimate - pop_den_se,
        xmax = pop_den_estimate + pop_den_se),
    height = 0.2,
    linewidth = 1.1
  ) +
  
  geom_vline(xintercept = 0, color = "red") +
  
  facet_wrap(~ variable, nrow = 1, scales="free_x") +
  
  theme_minimal() +
  
  theme(
    panel.grid = element_blank(),                     
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    axis.text = element_text(family = "Times New Roman", size = 30),
    axis.title = element_text(family = "Times New Roman", size = 35),
    strip.text = element_text(family = "Times New Roman", size = 35, face="bold"),
    panel.spacing = unit(1.5, "lines")               
  ) +
  labs(x = "Effect of Population Density on Phenology", y = "Species Identification Number") 

# the really low slop for duration and offset is Hypoprepia fucosa
ggsave("Figures/combined_plot_phenology_slopes_of_all_species_pop_den.png",
       width = 21, height = 11, units = "in", bg = "transparent")

