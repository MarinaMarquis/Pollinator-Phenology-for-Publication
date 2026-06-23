################# This script shows the distribution of GHMI values for each year in the data set

################# Load packages 
library(ggplot2)
library(dplyr)


################# Read in the data
observations_with_landsat_variables <- readRDS("Data/observations_with_landsat_variables.rds") 
#raw data of observations, GHMI, and grid cell. Filtered to only include 
#observations of species that will be used for analysis 

fp_data <- readRDS("Data/final_phenology_df_for_analysis.RDS")
#the FINAL data frame that was used for in the GAMs so that our raw data can be matched to the
#species x grid combinations that were actually used for analysis


#Valid species x grid combinations that were used for analysis
valid_combos <- fp_data %>%
  select(species, grid) %>%
  distinct()

# Filter data set to match the species x grid combos used for analysis 
observations_with_landsat_variables <- observations_with_landsat_variables %>%
  inner_join(valid_combos, by = c("species" = "species","grid_id" = "grid"))


#################
# Look at frequency of observations across the years, grouped by GHMI. 
# So for each GHMI level, how many observations were recorded each year? 

# Place observations into GHMI bins, for plotting. Summarise number of observations 
# in each GHMI bin each year 
obs_over_GHMI <- observations_with_landsat_variables %>%
  mutate(GHMI_bin = cut(mean_GHMI, breaks = seq(0, 1, by = 0.1))) %>%
  group_by(GHMI_bin, year) %>%
  summarise(obs = n(), .groups = "drop")


# Now plot multiple years using facet_wrap
ggplot(obs_over_GHMI, aes(x = GHMI_bin, y = obs)) +
  geom_col(fill = "steelblue", color = "white") +
  facet_wrap(~year, scales = "free_y") +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "GHMI",
       y = "Number of Observations",
       title = "Frequency of Observations Across GHMI Bins by Year")


# Save it 
ggsave("Figures/Supplementary Figure 6.png", width=7, height=6, units="in")

