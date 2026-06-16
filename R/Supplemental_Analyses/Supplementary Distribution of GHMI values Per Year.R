################# This script shows the distibution of GHMI values for each year in the data set


################# Read in the data
observations_with_landsat_variables <- readRDS("Data/observations_with_landsat_variables.rds") 
#raw data of observations, GHMI, and grid cell. Filtered to only include 
#observations of species that will be used for analysis 

################# Load packages 
library(ggplot2)



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

