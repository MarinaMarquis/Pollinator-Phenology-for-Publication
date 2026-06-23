# This script is to combine data and prepare it for analysis

#############################################################################################################

# Load Packages 
library(dplyr)
library(phenesse)
library(sf)
library(lubridate)
library(purrr)
library(moments)
library(diptest)
library(ggplot2)


# Read in data  
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI, precip, and temp for each grid 
phenology_estimates_all_species_each_grid <- readRDS('Data/Phenology Data/phenology_estimates_by_grid_by_species.RDS') #Phenology
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #geoJSON of gridded map of Bioregion NA24
#estimates for each species in each grid 
taxonomy <- readRDS("Data/iNaturalist_pollinator_observations.rds") %>%
  dplyr::select(species, genus, family, order) %>%
  distinct() # Read in observations and get the higher level taxonomy

#############################################################################################################

### Merge phenology_estimates_all_species_each_grid with filtered_5_with_landsat so that 
#   the data frame with phenology estimates of each species in each grid also has 
#   mean GHMI, precip, and temp for each grid

# Merge them into one data set with observations and mean GHMI, precipitation, 
# and temperature per grid: 
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid %>%
  left_join(filtered_5_with_landsat %>%
      select(grid_id, mean_GHMI = mean, temp, prcp), by = c("grid" = "grid_id")) %>%
  left_join(taxonomy, by = "species")

# Filter to include only species that are found in at least twenty grids
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species %in% (
    group_by(., species) %>%
      summarize(n_grids = n(), .groups = 'drop') %>%
      filter(n_grids >= 20) %>%
      pull(species)
  ))

# Check that it worked 
check <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  distinct(species, grid) %>%
  group_by(species) %>%
  summarize(n_grids = n()) %>%
  arrange(n_grids)
print(check, n = Inf)

#Look at all the species in the data set after this filter
unique(phenology_estimates_all_species_each_grid_with_GHMI$species)


#############################################################################################################


#Removing species that aren't pollinators or that we cannot determine to be pollinators (because 
#there is insufficient peer-reviewed information on their diet) using the species in the filtered_5 dataset. 
#This is just a cautionary step to make sure this species list matches the one
#we were using from our manual literature search. 
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species %in% filtered_5$species)

unique(phenology_estimates_all_species_each_grid_with_GHMI$species) #double check new species list



#############################################################################################################


# Let's see how many phenology estimates exceeded 365 days of year 
sum(phenology_estimates_all_species_each_grid_with_GHMI$onset > 365, na.rm = TRUE) #no instances
sum(phenology_estimates_all_species_each_grid_with_GHMI$offset > 365, na.rm = TRUE) #10 instances 
sum(phenology_estimates_all_species_each_grid_with_GHMI$duration > 365, na.rm = TRUE) #0 instance 

# We need to investigate these values
overestimates <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(offset > 365 | duration > 365)%>%
  print()

# Subset the data, make sure we are only using the species used in our phenology_estimates_all_species_each_grid_with_GHMI df
filtered_5 <- filtered_5 %>%
  semi_join(
    phenology_estimates_all_species_each_grid_with_GHMI %>%
      dplyr::select(species, grid) %>%
      distinct(),
    by = c("species" = "species", "grid_id" = "grid")
  ) 
obs <- filtered_5 %>%
  filter(species == species_of_interest, grid_id == grid_of_interest) %>%
  mutate(day_of_year = as.integer(lubridate::yday(eventDate))) %>%
  filter(day_of_year > 0)

# View results for suspicious species and grids only
suspicious_species_grids <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(offset > 365 | duration > 365) %>%
  select(species, grid) %>%
  distinct()
suspicious_species_grids

# Calculate skewness and sample size per species-grid combo
skewness_suspicious <- filtered_5 %>%
  filter(paste(species, grid_id) %in% paste(
    overestimates$species, overestimates$grid
  )) %>%
  mutate(day_of_year = as.integer(lubridate::yday(eventDate))) %>%
  filter(day_of_year > 0) %>%
  group_by(species, grid_id) %>%
  summarise(
    skewness = if(n() > 2) skewness(day_of_year, na.rm = TRUE) else NA_real_,
    n_obs = n(),
    .groups = "drop"
  )
print(skewness_suspicious, n=10)

#Negative skewness values indicate left-skewness (tail on left side). Positive skewness means right-skew.
#Most of the weird phenology estimates are left-skewed. Many of the largest left (negative) skews are 
#with relatively small sample sizes (n_obs). So it looks like a low sample size and strong negative skew = high risk 
#of offset/duration > 365 days. Larger sample size can reduce overestimation, but left skewness alone can 
#push estimates past 365. We do see some moderate negative skews with moderate n. This suggests that sample 
#size helps but skew can still produce weird estimates. In these cases, the right skew is causing the right 
#tail of the Weibull distribution to stretch past the 365 day mark, leading to overestimation of the offset/
#duration values. In conclusion, overestimation can be caused by: left or right skew, year-round observations,
#and low sample size. Larger sample size can reduce some of the overestimation but not entirely prevent it. 


# Now looking at bimodality of seasons 
bimodality_results <- filtered_5 %>%
  mutate(day_of_year = lubridate::yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  group_by(species, grid_id) %>%
  summarise(
    n_obs = n(),
    dip_p_value = if(n_obs > 10) dip.test(day_of_year)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(is_bimodal = dip_p_value < 0.05)

bimodality_results %>% filter(is_bimodal)

# Looking at how many of the suspicious estimates were bimodal
suspicious_bimodal <- overestimates %>%
  left_join(bimodality_results, 
            by = c("species" = "species", "grid" = "grid_id")) %>%
  filter(is_bimodal == TRUE)

# View how many suspicious estimates are also bimodal
print(suspicious_bimodal)
#Only 2 of the 13 suspicious estimates were bimodal. 

# Now join with skewness and sample size info, just so we can compare 
suspicious_bimodal_with_skew <- suspicious_bimodal %>%
  left_join(skewness_suspicious, by = c("species" = "species", "grid" = "grid_id"))

# View result
print(suspicious_bimodal_with_skew)

#Bimodality alone will not cause overestimation. Overestimation occurs when: 
# a) the peaks are far apart with lots of activity between them, causing the Weibull distribution to be 
#fitted as one long season, b) peaks are clustered near the beginning and end of year, so that the 
#distribution/estimation to "wrap around" the calendar year, and c) sample size is too small for the peaks 
#to be detected. 



# Plot of suspicious grid-species combos, to show all cases that can cause overestimation: left skew, low 
# sample size, year-round estimates, bimodality with large separation between peaks, and bimodality with 
#year-round activity  

obs_suspicious <- filtered_5 %>%
  mutate(day_of_year = lubridate::yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  semi_join(suspicious_species_grids, by = c("species" = "species", "grid_id" = "grid"))

# Create a combined label for faceting
obs_suspicious <- obs_suspicious %>%
  mutate(species_grid = paste0(species, " (Grid ", grid_id, ")"))

# Plot histograms faceted by species-grid
ggplot(obs_suspicious, aes(x = day_of_year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  facet_wrap(~ species_grid, scales = "free_y") +
  labs(
    title = "Histogram of Observations by Day of Year for Suspicious Species-Grids",
    x = "Day of Year",
    y = "Observation Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# IN CONCLUSION, overestimation is caused by: 

#Left skew – The tail of observations at the start of the year can push the Weibull fit past 365 days.
#Low sample size – Small and makes the model more sensitive to extreme or unevenly spaced observations.
#Year-round activity – Continuous activity with no clear seasonality causes the Weibull to stretch 
#the “season” beyond the calendar year.
#Bimodality with peaks far apart or at the beginning and end of the year – Large separation 
#between peaks creates an artificially long fitted duration.
#Bimodality with year-round seasonality – Even when peaks are moderate, if there’s activity in the
#gaps between them, the model interprets it as a single long season, inflating duration or offset.


# We will now filter out all instances of overestimation, since we know what causes them and have determined
# that this is not an issue with the entire data set and the way we were estimating phenology
phenology_filtered <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(!(offset > 365))

#Check that it worked
sum(phenology_filtered$offset > 365, na.rm = TRUE) #0 instances 

#Look at new species and grids  
length(unique(phenology_filtered$species)) #54 species 
length(unique(phenology_filtered$family)) #20 families  
length(unique(phenology_filtered$order)) #4 orders 
length(unique(phenology_filtered$grid)) #758 grids 

# How many grids per species 
grid_per_spec <- phenology_filtered %>%
  group_by (species)%>%
  summarise(grid_per_spec = n_distinct(grid))%>%
  arrange(desc(grid_per_spec))
grid_per_spec #Papilio glaucus found in most grids (386 grids)


#Save it: 
saveRDS(phenology_filtered, "Data/phenology_estimates_data_for_analysis.rds") 

#############################################################################################################
### Explore other variables 

# Now get the mean latitude and longitude for each grid
grids_centroids <- five_km_grids %>%
  st_centroid() %>%                                 
  mutate(lon = st_coordinates(.)[, 1],             
         lat = st_coordinates(.)[, 2]) %>%        
  st_drop_geometry() %>%                            
  select(grid_id, lon, lat) %>%
  group_by(grid_id) %>%
  summarise(lon = first(lon),
            lat = first(lat),
            .groups = "drop")

# Now add the mean lon and lat to our phenology_filtered
phenology_filtered <- left_join(phenology_filtered, grids_centroids, by=c("grid"="grid_id"))%>%
  mutate(species = as.factor(species))



########################################################################################################### 
### Let's explore the range of GHMI values in the data set: 

summary(fp_rel$mean_GHMI)
sd(fp_rel$mean_GHMI)

# Plot it
ggplot(fp_rel, aes(x = mean_GHMI)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic() +
  labs(x = "Mean GHMI", y = "Count",
       title = "Distribution of GHMI in the data set")
# Looks like there is some skew towards higher GHMI values in the data set 

# Summarize by species 
fp_data_summary <- phenology_filtered %>%
  group_by(species) %>%
  summarise(min_GHMI = min(mean_GHMI, na.rm = TRUE),
            max_GHMI = max(mean_GHMI, na.rm = TRUE),
            n = n()) %>%
  arrange(min_GHMI) %>%
  print(n = 50)


# Flagging all species with narrow GHMI ranges 
check_species <- function(df) {
  m <- gam(duration ~ s(mean_GHMI, k = min(5, length(unique(df$mean_GHMI)))),
           data = df, method = "REML")
  tibble(
    n = nrow(df),
    range = diff(range(df$mean_GHMI)),
    sd = sd(df$mean_GHMI),
    edf = summary(m)$edf[1]  
  )
}
results <- phenology_filtered %>% group_by(species) %>% group_modify(~check_species(.x))
print(results, n=107)


# Removing species with a range less than 0.3 and a standard deviation less than 0.10 so that they
# can be properly fitted to GAMs
flagged_species <- results %>%
  filter(range < 0.3 | sd < 0.1)
phenology_filtered <- phenology_filtered %>%
  filter(!species %in% flagged_species$species)

summary(phenology_filtered$mean_GHMI)
sd(phenology_filtered$mean_GHMI)

# Save it 
saveRDS(phenology_filtered, "Data/final_phenology_df_for_analysis.RDS")

########################################################################################################### 
### Summarize data after final levels of filtering 

# Table 1: Produce a table summarizing the species being used in the GAMs, the number of grid cells that 
# each species has, the number of observations for each species, and the range of GHMI values 
# across all grid cells for each species 
data_for_models_summary <- phenology_filtered %>%
  group_by(species) %>%
  summarise(
    n_grid_cells = n_distinct(grid),
    min_GHMI = min(mean_GHMI, na.rm = TRUE),
    max_GHMI = max(mean_GHMI, na.rm = TRUE),
    GHMI_range = max_GHMI - min_GHMI,
    .groups = "drop"
  ) %>%
  arrange(desc(n_grid_cells))%>%
  mutate(across(c(min_GHMI, max_GHMI, GHMI_range), ~round(.x, 3))) %>%
  arrange(desc(n_grid_cells))%>%
  print()

# Export to CSV 
write.csv(data_for_models_summary, "Data/data_for_models_summary.csv")

### Look at the make-up of our data after this final level of filtering: 
#Look at new species and grids  
length(unique(phenology_filtered$species)) #52 species 
length(unique(phenology_filtered$family)) #20 families  
length(unique(phenology_filtered$order)) #4 orders 
length(unique(phenology_filtered$grid)) #756 grids 
species_per_order <- phenology_filtered %>%
  group_by(order) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species))%>%
  print()


# Also want to know how many pollinator observations we ended up using in this study
obs_used <- filtered_5 %>%  #filter raw observation data to only include grid/species combos used in fp_data
  semi_join(phenology_filtered, by = c("species" = "species", "grid_id" = "grid"))

n_obs_used <- nrow(obs_used) # Total # of obs used to produce phenology estimates that we actually
#used for the GAMs
n_obs_used   #83012

# Per species
obs_used_per_species <- obs_used %>%
  count(species, name = "n_obs")%>%
  print()

# Per species × grid cell
obs_used_per_sp_grid <- obs_used %>%
  count(species, grid_id, name = "n_obs")%>%
  print()

# Distribution of climatic variables used in the models 

# From our df, get the GHMI, temp, and precip of each GHMI so we can look at their ranges 
# and averages 
variable_summaries <- phenology_filtered %>%
  select(grid, prcp, temp, mean_GHMI)%>%
  distinct(grid, prcp, temp, mean_GHMI)
sum(duplicated(variable_summaries$grid)) #make sure it worked 

range(variable_summaries$temp) # temp ranged from 6.984389 to 18.213873 C
mean(variable_summaries$temp) # avg temp is 13.57113 C
sd(variable_summaries$temp) # temp has a standard deviation of 2.643663
range(variable_summaries$prcp) # prcp ranged from around 2.490541 5.978967 mm 
mean(variable_summaries$prcp) #average of 3.759373 mm of precip 
sd(variable_summaries$prcp) # prcp has a standard deviation of 0.5650605
range(variable_summaries$mean_GHMI) # GHMI ranged from 0.0002226816 to 0.9191919168
mean(variable_summaries$mean_GHMI) #average GHMI of 0.4998607
sd(variable_summaries$mean_GHMI) #GHMI has a standard deviation of 0.2025444




