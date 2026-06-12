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


# Read in data  
filtered_5 <- readRDS("Data/filtered_5.rds") #observations used to make phenology estimates 
filtered_5_with_landsat <- read.csv("Data/filtered_5_with_GHMI.csv") # mean GHMI for each grid 
phenology_estimates_all_species_each_grid <- readRDS('Data/Phenology Data/phenology_estimates_by_grid_by_species.RDS') #Phenology 
#estimates for each species in each grid 
taxonomy <- readRDS("Data/iNaturalist_pollinator_observations.rds") %>%
  dplyr::select(species, genus, family, order) %>%
  distinct() # Read in observations and get the higher level taxonomy

#############################################################################################################

### Merge phenology_estimates_all_species_each_grid with filtered_5_with_landsat so that 
#   the data frame with phenology estimates of each species in each grid also has mean GHMI for each grid

# Merge them into one data set with observations and mean GHMI per grid: 
phenology_estimates_all_species_each_grid_with_GHMI <- phenology_estimates_all_species_each_grid %>%
  left_join(filtered_5_with_landsat %>%
              select(grid_id, mean_GHMI = mean), by = c("grid" = "grid_id")) %>%
  left_join(., taxonomy, by="species")


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
#there is insufficient peer-reviewed information on their diet) using the species in the filtered_5 dataset
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

# Example species and grid from suspicious list, e.g. Hypena scabra at grid 68
species_of_interest <- "Lambdina fervidaria"
grid_of_interest <- 68

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

# Basic stats
summary(obs$day_of_year)
cat("Number of observations:", nrow(obs), "\n")
cat("Max observed day:", max(obs$day_of_year), "\n")

# Histogram
ggplot(obs, aes(x = day_of_year)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle(paste("Day of year distribution for", species_of_interest, "in grid", grid_of_interest))

# Print phenology estimates for reference
phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species == species_of_interest, grid == grid_of_interest) %>%
  select(onset, median, offset, duration) %>%
  print()

# For this sample species, n isn't terribly low so it may not be the issue. We are likely getting an estimate over 
#365 because the data are left-skewed, so the Weibull distribution tries to fit this shape and goes over 
#the 365 day mark. Let's see if this is happening with the other weird estimates: 


# View results for suspicious species and grids only
suspicious_species_grids <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(offset > 365 | duration > 365) %>%
  select(species, grid) %>%
  distinct()


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
print(skewness_suspicious, n=21)

#Negative skewness values indicate left-skewness (tail on left side). Positive skewness means right-skew.
#Most of the weird phenology estimates are left-skewed. Many of the largest left (negative) skews are 
#with relatively small sample sizes (n_obs). So it looks like a low sample size and strong negative skew = high risk 
#of offset/duration > 365 days. Larger sample size can reduce overestimation, but left skewness alone can 
#push estimates past 365. We do see some moderate negative skews with moderate n. This suggests that sample 
#size helps but skew can still produce weird estimates. In these cases, the right skew is causing the right 
#tail of the Weibull distribution to stretch past the 365 day mark, leading to overestimation of the offset/
#duration values. In conclusion, overestimation can be caused by: left or right skew, year-round observations,
#and low sample size. Larger sample size can reduce some of the overestimation but not entirely prevent it. 


# Let's investigate the right skew to confirm  

# Specify species-grid combo to investigate the right skew
sp <- "Clogmia albipunctatus"
this_grid <- 16896

# Pull observations for this species-grid
obs <- filtered_5 %>%
  filter(species == sp, grid_id == this_grid) %>%
  mutate(day_of_year = as.integer(yday(eventDate))) %>%
  filter(day_of_year > 0)

# Summary stats
print(summary(obs$day_of_year))
cat("Number of observations:", nrow(obs), "\n")

# Histogram of day-of-year
p <- ggplot(obs, aes(x = day_of_year)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  ggtitle(paste0("Day of year distribution for ", sp, " in grid ", this_grid))
print(p)

# Pull phenology estimate for just this species-grid
phenos <- phenology_estimates_all_species_each_grid_with_GHMI %>%
  filter(species == sp, grid == this_grid)
print(phenos)


#The right skew is actually quite weak. Clogmia albipunctatus in grid 16896 shows two peaks
#with continuous activity between peaks. Because of this weakly bimodal distribution, Weibull is treating
#these peaks as one long season. So right skew causes estimation issues, but the real issue seems to be 
#continuous (year-round) activity and bimodality. 


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

# Now looking at how many of my suspicious estimates were bimodal
suspicious_bimodal <- overestimates %>%
  left_join(bimodality_results, 
            by = c("species" = "species", "grid" = "grid_id")) %>%
  filter(is_bimodal == TRUE)

# View how many suspicious estimates are also bimodal
print(suspicious_bimodal)
#Only 1 of the 13 suspicious estimates were bimodal. 

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


# We'll compare this case of bimodality with overestimation to the rest of the cases of bimodality
# that didn't cause overestimation in my data set. 


# Identify all bimodal cases
all_bimodal <- bimodality_results %>%
  filter(is_bimodal == TRUE)


# Flag overestimated bimodal cases
all_bimodal <- all_bimodal %>%
  mutate(overestimated = ifelse(
    paste(species, grid_id) %in% paste(suspicious_bimodal$species, suspicious_bimodal$grid),
    TRUE, FALSE
  ))


# Calculate peak separation for all bimodal cases. Ex: difference between two highest peaks in 
#day_of_year histogram
peak_separation_data <- filtered_5 %>%
  filter(paste(species, grid_id) %in% paste(all_bimodal$species, all_bimodal$grid_id)) %>%
  mutate(day_of_year = lubridate::yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  group_by(species, grid_id) %>%
  summarise(
    peak_separation = {
      h <- hist(day_of_year, plot = FALSE, breaks = seq(0, 365, by = 5))$counts
      peaks <- order(h, decreasing = TRUE)[1:2]   # indices of two largest peaks
      abs(diff(peaks) * 5)                        # convert to days (binwidth = 5)
    },
    .groups = "drop"
  )

# Merge peak separation into bimodal dataset
all_bimodal <- all_bimodal %>%
  left_join(peak_separation_data, by = c("species" = "species", "grid_id" = "grid_id"))


# Get offsets from phenology estimates
all_bimodal <- all_bimodal %>%
  select(species, grid_id, peak_separation, overestimated)%>%
  left_join(
    phenology_estimates_all_species_each_grid_with_GHMI %>%
      select(species, grid, offset),
    by = c("species" = "species", "grid_id" = "grid")
  )

# Plot it
ggplot(all_bimodal, aes(x = peak_separation, y = offset, color = overestimated)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("gray", "red")) +
  labs(title = "Phenology duration vs. peak separation",
       x = "Peak separation (days)",
       y = "Estimated offset (days)",
       color = "Overestimated Offset/Bimodal") +
  theme_minimal()


# Summary stats for comparison
all_bimodal %>%
  group_by(overestimated) %>%
  summarise(
    mean_peak_sep = mean(peak_separation, na.rm = TRUE),
    mean_duration  = mean(offset, na.rm = TRUE),
    n_cases        = n(),
    .groups = "drop"
  ) %>%
  print()

#This plot shows that, generally, as the amount of separation between peaks in a bimodal distribution 
#increases, the estimated offset of species-grid combos with bimodal distribution increases as well. 
#Offset estimates are also higher when peak separation is very low. Our case of bimodal distribution
#that produced biologically impossible estimates either had very high or very low peak separation. 
#This figure shows that peak separation can contribute to overestimation, as there is a 
#trend toward higher estimation with more extreme (high/low) peak separation, but that bimodality and 
#peak separation alone do not cause overestimation. 

# Let's look at what else could cause a bimodal distribution to overestimate: 

# Subset only bimodal species-grid combos
bimodal_cases <- bimodality_results %>%
  filter(is_bimodal == TRUE)

# Calculate metrics per species-grid
bimodal_metrics <- filtered_5 %>%
  mutate(day_of_year = yday(eventDate)) %>%
  filter(day_of_year > 0) %>%
  semi_join(
    bimodal_cases %>% select(species, grid_id),
    by = c("species" = "species", "grid_id" = "grid_id")
  ) %>%
  group_by(species, grid_id) %>%
  summarise(
    n_obs       = n(),
    skewness_doy = skewness(day_of_year, na.rm = TRUE),
    min_doy     = min(day_of_year),
    max_doy     = max(day_of_year),
    peak_sep    = max(day_of_year) - min(day_of_year), # rough proxy for peak separation
    .groups = "drop"
  ) %>%
  # Join in phenology estimates
  left_join(
    phenology_estimates_all_species_each_grid_with_GHMI %>%
      select(species, grid, onset, offset, duration),
    by = c("species" = "species", "grid_id" = "grid")
  ) %>%
  # Flag overestimated cases
  mutate(overestimated = duration > 365 | offset > 365)

print(bimodal_metrics)

# Visualize overestimated vs rest. This plot shows the total range of activity throughout the year.
ggplot(bimodal_metrics, aes(x = peak_sep, y = offset, color = overestimated)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("gray", "red")) +
  labs(title = "Offset vs. Peak Separation in Bimodal Species-Grid Combos",
       x = "Peak Separation (days, min-max)",
       y = "Estimated Offset (days)",
       color = "Overestimated") +
  theme_minimal()
#In this plot, bimodal distributions with longer peak separation (longer range of observations, more 
#likely to be year-round activity) have higher offset estimates. The bimodal distributions with overestimations
#have very high peak separation. In these cases, they have nearly year-round activity, which inflates the 
#offset. 

# Summarize 
bimodal_metrics %>%
  group_by(overestimated) %>%
  summarise(
    mean_peak_sep = mean(peak_sep, na.rm = TRUE),
    mean_offset   = mean(offset, na.rm = TRUE),
    mean_duration = mean(duration, na.rm = TRUE),
    mean_n_obs    = mean(n_obs, na.rm = TRUE),
    mean_skew     = mean(skewness_doy, na.rm = TRUE),
    n_cases       = n(),
    .groups = "drop"
  ) %>%
  print()
#sample size isn't causing overestimation here. 


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

#how many grids per species 
grid_per_spec <- phenology_filtered %>%
  group_by (species)%>%
  summarise(grid_per_spec = n_distinct(grid))
grid_per_spec #Papilio glaucus found in most grids (386 grids)



#############################################################################################################

#Save it: 
saveRDS(phenology_filtered, "Data/phenology_estimates_data_for_analysis.rds") 




