# GAM analysis for Pollinator Flight Period

########################################################################################################### 

# Load Packages 
library(readr)
library(tidyverse)
library(mgcv)
library(broom)
library(sf)
library(MuMIn)
library(patchwork)
library(gratia)
set.seed(120)

# Read in data 
fp_data <- readRDS("Data/phenology_estimates_data_for_analysis.rds") #phenology estimate data
climate <- read.csv("Data/Spatial Data/Climate_Data/climate_summarized.csv")
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") #geoJSON of 

# add climate data to fp_data
fp_data <- left_join(fp_data, climate %>% select(grid_id, temp, prcp), by=c("grid"="grid_id"))
                                                                                             #NA24
filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data, reading this in 
#so we can see how many observations we used for analysis after all of the filtering 

########################################################################################################### 

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

# Now add the mean lon and lat to our fp_data
fp_data <- left_join(fp_data, grids_centroids, by=c("grid"="grid_id"))%>%
  mutate(species = as.factor(species))

# Clean the data so we just have records with duration flight period
fp_data_duration <- fp_data %>%
  filter(complete.cases(duration)) %>%
  mutate(species = as.factor(species))

# Let's see the distribution of duration flight period
hist(fp_data_duration$duration)
# looks close to normal!

# Pull only relevant data for the models
fp_rel <- fp_data %>%
  dplyr::select(duration, onset, offset, mean_GHMI, temp, prcp, lon, lat)  

# Lets get a summary of the data so we can see each value's distribution
summary(fp_rel)

# let's see if there is any multicolinearity
cor(fp_rel, method="pearson")
# I don't see anything too concerning here


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
fp_data_summary <- fp_data %>%
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
results <- fp_data %>% group_by(species) %>% group_modify(~check_species(.x))
print(results, n=107)


# Removing species with a range less than 0.3 and a standard deviation less than 0.10 so that they
# can be properly fitted to GAMs
flagged_species <- results %>%
  filter(range < 0.3 | sd < 0.1)
fp_data <- fp_data %>%
  filter(!species %in% flagged_species$species)

summary(fp_data$mean_GHMI)
sd(fp_data$mean_GHMI)

# Save it 
saveRDS(fp_data, "Data/final_phenology_df_for_analysis.RDS")



# Quick visualization 
ggplot(fp_data, aes(x = mean_GHMI)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size=14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in the Data Set After Filtering")

# Save it 
ggsave("Figures/distribution_of_GHMI_values_in_GAM_dataset.png", width=6, height=6, units="in")

# Produce a table summarizing the species being used in the GAMs, the number of grid cells that 
# each species has, the number of observations for each species, and the range of GHMI values 
# across all grid cells for each species 
data_for_models_summary <- fp_data %>%
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

# Export to CSV for easy sharing/use in Excel
write.csv(data_for_models_summary, "Data/data_for_models_summary.csv")

### Look at the make-up of our data after this final level of filtering: 
#Look at new species and grids  
length(unique(fp_data$species)) #52 species 
length(unique(fp_data$family)) #20 families  
length(unique(fp_data$order)) #4 orders 
length(unique(fp_data$grid)) #756 grids 
species_per_order <- fp_data %>%
  group_by(order) %>%
  summarise(n_species = n_distinct(species)) %>%
  arrange(desc(n_species))%>%
  print()


# Also want to know how many pollinator observations we ended up using in this study
obs_used <- filtered_5 %>%  #filter raw observation data to only include grid/species combos used in fp_data
  semi_join(fp_data, by = c("species" = "species", "grid_id" = "grid"))

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
variable_summaries <- fp_data %>%
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

########################################################################################################### 

# Explore data relationships ----------------------------------------------

# Now let's explore the distribution of each variable
fp_long <- fp_rel %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot histograms using facet_wrap
ggplot(fp_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_classic() +
  labs(x = NULL, y = "Count", title = "Histograms of All Variables") +
  theme(strip.text = element_text(size = 10))
# since the response will determine the family that we add to the model, I am paying
# close attention to duration, onset, and offset. Onset and offset look close to 
# normally distributed, but duration looks positively skewed. We will keep that in mind
# when determining the modeling.


########################################################################################################### 

# GAM Model Testing -------------------------------------------------------

# We will do model testing on all data with species as a random effect, and
# on select species to see if the models pass the tests

# Let's find a species with a decent amount of data
fp_data_sp <- fp_data %>%
  group_by(species) %>%
  summarise(count=n()) %>%
  arrange(desc(count))%>%
  print(n=300)

# Bombus impatiens has the most data, so let's do that species for one
Bombus_impatiens <- fp_data %>%
  filter(species == "Bombus impatiens")

# Let's also do a species that has less data like Halictus ligatus
Halictus_ligatus <- fp_data %>% 
  filter(species == "Halictus ligatus")





## Duration -------------------------------------------------------

# Let's do some model testing to see which model will best fit the data
# we will start with modeling species as a random effect

# Comparing gaussian and gamma models to see which is a better fit
mod_gauss_duration <- gam(duration ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                          family = gaussian(),
                          method = "REML",
                          data = fp_data)
mod_gamma_duration <- gam(duration ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                          family = Gamma(link = "log"),
                          method = "REML",
                          data = fp_data)
AIC(mod_gauss_duration, mod_gamma_duration) #gaussian has lower AIC
gam.check(mod_gauss_duration)
#Gaussian model is a better fit. We'll use this for duration models 

# Start with a null model
gam_null <- gam(duration ~ 1 + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_null)
gam.check(gam_null)

# let's try another null model that controls for temperature and precipitation
gam_null_temp_prcp <- gam(duration ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_null_temp_prcp)
gam.check(gam_null_temp_prcp)

# Now add mean_GHMI
gam_1 <- gam(duration ~ mean_GHMI + s(temp) + s(prcp) +
               s(lat, lon, k = 170, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1) #GHMI is a not sig. predictor of duration (p=0.883), but species is sig. (<2e-16) 
#and  so is lat/long(<2e-16) and temp (9.81e-07)
gam.check(gam_1)
gam.check(gam_1)$k.check
#The mean GHMI seems to explain very little deviance in the model, species and lat/long explain much 
#more variation. Model fit not much higher than null model. 

draw(gam_1, select = "s(temp)") #taking a closer look at the relationship between temp and duration
# Lots of variation, but overall duration is generally increasing at higher temperatures

# Let's try to have mean_GHMI as a smooth term
gam_1_smooth <- gam(duration ~ s(mean_GHMI, k = 50) + s(temp) + s(prcp) +
               s(lat, lon, k = 100, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1_smooth) #GHMI is not a sig. predictor of duration (p=0.802), but species is sig. (<2e-16) 
# and so is lat/long(<2e-16) and temp (7.37e-06)
gam.check(gam_1_smooth)
gam.check(gam_1_smooth)$k.check
#The EDF for GMHI is 1, so the model is saying there is no non-linear relationship between duration and GMHI

AIC(gam_1, gam_1_smooth)

# Now let's see how they rank
aic_null_dur <- AIC(gam_null)
print(aic_null_dur)
aic_null_dur_tp <- AIC(gam_null_temp_prcp)
print(aic_null_dur_tp)
aic_full_dur <- AIC(gam_1)
print(aic_full_dur)
aic_full_dur_smooth <- AIC(gam_1_smooth)
print(aic_full_dur_smooth)
#Adding temperature and precipitation does help
#However, we find null model performed better so GMHI does not help explain duration 

# Getting delta AIC 
aic_values_dur <- c(
  null_temp_prcp = aic_null_dur_tp,
  GHMI = aic_full_dur,
  GHMI_smooth = aic_full_dur_smooth
)
delta_aic_dur <- aic_values_dur - min(aic_values_dur)
delta_aic_dur




# Let's repeat for an individual species
gam_null_bi <- gam(duration ~ 1  + s(temp) + s(prcp) + s(lat, lon, k = 10, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Bombus_impatiens)
summary(gam_null_bi)
gam.check(gam_null_bi)


# Now add mean_GHMI
gam_1_bi <- gam(duration ~ mean_GHMI + s(temp) + s(prcp) +
                  s(lat, lon, bs="tp"), 
                family = gaussian(),
                method = "REML",
                data=Bombus_impatiens)
summary(gam_1_bi) #GHMI sig. predictor of duration (p=0.0306)
gam.check(gam_1_bi)
gam.check(gam_1_bi)$k.check
#The mean GHMI seems to explain very little deviance in the model

gam_1_bi_smooth <- gam(duration ~ s(mean_GHMI) + s(temp) + s(prcp) +
                  s(lat, lon, bs="tp"), 
                family = gaussian(),
                method = "REML",
                data=Bombus_impatiens)
summary(gam_1_bi_smooth) #GHMI sig. predictor of duration (p=0.0306)
gam.check(gam_1_bi_smooth)

# Now let's see how they rank
# We will be using AICc due to the small sample size
AICc(gam_null_bi)
AICc(gam_1_bi)
#So in this case the adding GHMI improved model fit, but only slightly 

# Let's try one more time for the species with little data
gam_null_hl <- gam(duration ~ 1 + mean_GHMI + s(temp) + s(prcp) + s(lat, lon, k=5, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Halictus_ligatus)
summary(gam_null_hl)
gam.check(gam_null_hl)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl <- gam(duration ~ mean_GHMI +
                  s(lat, lon, k=5, bs="tp"), 
                family = gaussian(),
                method = "REML",
                data=Halictus_ligatus)
summary(gam_1_hl)
gam.check(gam_1_hl)
gam.check(gam_1_hl)$k.check
#The mean GHMI seems to explain very little deviance in the model

# Now let's see how they rank
AICc(gam_null_hl)
AICc(gam_1_hl)
# So in this case the null model performed better, so GHMI does not help explain duration

# What I have learned is that we will need to specify different k values for lat/lon based
# on the number of grids for the species and that GMHI is not likely to explain duration.






## Onset -------------------------------------------------------

# Let's take a closer look at the distribution of onset
hist(fp_data$onset)
#It looks normal 

# Start with modeling species as a random effect

# Comparing gaussian and gamma models to see which is a better fit
mod_gauss_onset <- gam(onset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = gaussian(),
                       method = "REML",
                       data = fp_data)
mod_gamma_onset <- gam(onset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = Gamma(link = "log"),
                       method = "REML",
                       data = fp_data)
AIC(mod_gauss_onset, mod_gamma_onset) #gaussian has lower AIC
gam.check(mod_gauss_onset)
#Gaussian model is a better fit. We'll use this for onset models 

# Start with a null model
gam_null_on <- gam(onset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                   family = gaussian(),
                   method = "REML",
                   data=fp_data)
summary(gam_null_on)
gam.check(gam_null_on)


# Now add mean_GHMI
gam_1_on <- gam(onset ~ mean_GHMI + s(temp) + s(prcp) +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_1_on)
gam.check(gam_1_on)
# The mean GHMI significantly predicts onset (p = 2e-04)
draw(gam_1_on, select = "s(temp)") #taking a closer look at the relationship between temp and onset. 
#Interpretation: Onset is earliest at intermediate temperatures, but delayed at both low and 
#high temperature extremes.

# let's try it with a smooth term for GHMI
gam_1_on_smooth <- gam(onset ~ s(mean_GHMI, k=20) + s(temp) + s(prcp) +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_1_on_smooth)
gam.check(gam_1_on_smooth)
# The EDF is 1 for GHMI, so the model is treating it as a linear term

# Now let's see how they rank
aic_null_on <- AIC(gam_null_on)
print(aic_null_on)
aic_full_on <- AIC(gam_1_on)
print(aic_full_on)
aic_full_on_smooth <- AIC(gam_1_on_smooth)
print(aic_full_on_smooth)
#In this case, adding GHMI increases model fit but by very little  

# Getting delta AIC
aic_values_on <- c(
  null = aic_null_on,
  GHMI = aic_full_on
)

delta_aic_on <- aic_values_on - min(aic_values_on)
delta_aic_on

#Strong evidence that adding GHMI improves model fit for onset  



# Let's repeat for an individual species
gam_null_bi_on <- gam(onset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 10, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=Bombus_impatiens)
summary(gam_null_bi_on)
gam.check(gam_null_bi_on)


# Now add mean_GHMI
gam_1_bi_on <- gam(onset ~ mean_GHMI + s(temp) + s(prcp) +
                     s(lat, lon, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Bombus_impatiens)
summary(gam_1_bi_on) #GHMI not a sig. predictor of onset 
gam.check(gam_1_bi_on)
#While mean_GHMI is not significant, it does improve deviance explained

# Now let's see how they rank
AICc(gam_null_bi_on)
AICc(gam_1_bi_on)
#Adding GHMI decreases model fit 

# Let's try one more time for the species with little data
gam_null_hl_on <- gam(onset ~ 1 + s(lat, lon, k=5, bs="tp"), 
                      family = gaussian(),
                      method = "REML",
                      data=Halictus_ligatus)
summary(gam_null_hl_on)
gam.check(gam_null_hl_on)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl_on <- gam(onset ~ mean_GHMI +
                     s(lat, lon, k=5, bs="tp"), 
                   family = gaussian(),
                   method = "REML",
                   data=Halictus_ligatus)
summary(gam_1_hl_on) #GHMI not a sig. predictor of onset 
gam.check(gam_1_hl_on)
#The GHMI does improve deviance explained in the model

# Now let's see how they rank
AICc(gam_null_hl_on)
AICc(gam_1_hl_on)
#The null model does better in this case








## Offset -------------------------------------------------------

# Let's take a closer look at the distribution of offset
hist(fp_data$offset)
#It looks normal, so guassian is probably going to be the best distribution

# Comparing gaussian and gamma models to see which is a better fit, to make sure
mod_gauss_offset <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = gaussian(),
                       method = "REML",
                       data = fp_data)
mod_gamma_offset <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = Gamma(link = "log"),
                       method = "REML",
                       data = fp_data)
AIC(mod_gauss_offset, mod_gamma_offset) #gaussian has lower AIC
gam.check(mod_gauss_offset)
#Gaussian model is a better fit. We'll use this for offset models 

# Start with modeling species as a random effect

# Start with a null model
gam_null_off <- gam(offset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                    family = gaussian(),
                    method = "REML",
                    data=fp_data)
summary(gam_null_off)
gam.check(gam_null_off)


# Now add mean_GHMI
gam_1_off <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) +
                   s(lat, lon, k = 170, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data)
summary(gam_1_off) #GHMI is a sig. predictor of offset (p=3.27e-09)
gam.check(gam_1_off) #not much difference in deviance explained between this model and the null 

# let's try including GHMI as a smooth term
# Now add mean_GHMI
gam_1_off_smooth <- gam(offset ~ s(mean_GHMI) + s(temp) + s(prcp) +
                   s(lat, lon, k = 170, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data)
summary(gam_1_off_smooth) 
gam.check(gam_1_off_smooth)
# effective degrees of freedom for GHMI is 1, so we should treat this as a linear term

#Visualizing the spatial smooth (lat/long)
plot(gam_1_off, select = 1)

# Now let's see how they rank
aic_null_off <- AIC(gam_null_off)
print(aic_null_off)
aic_full_off <- AIC(gam_1_off)
print(aic_full_off)
aic_full_off_smooth <- AIC(gam_1_off_smooth)
print(aic_full_off_smooth)
#In this case, adding GHMI increases model fit 

# Getting delta AIC 
aic_values_off <- c(
  null = aic_null_off,
  GHMI = aic_full_off
)
delta_aic_off <- aic_values_off - min(aic_values_off)
delta_aic_off
#Adding GHMI strongly improves model fit 



# Let's repeat for an individual species
gam_null_bi_off <- gam(offset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 10, bs="tp"), 
                       family = gaussian(),
                       method = "REML",
                       data=Bombus_impatiens)
summary(gam_null_bi_off)
gam.check(gam_null_bi_off)


# Now add mean_GHMI
gam_1_bi_off <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) +
                      s(lat, lon, bs="tp"), 
                    family = gaussian(),
                    method = "REML",
                    data=Bombus_impatiens)
summary(gam_1_bi_off) #GHMI is a sig. predictor of onset (p=0.000159)
gam.check(gam_1_bi_off) #adding GHMI increases deviance explained 

# Now let's see how they rank
AICc(gam_null_bi_off)
AICc(gam_1_bi_off)
#GHMI does improve the model 

# Let's try one more time for the species with little data
gam_null_hl_off <- gam(offset ~ 1 + s(lat, lon, k=5, bs="tp"), 
                       family = gaussian(),
                       method = "REML",
                       data=Halictus_ligatus)
summary(gam_null_hl_off)
gam.check(gam_null_hl_off)
#In this case, we had to specify the k for lat/lon that was less than the degrees of freedom

# Now add mean_GHMI
gam_1_hl_off <- gam(offset ~ mean_GHMI +
                      s(lat, lon, k=5, bs="tp"), 
                    family = gaussian(),
                    method = "REML",
                    data=Halictus_ligatus)
summary(gam_1_hl_off) #GHMI is not a sig. predictor of onset 
gam.check(gam_1_hl_off)
# The GHMI does improve deviance explained in the model. Model seems to be overfitted. 

# Now let's see how they rank
AICc(gam_null_hl_off)
AICc(gam_1_hl_off)
#The ghmi model does better but these results are odd. Model is probably overfitted. 














########################################################################################################### 
################# GAM models by species ---------------------------------------------------

# With all the knowledge from model testing, we are ready to create a function to 
# examine GAM models by species

# For each species, we will run 6 GAM models for duration, onset, and offset, where
# we have one NULL model and one model with GMHI. Then we will calculate the model outputs
# and the model weight of models of interest when compared to NULL models

gam_by_species <- function(species_name){
  
  # filter the fp_data to that species
  fp_data_sp <- fp_data %>%
    filter(species == species_name)
  
  # pull taxonomic info (assumes order, family, genus are consistent per species)
  tax_info <- fp_data_sp %>%
    distinct(order, family, genus, species) %>%
    slice(1)
  
  # define k value
  k_val <- 20
  
  ### duration ###
  gam_null_dur <- gam(duration ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(), method = "REML", data=fp_data_sp)
  gam_ghmi_dur <- gam(duration ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(), method = "REML", data=fp_data_sp)
  sum_gam_null_dur <- summary(gam_null_dur)
  sum_gam_ghmi_dur <- summary(gam_ghmi_dur)
  pval_spatial_dur <- sum_gam_ghmi_dur$s.table["s(lat,lon)", "p-value"]
  
  aic_val_dur <- c(AICc(gam_null_dur), AICc(gam_ghmi_dur))
  delta_aic_dur <- aic_val_dur - min(aic_val_dur)
  weights_dur <- exp(-0.5 * delta_aic_dur) / sum(exp(-0.5 * delta_aic_dur))
  
  ### onset ###
  gam_null_on <- gam(onset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                     family = gaussian(), method = "REML", data=fp_data_sp)
  gam_ghmi_on <- gam(onset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                     family = gaussian(), method = "REML", data=fp_data_sp)
  sum_gam_null_on <- summary(gam_null_on)
  sum_gam_ghmi_on <- summary(gam_ghmi_on)
  pval_spatial_on <- sum_gam_ghmi_on$s.table["s(lat,lon)", "p-value"]
  
  aic_val_on <- c(AICc(gam_null_on), AICc(gam_ghmi_on))
  delta_aic_on <- aic_val_on - min(aic_val_on)
  weights_on <- exp(-0.5 * delta_aic_on) / sum(exp(-0.5 * delta_aic_on))
  
  ### offset ###
  gam_null_off <- gam(offset ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(), method = "REML", data=fp_data_sp)
  gam_ghmi_off <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
                      family = gaussian(), method = "REML", data=fp_data_sp)
  sum_gam_null_off <- summary(gam_null_off)
  sum_gam_ghmi_off <- summary(gam_ghmi_off)
  pval_spatial_off <- sum_gam_ghmi_off$s.table["s(lat,lon)", "p-value"]
  
  aic_val_off <- c(AICc(gam_null_off), AICc(gam_ghmi_off))
  delta_aic_off <- aic_val_off - min(aic_val_off)
  weights_off <- exp(-0.5 * delta_aic_off) / sum(exp(-0.5 * delta_aic_off))
  
  ### summary table ###
  gam_table <- data.frame(
    order = tax_info$order,
    family = tax_info$family,
    genus = tax_info$genus,
    species = tax_info$species,
    model = c("duration", "onset", "offset"),
    GHMI_estimate = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Estimate"],
                      sum_gam_ghmi_on$p.table["mean_GHMI", "Estimate"],
                      sum_gam_ghmi_off$p.table["mean_GHMI", "Estimate"]),
    GHMI_se = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Std. Error"],
                sum_gam_ghmi_on$p.table["mean_GHMI", "Std. Error"],
                sum_gam_ghmi_off$p.table["mean_GHMI", "Std. Error"]),
    GHMI_tval = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "t value"],
                  sum_gam_ghmi_on$p.table["mean_GHMI", "t value"],
                  sum_gam_ghmi_off$p.table["mean_GHMI", "t value"]),
    GHMI_pval = c(sum_gam_ghmi_dur$p.table["mean_GHMI", "Pr(>|t|)"],
                  sum_gam_ghmi_on$p.table["mean_GHMI", "Pr(>|t|)"],
                  sum_gam_ghmi_off$p.table["mean_GHMI", "Pr(>|t|)"]),
    adj_r2 = c(sum_gam_ghmi_dur$r.sq,
               sum_gam_ghmi_on$r.sq,
               sum_gam_ghmi_off$r.sq),
    dev_exp = c(sum_gam_ghmi_dur$dev.expl,
                sum_gam_ghmi_on$dev.expl,
                sum_gam_ghmi_off$dev.expl),
    dev_exp_diff_comp_null = c(sum_gam_ghmi_dur$dev.expl - sum_gam_null_dur$dev.expl,
                               sum_gam_ghmi_on$dev.expl - sum_gam_null_on$dev.expl,
                               sum_gam_ghmi_off$dev.expl - sum_gam_null_off$dev.expl),
    sample_size = c(sum_gam_ghmi_dur$n,
                    sum_gam_ghmi_on$n,
                    sum_gam_ghmi_off$n),
    model_weight_comp_null = c(weights_dur[2],
                               weights_on[2],
                               weights_off[2]),
    spatial_pval = c(pval_spatial_dur, pval_spatial_on, pval_spatial_off), 
    delta_AIC = c(delta_aic_dur[2],
                  delta_aic_on[2],
                  delta_aic_off[2])
  )
  
  return(list(
    summary_table = gam_table,
    models = list(
      duration = gam_ghmi_dur,
      onset = gam_ghmi_on,
      offset = gam_ghmi_off
    )
  ))
}


# Get list of species 
count_sp <- fp_data %>%
  group_by(species) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
species_list <- as.vector(count_sp[!count_sp$count<6,]$species)

# Now use the function to get model outputs for all species
species_gam_full <- setNames(lapply(species_list, gam_by_species), species_list)

# Save for use in other scripts
saveRDS(species_gam_full, "Data/GAM_results/species_gam_full_w_climate.rds")

# Extract summary tables into a single dataframe
species_gam <- bind_rows(lapply(species_gam_full, function(x) x$summary_table))

# Save it 
write_csv(species_gam, "Data/GAM_results/gam_results_by_species_w_climate.csv")



# Table with only species that have p-values < 0.05
species_gam_significant_p_only <- species_gam %>%
  filter(GHMI_pval < 0.05)


# Save it 
write_csv(species_gam_significant_p_only, "Data/GAM_results/species_gam_significant_p_only_w_climate.csv")


# Look at sample sizes to compare sample size of sig species versus non-sig species 
species_gam <- species_gam %>%
  mutate(sig_flag = ifelse(GHMI_pval < 0.05, "significant", "not_significant"))
sample_size_summary <- species_gam %>%
  group_by(model, sig_flag) %>%
  summarise(
    mean_n = mean(sample_size, na.rm = TRUE),
    median_n = median(sample_size, na.rm = TRUE),
    min_n = min(sample_size, na.rm = TRUE),
    max_n = max(sample_size, na.rm = TRUE),
    n_species = n(),
    .groups = "drop"
  )%>%
  print()

# Look at orders represented in the data set (both sig. and not sig. models)
species_per_order_gam <- species_gam %>%
  group_by(model, sig_flag, order) %>%
  summarise(
    n_species = n_distinct(species),
    .groups = "drop"
  ) %>%
  arrange(model, sig_flag, desc(n_species)) %>%
  print()


# Looking at the direction of the estimates of only significant models

effects_all_sig <- species_gam_significant_p_only %>%
  select(species, model, GHMI_estimate)
print(effects_all_sig)

effects_all_sig%>%
  filter(model=="onset")
effects_all_sig%>%
  filter(model=="offset")


########################################################################################################### 
############################## Interpreting Model Outputs: 
### Filter for significant p-value results, to compare: 

length(unique(species_gam_significant_p_only$species)) #19 species sig. 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="onset"])) #10 sig. for onset 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="offset"])) #12 sig. for offset
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="duration"])) #6 sig. for duration 




### Looking at the geographic location (lat/long) smoother for significant models only: 

# Count models per species by spatial effect 
spatial_summary <- species_gam_significant_p_only %>%
  group_by(species) %>%
  summarize(
    models_with_spatial_effect = sum(spatial_pval < 0.05),  #(p-value of lat/long is significant)
    models_without_spatial_effect = sum(spatial_pval >= 0.05), #(p-value of lat/long is not significant)
    total_models = n()
  ) %>%
  arrange(desc(models_with_spatial_effect))

# Species where lat/long is not sig. 
species_no_spatial_effect <- spatial_summary %>%
  filter(models_with_spatial_effect == 0) %>%
  pull(species)

print(species_no_spatial_effect)

#For the following 10 species, spatial location is not influencing phenology beyond 
#what GHMI explains: Bombus bimaculatus, Bombus impatiens, Epargyreus clarus,     
#Marimatha nigrofimbria, Papilio troilus, Phyciodes tharos, Pieris rapae, 
#Pyrrharctia isabella, Scopula limboundata, Strymon melinus


#Species where lat/long is sig.
species_with_spatial_effect <- spatial_summary %>%
  filter(models_with_spatial_effect > 0) %>%
  pull(species)
print(species_with_spatial_effect)

# For the following 11 species, at lat/long is sig. influencing at least one phenology variable 
# beyond what GHMI explains: Apis mellifera, Chauliognathus marginatus, Danaus plexippus,         
# Hylephila phyleus, Papilio glaucus, Tetraopes tetrophthalmus, Xylocopa virginica, 
# Battus philenor, Bombus griseocollis, Bombus pensylvanicus, Protographium marcellus 

# Looking at which models have spatial significance for each species: 
sig_spatial_models <- species_gam_significant_p_only %>%
  filter(species %in% species_with_spatial_effect, spatial_pval < 0.05) %>%
  select(species, model, spatial_pval) %>%
  arrange(species, model)

sig_spatial_summary <- sig_spatial_models %>%
  group_by(species) %>%
  summarize(significant_models = paste(model, collapse = ", "))

print(sig_spatial_summary, n=14)

# Of the 11 species, 9 showed spatial significance for onset, 6 for offset, and 3 
# for duration. 








### Looking at the effect of GHMI on flight period, and the direction of this effect 
effects <- species_gam_significant_p_only %>%
  select(species, model, GHMI_estimate)
print(effects)

# Example: Bombus impatiens, offset increases by ~35 days for every 1-unit increase in GHMI. This means 
# that offset occurs ~35 days later in fully anthropogenized areas compared to natural areas. 

#Look at GHMI effect for onset, offset, and duration
effects_onset <- effects %>%
  filter(model=="onset")%>%
  arrange(desc(GHMI_estimate))
print(effects_onset)

effects_offset <- effects %>%
  filter(model=="offset")%>%
  arrange(desc(GHMI_estimate))
print(effects_offset)

effects_duration <- effects %>%
  filter(model=="duration")%>%
  arrange(desc(GHMI_estimate))
print(effects_duration)

#Let's figure out why Clogmia albipunctatus has a very large duration effect size 
C_albipunctatus_data <- fp_data %>%
  filter(species == "Clogmia albipunctatus") %>%
  summarise(range = diff(range(mean_GHMI)),
            sd    = sd(mean_GHMI))%>%
  print()
# SD and range do not seem problematic 

#Look at model 
model <- species_gam_full[["Clogmia albipunctatus"]]$models$duration
plot(model, pages = 1)
gam.check(model)

#Look at Cook's distance 
cooks <- cooks.distance(model)
cooks
plot(cooks, type="h"); abline(h = 4/length(cooks), col="red")
#It looks like one duration estimate for a grid cell is an outlier and throwing off the estimate. 
#This is likely able to inflate the estimate so much because of the small sample size (n=9) used
#in this model. 

#Refit the model without this point to see if it changes the results of the original model by much 
fp_data_clogmia <- fp_data %>%
  filter(species == "Clogmia albipunctatus")

model <- species_gam_full[["Clogmia albipunctatus"]]$models$duration
cooks <- cooks.distance(model)
i_bad <- which.max(cooks)
fp_data_clogmia_no1 <- fp_data_clogmia[-i_bad, ]

k_val <- ifelse(nrow(fp_data_clogmia_no1) <= 20,
                nrow(fp_data_clogmia_no1) - 1,
                20)

m_no1 <- gam(duration ~ mean_GHMI + s(lat, lon, k = k_val, bs = "tp"),
             family = gaussian(),
             method = "REML",
             data = fp_data_clogmia_no1)

summary(m_no1)
gam.check(m_no1)
#Okay so the outlier didn't change the model results by much. This means that this "extreme" 
#estimate is actually due to interpretation of days over a narrow range of GHMI values. For 
#every 1.0 increase in GHMI, predicted flight-period duration lengthens by about 500 days. However, 
#this species does not have the full range of GHMI values (only spans 0.38). So it's actually 
#0.38 × 500 ≈ 190 days. Worded as: The model estimates that flight-period duration increases by 
#roughly 500 days per 1.0-unit increase in GHMI, which translates to about 190 days over the 
#0.38-unit GHMI range actually observed for this species.


# Do a count of the effects and their direction 
interpret_ghmi_effect <- function(df) {
  df$GHMI_effect_interpretation <- NA_character_
  
  for (i in seq_len(nrow(df))) {
    est <- df$GHMI_estimate[i]
    model <- df$model[i]
    species <- df$species[i]
    
    if (model == "onset") {
      # For onset (Gamma + log link), exponentiate estimate to get multiplicative change
      perc_change <- (exp(est) - 1) * 100
      direction <- ifelse(perc_change < 0, "earlier", "later")
      perc_change_abs <- abs(round(perc_change, 1))
      interp <- paste0(
        species, ": Onset occurs about ", perc_change_abs, "% ", direction, " in fully urban vs natural areas."
      )
    } else {
      # For offset and duration (Gaussian), interpret estimate as days difference
      days_diff <- round(est, 1)
      direction <- ifelse(days_diff < 0, "earlier/shorter", "later/longer")
      days_diff_abs <- abs(days_diff)
      interp <- paste0(
        species, ": ", model, " is about ", days_diff_abs, " days ", direction, " in fully urban vs natural areas."
      )
    }
    
    df$GHMI_effect_interpretation[i] <- interp
  }
  
  return(df)
}



count_direction <- function(df) {
  # Create a helper column for direction based on estimate sign
  df$direction <- ifelse(df$GHMI_estimate < 0, "earlier_or_shorter", "later_or_longer")
  
  # Count how many species by model and direction
  summary_table <- table(df$model, df$direction)
  
  return(summary_table)
}

direction_counts <- count_direction(species_gam_significant)
print(direction_counts)










### Looking at model fit using adj R^2, adj R squared, and dev_exp

species_gam_significant_p_only %>%
  group_by(model) %>%
  summarise(
    min_r2 = min(adj_r2, na.rm = TRUE),
    max_r2 = max(adj_r2, na.rm = TRUE),
    mean_r2 = mean(adj_r2, na.rm = TRUE),
    min_dev = min(dev_exp, na.rm = TRUE),
    max_dev = max(dev_exp, na.rm = TRUE),
    mean_dev = mean(dev_exp, na.rm = TRUE)
  )

#Onset models, on average, have slightly better R² and deviance explained than offset or duration.
#However, these models still have a large range of deviance explained (lots of variation). 


# Deviance and adjusted r^2 explained per model, with species listed 
species_gam_significant_p_only %>%
  group_by(model) %>%
  arrange(model, desc(dev_exp)) %>%
  dplyr::select(model, species, dev_exp, adj_r2)%>%
  print(n=29)

# Now compare this list of models with the models that have sig. of the spatial smooth (lat/long): 
print(sig_spatial_summary)
#It looks like the onset models that do show spatial significance (Clogmia albipunctatus, Eristalis tenax, 
#Vespula squamosa) are also the strongest fitting models based on deviance explained and adjusted R² 
#(all >0.45 deviance and adj R²), with the exception of Xylocopa virginica 






### Now visualizing the significant models: 


# Separate species into what phenology variable GHMI sig. predicts, based on GAM models: 
onset_species <- c("Xylocopa virginica", "Bombus griseocollis", "Papilio troilus", 
                   "Eremnophila aureonotata", "Eristalis tenax", "Vespula squamosa", 
                   "Clogmia albipunctatus", "Helicoverpa zea")

offset_species <- c("Bombus impatiens", "Papilio glaucus", "Danaus plexippus", "Epargyreus clarus",
                    "Phyciodes tharos", "Hylephila phyleus", "Pyrrharctia isabella", "Battus philenor", 
                    "Tetraopes tetrophthalmus","Noctua pronuba", "Limenitis arthemis")

duration_species <- c("Xylocopa virginica", "Apis mellifera", "Pyrrharctia isabella", 
                      "Papilio troilus", "Noctua pronuba", "Clogmia albipunctatus")






# Build plot function
plot_model_group <- function(species_vec, model_name, ncol = 3) {
  plots <- lapply(species_vec, function(sp) {
    gam_obj <- species_gam_full[[sp]]$models[[model_name]]
    
    # Build a data frame over a range of GHMI values
    ghmi_vals <- tibble(
      mean_GHMI = seq(0, 1, length.out = 100),
      lat = mean(gam_obj$model$lat, na.rm = TRUE),  # use average lat/lon for other predictors
      lon = mean(gam_obj$model$lon, na.rm = TRUE)
    )
    
    # Predict partial effect
    preds <- predict(gam_obj, newdata = ghmi_vals, type = "link", se.fit = TRUE)
    
    plot_df <- ghmi_vals %>%
      mutate(
        fit = preds$fit,
        se = preds$se.fit,
        upper = fit + 2 * se,
        lower = fit - 2 * se
      )
    
    # Plot manually
    ggplot(plot_df, aes(x = mean_GHMI, y = fit)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.4) +
      labs(title = sp,
           subtitle = paste0("Effect of Urbanization on ", model_name),
           x = "GHMI (Urbanization Index)",
           y = "Partial Effect (link scale)") +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        axis.title = element_text(size = 9)
      )
  })
  
  wrap_plots(plots, ncol = ncol)
}




plot_model_group(onset_species, "onset")
plot_model_group(offset_species, "offset")
plot_model_group(duration_species, "duration")

# Save the plots 
ggsave("Figures/GAM_onset_species.png", plot_model_group(onset_species, "onset"), 
       width = 8, height = 8, dpi = 300)

ggsave("Figures/GAM_offset_species.png", plot_model_group(offset_species, "offset"), 
       width = 8, height = 8, dpi = 300)

ggsave("Figures/GAM_duration_species.png", plot_model_group(duration_species, "duration"), 
       width = 9, height = 10, dpi = 300)







### Relationships summary: In more urban areas, 

#1   Earlier onset, longer duration: Xylocopa virginica
#2   Later onset, shorter duration: Papilio troilus
#3   Later offset, longer duration: Pyrrharctia isabella, Noctua pronuba

#4   Later onset: Eristalis tenax, Helicoverpa zea
#5   Earlier onset: Eremnophila aureonotata, Vespula squamosa, Clogmia albipunctatus
#6   Later offset: Bombus impatiens, Papilio glaucus, Danaus plexippus, Epargyreus clarus, Phyciodes tharos,
#7                 Battus philenor, Limenitis arthemis 
#8   Earlier offset: Hylephila phyleus, Tetraopes tetrophthalmus
#9   Longer duration: Apis mellifera, Hyproprepia fucosa

# 1-3 make sense, 4-9 make less sense. For example, if onset starts later in the year for more urban areas, why aren't we 
# seeing total duration being decreased in these urban areas as well? If the species starts its season later (and doesn't 
# end later), that would mean less days of activity. But we're not seeing that, so what gives? 

# We need to investigate this. Because flight period duration is derived from the difference 
#between onset and offset, and both onset and offset are estimated from the same observations, 
#their estimation errors can be correlated. This means that even when GHMI significantly predicts 
#onset or offset, the corresponding duration model may not show a significant effect. Let's 
#calculate delta duration by hand as the difference between the predicted offset and onset 
#for high versus low GHMI values: 
#delta duration =(predicted offset at high GHMI − predicted onset at high GHMI) − (predicted 
# offset at low GHMI − predicted onset at low GHMI). 
#This gives us the expected change in duration associated with a change in GHMI, independent of 
#the statistical significance of the duration model itself. By comparing delta duration 
#across species, we can identify cases where onset or offset shifts substantially but 
#duration shows little or inconsistent change, helping to explain the “weird” cases in our models.

#Splitting up sig. models by phenological estimate 
sig_onset <- unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model == "onset"])
sig_offset <- unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model == "offset"])
sig_duration <- unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model == "duration"])


#Weird species model cases: onset or offset significant, duration not
weird_species <- setdiff(union(sig_onset, sig_offset), sig_duration)

#Function to calculate delta duration
calc_delta_duration <- function(sp) {
  onset_mod <- species_gam_full[[sp]]$models$onset
  offset_mod <- species_gam_full[[sp]]$models$offset
  
  # Prediction data (low GHMI = 0, high GHMI = 1)
  pred_data <- tibble(
    mean_GHMI = c(0, 1),
    lat = mean(onset_mod$model$lat, na.rm = TRUE),
    lon = mean(onset_mod$model$lon, na.rm = TRUE)
  )
  
  # Get predictions with SEs
  pred_onset  <- predict(onset_mod,  newdata = pred_data, se.fit = TRUE)
  pred_offset <- predict(offset_mod, newdata = pred_data, se.fit = TRUE)
  
  # Extract fits and SEs
  onset_low  <- pred_onset$fit[1];  onset_high  <- pred_onset$fit[2]
  offset_low <- pred_offset$fit[1]; offset_high <- pred_offset$fit[2]
  
  se_onset_low  <- pred_onset$se.fit[1];  se_onset_high  <- pred_onset$se.fit[2]
  se_offset_low <- pred_offset$se.fit[1]; se_offset_high <- pred_offset$se.fit[2]
  
  # Durations at low and high GHMI
  duration_low  <- offset_low  - onset_low
  duration_high <- offset_high - onset_high
  
  # Propagate SE for durations (Var(A−B) = Var(A)+Var(B))
  se_duration_low  <- sqrt(se_offset_low^2  + se_onset_low^2)
  se_duration_high <- sqrt(se_offset_high^2 + se_onset_high^2)
  
  # Delta values
  delta_onset    <- onset_high  - onset_low
  delta_offset   <- offset_high - offset_low
  delta_duration <- duration_high - duration_low
  
  # SE for deltas (difference of two values → add variances)
  se_delta_onset    <- sqrt(se_onset_low^2  + se_onset_high^2)
  se_delta_offset   <- sqrt(se_offset_low^2 + se_offset_high^2)
  se_delta_duration <- sqrt(se_duration_low^2 + se_duration_high^2)
  
  tibble(
    species = sp,
    delta_onset = delta_onset,
    delta_offset = delta_offset,
    delta_duration = delta_duration,
    se_onset = se_delta_onset,
    se_offset = se_delta_offset,
    se_duration = se_delta_duration
  )
}

delta_weird <- bind_rows(lapply(weird_species, calc_delta_duration))
delta_weird

#Duration is a function of both onset and offset. When both shift in the same direction, the net 
#change in duration can be minimal, resulting in no significance for the duration model. This can 
#also be caused by the "noise" of both models. If one or both has a high SE, then the combined 
#SE for duration would be double, making the model significance lower. This could ALSO be from 
#cases when onset and offset shift in opposite directions, resulting in duration inflating but
#variance still being high 



# Combine delta_weird with onset/offset deltas
delta_combined <- delta_weird %>%
  pivot_longer(
    cols = c(delta_onset, delta_offset, delta_duration),
    names_to = "phenology_variable",
    values_to = "delta_days"
  ) %>%
  mutate(
    phenology_variable = recode(
      phenology_variable,
      delta_onset = "Onset",
      delta_offset = "Offset",
      delta_duration = "Duration"
    ),
    se_days = case_when(
      phenology_variable == "Onset"    ~ se_onset,
      phenology_variable == "Offset"   ~ se_offset,
      phenology_variable == "Duration" ~ se_duration
    )
  )

# Plot it
ggplot(delta_combined, aes(x = reorder(species, delta_days), 
                           y = delta_days, fill = phenology_variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = delta_days - 2*se_days, 
                    ymax = delta_days + 2*se_days), 
                width = 0.3, position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  coord_flip() +
  labs(title = "Predicted Delta in Onset, Offset, and Duration by Species",
       subtitle = "With ±2 SE uncertainty from GAM predictions",
       x = "Species",
       y = "Predicted change (days)",
       fill = "Phenology Variable") +
  theme_minimal(base_size = 12)



delta_weird %>%
  mutate(
    z_onset = delta_onset / se_onset,
    z_offset = delta_offset / se_offset,
    z_duration = delta_duration / se_duration
  ) %>%
  select(species, starts_with("delta"), starts_with("se"), starts_with("z"))



### This likely ties into why we saw that for all pollinators as a group, GHMI sig. predicts offset
#   but not onset and not really duration (duration was just barely significant). We'll explore this
#   more carefully here
# Look at variance for the overall models
summary(gam_1)$scale      #duration
summary(gam_1_off)$scale  #offset 
summary(gam_1_on)$scale   #onset
#Offset has lower residual variance (0.3873) than onset (0.4091) and especially duration (0.4720).
#This means the GAM has a clearer signal-to-noise ratio for offset than for duration.



###Look at slopes
offset_slopes <- species_gam %>% filter(model=="offset")
ggplot(offset_slopes, aes(x = GHMI_estimate)) + geom_histogram(bins=30) + ggtitle("Distribution of species-level offset slopes")

# look at mean and SD of species offset slopes
offset_slopes %>% summarise(mean = mean(GHMI_estimate, na.rm=TRUE), sd = sd(GHMI_estimate, na.rm=TRUE))

#Compare with duration slopes
duration_slopes <- species_gam %>% filter(model=="duration")
ggplot(duration_slopes, aes(x = GHMI_estimate)) + geom_histogram(bins=30) + ggtitle("Distribution of species-level duration slopes")

# look at mean and SD of species duration slopes
duration_slopes %>% summarise(mean = mean(GHMI_estimate, na.rm=TRUE), sd = sd(GHMI_estimate, na.rm=TRUE))

#Pollinator seasons in more urban areas tend to end later, but the beginning doesn’t shift 
#systematically. This means some species do show longer seasons, but because onset responses 
#vary so much across species, the duration trend is inconsistent overall. That’s why offset is 
#strongly significant, while duration is only weakly significant. 

### Plotting this variance: 
onset_slopes  <- species_gam %>% filter(model == "onset")
offset_sd <- sd(offset_slopes$GHMI_estimate, na.rm = TRUE)
onset_sd  <- sd(onset_slopes$GHMI_estimate, na.rm = TRUE)
duration_sd <- sd(duration_slopes$GHMI_estimate, na.rm = TRUE)

tibble(
  variable = c("Onset", "Offset", "Duration"),
  sd_estimate = c(onset_sd, offset_sd, duration_sd)
)

var_df <- onset_slopes %>%
  inner_join(offset_slopes, by = "species", suffix = c("_onset","_offset")) %>%
  mutate(
    duration_calc = GHMI_estimate_offset - GHMI_estimate_onset
  )

var_summary <- tibble(
  metric = c("Onset", "Offset", "Duration"),
  variance = c(
    var(var_df$GHMI_estimate_onset, na.rm = TRUE),
    var(var_df$GHMI_estimate_offset, na.rm = TRUE),
    var(var_df$duration_calc, na.rm = TRUE)
  )
)

ggplot(var_summary, aes(x = metric, y = variance, fill = metric)) +
  geom_col() +
  geom_text(aes(label = round(variance, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("orange", "purple", "steelblue")) +
  labs(
    title = "Variance Resulting from Onset and Offset to Duration",
    y = "Variance of GHMI Slopes",
    x = "Phenology Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")







### Moving on to spatial smooths 

# Plotting spatial smooths to look at whether there is spatial significance for each of the 16 species:

# Create vectors for each model type
sig_spatial_onset <- sig_spatial_models %>% filter(model == "onset")
sig_spatial_offset <- sig_spatial_models %>% filter(model == "offset")
sig_spatial_duration <- sig_spatial_models %>% filter(model == "duration")

# Plot function 
plot_spatial_model_group <- function(df, ncol = 2) {
  plots <- lapply(seq_len(nrow(df)), function(i) {
    sp <- df$species[i]
    mod <- df$model[i]
    
    gam_obj <- species_gam_full[[sp]]$models[[mod]]
    
    smooth_labels <- gratia::smooths(gam_obj)
    spatial_index <- which(smooth_labels == "s(lat,lon)")
    
    if (length(spatial_index) == 0) {
      warning(paste("No spatial smooth for", sp, mod))
      return(NULL)
    }
    
    draw(gam_obj, select = spatial_index, residuals = FALSE, contour = TRUE) +
      labs(title = sp,
           subtitle = paste("Spatial Effect:", mod),
           fill = "Partial Effect") +
      scale_fill_viridis_c(option = "plasma") +  # optional: cleaner color scale
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5),
        legend.position = "right"
      )
  })
  
  wrap_plots(plots[lengths(plots) > 0], ncol = ncol)
}




# Plot them 
plot_spatial_model_group(sig_spatial_onset, ncol = 2)
plot_spatial_model_group(sig_spatial_offset, ncol = 2)
plot_spatial_model_group(sig_spatial_duration, ncol = 2)


#These plots show the partial effect of latitude and longitude on a phenological metric 
#(onset, offset, or duration), after accounting for GHMI (urbanization).The colors (partial effects)
#represent the relative effect of space (lat/long) on the response variable (onset, offset, or duration). 
# Cool colors (e.g., blue) might indicate regions where the flight period is earlier or is 
#shorter than average. Warm colors (e.g., yellow/red) indicate later or longer values.The contour 
#lines show where the spatial effect is constant — like elevation on a topographic map.Tight contours =
#spatial effect changes rapidly (i.e., localized hot/cold spots).Broad smooth gradients = gradual 
#spatial variation.



### Examining cases of very large estimates (more than 365 days of year) for sig. models: 
#Vespula squamosa(onset estimate) and Clogmia albipunctatus (duration and onset estimates)
print(results, n=107)
#Vespula squamosa: n (# of obs) = 10, range of GHMI values = 0.394, sd of GHMI values = 0.132
#Clogmia albipunctatus: n (# of obs) = 9, range of GHMI values = 0.375, sd of GHMI values = 0.132

# Looks like the inflated estimates are due to narrow range of GHMI values and small sample size


# Relationship between average year of observations per grid cell
year_per_grid <- filtered_5 %>% 
  group_by(grid_id) %>%
  summarise(mean_year = mean(year))

ggplot(year_per_grid, aes(x = mean_year)) +
  geom_histogram(binwidth = 1, boundary = 0, color = NA, fill = "grey30") +
  scale_x_continuous(breaks = seq(min(filtered_5$year), max(filtered_5$year), by = 2)) +
  labs(
    x = "Year",
    y = "Number of Grids"
  ) +
  theme_minimal(base_size = 14)

ggsave("Figures/Average_Year_Sampling_Grid_Cell.jpeg", height=5, width=5, units="in")

# simple observations by year
ggplot(filtered_5, aes(x = year)) +
  geom_histogram(binwidth = 1, boundary = 0, color = NA, fill = "grey30") +
  scale_x_continuous(breaks = seq(min(filtered_5$year), max(filtered_5$year), by = 2)) +
  labs(
    x = "Year",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 14)

ggsave("Figures/Observations_by_year.jpeg", height=5, width=5, units="in")
