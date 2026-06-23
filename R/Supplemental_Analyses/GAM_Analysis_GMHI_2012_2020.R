# Supplemental Analysis: GAM for Pollinator Flight Period versus GHMI, using
# phenology estimates obtained from observations from years 2012-2020. 

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
fp_data <- readRDS("Data/phenology_estimates_sample_subset.rds") #phenology estimate data
climate <- read.csv("Data/Spatial Data/Climate_Data/climate_summarized.csv")
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") 
GHMI <- read.csv("Data/Spatial Data/GHMI/mean_gHM.csv")

# add climate data to fp_data
fp_data <- left_join(fp_data, climate %>% select(grid_id, temp, prcp), by=c("grid"="grid_id"))

# add GHMI to fp_data
fp_data <- left_join(fp_data, GHMI %>% rename(mean_GHMI=mean) %>% select(grid_id, mean_GHMI), by=c("grid"="grid_id"))
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


#################################################### Figure S7: Histogram of GHMI

# Quick visualization 
ggplot(fp_data, aes(x = mean_GHMI)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_classic(base_size=14) +
  labs(x = "GHMI", y = "Count",
       title = "Distribution of GHMI in the Data Set After Filtering")


################################################### 
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


### Look at the make-up of our data after this final level of filtering: 
#Look at new species and grids  
length(unique(fp_data$species)) #52 species 
length(unique(fp_data$grid)) #756 grids 

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

# We will do model testing on all data with species as a random effect




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
AIC(mod_gauss_duration, mod_gamma_duration)
gam.check(mod_gauss_duration)
 

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
summary(gam_1) 
gam.check(gam_1)
gam.check(gam_1)$k.check


# Let's try to have mean_GHMI as a smooth term
gam_1_smooth <- gam(duration ~ s(mean_GHMI, k = 50) + s(temp) + s(prcp) +
               s(lat, lon, k = 100, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1_smooth) 
gam.check(gam_1_smooth)
gam.check(gam_1_smooth)$k.check


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

# Getting delta AIC 
aic_values_dur <- c(
  null_temp_prcp = aic_null_dur_tp,
  GHMI = aic_full_dur,
  GHMI_smooth = aic_full_dur_smooth
)
delta_aic_dur <- aic_values_dur - min(aic_values_dur)
delta_aic_dur






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
AIC(mod_gauss_onset, mod_gamma_onset) 
gam.check(mod_gauss_onset)


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


# let's try it with a smooth term for GHMI
gam_1_on_smooth <- gam(onset ~ s(mean_GHMI, k=20) + s(temp) + s(prcp) +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_1_on_smooth)
gam.check(gam_1_on_smooth)


# Now let's see how they rank
aic_null_on <- AIC(gam_null_on)
print(aic_null_on)
aic_full_on <- AIC(gam_1_on)
print(aic_full_on)
aic_full_on_smooth <- AIC(gam_1_on_smooth)
print(aic_full_on_smooth)
 

# Getting delta AIC
aic_values_on <- c(
  null = aic_null_on,
  GHMI = aic_full_on
)

delta_aic_on <- aic_values_on - min(aic_values_on)
delta_aic_on







## Offset -------------------------------------------------------

# Let's take a closer look at the distribution of offset
hist(fp_data$offset)


# Comparing gaussian and gamma models to see which is a better fit, to make sure
mod_gauss_offset <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = gaussian(),
                       method = "REML",
                       data = fp_data)
mod_gamma_offset <- gam(offset ~ mean_GHMI + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = Gamma(link = "log"),
                       method = "REML",
                       data = fp_data)
AIC(mod_gauss_offset, mod_gamma_offset) 
gam.check(mod_gauss_offset)


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
summary(gam_1_off) 
gam.check(gam_1_off) 

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


#Visualizing the spatial smooth (lat/long)
plot(gam_1_off, select = 1)

# Now let's see how they rank
aic_null_off <- AIC(gam_null_off)
print(aic_null_off)
aic_full_off <- AIC(gam_1_off)
print(aic_full_off)
aic_full_off_smooth <- AIC(gam_1_off_smooth)
print(aic_full_off_smooth)


# Getting delta AIC 
aic_values_off <- c(
  null = aic_null_off,
  GHMI = aic_full_off
)
delta_aic_off <- aic_values_off - min(aic_values_off)
delta_aic_off










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

# Extract summary tables into a single dataframe
species_gam <- bind_rows(lapply(species_gam_full, function(x) x$summary_table))



# Table with only species that have p-values < 0.05
species_gam_significant_p_only <- species_gam %>%
  filter(GHMI_pval < 0.05)


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


length(unique(species_gam_significant_p_only$species)) #19 species sig. 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="onset"])) #10 sig. for onset 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="offset"])) #12 sig. for offset
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="duration"])) #6 sig. for duration 


