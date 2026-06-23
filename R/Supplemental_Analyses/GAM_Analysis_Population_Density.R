# Supplemental Analysis: GAM for Pollinator Flight Period versus Population Density

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
fp_data <- readRDS("Data/final_phenology_df_for_analysis.RDS") #phenology estimate data
five_km_grids <- st_read("Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson") 
pop_den <- read.csv("Data/Spatial Data/Population_Density/mean_pop_density.csv")


# join with pop density data, we will use the 2020 census data since that is nearest to when a majority of the data was gathered
fp_data <- left_join(fp_data, pop_den %>% select(grid_id, pop_den_2020), by=c("grid"="grid_id")) 
fp_data <- fp_data %>% rename(pop_den = pop_den_2020) %>% select(-mean_GHMI)

filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data, reading this in 
#so we can see how many observations we used for analysis after all of the filtering 

########################################################################################################### 


# Clean the data so we just have records with duration flight period
fp_data_duration <- fp_data %>%
  filter(complete.cases(duration)) %>%
  mutate(species = as.factor(species))

# Let's see the distribution of duration flight period
hist(fp_data_duration$duration)
# looks close to normal!

# Pull only relevant data for the models
fp_rel <- fp_data %>%
  dplyr::select(duration, onset, offset, pop_den, lon, lat)  

# Lets get a summary of the data so we can see each value's distribution
summary(fp_rel)

# let's see if there is any multicolinearity
cor(fp_rel, method="pearson")
# I don't see anything too concerning here



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
mod_gauss_duration <- gam(duration ~ pop_den + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                          family = gaussian(),
                          method = "REML",
                          data = fp_data)
mod_gamma_duration <- gam(duration ~ pop_den + + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                          family = Gamma(link = "log"),
                          method = "REML",
                          data = fp_data)
AIC(mod_gauss_duration, mod_gamma_duration) 
gam.check(mod_gauss_duration)

plot(mod_gauss_duration, select=1)

# Start with a null model
gam_null <- gam(duration ~ 1 + s(temp) + s(prcp) + s(lat, lon, k = 170, bs="tp") + s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_null)
gam.check(gam_null)


# Now add pop_den
gam_1 <- gam(duration ~ pop_den + + s(temp) + s(prcp) +
               s(lat, lon, k = 170, bs="tp") + 
               s(species, bs="re"), 
             family = gaussian(),
             method = "REML",
             data=fp_data)
summary(gam_1) 
gam.check(gam_1)
gam.check(gam_1)$k.check




# Now let's see how they rank
aic_null_dur <- AIC(gam_null)
print(aic_null_dur)
aic_full_dur <- AIC(gam_1)
print(aic_full_dur)


# Getting delta AIC 
aic_values_dur <- c(
  null = aic_null_dur,
  GHMI = aic_full_dur
)
delta_aic_dur <- aic_values_dur - min(aic_values_dur)
delta_aic_dur


 

## Onset -------------------------------------------------------

# Let's take a closer look at the distribution of onset
hist(fp_data$onset)
#It looks normal 

# Start with modeling species as a random effect

# Comparing gaussian and gamma models to see which is a better fit
mod_gauss_onset <- gam(onset ~ pop_den + + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = gaussian(),
                       method = "REML",
                       data = fp_data)
mod_gamma_onset <- gam(onset ~ pop_den + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
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


# Now add pop_den
gam_1_on <- gam(onset ~ pop_den + s(temp) + s(prcp) +
                  s(lat, lon, k = 170, bs="tp") + 
                  s(species, bs="re"), 
                family = gaussian(),
                method = "REML",
                data=fp_data)
summary(gam_1_on)
gam.check(gam_1_on)

# Now let's see how they rank
aic_null_on <- AIC(gam_null_on)
print(aic_null_on)
aic_full_on <- AIC(gam_1_on)
print(aic_full_on)

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
mod_gauss_offset <- gam(offset ~ pop_den + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
                       family = gaussian(),
                       method = "REML",
                       data = fp_data)
mod_gamma_offset <- gam(offset ~ pop_den + s(temp) + s(prcp) + s(lat, lon) + s(species, bs="re"),
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


# Now add pop_den
gam_1_off <- gam(offset ~ pop_den + s(temp) + s(prcp) +
                   s(lat, lon, k = 170, bs="tp") + 
                   s(species, bs="re"), 
                 family = gaussian(),
                 method = "REML",
                 data=fp_data)
summary(gam_1_off) 
gam.check(gam_1_off) 

#Visualizing the spatial smooth (lat/long)
plot(gam_1_off, select = 1)

# Now let's see how they rank
aic_null_off <- AIC(gam_null_off)
print(aic_null_off)
aic_full_off <- AIC(gam_1_off)
print(aic_full_off)


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
  gam_ghmi_dur <- gam(duration ~ pop_den + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
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
  gam_ghmi_on <- gam(onset ~ pop_den + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
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
  gam_ghmi_off <- gam(offset ~ pop_den + s(temp) + s(prcp) + s(lat, lon, k = k_val, bs="tp"), 
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
    pop_den_estimate = c(sum_gam_ghmi_dur$p.table["pop_den", "Estimate"],
                      sum_gam_ghmi_on$p.table["pop_den", "Estimate"],
                      sum_gam_ghmi_off$p.table["pop_den", "Estimate"]),
    pop_den_se = c(sum_gam_ghmi_dur$p.table["pop_den", "Std. Error"],
                sum_gam_ghmi_on$p.table["pop_den", "Std. Error"],
                sum_gam_ghmi_off$p.table["pop_den", "Std. Error"]),
    pop_den_tval = c(sum_gam_ghmi_dur$p.table["pop_den", "t value"],
                  sum_gam_ghmi_on$p.table["pop_den", "t value"],
                  sum_gam_ghmi_off$p.table["pop_den", "t value"]),
    pop_den_pval = c(sum_gam_ghmi_dur$p.table["pop_den", "Pr(>|t|)"],
                  sum_gam_ghmi_on$p.table["pop_den", "Pr(>|t|)"],
                  sum_gam_ghmi_off$p.table["pop_den", "Pr(>|t|)"]),
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
saveRDS(species_gam_full, "Data/GAM_results/species_gam_pop_den.rds")

# Extract summary tables into a single dataframe
species_gam <- bind_rows(lapply(species_gam_full, function(x) x$summary_table))

# Save it 
write_csv(species_gam, "Data/GAM_results/gam_results_by_species_pop_den.csv")


# Table with only species that have p-values < 0.05
species_gam_significant_p_only <- species_gam %>%
  filter(GHMI_pval < 0.05)

# Save it 
write_csv(species_gam_significant_p_only, "Data/GAM_results/species_gam_significant_p_only_pop_den.csv")


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




length(unique(species_gam_significant_p_only$species)) #21 species sig. 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="onset"])) #11 sig. for onset 
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="offset"])) #14 sig. for offset
length(unique(species_gam_significant_p_only$species[species_gam_significant_p_only$model=="duration"])) #6 sig. for duration 



