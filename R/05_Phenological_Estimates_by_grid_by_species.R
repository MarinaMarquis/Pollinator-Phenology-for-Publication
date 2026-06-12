# Quantifying Flight Period by Grid Cell
 
#   This script reads in the 'filtered_5' data, which originates from the
#   R script: "Filtering data for phenological estimates.R". It then calculates the phenology
#   estimates for each species found in each grid cell of eco-region NA24

########################################################################################################### 


# Load packages 
library(dplyr)
library(phenesse)
library(sf)
library(lubridate)
library(purrr)


#Read in the data 
filtered_5 <- readRDS("Data/filtered_5.rds") # joined grid and pollinators data


########################################################################################################### 
### Write a function to get estimates for any species
get_phenology_estimates_function <- function(species_name, grid_number) {
  
  message(paste0("Calculating phenology estimates for grid id: ", grid_number))
  message(paste0("Calculating phenology estimates for ", species_name))
  
  tryCatch({
    # Filter data to the relevant grid
    grid_data <- filtered_5 %>%
      filter(grid_id == grid_number)
    
    # Get data for a given species
    data <- grid_data %>%
      filter(species == species_name) %>%
      dplyr::select(species, grid_id, eventDate) %>%
      mutate(day_of_year = as.integer(yday(eventDate))) %>%
      filter(day_of_year > 0)
    
    # Check if there are enough data points to run estimates
    if (nrow(data) < 3) {
      message("Not enough data to estimate phenology.")
      return(data.frame(species = species_name, 
                        grid = grid_number,
                        onset = NA, 
                        median = NA, 
                        offset = NA, 
                        duration = NA))
    }
    
    # Get onset (0.1 percentile)
    onset <- weib_percentile(observations = data$day_of_year, 
                             percentile = 0.1, iterations = 500)
    
    # Get offset (0.9 percentile)
    offset <- weib_percentile(observations = data$day_of_year, 
                              percentile = 0.9, iterations = 500)
    
    # Get median (0.5 percentile)
    median <- weib_percentile(observations = data$day_of_year, 
                              percentile = 0.5, iterations = 500)
    
    # Create summary dataframe
    summary_df <- data.frame(species = species_name, 
                             grid = grid_number, 
                             onset = onset, 
                             median = median,
                             offset = offset) %>%
      mutate(duration = offset - onset)
    
    return(summary_df)
  }, error = function(e) {
    # Return a message and an NA-filled row if an error occurs
    message(paste0("Error for species ", species_name, ": ", e$message))
    return(data.frame(species = species_name, 
                      grid = grid_number,
                      onset = NA, 
                      median = NA, 
                      offset = NA, 
                      duration = NA))
  })
}


# Test the function for one species in one grid
get_phenology_estimates_function("Dione vanillae", 1656)


########################################################################################################### 


### Function for EVERY species in ONE grid
get_phenology_estimates_one_grid <- function(grid_number){
  
  # Exploration of grid 4147 (it has lots of species)
  grid_dat <- filtered_5 %>%
    filter(grid_id==grid_number)
  
  phenology_estimates <- map_dfr(unique(grid_dat$species), 
                                 ~get_phenology_estimates_function(.x, grid_number)) %>%
    mutate(grid_id=grid_number)
  
  return(phenology_estimates)
  
}

# Testing all species in grid 7 
get_phenology_estimates_one_grid(7)



########################################################################################################### 



### Apply this function over every species in each grid  
phenology_estimates_all_species_each_grid <- filtered_5 %>%
  distinct(species, grid_id) %>%  # Get unique species-grid combinations
  pmap_dfr(~get_phenology_estimates_function(.x, .y))  # Apply function to each combination


saveRDS(phenology_estimates_all_species_each_grid, "Data/Phenology Data/phenology_estimates_by_grid_by_species.RDS")







