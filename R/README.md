# The scripts should be run/read in the order detailed below.

## 1. 01_create_grid_shapefiles_of_northeast.R

- **Input** = one_earth-bioregions-2023.geojson

- **Outputs**

  - NA24_gridded_map.geojson

  - NA24_gridded_map.dbf

  - NA24_gridded_map.prj

  - NA24_gridded_map.shp

  - NA24_gridded_map.shx

  - NA_24_clipped.geojson

## 2. 02_join_inaturalist_pollinators_with_grids.R

- **Input**

  - NA24_gridded_map.geojson

  - iNaturalist_pollinator_observations.rds

- **Output** = pollinators_joined_with_grids_5.rds

## 3. 03_Filtering Data for Phenological Estimates.R

- **Input**

  - pollinators_joined_with_grids_5.rds

  - NA24_gridded_map.geojson

- **Output** = filtered_5.rds first version, repeats below

## 4. 04_Grid_Exploration.R

- **Input**

  - filtered_5.rds

  - NA24_gridded_map.geojson

  - NA_24_clipped.geojson

- **Output**

  - number_species_per_grid.png double check

## 5. 05_Phenological_Estimates_by_grid_by_species.R

- **Input** = filtered_5.csv

- **Output** = phenology_estimates_by_grid_by_species.RDS

## 6. 06_link_GHMI_and_climate_to_grids.R

- **Input**

  - mean_gHM.csv

  - climate.csv

  - filtered_5_up.rds

- **Output**

  - climate_summarized.csv

  - filtered_5_with_GHMI.csv

## 7. 07_prepare_data_for_analysis.R

- **Input**

  - filtered_5.rds

  - filtered_5_with_GHMI.csv

  - phenology_estimates_by_grid_by_species.RDS

  - iNaturalist_pollinator_observations.rds

- **Output** = phenology_estimates_data_for_analysis.rds

## 8. 08_Empirical_Data_Figures.R

- **Input**

  - filtered_5.csv

  - filtered_5_with_GHMI.csv

  - NA24_gridded_map.geojson

  - NA_24_clipped.geojson

  - phenology_estimates_data_for_analysis.rds

- **Output**

  - Lepidoptera_Observations_in_Low_and_High_GHMI.png

  - Hymenoptera_Observations_in_Low_and_High_GHMI.png

  - Coleoptera_Observations_in_Low_and_High_GHMI

  - Diptera_Observations_in_Low_and_High_GHMI.png

  - map_of_species_per_grid_cell.png

  - map_of_species_per_grid_cell_centroids.png

  - map_of_observations_per_grid_cell.png

  - GHMI_map_of_Bioregion_NA24.png

  - distribution_of_GHMI_values_in_Bioregion_NA24.png

  - map_of_US_and_BioregionNA24.png

  - NA24_satellite_cutout.png

## 9. 09_Phenology Figures.R

- I**nput**

  - final_phenology_df_for_analysis.RDS

  - gam_results_by_species_w_climate.csv

  - species_gam_full_w_climate.rds

- **Output**

  - slope_of_species_duration_plot_w_climate.png

  - slope_of_species_onset_plot_w_climate.png

  - slope_of_species_offset_plot_w_climate.png

  - combined_plot_phenology_slopes_of_all_species_w_climate.png

  - combined_plot_phenology_slopes_of_all_species_w_climate_solid_background.png

  - combined_plot_phenology_slopes_of_all_species_with_climate_PowerPoint_dimensions.png

  - duration_across_ghmi_for_6_species_w_climate.png

  - duration_across_ghmi_for_6_species_w_climate_solid_white_background.png

  - duration_across_ghmi_for_6_species_w_climate_PowerPoint_dimensions.png

  - onset_across_ghmi_for_10_species_w_climate.png

  - onset_across_ghmi_for_10_species_w_climate_solid_white_background.png

  - onset_across_ghmi_for_10_species_w_climate_PowerPoint_dimensions.png

  - offset_across_ghmi_for_12_species_w_climate.png

  - offset_across_ghmi_for_12_species_w_climate_solid_white_background.png

  - offset_across_ghmi_for_12_species_w_climate_PowePoint_dimensions.png

## 10. 10_GAM_Analysis_GHMI.R

- **Input**

  - phenology_estimates_data_for_analysis.rds

  - Climate_Data/climate_summarized.csv

  - NA24_gridded_map.geojson

  - filtered_5.rds

- **Output**

  - final_phenology_df_for_analysis.RDS

  - distribution_of_GHMI_values_in_GAM_dataset.png

  - data_for_models_summary.csv

  - species_gam_full_w_climate.rds

  - gam_results_by_species_w_climate.csv

  - species_gam_significant_p_only_w_climate.csv

  - Average_Year_Sampling_Grid_Cell.jpeg

  - Observations_by_year.jpeg
