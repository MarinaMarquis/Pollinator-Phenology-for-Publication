###### The scripts for the pollinator phenology project should be run/read in the 
#      order detailed below. 


###### 1. create_grid_shapefiles_of_northeast.R
#         Input = one_earth-bioregions-2023.geojson
#         Output = 
#                 a) NA24_gridded_map.geojson 
#                 b) NA24_gridded_map.dbf
#                 c) NA24_gridded_map.prj
#                 d) NA24_gridded_map.shp
#                 e) NA24_gridded_map.shx
#                 f) NA_24_clipped.geojson


###### 2. join_inaturalist_pollinators_with_grids.R
#         Input = 
#                 a) NA24_gridded_map.geojson
#                 b) iNaturalist_pollinator_observations.rds
#         Output = pollinators_joined_with_grids_5.rds


###### 3. Filtering Data for Phenological Estimates.R 
#         Input = 
#                 a) pollinators_joined_with_grids_5.rds
#                 b) NA24_gridded_map.geojson
#         Output = filtered_5.rds                                   first version, repeats below


###### 4. Filtering Data for Phenological Estimates_up.R 
#         Input = 
#                 a) pollinators_joined_with_grids_5.rds
#                 b) NA24_gridded_map.geojson
#         Output = filtered_5_up.rds                               check why this repeats


###### 5. Grid_Exploration.R 
#         Input = 
#                 a) filtered_5.rds
#                 b) NA24_gridded_map.geojson
#                 c) NA_24_clipped.geojson
#         Output = 
#                 a) number_species_per_grid.png                 double check 


###### 6. Phenological_Estimates_by_grid_by_species.R 
#         Input = filtered_5.csv
#         Output = phenology_estimates_by_grid_by_species.RDS


###### 7. link_GHMI_and_climate_to_grids.R
#         Input = 
#                 a) mean_gHM.csv
#                 b) climate.csv
#                 c) filtered_5_up.rds
#         Output = 
#                 a) climate_summarized.csv
#                 b) filtered_5_with_GHMI.csv



###### 8. prepare_data_for_analysis.R
#         Input = 
#                 a) filtered_5.rds
#                 a) filtered_5_with_GHMI.csv
#                 b) phenology_estimates_by_grid_by_species.RDS
#                 c) iNaturalist_pollinator_observations.rds
#         Output = phenology_estimates_data_for_analysis.rds


###### 9. Empirical_Data_Figures.R
#         Input =
#                a)  filtered_5.csv
#                b)  filtered_5_with_GHMI.csv
#                c)  NA24_gridded_map.geojson 
#                d)  NA_24_clipped.geojson
#                e)  phenology_estimates_data_for_analysis.rds
#         Output = 
#                 a) Lepidoptera_Observations_in_Low_and_High_GHMI.png 
#                 b) Hymenoptera_Observations_in_Low_and_High_GHMI.png
#                 c) Coleoptera_Observations_in_Low_and_High_GHMI
#                 d) Diptera_Observations_in_Low_and_High_GHMI.png
#                 e) map_of_species_per_grid_cell.png
#                 f) map_of_species_per_grid_cell_centroids.png
#                 g) map_of_observations_per_grid_cell.png
#                 h) GHMI_map_of_Bioregion_NA24.png
#                 i) distribution_of_GHMI_values_in_Bioregion_NA24.png
#                 j) map_of_US_and_BioregionNA24.png
#                 k) NA24_satellite_cutout.png


###### 10. Phenology Figures.R
#         Input =
#                 a) final_phenology_df_for_analysis.RDS
#                 b) gam_results_by_species_w_climate.csv
#                 c) species_gam_full_w_climate.rds
#         Output = 
#                 a) slope_of_species_duration_plot_w_climate.png
#                 b) slope_of_species_onset_plot_w_climate.png
#                 c) slope_of_species_offset_plot_w_climate.png
#                 d) combined_plot_phenology_slopes_of_all_species_w_climate.png
#                 e) combined_plot_phenology_slopes_of_all_species_w_climate_solid_background.png
#                 f) combined_plot_phenology_slopes_of_all_species_with_climate_PowerPoint_dimensions.png
#                 g) duration_across_ghmi_for_6_species_w_climate.png
#                 h) duration_across_ghmi_for_6_species_w_climate_solid_white_background.png
#                 i) duration_across_ghmi_for_6_species_w_climate_PowerPoint_dimensions.png
#                 j) onset_across_ghmi_for_10_species_w_climate.png
#                 k) onset_across_ghmi_for_10_species_w_climate_solid_white_background.png
#                 l) onset_across_ghmi_for_10_species_w_climate_PowerPoint_dimensions.png
#                 m) offset_across_ghmi_for_12_species_w_climate.png
#                 n) offset_across_ghmi_for_12_species_w_climate_solid_white_background.png
#                 o) offset_across_ghmi_for_12_species_w_climate_PowePoint_dimensions.png



###### 11. GAM_Analysis_GHMI.R
#         Input = 
#                 a) phenology_estimates_data_for_analysis.rds
#                 b) Climate_Data/climate_summarized.csv
#                 c) NA24_gridded_map.geojson
#                 d) filtered_5.rds
#         Output = 
#                 a) final_phenology_df_for_analysis.RDS
#                 b) distribution_of_GHMI_values_in_GAM_dataset.png
#                 c) data_for_models_summary.csv
#                 d) species_gam_full_w_climate.rds
#                 e) gam_results_by_species_w_climate.csv
#                 f) species_gam_significant_p_only_w_climate.csv
#                 g) Average_Year_Sampling_Grid_Cell.jpeg
#                 h) Observations_by_year.jpeg






