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
#         Output = filtered_5.rds                                   first version


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
#                e)  mean_gHM.csv
#                f)  phenology_estimates_data_for_analysis.rds
#         Output = 
#                 a) frequency_Dione_vanillae_observations_over_time_grid_1656.png 
#                 b) frequency_Bombus_impatiens_observations_over_time_grid_1656.png
#                 c) frequency_Phoebis_sennae_observations_over_time_grid_1656.png
#                 d) frequency_Epargyreus_clarus_observations_over_time_grid_1656.png
#                 e) frequency_Hylephila_phyleus_observations_over_time_grid_1656.png
#                 f) frequency_all_species_observations_over_time_grid_1656.png
#                 g) frequency_Bombus_impatiens_observations_over_time_all_grids.png
#                 h) observation_frequency_over_time_by_family.png
#                 i) observation_frequency_over_time_by_order.png
#                 j) observations_with_landsat_variables.rds
#                 k) Lepodoptera_Observations_in_Low_and_High_GHMI.png
#                 l) Dione_vanillae_Observations_in_Low_and_High_GHMI.png
#                 m) Dione_vanillae_Observations_in_Low_and_High_GHMI_two_figures.png
#                 n) map_of_species_per_grid_cell.png
#                 o) map_of_observations_per_grid_cell.png
#                 p) GHMI_map_of_Bioregion_NA24.png
#                 q) Bombus_impatiens_observations_across_grids.png
#                 r) Papilio_glaucus_observations_across_grids.png
#                 s) Xylocopa_virginica_observations_across_grids.png
#                 t) Apis_mellifera_observations_across_grids.png


###### 9. Phenology Figures.R
#         Input = phenology_estimates_data_for_analysis.rds
#         Output = 
#                 a) phenology_estimates_all_species_each_grid_with_landsat
#                 b) phenology_estimates_example_for_grid_390.png
#                 c) phenology_estimates_all_species_across_all_grids.png
#                 d) phenology_estimates_all_species_across_all_grids_separate_graphs.png
#                 e) phenology_estimates_Apis_mellifera_all_grids.png
#                 f) phenology_estimates_Automeris_io_all_grids.png
#                 g) Automeris_io_medians_Fl_map.png
#                 h) Dione_vanillae_medians_Fl_map.png
#                 i) Lepidoptera_medians_Fl_map.png
#                 j) Hymenoptera_medians_Fl_map.png
#                 k) Diptera_medians_Fl_map.png
#                 l) Coleoptera_medians_Fl_map.png
#                 m) medians_Fl_map.png
#                 n) mean_median_offset_duration_in_low_versus_high_ghmi.png
#                 o) total_duration_low_versus_high_urban_for_10_random_species.png
#                 p) onset_low_versus_high_urban_for_10_random_species.png
#                 q) onset_median_offset_in_low_versus_high_urban.png
#                 r) total_duration_in_low_versus_high_urban_for_10_random_leps.png
#                 s) total_duration_in_low_versus_high_urban_for_functional_groups_10_species.png
#                 t) total_duration_in_low_versus_high_urban_for_10_pre-selected_species.png
#                 u) onset_in_low_versus_high_urban_for_10_pre-selected_species.png
#                 v) offset_in_low_versus_high_urban_for_10_pre-selected_species.png 
#                 w) slope_of_species_duration_plot_20_random_species.png
#                 x) slope_of_ten_selected_species_duration_plot.png
#                 y) slope_of_ten_selected_species_onset_plot.png
#                 z) slope_of_ten_selected_species_offset_plot.png
#                 aa) slope_of_all_Lepidoptera_species_duration_plot.png
#                 bb) slope_of_species_duration_plot.png
#                 cc) slope_of_species_onset_plot.png
#                 dd) slope_of_species_offset_plot.png



###### 10. GAM_Analysis_GHMI.R
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






