### The scripts should be run/read in the order detailed below.

1.) 01_create_grid_shapefiles_of_northeast.R

2.) 02_join_inaturalist_pollinators_with_grids.R

3.) 03_Filtering Data for Phenological Estimates.R

4.) 04_Grid_Exploration.R

5.) 05_Phenological_Estimates_by_grid_by_species.R

6.) 06_link_GHMI_and_climate_to_grids.R

7.) 07_prepare_data_for_analysis.R

-   Table Produced:

    -   Table 1: `data_for_models_summary.csv`

8.) 08_Empirical_Data_Figures.R

-   Figures Produced:

    -   Figure 1: `map_of_species_per_grid_cell.png`, `Bombus_impatiens_observations_across_grids_sat.png`, `Papilio_glaucus_observations_across_grids.png`, `Xylocopa_virginica_observations_across_grids_sat.png`, and `map_of_US_and_BioregionNA24.png`

    -   Figure 2: `map_of_species_per_grid_cell.png`

    -   Figure S4: `Change_in_Pop_Den_by_Year.jpeg`

    -   Figure S7: `distribution_of_GHMI_values_in_Bioregion_NA24.png` and `distribution_of_GHMI_values_in_GAM_dataset.png`

    -   Figure S8: `Lepidoptera_Observations_in_Low_and_High_GHMI.png`, `Hymenoptera_Observations_in_Low_and_High_GHMI.png`, `Coleoptera_Observations_in_Low_and_High_GHMI.png`, and `Diptera_Observations_in_Low_and_High_GHMI.png`

    -   Figure S3: `Supplementary Figure 3.png`

9) 09_GAM_Analysis_GHMI.R

-   Tables Produced:

    -   Table 2: `species_gam_significant_p_only_w_climate.csv`

    -   Table S3: `gam_results_by_species_w_climate.csv`

10) 10_Phenology Figures.R

-   Figures Produced:

    -   Figure 3: `combined_plot_phenology_slopes_of_all_species_with_climate_PowerPoint_dimensions.png`

        -   This figure was also used for Part B of Figure S2.

    -   Figure S10: `duration_across_ghmi_for_6_species_w_climate_PowerPoint_dimensions.png`

    -   Figure 4: `onset_across_ghmi_for_10_species_w_climate_PowerPoint_dimensions.png`

    -   Figure S9: `offset_across_ghmi_for_12_species_w_climate_PowePoint_dimensions.png`

11) Supplementary Distribution of GHMI values Per Year.R

-   Figures Produced:

    -   Figure S6: `Supplementary Figure 6.png`

12) Supplementary Phenology Exploration.R

-   Figures Produced:

    -   Figure S5: `subset_versus_full_data_onset_over_GHMI_10_species.png`, `subset_versus_full_data_offset_over_GHMI_10_species.png`, and `subset_versus_full_data_total_duration_over_GHMI_10_species.png`

13) supp_spatial_resampling_GHMI_values.R

14) GAM_Analysis_Population_Density.R

15) Compare_GHMI_and_Pop_Den.R

-   Figures Produced:

    -   Figure S1: `pop_den_vs_ghmi_slope.jpeg`

    -   Figure S2 (A): `combined_plot_phenology_slopes_of_all_species_pop_den.png`

16) GAM_Analysis_GHMI_2012_2020.R
