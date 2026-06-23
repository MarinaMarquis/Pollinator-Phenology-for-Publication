This repository contains all the code and files used in the analyses reported in "Anthropogenic modification shifts pollinator phenology in the eastern United States" article, which is published in Royal Society Open Science.

# Data Folder

This folder contains all data used in the analyses. The only datasets not included are the satellite imagery data used to create Figure 1, as this data was pulled directly into the `08_Empirical_Data_Figures.R` script from ESRI World Imagery. The raw iNaturalist pollinator data was downloaded using a custom boundary of the eastern United States on August 6, 2025. The full raw dataset is too large to share on GitHub, but see <https://doi.org/10.15468/dl.7dxq94>. We do provide a filtered version of this dataset in this repository, which includes only "Research Grade" observations of the insect species that were verified as pollinators using a manual literature search of adult diets. We additionally provide the filtered NA_24_clipped.geojson boundaries to only include those located within One Earth Appalachia & Allegheny Interior Forests Bioregion (Bioregion NA24). The following is a description of data present in this repository:

**Spatial Data/Climate_Data/climate.csv** – A CSV file containing mean daily minimum and maximum temperature and precipitation for each grid cell in Bioregion NA24. This data was obtained from Daymet V4 (Thornton et al. 2022) and was summarized in Google Earth Engine. Citation: Thornton, M.M., R. Shrestha, Y. Wei, P.E. Thornton, S-C. Kao, and B.E. Wilson. (2022). Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 4 R1. ORNL DAAC, Oak Ridge, Tennessee, USA. <https://doi.org/10.3334/ORNLDAAC/2129>

**Spatial Data/Climate_Data/climate_summarized.csv** – A CSV file that is an export of `06_link_GHMI_and_climate_to_grids.R`. This file contains the summarized climatic variables by grid cell which was used in the main analysis.

**Spatial Data/ecoregion geojson/one_earth-bioregions-2023.geojson** – A geojson file containing all One Earth Bioregions. Citation: One Earth (2020). One Earth Bioregions. One Earth. <https://www.oneearth.org/bioregions-2023>

**Spatial Data/ecoregion geojson/NA_24_clipped.geojson** – A geojson file of Bioregion NA24 (a region spanning parts of the eastern United States).

**Spatial Data/GHMI/mean_gHM.csv** – A CSV file containing the average GHMI (Global Human Modification Index) number for each 5 x 5 km grid cell in Bioregion NA24. This data was obtained from the Global Human Modification data set (gHM) and summarized in Google Earth Engine. Citation: Kennedy, C. M., Oakleaf, J. R., Theobald, D. M., Baruch-Murdo, S., & Kiesecker, J. (2019). Managing the middle: A shift in conservation priorities based on the global human modification gradient. Global Change Biology, 25(3), 811-826. <https://doi.org/10.1111/gcb.14549>

**Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson** – A geojson file of Bioregion NA24 (a region spanning parts of the eastern United States), with the region divided into 5 x 5 km grid cells.

**Spatial Data/Population_Density/mean_pop_density.csv** – A CSV file containing the mean population density in 2000, 2005, 2010, 2015, and 2020 of each grid cell in Bioregion NA24. This data was obtained from the gridded population of the world version 4.11 dataset (CIESIN 2018) and was summarized in Google Earth Engine. Citation: [CIESIN] Center for International Earth Science Information Network - Columbia University. (2018). Gridded Population of the World, Version 4 (GPWv4): Population Density, Revision 11. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). <https://doi.org/10.7927/H49C6VHW>. Accessed 31 Mar 2026.

**iNaturalist_pollinator_observations.rds** – An RDS file containing all iNaturalist insect observations within the orders Coleoptera, Diptera, Hymenoptera, and Lepidoptera observed in the eastern United States in the years 2008-2024. Citation: GBIF.org (2025, August 6). GBIF occurrence download [Dataset]. Global Biodiversity Information Facility. <https://doi.org/10.15468/dl.7dxq94>

**pollinators_joined_with_grids_5.RDS** – An RDS file containing all iNaturalist insect observations within the orders Coleoptera, Diptera, Hymenoptera, and Lepidoptera observed in Bioregion NA24 2008-2024. All insect observations are linked to the 5 x 5 km grid cell within this Bioregion where they were observed.

**filtered_5.rds** – An rds file containing all insect pollinator observations, filtered to only include species that were defined as pollinators from a manual literature search of adult diets, and to only include species with at least ten observations per 5 x 5 km grid cell.

**Phenology Data/phenology_estimates_by_grid_by_species.RDS** – An RDS file containing estimates of the onset, offset, and total duration of season for each pollinator species in each 5 x 5 km grid cell of Bioregion NA24 that met our filtering criteria.

**filtered_5_with_GHMI.csv** – A CSV file containing mean GHMI, precipitation, and temperature for each 5 x 5 km grid cell in Bioregion NA24.

**phenology_estimates_data_for_analysis.rds** – An RDS file containing onset, offset, and duration of each species in each grid cell in Bioregion NA24 that met filtering criteria, as well as the mean GHMI, precipitation, temperature of each grid cell. This data frame was filtered to only include pollinator species that were found in at least 20 grids and to exclude phenology estimates that exceeded 365 day of year.

**final_phenology_df_for_analysis.RDS** – An RDS file containing onset, offset, and duration of each species in each grid cell in Bioregion NA24 that met filtering criteria, as well as the mean GHMI, precipitation, temperature of each grid cell. This data frame was filtered to exclude species with a GHMI range less than 0.3 and a GHMI standard deviation less than 0.10.

**data_for_models_summary.csv** – A CSV file that summarizes the pollinator species used in the GAMs, the number of grid cells that each species has, the number of observations for each species, and the range of GHMI values across all grid cells for each species.

**observations_with_landsat_variables.rds** – An RDS file containing the raw data of pollinator observations and GHMI value of each grid cell in the data set used for analysis.

**GAM_results/species_gam_full.rds** – An RDS file containing the full model outputs from Species-Specific models GHMI as the anthropogenic modification predictor.

**GAM_results/species_gam_full_w_climate.rds** – An RDS file containing the full model outputs from Species-Specific models that include climate variables (temperature and precipitation) and GHMI as the anthropogenic modification predictor.

**GAM_results/species_gam_full_pop_den.rds** – An RDS file containing the full model outputs from Species-Specific models that include climate variables (temperature and precipitation) and population density as the anthropogenic modification predictor.

**GAM_results/gam_results_by_species.csv** – A CSV file containing a summary of the model outputs from Species-Specific models and GHMI as the anthropogenic modification predictor. These results are compared to the gam_results_by_species_w_climate.csv to determine if climatic variables improve models.

**GAM_results/gam_results_by_species_w_climate.csv** – A CSV file containing a summary of the model outputs from Species-Specific models that include climate variables (temperature and precipitation) and GHMI as the anthropogenic modification predictor.

**GAM_results/gam_results_by_species_pop.den.csv** – A CSV file containing a summary of the model outputs from Species-Specific models that include climate variables (temperature and precipitation) and population density as the anthropogenic modification predictor.

**GAM_results/species_gam_significant_p_only_w_climate.csv** – A CSV file containing the model outputs from Species-Specific models with GHMI as the anthropogenic modification and climatic variables as predictors, filtered to only include results for species that showed GHMI significantly influenced at least one phenological estimate (based on p-value \< 0.05).

**GAM_results/species_gam_significant_p_only_pop_den.csv** – A CSV file containing the model outputs from Species-Specific models with population density as the predictor, filtered to only include results for species that showed GHMI significantly influenced at least one phenological estimate (based on p-value \< 0.05).

**phenology_estimates_sample_subset.RDS** – An RDS file containing the onset, offset, and total duration of season for the top 10 most abundant species in Bioregion NA24. Estimates were obtained from pollinator observations from the years 2012-2020.

**phenology_estimates_of_top_ten_species_for_2012_to_2020.RDS** – An RDS file containing the onset, offset, and total duration of season for the top 10 most abundant species in Bioregion NA24, filtered to include only species x grid combos used in GAMs. Estimates were obtained from pollinator observations from the years 2012-2020.

**phenology_estimates_of_top_ten_species_for_all_years.RDS** – An RDS file containing the onset, offset, and total duration of season for the top 10 most abundant species in Bioregion NA24, filtered to include only species x grid combos used in GAMs. Estimates were obtained from pollinator observations from the years 2008 - 2024.

# Figures Folder

This folder contains all the main figures and the supplemental figures and tables presented in the article, listed below:

-   Figure 1.png

-   Figure 2.png

-   Figure 3.png

-   Figure 4.png

-   Supplementary Figure 1.jpeg

-   Supplementary Figure 2.jpg

-   Supplementary Figure 3.jpg

-   Supplementary Figure 4.jpeg

-   Supplementary Figure 5.jpg

-   Supplementary Figure 6.png

-   Supplementary Figure 7.png

-   Supplementary Figure 8.png

-   Supplementary Figure 9.png

-   Supplementary Figure 10.png

-   Supplementary Table 1.docx

-   Supplementary Table 2.docx

-   Supplementary Table 3.docx

# R Folder

This folder contains 16 R scripts which can be used to repeat the results presented in the article and supplemental material. Below is a description of each script:

**01_create_grid_shapefiles_of_northeast.R** – This script defines Bioregion NA24 from the `one_earth-bioregions-2023.geojson` file to produce the clipped `NA_24_clipped.geojson` file and creates a gridded map of Bioregion NA24 ( `NA24_gridded_map.geojson` ) with the region divided into 5 x 5 km grid cells.

**02_join_inaturalist_pollinators_with_grids.R** – This script joins the GBIF download of iNaturalist pollinator observations (`iNaturalist_pollinator_observations.rds`) with the gridded map of Bioregion NA24 (`NA24_gridded_map.geojson`) so that each pollinator observation is linked to the Bioregion NA24 grid that it was observed in. The resulting data frame is `pollinators_joined_with_grids_5.rds`.

**03_Filtering Data for Phenological Estimates.R** – This script filters the `pollinators_joined_with_grids_5.rds` data set to only include insect species defined as a pollinator from a manual literature search (contains functional mouth parts as an adult and eats the reproductive parts of flowers as an adult). This script also filters the data set to include only insect pollinator species with at least 10 observations per 5 x 5 km grid cell in Bioregion NA24.

**04_Grid_Exploration.R** – This script is used to summarize the grid-level pollinator data.

**05_Phenological_Estimates_by_grid_by_species.R** – This script creates a function to use a Weibull probability distribution to estimate the onset, median, offset, and total duration of season for each pollinator species in each grid of Bioregion NA24. Onset, offset, and duration are used as phenological estimates going forward.

**06_link_GHMI_and_climate_to_grids.R** – This script creates a data frame with the mean GHMI, temperature, and precipitation in each 5 x 5 km grid cell of Bioregion NA24.

**07_prepare_data_for_analysis.R** – This script prepares the phenology data for GAM models. Climate, geographic, and human change variables are added to the grid-level phenology estimates (phenology estimates for each species in each grid), so that each grid contains the phenology estimates of all pollinator species observed there, and mean temperature, precipitation, GHMI, latitude, and longitude. This data set is then filtered to only include species that are A) found in at least 20 grids, and B) contain GHMI values across grid cells with a range greater than 0.3 and a standard deviation greater than 0.10. Lastly, the script explores phenological estimates that exceed 365 days of year, determines that they are not indicative of larger biases with the phenology estimate methods, and removes these skewed estimates from the data set. This script produces Table 1.

**08_Empirical_Data_Figures.R** – This script prepares the main text and supplementary figures produced using empirical data (not phenology data). This script produces the complete or components of the following figures: Figure 1, Figure 2, Figure S4, Figure S7, Figure S8, and Figure S3.

**09_GAM_Analysis_GHMI.R** – In this script, GAMs are constructed to measure the influence of GHMI on phenology estimates for individual species (Species-Specific Models) and across species in Bioregion NA24 (Regional-Level Models). This script produces Table 2 and Table S3.

**10_Phenology Figures.R** – In this script, phenology data is used to produced the following figures: Figure 3, Part B of Figure S2, Figure S10, Figure 4, and Figure S9.

**Supplemental_Analyses/Supplementary Distribution of GHMI values Per Year.R** – This script shows the distribution of GHMI values for each year in the data set. It produces Figure S6.

**Supplemental_Analyses/Supplementary Phenology Exploration.R** – This script tests whether sampling effort in certain years influenced our phenology estimates of species. In this script, a sub-sample of observations of the 10 most abundant species in the data set is taken and used to estimate their onset, offset, and total duration of activity for the years 2012-2020. This is compared to the phenological estimates obtained from observations of the same 10 species from the years 2009-2024. This script produces Figure S5.

**Supplemental_Analyses/supp_spatial_resampling_GHMI_values.R** – In this script, phenology data is subsampled so that there is an even number of data per 0.2 interval of GHMI index. This subsampled data set is then used for Regional-Level Model GAMs and compared to the results of GAMs with data that is not subsampled. This is done to assess whether the spatial bias towards human-modified areas is affecting the GAM results.

**Supplemental_Analyses/GAM_Analysis_Population_Density.R** – This script re-runs all of the GAMs produced in the `09_GAM_Analysis_GHMI.R` script with a population density rather than GHMI as a response variable. This was done to assess the extent to which GHMI mirrors urbanization in its relationship to phenology.

**Supplemental_Analyses/Compare_GHMI_and_Pop_Den.R** – In this script, the effect of population density versus anthropogenic modification (GHMI) on phenology estimates to assess the extent to which GHMI mirrors urbanization in its relationship to phenology. This script produces Figure S1 and Part A of Figure S2.

**Supplemental_Analyses/GAM_Analysis_GHMI_2012_2020.R** – This script re-runs all of the GAMs produced in the `09_GAM_Analysis_GHMI.R` script with a phenology data obtained from observations spanning the years 2012-2020. This was done to assess whether temporal bias was influencing GAM results.
