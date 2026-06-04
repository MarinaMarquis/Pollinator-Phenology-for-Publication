### Marina Marquis
### 8/6/25
### In this file, I clip the ecoregions geoJSON file so that I have a geoJSON file of just 
#   eco-region NA24 (a region in the Northeastern United States). I then create a 
#   bounding box of this region to filter the GBIF pollinator occurrence data to this general
#   area. Lastly, I create a gridded map of eco-region NA24, with each grid being 5 x 5 km. 



###############################################################################################################

# Read in packages 
library(sf)
library(tidyverse)

# Read in data 
ecoregions <- st_read("Data/Spatial Data/ecoregion geojson/one_earth-bioregions-2023.geojson") # ecoregion geoJSON


###############################################################################################################


# Filter to only ecoregion NA24 
NA_24 <- ecoregions %>%
  dplyr::filter(Bioregions == "NA24")

# Look at it 
plot(NA_24)

# Save the NA_24 polygon as a GeoJSON file
st_write(NA_24, "Data/Spatial Data/ecoregion geojson/NA_24_clipped.geojson", driver = "GeoJSON", delete_dsn = TRUE)

# Create a bbox of the clipped geojson so that we can use it to extract insect 
# pollinator occurrence data from GBIF in the general area 
bbox <- st_bbox(NA_24)
print(bbox)
bbox_wkt <- sprintf(
  "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
  bbox["xmin"], bbox["ymin"],  # bottom-left
  bbox["xmin"], bbox["ymax"],  # top-left
  bbox["xmax"], bbox["ymax"],  # top-right
  bbox["xmax"], bbox["ymin"],  # bottom-right
  bbox["xmin"], bbox["ymin"]   # close polygon (bottom-left again)
)

cat(bbox_wkt)



###############################################################################################################


### Create 5 x 5 km grids across the entire clipped geoJSON of eco-region NA24


# Reproject to a projected CRS (e.g., North America Albers Equal Area) so that grids are
# interpreted in km and not degrees 
NA_24_proj <- st_transform(NA_24, 5070)

# Create 5 km grid
cell_5 <- 5000  # 5,000 meters = 5 km
NA24_grid_5 <- st_make_grid(NA_24_proj, cellsize = c(cell_5, cell_5), what = "polygons")
NA24_grids_sf <- st_sf(geometry = NA24_grid_5)

# Clip to the ecoregion polygon
grid_adj_NA24 <- st_intersection(NA24_grids_sf, NA_24_proj) %>%
  mutate(grid_id = row_number())

# Reproject back to WGS84 (EPSG:4326) only for export because geoJSON requires 
# coordinates in WGS 84
grid_adj_NA24_wgs <- st_transform(grid_adj_NA24, 4326)

# Take a look at the maps
plot(st_geometry(grid_adj_NA24))        # Projected view
plot(st_geometry(grid_adj_NA24_wgs))    # Reprojected WGS84 view

# Number of  grid cells in this map: 24128
nrow(grid_adj_NA24_wgs)

# Export to GeoJSON for use in GBIF or elsewhere
st_write(grid_adj_NA24_wgs, "Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.geojson", delete_dsn = TRUE)

# Export as Shapefile as well, for use in Google Earth Engine code editor 
st_write(grid_adj_NA24_wgs, "Data/Spatial Data/gridded map of NA24 region/NA24_gridded_map.shp",
         delete_dsn = TRUE)
shp_dir2 <- "Data/Spatial Data/gridded map of NA24 region/"
zipfile2 <- file.path(shp_dir2, "NA24_gridded_map.zip")
files_to_zip2 <- list.files(shp_dir2, pattern = "NA24_gridded_map.*$", full.names = TRUE)
zip(zipfile2, files_to_zip2)

