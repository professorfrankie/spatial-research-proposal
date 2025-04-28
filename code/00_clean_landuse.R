# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)
library(tidyverse)


landuse <- read_csv("raw_data/lulc_munis.csv") |> 
  transmute(muni_id = CD_MUN, 
            year = as.numeric(substr(bandName, 16, 19)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("landuse_id", "area"), sep = "=") |>  
  mutate(landuse_id = as.numeric(landuse_id)) |> 
  transmute(muni_id, year, landuse_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

# need to map it to mining substances names
# maybe map together certain substances to make it more feasible
# compute shares by relating it to municipality area using the shapefile?
shp_munis <- read_sf("raw_data/BR_Municipios_2021.shp")

print(unique(landuse$landuse_id))

# mapping of landuse_id to landuse class
landuse_classes <- tibble(
  landuse_id = c(
    # Forest
    3, 4, 5, 6, 49,
    # Herbaceous and Shrubby Vegetation
    11, 12, 32, 29, 50,
    # Farming
    15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21,
    # Non-vegetated area
    23, 24, 30, 25,
    # Water
    33, 31,
    # Not observed
    0
  ),
  
  landuse_class = c(
    # Forest
    "Forest Formation", "Savanna Formation", "Mangrove", "Floodable Forest", "Wooded Sandbank Vegetation",
    # Herbaceous and Shrubby Vegetation
    "Wetland", "Grassland", "Hypersalibe Tidal Flat", "Rocky Outcrop", "Herbaceous Sandbank Vegetation",
    # Farming
    "Pasture", "Soybean", "Sugarcane", "Rice", "Cotton", "other temporary crops", "Coffe", "Citrus", "Palm oil", "Other perennial crops", "Forest Plantation", "Mosaic of uses",
    # Non-vegetated area
    "Beach", "Urban Area", "Mining", "Non-vegetated area",
    # Water
    "Water", "Aquaculture",
    # Not observed
    "Not observed"
  )
)

# Join landuse data with landuse classes
landuse <- landuse |> 
  left_join(landuse_classes, by = "landuse_id") |> 
  mutate(landuse_class = factor(landuse_class, levels = landuse_classes$landuse_class)) |> 
  select(muni_id, year, landuse_id, area_ha, landuse_class)


landuse_classes <- tibble(
  landuse_id = c(
    # Forest
    3, 4, 5, 6, 49,
    # Herbaceous and Shrubby Vegetation
    11, 12, 32, 29, 50,
    # Farming
    15, 39, 20, 40, 62, 41, 46, 47, 35, 48, 9, 21,
    # Non-vegetated area
    23, 24, 30, 25,
    # Water
    33, 31,
    # Not observed
    0
  ),
  
  landuse_class = c(
    # Forest
    "LC_Forest Formation", "LC_Savanna Formation", "LC_Mangrove", "LC_Floodable Forest", "LC_Wooded Sandbank Vegetation",
    # Herbaceous and Shrubby Vegetation
    "LC_Wetland", "LC_Grassland", "LC_Hypersalibe Tidal Flat", "LC_Rocky Outcrop", "LC_Herbaceous Sandbank Vegetation",
    # Farming
    "LC_Pasture", "LC_Soybean", "LC_Sugarcane", "LC_Rice", "LC_Cotton", "LC_other temporary crops", "LC_Coffe", "LC_Citrus", "LC_Palm oil", "LC_Other perennial crops", "LC_Forest Plantation", "LC_Mosaic of uses",
    # Non-vegetated area
    "LC_Beach", "LC_Urban Area", "LC_Mining", "LC_Non-vegetated area",
    # Water
    "LC_Water", "LC_Aquaculture",
    # Not observed
    "LC_Not observed"
  )
)

