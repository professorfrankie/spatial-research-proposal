# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)


mining <- read_csv("raw_data/mining_munis.csv") |> 
  transmute(muni_id = CD_MUN, 
            year = as.numeric(substr(bandName, 17, 20)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("mining_id", "area"), sep = "=") |>  
  mutate(mining_id = as.numeric(mining_id)) |> 
  transmute(muni_id, year, mining_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

# need to map it to mining substances names
# maybe map together certain substances to make it more feasible
# compute shares by relating it to municipality area using the shapefile?
shp_munis <- read_sf("raw_data/BR_Municipios_2021.shp")
