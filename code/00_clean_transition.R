# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)


transition <- read_csv("raw_data/transitions_munis.csv") |> 
  transmute(muni_id = CD_MUN, 
            year = substr(bandName, 12, 20), 
            year = str_replace_all(bandName, "_", ""), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("transition_id", "area"), sep = "=") |>  
  mutate(transition_id = as.numeric(transition_id)) |> 
  transmute(muni_id, year, transition_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)
