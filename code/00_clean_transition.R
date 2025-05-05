# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)
library(datazoom.amazonia)


transition <- readRDS("raw_data/land_use_change_v9.rds") |> 
  select(muni_id, year, forest_loss_all_gross, mining, forest_to_mining_gross,
         forest_to_mining_net) |>
  filter(between(year, 2000, 2023))

data("municipalities")
legal_amazon_df <- municipalities %>%
  filter(legal_amazon == 1) %>%
  select(muni_id = code_muni)

# Filter transition for Legal Amazon municipalities
transition_amazon <- transition %>%
  filter(muni_id %in% legal_amazon_df$muni_id)

transition_amazon_garimpo <- transition_amazon |> 
  filter(muni_id %in% df_model$muni_id)

transition_amazon_garimpo_zero <- transition_amazon_garimpo |> 
  filter(muni_id %in% c(1100940, 1300060, 1301100, 1303809, 1304237,
                        1400100, 1400175, 1504422, 1507102, 1507409, 
                        1600238, 1600303, 1700400, 2100477, 2103604, 
                        2110039, 2111201))

