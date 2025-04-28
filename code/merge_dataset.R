# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)


transition <- readRDS("raw_data/land_use_change_v9.rds")

####### DATSET GET TOGETHER #####

transition <- transition |> 
  mutate(muni_id = as.character(muni_id))
  #select(muni_id, year, forest_loss_all_net, mining, mining_gain_gross, mining_net, f_veg_to_mining_gross, forest_to_mining_gross, forest_to_mining_net)

df <- left_join(mining_clean, transition, by = c("muni_id", "year"))

landuse_new <- landuse |> 
  rename(area_ha_lu = area_ha) |> 
  mutate(muni_id = as.character(muni_id))

df <- left_join(df, landuse_new, by = c("muni_id", "year"))

## detect NAs in df

na_count <- sapply(df, function(x) sum(is.na(x)))

print(na_count)

gold_yearly <- gold_yearly |>
  rename(year = Year)

df_gold <- left_join(df, gold_yearly, by = "year")


## left_join with a shape file

muni_amaz <- read_sf("raw_data/BR_Municipios_2021.shp") |> 
  rename(muni_id = CD_MUN) |>
  select(muni_id, geometry)

df_shp <- left_join(df, shp_munis, by = "muni_id")
