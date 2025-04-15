# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)
library(tibble)
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)

mining <- read_csv("raw_data/mining_munis.csv", quote = "")


mining_clean <- mining |> 
  transmute(muni_id = CD_MUN, 
            muni_id = str_replace(muni_id, '"', ""),
            year = as.numeric(substr(bandName, 17, 20)), 
            histogram = replace(histogram, histogram == "{}", NA), 
            histogram = str_replace_all(histogram, "[{}]", ""),
            histogram = str_replace_all(histogram, '"', "")) |> 
  filter(!is.na(histogram)) |> 
  separate_rows(histogram, sep = ",") |> 
  separate(histogram, into = c("mining_id", "area"), sep = "=") |>  
  mutate(mining_id = as.numeric(mining_id)) |> 
  transmute(muni_id, year, mining_id, 
            area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

#count amount of observations that have {} in histogram column
mining %>%
  filter(histogram == "{}") %>%
  summarise(count = n())

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


# mapping of mining_id to substances and mining types
substances_all <- tibble(
  mining_id = c(
    # Artisanal
    202:213, 214, 215, 217, 218:222, 224, 225, 227:229,
    
    # Industrial
    102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
    124, 125, 127, 128, 129,
    
    # Other
    302, 303, 304, 305, 306, 307, 308, 309, 310, 311,
    312, 313, 314, 315, 317, 318, 319, 320, 321, 322,
    324, 325, 327, 328, 329
  ),
  substance = c(
    # Artisanal
    rep("artisanal_metallic_other", 12),     # 202–213
    "artisanal_metallic_tin",               # 214
    "artisanal_metallic_gold",              # 215
    rep("artisanal_non-metallic", 6), # 217–222
    rep("artisanal_precious stones",2),              # 224-225
    rep("artisanal_non_identified", 3),       # 227–229
    
    # Industrial
    rep("industrial_metallic_other", 12),
    "industrial_metallic_tin",               # 214
    "industrial_metallic_gold",              # 215
    rep("industrial_non-metallic", 6),
    rep("industrial_non_identified", 2), 
    rep("industrial_energetic", 3),
    # Other
    rep("mining_OTHER", 25)
  ),
)

mining <- mining |> 
  left_join(substances_all, by = "mining_id")

# check for missing years in muning data

first_years <- mining %>%
  group_by(muni_id) %>%
  summarise(start_year = min(year))

full_panel <- first_years %>%
  rowwise() %>%
  mutate(years = list(seq(start_year, 2023))) %>%
  unnest(years) %>%
  rename(year = years)

df_presence <- mining %>%
  distinct(muni_id, year) %>%
  mutate(present = TRUE)

panel_with_presence <- full_panel %>%
  left_join(df_presence, by = c("muni_id", "year")) %>%
  mutate(present = ifelse(is.na(present), FALSE, TRUE))

missing_years <- panel_with_presence %>%
  filter(!present)

print(unique(missing_years$muni_id))

# in total we have 790 municipalities with mining data. out of those 255 have missing years!!!

# modify mining dataset: set substances as columns + add missing years + set area_ha=0 when data for that year missing 

mining_full <- full_panel %>%
  left_join(mining, by = c("muni_id", "year"))

# Step 3: Replace NA area_ha with 0
mining_full <- mining_full %>%
  mutate(area_ha = ifelse(is.na(area_ha), 0, area_ha),
         mining_id = ifelse(is.na(mining_id), 0, mining_id),) %>%
  group_by(muni_id) %>%
  fill(mining_id, substance, .direction = "downup")

# Optional: If you want to pivot wider to have substance columns
mining_wide <- mining_full %>%
  pivot_wider(
    id_cols = c(muni_id, year),  # or add mining_id if needed
    names_from = substance,
    values_from = area_ha,
    values_fill = list(area_ha = 0)
  )

# modify mining dataset: set substances as columns + add missing years + set area_ha=lastyear when data for that year missing 

mining_full <- full_panel %>%
  left_join(mining, by = c("muni_id", "year"))

# Step 2: Fill mining_id and substance as before, and LOCF for area_ha
mining_full <- mining_full %>%
  group_by(muni_id) %>%
  arrange(year) %>%
  fill(mining_id, substance, .direction = "downup") %>%     # Fill both up and down for meta fields
  fill(area_ha, .direction = "down") %>%                     # Fill area_ha forward in time
  replace_na(list(area_ha = 0))                              # Still set NA to 0 if nothing ever existed

# Step 3: Pivot wider
mining_wide_year <- mining_full %>%
  pivot_wider(
    id_cols = c(muni_id, year),
    names_from = substance,
    values_from = area_ha,
    values_fill = list(area_ha = 0)
  )

# ------------------------------

# modify mining dataset: set substances as columns
mining_wide <- mining %>%
  pivot_wider(
    id_cols = c(muni_id, year),           # keep mining_id and year as identifiers
    names_from = substance,                 # create new columns from each unique substance
    values_from = area_ha,                  # values to fill the new columns
    values_fill = list(area_ha = 0)         # fill missing combinations with 0
  )

# add missing years with area_ha = 0
mining_full <- full_panel %>%
  left_join(mining, by = c("muni_id", "year"))
