# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggstream)

setwd("C:\\Users\\Utente\\Desktop\\garimpo")


mining1 <- read_csv("C:/Users/Utente/Desktop/garimpo/mining_munis.csv", quote = "")


mining_clean <- mining1 |> 
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

## change code to name for mining_id to create graphs

min_code <- mining_clean |> 
  distinct(mining_id) |> 
  arrange(mining_id)

print(min_code, n = 100)

min_art <- mining_clean |> 
  filter(mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229))

min_ind <- mining_clean |> 
  filter(mining_id %in% c(102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                          112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
                          124, 125, 127, 128, 129))

## mapping of mining_id to substances and mining types

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



mining2 <- left_join(mining_clean, substances_all, by = "mining_id")

artisanal <- c(
  "artisanal_non-metallic", 
  "artisanal_metallic_tin", 
  "artisanal_metallic_gold", 
  "artisanal_precious stones",
  "artisanal_metallic_other",
  "artisanal_non_identified"
  
)

industrial <- c(
  "industrial_non-metallic",
  "industrial_metallic_other",
  "industrial_non_identified",
  "industrial_metallic_gold",
  "industrial_energetic",
  "industrial_metallic_tin",
  "industrial_metallic_other"
)

## create variable for artisanal and industrial

mining2_AI <- mining2 |> 
  mutate(substance = case_when(
    substance %in% artisanal ~ "artisanal",
    substance %in% industrial ~ "industrial",
    TRUE ~ "other"
  )) |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))


mining2_AI$substance 

pal=c("#fde725",
      "#440154")

## timeseries with mining id and area

mining_sum <- mining2_AI |> 
  group_by(year, substance) |> 
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))

mining2_AI |> 
  group_by(year, substance) |> 
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  ## change scale y to make it readable withouth e
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Proportional stacked area chart for artisanal mining

mining2 |> 
  filter(substance %in% artisanal) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_area( size = 1) +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Substance") +
  ## change scale y to make it readable withouth e
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = seq(1985, 2023, 5)) +
  labs(
    title = "Proportional area of artisanal mining in Brazil",
    subtitle = "1985-2023",
    x = "Year",
    y = "Area (ha)",
    fill = "Substance"
  ) +
  theme_minimal() 

## Proportional stacked area chart for industrial mining

mining2 |> 
  filter(substance %in% industrial) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_area(size = 1) +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Substance") +
  ## change scale y to make it readable withouth e
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = seq(1985, 2023, 5)) +
  labs(
    title = "Proportional area of industrial mining in Brazil",
    subtitle = "1985-2023",
    x = "Year",
    y = "Area (ha)",
    fill = "Substance"
  ) +
  theme_minimal()

########## INVESTIGATE OTHER DATASET ############################################

mining_fra <- read_excel("mining_big.xlsx", sheet = "CITY_STATE_BIOME" )



mining_long <- mining_fra |> 
  pivot_longer(cols = '1985':'2023',
               names_to = "year",
               values_to = "area") |> 
  rename(muni_id = geocode,
         mining_id = class_id) |> 
  mutate(mining_id = as.numeric(mining_id)) |> 
  mutate(area_ha = as.numeric(area) * 30^2 / 10^4) |> 
  arrange(muni_id, year)

mining_long_subs <- left_join(mining_clean, substances_all, by = "mining_id")

mining_long_AI <- mining_long_subs |> 
  mutate(substance = case_when(
    substance %in% artisanal ~ "artisanal",
    substance %in% industrial ~ "industrial",
    TRUE ~ "other"
  )) |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))

mining_long_sum <- mining_long_AI  |> 
  group_by(year, substance) |> 
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))

##################################################################################

## UNDERSTANDING WHICH MUNIS DISAPPEAR

library(sf)

shp_munis <- read_sf("BR_Municipios_2021.shp") |> 
  rename(muni_id = CD_MUN)

garimpo <- left_join(mining_clean,shp_munis, by = "muni_id")

garimpo_85_23 <- garimpo |> filter(year == c(1985, 2023), mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229))


garimpo_85_23 |>
  ggplot(aes(fill = area_ha)) +
  geom_sf(data = shp_munis, fill = "lightgrey") +
  geom_sf(data = desaparecidos_art,                      # highlight layer
          aes(geometry = geometry), 
          fill = NA, color = "red", size = 1) +  
  geom_sf(aes(geometry = geometry)) +
  theme_minimal() +
  facet_wrap(~ year)

class(desaparecidos_art)

art_1985 <- garimpo |> filter(year == 2023, mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229)) 
art_2023 <- garimpo |> filter(year == 1985, mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229))

desaparecidos_art <- as.data.frame(setdiff(art_2023$muni_id, art_1985$muni_id)) |> 
  rename(muni_id = `setdiff(art_2023$muni_id, art_1985$muni_id)`) |> 
  left_join(shp_munis, by = "muni_id")


garimpo_des <- filter(garimpo, muni_id %in% desaparecidos_art)
desaparecidos_art <- as.data.frame(desaparecidos_art) |>
  rename(muni_id = desaparecidos_art) |>
  left_join(shp_munis, by = "muni_id")
                            
ind_85 <- garimpo |> filter(year == 1985, mining_id %in% c(102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                          112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
                          124, 125, 127, 128, 129))

ind_23 <- garimpo |> filter(year == 2023, mining_id %in% c(102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
                          112, 113, 114, 115, 117, 118, 119, 120, 121, 122,
                          124, 125, 127, 128, 129))
desaparecidos_ind <- setdiff(ind_85$muni_id, ind_23$muni_id)

garimpo %>%
  filter(muni_id == "1100924") %>%
  ggplot(aes(x = year, y = area_ha)) +
  geom_line(color = "darkgreen", size = 1) +
  labs(
    title = "Evoluzione dell'area di garimpo nel municipio 1100924 (Chupinguaia)",
    x = "Anno",
    y = "Area (ha)"
  ) +
  theme_minimal()

years <- 1985:2023


all_years_munis <- mining_clean %>%
  group_by(muni_id) %>%
  reframe(
    missing_years = setdiff(years, unique(year))
  ) %>%
  filter(length(missing_years) > 0) |> 
  mutate(missing = ifelse(length(missing_years) > 0, "Missing", "Complete"))


shp_munis_missing <- shp_munis %>%
  left_join(all_years_munis, by = "muni_id")

unique <- as.data.frame(unique(all_years_munis$muni_id))

print(unique, n = 100)


### check how many munis are in 1985 and not 2023

art_2023 <- mining_clean |> filter(year == 2023, mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229)) 
art_1985 <- mining_clean |> filter(year == 1985, mining_id %in% c(202:213, 214, 215, 217, 218:222, 224, 225, 227:229))

desaparecidos_art <- as.data.frame(setdiff(art_1985$muni_id, art_2023$muni_id)) |> 
  rename(muni_id = `setdiff(art_2023$muni_id, art_1985$muni_id)`) 

## count how many munis are in 1985 and not 2023

sum(desaparecidos_art$muni_id)


garimpo_des <- filter(garimpo, muni_id %in% desaparecidos_art)
desaparecidos_art <- as.data.frame(desaparecidos_art) |>
  rename(muni_id = desaparecidos_art) |>
  left_join(shp_munis, by = "muni_id")
