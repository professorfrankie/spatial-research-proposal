# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggstream)

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



mining1 <- left_join(mining_clean, substances_all, by = "mining_id")

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

mining2_AI <- mining1 |> 
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
  labs(
    title = "Brazil’s Mining Landscape Through the Years",
    subtitle = "1985-2023",
    x = "Year",
    y = "Area (ha)",
    fill = ""
  ) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(
    labels = scales::comma,     
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  theme_minimal()

# Proportional stacked area chart for artisanal mining

mining_art <- mining1 |> 
  filter(substance %in% artisanal) |>  
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |> 
  mutate(substance = recode(
    substance,
    "artisanal_metallic_other" = "Other",
    "artisanal_metallic_tin" = "Tin",
    "artisanal_metallic_gold" = "Gold",
    "artisanal_non-metallic" = "Non-metallic",
    "artisanal_precious stones" = "Precious stones",
    "artisanal_non_identified" = "Non-identified"
  ))

gold <- read_csv("raw_data/monthly.csv")

gold$Date <- as.Date(paste(gold$Date, "01", sep="-"), format="%Y-%m-%d")

gold_yearly <- gold %>%
  filter(between(Date, as.Date('1985-01-01'), as.Date('2023-12-01'))) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(MeanGoldPrice = mean(Price, na.rm = TRUE))

gold_yearly <- gold_yearly |>
  rename(year = Year)

ggplot(mining_art,aes(x = year, y = area_ha, fill = substance)) +
  geom_area( size = 1) +
  scale_fill_viridis_d(option = "D", direction = 1, name = "Substance") +
  ## change scale y to make it readable withouth e
  scale_y_continuous(
    labels = scales::comma,     
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  scale_x_continuous(breaks = seq(1985, 2023, 5)) +
  labs(
    title = "Proportional area of artisanal mining in Brazil",
    subtitle = "1985-2023",
    x = "Year",
    y = "Area (ha)",
    fill = "Substance"
  ) +
  theme_minimal()

# 1. set up the margins: bottom, left, top, right
#    we leave extra space on the left for the second axis
par(mar = c(5, 4, 4, 2) + 0.1)  

# 2. plot the primary series (mining area) with its left‐axis
plot(mining_art$year, mining_art$area_ha,
     type  = "l",
     col   = "steelblue",
     lwd   = 2,
     xlab  = "Year",
     ylab  = "Artisanal mining area (ha)",
     ylim  = range(0, mining_art$area_ha, na.rm = TRUE)
)

# 3. overlay without redrawing axes or labels
par(new = TRUE)

# 4. plot the secondary series (gold price), no axes/labels
plot(gold_yearly$year, gold_yearly$MeanGoldPrice,
     type   = "l",
     col    = "firebrick",
     lwd    = 2,
     axes   = FALSE,
     xlab   = "",
     ylab   = "",
     ylim   = range(gold_yearly$MeanGoldPrice, na.rm = TRUE)
)

# 5. draw a second y‐axis on the left, but offset outward by 3 lines
axis(side = 2,                                        # left side
     at   = pretty(range(gold_yearly$MeanGoldPrice)), # tick positions
     labels = pretty(range(gold_yearly$MeanGoldPrice)),
     line = 3                                         # move it left
)

# 6. label that new axis
mtext("Mean gold price (USD)", side = 2, line = 5, col = "firebrick")

# 7. add a legend
legend("topright", inset = .02,
       legend = c("Mining area (ha)", "Mean gold price"),
       col    = c("steelblue",    "firebrick"),
       lwd    = 2,
       bg     = "white"
)

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

shp_munis <- st_read("raw_data/BR_Municipios_2021.shp") |> 
  rename(muni_id = CD_MUN)

garimpo_sf <- left_join(mining_clean, shp_munis, by = "muni_id") |> 
  st_as_sf()
library(datazoom.amazonia)

muni_area <- read_excel("raw_data/AR_BR_RG_UF_RGINT_RGI_MUN_2023.xls") |> 
  select(muni_id = CD_MUN, area_km2 = AR_MUN_2023) |> 
  mutate(area_ha = area_km2*10^4) |> 
  select(muni_id, area_ha)

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

garimpo22_amazzon <- garimpo_sf |> 
  filter(year == 2022) |> 
  filter(muni_id %in% legal_amazon_munis)

write_sf(garimpo22_amazzon, "garimpo22_amazzon.shp")
library(tmap)

# 2. Switch to “plot” mode
tmap_mode("plot")

# 3. Plot
tm_shape(shp_munis) + 
  tm_borders() +  # Add the borders of Brazilian municipalities
  tm_shape(garimpo22_amazzon) + 
  tm_polygons(
    "area_ha",
    title = "Artisanal Mining Area (ha)",
    palette = "viridis", 
    style = "quantile", 
    n = 5,
    legend.show = TRUE
  ) +
  tm_layout(
    main.title = "Artisanal Mining Area in Brazil, 2022",
    legend.outside = TRUE,
    legend.title.size = 1.2,
    legend.text.size = 1.0,
    frame = FALSE
  ) +
  tm_credits("Data: Your Source Here", position = c("LEFT", "BOTTOM"))

















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
