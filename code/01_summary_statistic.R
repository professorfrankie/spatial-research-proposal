# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(ggstream)
library(datazoom.amazonia)
mining <- read_csv("raw_data/mining_munis.csv", quote = "")

df_raw <- read_excel("raw_data/CMO-Historical-Data-Annual.xlsx", 
                     sheet = "Annual Prices (Real)",
                     skip = 5, 
                     col_names = FALSE)

colnames(df_raw) <- paste(df_raw[1, ], df_raw[2, ], sep = "\n")

# Remove the first two rows from the data
real_prices <- df_raw[-c(1,2), ] %>%
  rename(
    year = `NA\nNA`  
  ) %>%
  select(c(
    year,
    `Gold\n($/troy oz)`,
    `Tin\n($/mt)`,
    `Iron ore, cfr spot\n($/dmtu)`
  )) 

real_prices <- real_prices |> 
  rename(
    Gold = `Gold\n($/troy oz)`,
    Tin = `Tin\n($/mt)`,
    Iron_ore = `Iron ore, cfr spot\n($/dmtu)`
  )


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
  arrange(muni_id, year) |> 
  filter(between(year, 2002, 2022))

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

mining2_all <- mining1 |> 
  mutate(substance = case_when(
    substance %in% artisanal ~ "artisanal",
    substance %in% industrial ~ "industrial",
    TRUE ~ "other"
  )) |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))

mining_A_all <- mining2_all |> 
  filter(substance %in% "artisanal") |> 
  mutate(
    legal_amazon = ifelse(muni_id %in% legal_amazon_munis, 1, 0)
  )

## number of municipalitys

mining_A_all |> 
  group_by(legal_amazon) |> 
  summarise(
    n_munis = n_distinct(muni_id),
    n_years = n_distinct(year)
  )



## percentage of artisanl mining in legal amazon vs rest of Brasil

mining_A_all1 <- mining_A_all |> 
  group_by(year, legal_amazon) |> 
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")

mining_A_pct <- mining_A_all1 |> 
  group_by(year) |> 
  mutate(
    total_area = sum(area_ha),
    pct = (area_ha / total_area) * 100
  ) |> 
  ungroup()

## more or less all artisanal mining is 90% in legal amazon vs rest of Brasil


## percentage of artisanal mining in legal amazon vs all brasil

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

mining2_AI <- mining1 |> 
  mutate(substance = case_when(
    substance %in% artisanal ~ "artisanal",
    substance %in% industrial ~ "industrial",
    TRUE ~ "other"
  )) |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other"))) |> 
  filter(muni_id %in% legal_amazon_munis)


## percentage of area_ha by substance

mining2_AI |> 
  group_by(substance,year) |> 
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE)) |> 
  mutate(percentage = total_area_ha / sum(total_area_ha) * 100)

## 77.3% of mining area in legal amazon is artisanal,
## 22.7% is industrial

mining2_AI |>
  filter(muni_id %in% legal_amazon_munis & 
           substance %in% c("artisanal")) |>
  summarise(
    area_ha = sum(area_ha, na.rm = TRUE),
    n_munis = unique(n_distinct(muni_id)),
    n_years = n_distinct(year))

## actual number of municipalities affect by mining both artisanal and industrial are 136
## total number now is 776. Total area of mining is 5329679 hectare for all 39 years

mining2_AI |> 
  summarise(
    max_area_ha = max(area_ha, na.rm = TRUE),
    min_area_ha = min(area_ha, na.rm = TRUE)
  )

## max area_ha is 71581 hectar in 2022 and it is mining and min is 0.0208 in 1987
## and it is industrial mining

mining2_AI$substance 

pal=c("#fde725",
      "#440154")

## timeseries with mining id and area

mining_sum <- mining2_AI |> 
  group_by(year, substance) |> 
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |> 
  mutate(substance = factor(substance, levels = c("artisanal", "industrial", "other")))

## explore the cumulative 

mining_sum |> 
  summarise(
    max_area_ha = max(area_ha, na.rm = TRUE),
    min_area_ha = min(area_ha, na.rm = TRUE)
  )

## the max area is 268499 hectare in 2023 and it is artisanal
## the min area is 8113 hectare in 1985 and it is industrial

real_prices <- real_prices %>%
  mutate(Gold = as.numeric(Gold),
         Tin = as.numeric(Tin)) |> 
  filter(between(year, 2002, 2022))

# Compute value ranges
mining_total_range <- range(mining_sum$area_ha, na.rm = TRUE)
gold_price_range <- range(real_prices$Gold, na.rm = TRUE)
tin_price_range <- range(real_prices$Tin, na.rm = TRUE)

# Rescale gold prices to match mining area visual scale
real_prices <- real_prices %>%
  mutate(Gold_scaled = scales::rescale(Gold, to = mining_total_range),
         Tin_scaled = scales::rescale(Tin, to = mining_total_range))

# Final plot
main <- ggplot() +
  # Stacked mining bars
  geom_col(
    data = mining_sum,
    aes(x = year, y = area_ha, fill = substance),
    position = "stack"
  ) +
  # Gold price line with color aesthetic mapped
  geom_line(
    data = real_prices,
    aes(x = year, y = Gold_scaled, color = "Gold Price"),
    size = 1
  ) +
  # You can uncomment Tin price similarly with a different color and label
  #geom_line(
  #data = real_prices,
  #aes(x = year, y = Tin_scaled, color = "Tin Price"),
  #size = 1
  #) +
  scale_fill_manual(
    name = "Mining Type",
    values = c("artisanal" = "gold", "industrial" = "navyblue")
  ) +
  scale_color_manual(
    name = NULL,   # legend title for line, or give it a name
    values = c("Gold Price" = "red"),
    guide = guide_legend(override.aes = list(linetype = 1, size = 1.5))
  ) +
  scale_y_continuous(
    name = "Area (ha)",
    labels = scales::comma,
    breaks = seq(0, 300000, by = 50000),  # or try 25000
    sec.axis = sec_axis(
      transform = ~ scales::rescale(., from = mining_total_range, to = gold_price_range),
      name = "Mean Gold Price (USD/oz)"
    )
  ) +
  scale_x_continuous(breaks = seq(2002, 2022)) +
  labs(
    title = "Brazil’s Mining Landscape Through the Years",
    subtitle = "2002–2022",
    x = "Year",
    fill = "Mining Type",
    caption = "Data: MapBiomas, World Bank",
    color = NULL
  ) +
  guides(
    fill = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

main

ggsave("figures/mining_timeseries.png", main, width = 10, height = 6, dpi = 300)

## explore mining_art

mining_art |> 
  summarise(
    max_area_ha = max(area_ha, na.rm = TRUE),
    min_area_ha = min(area_ha, na.rm = TRUE)
  )

## min is 0.45 hectare and it is "Other" in 1985, 
## max is 236147 hectare and it is Gold in 2023.

## Proportional stacked area chart for artisanal mining

mining_art <- mining1 |>  
  filter(substance %in% artisanal &
           muni_id %in% legal_amazon_munis) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "artisanal_metallic_other" = "Other",
    "artisanal_metallic_tin" = "Tin",
    "artisanal_metallic_gold" = "Gold",
    "artisanal_non-metallic" = "Non-metallic",
    "artisanal_non_identified" = "Non-identified",
    "artisanal_precious stones" = "Precious stones"
  ))

## percentage of substances in artisanal mining per year

mining_art <- mining_art |> 
  group_by(year) |> 
  mutate(
    total_area_ha = sum(area_ha, na.rm = TRUE),
    .groups = "drop"
    
  ) 

## plot

mining_art |> 
  ggplot(aes(x = year, y = area_ha, fill = substance)) +
  geom_area(size = 1) +
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

mining_art <- mining_art |>
  mutate(
    pct = area_ha / total_area_ha * 100
  )

## pct in 2022 87% was gold extraction


## Proportional stacked area chart for industrial mining

mining_ind <- mining1 |>  
  filter(substance %in% industrial &
           muni_id %in% legal_amazon_munis) |>
  group_by(year, substance) |>
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") |>
  mutate(substance = recode(
    substance,
    "industrial_metallic_other" = "Other",
    "industrial_metallic_tin" = "Tin",
    "industrial_metallic_gold" = "Gold",
    "industrial_non-metallic" = "Non-metallic",
    "industrial_non_identified" = "Non-identified",
    "industrial_energetic" = "Energetic"
  ))


mining_ind |> 
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

## explore mining_ind

mining_ind |> 
  summarise(
    max_area_ha = max(area_ha, na.rm = TRUE),
    min_area_ha = min(area_ha, na.rm = TRUE)
  )

## min is 2.52 hectare and it is "Non-identifies" in 1987,
## max is 40544 hectare and it is "Other" in 2023.

## pct of substances in industrial mining per year

mining_ind <- mining_ind |> 
  group_by(year) |> 
  mutate(
    pct = area_ha / sum(area_ha, na.rm = TRUE) * 100,
    .groups = "drop"
    
  )

##################################################################################

## UNDERSTANDING WHICH MUNIS DISAPPEAR

library(sf)

shp_munis <- st_read("raw_data/BR_Municipios_2021.shp") |> 
  rename(muni_id = CD_MUN)

garimpo_sf <- left_join(mining_clean, shp_munis, by = "muni_id") |> 
  st_as_sf()


muni_area <- read_excel("raw_data/AR_BR_RG_UF_RGINT_RGI_MUN_2023.xls") |> 
  select(muni_id = CD_MUN, area_km2 = AR_MUN_2023) |> 
  mutate(area_ha = area_km2*10^4) |> 
  select(muni_id, area_ha)

data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

garimpo_amazzon <- mining_clean |> 
  filter(muni_id %in% legal_amazon_munis) |> 
  left_join(shp_munis, by = "muni_id")

garimpo_02 <- garimpo_amazzon |> 
  filter(year == 2002)

garimpo_02 <- st_as_sf(garimpo_02)

garimpo_22 <- garimpo_amazzon |> 
  filter(year == 2022)

garimpo_22 <- st_as_sf(garimpo_22)

amazon_muni <- st_read("raw_data/Mun_Amazonia_Legal_2022.shp")

## MAP

combined_area <- c(garimpo_02$area_ha, garimpo_22$area_ha)
breaks <- quantile(combined_area, probs = seq(0, 1, length.out = 6), na.rm = TRUE)


library(ggplot2)
library(sf)
library(dplyr)
library(viridis)
library(patchwork)
library(ggspatial)
library(rosm)

library(prettymapr) # For annotation_map_tile

garimpo_all <- bind_rows(
  garimpo_02 %>% mutate(year = 2002),
  garimpo_22 %>% mutate(year = 2022)
)

# Add 'year' column and combine data
garimpo_all <- garimpo_all %>%
  mutate(area_cat = cut(
    area_ha,
    breaks = c(0, 10, 50, 100, 500, 1000, 5000, Inf),
    labels = c("0–10", "10–50", "50–100", "100–500", "500–1,000", "1,000–5,000", ">5,000"),
    include.lowest = TRUE
  ))

# 1. Transform data to Web Mercator (required by tile servers like OSM)
crs_web <- 3857
amazon_muni_proj <- st_transform(amazon_muni, crs_web)
garimpo_all_proj <- st_transform(garimpo_all, crs_web)

# 2. Prepare background map layer
bg_map <- annotation_map_tile(type = "osm", zoom = 6)  # Try "osm", "cartolight", "cartodark", etc.

# 3. Recreate maps with basemap
p1 <- ggplot() +
  bg_map +
  geom_sf(data = amazon_muni_proj, fill = "white", color = "black", linewidth = 0.1, alpha = 0.5) +
  geom_sf(data = filter(garimpo_all_proj, year == 2002), 
          aes(fill = area_cat), color = "black", size = 0.1) +
  scale_fill_viridis_d(
    name = "Garimpo Area (ha)",
    option = "D", 
    direction = -1, 
    drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  ggtitle("Garimpo Area in 2002") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

p2 <- ggplot() +
  bg_map +
  geom_sf(data = amazon_muni_proj, fill = "white", color = "black", linewidth = 0.1, alpha = 0.5) +
  geom_sf(data = filter(garimpo_all_proj, year == 2022), 
          aes(fill = area_cat), color = "black", size = 0.1) +
  scale_fill_viridis_d(
    name = "Garimpo Area (ha)",
    option = "D",
    direction = -1,
    drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  ggtitle("Garimpo Area in 2022") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

# Create bounding box around Amazon
amazon_bbox <- st_bbox(amazon_muni) %>% st_as_sfc() %>% st_transform(crs_web)

# Load a simple Brazil shapefile (you might need to import it)
brasil <- geobr::read_country()

inset <- ggplot() +
  theme_void() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white") +
  geom_sf(data = st_transform(brasil, crs_web), fill = "grey90", color = "grey40") +
  geom_sf(data = amazon_bbox, fill = NA, color = "red", size = 1)

combined <- p1 + p2 + 
  plot_layout(guides = "collect", ncol = 2) &  # collect legend into one
  theme(legend.position = "bottom")            # put legend below

combined

library(cowplot)

final_plot <- ggdraw() +
  draw_plot(combined) +
  draw_plot(inset, x = 0.90, y = 0.8, width = 0.10, height = 0.10) +
  draw_label(
    "Data: MapBiomass", 
    x = 0.98, y = 0.04, hjust = 1,  # Right-aligned
    size = 10, fontface = "italic"
  )

final_plot

# Save the final plot

ggsave("figures/garimpo_area_2002_2022.png", final_plot, width = 12, height = 6, dpi = 300)