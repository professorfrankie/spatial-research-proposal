library(tidyverse)
library(sf)
library(geobr)
library(viridis)
library(ggtext)
library(datazoom.amazonia)

# Load and clean airstrip data
airstrip <- read_csv("raw_data/Illegal-Airstrips-NYT-Intercept-Public.csv") %>%
  select(-c(1, 3, 7)) %>%
  rename(miningproximity = 2) %>%
  arrange(desc(miningproximity)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4674)

# Load legal Amazon shapefile
legal_amazon <- read_sf("raw_data/Limites_Amazonia_Legal_2022.shp") %>%
  st_set_crs(4674) %>%
  st_transform(crs = 4674)  

# Initial plot: Airstrips only
ggplot() +
  geom_sf(data = legal_amazon, fill = "grey80", alpha = 0.3) +
  geom_sf(data = airstrip, aes(color = miningproximity)) +
  scale_colour_manual(values = c("#CDC9C9", "#FFD700")) +
  labs(
    title = "**Airstrips in the Brazilian Legal Amazon that are <span style = 'color:#CDC9C9;'>more</span><br>
            and <span style='color:#FFD700;'>less than 20km away</span> from mines**"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_markdown()
  ) +
  coord_sf(ylim = c(-19, 6))

# Load and clean mining data
mining_clean <- read_csv("raw_data/mining_munis.csv", quote = "") %>%
  transmute(
    muni_id = str_replace(CD_MUN, '"', ""),
    year = as.numeric(str_sub(bandName, 17, 20)),
    histogram = str_remove_all(histogram, "[{}\"]")
  ) %>%
  filter(histogram != "") %>%
  separate_rows(histogram, sep = ",") %>%
  separate(histogram, into = c("mining_id", "area"), sep = "=") %>%
  mutate(
    mining_id = as.numeric(mining_id),
    area_ha = as.numeric(area) * 900 / 10000
  ) %>%
  select(muni_id, year, mining_id, area_ha)

# Merge with municipality geometry
muni_sf <- read_municipality(year = 2022, showProgress = FALSE) %>%
  mutate(muni_id = as.character(code_muni))  # create a matching ID column
mining_sf <- muni_sf %>%
  left_join(mining_clean, by = "muni_id") %>%
  st_transform(4674)

# Filter for 2022 mining and Legal Amazon municipalities
data("municipalities")
legal_amazon_munis <- municipalities %>%
  filter(legal_amazon == 1) %>%
  pull(code_muni)

# Load and prep municipalities
munis_geom <- read_municipality(year = 2022, showProgress = FALSE) %>%
  select(code_muni, geom) %>%
  mutate(code_muni = as.character(code_muni))
# Drop geometry first, do the join, then reattach geometry
mining_2022 <- mining_sf %>%
  filter(year == 2022, muni_id %in% legal_amazon_munis) %>%
  group_by(muni_id) %>%
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    st_drop_geometry(munis_geom),  # drop geometry from spatial frame
    by = c("muni_id" = "code_muni")
  ) %>%
  st_as_sf(crs = 4674)

# Final combined plot
ggplot() +
  geom_sf(data = legal_amazon, fill = "white", alpha = 0.3) +
  geom_sf(data = mining_2022, aes(fill = total_area_ha), color = "black", size = 0.5) +
  scale_fill_viridis_c(direction = -1, option = "A", name = "Total Mining Area (ha)") +
  geom_sf(data = airstrip, aes(colour = miningproximity)) +
  scale_colour_manual(
    values = c("TRUE" = "#802582", "FALSE" = "#e85362"),
    labels = c("TRUE" = "< 20km from mine", "FALSE" = "≥ 20km from mine"),
    name = "Airstrip Proximity"
  ) +
  labs(title = "**Airstrips and Mining Area in the Brazilian Legal Amazon in 2022**") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16, face = "bold")
  ) +
  coord_sf(ylim = c(-19, 6))

