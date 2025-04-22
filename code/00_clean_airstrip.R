library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(ggtext)
install.packages("geobr")
library(geobr)
library(viridis)


airstrips <- read_csv("raw_data/Illegal-Airstrips-NYT-Intercept-Public.csv")
airstrip <- airstrips[-c(1,3,7)]
colnames(airstrip)[2] <- "miningproximity"

legal_amazon <- read_sf("raw_data/Limites_Amazonia_Legal_2022.shp")

# Arrange the data so that TRUE values come first
airstrip <- airstrip %>%
  arrange(desc(miningproximity))

ggplot() +
  geom_sf(data = legal_amazon, fill = "grey80", alpha = 0.3) +
  geom_point(data = airstrip, aes(x = Longitude, y = Latitude, colour = miningproximity)) +
  scale_colour_manual(values = c( "#CDC9C9", "#FFD700")) +
  labs(
    title = "**Airstrips in the Brazilian legal Amazon that are <span style = 'color:#CDC9C9;'>more</span><br> and
    <span style='color:#FFD700;'>less than 20km away</span> from mines**",
  ) +
  theme_void() +
  theme(
    legend.position = "none",  
    plot.title.position = "plot",
    plot.title = element_markdown()) +
  ylim(-19, 6)

#upload mining data

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

# overlap mining 2022 with airstrip data

muni_sf <- geobr::read_municipality(year = 2010,
                                    showProgress = FALSE)
#> Using year/date 2010
colnames(muni_sf)[1] <- "muni_id"
mining_clean$muni_id <- as.character(mining_clean$muni_id)
muni_sf$muni_id <- as.character(muni_sf$muni_id)

# merge data
mining_sf <- left_join(muni_sf, mining_clean, by = 'muni_id')
airstrip_true <- airstrip %>%
  filter(miningproximity == "TRUE")
mining_2022 <- mining_sf %>%
  filter(year == "2022")

st_crs(legal_amazon)
st_crs(mining_2022)
st_crs(airstrip_true)
st_crs(airstrip)

st_crs(legal_amazon) <- 4674
airstrip_true <- st_as_sf(
  airstrip_true,
  coords = c("Longitude", "Latitude"),
  crs = 4674  # SIRGAS 2000, which you set for the others
)
airstrip <- st_as_sf(
  airstrip,
  coords = c("Longitude", "Latitude"),
  crs = 4674  # SIRGAS 2000, which you set for the others
)

common_crs <- 4674


legal_amazon <- st_transform(legal_amazon, common_crs)
mining_2022 <- st_transform(mining_2022, common_crs)
airstrip_true <- st_transform(airstrip_true, common_crs)
airstrip <- st_transform(airstrip, common_crs)

muni_amaz <- read_sf("raw_data/BR_Municipios_2021.shp")
st_crs(muni_amaz) <- 4674

install.packages("datazoom.amazonia")
library(datazoom.amazonia)
data("municipalities")
mining_2022 <- st_transform(mining_2022, st_crs(muni_amaz))
# Filter municipalities dataset for those in the Legal Amazon
legal_amazon_municipalities <- municipalities %>%
  filter(legal_amazon == 1)

# Filter mining_2022 dataset based on muni_id
mining_2022amaz <- mining_2022 %>%
  filter(muni_id %in% legal_amazon_municipalities$code_muni)
mining_2022amaz_summarized <- mining_2022amaz[-c(2,3,4,5,6)]
mining_2022amaz_summarized <- mining_2022amaz_summarized %>%
  group_by(muni_id) %>%
  summarise(
    total_area_ha = sum(area_ha, na.rm = TRUE),
    geom = st_union(geom)  # merges all geometries for the same muni_id
  ) %>%
  st_as_sf()

# plot map
ggplot() +
  geom_sf(data = legal_amazon, fill = "grey80", alpha = 0.3) +
  geom_sf(data = mining_2022amaz_summarized, aes(fill = total_area_ha), color = "black") +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data = airstrip_true, aes(colour = miningproximity)) +
  scale_colour_manual(values = c("orange")) +
  labs(
    title = "**Airstrips in the Brazilian legal Amazon that are <span style = 'color:#CDC9C9;'>more</span><br> and
    <span style='color:#FFD700;'>less than 20km away</span> from mines**"
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown()
  ) +
  coord_sf(ylim = c(-19, 6))


ggplot() +
  geom_sf(data = legal_amazon, fill = "white", alpha = 0.3) +
  geom_sf(data = mining_2022amaz_summarized, aes(fill = total_area_ha), color = "black", size = 0.5) +
  scale_fill_viridis_c(direction = -1, option = "A", name = "Total Mining Area (ha)") +
  geom_sf(data = airstrip, aes(colour = miningproximity)) +
  scale_colour_manual(values = c("#e85362", "#802582"),
                      labels = c("TRUE" = "< 20km from mine", "FALSE" = "≥ 20km from mine"),
                      name = "Airstrip Proximity") +
  labs(
    title = "**Airstrips and Mining Area in the Brazilian Legal Amazon in 2022**"
  ) +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16, face = "bold"),
    plot.subtitle = element_markdown(size = 12)
  ) +
  coord_sf(ylim = c(-19, 6))
