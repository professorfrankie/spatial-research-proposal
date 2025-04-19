library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(ggtext)
library(mapview)


airstrips <- read_csv("raw_data/Illegal-Airstrips-NYT-Intercept-Public.csv")
airstrip <- airstrips[-c(1,3,7)]
colnames(airstrip)[2] <- "miningproximity"

mapview(airstrip, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

legal_amazon <- read_sf("raw_data/Limites_Amazonia_Legal_2022.shp")

# Arrange the data so that TRUE values come first
airstrip <- airstrip %>%
  arrange(desc(miningproximity))

ggplot() +
  geom_sf(data = legal_amazon, fill = "grey80", alpha = 0.3) +
  geom_point(data = airstrip, aes(x = Longitude, y = Latitude, colour = miningproximity)) +
  scale_colour_manual(values = c( "#FFD700", "#CDC9C9")) +
  labs(
    title = "**Airstrips in the Brazilian legal Amazon that are <span style = 'color:#CDC9C9;'>more</span><br> and
    <span style='color:#FFD700;'>less than 20km away</span> from artisanal mines**",
    #fill = "Gender"
  ) +
  theme_void() +
  theme(
    legend.position = "none",  
    plot.title.position = "plot",
    plot.title = element_markdown()) +
  ylim(-19, 6)


