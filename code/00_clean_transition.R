# File to process various GEE-Mapbiomas files
library(dplyr)
library(stringr)
library(sf)


transition <- readRDS("raw_data/land_use_change_v9.rds")
