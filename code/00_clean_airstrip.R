library(dplyr)
library(stringr)
library(sf)
library(tidyverse)


airstrips <- read_csv("raw_data/Illegal-Airstrips-NYT-Intercept-Public.csv")
airsprip <- airstrips[-c(1,7)]
