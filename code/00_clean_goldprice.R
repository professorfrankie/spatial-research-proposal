library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(lubridate)


gold <- read_csv("raw_data/monthly.csv")
# set Date as date
gold$Date <- as.Date(paste(gold$Date, "01", sep="-"), format="%Y-%m-%d")

gold_yearly <- gold %>%
  filter(between(Date, as.Date('1985-01-01'), as.Date('2023-12-01'))) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(MeanGoldPrice = mean(Price, na.rm = TRUE))
