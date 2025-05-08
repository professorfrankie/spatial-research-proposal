library(dplyr)
library(stringr)
library(sf)
library(tidyverse)
library(readr)
library(ggplot2)

gold <- gold <- read_csv("raw_data/annual.csv") |>
  mutate(
    lag1 = Price - lag(Price, 1),
    lag2 = Price - lag(Price, 2),
    lag3 = Price - lag(Price, 3),
    lag4 = Price - lag(Price, 4)
  ) |>
  filter(between(Date, 2002, 2022))

plot1 <- ggplot(gold, aes(x = Date, y = lag1)) +
  geom_line(color = "blue") +
  labs(
    title = "Lagged Gold Price (t-1)",
    x = "Year",
    y = "Lagged Price"
  ) +
  theme_minimal()
print(plot1)

plot2 <- ggplot(gold, aes(x = Date, y = lag2)) +
  geom_line(color = "red") +
  labs(
    title = "Lagged Gold Price (t-2)",
    x = "Year",
    y = "Lagged Price"
  ) +
  theme_minimal()
print(plot2)

plot3 <- ggplot(gold, aes(x = Date, y = lag3)) +
  geom_line(color = "green") +
  labs(
    title = "Lagged Gold Price (t-3)",
    x = "Year",
    y = "Lagged Price"
  ) +
  theme_minimal()
print(plot3)

plot4 <- ggplot(gold, aes(x = Date, y = lag4)) +
  geom_line(color = "purple") +
  labs(
    title = "Lagged Gold Price (t-4)",
    x = "Year",
    y = "Lagged Price"
  ) +
  theme_minimal()
print(plot4)

# Plotting all lagged prices together

plot_all <- ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = lag1, color = "Lag 1"), linewidth = 1) +
  geom_line(aes(y = lag2, color = "Lag 2"), linewidth = 1) +
  geom_line(aes(y = lag3, color = "Lag 3"), linewidth = 1) +
  geom_line(aes(y = lag4, color = "Lag 4"), linewidth = 1) +
  geom_line(aes(y = 0), linewidth = 1) +
  labs(
    title = "Lagged Gold Prices",
    x = "Year",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal()
print(plot_all)         
