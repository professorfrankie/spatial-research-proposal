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
    lag4 = Price - lag(Price, 4),
    log_gold1 = log(Price)- log(lag(Price, 1)),
    log_gold2 = log(Price)- log(lag(Price, 2)),
    log_gold3 = log(Price)- log(lag(Price, 3)),
    log_gold4 = log(Price)- log(lag(Price, 4)),
    prc_gold1 = (Price - lag(Price, 1))/lag(Price, 1),
    prc_gold2 = (Price - lag(Price, 2))/lag(Price, 2),
    prc_gold3 = (Price - lag(Price, 3))/lag(Price, 3),
    prc_gold4 = (Price - lag(Price, 4))/lag(Price, 4)
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

#plot the lod prices all together

plot_log_all <- ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = log_gold1, color = "Log Lag 1"), linewidth = 1) +
  geom_line(aes(y = log_gold2, color = "Log Lag 2"), linewidth = 1) +
  geom_line(aes(y = log_gold3, color = "Log Lag 3"), linewidth = 1) +
  geom_line(aes(y = log_gold4, color = "Log Lag 4"), linewidth = 1) +
  geom_line(aes(y = 0), linewidth = 1) +
  labs(
    title = "Log Lagged Gold Prices",
    x = "Year",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal()
print(plot_log_all)

#plot the prc prices all together

plot_prc_all <- ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = prc_gold1, color = "Price Change Lag 1"), linewidth = 1) +
  geom_line(aes(y = prc_gold2, color = "Price Change Lag 2"), linewidth = 1) +
  geom_line(aes(y = prc_gold3, color = "Price Change Lag 3"), linewidth = 1) +
  geom_line(aes(y = prc_gold4, color = "Price Change Lag 4"), linewidth = 1) +
  geom_line(aes(y = 0), linewidth = 1) +
  labs(
    title = "Price Change Lagged Gold Prices",
    x = "Year",
    y = "Price",
    color = "Legend"
  ) +
  theme_minimal()
print(plot_prc_all)
