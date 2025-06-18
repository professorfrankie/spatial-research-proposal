library(dplyr)
library(readxl)

# Load the data
df_raw <- read_excel("raw_data/CMO-Historical-Data-Annual.xlsx", 
                   sheet = "Annual Prices (Nominal)",
                   skip = 5, 
                   col_names = FALSE)

colnames(df_raw) <- paste(df_raw[1, ], df_raw[2, ], sep = "\n")

# Remove the first two rows from the data
df <- df_raw[-c(1,2), ] %>%
  rename(
    year = `NA\nNA`  
  ) %>%
  select(-c(
    `Crude oil, Brent\n($/bbl)`,
    `Crude oil, Dubai\n($/bbl)`,
    `Crude oil, WTI\n($/bbl)`,
    `Natural gas, US\n($/mmbtu)`,
    `Natural gas, Europe\n($/mmbtu)`,
    `Liquefied natural gas, Japan\n($/mmbtu)`,
    `Tea, Colombo\n($/kg)`,
    `Tea, Kolkata\n($/kg)`,
    `Tea, Mombasa\n($/kg)`,
    `Sugar, EU\n($/kg)`,
    `Sugar, US\n($/kg)`,
    `Rice, Thai 25%\n($/mt)`,
    `Rice, Viet Namese 5%\n($/mt)`,
    `Sorghum\n($/mt)`,
    `Barley\n($/mt)`
  )) %>%
  filter(year >= 2000 & year <= 2022)

#standardize the data
df_std <- df %>%
  select(-year) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale() %>%
  as.data.frame()

#variance-covariance matrix
cov_matrix <- cov(df_std)

pca <- prcomp(df_std, center = TRUE, scale. = TRUE)
summary(pca)

loadings <- pca$rotation  # Columns = PCs, Rows = original variables
print(loadings)

gold_vector <- loadings["Gold\n($/troy oz)", ]

# Cosine similarity between gold and others
cosine_sim <- apply(loadings, 1, function(x) sum(x * gold_vector) / (sqrt(sum(x^2)) * sqrt(sum(gold_vector^2))))

# Remove gold from comparison
cosine_sim <- cosine_sim[names(cosine_sim) != "Gold\n($/troy oz)"]

# Most aligned (cosine ~ 1)
most_similar <- names(which.max(cosine_sim))

# Most orthogonal (cosine ~ 0)
most_orthogonal <- names(which.min(abs(cosine_sim)))

# Print results
cat("Most similar to Gold:", most_similar, "\n")
cat("Most orthogonal to Gold:", most_orthogonal, "\n")

library(ggplot2)

df_loadings <- as.data.frame(loadings[, 1:2])
df_loadings$Commodity <- rownames(df_loadings)

ggplot(df_loadings, aes(x = PC1, y = PC2, label = Commodity)) +
  geom_point() +
  geom_text(nudge_y = 0.02) +
  coord_equal() +
  ggtitle("Commodity Loadings in PCA Space")



