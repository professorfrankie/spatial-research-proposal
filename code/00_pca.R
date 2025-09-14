# --- 0) Packages --------------------------------------------------------------
library(dplyr)
library(readxl)
library(factoextra)

# --- 1) Load & prepare the data ----------------------------------------------
df_raw <- read_excel(
  "raw_data/CMO-Historical-Data-Annual.xlsx",
  sheet = "Annual Prices (Real)",
  skip = 5,
  col_names = FALSE
)

# Use the first two rows as a two-line header
colnames(df_raw) <- paste(df_raw[1, ], df_raw[2, ], sep = "\n")

# Keep year + selected commodities, 2002–2022
df <- df_raw[-c(1, 2), ] %>%
  rename(year = `NA\nNA`) %>%
  select(
    year,
    `Platinum\n($/troy oz)`,
    `Gold\n($/troy oz)`,
    `Silver\n($/troy oz)`,
    `Zinc\n($/mt)`,
    `Nickel\n($/mt)`,
    `Tin\n($/mt)`,
    `Copper\n($/mt)`,
    `Lead\n($/mt)`,
    `Iron ore, cfr spot\n($/dmtu)`,
    `Phosphate rock\n($/mt)`
  ) %>%
  mutate(
    year = as.integer(as.character(year)),
    across(-year, ~ as.numeric(.))
  ) %>%
  filter(year >= 2002, year <= 2022)

# --- 2) Standardize the predictors (not the year) -----------------------------
X_std <- df %>%
  select(-year) %>%
  scale() %>%
  as.data.frame()

# --- 3) PCA on standardized data (avoid double-scaling) -----------------------
pca <- prcomp(X_std, center = FALSE, scale. = FALSE)

# Quick diagnostics (optional)
# summary(pca)
# fviz_pca_biplot(pca, label = "var", repel = TRUE)

# --- 4) Identify PCs relative to Gold -----------------------------------------
loadings <- pca$rotation                 # rows = variables, cols = PCs
gold_loadings <- loadings["Gold\n($/troy oz)", ]  # Gold’s loading across PCs

# PC most aligned with Gold: largest |loading|
pc_most_similar <- names(which.max(abs(gold_loadings)))

# PC most orthogonal to Gold: smallest |loading|
pc_most_orthogonal <- names(which.min(abs(gold_loadings)))

# Variance explained (for context)
var_explained <- (pca$sdev^2) / sum(pca$sdev^2)
names(var_explained) <- colnames(loadings)

cat("PC most aligned with Gold:", pc_most_similar,
    "| |Gold loading| =", round(abs(gold_loadings[pc_most_similar]), 3),
    "| Var explained =", round(100 * var_explained[pc_most_similar], 1), "%\n")

cat("PC most orthogonal to Gold:", pc_most_orthogonal,
    "| |Gold loading| =", round(abs(gold_loadings[pc_most_orthogonal]), 3),
    "| Var explained =", round(100 * var_explained[pc_most_orthogonal], 1), "%\n")

# --- 5) Build shock time series (scores along those PCs) ----------------------
scores <- as.data.frame(pca$x)  # rows = years, cols = PCs (same order as df)

# Orient the “Gold-aligned” PC so a positive score corresponds to a positive Gold loading
gold_sign <- sign(gold_loadings[pc_most_similar])
gold_aligned_shock <- gold_sign * scores[[pc_most_similar]]

# Orthogonal PC’s sign is arbitrary; we keep it as is
gold_orthogonal_shock <- scores[[pc_most_orthogonal]]

df_shocks <- df %>%
  transmute(
    year,
    gold_aligned_shock    = gold_aligned_shock,
    gold_orthogonal_shock = gold_orthogonal_shock
  )

print(head(df_shocks, 10))

write.csv(
  df_shocks,
  "processed_data/pca_shocks.csv",
  row.names = FALSE
)

# --- 6) (Optional) sanity checks / visuals ------------------------------------
# Barplot of |Gold| loadings across PCs
# barplot(abs(gold_loadings), las = 2, ylab = "|loading|", main = "Gold loading across PCs")

# --- Precious metals PCA (Gold + Silver + Platinum) ---------------------------
precious_df <- df %>%
  select(`Platinum\n($/troy oz)`,
         `Gold\n($/troy oz)`,
         `Silver\n($/troy oz)`)

precious_pca <- prcomp(scale(precious_df), center = FALSE, scale. = FALSE)
precious_shock <- precious_pca$x[, 1]  # first PC

# --- Non-precious metals PCA (excluding Gold) --------------------------------
nonprecious_df <- df %>%
  select(`Zinc\n($/mt)`,
         `Nickel\n($/mt)`,
         `Tin\n($/mt)`,
         `Copper\n($/mt)`,
         `Lead\n($/mt)`,
         `Iron ore, cfr spot\n($/dmtu)`,
         `Phosphate rock\n($/mt)`)

nonprecious_pca <- prcomp(scale(nonprecious_df), center = FALSE, scale. = FALSE)
nonprecious_shock <- nonprecious_pca$x[, 1]  # first PC

# --- Combine into shock series ------------------------------------------------
df_shocks_grouped <- df %>%
  transmute(
    year,
    precious_shock    = precious_shock,
    nonprecious_shock = nonprecious_shock
  )

print(head(df_shocks_grouped, 10))

write.csv(
  df_shocks_grouped,
  "processed_data/pca_grouped_shocks.csv",
  row.names = FALSE
)
