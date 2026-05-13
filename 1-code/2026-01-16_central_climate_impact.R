set.seed(123)

# =========================
# 1. LIBRARIES
# =========================

library(readxl)
library(ahead)
library(zoo)
library(dplyr)

# =========================
# 2. DONNEES
# =========================

df <- read_excel(
  "C:/Users/OumyDIONE/Desktop/Travaux _with/Data/ESG_data_2000_2024.xlsx"
)

df <- df[order(df$time), ]

# =========================
# 3. SCORE CLIMAT CONVEXE MAX^5
# =========================

Z_RX5 <- as.numeric(scale(df$RX5day))
Z_SPEI <- -as.numeric(scale(df$SPEI))
Z_WIND <- as.numeric(scale(df$WindExtreme))

Q_RX5 <- pnorm(Z_RX5)
Q_SPEI <- pnorm(Z_SPEI)
Q_WIND <- pnorm(Z_WIND)

eps <- 1e-8

Q_RX5 <- pmin(pmax(Q_RX5, eps), 1 - eps)
Q_SPEI <- pmin(pmax(Q_SPEI, eps), 1 - eps)
Q_WIND <- pmin(pmax(Q_WIND, eps), 1 - eps)

df$climate_score <- pmax(Q_RX5, Q_SPEI, Q_WIND)^5
df$climate_score <- as.numeric(scale(df$climate_score))

df$climate_ma6 <- zoo::rollmean(
  df$climate_score,
  6,
  fill = NA,
  align = "right"
)

df$climate_lag12 <- dplyr::lag(df$climate_ma6, 12)

# Suppression des NA liés au lissage et au retard
df <- df[!is.na(df$climate_lag12), ]

# =========================
# 4. VARIABLES ECONOMIQUES
# =========================

Y <- as.matrix(
  df[, c(
    "CAC40",
    "IPI_growth",
    "Inflation",
    "ShortRate",
    "LongRate"
  )]
)

climate <- df$climate_lag12

# =========================
# 5. MODELE BASELINE ridge2f
# =========================

model_baseline <- ridge2f(
  Y,
  h = 2,
  lags = 5,
  nb_hidden = 20,
  lambda_1 = 2.05,
  lambda_2 = 7.1,
  nodes_sim = "sobol",
  activ = "relu",
  seed = 123
)

residuals_mat <- model_baseline$residuals

# Alignement climat / résidus
clim_res <- tail(climate, nrow(residuals_mat))

# =========================
# 6. TEST 1 : CLIMAT -> RESIDUS
# =========================

cat("\n=========================\n")
cat("TEST 1 : Effet climat sur les résidus ridge2f\n")
cat("=========================\n")

for (v in colnames(Y)) {
  
  cat("\nVariable :", v, "\n")
  
  m <- lm(residuals_mat[, v] ~ clim_res)
  print(summary(m))
}

# =========================
# 7. TEST 2 : NON-LINEARITE
# =========================

cat("\n=========================\n")
cat("TEST 2 : Effet non linéaire du climat\n")
cat("=========================\n")

for (v in colnames(Y)) {
  
  cat("\nVariable :", v, "\n")
  
  m <- lm(residuals_mat[, v] ~ clim_res + I(clim_res^2))
  print(summary(m))
}

# =========================
# 8. TEST 3 : DYNAMIQUE AUTOREGRESSIVE
# =========================

cat("\n=========================\n")
cat("TEST 3 : Effet climat sur la dynamique autorégressive\n")
cat("=========================\n")

lag_Y <- Y[1:(nrow(Y) - 1), ]
Y_current <- Y[2:nrow(Y), ]
climate_current <- climate[2:length(climate)]

for (v in 1:ncol(Y)) {
  
  cat("\nVariable :", colnames(Y)[v], "\n")
  
  m <- lm(Y_current[, v] ~ lag_Y[, v] + climate_current)
  print(summary(m))
}

# =========================
# 9. TEST 4 : GAIN EXPLICATIF DU CLIMAT
# =========================

cat("\n=========================\n")
cat("TEST 4 : Comparaison avec / sans climat\n")
cat("=========================\n")

for (v in 1:ncol(Y)) {
  
  cat("\nVariable :", colnames(Y)[v], "\n")
  
  m1 <- lm(Y_current[, v] ~ lag_Y[, v])
  m2 <- lm(Y_current[, v] ~ lag_Y[, v] + climate_current)
  
  cat("\n--- Sans climat ---\n")
  print(summary(m1))
  
  cat("\n--- Avec climat ---\n")
  print(summary(m2))
  
  cat("\n--- ANOVA ---\n")
  print(anova(m1, m2))
}