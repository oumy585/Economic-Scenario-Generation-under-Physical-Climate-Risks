set.seed(123)

library(readxl)
library(ahead)
library(zoo)
library(dplyr)
library(ggplot2)
library(patchwork)
library(writexl)

# =========================
# 1. DATA IMPORT
# =========================
df_geo <- read_excel(
  "ESG_data_2000_2024.xlsx"
)

df_geo <- df_geo[order(df_geo$time), ]

Y <- as.matrix(
  df_geo[, c("CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate")]
)

# =========================
# 2. GEOMETRIC CLIMATE STRESS INDICATOR
# =========================
df_geo$SPEI_stress <- -df_geo$SPEI

df_geo$RX5_scaled <- 1 + 99 * (
  df_geo$RX5day - min(df_geo$RX5day, na.rm = TRUE)
) / (
  max(df_geo$RX5day, na.rm = TRUE) - min(df_geo$RX5day, na.rm = TRUE)
)

df_geo$SPEI_scaled <- 1 + 99 * (
  df_geo$SPEI_stress - min(df_geo$SPEI_stress, na.rm = TRUE)
) / (
  max(df_geo$SPEI_stress, na.rm = TRUE) - min(df_geo$SPEI_stress, na.rm = TRUE)
)

df_geo$Wind_scaled <- 1 + 99 * (
  df_geo$WindExtreme - min(df_geo$WindExtreme, na.rm = TRUE)
) / (
  max(df_geo$WindExtreme, na.rm = TRUE) - min(df_geo$WindExtreme, na.rm = TRUE)
)

df_geo$climate_score_geo <- (
  df_geo$RX5_scaled *
    df_geo$SPEI_scaled *
    df_geo$Wind_scaled
)^(1 / 3)

df_geo$climate_geo_ma6 <- zoo::rollmean(
  df_geo$climate_score_geo,
  6,
  fill = NA,
  align = "right"
)

df_geo$climate_lag12 <- dplyr::lag(df_geo$climate_geo_ma6, 12)

# =========================
# 3. BASELINE ESG MODEL
# =========================
h <- 360

model_central <- ridge2f(
  Y,
  h = h,
  lags = 5,
  nb_hidden = 20,
  lambda_1 = 2.05,
  lambda_2 = 7.1,
  nodes_sim = "sobol",
  activ = "relu",
  seed = 123
)

central <- model_central$mean
res_all <- as.matrix(model_central$residuals)

# =========================
# 4. HISTORICAL ONE-YEAR LOSS
# =========================
compute_loss_hist <- function(df_in) {
  
  n <- nrow(df_in)
  loss <- rep(NA, n)
  
  for (t in 1:(n - 12)) {
    
    x1  <- df_in[t, ]
    x12 <- df_in[t + 12, ]
    
    r_eq     <- (x12$CAC40     - x1$CAC40) / 100
    dy_short <- (x12$ShortRate - x1$ShortRate) / 100
    dy_long  <- (x12$LongRate  - x1$LongRate) / 100
    dpi      <- (x12$Inflation - x1$Inflation) / 100
    
    loss[t] <- -(
      100 * r_eq -
        2 * dy_short * 300 -
        8 * dy_long * 400 -
        200 * dpi
    )
  }
  
  loss
}

df_geo$loss_12m <- compute_loss_hist(df_geo)

# =========================
# 5. HISTORICAL EVT SAMPLE
# =========================
df_evt <- df_geo %>%
  select(loss_12m, climate_lag12) %>%
  na.omit()

u_hist <- as.numeric(quantile(df_evt$loss_12m, 0.95))

sev_data <- df_evt %>%
  filter(loss_12m > u_hist) %>%
  mutate(excess = loss_12m - u_hist)

stress_threshold <- as.numeric(quantile(df_evt$climate_lag12, 0.60))

sev_data$regime <- ifelse(
  sev_data$climate_lag12 >= stress_threshold,
  "stress",
  "normal"
)

excess_normal <- sev_data$excess[sev_data$regime == "normal"]
excess_stress <- sev_data$excess[sev_data$regime == "stress"]

if (length(excess_normal) < 3) excess_normal <- sev_data$excess
if (length(excess_stress) < 3) excess_stress <- sev_data$excess

mean_normal <- mean(excess_normal)
mean_stress <- mean(excess_stress)

cat("\n=== HISTORICAL EVT CHECK: GEOMETRIC SCORE ===\n")
cat("Mean excess, normal regime:", mean_normal, "\n")
cat("Mean excess, stress regime:", mean_stress, "\n")

# =========================
# 6. MOVING BLOCK BOOTSTRAP
# =========================
draw_block <- function(pool, block_length) {
  
  n <- nrow(pool)
  start <- sample(1:(n - block_length + 1), 1)
  
  pool[start:(start + block_length - 1), , drop = FALSE]
}

simulate_path <- function() {
  
  h <- nrow(central)
  sim <- matrix(NA, h, ncol(central))
  colnames(sim) <- colnames(central)
  
  t <- 1
  
  while (t <= h) {
    
    block <- draw_block(res_all, 12)
    
    L <- nrow(block)
    end_t <- min(t + L - 1, h)
    L_use <- end_t - t + 1
    
    sim[t:end_t, ] <- central[t:end_t, ] + block[1:L_use, ]
    
    t <- end_t + 1
  }
  
  sim
}

# =========================
# 7. SIMULATED ONE-YEAR LOSS
# =========================
compute_loss <- function(sim) {
  
  x1  <- sim[1, ]
  x12 <- sim[12, ]
  
  r_eq     <- (x12["CAC40"] - x1["CAC40"]) / 100
  dy_short <- (x12["ShortRate"] - x1["ShortRate"]) / 100
  dy_long  <- (x12["LongRate"]  - x1["LongRate"]) / 100
  dpi      <- (x12["Inflation"] - x1["Inflation"]) / 100
  
  -(
    100 * r_eq -
      2 * dy_short * 300 -
      8 * dy_long * 400 -
      200 * dpi
  )
}

# =========================
# 8. BASELINE MONTE CARLO SIMULATION
# =========================
n_sim <- 5000
loss_base <- numeric(n_sim)

for (i in 1:n_sim) {
  sim <- simulate_path()
  loss_base[i] <- compute_loss(sim)
}

u_sim <- as.numeric(quantile(loss_base, 0.95))

# =========================
# 9. CLIMATE-CONDITIONED TAIL ADJUSTMENT
# =========================
lm_excess <- lm(log1p(excess) ~ climate_lag12, data = sev_data)

clim_ref <- median(df_evt$climate_lag12, na.rm = TRUE)

pred_ref_log <- predict(
  lm_excess,
  newdata = data.frame(climate_lag12 = clim_ref)
)

pred_ref <- as.numeric(exp(pred_ref_log) - 1)

r_max <- 1.25

cat("\n=== CLIMATE TAIL ADJUSTMENT: GEOMETRIC SCORE ===\n")
cat("Reference climate level:", clim_ref, "\n")
cat("Predicted reference excess:", pred_ref, "\n")
cat("Upper bound r_max:", r_max, "\n")

loss_clim <- numeric(n_sim)
climate_draws <- sample(df_evt$climate_lag12, n_sim, replace = TRUE)

for (i in 1:n_sim) {
  
  loss_i <- loss_base[i]
  
  if (loss_i <= u_sim) {
    
    loss_clim[i] <- loss_i
    
  } else {
    
    excess_base <- loss_i - u_sim
    
    pred_i_log <- predict(
      lm_excess,
      newdata = data.frame(climate_lag12 = climate_draws[i])
    )
    
    pred_i <- as.numeric(exp(pred_i_log) - 1)
    
    r_i <- pred_i / pred_ref
    r_i <- min(max(r_i, 1), r_max)
    
    loss_clim[i] <- u_sim + r_i * excess_base
  }
}

# =========================
# 10. MAIN RESULTS
# =========================
VaR_base <- as.numeric(quantile(loss_base, 0.995))
VaR_clim <- as.numeric(quantile(loss_clim, 0.995))

ES_base <- mean(loss_base[loss_base >= VaR_base])
ES_clim <- mean(loss_clim[loss_clim >= VaR_clim])

results_main_geo <- data.frame(
  Score = "Geometric_1_100",
  VaR_base = VaR_base,
  VaR_clim = VaR_clim,
  Delta_VaR = VaR_clim - VaR_base,
  Delta_VaR_pct = 100 * (VaR_clim - VaR_base) / abs(VaR_base),
  ES_base = ES_base,
  ES_clim = ES_clim,
  Delta_ES = ES_clim - ES_base,
  Delta_ES_pct = 100 * (ES_clim - ES_base) / abs(ES_base)
)

cat("\n=== MAIN RESULTS: GEOMETRIC SCORE ===\n")
print(results_main_geo)

# =========================
# 11. BOOTSTRAP SIGNIFICANCE TEST ON ES
# =========================
n_boot <- 1000
diff_ES <- numeric(n_boot)

for (b in 1:n_boot) {
  
  idx <- sample(1:n_sim, replace = TRUE)
  
  lb <- loss_base[idx]
  lc <- loss_clim[idx]
  
  VaR_b <- as.numeric(quantile(lb, 0.995))
  VaR_c <- as.numeric(quantile(lc, 0.995))
  
  ES_b <- mean(lb[lb >= VaR_b])
  ES_c <- mean(lc[lc >= VaR_c])
  
  diff_ES[b] <- ES_c - ES_b
}

results_boot_ES_geo <- data.frame(
  Mean_diff_ES = mean(diff_ES),
  IC_2_5 = quantile(diff_ES, 0.025),
  IC_97_5 = quantile(diff_ES, 0.975),
  p_value = mean(diff_ES <= 0)
)

cat("\n=== BOOTSTRAP TEST ON DELTA ES: GEOMETRIC SCORE ===\n")
print(results_boot_ES_geo)

# =========================
# 12. HISTORICAL SEVERITY VALIDATION
# =========================
cat("\n=== SEVERITY VALIDATION: GEOMETRIC SCORE ===\n")

t_test <- t.test(excess ~ regime, data = sev_data)
wilcox_test <- wilcox.test(excess ~ regime, data = sev_data)

print(t_test)
print(wilcox_test)

set.seed(123)

B <- 2000
diff_mean <- numeric(B)

for (b in 1:B) {
  
  xs <- sample(excess_stress, length(excess_stress), replace = TRUE)
  xn <- sample(excess_normal, length(excess_normal), replace = TRUE)
  
  diff_mean[b] <- mean(xs) - mean(xn)
}

results_severity_geo <- data.frame(
  Mean_normal = mean_normal,
  Mean_stress = mean_stress,
  Diff_stress_normal = mean_stress - mean_normal,
  Bootstrap_mean_diff = mean(diff_mean),
  Bootstrap_IC_2_5 = quantile(diff_mean, 0.025),
  Bootstrap_IC_97_5 = quantile(diff_mean, 0.975),
  Bootstrap_p_value = mean(diff_mean <= 0),
  T_test_p_value = t_test$p.value,
  Wilcox_p_value = wilcox_test$p.value
)

cat("\n=== BOOTSTRAP SEVERITY TEST: GEOMETRIC SCORE ===\n")
print(results_severity_geo)

cat("\n=== EXCESS REGRESSION: GEOMETRIC SCORE ===\n")
print(summary(lm_excess))

results_regression_geo <- data.frame(
  Intercept = coef(lm_excess)[1],
  Beta_climate = coef(lm_excess)[2],
  P_value_climate = summary(lm_excess)$coefficients[2, 4],
  R_squared = summary(lm_excess)$r.squared,
  Adj_R_squared = summary(lm_excess)$adj.r.squared
)

# =========================
# 13. EVT THRESHOLD ROBUSTNESS
# =========================
cat("\n=== EVT THRESHOLD ROBUSTNESS: GEOMETRIC SCORE ===\n")

tau_grid <- c(0.90, 0.95, 0.975)

results_tau_geo <- data.frame(
  tau = numeric(),
  u_hist = numeric(),
  u_sim = numeric(),
  n_excess = numeric(),
  VaR = numeric(),
  ES = numeric(),
  Delta_VaR = numeric(),
  Delta_ES = numeric()
)

set.seed(123)

for (tau_j in tau_grid) {
  
  u_hist_j <- as.numeric(quantile(df_evt$loss_12m, tau_j))
  
  sev_data_j <- df_evt %>%
    filter(loss_12m > u_hist_j) %>%
    mutate(excess = loss_12m - u_hist_j)
  
  lm_excess_j <- lm(log1p(excess) ~ climate_lag12, data = sev_data_j)
  
  clim_ref_j <- median(df_evt$climate_lag12, na.rm = TRUE)
  
  pred_ref_log_j <- predict(
    lm_excess_j,
    newdata = data.frame(climate_lag12 = clim_ref_j)
  )
  
  pred_ref_j <- as.numeric(exp(pred_ref_log_j) - 1)
  
  u_sim_j <- as.numeric(quantile(loss_base, tau_j))
  
  loss_clim_j <- numeric(n_sim)
  climate_draws_j <- sample(df_evt$climate_lag12, n_sim, replace = TRUE)
  
  for (i in 1:n_sim) {
    
    loss_i <- loss_base[i]
    
    if (loss_i <= u_sim_j) {
      
      loss_clim_j[i] <- loss_i
      
    } else {
      
      excess_base <- loss_i - u_sim_j
      
      pred_i_log <- predict(
        lm_excess_j,
        newdata = data.frame(climate_lag12 = climate_draws_j[i])
      )
      
      pred_i <- as.numeric(exp(pred_i_log) - 1)
      
      r_i <- pred_i / pred_ref_j
      r_i <- min(max(r_i, 1), r_max)
      
      loss_clim_j[i] <- u_sim_j + r_i * excess_base
    }
  }
  
  VaR_j <- as.numeric(quantile(loss_clim_j, 0.995))
  ES_j  <- mean(loss_clim_j[loss_clim_j >= VaR_j])
  
  results_tau_geo <- rbind(
    results_tau_geo,
    data.frame(
      tau = tau_j,
      u_hist = u_hist_j,
      u_sim = u_sim_j,
      n_excess = nrow(sev_data_j),
      VaR = VaR_j,
      ES = ES_j,
      Delta_VaR = VaR_j - VaR_base,
      Delta_ES = ES_j - ES_base
    )
  )
}

print(results_tau_geo)

# =========================
# 14. SCALING CAP ROBUSTNESS
# =========================
cat("\n=== SCALING CAP ROBUSTNESS: GEOMETRIC SCORE ===\n")

rmax_grid <- c(1.20, 1.25, 1.30)

results_rmax_geo <- data.frame(
  r_max = numeric(),
  VaR = numeric(),
  ES = numeric(),
  Delta_VaR = numeric(),
  Delta_ES = numeric()
)

set.seed(123)

for (rmax_j in rmax_grid) {
  
  loss_tmp <- numeric(n_sim)
  climate_draws_tmp <- sample(df_evt$climate_lag12, n_sim, replace = TRUE)
  
  for (i in 1:n_sim) {
    
    loss_i <- loss_base[i]
    
    if (loss_i <= u_sim) {
      
      loss_tmp[i] <- loss_i
      
    } else {
      
      excess_base <- loss_i - u_sim
      
      pred_i_log <- predict(
        lm_excess,
        newdata = data.frame(climate_lag12 = climate_draws_tmp[i])
      )
      
      pred_i <- as.numeric(exp(pred_i_log) - 1)
      
      r_i <- pred_i / pred_ref
      r_i <- min(max(r_i, 1), rmax_j)
      
      loss_tmp[i] <- u_sim + r_i * excess_base
    }
  }
  
  VaR_tmp <- as.numeric(quantile(loss_tmp, 0.995))
  ES_tmp  <- mean(loss_tmp[loss_tmp >= VaR_tmp])
  
  results_rmax_geo <- rbind(
    results_rmax_geo,
    data.frame(
      r_max = rmax_j,
      VaR = VaR_tmp,
      ES = ES_tmp,
      Delta_VaR = VaR_tmp - VaR_base,
      Delta_ES = ES_tmp - ES_base
    )
  )
}

print(results_rmax_geo)