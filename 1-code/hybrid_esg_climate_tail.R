set.seed(123)

library(readxl)
library(ahead)
library(zoo)
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================
# 1. DATA IMPORT
# =========================
df <- read_excel(
  "ESG_data_2000_2024.xlsx"
)

df <- df[order(df$time), ]

Y <- as.matrix(
  df[, c("CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate")]
)

# =========================
# 2. CLIMATE STRESS INDICATOR
# =========================
df$climate_score <- rowMeans(
  cbind(
    as.numeric(scale(df$RX5day)),
    -as.numeric(scale(df$SPEI)),
    as.numeric(scale(df$WindExtreme))
  ),
  na.rm = TRUE
)

df$climate_ma6 <- zoo::rollmean(
  df$climate_score,
  6,
  fill = NA,
  align = "right"
)

df$climate_lag12 <- dplyr::lag(df$climate_ma6, 12)

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

df$loss_12m <- compute_loss_hist(df)

# =========================
# 5. HISTORICAL EVT SAMPLE
# =========================
df_evt <- df %>%
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

cat("\n=== HISTORICAL EVT CHECK ===\n")
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

cat("\n=== CLIMATE TAIL ADJUSTMENT ===\n")
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
# 10. TAIL RISK MEASURES
# =========================
VaR_base <- as.numeric(quantile(loss_base, 0.995))
VaR_clim <- as.numeric(quantile(loss_clim, 0.995))

ES_base <- mean(loss_base[loss_base >= VaR_base])
ES_clim <- mean(loss_clim[loss_clim >= VaR_clim])

cat("\n=== MAIN RESULTS ===\n")
cat("Baseline VaR:", VaR_base, "\n")
cat("Climate-adjusted VaR:", VaR_clim, "\n")
cat("Delta VaR:", VaR_clim - VaR_base, "\n\n")

cat("Baseline ES:", ES_base, "\n")
cat("Climate-adjusted ES:", ES_clim, "\n")
cat("Delta ES:", ES_clim - ES_base, "\n")

# =========================
# 11. BOOTSTRAP SIGNIFICANCE TEST
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

cat("\n=== BOOTSTRAP TEST ON DELTA ES ===\n")
cat("Mean Delta ES:", mean(diff_ES), "\n")
print(quantile(diff_ES, c(0.025, 0.975)))
cat("Empirical p-value:", mean(diff_ES <= 0), "\n")

# =========================
# 12. HISTORICAL SEVERITY VALIDATION
# =========================
cat("\n=== SEVERITY VALIDATION ===\n")

t_test <- t.test(excess ~ regime, data = sev_data)
print(t_test)

wilcox_test <- wilcox.test(excess ~ regime, data = sev_data)
print(wilcox_test)

cat("\n=== BOOTSTRAP SEVERITY TEST ===\n")

set.seed(123)

B <- 2000
diff_mean <- numeric(B)

for (b in 1:B) {
  
  xs <- sample(excess_stress, length(excess_stress), replace = TRUE)
  xn <- sample(excess_normal, length(excess_normal), replace = TRUE)
  
  diff_mean[b] <- mean(xs) - mean(xn)
}

cat("Mean difference:", mean(diff_mean), "\n")
print(quantile(diff_mean, c(0.025, 0.975)))
cat("Empirical p-value:", mean(diff_mean <= 0), "\n")

cat("\n=== EXCESS REGRESSION ===\n")
print(summary(lm_excess))

# =========================
# 13. EVT THRESHOLD SENSITIVITY
# =========================
cat("\n=== EVT THRESHOLD SENSITIVITY ===\n")

for (q in c(0.90, 0.95, 0.975)) {
  
  u_test <- as.numeric(quantile(df_evt$loss_12m, q))
  
  sev_test <- df_evt %>%
    filter(loss_12m > u_test) %>%
    mutate(excess = loss_12m - u_test)
  
  sev_test$regime <- ifelse(
    sev_test$climate_lag12 >= stress_threshold,
    "stress",
    "normal"
  )
  
  cat("\nQuantile:", q, "\n")
  print(tapply(sev_test$excess, sev_test$regime, mean))
}

# =========================
# 14. PCA VALIDATION PLOT
# =========================
set.seed(123)

hist_data <- df %>%
  select(CAC40, IPI_growth, Inflation, ShortRate, LongRate) %>%
  na.omit()

sim <- simulate_path()

base_data <- as.data.frame(sim)
colnames(base_data) <- c("CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate")
base_data <- na.omit(base_data)

all_data <- rbind(hist_data, base_data)

pca <- prcomp(all_data, scale. = TRUE)

scores <- as.data.frame(pca$x[, 1:2])
colnames(scores) <- c("PC1", "PC2")

hist_pca <- scores[1:nrow(hist_data), ]
hist_pca$type <- "Historical"

base_pca <- scores[(nrow(hist_data) + 1):nrow(scores), ]
base_pca$type <- "Baseline"

hist_sample <- hist_pca %>% sample_n(min(800, nrow(hist_pca)))
base_sample <- base_pca %>% sample_n(min(800, nrow(base_pca)))

df_plot <- rbind(hist_sample, base_sample)

col_hist <- "#66C2A5"
col_base <- "#8DA0CB"

p_a <- ggplot(df_plot, aes(x = PC1, y = PC2, color = type, fill = type)) +
  stat_ellipse(geom = "polygon", alpha = 0.10, color = NA) +
  geom_point(size = 0.9, alpha = 0.35) +
  scale_color_manual(values = c("Historical" = col_hist, "Baseline" = col_base)) +
  scale_fill_manual(values = c("Historical" = col_hist, "Baseline" = col_base)) +
  labs(
    x = "PC1",
    y = "PC2",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    legend.text = element_text(size = 10)
  )

print(p_a)

# =========================
# 15. LOSS DENSITY PLOTS
# =========================
col_baseline <- "#2C7FB8"
col_climate  <- "#D95F02"

df_b <- data.frame(loss = loss_base)
df_c <- data.frame(loss = loss_clim)

xlim_range <- range(c(loss_base, loss_clim))

label_base <- paste0(
  "VaR = ", round(VaR_base, 2),
  "\nES = ", round(ES_base, 2)
)

label_clim <- paste0(
  "VaR = ", round(VaR_clim, 2),
  "\nES = ", round(ES_clim, 2)
)

p1 <- ggplot(df_b, aes(x = loss)) +
  geom_density(color = col_baseline, linewidth = 1.2) +
  geom_vline(xintercept = VaR_base, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = ES_base, linetype = "solid", color = "black") +
  annotate(
    "text",
    x = -42,
    y = 0.040,
    label = label_base,
    hjust = 0,
    size = 4
  ) +
  xlim(xlim_range) +
  labs(
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

p2 <- ggplot(df_c, aes(x = loss)) +
  geom_density(color = col_climate, linewidth = 1.2) +
  geom_vline(xintercept = VaR_clim, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = ES_clim, linetype = "solid", color = "black") +
  annotate(
    "text",
    x = -42,
    y = 0.040,
    label = label_clim,
    hjust = 0,
    size = 4
  ) +
  xlim(xlim_range) +
  labs(
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p1 | p2)

# =========================
# 16. CONDITIONAL TAIL SEVERITY PLOT
# =========================
p_c <- ggplot(sev_data, aes(x = excess, fill = regime)) +
  geom_density(alpha = 0.45, color = NA) +
  scale_fill_manual(values = c("normal" = "#2C7FB8", "stress" = "#D95F02")) +
  labs(
    x = "Excess loss",
    y = "Density",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

print(p_c)

# =========================
# 17. SCALING CAP ROBUSTNESS
# =========================
cat("\n=== SCALING CAP ROBUSTNESS ===\n")

rmax_grid <- c(1.20, 1.25, 1.30)

results_rmax <- data.frame(
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
  
  results_rmax <- rbind(
    results_rmax,
    data.frame(
      r_max = rmax_j,
      VaR = VaR_tmp,
      ES = ES_tmp,
      Delta_VaR = VaR_tmp - VaR_base,
      Delta_ES = ES_tmp - ES_base
    )
  )
}

print(results_rmax)

# =========================
# 18. EVT THRESHOLD ROBUSTNESS FOR VAR AND ES
# =========================
cat("\n=== EVT THRESHOLD ROBUSTNESS FOR VAR AND ES ===\n")

tau_grid <- c(0.90, 0.95, 0.975)

results_tau <- data.frame(
  tau = numeric(),
  u_hist = numeric(),
  u_sim = numeric(),
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
  
  if (nrow(sev_data_j) < 5) {
    warning(paste("Few exceedances for tau =", tau_j))
  }
  
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
  
  results_tau <- rbind(
    results_tau,
    data.frame(
      tau = tau_j,
      u_hist = u_hist_j,
      u_sim = u_sim_j,
      VaR = VaR_j,
      ES = ES_j,
      Delta_VaR = VaR_j - VaR_base,
      Delta_ES = ES_j - ES_base
    )
  )
}

print(results_tau)