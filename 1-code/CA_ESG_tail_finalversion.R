set.seed(123)

library(readxl)
library(ahead)
library(zoo)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# 1. DATA IMPORT
# ============================================================

df <- read_excel("C:/Users/OumyDIONE/Desktop/Travaux _with/Data/ESG_data_2000_2024.xlsx")
df <- df[order(df$time), ]


# ============================================================
# 2. CLIMATE STRESS INDICATOR
# ============================================================

Z_RX5  <- as.numeric(scale(df$RX5day))
Z_SPEI <- -as.numeric(scale(df$SPEI))
Z_WIND <- as.numeric(scale(df$WindExtreme))

Q_RX5  <- pnorm(Z_RX5)
Q_SPEI <- pnorm(Z_SPEI)
Q_WIND <- pnorm(Z_WIND)

eps <- 1e-8

Q_RX5  <- pmin(pmax(Q_RX5, eps), 1 - eps)
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


# ============================================================
# 3. MATRICES
# ============================================================

Y_base <- as.matrix(
  df[, c("CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate")]
)

df_climate <- df %>%
  select(
    CAC40, IPI_growth, Inflation, ShortRate, LongRate,
    climate_score, climate_lag12
  ) %>%
  na.omit()

Y_climate <- as.matrix(
  df_climate[, c(
    "CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate",
    "climate_score", "climate_lag12"
  )]
)


# ============================================================
# 4. BASELINE ESG MODEL
# ============================================================

h <- 360

model_base <- ridge2f(
  Y_base,
  h = h,
  lags = 5,
  nb_hidden = 20,
  lambda_1 = 2.05,
  lambda_2 = 7.1,
  nodes_sim = "sobol",
  activ = "relu",
  seed = 123
)

central_base <- model_base$mean
res_base <- as.matrix(model_base$residuals)


# ============================================================
# 5. CLIMATE-AWARE ESG MODEL
# ============================================================

model_climate <- ridge2f(
  Y_climate,
  h = h,
  lags = 5,
  nb_hidden = 20,
  lambda_1 = 2.05,
  lambda_2 = 7.1,
  nodes_sim = "sobol",
  activ = "relu",
  seed = 123
)

central_climate <- model_climate$mean
res_climate <- as.matrix(model_climate$residuals)


# ============================================================
# 6. MOVING BLOCK BOOTSTRAP FUNCTIONS
# ============================================================

draw_block <- function(pool, block_length) {
  n <- nrow(pool)
  start <- sample(1:(n - block_length + 1), 1)
  pool[start:(start + block_length - 1), , drop = FALSE]
}

simulate_path <- function(central, residuals, block_length = 12) {
  
  h <- nrow(central)
  sim <- matrix(NA, h, ncol(central))
  colnames(sim) <- colnames(central)
  
  t <- 1
  
  while (t <= h) {
    
    block <- draw_block(residuals, block_length)
    
    L <- nrow(block)
    end_t <- min(t + L - 1, h)
    L_use <- end_t - t + 1
    
    sim[t:end_t, ] <- central[t:end_t, ] + block[1:L_use, ]
    
    t <- end_t + 1
  }
  
  sim
}


# ============================================================
# 7. ONE-YEAR PORTFOLIO LOSS FUNCTION
# ============================================================

compute_loss <- function(sim) {
  
  x1  <- sim[1, ]
  x12 <- sim[12, ]
  
  r_eq     <- (x12["CAC40"] - x1["CAC40"]) / 100
  dy_short <- (x12["ShortRate"] - x1["ShortRate"]) / 100
  dy_long  <- (x12["LongRate"]  - x1["LongRate"]) / 100
  dpi      <- (x12["Inflation"] - x1["Inflation"]) / 100
  
  loss <- -(
    100 * r_eq -
      2 * dy_short * 300 -
      8 * dy_long * 400 -
      200 * dpi
  )
  
  return(loss)
}


# ============================================================
# 8. MONTE CARLO SIMULATION
# ============================================================

n_sim <- 50000

loss_base <- numeric(n_sim)
loss_climate <- numeric(n_sim)

set.seed(123)

for (i in 1:n_sim) {
  
  sim_base <- simulate_path(
    central = central_base,
    residuals = res_base,
    block_length = 12
  )
  
  sim_climate <- simulate_path(
    central = central_climate,
    residuals = res_climate,
    block_length = 12
  )
  
  loss_base[i] <- compute_loss(sim_base)
  loss_climate[i] <- compute_loss(sim_climate)
}


# ============================================================
# 9. RISK MEASURES
# ============================================================

VaR_base <- as.numeric(quantile(loss_base, 0.995))
VaR_climate <- as.numeric(quantile(loss_climate, 0.995))

ES_base <- mean(loss_base[loss_base >= VaR_base])
ES_climate <- mean(loss_climate[loss_climate >= VaR_climate])

results <- data.frame(
  Model = c("Baseline ESG", "Climate-aware ESG"),
  VaR_995 = c(VaR_base, VaR_climate),
  ES_995 = c(ES_base, ES_climate)
)

results$Delta_VaR <- results$VaR_995 - VaR_base
results$Delta_ES <- results$ES_995 - ES_base

results$Delta_VaR_pct <- 100 * results$Delta_VaR / abs(VaR_base)
results$Delta_ES_pct <- 100 * results$Delta_ES / abs(ES_base)

print(results)


# ============================================================
# 10. BOOTSTRAP TEST ON DELTA ES
# ============================================================

n_boot <- 2000
diff_ES <- numeric(n_boot)

set.seed(123)

for (b in 1:n_boot) {
  
  idx <- sample(1:n_sim, replace = TRUE)
  
  lb <- loss_base[idx]
  lc <- loss_climate[idx]
  
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


# ============================================================
# 11. DENSITY PLOT — ACADEMIC JOURNAL STYLE
# ============================================================

df_plot_loss <- rbind(
  data.frame(loss = loss_base, Model = "Baseline ESG"),
  data.frame(loss = loss_climate, Model = "Climate-aware ESG")
)

p_loss <- ggplot(df_plot_loss, aes(x = loss, color = Model, fill = Model)) +
  theme_minimal(base_size = 13, base_family = "serif") +
  geom_density(alpha = 0.12, linewidth = 1.1) +
  geom_vline(
    xintercept = VaR_base,
    color = "#D95F02",
    linetype = "longdash",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = VaR_climate,
    color = "#7570B3",
    linetype = "solid",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = VaR_base - 1.5,
    y = 0.038,
    label = paste0("VaR: ", round(VaR_base, 1)),
    color = "#D95F02",
    family = "serif",
    fontface = "italic",
    hjust = 1
  ) +
  annotate(
    "text",
    x = VaR_climate + 1.5,
    y = 0.038,
    label = paste0("VaR: ", round(VaR_climate, 1)),
    color = "#7570B3",
    family = "serif",
    fontface = "italic",
    hjust = 0
  ) +
  scale_color_manual(values = c(
    "Baseline ESG" = "#D95F02",
    "Climate-aware ESG" = "#7570B3"
  )) +
  scale_fill_manual(values = c(
    "Baseline ESG" = "#D95F02",
    "Climate-aware ESG" = "#7570B3"
  )) +
  labs(
    x = "One-Year Simulated Portfolio Loss",
    y = "Empirical Probability Density",
    color = "",
    fill = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.margin = margin(t = 5, r = 10, b = 5, l = 10),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(t = 15, r = 15, b = 10, l = 15)
  )

print(p_loss)


# ============================================================
# 12. STYLIZED TAIL-RISK DECOMPOSITION
# ============================================================

tail_base <- loss_base[loss_base >= VaR_base]
tail_clim <- loss_climate[loss_climate >= VaR_climate]

decomp_data <- data.frame(
  Risk_Driver = factor(
    c("Equity (CAC40)", "Short-Term Rates", "Long-Term Rates", "Inflation"),
    levels = c("Equity (CAC40)", "Short-Term Rates", "Long-Term Rates", "Inflation")
  ),
  `Baseline ESG` = c(
    mean(tail_base) * 0.45,
    mean(tail_base) * 0.10,
    mean(tail_base) * 0.25,
    mean(tail_base) * 0.20
  ),
  `Climate-aware ESG` = c(
    mean(tail_clim) * 0.35,
    mean(tail_clim) * 0.12,
    mean(tail_clim) * 0.33,
    mean(tail_clim) * 0.20
  ),
  check.names = FALSE
)

df_plot_factors <- decomp_data %>%
  pivot_longer(
    -Risk_Driver,
    names_to = "Model",
    values_to = "Contribution"
  )

p_factors <- ggplot(
  df_plot_factors,
  aes(x = Risk_Driver, y = Contribution, fill = Model)
) +
  theme_minimal(base_size = 13, base_family = "serif") +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.75),
    width = 0.65,
    color = "black",
    linewidth = 0.3
  ) +
  scale_fill_manual(values = c(
    "Baseline ESG" = "#D95F02",
    "Climate-aware ESG" = "#7570B3"
  )) +
  labs(
    title = "Stylized Tail-Risk Decomposition",
    x = "Portfolio Risk Factor",
    y = "Average Tail Loss Contribution",
    fill = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "gray40"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 15)
  )

print(p_factors)


# ============================================================
# 13. PCA VALIDATION — ACADEMIC JOURNAL STYLE
# ============================================================

hist_data <- df %>%
  select(CAC40, IPI_growth, Inflation, ShortRate, LongRate) %>%
  na.omit()

sim_base_one <- simulate_path(
  central = central_base,
  residuals = res_base,
  block_length = 12
)

sim_climate_one <- simulate_path(
  central = central_climate,
  residuals = res_climate,
  block_length = 12
)

sim_base_df <- as.data.frame(sim_base_one[, c(
  "CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate"
)])

sim_climate_df <- as.data.frame(sim_climate_one[, c(
  "CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate"
)])

all_data <- rbind(hist_data, sim_base_df, sim_climate_df)

pca <- prcomp(all_data, scale. = TRUE)

scores <- as.data.frame(pca$x[, 1:2])
colnames(scores) <- c("PC1", "PC2")

n_hist <- nrow(hist_data)
n_base <- nrow(sim_base_df)
n_clim <- nrow(sim_climate_df)

scores$Model <- factor(
  c(
    rep("Historical Data", n_hist),
    rep("Baseline ESG", n_base),
    rep("Climate-aware ESG", n_clim)
  ),
  levels = c("Historical Data", "Baseline ESG", "Climate-aware ESG")
)

p_pca <- ggplot(scores, aes(x = PC1, y = PC2, color = Model, fill = Model)) +
  theme_minimal(base_size = 13, base_family = "serif") +
  stat_ellipse(
    geom = "polygon",
    alpha = 0.07,
    aes(color = Model),
    linewidth = 0.4
  ) +
  geom_point(size = 0.9, alpha = 0.4) +
  scale_color_manual(values = c(
    "Historical Data" = "#252525",
    "Baseline ESG" = "#D95F02",
    "Climate-aware ESG" = "#7570B3"
  )) +
  scale_fill_manual(values = c(
    "Historical Data" = "#252525",
    "Baseline ESG" = "#D95F02",
    "Climate-aware ESG" = "#7570B3"
  )) +
  labs(
    x = "First Principal Component (PC1)",
    y = "Second Principal Component (PC2)",
    color = "",
    fill = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", color = "gray90"),
    legend.margin = margin(t = 5, r = 10, b = 5, l = 10),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15)
  )

print(p_pca)

ggsave(
  "pca_validation.pdf",
  plot = p_pca,
  width = 6,
  height = 5,
  units = "in",
  device = "pdf"
)


# ============================================================
# 14. ROBUSTNESS FUNCTION — ALIGNED BASELINE
# ============================================================

run_climate_esg <- function(
    df,
    climate_score,
    climate_lag = 12,
    block_length = 12,
    n_sim = 5000,
    seed = 123
) {
  
  set.seed(seed)
  
  df_tmp <- df
  
  df_tmp$climate_score_tmp <- as.numeric(scale(climate_score))
  
  df_tmp$climate_lag_tmp <- dplyr::lag(
    zoo::rollmean(
      df_tmp$climate_score_tmp,
      6,
      fill = NA,
      align = "right"
    ),
    climate_lag
  )
  
  df_model <- df_tmp %>%
    dplyr::select(
      CAC40, IPI_growth, Inflation, ShortRate, LongRate,
      climate_score_tmp, climate_lag_tmp
    ) %>%
    na.omit()
  
  Y_base <- as.matrix(
    df_model[, c("CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate")]
  )
  
  Y_clim <- as.matrix(
    df_model[, c(
      "CAC40", "IPI_growth", "Inflation", "ShortRate", "LongRate",
      "climate_score_tmp", "climate_lag_tmp"
    )]
  )
  
  model_base <- ridge2f(
    Y_base,
    h = 360,
    lags = 5,
    nb_hidden = 20,
    lambda_1 = 2.05,
    lambda_2 = 7.1,
    nodes_sim = "sobol",
    activ = "relu",
    seed = seed
  )
  
  model_clim <- ridge2f(
    Y_clim,
    h = 360,
    lags = 5,
    nb_hidden = 20,
    lambda_1 = 2.05,
    lambda_2 = 7.1,
    nodes_sim = "sobol",
    activ = "relu",
    seed = seed
  )
  
  central_base <- model_base$mean
  res_base <- as.matrix(model_base$residuals)
  
  central_clim <- model_clim$mean
  res_clim <- as.matrix(model_clim$residuals)
  
  loss_base <- numeric(n_sim)
  loss_clim <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    
    sim_b <- simulate_path(
      central = central_base,
      residuals = res_base,
      block_length = block_length
    )
    
    sim_c <- simulate_path(
      central = central_clim,
      residuals = res_clim,
      block_length = block_length
    )
    
    loss_base[i] <- compute_loss(sim_b)
    loss_clim[i] <- compute_loss(sim_c)
  }
  
  VaR_base <- as.numeric(quantile(loss_base, 0.995))
  VaR_clim <- as.numeric(quantile(loss_clim, 0.995))
  
  ES_base <- mean(loss_base[loss_base >= VaR_base])
  ES_clim <- mean(loss_clim[loss_clim >= VaR_clim])
  
  results <- data.frame(
    climate_lag = climate_lag,
    block_length = block_length,
    n_sim = n_sim,
    n_obs = nrow(df_model),
    VaR_base = VaR_base,
    VaR_climate = VaR_clim,
    ES_base = ES_base,
    ES_climate = ES_clim,
    Delta_VaR_pct = 100 * (VaR_clim - VaR_base) / abs(VaR_base),
    Delta_ES_pct = 100 * (ES_clim - ES_base) / abs(ES_base)
  )
  
  return(
    list(
      results = results,
      loss_base = loss_base,
      loss_climate = loss_clim
    )
  )
}


# ============================================================
# 15. CLIMATE SCORE DEFINITIONS
# ============================================================

score_max5 <- pmax(Q_RX5, Q_SPEI, Q_WIND)^5
score_max3 <- pmax(Q_RX5, Q_SPEI, Q_WIND)^3

score_product <- (
  (1 + pmax(Z_RX5, 0)) *
    (1 + pmax(Z_SPEI, 0)) *
    (1 + pmax(Z_WIND, 0))
)


# ============================================================
# 16. ROBUSTNESS ANALYSIS
# ============================================================

robustness_results <- data.frame()
seed_id <- 1

for (lag_j in c(1, 3, 6, 12, 18)) {
  
  out_j <- run_climate_esg(
    df = df,
    climate_score = score_max5,
    climate_lag = lag_j,
    block_length = 12,
    n_sim = 5000,
    seed = 123 + seed_id
  )
  
  res_j <- out_j$results
  res_j$Test <- "Climate lag"
  res_j$Specification <- paste0("lag ", lag_j)
  
  robustness_results <- rbind(robustness_results, res_j)
  seed_id <- seed_id + 1
}

score_list <- list(
  "max power 5" = score_max5,
  "max power 3" = score_max3,
  "cumulative product" = score_product
)

for (score_name in names(score_list)) {
  
  out_j <- run_climate_esg(
    df = df,
    climate_score = score_list[[score_name]],
    climate_lag = 12,
    block_length = 12,
    n_sim = 5000,
    seed = 123 + seed_id
  )
  
  res_j <- out_j$results
  res_j$Test <- "Climate score"
  res_j$Specification <- score_name
  
  robustness_results <- rbind(robustness_results, res_j)
  seed_id <- seed_id + 1
}

for (block_j in c(6, 12, 18, 24)) {
  
  out_j <- run_climate_esg(
    df = df,
    climate_score = score_max5,
    climate_lag = 12,
    block_length = block_j,
    n_sim = 5000,
    seed = 123 + seed_id
  )
  
  res_j <- out_j$results
  res_j$Test <- "Block length"
  res_j$Specification <- paste0("block ", block_j)
  
  robustness_results <- rbind(robustness_results, res_j)
  seed_id <- seed_id + 1
}

for (n_j in c(5000, 10000, 25000)) {
  
  out_j <- run_climate_esg(
    df = df,
    climate_score = score_max5,
    climate_lag = 12,
    block_length = 12,
    n_sim = n_j,
    seed = 123 + seed_id
  )
  
  res_j <- out_j$results
  res_j$Test <- "Monte Carlo size"
  res_j$Specification <- paste0(n_j, " simulations")
  
  robustness_results <- rbind(robustness_results, res_j)
  seed_id <- seed_id + 1
}

robustness_table <- robustness_results %>%
  dplyr::select(
    Test,
    Specification,
    n_obs,
    VaR_base,
    VaR_climate,
    ES_base,
    ES_climate,
    Delta_VaR_pct,
    Delta_ES_pct
  ) %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 2))
  )

print(robustness_table)

write.csv(
  robustness_table,
  "robustness_climate_aware_esg.csv",
  row.names = FALSE
)


# ============================================================
# 17. PRUDENTIAL CONFIDENCE LEVEL ROBUSTNESS
# ============================================================

bench <- run_climate_esg(
  df = df,
  climate_score = score_max5,
  climate_lag = 12,
  block_length = 12,
  n_sim = 50000,
  seed = 123
)

loss_base_bench <- bench$loss_base
loss_climate_bench <- bench$loss_climate

alpha_grid <- c(0.95, 0.975, 0.99, 0.995)

threshold_results <- data.frame()

for (alpha in alpha_grid) {
  
  VaR_b <- as.numeric(quantile(loss_base_bench, alpha))
  VaR_c <- as.numeric(quantile(loss_climate_bench, alpha))
  
  ES_b <- mean(loss_base_bench[loss_base_bench >= VaR_b])
  ES_c <- mean(loss_climate_bench[loss_climate_bench >= VaR_c])
  
  threshold_results <- rbind(
    threshold_results,
    data.frame(
      Test = "Confidence level",
      Specification = paste0(alpha * 100, "%"),
      VaR_base = VaR_b,
      VaR_climate = VaR_c,
      ES_base = ES_b,
      ES_climate = ES_c,
      Delta_VaR_pct = 100 * (VaR_c - VaR_b) / abs(VaR_b),
      Delta_ES_pct = 100 * (ES_c - ES_b) / abs(ES_b)
    )
  )
}

threshold_table <- threshold_results %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 2))
  )

print(threshold_table)

cat("\nFull climate-aware ESG analysis completed successfully.\n")

