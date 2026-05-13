set.seed(123)

library(readxl)
library(ahead)
library(zoo)
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================
# 1. DONNEES
# =========================
df <- read_excel(
  "C:/Users/OumyDIONE/Desktop/Travaux _with/Data/ESG_data_2000_2024.xlsx"
)

df <- df[order(df$time), ]

Y <- as.matrix(
  df[, c("CAC40","IPI_growth","Inflation","ShortRate","LongRate")]
)

# =========================
# 2. SCORES CLIMATIQUES
# =========================

Z_RX5  <- as.numeric(scale(df$RX5day))
Z_SPEI <- -as.numeric(scale(df$SPEI))
Z_WIND <- as.numeric(scale(df$WindExtreme))

# Score actuel : additif
df$climate_add <- rowMeans(
  cbind(Z_RX5, Z_SPEI, Z_WIND),
  na.rm = TRUE
)

# Quantiles empiriques via pnorm
Q_RX5  <- pnorm(Z_RX5)
Q_SPEI <- pnorm(Z_SPEI)
Q_WIND <- pnorm(Z_WIND)

eps <- 1e-8

Q_RX5  <- pmin(pmax(Q_RX5, eps), 1 - eps)
Q_SPEI <- pmin(pmax(Q_SPEI, eps), 1 - eps)
Q_WIND <- pmin(pmax(Q_WIND, eps), 1 - eps)

# Proposition Pierre-Charles : max convexe
df$climate_max5 <- pmax(Q_RX5, Q_SPEI, Q_WIND)^5

# Proposition Aurélien : interaction multiplicative
df$climate_prod <- 
  (1 + pmax(Z_RX5, 0)) *
  (1 + pmax(Z_SPEI, 0)) *
  (1 + pmax(Z_WIND, 0))

# Proposition Pierre-Charles : score logarithmique des queues
df$climate_logtail <- 
  (-log10(1 - Q_RX5)  - 1) +
  (-log10(1 - Q_SPEI) - 1) +
  (-log10(1 - Q_WIND) - 1)

# On centre-réduit les scores pour les rendre comparables
df$climate_add_z     <- as.numeric(scale(df$climate_add))
df$climate_max5_z    <- as.numeric(scale(df$climate_max5))
df$climate_prod_z    <- as.numeric(scale(df$climate_prod))
df$climate_logtail_z <- as.numeric(scale(df$climate_logtail))

# =========================
# 3. FONCTION POUR CREER LE LAG CLIMATIQUE
# =========================

make_climate_lag <- function(score) {
  ma6 <- zoo::rollmean(score, 6, fill = NA, align = "right")
  lag12 <- dplyr::lag(ma6, 12)
  return(lag12)
}

# =========================
# 4. RIDGE2F BASELINE
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
# 5. PERTE HISTORIQUE
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
    
    loss[t] <- -(100 * r_eq - 2 * dy_short * 300 - 8 * dy_long * 400 - 200 * dpi)
  }
  
  return(loss)
}

df$loss_12m <- compute_loss_hist(df)

# =========================
# 6. BOOTSTRAP RIDGE
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
  
  return(sim)
}

compute_loss <- function(sim) {
  
  x1  <- sim[1, ]
  x12 <- sim[12, ]
  
  r_eq     <- (x12["CAC40"]     - x1["CAC40"]) / 100
  dy_short <- (x12["ShortRate"] - x1["ShortRate"]) / 100
  dy_long  <- (x12["LongRate"]  - x1["LongRate"]) / 100
  dpi      <- (x12["Inflation"] - x1["Inflation"]) / 100
  
  loss <- -(100 * r_eq - 2 * dy_short * 300 - 8 * dy_long * 400 - 200 * dpi)
  
  return(loss)
}

# =========================
# 7. MONTE CARLO BASELINE
# =========================

n_sim <- 5000
loss_base <- numeric(n_sim)

for (i in 1:n_sim) {
  sim <- simulate_path()
  loss_base[i] <- compute_loss(sim)
}

VaR_base <- as.numeric(quantile(loss_base, 0.995))
ES_base  <- mean(loss_base[loss_base >= VaR_base])

cat("\n=== BASELINE ===\n")
cat("VaR base :", VaR_base, "\n")
cat("ES base  :", ES_base, "\n")

# =========================
# 8. FONCTION COMPLETE D'AJUSTEMENT CLIMATIQUE
# =========================

run_climate_tail_model <- function(df, climate_score_name, loss_base, tau = 0.95, r_max = 1.25) {
  
  df$climate_lag12 <- make_climate_lag(df[[climate_score_name]])
  
  df_evt <- df %>%
    select(loss_12m, climate_lag12) %>%
    na.omit()
  
  u_hist <- as.numeric(quantile(df_evt$loss_12m, tau))
  
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
  
  lm_excess <- lm(log1p(excess) ~ climate_lag12, data = sev_data)
  
  clim_ref <- median(df_evt$climate_lag12, na.rm = TRUE)
  
  pred_ref_log <- predict(
    lm_excess,
    newdata = data.frame(climate_lag12 = clim_ref)
  )
  
  pred_ref <- as.numeric(exp(pred_ref_log) - 1)
  
  u_sim <- as.numeric(quantile(loss_base, tau))
  
  n_sim <- length(loss_base)
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
  
  VaR_base <- as.numeric(quantile(loss_base, 0.995))
  ES_base  <- mean(loss_base[loss_base >= VaR_base])
  
  VaR_clim <- as.numeric(quantile(loss_clim, 0.995))
  ES_clim  <- mean(loss_clim[loss_clim >= VaR_clim])
  
  t_test <- tryCatch(
    t.test(excess ~ regime, data = sev_data),
    error = function(e) NA
  )
  
  wilcox_test <- tryCatch(
    wilcox.test(excess ~ regime, data = sev_data),
    error = function(e) NA
  )
  
  B <- 2000
  diff_mean <- numeric(B)
  
  for (b in 1:B) {
    
    xs <- sample(excess_stress, length(excess_stress), replace = TRUE)
    xn <- sample(excess_normal, length(excess_normal), replace = TRUE)
    
    diff_mean[b] <- mean(xs) - mean(xn)
  }
  
  boot_ci <- quantile(diff_mean, c(0.025, 0.975))
  boot_p  <- mean(diff_mean <= 0)
  
  result <- list(
    climate_score = climate_score_name,
    df_evt = df_evt,
    sev_data = sev_data,
    lm_excess = lm_excess,
    loss_clim = loss_clim,
    u_hist = u_hist,
    u_sim = u_sim,
    mean_normal = mean_normal,
    mean_stress = mean_stress,
    t_pvalue = ifelse(is.list(t_test), t_test$p.value, NA),
    wilcox_pvalue = ifelse(is.list(wilcox_test), wilcox_test$p.value, NA),
    boot_mean_diff = mean(diff_mean),
    boot_ci_low = boot_ci[1],
    boot_ci_high = boot_ci[2],
    boot_pvalue = boot_p,
    beta = coef(lm_excess)["climate_lag12"],
    beta_pvalue = summary(lm_excess)$coefficients["climate_lag12", "Pr(>|t|)"],
    r_squared = summary(lm_excess)$r.squared,
    VaR_base = VaR_base,
    VaR_clim = VaR_clim,
    ES_base = ES_base,
    ES_clim = ES_clim,
    Delta_VaR = VaR_clim - VaR_base,
    Delta_ES = ES_clim - ES_base,
    Delta_VaR_pct = 100 * (VaR_clim - VaR_base) / abs(VaR_base),
    Delta_ES_pct = 100 * (ES_clim - ES_base) / abs(ES_base)
  )
  
  return(result)
}

# =========================
# 9. COMPARAISON DES SCORES
# =========================

scores_to_test <- c(
  "climate_add_z",
  "climate_max5_z",
  "climate_prod_z",
  "climate_logtail_z"
)

results_list <- list()

for (score_name in scores_to_test) {
  
  cat("\n============================\n")
  cat("SCORE :", score_name, "\n")
  cat("============================\n")
  
  results_list[[score_name]] <- run_climate_tail_model(
    df = df,
    climate_score_name = score_name,
    loss_base = loss_base,
    tau = 0.95,
    r_max = 1.25
  )
  
  print(summary(results_list[[score_name]]$lm_excess))
}

# =========================
# 10. TABLEAU FINAL
# =========================

results_table <- do.call(
  rbind,
  lapply(results_list, function(x) {
    data.frame(
      climate_score = x$climate_score,
      mean_normal = x$mean_normal,
      mean_stress = x$mean_stress,
      beta = x$beta,
      beta_pvalue = x$beta_pvalue,
      r_squared = x$r_squared,
      t_pvalue = x$t_pvalue,
      wilcox_pvalue = x$wilcox_pvalue,
      boot_mean_diff = x$boot_mean_diff,
      boot_ci_low = x$boot_ci_low,
      boot_ci_high = x$boot_ci_high,
      boot_pvalue = x$boot_pvalue,
      VaR_base = x$VaR_base,
      VaR_clim = x$VaR_clim,
      ES_base = x$ES_base,
      ES_clim = x$ES_clim,
      Delta_VaR = x$Delta_VaR,
      Delta_ES = x$Delta_ES,
      Delta_VaR_pct = x$Delta_VaR_pct,
      Delta_ES_pct = x$Delta_ES_pct
    )
  })
)

print(results_table)

# =========================
# 11. TEST DE SIGNIFICATIVITE SUR ES
# =========================

bootstrap_ES_diff <- function(loss_base, loss_clim, n_boot = 1000) {
  
  n_sim <- length(loss_base)
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
  
  return(data.frame(
    mean_diff_ES = mean(diff_ES),
    ci_low = quantile(diff_ES, 0.025),
    ci_high = quantile(diff_ES, 0.975),
    p_value = mean(diff_ES <= 0)
  ))
}

ES_tests <- do.call(
  rbind,
  lapply(names(results_list), function(score_name) {
    
    test <- bootstrap_ES_diff(
      loss_base = loss_base,
      loss_clim = results_list[[score_name]]$loss_clim,
      n_boot = 1000
    )
    
    test$climate_score <- score_name
    
    return(test)
  })
)

ES_tests <- ES_tests %>%
  select(climate_score, everything())

print(ES_tests)

# =========================
# 12. ROBUSTESSE AU SEUIL EVT
# =========================

tau_grid <- c(0.90, 0.95, 0.975)

results_tau_all <- data.frame()

for (score_name in scores_to_test) {
  
  for (tau_j in tau_grid) {
    
    res_tau <- run_climate_tail_model(
      df = df,
      climate_score_name = score_name,
      loss_base = loss_base,
      tau = tau_j,
      r_max = 1.25
    )
    
    results_tau_all <- rbind(
      results_tau_all,
      data.frame(
        climate_score = score_name,
        tau = tau_j,
        VaR_clim = res_tau$VaR_clim,
        ES_clim = res_tau$ES_clim,
        Delta_VaR = res_tau$Delta_VaR,
        Delta_ES = res_tau$Delta_ES,
        Delta_VaR_pct = res_tau$Delta_VaR_pct,
        Delta_ES_pct = res_tau$Delta_ES_pct
      )
    )
  }
}

print(results_tau_all)

# =========================
# 13. ROBUSTESSE AU CAP r_max
# =========================

rmax_grid <- c(1.20, 1.25, 1.30)

results_rmax_all <- data.frame()

for (score_name in scores_to_test) {
  
  for (rmax_j in rmax_grid) {
    
    res_rmax <- run_climate_tail_model(
      df = df,
      climate_score_name = score_name,
      loss_base = loss_base,
      tau = 0.95,
      r_max = rmax_j
    )
    
    results_rmax_all <- rbind(
      results_rmax_all,
      data.frame(
        climate_score = score_name,
        r_max = rmax_j,
        VaR_clim = res_rmax$VaR_clim,
        ES_clim = res_rmax$ES_clim,
        Delta_VaR = res_rmax$Delta_VaR,
        Delta_ES = res_rmax$Delta_ES,
        Delta_VaR_pct = res_rmax$Delta_VaR_pct,
        Delta_ES_pct = res_rmax$Delta_ES_pct
      )
    )
  }
}

print(results_rmax_all)

# =========================
# 14. GRAPHE COMPARAISON VaR / ES
# =========================

plot_table <- results_table %>%
  select(climate_score, Delta_VaR_pct, Delta_ES_pct) %>%
  tidyr::pivot_longer(
    cols = c(Delta_VaR_pct, Delta_ES_pct),
    names_to = "risk_measure",
    values_to = "increase_pct"
  )

ggplot(plot_table, aes(x = climate_score, y = increase_pct, fill = risk_measure)) +
  geom_col(position = "dodge") +
  labs(
    x = "Climate score",
    y = "Increase in tail risk (%)",
    fill = "",
    title = "Impact of alternative climate stress indicators on tail risk"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

# =========================
# 15. GRAPHE DES DENSITES POUR LE MEILLEUR SCORE
# =========================

best_score <- results_table$climate_score[which.max(results_table$Delta_ES_pct)]

cat("\nMeilleur score selon Delta ES (%) :", best_score, "\n")

loss_clim_best <- results_list[[best_score]]$loss_clim

VaR_clim_best <- as.numeric(quantile(loss_clim_best, 0.995))
ES_clim_best  <- mean(loss_clim_best[loss_clim_best >= VaR_clim_best])

df_b <- data.frame(loss = loss_base, model = "Baseline")
df_c <- data.frame(loss = loss_clim_best, model = "Climate-adjusted")

xlim_range <- range(c(loss_base, loss_clim_best))

p1 <- ggplot(df_b, aes(x = loss)) +
  geom_density(color = "#2C7FB8", linewidth = 1.2) +
  geom_vline(xintercept = VaR_base, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = ES_base, linetype = "solid", color = "black") +
  xlim(xlim_range) +
  labs(
    title = "Baseline model",
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

p2 <- ggplot(df_c, aes(x = loss)) +
  geom_density(color = "#D95F02", linewidth = 1.2) +
  geom_vline(xintercept = VaR_clim_best, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = ES_clim_best, linetype = "solid", color = "black") +
  xlim(xlim_range) +
  labs(
    title = paste0("Climate-adjusted model: ", best_score),
    x = "Loss",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

print(p1 | p2)

# =========================
# 16. GRAPHE SEVERITE CONDITIONNELLE DU MEILLEUR SCORE
# =========================

sev_best <- results_list[[best_score]]$sev_data

ggplot(sev_best, aes(x = excess, fill = regime)) +
  geom_density(alpha = 0.45, color = NA) +
  scale_fill_manual(values = c("normal" = "#2C7FB8", "stress" = "#D95F02")) +
  labs(
    x = "Excess loss",
    y = "Density",
    fill = "",
    title = paste0("Climate-dependent tail severity: ", best_score)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

