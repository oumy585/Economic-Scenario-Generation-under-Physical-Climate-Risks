library(readxl)
library(ahead)
library(bayesianrvfl)
library(caret)
library(foreach)
library(misc)

##############################
# DATA
##############################

df <- read_excel(
  "C:/Users/OumyDIONE/Desktop/Travaux _with/Data/ESG_data_2000_2024.xlsx"
)

df <- df[, c(
  "CAC40",
  "IPI_growth",
  "Inflation",
  "ShortRate",
  "LongRate"
)]

df_ts <- ts(
  as.matrix(df),
  start = c(2000,1),
  frequency = 12
)

##############################
# SPLIT
##############################

split_df_ts <- misc::splitts(
  df_ts,
  split_prob = 0.85
)

train_set <- split_df_ts$training
test_set  <- split_df_ts$testing

##############################
# TIME SLICES CV
##############################

time_slices <- caret::createTimeSlices(
  y = train_set[,1],
  initialWindow = 60,
  horizon = 24,
  skip = 12,
  fixedWindow = FALSE
)

##############################
# OBJECTIVE FUNCTION
##############################

objective_function_ridge <- function(params){
  
  lags <- round(params[1])
  nb_hidden <- round(params[2])
  lambda1 <- 10^params[3]
  lambda2 <- 10^params[4]
  
  slice_errors <- foreach(
    i = seq_along(time_slices$train),
    .combine = c,
    .packages = c("ahead","misc")
  ) %do% {
    
    train_idx <- time_slices$train[[i]]
    test_idx  <- time_slices$test[[i]]
    
    if(max(train_idx) > nrow(train_set)) return(NA)
    if(max(test_idx)  > nrow(train_set)) return(NA)
    
    y_train <- train_set[train_idx,,drop=FALSE]
    y_test  <- train_set[test_idx,,drop=FALSE]
    
    if(nrow(y_train) <= lags + 1) return(NA)
    
    fit <- tryCatch(
      ahead::ridge2f(
        y = y_train,
        h = nrow(y_test),
        lags = lags,
        nb_hidden = nb_hidden,
        lambda_1 = lambda1,
        lambda_2 = lambda2,
        type_pi = "movingblockbootstrap"
      ),
      error = function(e) return(NA)
    )
    
    if(is.na(fit)[1]) return(NA)
    
    sapply(
      1:ncol(fit$lower),
      function(j)
        misc::winkler_score(
          actual = y_test[,j],
          lower = fit$lower[,j],
          upper = fit$upper[,j],
          level = 95
        )
    )
  }
  
  median(slice_errors, na.rm = TRUE)
}

##############################
# BOUNDS (REALISTES POUR GSE)
##############################

bounds <- list(
  lags = c(1, 12),
  nb_hidden = c(5, 20),
  log10_lambda1 = c(-2, 2),
  log10_lambda2 = c(0, 6)
)

##############################
# BAYES OPT
##############################

res_opt <- bayesianrvfl::bayes_opt(
  objective_function_ridge,
  lower = c(
    bounds$lags[1],
    bounds$nb_hidden[1],
    bounds$log10_lambda1[1],
    bounds$log10_lambda2[1]
  ),
  upper = c(
    bounds$lags[2],
    bounds$nb_hidden[2],
    bounds$log10_lambda1[2],
    bounds$log10_lambda2[2]
  )
)

##############################
# RESULT
##############################

res_opt


#######################################################################################
best <- res_opt$best_param

lags_opt <- round(best[1])
hidden_opt <- round(best[2])
lambda1_opt <- 10^best[3]
lambda2_opt <- 10^best[4]


fit_final <- ahead::ridge2f(
  y = train_set,
  h = nrow(test_set),
  lags = lags_opt,
  nb_hidden = hidden_opt,
  lambda_1 = lambda1_opt,
  lambda_2 = lambda2_opt,
  type_pi = "movingblockbootstrap"
)



pred_mean <- fit_final$mean
pred_lower <- fit_final$lower
pred_upper <- fit_final$upper


winkler_test <- sapply(
  1:ncol(test_set),
  function(j)
    misc::winkler_score(
      actual = test_set[,j],
      lower = pred_lower[,j],
      upper = pred_upper[,j],
      level = 95
    )
)

winkler_test

mae_test <- colMeans(abs(test_set - pred_mean))

rmse_test <- sqrt(
  colMeans((test_set - pred_mean)^2)
) 

mae_test

rmse_test
