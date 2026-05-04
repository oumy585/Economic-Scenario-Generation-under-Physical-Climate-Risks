
###########################
# Swaps cashflows matrix
swap_cashflows_matrix <- function(swap_rates, maturities, 
                         tenor_swaps = c("1m", "3m", "6m", "1y"))
{
  # maturities in years, a vector with one maturity per swap
  
  nb_swaps <- length(swap_rates)
  if(nb_swaps != length(maturities)) stop("There must be as much swap rates as maturities")
  
  freq_swaps <- match.arg(tenor_swaps)
  freq <- switch(freq_swaps, 
                 "1m" = 1/12,
                 "3m" = 1/4,
                 "6m" = 1/2,
                 "1y" = 1)
  
  cashflow_dates <- seq(from = freq, to = max(maturities), by = freq)
  nb_cashflow_dates <- length(cashflow_dates)
  nb_cashflow_dates_swaps <- (1/freq)*(maturities)
  
  swap_cashflows_matrix <- matrix(0, nrow = nb_swaps, ncol = nb_cashflow_dates)
  cashflow_dates_matrix <- matrix(0, nrow = nb_swaps, ncol = nb_cashflow_dates)
  nb_swap_dates <- rep(0, nb_swaps)
  
  for (i in 1:nb_swaps)
  {
    cashflows_swap_i <- rep(-1, nb_cashflow_dates)
    nb_cashflow_dates_swaps_i <- nb_cashflow_dates_swaps[i]
    swap_rate_i_times_freq <- swap_rates[i]*freq
    cashflows_swap_i[1:nb_cashflow_dates_swaps_i] <- rep(swap_rate_i_times_freq, 
                                                         nb_cashflow_dates_swaps_i)
    cashflows_swap_i[nb_cashflow_dates_swaps_i] <- cashflows_swap_i[nb_cashflow_dates_swaps_i] + 1
    swap_cashflows_matrix[i,] <- cashflows_swap_i
    nb_swap_dates[i] <- sum(cashflows_swap_i > -0.5)
    cashflow_dates_matrix[i, 1:nb_cashflow_dates_swaps_i] <- cashflow_dates[1:nb_cashflow_dates_swaps_i]  
  }
  
  names_swaps <- paste0("swap", 1:nb_swaps)
  names_dates <- paste0(cashflow_dates, "y")   
  
  rownames(swap_cashflows_matrix) <- names_swaps
  colnames(swap_cashflows_matrix) <- names_dates
  rownames(cashflow_dates_matrix) <- names_swaps
  colnames(cashflow_dates_matrix) <- names_dates
  names(nb_swap_dates) <- names_swaps 
  
  out <- list(nrow(cashflow_dates_matrix), maturities, nb_swap_dates, 
              swap_rates, cashflow_dates_matrix, swap_cashflows_matrix, freq)
  
  names(out) <- c("nb_swaps", "swaps_maturities", "nb_swap_dates", 
                  "swap_rates", "cashflow_dates", "cashflow_matrix", "tenor_swaps")
  
  return(out)
}

###########################

# 
# cashflows_info = swap_cashflows_matrix(swap_rates = c(-0.00337,
#                                                       -0.003610,
#                                                       -0.003647,
#                                                       -0.003413,
#                                                       -0.003047),
#                       maturities = c(0.5,1, 2, 3, 5),
#                       tenor_swaps = "6m")

cashflows_info = swap_cashflows_matrix(swap_rates = c(0.03,0.029,0.028,0.027,0.026),
                                       maturities = c(0.5,1, 2, 3, 5),
                                       tenor_swaps = "6m")


###########################
# Bootstrap zero-coupon prices from swap rates
bootstrap_zc <- function(cashflows_info, x0 = NULL, a = NULL, sigma = NULL)
{
  # On prend toutes les infos des swaps
  swap_rates <- cashflows_info$swap_rates
  nb_swaps <- cashflows_info$nb_swaps  
  nb_swap_dates <- cashflows_info$nb_swap_dates  
  cashflows_dates <- cashflows_info$cashflow_dates  
  cashflows_matrix <- cashflows_info$cashflow_matrix
  maturities <- cashflows_info$swaps_maturities
  tenor <- cashflows_info$tenor_swaps
   
  p <- rep(x = 0, times = nb_swap_dates[nb_swaps], each = 1 )
  
  # Solving for p, the first zero-coupon price (until maturity T1)
  nb_swap_dates1 <- nb_swap_dates[1]  
  p1 <- try(uniroot(function(p){
    return(crossprod(cashflows_matrix[1, 1:nb_swap_dates1], p) - 1)}, 
    interval = c(-1, 1), extendInt = "yes"), silent = TRUE)$root
  # Le but est de trouver p la racine tel que Cp-v=0 <=> Cp-1=0
  # $root gives the location of the root point
  p[1] <- p1
  #print(p)
  # Objective function for p2, p3, ..., pn
  # i: index for swaps; j: index for cashflow-payment dates
  new_p <- rep(x = 0, times = nb_swap_dates[nb_swaps], each = 1/tenor )
  
  OF <- function(p_opt) 
  {
    #cat("=======", "\n")
    #cat("i:", i, "\n")
    #cat("=======", "\n")
    
    #cat("=======", "\n")
    #cat("ligne:", ligne, "\n")
    #cat("=======", "\n")
    
    new_p[i] <- p_opt
    #print(new_p)
    
    #print(cashflows_matrix[ligne, 1:i])
    
    #print(p_opt)
    
    #cat("\n")
    #cat("prices:", new_p[1:i], "\n")
    
    diff <- i-indice_avant
    
    #cat("diff:", diff, "\n")
    if(i-indice_avant>1){
      #cat("avant interpolation prices:", new_p[1:i], "\n")
      
      new_p[indice_avant:i] = approx(c(new_p[indice_avant],new_p[i]),n=(i-indice_avant+1))$y
      
      #cat("apres interpolation prices:", new_p[1:i], "\n")
      
    }
    
    #cat("diffprice:", crossprod(cashflows_matrix[ligne, 1:i], new_p[1:i]) - 1, "\n")
    #cat("\n")
    
    return(crossprod(cashflows_matrix[ligne, 1:i], new_p[1:i]) - 1)
  }
  
  indice_avant <- 1
  lignes <- 1:nb_swaps
  k = 2
  
  # Looping on the on the other swaps
  for (i in nb_swap_dates[2:nb_swaps])
  {
    ligne <- lignes[k]
    new_p[1:i] <- p[1:i]
    #print(p)
    #print(new_p)

    pi <- try(uniroot(OF, interval = c(-1, 1), extendInt = "yes"), silent = TRUE)$root

    p[i] <- pi
    diff <- i-indice_avant
    #cat("diff:", diff, "\n")
    if(i-indice_avant>1){
      #cat("avant interpolation prices:", p[1:i], "\n")
      p[indice_avant:i] = approx(c(log(p[indice_avant]),log(p[i])),n=(i-indice_avant+1))$y
      p[indice_avant:i] = exp(p[indice_avant:i])
      #cat("apres interpolation prices:", p[1:i], "\n")
    }
    indice_avant <- i
    #print(indice_avant)
    k = k + 1

  }
  
  #print(p)
  
  plot(1:nb_swap_dates[nb_swaps], p, type = 'l')
  points(1:nb_swap_dates[nb_swaps], p, col = "red")
  
  # The pi's
  names(p) <- paste0("p", 1:nb_swap_dates[nb_swaps])
  
  return(p)
}

###########################

p = bootstrap_zc(cashflows_info = cashflows_info)
p

###########################
# Global function

Taux_Spots_parLigne <- function(swap_rates, maturities,tenor_swaps)
{
  
  cashflows_info = swap_cashflows_matrix(swap_rates = swap_rates, 
                        maturities = maturities, 
                        tenor_swaps = tenor_swaps)
  
  freq_swaps <- tenor_swaps
  freq <- switch(freq_swaps, 
                 "1m" = 1/12,
                 "3m" = 1/4,
                 "6m" = 1/2,
                 "1y" = 1)
  
  maturites <- seq(from = freq, to = max(maturities), by = freq)
  
  prix_zc <- bootstrap_zc(cashflows_info = cashflows_info)
  
  return(-1/maturites*log(prix_zc))
  
}


###########################
# 
# tx_spot = Taux_Spots_parLigne(swap_rates = c(-0.00337,
#                                              -0.003610,
#                                              -0.003647,
#                                              -0.003413,
#                                              -0.003047),
#                               maturities = c(0.5,1, 2, 3, 5),
#                               tenor_swaps = "6m")
# tx_spot
# 
# 
# 
# tx_spot2 = Taux_Spots_parLigne(swap_rates =c(0.01,0.02,0.023,0.042,0.021),
#                               maturities = c(0.5,1, 2, 3, 5),
#                               tenor_swaps = "6m")
# tx_spot2


###########################

