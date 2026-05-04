
#- mesure de l'erreur Scaled Pinbal Loss

scaled_pinball_loss <- function(
        y_train,
        y_true,
        y_pred,
        prob
) {
    # Function to calculate Pinball loss for quantile regression
    
    # Import required libraries
    library(Metrics)  # To calculate mean absolute error (MAE)
    
    # Check the length of y_true and y_pred
    if (length(y_true) != length(y_pred)) {
        stop("y_true and y_pred must have the same length.")
    }
    
    if (prob<=0 || prob>=1) {
      stop("prob need to be between 0 and 1")
    }
    
    # Calculate the difference between y_true and quantile(y_pred)
    # with apply we considerate y_pred as a matrix 
    if(ncol(y_true)>=2){
      
      ligne <- apply(y_pred,2,quantile,prob) #for a matrix 1 indicates rows and 2 indicates columns
      print(ligne)
      y_new <- y_true
      
      for (i in 1:nrow(y_true)){
        y_new[i,] <- ligne
      }
      print("y_new")
      print(y_new)
      diff <- y_true - y_new
      
    } else {
      # With quantile we considerate y_pred as a vector
      diff <- y_true - quantile(x=y_pred,probs = prob)
    }
    
    diff = as.matrix(diff)
    print(diff)
    
    # Calculate the sign vector (1 for positive or zero differences, 0 for negative differences)
    sign <- ifelse(diff >= 0, 1, 0)
    
    
    # Calculate the Pinball loss for each data point
    loss <- prob * sign * diff - (1 - prob) * (1 - sign) * diff
    
    print(loss)
    
    # Calculate the output errors, weighted by sample_weight (if provided)
    output_errors <- mean(loss)
    
    #print(output_errors)

    n <- nrow(y_train)
    h <- nrow(y_true)
    
    y_true_1 = as.matrix(y_train[1:(n-1),])
    y_true_2 = as.matrix(y_train[2:n,])
    
    #print(y_true_1)
    #print(y_true_2)
    
    dif_y = mean(abs(y_true_2-y_true_1))

    spl = 1/h*output_errors/(1/(n-1)*dif_y)
    
    #print("SPL =")
    #print(spl)
    # Return the output_errors 
    return(spl)

}


#######################################

mean_scaled_pinball_loss <- function(
    y_train,
    y_true,
    y_pred
){
  res = 0
  quant = c(0.005,0.025,0.165,0.25,0.5,0.75,0.835,0.975,0.995)
  for( i in quant){ res = res + scaled_pinball_loss(y_train, y_true,y_pred,i) }
  res = 1/length(quant) * res
  return( res )
}


#######################################


eval_metric_spl <- function(y_train, preds, actual){
  return(mean_scaled_pinball_loss(y_train, actual,preds))
}