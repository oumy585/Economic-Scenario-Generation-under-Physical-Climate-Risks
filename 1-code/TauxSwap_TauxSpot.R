dossier <- "./"

source(paste0(dossier,"/traitements_donnees.R"))
source(paste0(dossier,"/Bootstrap_Yield_Curve.R"))

#EUSA_ST_mois #données avec pour chaque ligne une date et en colonne les maturités

##############
#- Transformation des taux swaps contre EURIBOR 6M en des taux spots 

######
# Exemple pour la première ligne 
EUSA[,-1] <- EUSA[,-1]/100

ligne_1 = as.matrix(EUSA[1,2:ncol(EUSA)])
ligne_1
class(ligne_1)

# Plot de la série avant modifs
plot(x = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30), y = ligne_1*100, type = 'l', xlab = "Maturités", ylab = "Taux Swaps", main = "Courbe des Taux Swaps au 31 Mars 2000")
points(x = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30), y = ligne_1*100, col = "red")



cashflows_info = swap_cashflows_matrix(swap_rates = ligne_1, 
                                       maturities = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30), 
                                       tenor_swaps = "6m")

#ncol(cashflows_info$cashflow_dates)
#cashflows_info$cashflow_matrix


PrixZC_ligne1 <- bootstrap_zc(cashflows_info = cashflows_info)

TxSpot_ligne1 <- Taux_Spots_parLigne(swap_rates = ligne_1, 
                                     maturities = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30), 
                                     tenor_swaps = "6m")
TxSpot_ligne1

# Plot de la série après modifs
plot(x = seq(0.5,30,by = 0.5),y=TxSpot_ligne1*100, type = 'l', xlab = "Maturités", ylab = "Taux Spots", main = "Courbe des Taux Spots au 31 Mars 2000")
points(x = seq(0.5,30,by = 0.5), y = TxSpot_ligne1*100, col = "red")


######
# Généralisation à toutes les lignes

# Création d'une fonction
# swap_rates_data : dataframe avec la première colonne de date et en ligne les maturités 
Taux_Spots <- function(swap_rates_data, maturities,tenor_swaps)
{
  
  ligne_1 = as.matrix(swap_rates_data[1,2:ncol(swap_rates_data)])
  
  cashflows_info = swap_cashflows_matrix(swap_rates = ligne_1, 
                                         maturities = maturities , 
                                         tenor_swaps = tenor_swaps)
  
  # Création de la matrice vide 
  tx_spots <- matrix(data = NA, nrow = nrow(swap_rates_data), ncol = ncol(cashflows_info$cashflow_dates) )
  #print(nrow(swap_rates_data))
  
  for(i in 1:nrow(swap_rates_data))
  {
    ligne = as.matrix(swap_rates_data[i,2:ncol(swap_rates_data)])
    
    tx_spots[i,] <- Taux_Spots_parLigne(swap_rates = ligne, 
                                         maturities = maturities, 
                                         tenor_swaps = tenor_swaps)
  }

  
  if (!any(is.na(tx_spots)))
  {
    return(tx_spots)
  }
  else
  {
    return("ERREUR")
  }
}

######

TX_SPOTS <- Taux_Spots(swap_rates_data = EUSA, maturities = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30), tenor_swaps = "6m")
TX_SPOTS

##############

# Ajout des dates 
TX_SPOTS_date <- matrix(data = NA, nrow = nrow(TX_SPOTS), ncol = ncol(TX_SPOTS)+1) 
TX_SPOTS_date <- as.data.frame(TX_SPOTS_date)
TX_SPOTS_date[,1] <- EUSA[,1]
TX_SPOTS_date[,2:ncol(TX_SPOTS_date)] = TX_SPOTS
any(is.na(TX_SPOTS_date))
##############
#- Transformation en ST avec xts
# Conversion de la colonne de dates en un objet de type 'xts'
TauxSpots_ST_MOIS<- xts(TX_SPOTS_date[, -1], order.by = as.Date(TX_SPOTS_date$V1))

TauxSpots_ST_MOIS_c = TauxSpots_ST_MOIS


plot(TauxSpots_ST_MOIS, main = "Courbe des Taux Spot", ylab = "Taux Spot", xlab = "Date")

 

