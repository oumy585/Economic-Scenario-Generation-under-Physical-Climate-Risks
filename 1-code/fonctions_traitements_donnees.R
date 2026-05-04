#- Import des data sous format csv
# Chemin pour aller chercher les donnees dans le dossier Input
dossier <- "./"
dossierInput = paste(dossier,"/Input/",sep="")

import_data_virgule <- function(nom_data){
  nom_data <- read.csv2(paste0(dossierInput, "/",nom_data,".csv"), sep=",")
  return(nom_data)
}


import_data <- function(nom_data){
  nom_data <- read.csv2(paste0(dossierInput, "/",nom_data,".csv"))
  return(nom_data)
}


#-Gestion des formats 
numerique <- function(var){
  if (class(var) == "character"){
  var = as.numeric(var)
  }
  return(var)
}

date <- function(var){
  if (class(var) == "character"){
    var = as.Date(var)
  }
  return(var)
}

#- Suppression des colonnes
sup_col <- function(indices,data){
  for(i in indices){
    data <- data[-i]
  }
  return(data)
}


#- Interpolation linéaire si NA 
# Fonction pour effectuer l'interpolation linéaire
interpolation_lineaire <- function(data) {
  n <- length(data)
  na_indice <- which(is.na(data))  # Indices des valeurs manquantes
  
  for (i in na_indice) {
    # Trouver les indices des valeurs précédente et suivante non manquantes
    prev_val <- max(which(!is.na(data[1:i])))
    next_val <- min(which(!is.na(data[(i+1):n]))) + i
    
    # Calculer la valeur interpolée
    data[i] <- data[prev_val] + (data[next_val] - data[prev_val]) / (next_val - prev_val) * (i - prev_val)
  }
  
  return(data)
}

na_interpolation_lineaire <- function(data){
  if (any(is.na(data))){
    return(interpolation_lineaire(data))
  }
}


#- Passage d'indice à un log return
# Formule : log_return_t = log(price_t / price_{t-1})
log_return <- function(col_data_histo){
  temp <- log(col_data_histo[2:length(col_data_histo)]/col_data_histo[1:(length(col_data_histo)-1)])
  return(temp*100)
}


#- Ajout de la volatilite historique dans la !! troisième colonne !! 
  #Hyp : 1 et 2 colonne pour resp. date et indice 

# col_data_histo : colonne qui sert au calcul de la volatilite historique

vol_histo <- function(Retard,data,col_data_histo){
  
  names(data)[2] <- "VOL" # Renommmer la colonne
  data$VOL <- 0 # On laisse une colonne pour mettre la volatilité
  
  for(i in Retard:nrow(data)){
    somme = 0
    m = mean(col_data_histo[(i-Retard+1):i])
    for(k in 0:(Retard-1)){
      somme = somme + (col_data_histo[i-k]-m)^2
    }
    data$VOL[i] <- sqrt(1/Retard * somme)
  }
  
  
  # On supprime les lignes où la volatilité est nulle
  data <- data[Retard:nrow(data),]
  
  return(data)
}





