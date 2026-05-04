install.packages("NMOF")
library(NMOF)
library(xts)
dossier <- "./"
dossierFigures=paste(dossier,"/figures",sep="")
source(paste0(dossier,"/TauxSwap_TauxSpot.R"))

#################################################

#install.packages("NMOF")
library(NMOF)

#################################################
TX_SPOTS_date[,2:ncol(TX_SPOTS_date)] = TX_SPOTS_date[,2:ncol(TX_SPOTS_date)] * 100
TauxSpots_ST_MOIS = TauxSpots_ST_MOIS *100
data = TX_SPOTS_date #dataframe
data_ST_mois = TauxSpots_ST_MOIS #STM

#################################################
#- Extraction des composantes de Nelson Siegel

#Initialisation de la gestion des figures
sauveFigure=function(){
  numeroFigure=length(list.files(dossierFigures))+1
  #print(numeroFigure)
  png(file=paste(dossierFigures,"/fig",numeroFigure,".png", sep=""), width  = 2*2100, height = 2100, res=300, pointsize= 12)
} 

# On prend l'échelle suivante : on se base sur la plus petite maturité qui est 6 mois => on dira que 6 mois est 0.5 pour 6/12 mois = 1/2
## A ce moment dans le df data on a 13 maturites : 6M, 1A, 2A, 3A, 4A, 5A, 7A, 10A, 12A, 15A, 20A, 25A, 30A où j'ai les valeurs par mois entre 01/01/2001 et 31/05/2023 269 observations

x <- c(0.5,1,2,3,4,5,7,10,12,15,20,25,30)
y <- c(1:nrow(EUSA))
z <- as.matrix(data[,2:ncol(EUSA)]*10^2)
z_trans <- t(z)

# Dans mon fichier de données data.csv l'unite est le %. Sachant que 0.01% donne un 1 bp (bps = basis points), on multiplie nos valeur par 100.
# Par exemple : Une hausse du taux d’intérêt de 150 points de base équivaut à une hausse de 1,5 %

#sauveFigure()
persp(x,y,z_trans,theta=-220,phi=10,xlab='Maturite',ylab='Courbe',zlab='Taux (en bp)',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title("Historique de courbes EUSA par mois")
#dev.off()


#################################################
#- Extraction des composantes de Nelson Siegel

x <- seq(0.5, 30, by = 0.5)
y <- c(1:nrow(data))
z <- as.matrix(data[,2:ncol(data)]*10^2)
z_trans <- t(z)

# Dans mon fichier de données data.csv l'unite est le %. Sachant que 0.01% donne un 1 bp (bps = basis points), on multiplie nos valeur par 100.
# Par exemple : Une hausse du taux d’intérêt de 150 points de base équivaut à une hausse de 1,5 %

#sauveFigure()
persp(x,y,z_trans,theta=-220,phi=10,xlab='Maturite',ylab='Courbe',zlab='Taux (en bp)',col="lightgreen",expand=0.5,shade=0.8,ticktype="detailed")
title("Historique des courbes de taux Spot par mois")
#dev.off()

#################################################
# On va commencer par fixer certains paramètres
fixeTau0=2.25 #Parametres d'echelle : estime (NULL) ou fixe (valeur)
# Je fais le choix de le fixer comme F. Planchet dans son exemple

maturites = c(0.5,1,2,3,4,5,7,10,12,15,20,25,30) #nom des colonnes sous forme de string donc je le fais a main
maturites_txSpot = seq(0.5, 30, by = 0.5)

#################################################
#-------------------------------------------------------------------------------
# Corresponcance entre la formule du memoire et le nom des variables choisies :
# - alpha = fixeTau0 = tau = param[4]
# - mu_1 = mu_1 = param[1]
# - mu_2 = mu_2 = param[2]
# - mu_3 = mu_3 = param[3]
# - T = maturite
# - dataTaux = fichier QUE des taux => le cas quand on le transforme par une série temporelle
# - R(t,T) = Yields

#-------------------------------------------------------------------------------
#recuperation des resultats avec la fonction NSf

facteursNs =  NMOF::NSf(lambda = fixeTau0,tm=maturites)
mat_coefs_NS = matrix(NA,ncol=3,nrow = nrow(EUSA_ST_mois))

for (i in 1:nrow(EUSA_ST_mois)){
  mat_coefs_NS[i,] = .lm.fit(x = facteursNs, y = EUSA_ST_mois[i,])$coefficients
}
# On a mu(1), mu(2) et mu(3)


mat_coefs = matrix(NA,ncol=4,nrow = nrow(EUSA_ST_mois))
colnames(mat_coefs) = c("tauxCourt", "tauxLong", "convexite","tau")

#- Taux court 
mat_coefs[,1] = mat_coefs_NS[,1] + mat_coefs_NS[,2]

#- Taux long
mat_coefs[,2] = mat_coefs_NS[,1]

#- Convexité 
mat_coefs[,3] = mat_coefs_NS[,3]


mat_coefs[,4] = matrix(fixeTau0,ncol=1,nrow = nrow(data_ST_mois))


#-------------------------------------------------------------------------------
#Recalcul des coefficient r,l et c e partir des beta(i) : l=beta(1), l-r=-beta(2), c=beta(3)

tauxCourtHist=mat_coefs[,'tauxCourt']
tauxLongHist =mat_coefs[,"tauxLong"]
convexiteHist=mat_coefs[,"convexite"]
tauHist=mat_coefs[,"tau"]

#-------------------------------------------------------------------------------

#sauveFigure()
xLabels=c(1:nrow(EUSA_ST_mois))
plot(xLabels,mat_coefs[,"tauxCourt"],xlab="Date",ylab="Parametre",type="l",ylim=c(min(mat_coefs[1:3,]),max(mat_coefs[1:3,])))
lines(xLabels,mat_coefs[,"tauxLong"],col="red")
lines(xLabels,mat_coefs[,"convexite"],col="blue")
title("Historique des parametres avec EUSA")
legend("topleft", legend = c("Taux court","Taux long","Convexite"), col = c("black","red","blue"), pch = 15, bty = "n", pt.cex = 1, cex = 0.8, horiz = F, inset = c(0.1, 0.1))
#dev.off()


#### Observation : taux court > taux long parfois or normalement pas le cas : taux long > taux court car + risqué ??? A VERIFIER 

#-------------------------------------------------------------------------------


#### On a maintenant nos 3 séries qui sont ajustes on va pouvoir les stocker dans un dataframe pour les transformer ensuite en STM pour pouvoir appliquer rvfl comme pour les actions

df_txCourtLongConv = data[1:nrow(EUSA),1:4]

## On change le nom des colonnes
colnames(df_txCourtLongConv) <- c("DATE","tauxCourt","tauxLong","convexite")

## Affectation des valeurs dans chaque colonne
df_txCourtLongConv$tauxCourt = tauxCourtHist
df_txCourtLongConv$tauxLong = tauxLongHist
df_txCourtLongConv$convexite = convexiteHist

## Le dataframe est créé : Passons à la création de la STM

# Transformation en série temporelle (comme pour les actions) 
# Conversion de la colonne de dates en un objet de type 'xts'
txCourtLongConv_ST_mois <- xts(df_txCourtLongConv[, -1], order.by = as.Date(df_txCourtLongConv$DATE))
# txCourtLongConv_ST_mois


#-------------------------------------------------------------------------------

#### Extractions des composantes 

#recuperation des resultats avec la fonction NSf

facteursNs_Spot =  NMOF::NSf(lambda = fixeTau0,tm=maturites_txSpot)
mat_coefs_NS_Spot = matrix(NA,ncol=3,nrow = nrow(data_ST_mois))

for (i in 1:nrow(data_ST_mois)){
  mat_coefs_NS_Spot[i,] = .lm.fit(x = facteursNs_Spot, y = data_ST_mois[i,])$coefficients
}
# On a mu(1), mu(2) et mu(3)

df = data[1:nrow(data),1:4]
## On change le nom des colonnes
colnames(df) <- c("DATE","comp1","comp2","comp3")
df$comp1 <- mat_coefs_NS_Spot[,1]
df$comp2 <- mat_coefs_NS_Spot[,2]
df$comp3 <- mat_coefs_NS_Spot[,3]

ComposantesNS_ST_mois <- xts(df[, -1], order.by = as.Date(df$DATE))
names(ComposantesNS_ST_mois)

####on enregistre avant de perdre les ecrasement
saveRDS(txCourtLongConv_ST_mois, file = "txCourtLongConv_ST_mois.rds")
saveRDS(ComposantesNS_ST_mois, file = "ComposantesNS_ST_mois.rds")



