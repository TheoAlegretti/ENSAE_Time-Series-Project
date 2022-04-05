df <- read.csv('/Users/theoalegretti/Desktop/Cours /ENSAE/S2/time series /TD/Donnees1.csv')



library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(zoo)

plot(df$XM ,type="l")


df <- df[,-4]

#Trend + seasonnality 

series <- df

#############################################################################
tsvar <- ts(df,frequency = 12)

#Additive ? 
decomposedadd <- decompose(tsvar, type="additive") 
plot(decomposedadd)

#Detrend 
var <- tsvar - decomposedadd$trend
plot(var,type='l')

#deseasonnality 
var <- var - decomposedadd$seasonal
plot(var,type='l')
#############################################################################

#Autre facon de déseasonnalité 
lagged <- lag(series,12)
desaison <- series - lagged
plot(desaison,type='l')

#Ici, on considère que la serie n'est pas stationnaire du fait de la saisonnalité

#diff ordinaire <=> Intégration d'ordre 1 => retire la trend et le reste du caractère déterministe
dserie  <- serie  -lag(serie ,1)

plot(dserie)

acf(dserie)
pacf(dserie)

#On va appliquer un SARIMA 
#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################

#Correction TD 

#On regarde les autocorrélation de la series => reperer la frequences de la saisonnalité 

acf(desaison,lag.max = 100)
result <- acf(series,lag.max = 100)

#on extrait les lag les plus significatifs (selon un certain seuil) voir correction 

#desaison était bon 

plot(serie)
length(serie)


#C'est bien une répétition au lag 12 , on va appliquer un filtre de saison 12 





