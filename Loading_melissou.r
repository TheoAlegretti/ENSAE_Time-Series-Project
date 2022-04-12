#Installation des packages
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("hablar")
#install.packages("tibble")
#install.packages("jsonlite")
#install.packages("plyr")
#install.packages("readxl")
#install.packages("rvest")

#Packages
library(dplyr) 
library(tidyverse)
library(hablar) 

#Path
path_melissa = '/Users/melissa/Desktop/Scolaire/ENSAE/2A/Semestre 2/Time Series/Projet/ENSAE_Time-Series-Project/Malt_Production.csv'
#Loading
df <- read.csv(path_melissa, sep=';', header =T)
names(df) <- c('Time','Y','Code')
date  <- df[,1]
Tmax = length(date)
df <- df[4:Tmax,1:2]
df <- arrange(df,Time)
df <- df %>% convert(int(Y))

summary(df$Y)

#tsY est un objet de traval pratique (plus leger et associe les dates :) ) 
tsY <-ts(df$Y,start = c(1990,1), end = c(2022,2), frequency = 12, names = c("Y") ) 


#on est bon => on peut plot pour voir la tête du machin 
plot(tsY,type="l", xlab='Années', ylab='Indice brut de production industrielle', main='Série temporelle initiale représentant la production de malt en France')
abline(h=mean(df$Y),col="red")

#Décomposition de la série initiale
plot(decompose(tsY), xlab='Années', main='Décomposition cycle-tendance-résidus de la série initiale') 

#Application d'une transformation logarithmique à la série initiale pour lisser les pics
logtsY <- log(tsY)
par(mfrow=c(2,1))
plot(logtsY,type="l", xlab='Années', ylab="Log de l'IPI")

#On trace l'ACF de la série
acf(logtsY, main="",50, xlab="Lag")








