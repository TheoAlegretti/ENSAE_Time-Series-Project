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
plot(tsY,type="l")
abline(h=mean(df$Y),col="red")









