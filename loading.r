#Packages
library(tidyverse) #install.packages("tidyverse")
library(plyr) #install.packages("plyr") #install.packages("dplyr")
library(tidyverse)
library(hablar) #install.packages("hablar") #install.packages("tibble")

#Path

#Theo : 
Path_theo = '/Users/theoalegretti/Documents/GitHub/ENSAE_Time-Series-Project/Malt_Production.csv'


#Loading

df <- read.csv(Path_theo,sep=';', header =T)
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









