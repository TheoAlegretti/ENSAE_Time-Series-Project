#Packages
library(tidyverse) #install.packages("tidyverse")
library(plyr) #install.packages("plyr") #install.packages("dplyr")
library(tidyverse)
library(hablar) #install.packages("hablar") #install.packages("tibble")
library(tseries) #install.packages("tseries")
library(forecast) #install.packages("forecast")
library(TSA) #install.packages("TSA")
library(FitARMA) #install.packages("FitARMA")
library(aTSA) #install.packages("aTSA")

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


#log(serie) => supprimer les valeurs extremes
ltsY <- log(tsY)

plot(ltsY,type="l")
abline(h=mean(log(df$Y),col="red"))

#les chocs semblent être stochastique (pas de changement structurelle de la serie tempo à posteri)
#besoin de les indiquer par des dummies ? previsible ? 

#Il ne semble pas avoir de trend apparante, la saisonnalité m'a pas l'air très apparente => on va utiliser un outil pour décomposer la série


#On regarde les ACF/PACF pour la saisonalité  

#serie brute : 

acf(tsY,lag.max = 200)
pacf(tsY,lag.max = 50)

#serie log : 

acf(ltsY,lag.max = 200)
pacf(ltsY,lag.max = 50)

#décomposition (additive (cst dans le temps de la forme : ) et multiplicative (évolution linéaire dans le temps : ))
decomposedadd <- decompose(tsY, type="additive")
decomposedaddl <- decompose(ltsY, type="additive")
plot(decomposedaddl)
plot(decomposedadd)

decomposedmu <- decompose(tsY, type="multiplicative") 
plot(decomposedmu)

#on peut faire un ols tq : 
fita <- tslm(tsY ~ trend + season)
summary(fita)


#On détrend + on identifie la saisonnalité 

#Sans Trend
dtY <- tsY-decomposedadd$trend 
plot (dtY) #on est centré en 0 ok 
fitdt <- tslm(dtY ~ season)
summary(fitdt)

temps <-c(1:length(tsY))
reg <- lm(tsY~temps)
summary(reg)
res <- residuals(reg)
trendl <-tsY-res
#plot de la trend
plot(trendl) 

#plot de la saisonalité en grand 
s <- decomposedadd$seasonal
plot(s[1:36],type='l')
abline(v=c(12,24),col="red",lwd=3, lty=2)
abline(h=1,lty=2,lwd=1,color='purple')
abline(v=c(2,14,26),col="green") #février pique bas 
abline(v=c(9,21,33),col="blue")


#sans seasonalité 
ssY <- tsY - decomposedadd$seasonal
plot(ssY,type="l")

#sans trend et seasonalité (semble stationnaire)
sdY <- tsY - decomposedadd$seasonal-decomposedadd$trend 
plot(sdY,type="l")
  

#meme chose en mode log 
sdYl <- ltsY - decomposedaddl$seasonal-decomposedaddl$trend 
plot(sdYl,type="l")


adf <- adf.test(sdY)
#stationnaire ! 



#on recheck les ACF/PACF 
sdYn <- na.omit(sdY)
acf(sdYn,lag.max = 100 )
pacf(sdYn,lag.max = 100)
 #log
sdYln<- na.omit(sdYl)
acf(sdYln,lag.max = 100)

pacf(sdYln,lag.max = 50)
#un lag en début d'année significatifs est encore la => AR(p) p>=3 ? 

#test d'un AR(6)
reg <- lm(sdYn ~ zlag(sdYn,1) + zlag(sdYn,2) +zlag(sdYn,3)+zlag(sdYn,4)+zlag(sdYn,5)+zlag(sdYn,6)+zlag(sdYn,7)+zlag(sdYn,8)+zlag(sdYn,9))
summary(reg)


#test ARMA(3,3) refaire avec un R moins pété que le mien 
fitarma2_2 <- FitARMA(sdYn,order=c(3,0,3), demean= TRUE, MeanMLEQ= FALSE,pApprox = 5, MaxLag=20)
summary(fitarma2_2, which="all")
coef(fitarma2_2)   
res <- fitarma2_2$res
acf(res)

#intégration à l'ordre 1 ? 






