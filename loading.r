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
library(LSTS) #install.packages('LSTS')
library(astsa) #install.packages('astsa')
library(fUnitRoots)#install.packages('fUnitRoots')
library(timeDate)
library(mvmeta) #install.packages('mvmeta')
#library(zoo) #install.packages('zoo')
library(fpp2) #install.packages('fpp2')
library(equatiomatic) #install.packages('equatiomatic')
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
#Chocs ? 2018 => Coupe du monde et forte chaleur 
#2020 => crise covid (confinement et fermeture des bars)

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
abline(v=c(9,21,33),col="blue") #et septembre (fin des vacances (bar saisonnier ... ))


#sans seasonalité 
ssY <- tsY - decomposedadd$seasonal
plot(ssY,type="l")

#sans trend et seasonalité (semble stationnaire)
sdY <- tsY - decomposedadd$seasonal-decomposedadd$trend 
plot(sdY,type="l")
  

#meme chose en mode log 
sdYl <- ltsY - decomposedaddl$seasonal-decomposedaddl$trend 
plot(sdYl,type="l")


adf <- adf.test(sdY,nlag = 30)

pp.test(sdY, type = "Z_tau", lag.short = TRUE, output = TRUE)

kpss.test(sdY)

#Les trois tests confirment que notre série n'est pas une marche aléatoire (avec/sans dérive/trend)
#On peut donc modéliser la série telle qu'elle sans intégrer la série à l'ordre 1/2 


#test avec I(1) => delta-Y_t = Y_t - Y_t-1
dsdYl <- sdYl -zlag(sdYl,1)

plot(dsdYl,type ='l')
adf <- adf.test(dsdYl,nlag = NULL)

dsdYn <- na.omit(dsdYl)
acf(dsdYn,lag.max = 100 )
pacf(dsdYn,lag.max = 100)

#Inutile (serie déjà stationnaire à l'ordre 0, ajouter l'ordre 1 nous ferai perdre une observation. 

#on recheck les ACF/PACF 
sdYn <- na.omit(sdY)
acf(sdYn,lag.max = 100 )
pacf(sdYn,lag.max = 100)


#PACF nous présente un MA(6) et ACF un AR(4) (les chocs lointain sont des crises (2020 / 2015) => introduction de dummies ? )
#Idée => test de multiples modèles en utilisant le BIC comme critères d'information (le plus réstrictif)
#on construit l'ensemble des modèles possible (6 modèles MA) (4 modèles AR) (ARMA(4,6)=>ARMA(1,1) 24 modèles) Total model = 34 

#modelisation : ARIMA (I = 0) 

i <- 0
fit1 <- Arima(sdY, order=c(0,i,6))
fit2 <- Arima(sdY, order=c(0,i,5))
fit3 <- Arima(sdY, order=c(0,i,4))
fit4 <- Arima(sdY, order=c(0,i,3))
fit5 <- Arima(sdY, order=c(0,i,2))
fit6 <- Arima(sdY, order=c(0,i,1))
fit7 <- Arima(sdY, order=c(1,i,6))
fit8 <- Arima(sdY, order=c(1,i,5))
fit9 <- Arima(sdY, order=c(1,i,4))
fit10 <- Arima(sdY, order=c(1,i,3))
fit11 <- Arima(sdY, order=c(1,i,2))
fit12 <- Arima(sdY, order=c(1,i,1))
fit13 <- Arima(sdY, order=c(2,i,6))
fit14 <- Arima(sdY, order=c(2,i,5))
fit15 <- Arima(sdY, order=c(2,i,4))
fit16 <- Arima(sdY, order=c(2,i,3))
fit17 <- Arima(sdY, order=c(2,i,2))
fit18 <- Arima(sdY, order=c(2,i,1))
fit19 <- Arima(sdY, order=c(3,i,6))
fit20 <- Arima(sdY, order=c(3,i,5))
fit21 <- Arima(sdY, order=c(3,i,4))
fit22 <- Arima(sdY, order=c(3,i,3))
fit23 <- Arima(sdY, order=c(3,i,2))
fit24 <- Arima(sdY, order=c(3,i,1))
fit25 <- Arima(sdY, order=c(4,i,6))
fit26 <- Arima(sdY, order=c(4,i,5))
fit27 <- Arima(sdY, order=c(4,i,4))
fit28 <- Arima(sdY, order=c(4,i,3))
fit29 <- Arima(sdY, order=c(4,i,2))
fit30 <- Arima(sdY, order=c(4,i,1))
fit31 <- Arima(sdY, order=c(4,i,0))
fit32 <- Arima(sdY, order=c(3,i,0))
fit33 <- Arima(sdY, order=c(2,i,0))
fit34 <- Arima(sdY, order=c(1,i,0))

(BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16,fit17,fit18,fit19,fit20,fit21,fit22,fit23,fit24,fit25,fit26,fit27,fit28,fit29,fit30,fit31,fit32,fit33,fit34))
(AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13,fit14,fit15,fit16,fit17,fit18,fit19,fit20,fit21,fit22,fit23,fit24,fit25,fit26,fit27,fit28,fit29,fit30,fit31,fit32,fit33,fit34))


#i = 0 
# fit 24 (ARIMA(3,0,1)) et 28 (ARIMA(4,0,3) ont le BIC les plus faibles
#fit 22 (ARIMA(3,0,3)) / 29 (ARIMA(4,0,2)) / 28 (ARIMA(4,0,3)) choisit par l'AIC  

#regardons les résidus et les test de Ljung-Box pour la blancheur des résidus
#H0 = résidus indépendant dans le temps (pas d'autocorrélation) => White noise 
#H1 = résidus dépendant => white noise rejeté 
#On a besoin d'une P-value > 5 % pour "valider" notre modèle 

checkresiduals(fit24) #pvalue à 1 % => white noise pas ok à 5 % 
checkresiduals(fit28) #pvalue à 8 % => white noise ok à 5 % 
checkresiduals(fit22) #pvalue à 7 % => white noise  ok à 5 % 
checkresiduals(fit29) #pvalue à 11 % => white noise ok à 5 % 

#Le modèle qui semble le plus optimal est le fit28 => ARIMA(4,0,3)
#on v


#on peut check la significativité des coefficients avec un risque de 1 er espece de 5 % (t-stat )
signif <- function(estim){
  coef <- estim$coef
  se <- sqrt(diag(estim$var.coef))
  t <- coef/se
  pval <- (1-pnorm(abs(t)))*2
  return(rbind(coef,se,pval))
}
signif(fit28) # on voit 3 coefficients = 0 à 5 % 
signif(fit22) # ici, simplement l'intercept =0 (normal on a détrender la série)
signif(fit29) #ici le 1 er coefficient du MA(1) = 0  #pas utilisable dans le modèle SARIMA (modele pas définit)


#nous avons les ordres maximums p,q => on réintroduit la saisonnalité
#on prend le modèle le plus large et on test les combinaisons possible, on regardera la parcimonie du modèle (significativité)

#on test donc SARIMA(p,0,q,2,0,2)[12] avec p = 0,..,4 et q = 0,..,3 soit 12 modèles
models <- list()
AIC <- list()
BIC <- list()

for (p in 0:4){
  for (q in 0:3){
    model <- paste('AR',(as.character(p)),'_','MA',as.character(q))
    fit <- sarima(tsY,p,0,q,2,0,1,12)
    bic <- fit$BIC
    aic <- fit$AIC
    models <- append(models,model)
    BIC <- append(BIC,bic)
    AIC <- append(AIC,aic)
  }}

paste((models),(AIC),(BIC))

#sarma (1,0,1)(2,0,1)[12] meilleur AIC / BIC (mais autocorrélation des résidus)

(fit <- sarima(tsY,1,0,1,2,0,1,12))

#probleme de sélection des variables avec le modèle :=> modele SARIMA(3,0,2,2,0,1,12) donne des résidus indépendants + coefficients  significatifs 

#le modèle le plus parcimonieux avec comme résidus un bruit blanc est un SARMA(3,2)(2,0,1)*12

(fit <- sarima(tsY,3,0,2,2,0,1,12)) # <=> (fit23) => signif(fit23)


#juste les probabilité des quantiles de la loi normale ont du mal sur les queues de distribution => on l'explique facilement avec les outliers de crises 

#on lance le forecasting : 
forecasting <- sarima.for(tsY,24,3,0,2,2,0,1,12, gg=TRUE, main='Forescating sur 1 ans et demi') 




#augmenter le modèle pour prédire plus rapidement l'information ? 

#utiliser une série co-intégré et exploiter cette co-intégration pour prédire la fabrication de Malt. 
#exemple de série co-intégré ? => consommation de bière en france 


