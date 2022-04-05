library(TSA)
library(pastecs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(forecast)
library(readr)
library("readxl")
library(tidyverse)
library(urca)
library(tseries)
library(xts)
library(xtable)
library(tsDyn)
library(fGarch)
library(FitARMA)
library(fUnitRoots)
library(vars)
library(maps)





df <- read.csv("F:/data/data econofi/dataeconofi.csv", dec=".",sep=',', header = T)


names(df)[names(df) == "NASDAQCOM"] <- "Nasdaq"
names(df)[names(df) == "SFTPINDM114SFRBSF"] <- "Techpulse"


# Analyseeeeeeeeeeeee # 
#Nasdaq : on a 268 observation, allant de janvier 1992 à avril 2014 # 
# Objectif : analyser la série et prévoir les crises avec l'indice de Tech Pulse # 

date  <- df[,1]
Tmax = length(date)

#On va créer le log et la différence 1 er et en série temporel# 

lnNasdaq <- log(df$Nasdaq)
dlnNasdaq <- lnNasdaq -zlag(lnNasdaq,1)
ddlnNasdaq <- lnNasdaq -2*zlag(lnNasdaq,1)+zlag(lnNasdaq,2)

lnTechpulse <- log(df$Techpulse)
dlnTechpulse <- lnTechpulse -zlag(lnTechpulse,1)
ddlnTechpulse <- lnTechpulse -2*zlag(lnTechpulse,1)+zlag(lnTechpulse,2)

tsNasdaq <- ts(df$Nasdaq,start = c(1992,1), end = c(2014,4), frequency = 12, names = c("Nasdaq") ) 
tslnNasdaq <- ts(lnNasdaq,start = c(1992,1), end = c(2014,4), frequency =12 , names = c("lnNasdaq") ) 
tsdlnNasdaq <- ts(dlnNasdaq,start = c(1992,2), end =c(2014,4),frequency = 12, names= c('dlnNasdaq'))
tsddlnNasdaq <- ts(ddlnNasdaq,start=c(1992,3),end=c(2014,4),frequency = 12, names =c('ddlnNasdaq'))
# On check le graphique de base : première intuition pour voir si la série est stationnaire ou non 
#On fera un test ADF pour vérifier 

#Série brute
plot(tsNasdaq,type="l")
abline(h=mean(df$Nasdaq),col="red")
#série log 
plot(tslnNasdaq,type='l')
#I(1) du log 
plot(tsdlnNasdaq,type='l')
abline(h=0,col="red")
#I(2) du log : 
plot(tsddlnNasdaq,type='l')


# On regarde la normalité de la série : 
sk <- skewness(dlnNasdaq,na.rm=TRUE)
kurt <- kurtosis(dlnNasdaq,na.rm=TRUE)
# c'est pas fabuleux mais c'est normal d'oberver ça ! 

# On regarde les autocorrelogrammes de la I(0) pour voir le caractère autoregréssif de la série et non stationnaire par inférence:

#La série brute : 
acf(tsNasdaq)
pacf(tsNasdaq)
#On conclue que la série est largement corrélé à elle même, on ne peut donc pas l'étudier ainsi sans la corrigé de ces compossantes déterministes 
#On regarde ln(serie brute): 
acf(tslnNasdaq)
pacf(tslnNasdaq)
#C'est la même chose : nous allons travailler sur le rendement qui est la série logarithme intégré d'ordre 1. 
tsdlnNasdaq <- na.omit(tsdlnNasdaq)
ac <- acf(tsdlnNasdaq,lag.max = 100)
pac <- pacf(tsdlnNasdaq,lag.max = 100)

#On regarde la série intégré d'ordre 2 : 
tsddlnNasdaq <- na.omit(tsddlnNasdaq)
ac <- acf(tsddlnNasdaq,lag.max = 300)
pac <- pacf(tsddlnNasdaq,lag.max = 300)
# le travail à l'ordre 2 semble pas favoriser notre etude, nous allons donc garder l'intégration d'ordre 1 


#On voit qu'il y a une pluralité d'autocorrélations à l'ordre 1,3,4,5 ... 
# Nous avons bien une composante tendancielle ou saisonniaire que nous allons identifier : 

#La composante saisonnière étant pas définie intuitivement dans le graphique, on va regarder la décomposition additive et multiplicative 
decomposedadd <- decompose(tsdlnNasdaq, type="additive") 
plot(decomposedadd)

decomposedmul <- decompose(tsdlnNasdaq, type="multiplicative") 
plot(decomposedmul)

#La difference semble minime, on va garder la décomposition additive que l'on écrit 
fita <- tslm(tsdlnNasdaq ~ trend + season)
summary(fita)

# Il semble qu'il existe de la saisonnalité dans le dlnNasdaq : on utilisera la décomposition additive . 
#Il faudrait appliqué un filtre de moyennes mobiles à la série pour retirer la saionnalité : série CSV

#On détrend  et on identifie la saisonnalité : 
dtNasdaq <- tsdlnNasdaq-decomposedadd$trend 
plot (dtNasdaq)
fitdt <- tslm(dtNasdaq ~ season)
summary(fitdt)

temps <-c(1:length(dlnNasdaq))
reg <- lm(lnNasdaq~temps)
summary(reg)
res <- residuals(reg)
trendl <-dlnNasdaq-res
plot(trendl) 


#On retire la saisonnalité : 
ssdlnNasdaq <- tsdlnNasdaq - decomposedadd$seasonal
plot(ssdlnNasdaq,type="l")

fits <- tslm(ssdlnNasdaq ~ trend + season)
summary(fits)


# On va maintenant regresser I(1) de ln(Nasdaq) avec ses 6 premiers retards : 

reg <- lm(ssdlnNasdaq ~ zlag(ssdlnNasdaq,1) + zlag(ssdlnNasdaq,2) +zlag(ssdlnNasdaq,3)+zlag(ssdlnNasdaq,4)+zlag(ssdlnNasdaq,5)+zlag(ssdlnNasdaq,6)+zlag(ssdlnNasdaq,7)+zlag(ssdlnNasdaq,8)+zlag(ssdlnNasdaq,9)+zlag(ssdlnNasdaq,10)+zlag(ssdlnNasdaq,11)+zlag(ssdlnNasdaq,12)+zlag(ssdlnNasdaq,13)+zlag(ssdlnNasdaq,14)+zlag(ssdlnNasdaq,15)+zlag(ssdlnNasdaq,16)+zlag(ssdlnNasdaq,17))
summary(reg)



#Le lag 13 et 1  sont significatif à 5 % et le lag 9 à 1% , on va les garder et refaire la régression 
reg <- lm(ssdlnNasdaq ~ lag(ssdlnNasdaq,13) + lag(ssdlnNasdaq,9))
summary(reg)

#On retient le lag 13 et 1  : 
reg <- lm(ssdlnNasdaq ~ lag(ssdlnNasdaq,13) + lag(ssdlnNasdaq,1))
summary(reg)

# On check les résidus (homoscédasticité et normalité) : 
res <- reg$residuals
tsres <- ts(res,start=c(1994,2),end=c(2014,4),frequency = 12)
plot(tsres,type="l") #On voit que c'est pas mal répartie sauf en période de crise ou le modèle ne comprend rien 

acf(res) # Il y a des retards significatifs : autocorrélation des résidus : on va devoir les blanchirs avec un MA 


library(FitARMA)

fitarma2_2 <- FitARMA(ssdlnNasdaq,order=c(2,0,2), demean= TRUE, MeanMLEQ= FALSE,pApprox = 5, MaxLag=20)
summary(fitarma2_2, which="all")
coef(fitarma2_2)   

res22 <- fitarma2_2$res
plot(res22,type='l')
acf(res22)
pacf(res22)
LjungBoxTest(res22, k=0, lag.max=30, StartLag=1)

#Je suppose que l'autocorrélation des résidus est due à l'hétéroscédasticité, on voit que la crise de 2008 a changer structurellement le modèle 
#

plot(res22)

#TEST ADF : 
ddlnNasdaq <- na.omit(ddlnNasdaq)
dlnNasdaq <- na.omit(dlnNasdaq)

ADFTEST <- ur.df(lnNasdaq, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST)
#Pas stationnaire en I(0)

ADFTEST2 <- ur.df(dlnNasdaq, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST2)

#Stationnaire en I(1)

ADFTEST3 <- ur.df(ddlnNasdaq, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST3)

#Stationnaire en I(2)

#On test avec la seconde série : Techpulse 
ddlnTechpulse <- na.omit(ddlnTechpulse)
dlnTechpulse <- na.omit(dlnTechpulse)

ADFTEST <- ur.df(lnTechpulse, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST)

#Pas stationnaire en I(0)

ADFTEST2 <- ur.df(dlnTechpulse, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST2)

#Stationnaire en I(1)

ADFTEST3 <- ur.df(ddlnTechpulse, type =  "none", lags = 1, selectlags = "BIC" )
summary(ADFTEST3)

#Stationnaire en I(2)

# On regarde les autocorrelogrammes 
tsdlnTechPulse <- na.omit(tsdlnTechPulse)

ac <- acf(tsdlnTechPulse)
pac <- pacf(tsdlnTechPulse)


#Composante saisonnière 
decomposedadd <- decompose(tsdlnTechPulse, type="additive") 
plot(decomposedadd)

fit <- tslm(tsdlnTechPulse~ trend + season)
summary(fit)

#Plot de la composante stochastique
plot(as.ts(decomposedadd$random), main = "Graphique de la composante stochastique")

#Regression sur 10 retards
reg <- lm(dlnTechPulse ~ zlag(dlnTechPulse,1)+zlag(dlnTechPulse,2)+zlag(dlnTechPulse,3)+zlag(dlnTechPulse,4) + zlag(dlnTechPulse,5) + zlag(dlnTechPulse,6) + zlag(dlnTechPulse,7) + zlag(dlnTechPulse,8) + zlag(dlnTechPulse,9))
summary(reg)
res <- reg$residuals
acf(res)

reg <- lm(dlnTechPulse ~ zlag(dlnTechPulse,1)+zlag(dlnTechPulse,2)+zlag(dlnTechPulse,3)+zlag(dlnTechPulse,4))
summary(reg)
res <- reg$residuals
acf(res)


test <- auto.arima(dlnTechPulse)
summary(test)
res <- test$residuals

fitarma2_2 <- FitARMA(dlnTechPulse,order=c(2,1,2), demean= TRUE, MeanMLEQ= FALSE,pApprox = 5, MaxLag=20)
summary(fitarma2_2, which="all")
coef(fitarma2_2)   
res <- fitarma2_2$res
acf(res)


res22 <- fitarma2_2$res
plot(res22,type='l')
acf(res22)
pacf(res22)
plot(res22, main ="Résidus du modèle ARMA")
abline(h = 0,col="red")


#Modele GARCH : 


install.packages("rugarch")
install.packages("rmgarch")
library(rugarch)
library(rmgarch)

#Garch non spécifié : 
univ_garch = ugarchspec()
univ_garch

mod <- ugarchfit(spec = univ_garch, data = tsddlnTechPulse)
mod



res33 <- ts(mod@fit$residuals,start = c(1992,3), end = c(2014,4), frequency = 12, names = c("mod@fit$residuals"))
plot(res33, main = "Résidus corrigés", type ="p")
tsfitvar <- ts(mod@fit$var,start = c(1992,3), end = c(2014,4), frequency = 12, names = c("mod@fit$var") )

plot(tsfitvar, main = "Graphique des variances corrigées", type = "p")
abline(h = 0,col="red")

#On prend nos série en I(1). 
#Le modele VAR serai donc : 



data <- array(1,dim = c(267,2))
data[,1] <- dlnTechpulse
data[,2] <- dlnNasdaq
data <- ts(data = data, start = c(1994,2), end = c(2014,4), frequency = 12, names = c("TechPulse", "Nasdaq"))


#TEst de co-intégration : 
install.packages("aTSA")
library(aTSA)
coint.test(dlnNasdaq, dlnTechpulse, d = 0, nlag = NULL, output = TRUE)

#La co-intégration est rejetter sans trend : le modèle VAR est faisable ! 

#Modele VAR : 
VARselect(data, lag.max = 20)$selection
varendo <- data_frame(data)

fitVar <- VAR(varendo, type = c('both'), lag.max = 5, ic = c('AIC'))
summary(fitVar)
fitvar<- VAR(data, type = c("both"), lag.max =5,ic = c("HQ"))
summary(fitvar)
plot(fitvar)

residvar<-resid(fitvar)

plot(residvar)

ser <- serial.test(fitvar, lags.pt = 16, type = "PT.asymptotic")
ser$serial

norm1 <- normality.test(fitvar)
norm1$jb.mul

#Nos résidus ne suivent pas une loi normale

plot(fitvar, names = "dlNasdaq")

#Les résidus semblent ne pas être autocorrélé : pas besoin de les blanchirs 

plot(fitvar, names = "dlTechpulse")


#Prediction VAR : 

prediction <- predict(fitvar, n.ahead = 30, ci = 0.95)

plot(prediction,type="l")

#Fonction de réponse VAR : 
irfc <- irf(fitvar, impulse = ('Nasdaq'), response = ('TechPulse'), n.ahead = 6,
            ortho = FALSE, cumulative = TRUE, boot = TRUE, ci = 0.95,runs = 1000, seed = NULL)
plot(irfc)

#Modele VECM : 

vecm <- VECM(data, lag=4, r=1, include = "both", beta= NULL, estim="ML", LRinclude="const")
summary(vecm)
sjd.vecm <- ca.jo(data, ecdet="const", type="eigen", K=2, spec="longrun")
summary(sjd.vecm)

resivecm <- vecm$residuals

plot(resivecm)
LjungBoxTest(resivecm, k=0, lag.max=30, StartLag=1)

prediction <- predict(vecm, n.ahead = 2)

plot(prediction,type="l")


#Fonction de réponse impulsives : 

irflevel <- irf(vecm, impulse = ("Nasdaq"), n.ahead = 6,
                ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95,runs = 1000, seed = NULL)
plot(irflevel)
irflevel <- irf(vecm, impulse = ("TechPulse"), n.ahead = 6,
                ortho = FALSE, cumulative = FALSE, boot = TRUE, ci = 0.95,runs = 1000, seed = NULL)
plot(irflevel)

