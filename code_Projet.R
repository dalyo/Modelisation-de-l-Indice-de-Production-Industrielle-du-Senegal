################################################################################
######## Code Projet Modelilsation de l'indice de production au senegal  #######
################################################################################

## Importation des library n�cessaire a l'�tude
library(tseries)
library(forecast)
library(lmtest)
library(vars)
library("astsa")
library(urca)
library(forecast)

#################################################
############# Analyse descriptive ###############
#################################################
setwd("C:/Users/Sid Ousmane/Documents/texte/MASTER_SID_UADB/M1SID/Semestre 2/S�ries Temporelles/Master_SID_S�ries_Temp")

### Importatation de la base de donne�e
IPI<- read.csv("IPI.csv", sep=";", header=T)

## Transformation de la base en type s�rie temporelle
IPI <- ts(IPI[,2], start=c(2008,1), end=c(2017,12), frequency=12)
IPI

## Restriction de la serie � l'ann�e 2016
IPI_2016 <- window(IPI, end=c(2016,12))
IPI_2016

## Description de la base de donn�e
summary(IPI_2016)
sd(IPI_2016)

## Represantation de la s�rie
ts.plot(IPI_2016, xlab ="ann�e", ylab ="Indice de production Industrielle")


## Calcul de la Variance
var(IPI_2016)


## Reduction de la dispersion de la s�rie en la stabilisant la variance
IPI_2016_L <- log(IPI_2016)
ts.plot(IPI_2016_L,main ="S�rie IPI_2016", xlab ="ann�e", ylab ="Indice de production Industrielle")


## Centrage de la serie transform� logorithmiquement
IPI_2016_LC <- IPI_2016_L -mean(IPI_2016_L)
ts.plot(IPI_2016_LC,main ="S�rie IPI_2016 centr� a transformation logarithmique", xlab ="ann�e", ylab ="Indice de production Industrielle")
acf(IPI_2016_LC, xlab= "Retards")


#### TEST DE STATIONNARITE

# affichage de la tendance de la s�rie
tend =stl(IPI_2016_LC, "per")
ts.plot(IPI_2016_LC, main ="Representation de la tendance sur la s�rie", xlab ="ann�es")
lines(tend$time.series[,2], col = "red")

# Affichage de l'aCF de la serie
acf(IPI_2016_LC, xlab= "R�tards")

# Test augmenter de Dickey-Fuller
adf.test(IPI_2016_LC)

# Test de Phillips Perron 
PP.test(IPI_2016_LC)

# Test KPSS
kpss.test(IPI_2016_LC)


### Differenciation de la s�rie et reprise des tests de stationnaires

# Differenciation de la s�rie
IPI_2016_LCD <- diff(IPI_2016_LC)


# Test augmenter de Dickey-Fuller
adf.test(IPI_2016_LCD)

# Test de Phillips Perron 
PP.test(IPI_2016_LCD)

# Test KPSS
kpss.test(IPI_2016_LCD)

# confirmation visuelle
tendD =stl(IPI_2016_LCD, "per")
ts.plot(IPI_2016_LCD, main ="Representation de la s�rie diff�renci�e", xlab ="ann�es")
lines(tendD$time.series[,2], col = "red")

# Saisonnalit�
acf(IPI_2016_LCD, lag.max=36, xlab= "Retards")
IPI_2016_LCDS <- diff(IPI_2016_LCD, lag = 12)
ts.plot(IPI_2016_LCDS, xlab ="ann�e", main ="Serie desaisonnalis�e")



##### Identification du mod�le
acf(IPI_2016_LCDS, xlab ="Retards", lag.max =36)
pacf(IPI_2016_LCDS, xlab ="Retards", lag.max = 36)

# Estimation des param�tres

model1 = arima(IPI_2016, order = c(0,1,1), seasonal=list(order=c(0,1,1),period=12))
model2 = arima(IPI_2016, order = c(1,1,1), seasonal=list(order=c(0,1,1),period=12))
model3 = arima(IPI_2016, order = c(2,1,1), seasonal=list(order=c(0,1,1),period=12))
model4 = arima(IPI_2016, order = c(1,1,0), seasonal=list(order=c(0,1,1),period=12))
model5 = arima(IPI_2016, order = c(2,1,0), seasonal=list(order=c(0,1,1),period=12))

model6 = arima(IPI_2016, order = c(0,1,1), seasonal=list(order=c(1,1,1),period=12))
model7 = arima(IPI_2016, order = c(1,1,1), seasonal=list(order=c(1,1,1),period=12))
model8 = arima(IPI_2016, order = c(2,1,1), seasonal=list(order=c(1,1,1),period=12))
model9 = arima(IPI_2016, order = c(1,1,0), seasonal=list(order=c(1,1,1),period=12))
model10 = arima(IPI_2016, order = c(2,1,0), seasonal=list(order=c(1,1,1),period=12))

model11 = arima(IPI_2016, order = c(0,1,1), seasonal=list(order=c(2,1,1),period=12))
model12 = arima(IPI_2016, order = c(1,1,1), seasonal=list(order=c(2,1,1),period=12))
model13 = arima(IPI_2016, order = c(2,1,1), seasonal=list(order=c(2,1,1),period=12))
model14 = arima(IPI_2016, order = c(1,1,0), seasonal=list(order=c(2,1,1),period=12))
model15 = arima(IPI_2016, order = c(2,1,0), seasonal=list(order=c(2,1,1),period=12))

model16 = arima(IPI_2016, order = c(0,1,1), seasonal=list(order=c(1,1,0),period=12))
model17 = arima(IPI_2016, order = c(1,1,1), seasonal=list(order=c(1,1,0),period=12))
model18 = arima(IPI_2016, order = c(2,1,1), seasonal=list(order=c(1,1,0),period=12))
model19 = arima(IPI_2016, order = c(1,1,0), seasonal=list(order=c(1,1,0),period=12))
model20 = arima(IPI_2016, order = c(2,1,0), seasonal=list(order=c(1,1,0),period=12))

model21 = arima(IPI_2016, order = c(0,1,1), seasonal=list(order=c(2,1,0),period=12))
model22 = arima(IPI_2016, order = c(1,1,1), seasonal=list(order=c(2,1,0),period=12))
model23 = arima(IPI_2016, order = c(2,1,1), seasonal=list(order=c(2,1,0),period=12))
model24 = arima(IPI_2016, order = c(1,1,0), seasonal=list(order=c(2,1,0),period=12))
model25 = arima(IPI_2016, order = c(2,1,0), seasonal=list(order=c(2,1,0),period=12))



#Significativit� des param�tres des mod�les estim�es
coeftest(model1)
coeftest(model2)
coeftest(model3)
coeftest(model4)
coeftest(model5)
coeftest(model6)
coeftest(model7)
coeftest(model8)
coeftest(model9)
coeftest(model10)
coeftest(model11)
coeftest(model12)
coeftest(model13)
coeftest(model14)
coeftest(model15)
coeftest(model16)
coeftest(model17)
coeftest(model18)
coeftest(model19)
coeftest(model20)
coeftest(model21)
coeftest(model22)
coeftest(model23)
coeftest(model24)
coeftest(model25)


# Crit�re de selection selon l'AIC
model1$aic
model5$aic
model16$aic
model19$aic
model20$aic
model21$aic
model24$aic
model25$aic

##### Analyse des r�sidus

## Representation graphique des r�sidus
par(mfrow=c(1,2))
hist(model1$residuals, col="grey", freq=F, xlab="R�sidus", main = "Histogramme des residus")
qqnorm(model1$residuals) 
qqline(model1$residuals, col="red")

# Test du t de student
t.test(model1$residuals)

#Fonction d'autocorr�lation et Statistique de ljung-Box des r�sidus
tsdiag(model1)

# Test de normalit� des r�sidus
jarque.bera.test(model1$residuals)

# Homosc�dasticit� des r�sidus
white.test(model1$residuals)

# Test d'autocorr�lation
Box.test(model1$residuals, lag = 1, type = c("Ljung-Box"))



################################################################
##############       Pr�vison         ##########################
################################################################

# prevision pour 12mois
h <- 12
arima.prev12<-predict(model1, n.ahead=h)
arima.prev12
arima.inf12<-arima.prev12$pred-1.96*arima.prev12$se
arima.inf12
arima.sup12<-arima.prev12$pred+1.96*arima.prev12$se
arima.sup12
ts.plot(IPI, arima.prev12$pred, arima.inf12, arima.sup12, main="prevision � l'horizon h=12", col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2), xlab ="Ann�es")

# pr�visions pour 24 mois
h <- 24
arima.prev24<-predict(model1, n.ahead=h)
arima.prev24
arima.inf24<-arima.prev24$pred-1.96*arima.prev24$se
arima.inf24
arima.sup24 <-arima.prev24$pred+1.96*arima.prev24$se
arima.sup24
ts.plot(IPI, arima.prev24$pred, arima.inf24, arima.sup24, main="prevision � l'horizon h=24", col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(3,3,2,2), xlab ="Ann�es")


#################################################################
########## Modelisation multivari�e #############################
#################################################################


###IMPORTATION DES DONNEES
IPC<- read.csv("IPC.csv", sep=";", header=T)
IPC <- ts(IPC[,2], start=c(2008,1), end=c(2017,12), frequency=12)
IPC


#Restriction de la variable IPC a l'ann�e 2016
IPC_2016 <- window(IPC, end=c(2016,12))
IPC_2016


###STATISTIQUES DESCRIPTIVES de la variable IPC
summary(IPC_2016)
sd(IPC_2016)

### Representation de la s�rie
ts.plot(IPC_2016, xlab ="ann�es", ylab ="Indice de production du Consommateur", main ="Evolution de l'indice de production du consommateur")

## Etudes transformation preliminaires de la serie
IPC_2016_L <- log(IPC_2016)
IPC_2016_LC <- IPC_2016_L - mean(IPC_2016_L)
ts.plot(IPC_2016_LC, xlab ="ann�es", ylab ="Indice de production du Consommateur", main ="S�rie IPC_2016 centr�e a transformation logarithmique")

# affichage de la tendance de la s�rie
tend_ipc =stl(IPC_2016_LC, "per")
ts.plot(IPC_2016_LC, main ="Representation de la tendance sur la s�rie", xlab ="ann�es")
lines(tend_ipc$time.series[,2], col = "red")

 # Affichage de l'aCF de la serie
acf(IPC_2016_LC, xlab="Retards")

# Test augmenter de Dickey-Fuller
adf.test(IPC_2016_LC)

# Test de Phillips Perron 
PP.test(IPC_2016_LC)

# Test KPSS
kpss.test(IPC_2016_LC)

### Differenciation de la s�rie et reprise des tests de stationnaires

# Differenciation de la s�rie
IPC_2016_LCD <- diff(IPC_2016_LC)

# Test augmenter de Dickey-Fuller
adf.test(IPC_2016_LCD)

# Test de Phillips Perron 
PP.test(IPC_2016_LCD)

# Test KPSS
kpss.test(IPC_2016_LCD)

# confirmation visuelle
tend_ipc_D =stl(IPC_2016_LCD, "per")
ts.plot(IPC_2016_LCD, main ="Representation de la s�rie diff�renci�e", xlab ="ann�es")
lines(tend_ipc_D$time.series[,2], col = "red")

######### Modelisation Multivari�e

data_2016 <- cbind(IPI_2016, IPC_2016)

## Observation des deux s�ries simultanemement
plot(data_2016, xlab="Ann�es")

## Observation des deux s�ries apr�s stationnaris�es
varmat <- as.matrix(cbind(IPI_2016_LCD,IPC_2016_LCD))
plot(varmat, xlab="Ann�es", main ="S�ries stationnaires")

####### Identification du mod�le

## ## Cr�ation de la matrice
serie3S=matrix(ncol=2,nrow=length(IPI_2016_LCD))

## Dans cette matrice on met les donn�es stationnaris� de ipi et ipc (IPI_2016_LCD et IPC_2016_LCD)
serie3S[1:length(IPI_2016_LCD),1]=IPI_2016_LCD 
serie3S[1:length(IPI_2016_LCD),2]=IPC_2016_LCD
VARselect(serie3S)


## Estimation des param�tres du mod�le VAR
varfit <- VAR(varmat, p=3)
summary(varfit)
plot(varfit)

## Test de Stabilit� du mod�le
roots(varfit)
varfit.stable <- stability(varfit, type = "OLS-CUSUM")
plot(varfit.stable)

############ Analyse des r�sidus

norm1 <- normality.test(varfit)

## Representation graphique
par(mfrow=c(1,2))
hist(norm1$resid, col="grey", freq=F, main = "Histogramme des residus", xlab="Residus")
z=seq(min(norm1$resid), max(norm1$resid), 0.01)
lines(z, dnorm(z,mean(norm1$resid),sd(norm1$resid)), col="blue")
qqnorm(norm1$resid) 
qqline(norm1$resid, col ="red")

## Test de normalit�
# L'hypothese nulle est la normalit?
# Si p-value < 5%, on rejette H0.
norm1$jb.mul
# p-value = 0.7737 > 0.05 , on ne rejette pas H0.

############## Autocorrelation des erreurs
# Test de Portmanteu H0 = non autocorrelation.
serial.test(varfit, lags.pt = 30)
# p-value = 0,1002  > 0.05 donc on ne peut rejetter H0.

#homosc�dasticit� des r�sidus
# test d'arch H0 = non autocorrelation
arch.test(varfit)


#### Test de causalit�

IPCcauseIPI=matrix(ncol=2,nrow=107)

#Remplissage de la matrice IPCcauseIPI
IPCcauseIPI[1:107,1]=IPI_2016_LCD
IPCcauseIPI[1:107,2]=IPC_2016_LCD
IPCcauseIPI=ts(IPCcauseIPI, start=c(2008,1), freq=12,names=c("IPI_2016_LCD","IPC_2016_LCD"))
causality(VAR(y=IPCcauseIPI,p=3), cause="IPC_2016_LCD")


#Fonctions de r�ponses impulsionnelles
impulresp <- irf(varfit, reponse="IPC_2016_LCD", n.ahead = 20, boot = TRUE)
plot(impulresp)

#D�composition de la variance
vardecomp <- fevd(varfit)##$IPI_2016_LCD
plot(vardecomp)


# Prevision de 12 mois
prevision12 <- predict(varfit, n.ahead=12)
plot(prevision12)

# Prevision de 24 mois
prevision24 <- predict(varfit, n.ahead=24)
plot(prevision24)
