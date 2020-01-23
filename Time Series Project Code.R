
library(dplyr)
library(forecast)
library(TSA)
library(tseries)

##########################################################################
#Read data, compute and divide train and test set
##########################################################################
df0 <- read.csv("/Users/ziranmin/Desktop/MA 585/MA585 Final Project/Right Data/000logan_daily_20071231_20181230.csv")
df1 <- ts(select(df0,TMAX))
df2 <- matrix(df1, ncol = 7,byrow = T)
df3 <- ts(round(rowMeans(df2),2))
train0 <- df3[1:520]
test <- df3[521:574]


##########################################################################
#First glance plot, decompse, ACF, PACF
##########################################################################
plot.ts(train0)
train1 <- ts(train0, frequency = 52)
plot.ts(train1)
plot(decompose(train1, type="additive"))
#plot(decompose(train1,type="multiplicative"))

par(mfrow=c(1,2))
acf(train1,lag.max = 500)
pacf(train1,lag.max = 220)

##########################################################################
#Unit Root test, whether need trend difference
##########################################################################
adf.test(train1)
kpss.test(train1,null="Trend")


##########################################################################
#Seasonal Differencing, try to determine p, q, P, Q from ACF and PACF 
##########################################################################

diff_train <- diff(train1,lag =52)

acf(diff_train,lag.max = 500)
pacf(diff_train,lag.max = 500)

acf(diff_train,lag.max = 100)
pacf(diff_train,lag.max = 100)
#(3,0,0) (3,1,0)52

dev.off()

##########################################################################
#Subset model method
##########################################################################


fit1 = armasubsets(y = diff_train, nar = 55,nma = 20, y.name ='diff_train', really.big=T)
plot(fit1)


fittry = armasubsets(y = diff_train, nar = 54, nma = 54, y.name ='diff_train', really.big=T)
plot(fittry)


##########################################################################
#Model Selection
##########################################################################





##########################################################################
#Auto Arima model method
##########################################################################
fit_auto = auto.arima(train1)
fit_auto
#ARIMA(0,0,1)(2,1,0)[52] 

fit_auto2 = auto.arima(diff_train)
fit_auto2
#ARIMA(1,0,2)(1,0,0)[52] with zero mean 

##########################################################################
#Model Diag
##########################################################################
#auto.arima
final_try_auto1 = Arima(train1 ,order=c(1,0,2), seasonal = list(order = c(1, 1, 0), period = 52)) 
tsdiag(final_try_auto1)
auto.arima(residuals(final_try_auto1))
### ARIMA(0,0,0)(0,0,1)[52] with zero mean AICc=3356.58  

final_try_auto2 = Arima(train1 ,order=c(0,0,1), seasonal = list(order = c(2, 1, 0), period = 52)) 
tsdiag(final_try_auto2) #bad
auto.arima(residuals(final_try_auto2))
### ARIMA(1,0,2)(0,0,2)[52] with non-zero mean AICc=3309.26 


##########################################################################
#list models
##########################################################################

final_try4 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(1, 1, 1), period = 52)) 
tsdiag(final_try4)
#### zero arima residual,AICc=3166.81 
auto.arima(residuals(final_try4))
qqnorm(residuals(final_try4)) 
qqline(residuals(final_try4))


final_try5 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(0, 1, 1), period = 52)) 
tsdiag(final_try5)
#### zero arima residual,AICc=3165.3   
auto.arima(residuals(final_try5))
qqnorm(residuals(final_try5)) 
qqline(residuals(final_try5))

##########################################################################
final_try1 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(1, 1, 1), period = 52)) 
tsdiag(final_try1)
auto.arima(residuals(final_try1))
#### ARIMA(0,0,0) with zero mean  AICc=3166.81 

final_try2 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(1, 1, 0), period = 52)) 
tsdiag(final_try2)
auto.arima(residuals(final_try2))
#ARIMA(0,0,0)(0,0,1)[52] with zero mean,AICc=3356.59

final_try3 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(0, 1, 1), period = 52)) 
tsdiag(final_try3)
auto.arima(residuals(final_try3))
#### ARIMA(0,0,0) with zero mean AICc=3165.3 

final_try4 = Arima(train1 ,order=c(1,0,0), seasonal = list(order = c(1, 1, 1), period = 52)) 
tsdiag(final_try4)
auto.arima(residuals(final_try4))
#### Error

final_try5 = Arima(train1, order=c(0,0,1), seasonal = list(order = c(1, 1, 1), period = 52)) 
tsdiag(final_try5) # bad
auto.arima(residuals(final_try5))
#### ARIMA(3,0,1) with zero mean AICc=3215.33 

final_try6 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(0, 1, 0), period = 52)) 
tsdiag(final_try6)
auto.arima(residuals(final_try6))
#### ARIMA(0,0,0)(2,0,0)[52] with non-zero mean AICc=3370.52

final_try7 = Arima(train1 ,order=c(1,0,0), seasonal = list(order = c(1, 1, 0), period = 52)) 
tsdiag(final_try7)
auto.arima(residuals(final_try7))
#### Error

final_try8 = Arima(train1 ,order=c(0,0,1), seasonal = list(order = c(1, 1, 0), period = 52)) 
tsdiag(final_try8) #bad
auto.arima(residuals(final_try8))
#### ARIMA(1,0,2)(0,0,1)[52] with zero mean AICc=3361.36

final_try9 = Arima(train1 ,order=c(1,0,0), seasonal = list(order = c(0, 1, 1), period = 52)) 
tsdiag(final_try9) #bad
auto.arima(residuals(final_try9))
#### ARIMA(0,0,0) with zero mean AICc=3252.56

final_try10 = Arima(train1 ,order=c(0,0,1), seasonal = list(order = c(0, 1, 1), period = 52)) 
tsdiag(final_try10) #bad
auto.arima(residuals(final_try10))
#### ARIMA(3,0,1) with zero mean  AICc=3229.29 

final_try11 = Arima(train1 ,order=c(0,0,0), seasonal = list(order = c(1, 1, 1), period = 52)) 
tsdiag(final_try11) #bad
auto.arima(residuals(final_try11))
#### ARIMA(1,0,1) with zero mean  AICc=3235.51

final_try12 = Arima(train1 ,order=c(1,0,0), seasonal = list(order = c(0, 1, 0), period = 52)) 
tsdiag(final_try12) #bad
auto.arima(residuals(final_try12))
#### ARIMA(3,0,1)(1,0,0)[52] with zero mean AICc=3412.11 

final_try13 = Arima(train1 ,order=c(0,0,1), seasonal = list(order = c(0, 1, 0), period = 52)) 
tsdiag(final_try13) #bad
auto.arima(residuals(final_try13))
#### ARIMA(1,0,1)(1,0,0)[52] with non-zero mean AICc=3418.78

final_try14 = Arima(train1 ,order=c(0,0,0), seasonal = list(order = c(1, 1, 0), period = 52)) 
tsdiag(final_try14) #bad
auto.arima(residuals(final_try14))
#### ARIMA(1,0,1)(0,0,1)[52] with zero mean AICc=3360.45

final_try15 = Arima(train1 ,order=c(0,0,0), seasonal = list(order = c(0, 1, 1), period = 52)) 
tsdiag(final_try15) #bad
auto.arima(residuals(final_try15))
#### ARIMA(1,0,1) with zero mean AICc=3240.98

##########################################################################

final_try16 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(2, 1, 2), period = 52)) 
tsdiag(final_try16) 
auto.arima(residuals(final_try16))
#### Error

final_try17 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(1, 1, 2), period = 52)) 
tsdiag(final_try17) 
auto.arima(residuals(final_try17))
#### ARIMA(0,0,0) with zero mean AICc=3165.75

final_try18 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(0, 1, 2), period = 52)) 
tsdiag(final_try18) 
auto.arima(residuals(final_try18))
#### ARIMA(0,0,0) with zero mean AICc=3167.16 

final_try19 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(2, 1, 1), period = 52)) 
tsdiag(final_try19) 
auto.arima(residuals(final_try19))
#### Error

final_try20 = Arima(train1 ,order=c(1,0,1), seasonal = list(order = c(2, 1, 0), period = 52)) 
tsdiag(final_try20) 
auto.arima(residuals(final_try20))
#### ARIMA(0,0,0) with zero mean 3313.19


##########################################################################
#Forcasting
##########################################################################


#HWfit = HoltWinters(train1,gamma=T) 
HWfit = HoltWinters(train1,seasonal = "multiplicative") 
HWfcast = forecast(HWfit, h=54)
HWerr = test-HWfcast$mean
HWmae = mean(abs(HWerr))
HWrmse = sqrt(mean(HWerr^2))
HWmape = mean(abs((HWerr*100)/test)) 
HWmae 
HWrmse 
HWmape 
plot(HWfcast)

arimafcast1=forecast(final_try17, h=54)
arimaerr1 = test- arimafcast1$mean
arimamae1 = mean(abs(arimaerr1))
arimarmse1 = sqrt(mean(arimaerr1^2))
arimamape1 = mean(abs((arimaerr1*100)/test)) 
arimamae1 
arimarmse1
arimamape1
plot(arimafcast1)

##########################################################################
ex =arima.sim(model=list(ar=c(0.4,rep(0,10),0.8,- 0.32),ma=c(0.7)),n=200)
exfit = Arima(ex,order=c(13,0,1),fixed=c(NA,NA,rep(0,9),rep(NA,4)))
exfit

