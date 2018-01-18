##importing data

dataSan <- read.table(file = "clipboard", 
                      sep = "\t", header=TRUE)
dataSan
##test multiple regression linear model W/ test for AIC/ BIC



model1=lm(Price.Per.Sqft~CPI+Average.Age.on.Market+Unemployment_Rate+Population_growth+Mortgage.rate+HPI,data=dataSan)
summary(model1)
forecast(model1)
library("fpp")
res<-residuals(model1)
res
Acf(res)
dwtest(model1, alt="two.sided")
### test each variable to see best model fit


#######
B1=c()#CPI
B2=c()#AverageAgeonMarket 
B3=c()#Uneploymentrate
B4=c()#Population
B5=c()#Mortgage
B6=c()#HPI


for (i in 1:nrow(dataSan))
{
  data=dataSan[i:(9+i),]
  model = lm(Price.Per.Sqft~CPI+Average.Age.on.Market+Unemployment_Rate+Population_growth+Mortgage.rate+HPI, data = data)
  m = step(model,scale = 0,direction = "backward")
  B1[length(B1)+1] = m$coefficients["CPI"]
  B2[length(B2)+1] = m$coefficients["Average.Age.on.Market"]
  B3[length(B3)+1] = m$coefficients["Unemployment_Rate"]
  B4[length(B4)+1] = m$coefficients["Population_growth"]
  B5[length(B5)+1] = m$coefficients["Mortgage.rate"]
  B6[length(B6)+1] = m$coefficients["HPI"]
} 


B1[is.na(B1)] = 0
B2[is.na(B2)] = 0
B3[is.na(B3)] = 0
B4[is.na(B4)] = 0
B5[is.na(B5)] = 0
B6[is.na(B6)] = 0
x = 1:length(B1)

m
##### dynamically checks every 10 months by shifting over 1 month each time


plot(x=x,y=B1,xlab="Time",ylab="CPI")
lines(x=x,y=B1)
plot(x=x,y=B2,xlab="Time",ylab="Average.Age.on.Market") ##cyclicality
lines(x=x,y=B2)
plot(x=x,y=B3,xlab="Time",ylab="Unemployment_Rate") ##cyclicality
lines(x=x,y=B3)
plot(x=x,y=B4,xlab="Time",ylab="Population_growth") #cyclicality
lines(x=x,y=B4)
plot(x=x,y=B5,xlab="Time",ylab="mortgage_rate") #cyclilcality
lines(x=x,y=B5)
plot(x=x,y=B6,xlab="Time",ylab="HPI") #cyclicality
lines(x=x,y=B6)


##########


###check for trend and seasonality

dataSan=ts(dataSan,start=c(2012,1),end=c(2017,9),frequency = 12)
plot(dataSan[,2])
fit1=stl(dataSan[,2],t.window=10, s.window=5)
plot(fit1)

library(fpp)

plot(dataSan[,2])

###ARIMA test

library(fpp)
test1=adf.test(dataSan[,2])
test1
###have to do differences
dataSan2=diff(dataSan[,2])
test2=adf.test(dataSan2)
test2

###now it is differenced run Arima

tsdisplay(dataSan2, main="Acf and Pacf")
###autoarima
auto.arima(dataSan[,2])
##next step model 

Acf(residuals(m1), main="residuals of ARIMA")
Box.test(residuals(m1), lag=10, fitdf=4, type="Ljung")
Box.test(residuals(m1)^2, 10, type="Ljung")

#forecasting with ARIMA errors

ArimaModel=Arima(y=dataSan[,2],order=c(2,1,2),xreg=dataSan[,3:8])
ArimaModel
f=30
d=auto.arima(dataSan[,3])
CPI=as.vector(forecast(d,f)$mean)
d=auto.arima(dataSan[,4])
Average.Age.on.Market=as.vector(forecast(d,f)$mean)
d=auto.arima(dataSan[,5])
Unemployment_rate=as.vector(forecast(d,f)$mean)
d=auto.arima(dataSan[,6])
population_growth=as.vector(forecast(d,f)$mean)
d=auto.arima(dataSan[,7])
Mortgage_rate=as.vector(forecast(d,f)$mean)
d=auto.arima(dataSan[,8])
HPI=as.vector(forecast(d,f)$mean)
plot(forecast(ArimaModel,xreg=cbind(CPI,Average.Age.on.Market,Unemployment_rate,population_growth,Mortgage_rate,HPI),h=f),xlab="Time",ylab="Average Price per Square Feet in market",main="Forecat of Average Price per Square feet: San Francisco")
     