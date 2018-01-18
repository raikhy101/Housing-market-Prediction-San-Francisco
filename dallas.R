##importing data

dataDallas <- read.table(file = "clipboard", 
                         sep = "\t", header=TRUE)
dataDallas
##test multiple regression linear model W/ test for AIC/ BIC



model1=lm(Price.Per.Sqft~CPI+Average.Age.on.Market+Unemployment_Rate+Population_growth+Mortgage.rate+HPI,data=dataDallas)
summary(model1)
### test each variable to see best model fit


###check for trend and seasonality

x1=c()#CPI
x2=c()#AverageAgeonMarket
x3=c()#Uneploymentrate
x4=c()#Population
x5=c()#Mortgage
x6=c()#HPI


for (i in 1:nrow(dataDallas))
{
  dataD=dataDallas[i:(9+i),]
  model = lm(Price.Per.Sqft~CPI+Average.Age.on.Market+Unemployment_Rate+Population_growth+Mortgage.rate+HPI, data = dataD)
  m = step(model,scale = 0,direction = "backward")
  x1[length(x1)+1] = m$coefficients["CPI"]
  x2[length(x2)+1] = m$coefficients["Average.Age.on.Market"]
  x3[length(x3)+1] = m$coefficients["Unemployment_Rate"]
  x4[length(x4)+1] = m$coefficients["Population_growth"]
  x5[length(x5)+1] = m$coefficients["Mortgage.rate"]
  x6[length(x6)+1] = m$coefficients["HPI"]
} 


x1[is.na(x1)] = 0
x2[is.na(x2)] = 0
x3[is.na(x3)] = 0
x4[is.na(x4)] = 0
x5[is.na(x5)] = 0
x6[is.na(x6)] = 0
x = 1:length(x1)

##### dynamically checks every 10 months by shifting over 1 month each time


plot(x=x,y=x1,xlab="Time",ylab="CPI")
lines(x=x,y=x1)
plot(x=x,y=x2,xlab="Time",ylab="Average.Age.on.Market") ##cyclicality
lines(x=x,y=x2)
plot(x=x,y=x3,xlab="Time",ylab="Unemployment_Rate") ##cyclicality
lines(x=x,y=x3)
plot(x=x,y=x4,xlab="Time",ylab="Population_growth") #cyclicality
lines(x=x,y=x4)
plot(x=x,y=x5,xlab="Time",ylab="mortgage_rate") #cyclilcality
lines(x=x,y=x5)
plot(x=x,y=x6,xlab="Time",ylab="HPI") #cyclicality
lines(x=x,y=x6)

res<-residuals(model1)
res
Acf(res)
dwtest(model1, alt="two.sided")



dataDallas1=ts(dataDallas,start=c(2012,1),end=c(2017,9),frequency = 12)
plot(dataDallas1[,2])
fit1=stl(dataDallas1[,2],t.window=10, s.window=5)
plot(fit1)


###ARIMA test
auto.arima(dataDallas1[,2])
library(fpp)
test1=adf.test(dataDallas1[,2])
test1
###have to do differences
dataDallas2=diff(dataDallas1[,2])
test2=adf.test(dataDallas2)
test2
###now it is differenced run Arima

tsdisplay(dataDallas2)
###autoarima
##next step model 

m1=arima(dataDallas1[,2],order=c(1,1,0))
m1
Acf(residuals(m1))
Box.test(residuals(m1), lag=10, fitdf=4, type="Ljung")


###Dynamic Regression model
ArimaModel=Arima(y=dataDallas[,2],order=c(1,1,0),xreg=dataDallas[,3:8])
ArimaModel
f=30
d=auto.arima(dataDallas[,3])
CPI=as.vector(forecast(d,f)$mean)
d=auto.arima(dataDallas[,4])
Average.Age.on.Market=as.vector(forecast(d,f)$mean)
d=auto.arima(dataDallas[,5])
Unemployment_rate=as.vector(forecast(d,f)$mean)
d=auto.arima(dataDallas[,6])
population_growth=as.vector(forecast(d,f)$mean)
d=auto.arima(dataDallas[,7])
Mortgage_rate=as.vector(forecast(d,f)$mean)
d=auto.arima(dataDallas[,8])
HPI=as.vector(forecast(d,f)$mean)
plot(forecast(ArimaModel,xreg=cbind(CPI,Average.Age.on.Market,Unemployment_rate,population_growth,Mortgage_rate,HPI),h=f),xlab="Time",ylab="Average Price per Square Feet in market",main="Forecat of Average Price per Square feet: Dallas")

