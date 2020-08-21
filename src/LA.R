library(tseries)
library(forecast)

#bring in data and data cleaning
hpi <- read.csv(file = "C://Users//Public//LAHPI.csv")
income <- read.csv(file = "C://Users//Public//LAIncome.csv")
gdp <- read.csv(file = "C://Users//Public//LAGDP.csv")
ur <- read.csv(file = "C://Users//Public//LAUR.csv")
default <- read.csv(file = "C://Users//Public//Default.csv")

def= c()
for (i in 1:nrow(default)){
  for (x in 1:3){
    def[length(def)+1] = default$Default[i]
  }
}
inc=c()

for (i in 1:nrow(income)){
  for (x in 1:12){
    inc[length(inc)+1] = income$Income[i]
  }
}

gdp1= c()
for (i in 1:nrow(gdp)){
  for (x in 1:12){
    gdp1[length(gdp1)+1] = gdp$GDP[i]
  }
}

name = colnames(hpi)
main = data.frame(hpi,inc[1:(length(inc)-3)], gdp1[1:(length(gdp1)-3)], ur$UR, def)
colnames(main) = c(name,"Income","GDP","UR","Default")

#multiple regression and step function
lmodel = lm(HPI~Income+GDP+UR+TBill+Mort+Default,data = main)
summary(lmodel)

x = 1:152
B1 = c()#Income
B2 = c()#GDP
B3 = c()#UR
B4 = c()#TBill
B5 = c()#Mort
B6 = c()#Default

T1 = c()#Income
T2 = c()#GDP
T3 = c()#UR
T4 = c()#TBill
T5 = c()#Mort
T6 = c()#Default

for (i in 1:(nrow(main)-49))
{
  data=main[i:(49+i),]
  model = lm(HPI~Income+GDP+UR+TBill+Mort+Default,data = data)
  m = step(model,scale = 0,direction = "backward")
  B1[length(B1)+1] = m$coefficients["Income"]
  B2[length(B2)+1] = m$coefficients["GDP"]
  B3[length(B3)+1] = m$coefficients["UR"]
  B4[length(B4)+1] = m$coefficients["TBill"]
  B5[length(B5)+1] = m$coefficients["Mort"]
  B6[length(B6)+1] = m$coefficients["Default"]
  xx = summary(m)
  TSTAT = xx$coefficients[,3]
  T1[length(T1)+1] = TSTAT["Income"]
  T2[length(T2)+1] = TSTAT["GDP"]
  T3[length(T3)+1] = TSTAT["UR"]
  T4[length(T4)+1] = TSTAT["TBill"]
  T5[length(T5)+1] = TSTAT["Mort"]
  T6[length(T6)+1] = TSTAT["Default"]
} 


B1[is.na(B1)] = 0
B2[is.na(B2)] = 0
B3[is.na(B3)] = 0
B4[is.na(B4)] = 0
B5[is.na(B5)] = 0
B6[is.na(B6)] = 0

T1[is.na(T1)] = 0
T2[is.na(T2)] = 0
T3[is.na(T3)] = 0
T4[is.na(T4)] = 0
T5[is.na(T5)] = 0
T6[is.na(T6)] = 0

plot(x=x,y=B1,xlab = "period",ylab = "Income/Capita coeff")
lines(x=x,y=B1)

plot(x=x,y=T1,xlab = "period",ylab = "Income/Capita t-stat")
lines(x=x,y=T1)

plot(x=x,y=B2,xlab = "period",ylab = "GDP/Capita coeff")
lines(x=x,y=B2)

plot(x=x,y=T2,xlab = "period",ylab = "GDP/Capita t-stat")
lines(x=x,y=T2)

plot(x=x,y=B3,xlab = "period",ylab = "UnemploymentRate coeff")
lines(x=x,y=B3)

plot(x=x,y=T3,xlab = "period",ylab = "UnemploymentRate t-stat")
lines(x=x,y=T3)

plot(x=x,y=B4,xlab = "period",ylab = "TBillRate coeff")
lines(x=x,y=B4)

plot(x=x,y=T4,xlab = "period",ylab = "TBillRate t-stat")
lines(x=x,y=T4)

plot(x=x,y=B5,xlab = "period",ylab = "MortgageRate coeff")
lines(x=x,y=B5)

plot(x=x,y=T5,xlab = "period",ylab = "MortgageRate t-stat")
lines(x=x,y=T5)

plot(x=x,y=B6,xlab = "period",ylab = "DefaultRate coeff")
lines(x=x,y=B6)

plot(x=x,y=T6,xlab = "period",ylab = "DefaultRate t-stat")
lines(x=x,y=T6)


#convert to time series and plot series
main = ts(main, start = c(2001,1), end = c(2017,9), frequency = 12)
HPI = main[,2]
plot(HPI)

#test level, trend, and seasonality
full = stl(main[,2],s.window = 5,t.window = 5)
#we chose to use s.window = 5 and t.window = 5 as they
#were most optimal
full
plot(full)

exponential = HoltWinters(main[,2])
exponential


#Dynamic Regression/ARIMA with Errors
acf(residuals(lmodel))
pacf(residuals(lmodel))
adf.test(residuals(lmodel))
diffres = diff(residuals(lmodel))
adf.test(diffres)
acf(diffres)
pacf(diffres)
Box.test(diffres)
dynamic = Arima(y = main[,2],order = c(5,1,0),xreg = main[,3:8])
h = 50 

m = auto.arima(main[,3])
Income = as.vector(forecast(m,h)$mean)
m = auto.arima(main[,4])
GDP = as.vector(forecast(m,h)$mean)
m= auto.arima(main[,5])
UR = as.vector(forecast(m,h)$mean)
m = auto.arima(main[,6])
TBill = as.vector(forecast(m,h)$mean)
m = auto.arima(main[,7])
Mort = as.vector(forecast(m,h)$mean)
m = auto.arima(main[,8])
Default = as.vector(forecast(m,h)$mean)

fcast=forecast(dynamic,xreg = cbind(Income,GDP,UR,TBill,Mort,Default),h = h)
plot(fcast)
accuracy(fcast)

