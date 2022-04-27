library(datasets)
library(forecast)
library(astsa)
#install.packages("devtools")
#devtools::install_github("FinYang/tsdl")
library(tsdl)
library(astsa)
library(dplyr)
library(forecast)
library(xts)

set.seed(444)


# Next we have CitiBike Data from ~2018 to ~2021.

```{r CitiBike ARIMA 1}

cb_rides <- read.csv(file = '/Users/mcmlxviii/Documents/NYCDS Academy/Capstone/CitiBike_Capstone_Project_Pitch/trips/time_series_2018_2021.csv')
head(cb_rides)
cb_rides['count.date.']
cb_ts = cb_rides %>% mutate(date=as.Date( paste(date, "01", sep="-"), format="%Y-%m-%d"))
class( cb_ts[1,1] )
head(cb_ts)

time_ser=ts(cb_ts, frequency=12, start=c(2018, 1))
time_ser=xts(cb_ts['count.date.'], cb_ts['date'], order.by=as.Date(cb_ts$date))
?xts
head(time_ser)
class(CitiBike)

plot.ts(time_ser)

CB.log = log(time_ser)

plot.ts(CB.log)

plot.ts(diff(CB.log))

Box.test(diff(CB.log), lag = log(length(diff(CB.log))))

CB.log.diff = diff(CB.log)

acf2(CB.log)
acf2(CB.log.diff)

#---------------------------------------------------

d=1
for(p in 1:3){
  for(q in 1:3){
    if(p+d+q<=6){
      model<-arima(x=CB.log, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

#bj.arima = arima(BJsales, order = c(0,2,1))
bj.arima = arima(BJsales, order = c(1,1,0))

bj.predict = forecast(bj.arima, h=12, level=80)

autoplot(bj.predict, main = "ARIMA(1,2,1) Prediction on CB Rides", ylab="Ride Count")

#--------------------------------------------


d = 1
DD = 1
per = 12
for(p in 1:2){
  for(q in 1:2){
    for(p_seasonal in 1:2){
      for(q_seasonal in 1:2){
        if(p+d+q+p_seasonal+DD+q_seasonal<=12){
          model<-arima(x=CB.log, order = c((p-1),d,(q-1)), seasonal = list(order=c((p_seasonal-1),DD,(q_seasonal-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,p_seasonal-1,DD,q_seasonal-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

CB.sarima = arima(x=CB.log, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = per))

CB.predict = forecast(CB.sarima, h=12, level = 80)

autoplot(CB.predict)
