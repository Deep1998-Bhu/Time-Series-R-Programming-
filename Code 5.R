library(TTR)

#Nottem dataset

data(nottem)
force(nottem)

# decompose TS in seasonal, trend and residual
nottem_stl<-stl(nottem,s.window = "periodic")

plot(nottem)

plot(nottem_stl)

nottem_stl$time.series[1:12,1]

data("AirPassengers")
# two different methods to decompose.
decompose_air<-decompose(AirPassengers,type="additive")
decompose_air$seasonal

stl_air<-stl(AirPassengers,s.window = "periodic")
stl_air$time.series[,1]

# SMA smoothing

plot(AirPassengers,type="l")

AirPassengers_ts<-ts(AirPassengers)
AirPassengers_ts_sma3<-SMA(AirPassengers_ts,n=3)
AirPassengers_ts
AirPassengers_ts_sma3
plot(AirPassengers_ts_sma3)
plot(AirPassengers_ts - AirPassengers_ts_sma3)

AirPassengers_ts_sma8<-SMA(AirPassengers_ts,n=8)
AirPassengers_ts
AirPassengers_ts_sma8
plot(AirPassengers_ts_sma8)
plot(AirPassengers_ts-AirPassengers_ts_sma8)


# EMA smoothing

ema3_AirPassengers<-EMA(AirPassengers, n=3)
ema8_AirPassengers<-EMA(AirPassengers, n=8)

plot.ts(ema3_AirPassengers)
plot.ts(ema8_AirPassengers)
plot.ts(EMA(AirPassengers, n=12))


# holts exponential smoothing : forecasting

AirPassengersF <- HoltWinters(AirPassengers, gamma=F)
AirPassengersF
AirPassengersF$SSE
plot(AirPassengersF)

library(forecast)
# Forecast into the future
AirPassengersF19 <- forecast(AirPassengersF, h=19)
plot(AirPassengersF19)

# Holt Winters exponential smoothing model: forecasting 
# to accommodate seasonality

hw_air<-HoltWinters(log(AirPassengers))
hw_air
hw_air$SSE   # measure fit
plot(hw_air)
library(forecast)
airF10<-forecast(hw_air, h= 10) # forecast 10 period ahead
plot(airF10)
airF10$mean # mean of the forecasted values
