library(tseries)
library(forecast)

#simulating from a Gaussian white noise process

x<-rnorm(10,0,1)

hist(x)
plot(x,type='l')

abline(h=mean(x),col='red')
abline(h=0,col='blue')

#repeat above code with large n

#diagnostic checking

#monthly volume of commercial bank real-estate loans, in billions of dollars
bank_case <- as.ts(scan("bank_case.txt"))
bank_case

bank_case_d1 <- diff(bank_case)
adf.test(bank_case_d1)

bank_case_d2 <- diff(bank_case_d1)
adf.test(bank_case_d2)

#ARIMA fit
bank_fit <- arima(x = bank_case, order = c(0, 2, 1))
bank_fit

res<-bank_fit$residuals
plot(res,type='l')
abline(h=0)

#Normality check
hist(res)
shapiro.test(res)
jarque.bera.test(res)
qqnorm(res)
qqline(res,col='red')

sam<-rchisq(100,4)
qqnorm(sam)
qqline(sam)

#Serial autocorrelation check
Box.test(res, lag = 10, type="Box-Pierce")
Box.test(res, lag = 10, type="Ljung-Box")

#Heteroscedasticity check
acf(res)
pacf(res)
acf(res^2)
pacf(res^2)

white.test(res)

checkresiduals(bank_fit)



