# Install and load required packages
install.packages("TSA", dependencies = TRUE)
library(TSA)
library(forecast)
library("quantmod")
library("tsDyn")
library("stats")
library(deSolve)
library(tseriesChaos)


# Set random seed for reproducibility
set.seed(123)

# PART 1: Simulated Data Example
# Simulate a Self-Exciting Threshold AR (SETAR) Process
n <- 500  # Number of observations
threshold <- 0  # Threshold value
phi1 <- 0.8  # AR coefficient for regime 1
phi2 <- -0.5  # AR coefficient for regime 2
sigma <- 1  # Standard deviation of noise
y <- numeric(n)

# Generate the SETAR process
for (t in 2:n) {
  if (y[t - 1] <= threshold) {
    y[t] <- phi1 * y[t - 1] + rnorm(1, mean = 0, sd = sigma)
  } else {
    y[t] <- phi2 * y[t - 1] + rnorm(1, mean = 0, sd = sigma)
  }
}

# Plot the simulated series
plot(y, type = "l", main = "Simulated SETAR Process", xlab = "Time", ylab = "y")
abline(h = threshold, col = "red", lty = 2)  # Add threshold line

# Fit a TAR model to the simulated data
tar_model_sim <- tar(y, p1 = 1, p2 = 1, d = 1, a = threshold)
summary(tar_model_sim)

# Plot residual diagnostics
checkresiduals(tar_model_sim)

# Forecast with TAR model
sim_forecast <- predict(tar_model_sim, n.ahead = 20)
plot(y, type = "l", main = "Simulated Data with TAR Model Forecast", xlab = "Time", ylab = "y")
lines((501):(520), sim_forecast$fit, col = "blue", type = "o")

# PART 2: Real Data Example (Nile River Data)
# Load the Nile dataset
data("Nile")
nile_flow <- as.numeric(Nile)

# Explore data
summary(nile_flow)
hist(nile_flow, breaks = 20, main = "Histogram of Nile Flow", xlab = "Flow")

#check for SETAR against linearity
setarTest(nile_flow, m=3, nboot=400)

selectSETAR(nile_flow, m=3, thDelay=1:2)

model <- setar(nile_flow, m=3, thDelay = 2, th=912)
summary(model)

# Forecast with SETAR model
nile_forecast <- predict(model, n.ahead = 10)
nile_forecast

#Another dataset - sunspots data
plot.ts(sunspot.year, xlab="Time", ylab="Yearly numbers of sunspots")

#Test for linearity
Keenan.test(sunspot.year)
Tsay.test(sunspot.year)

AICM = NULL
for (d in 1:9) {
  sunspot.tar.s = tar(sunspot.year, p1 = 9, p2 = 9, d = d, a=0.15, b=0.85)
  AICM = rbind(AICM, c(d, sunspot.tar.s$AIC, signif(sunspot.tar.s$thd,4), sunspot.tar.s$p1, sunspot.tar.s$p2))}
colnames(AICM) = c('d', 'nominal AIC', 'r', 'p1', 'p2')
rownames(AICM) = NULL
AICM

res = tlrt(sunspot.year, p=9, d=9, a=0.15, b=0.85)
res

sunspot.tar.best <- tar(sunspot.year, p1 = 9, p2 = 3, d = 9, a = 0.15, b = 0.85)
tsdiag(sunspot.tar.best)


sunspot.tar.pred <- predict(sunspot.tar.best, n.ahead = 10, n.sim=1000)
sunspot.pred = ts(c(sunspot.year, sunspot.tar.pred$fit), frequency=1, start=start(sunspot.year))
plot(sunspot.pred, type='n', ylim=range(c(sunspot.pred, sunspot.tar.pred$pred.interval)), ylab="Annual", xlab="Time")
lines(sunspot.year)
lines(window(sunspot.pred, start=end(sunspot.year) + c(0,1)), lty=2)
lines(ts(sunspot.tar.pred$pred.interval[2,], start=end(sunspot.year) + c(0,1), freq=1), lty=2)
lines(ts(sunspot.tar.pred$pred.interval[1,], start=end(sunspot.year) + c(0,1), freq=1), lty=2)

par(mfrow=c(1,1))

#fit linear AR model
#pacf(sunspot.year)
#try AR order 9
ord = 9
ar.mod <- arima(sunspot.year, order=c(ord,0,0), method="CSS-ML")
ar.fitted <- sunspot.year - ar.mod$res
sunspot.tar.fitted <- sunspot.year[(ord+1):length(sunspot.year)] - sunspot.tar.best$res

plot.ts(sunspot.year[10:289], xlab="Time",ylab="Yearly numbers of sunspots", main="Comparison of models")
lines(sunspot.tar.fitted, col="blue", lty=2)
lines(ar.fitted[10:289],col="red", lty=3)
legend('topleft', c("Actual","TAR","AR(9)"),lty=c(1,2,3), col=c("black","blue","red"))

#AR performance on simulated TAR models
set.seed(123)
#Lower regime parameters
i1 = 0.3
p1 = 0.5
s1 = 1

#Higher regime parameters
i2 = -0.2
p2 = -1.8
s2 = 1

thresh = -1
delay = 1

nobs = 200
#Simulate 200 obs of the above model
y=tar.sim(n=nobs,Phi1=c(i1,p1),Phi2=c(i2,p2),p=1,d=delay,sigma1=s1,thd=thresh,sigma2=s2)$y

#Use Tsay's test to deterimine order of best AR fit
ord <- Tsay.test(y)$order

#fit linear AR model
ar.mod <- arima(y, order=c(ord,0,0), method="CSS-ML")
ar.fitted <- y - ar.mod$res

plot.ts(y[(ord+1):nobs], xlab="Time",ylab="Value", main="Fitted AR")
lines(ar.fitted[(ord+1):nobs],col="red", lty=3)
legend('topleft', c("Simulated","AR(4)"),lty=c(1,3), col=c("black","red"))
abline(thresh,0,col="blue")
