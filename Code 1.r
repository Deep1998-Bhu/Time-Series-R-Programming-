				 #--------------------#
				 # US Population Data #
				 #--------------------#

# Load the tseries package
library(tseries)

# Read the USPOP Data
uspop=read.table("uspop.txt")

# By default, header = FALSE. What happens when header = TRUE?
uspop <- read.table("uspop.txt",header=TRUE)

# We could also use the scan function
uspop <- scan("uspop.txt")

# Plot the Raw Data
plot(uspop/1000000,type="o", main="US Population from 1970-1990",	xaxt='n', xlab="Year", ylab="Population (in Millions)",  lwd=2)
axis(1, seq(1,21,2), c(1970, 1972, 1974, 1976, 1978, 1980, 1982, 1984, 1986, 1988, 1990))

# Save the graph as a pdf file
pdf(file="uspop.pdf")
plot(uspop/1000000,type="o", main="US Population from 1970-1990",	xaxt='n', xlab="Year", ylab="Population (in Millions)",  lwd=2)
axis(1, seq(1,21,2), c(1970, 1972, 1974, 1976, 1978, 1980, 1982, 1984, 1986, 1988, 1990))
dev.off()
# Or you can click on the graph and use the graphical interface and click on File -> Save as...

# Let's work toward a time series that looks more stationary:
# Take square root of the time series and plot
root.uspop <- sqrt(uspop/1000000)
plot(root.uspop,type="o", main="US Population from 1970-1990",	xaxt='n', xlab="Year", ylab="Population (in Millions)",  lwd=2)
axis(1, seq(1,21,2), c(1970, 1972, 1974, 1976, 1978, 1980, 1982, 1984, 1986, 1988, 1990))

# Calculate a linear trend
nt = length(root.uspop)
fit <- lm(root.uspop ~ as.numeric(1:nt))

# Add trend line to graph
abline(fit)

# Remove trend from time series and plot it
y <- root.uspop - fitted(fit)
ts.plot(y)

# Calculate the mean and variance of the time series
mean(y)
var(y)
sd(y)

# Add mean to plot
abline(h=mean(y),col="red")

# Substract mean from time series
y = y - mean(y)
ts.plot(y)

# Calculate and plot sample acf
myacf = acf(y,plot=FALSE)
acf(y)
myacf

#################
# Simulating data
#################

# Simulate Gaussian white noise, find the sample acf, and compare with theoretical acf
x <- rnorm(100, 0, 1)
acf(x)
pacf(x)

# Simulate from an AR(1) process, e.g. X_t = .8X_{t-1} + Z_t
x <- arima.sim(list(ar=0.8), n = 100, sd = 1)
acf(x)
pacf(x)
