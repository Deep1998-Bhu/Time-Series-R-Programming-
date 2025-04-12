# Install and load necessary packages
library(urca)
library(vars)

# Load data
data(Canada)
Canada_ts <- ts(Canada, start = c(1980, 1), frequency = 4)
plot(Canada_ts, main = "Canada Macroeconomic Data", col = 1:4)

# Augmented Dickey-Fuller test on each series
adf_test_e <- ur.df(Canada[,1], type = "drift", selectlags = "AIC")
summary(adf_test_e)

# Johansen cointegration test
#H0:no cointegration

jo_test <- ca.jo(Canada_ts, type = "trace", K = 2, ecdet = "const", spec = "transitory")
summary(jo_test)

#Suggests r>1 but <=2 which means one needs a linear combination of 
#2 series for stationarity

# Convert Johansen test result to VECM representation
vecm_model <- cajorls(jo_test, r = 1)  # r is the cointegration rank, typically derived from Johansen test results
summary(vecm_model)

# Forecasting with ECM
var_model <- vec2var(jo_test, r = 1)  # Convert to VAR representation if needed for forecasting
forecast <- predict(var_model, n.ahead = 10)
plot(forecast)

par(mar=c(1,1,1,1))

#Cointegration tests

# Example data
data(Canada)
y <- Canada[, "prod"]  # Example variable 1
x <- Canada[, "e"]     # Example variable 2

# 1. Engle-Granger Test
eg_test <- ca.po(cbind(y, x), type="Pz")  # Phillips-Ouliaris test, which is an extension of Engle-Granger
summary(eg_test)

#residuals are non-stationary. Series are not cointegrated

# 2. Johansen Test (for multivariate cases)
johansen_test <- ca.jo(cbind(y, x), type="trace", ecdet="const", K=2)  # Trace test with constant
summary(johansen_test)

#residuals are non-stationary. Series are not cointegrated as r = 0

#Granger causality

# Load necessary package
library(vars)

# Create a time series dataset
data(Canada)
canada_diff <- diff(Canada)  # Difference to achieve stationarity if needed

# Choose lag length
lag_selection <- VARselect(canada_diff, lag.max = 5, type = "const")
p <- lag_selection$selection["AIC(n)"]

# Estimate VAR model
var_model <- VAR(canada_diff, p = p, type = "const")

# Perform Granger causality test
causality(var_model, cause = "e")  # Test if "e" Granger-causes other variables


#Haugh-Pierce Test

# Load necessary libraries
library(forecast)

# Example data: Assuming `y` and `x` are two stationary time series
y <- ts(rnorm(100), frequency = 1)
x <- ts(rnorm(100), frequency = 1)

# Step 1: Fit ARIMA models to each series
model_y <- auto.arima(y)
model_x <- auto.arima(x)

# Step 2: Extract residuals
residuals_y <- residuals(model_y)
residuals_x <- residuals(model_x)

# Step 3: Calculate cross-correlation between residuals
ccf_residuals <- ccf(residuals_y, residuals_x, plot = FALSE)

# Step 4: Calculate the Haugh-Pierce test statistic
K <- 5  # Set the maximum lag
Q_statistic <- length(y) * sum(ccf_residuals$acf[2:(K + 1)]^2)

# Step 5: Compare with chi-square critical value
p_value <- 1 - pchisq(Q_statistic, df = K)

# Results
Q_statistic
p_value

#Fail to reject null. No causality between the series.

