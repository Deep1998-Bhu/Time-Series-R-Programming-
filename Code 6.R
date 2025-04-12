library(forecast)
library(fracdiff)
library(tseries)
library(urca)

# Generate or import data
# Here, we generate a sample ARFIMA process for demonstration
set.seed(123)  # For reproducibility
n <- 1000  # Number of observations
d <- 0.3  # Fractional differencing parameter
ar_coeffs <- c(0.5)  # AR coefficients
ma_coeffs <- c(0.2)  # MA coefficients

# Simulate ARFIMA model
arfima_sim <- fracdiff.sim(n, d = d, ar = ar_coeffs, ma = ma_coeffs)

# Plot the simulated time series
plot(arfima_sim$series, main = "Simulated ARFIMA Time Series", ylab = "Value", xlab = "Time")

# Estimate the ARFIMA model
arfima_model <- arfima(arfima_sim$series)

# Summary of the ARFIMA model
summary(arfima_model)

# Plot the ACF and PACF of the residuals
par(mfrow = c(1, 2))
Acf(residuals(arfima_model), main = "ACF of Residuals")
Pacf(residuals(arfima_model), main = "PACF of Residuals")

# Check for stationarity of the residuals using the KPSS test
kpss_test <- ur.kpss(residuals(arfima_model))
summary(kpss_test)

# Forecasting using the ARFIMA model
forecast_horizon <- 20  # Forecast horizon
arfima_forecast <- forecast(arfima_model, h = forecast_horizon)

# Plot the forecast
plot(arfima_forecast, main = "ARFIMA Model Forecast", ylab = "Forecasted Value", xlab = "Time")

# Save results to a CSV file
results <- data.frame(Time = 1:length(arfima_sim$series), 
                      Value = arfima_sim$series)
write.csv(results, "arfima_simulated_data.csv", row.names = FALSE)

# Print the model coefficients
cat("Estimated ARFIMA Model Coefficients:\n")
print(coef(arfima_model))





# Load real data (using the AirPassengers dataset)
data("AirPassengers")
y <- AirPassengers

# Plot the time series data
plot(y, main = "Monthly Airline Passengers", ylab = "Number of Passengers", xlab = "Year")

# Check for stationarity using the KPSS test
kpss_test <- ur.kpss(y)
summary(kpss_test)

# Differencing the series to make it stationary (if needed)
differenced_y <- diff(y)

# Check ACF and PACF of differenced series
par(mfrow = c(1, 2))
Acf(differenced_y, main = "ACF of Differenced Series")
Pacf(differenced_y, main = "PACF of Differenced Series")

# Estimate ARFIMA model
arfima_model <- arfima(y)

# Summary of the ARFIMA model
summary(arfima_model)

# Plot the ACF and PACF of the residuals
par(mfrow = c(1, 2))
Acf(residuals(arfima_model), main = "ACF of Residuals")
Pacf(residuals(arfima_model), main = "PACF of Residuals")

# Check for stationarity of the residuals using the KPSS test
kpss_test_residuals <- ur.kpss(residuals(arfima_model))
summary(kpss_test_residuals)

# Forecasting using the ARFIMA model
forecast_horizon <- 12  # Forecast for the next year
arfima_forecast <- forecast(arfima_model, h = forecast_horizon)

# Plot the forecast
plot(arfima_forecast, main = "ARFIMA Model Forecast", ylab = "Forecasted Passengers", xlab = "Year")

# Print the estimated coefficients
cat("Estimated ARFIMA Model Coefficients:\n")
print(coef(arfima_model))

