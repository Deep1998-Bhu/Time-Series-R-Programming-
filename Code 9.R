# Spectral Analysis in R

# 1. Generate Time Series Data
# Let's create a synthetic time series with two sinusoidal components and noise
T <- 20  # Length of the time series
f1 <- 0.1  # Frequency of first sinusoidal component
f2 <- 0.3  # Frequency of second sinusoidal component

# Time points
t <- 0:(T-1)

# Generate signal
signal <- sin(2 * pi * f1 * t) + 0.5 * sin(2 * pi * f2 * t)

# Add random noise
set.seed(42)  # For reproducibility
noise <- 0.2 * rnorm(T)
time_series <- signal + noise

# Plot the time series
plot(t, time_series, type="o", col="blue", main="Synthetic Time Series", 
     xlab="Time", ylab="Value")

# 2. Compute Periodogram
# Use the Fast Fourier Transform (FFT) to compute the periodogram
fft_values <- fft(time_series)
frequencies <- (0:(T/2)) / T  # Frequencies of interest (up to Nyquist frequency)
power <- Mod(fft_values[1:(T/2 + 1)])^2 / T  # Periodogram values

# Plot the periodogram
plot(frequencies, power, type="h", col="red", main="Periodogram of the Time Series", 
     xlab="Frequency (Hz)", ylab="Power")

# Highlight dominant frequencies
abline(v=f1, col="blue", lty=2)
abline(v=f2, col="green", lty=2)
legend("topright", legend=c("f1 = 0.1 Hz", "f2 = 0.3 Hz"), 
       col=c("blue", "green"), lty=2, bty="n")

# 3. Smoothed Periodogram (Optional)
# To reduce noise in the periodogram, apply a smoothing technique
library(stats)  # Ensure the 'stats' package is loaded
smoothed_power <- filter(power, rep(1/3, 3), sides=2)

# Plot the smoothed periodogram
plot(frequencies, smoothed_power, type="l", col="purple", main="Smoothed Periodogram", 
     xlab="Frequency (Hz)", ylab="Smoothed Power")

# 4. Welch's Method (Segmented Periodogram)
# Divide the time series into overlapping segments and average periodograms
# Install and load the bspec package for Welch's method
install.packages("bspec")
library(bspec)

# Apply Welch's PSD estimation
psd_welch <- welchPSD(ts(time_series), seglength=10)

# Extract frequencies and power from Welch's method
welch_freq <- psd_welch$freq
welch_power <- psd_welch$power

# Plot Welch's periodogram
plot(welch_freq, welch_power, type="l", col="darkgreen", main="Welch's Periodogram", 
     xlab="Frequency (Hz)", ylab="Power")


#Example 2

# 1. Generate Time Series Data
# Create a synthetic time series with exponential decay and sinusoidal components
T <- 100  # Length of the time series
f1 <- 0.15  # Frequency of sinusoidal component
decay_rate <- 0.05  # Exponential decay factor

# Time points
t <- 0:(T-1)

# Generate signal with decay and sinusoidal pattern
signal <- exp(-decay_rate * t) * sin(2 * pi * f1 * t)

# Add random noise
set.seed(123)  # For reproducibility
noise <- 0.3 * rnorm(T)
time_series <- signal + noise

# Plot the time series
plot(t, time_series, type="o", col="blue", main="Synthetic Time Series with Decay", 
     xlab="Time", ylab="Value")

# 2. Compute Periodogram
# Use the Fast Fourier Transform (FFT) to compute the periodogram
fft_values <- fft(time_series)
frequencies <- (0:(T/2)) / T  # Frequencies of interest (up to Nyquist frequency)
power <- Mod(fft_values[1:(T/2 + 1)])^2 / T  # Periodogram values

# Plot the periodogram
plot(frequencies, power, type="h", col="red", main="Periodogram of the Time Series", 
     xlab="Frequency (Hz)", ylab="Power")

# Highlight dominant frequency
abline(v=f1, col="blue", lty=2)
legend("topright", legend=c(paste("f1 =", f1, "Hz")), 
       col=c("blue"), lty=2, bty="n")

# 3. Smoothed Periodogram
# Apply a smoothing technique to reduce noise
smoothed_power <- stats::filter(power, rep(1/5, 5), sides=2)

# Plot the smoothed periodogram
plot(frequencies, smoothed_power, type="l", col="purple", main="Smoothed Periodogram", 
     xlab="Frequency (Hz)", ylab="Smoothed Power")

# 4. Spectral Density Estimation using 'spectrum()'
# The 'spectrum' function in R performs spectral density estimation
density_estimation <- spectrum(time_series, main="Spectral Density Estimation", 
                               col="darkgreen", spans=c(3,3))


#Example 3 - Real Data

# 1. Load the AirPassengers Dataset
data("AirPassengers")  # Monthly airline passenger numbers from 1949 to 1960
time_series <- AirPassengers

# Plot the time series
plot(time_series, main="AirPassengers Time Series", col="blue", 
     ylab="Number of Passengers", xlab="Year")

# 2. Detrend the Time Series
# Remove trend using differencing to focus on seasonal components
detrended_series <- diff(time_series)

# Plot the detrended time series
plot(detrended_series, main="Detrended AirPassengers Time Series", col="red", 
     ylab="Differenced Passengers", xlab="Year")

# 3. Compute the Periodogram
# Use the FFT to compute the periodogram
T <- length(detrended_series)
fft_values <- fft(detrended_series)
frequencies <- (0:(T/2)) / T  # Frequencies up to Nyquist frequency
power <- Mod(fft_values[1:(T/2 + 1)])^2 / T  # Periodogram values

# Plot the periodogram
plot(frequencies, power, type="h", col="darkgreen", main="Periodogram of Detrended Series", 
     xlab="Frequency (1/Months)", ylab="Power")

# Highlight a possible seasonal frequency
abline(v=1/12, col="blue", lty=2)
legend("topright", legend="1/12 Frequency (Seasonal)", col="blue", lty=2, bty="n")

# 4. Spectral Density Estimation
# Use R's 'spectrum' function for smoother spectral density estimation
spectrum(detrended_series, main="Spectral Density Estimation", col="purple")

# Interpretation:
# - Peaks around 1/12 suggest annual seasonality, as expected in this dataset.
# - Additional peaks may indicate other cyclic behaviors in the data.





