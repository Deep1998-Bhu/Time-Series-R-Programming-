# Load necessary libraries
library(tidyverse)
library(caret)
library(Metrics)

# Simulate a time series dataset
set.seed(123)
n <- 300
time_series <- 20 + 5 * sin(seq(0, 10 * pi, length.out = n)) + rnorm(n, mean = 0, sd = 1)

# Create a data frame
data <- data.frame(time = 1:n, value = time_series)

# Create lagged features
create_lagged_features <- function(data, lags) {
  for (lag in 1:lags) {
    data[[paste0("lag_", lag)]] <- dplyr::lag(data$value, lag)
  }
  return(na.omit(data))  # Remove rows with NA caused by lagging
}

# Add 3 lagged features
lags <- 3
data <- create_lagged_features(data, lags)

# Split the data into train and test sets
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Train a linear regression model
model <- lm(value ~ ., data = train_data)
summary(model)

# Predict on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate model performance
mse <- mse(test_data$value, predictions)
cat("Mean Squared Error:", mse, "\n")

# Plot actual vs predicted
ggplot(test_data, aes(x = 1:nrow(test_data))) +
  geom_line(aes(y = value, color = "Actual")) +
  geom_line(aes(y = predictions, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values", x = "Index", y = "Value") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))


#k-NN

# Load necessary libraries
library(tidyverse)
library(class)

# Simulate a time series with sinusoidal and random patterns
set.seed(123)
n <- 500
time_series <- c(
  10 + 5 * sin(seq(0, 5 * pi, length.out = 200)),  # Sinusoidal pattern
  rnorm(150, mean = 10, sd = 2),                  # Random pattern
  20 + 3 * cos(seq(0, 3 * pi, length.out = 150))  # Another sinusoidal pattern
)

# Visualize the series
data <- data.frame(time = 1:n, value = time_series)
ggplot(data, aes(x = time, y = value)) +
  geom_line() +
  labs(title = "Simulated Time Series with Patterns", x = "Time", y = "Value")

# Segment the time series into overlapping windows
window_size <- 20
segments <- do.call(rbind, lapply(1:(n - window_size + 1), function(i) {
  segment <- time_series[i:(i + window_size - 1)]
  data.frame(
    start_time = i,
    mean_value = mean(segment),
    variance = var(segment),
    slope = coef(lm(segment ~ seq_along(segment)))[2]  # Linear trend (slope)
  )
}))

# Label data: Assume we know the sinusoidal patterns occur at specific intervals
segments$label <- ifelse(
  segments$start_time <= 200 | segments$start_time >= 351, 
  "Sinusoidal", 
  "Random"
)

# Split into train and test sets
set.seed(123)
train_indices <- sample(1:nrow(segments), size = 0.8 * nrow(segments))
train_data <- segments[train_indices, ]
test_data <- segments[-train_indices, ]

# Apply k-NN for classification
train_x <- train_data[, c("mean_value", "variance", "slope")]
train_y <- train_data$label
test_x <- test_data[, c("mean_value", "variance", "slope")]
test_y <- test_data$label

k <- 3  # Number of neighbors
predictions <- knn(train = train_x, test = test_x, cl = train_y, k = k)

# Evaluate accuracy
accuracy <- mean(predictions == test_y)
cat("Accuracy:", accuracy, "\n")

# Add predictions to the test data for visualization
test_data$predicted_label <- predictions

# Visualize patterns with their predictions
ggplot(test_data, aes(x = start_time)) +
  geom_point(aes(y = mean_value, color = predicted_label), size = 2) +
  labs(title = "Pattern Recognition Using k-NN", x = "Start Time", y = "Mean Value", color = "Predicted Label") +
  theme_minimal()



#Real Dataset


# Load necessary libraries
library(tidyverse)
library(randomForest)
library(Metrics)

# Load the AirPassengers dataset
data("AirPassengers")
ts_data <- as.numeric(AirPassengers)

# Create a data frame
data <- data.frame(time = 1:length(ts_data), value = ts_data)

# Create lagged features
create_lagged_features <- function(data, lags) {
  for (lag in 1:lags) {
    data[[paste0("lag_", lag)]] <- dplyr::lag(data$value, lag)
  }
  return(na.omit(data))  # Remove rows with NA caused by lagging
}

# Add lagged features (e.g., 12 months for seasonality)
lags <- 12
data <- create_lagged_features(data, lags)

# Split into train and test sets
train_size <- floor(0.8 * nrow(data))
train_data <- data[1:train_size, ]
test_data <- data[(train_size + 1):nrow(data), ]

# Train a Random Forest model
model <- randomForest(value ~ ., data = train_data, ntree = 100)

# Predict on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate model performance
mse <- mse(test_data$value, predictions)
cat("Mean Squared Error:", mse, "\n")

# Plot actual vs predicted
ggplot(test_data, aes(x = 1:nrow(test_data))) +
  geom_line(aes(y = value, color = "Actual")) +
  geom_line(aes(y = predictions, color = "Predicted")) +
  labs(title = "Actual vs Predicted Values", x = "Index", y = "Value") +
  scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red"))

#NNAR on Real Dataset

# Load the libraries
library(forecast)
library(ggplot2)

# Load the AirPassengers dataset
data("AirPassengers")

# Visualize the time series
autoplot(AirPassengers) +
  ggtitle("AirPassengers Dataset") +
  xlab("Year") +
  ylab("Number of Passengers")

# Fit a seasonal NNAR model to the data
nnar_model <- nnetar(AirPassengers)

# Display the model summary
summary(nnar_model)
# Forecast the next 24 months (2 years)
forecasted_values <- forecast(nnar_model, h = 24)

# Plot the forecast
autoplot(forecasted_values) +
  ggtitle("NNAR Forecast for AirPassengers") +
  xlab("Year") +
  ylab("Number of Passengers") +
  theme_minimal()

# Calculate accuracy metrics on the training data
accuracy(nnar_model)

# If you have a test set, split the data and evaluate further
# Example: Use the first 120 months as training and the rest as testing
train_data <- window(AirPassengers, end = c(1959, 12))
test_data <- window(AirPassengers, start = c(1960, 1))

# Fit the NNAR model on the training data
nnar_model_train <- nnetar(train_data)

# Forecast the test period
forecasted_test <- forecast(nnar_model_train, h = length(test_data))

# Compare predicted vs actual
autoplot(forecasted_test) +
  autolayer(test_data, series = "Actual", PI = FALSE) +
  ggtitle("NNAR Model: Forecast vs Actual") +
  xlab("Year") +
  ylab("Number of Passengers") +
  theme_minimal()

# Calculate accuracy on test data
accuracy(forecasted_test, test_data)


