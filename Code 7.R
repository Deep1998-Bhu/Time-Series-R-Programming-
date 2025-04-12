# Install necessary packages
install.packages("vars")

install.packages("MTS")

# Load the libraries
library(vars)
library(MTS)

# Step 1: Simulate Data
set.seed(123)

# Simulate two interrelated time series
n <- 200
e1 <- rnorm(n)
e2 <- rnorm(n)
y1 <- filter(e1, filter = c(0.5, -0.3), method = "recursive")
y2 <- filter(e2, filter = c(0.4, 0.2), method = "recursive")

# Combine into a matrix
sim_data <- cbind(y1, y2)
colnames(sim_data) <- c("Series1", "Series2")

# Convert to a time series object
sim_data_ts <- ts(sim_data)
plot.ts(sim_data_ts, main = "Simulated Data", col = c("blue", "red"))

# Step 2: Fit VAR Model
var_model <- VAR(sim_data_ts, p = 2)  # p is the lag order
summary(var_model)

# Step 3: Forecast using VAR
var_forecast <- VARpred(var_model, h = 30)  # 30 steps ahead forecast
print(var_forecast$pred)
plot(1:200,sim_data_ts[,1],type='l',xlim=c(0,235))
lines(201:230,var_forecast$pred[,1],col='red')

# Step 4: Fit VMA Model
vma_model <- VMA(sim_data_ts, q = 2)  # q is the moving average order
summary(vma_model)

# Step 5: Fit VARMA Model
varma_model <- VARMA(sim_data_ts, p = 1, q = 1)  # p is AR order, q is MA order
summary(varma_model)

# Step 6: Forecast using VARMA
varma_forecast <- VARMApred(varma_model, h = 30)  # 10 steps ahead
print(varma_forecast$pred)
plot(1:200,sim_data_ts[,1],type='l',xlim=c(0,235))
lines(201:230,varma_forecast$pred[,1],col='red')

# Step 7: Diagnostics for VAR and VARMA Models

MTSdiag(var_model, gof = 24, adj = 0, level = F)

MTSdiag(varma_model, gof = 24, adj = 0, level = F)


# Step 8: Real Data Example (Canada Dataset)
data(Canada)
head(Canada)

# Fit VAR model to real data
var_model_real <- VAR(Canada, p = 2)
summary(var_model_real)

# Forecast using VAR for real data
var_forecast <- VARpred(var_model_real, h = 10)  # 10 steps ahead forecast
print(var_forecast$pred)
plot(1:84,Canada[,1],type='l',xlim=c(0,95),ylim=c(925,980))
lines(85:94,var_forecast$pred[,1],col='red')

# Fit VARMA model to real data
varma_model_real <- VARMA(Canada, p = 1, q = 1)
summary(var_model_real)

# Forecast using VARMA for real data
varma_forecast <- VARMApred(varma_model_real, h = 10)  # 10 steps ahead forecast
print(varma_forecast$pred)
plot(1:84,Canada[,1],type='l',xlim=c(0,95),ylim=c(925,980))
lines(85:94,varma_forecast$pred[,1],col='red')

# Step 10: Compare Models using AIC
cat("AIC for VAR Model:", var_model_real$aic, "\n")
cat("AIC for VARMA Model:", varma_model_real$aic, "\n")


