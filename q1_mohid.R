# Required package loading
library(forecast)
library(tseries)
library(Metrics)

# Data loading and conversion
temperature_data <- read.csv('cet_temp.csv')

# Converting dataframe column to a time series object
temperature_series <- ts(temperature_data$avg_annual_temp_C, start=1900, frequency=1)

# Time Series Visualization
# (1) Plotting the time series
plot(temperature_series, ylab='Average Annual Temperature', main="Annual Average Temperature in Midlands")

# (2) Autocorrelation function
acf(temperature_series, main='Autocorrelation Function')

# (3) Partial autocorrelation function
pacf(temperature_series, main='Partial Autocorrelation Function')

# Stationarity Tests
# (1) Philips-Perron Test
PP.test(temperature_series)

# (2) Augmented Dickey-Fuller test
adf.test(temperature_series)

# Model Estimation using ARIMA
for(order in 3:7){
  model_ar <- arima(temperature_series, order=c(order,0,0))
  print(sprintf('AR(%d) AIC: %.2f', order, model_ar$aic))
}

# Choosing AR(5) based on AIC
best_model <- arima(temperature_series, order=c(5,0,0))
summary(best_model)

# Analysis of model residuals
# (1) Mean of residuals
mean_res <- mean(best_model$residuals, na.rm=TRUE)
print(sprintf('Mean of residuals: %.4f', mean_res))

# (2) Residuals distribution
hist(best_model$residuals, freq=FALSE, ylim=c(0, 0.7), xlab='Residuals', main='Residual Distribution')
res_density <- density(best_model$residuals, na.rm=TRUE)
lines(res_density)
normal_curve <- dnorm(res_density$x, mean=mean_res, sd=sd(best_model$residuals, na.rm=TRUE))
lines(res_density$x, normal_curve, col='blue', lwd=2)
legend('topright', legend=c('Density', 'Normal Fit'), col=c('black', 'blue'), lwd=2, bty='n')

# (3) Quantile-Quantile plot
qqnorm(best_model$residuals)
qqline(best_model$residuals)

# Model accuracy assessment
# (1) Error metrics
mape_value <- mape(as.numeric(temperature_series), fitted(best_model))
rmse_value <- rmse(as.numeric(temperature_series), fitted(best_model))
print(sprintf('MAPE: %.2f%%', mape_value * 100))
print(sprintf('RMSE: %.2f', rmse_value))

# (2) Overlay of Actual and Fitted Time Series
ts.plot(temperature_series, col='navy', lwd=2, main='Time Series vs. Fitted ARIMA', ylab='Temperature', xlab='Years')
lines(fitted(best_model), col='red', lwd=3)
legend('topleft', legend=c('Actual Series', 'Fitted ARIMA'), col=c('navy', 'red'), lwd=2, bty='n')
