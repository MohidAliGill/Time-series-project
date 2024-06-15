# Load the necessary package for time series analysis
library(forecast)

# Read the dataset
house_prices = read.csv('em_house_prices.csv')

# Create a time series
house_prices_ts = ts(house_prices$average_price_gbp, start = c(2010, 1), frequency = 12)

# Plot the time series
ts.plot(house_prices_ts, main='House Prices in East Midlands', xlab='Time (Months)', ylab='Price in GBP')

# Decompose the time series to analyze components
decomposed = stl(house_prices_ts, s.window = 'periodic')
plot(decomposed)
range(decomposed$time.series[,'seasonal'])
range(decomposed$time.series[,'remainder'])

# Analyze the autocorrelation of the time series
acf(house_prices_ts, lag.max = 48, main='ACF of House Prices')

pacf(house_prices_ts, lag.max = 48, main='PACF of House Prices')

# Test for stationarity
stationarity_test = PP.test(house_prices_ts)
print(stationarity_test)

# Determine optimal differencing level
for(d in 1:5){
  diff_ts = diff(house_prices_ts, differences = d)
  print(paste('Order =', d, 'Variance =', var(diff_ts)))
}
var(house_prices_ts)

# Plot the first differenced series
first_diff = diff(house_prices_ts)
ts.plot(first_diff, main='First Difference of Prices', xlab='Time (Months)', ylab='Differenced Prices')

# Analyze the differenced series
acf(first_diff, lag.max = 60, main='Differenced ACF')
abline(v=c(12,24,36,48), col='red', lty=2)

pacf(first_diff, lag.max = 48, main='Differenced PACF')

# Test stationarity again
stationarity_test_diff = PP.test(first_diff)
cat('P-value for PP test after differencing:', stationarity_test_diff$p.value, '\n')
print (stationarity_test_diff)

# Fit a SARIMA model
sarima_model = arima(first_diff, order = c(2, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12))
print(paste0('SARIMA(2,2,1)(0,1,1)[12] model, AIC = ', round(sarima_model$aic, 2)))

# Select and summarize the model
final_model = arima(first_diff, order = c(2, 2, 1), seasonal = list(order = c(0, 1, 1), period = 12))
summary(final_model)

# Diagnostic plots for the selected model
tsdiag(final_model)

# Distribution of residuals
hist(final_model$residuals, freq = FALSE, main='Residuals Distribution', xlab='Residuals')
lines(density(final_model$resid, na.rm = TRUE))

# Q-Q plot of residuals
qqnorm(final_model$residuals)
qqline(final_model$residuals)

# Evaluate model performance
summary(final_model)  # Extract MAE and RMSE1

# Plot actual vs. fitted values
plot(as.numeric(fitted(final_model)), as.numeric(first_diff), main='Actual vs fitted',xlab='fitted',ylab='actual')
abline(a=0, b=1, col='red')

# Forecast future values
forecasted_values = forecast(final_model, 6)
plot(forecasted_values)

# Re-fit the model on non-differenced data and forecast
original_model = arima(house_prices_ts, order = c(2, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12))
forecast_original = forecast(original_model, 6)
plot(forecast_original)
