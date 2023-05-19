  # Load the data
# Load libraries and data
library(forecast)
library(rminer)
library(readxl)
library(Metrics)
bebidas <- read_excel("C:/Users/Miguel Rebelo/Desktop/Projeto_HW/bebidas.xlsx")
  
  cat("Choose the beer:\n 1-STEELA\n 2-BUD\n")
  
  beer <- as.integer(readline());
  
  if(beer==1) {
    n_beer ="STELLA"
    TS <- ts(bebidas$STELLA, start = c(2019, 2), frequency = 52)
  } else {
    n_beer="BUD"
    TS <- ts(bebidas$BUD, start = c(2019, 2), frequency = 52)
  }
  
  # Set training and testing periods
  train_period <- c(1:floor(0.9*length(TS)))
  test_period <- c((floor(0.9*length(TS))+1):length(TS))
  
  # Create training and testing datasets
  train_data <- TS[1:train_period]
  test_data <- TS[(train_period+1):length(TS)]
  
  # Define the Weekly Naive model
  weekly_naive_model <- function(train_data, test_data) {
    # Calculate seasonal indices
    seasonal_index <- c(1, rep(0, 6))
    
    # Create the forecasts for the test data
    forecasts <- rep(0, length(test_data))
    
    # Only predict the last observation
    index <- length(test_data) - 3
    forecasts[1] <- test_data[index] * seasonal_index[1]
    
    # Return the forecasts
    return(forecasts)
  }
  
  # Generate forecasts for the next 7 days
  forecasts <- weekly_naive_model(train_data, test_data)[1:7]
  
  # Display the forecast for the last observation
  cat("Forecast: ", forecasts[1], "\n")
  
# nmae <- function(pred, actual) {
#    mean(abs(pred - actual) / actual)
#  }
  # Calculate the MAE and NMAE
  predicted <- weekly_naive_model(train_data, test_data)
  actual <- test_data
  mae_error <- mae(predicted, actual)
  nmae_error <- nmae(predicted, actual)
  
  cat("MAE: ", mae_error, "\n")
  #cat("NMAE: ", nmae_error, "\n")
  
  # Plot the forecasts and actual test data for the last week
  plot(test_data[(length(test_data)-6):length(test_data)], type = "l", col = "blue", ylim = c(0, max(TS)), xlab = "Days", ylab = "Sales Volume", main = paste0("Forecast for ", n_beer))
  lines(forecasts[(length(forecasts)-6):length(forecasts)], col = "red")
  legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)
  
  
  
