# AI Assistance Declaration: I used Gemini for translating algorithm pseudocode into base R array matrices.
# Prompts used: Core script syntax structure extraction. All final verification loops completed by myself.

sma <- function(data, period) { 
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  sma_values <- numeric(length(data) - period + 1)
  
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values) 
}
