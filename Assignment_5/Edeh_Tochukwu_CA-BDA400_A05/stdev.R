# AI Assistance Declaration: I used Gemini for loop transformation structures for arithmetic variation squares.

stdev <- function(data) { 
  mean_value <- sum(data) / length(data)
  diff_values <- numeric(length(data))
  
  for (i in 1:length(data)) {
    diff_values[i] <- data[i] - mean_value
  }
  
  squared_diff <- numeric(length(diff_values))
  for (i in 1:length(diff_values)) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }
  
  variance <- sum(squared_diff) / length(squared_diff)
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation) 
}
