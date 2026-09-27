# AI Assistance Declaration: I used Gemini for implementing loop tracking indexes for recursive data streams.

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  
  for (i in 1:length(data)) {
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  return(ema_values)
}
