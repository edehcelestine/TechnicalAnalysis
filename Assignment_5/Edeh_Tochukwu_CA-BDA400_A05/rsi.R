# AI Assistance Declaration: I used Gemini for translating Wilder's smoothing algorithms to sequential array pipes.

rsi <- function(data, period) { 
  diff_values <- diff(data)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  rsi_values <- rep(NA, length(data))
  
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    
    if (avg_loss == 0) {
      rs <- Inf
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values) 
}
