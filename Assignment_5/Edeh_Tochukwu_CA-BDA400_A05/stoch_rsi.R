# AI Assistance Declaration: I used Gemini for nested base-function mappings across multi-day rolling ranges.

stoch_rsi <- function(data, period, k_period, d_period) {
  rsi_values <- rsi(data, period)
  k_values <- rep(NA, length(rsi_values))
  
  for (i in period:length(rsi_values)) {
    window <- rsi_values[(i - period + 1):i]
    window_clean <- window[!is.na(window)]
    
    if (length(window_clean) > 0) {
      min_rsi <- min(window_clean)
      max_rsi <- max(window_clean)
      
      if (max_rsi != min_rsi) {
        k_values[i] <- (rsi_values[i] - min_rsi) / (max_rsi - min_rsi)
      } else {
        k_values[i] <- 0
      }
    }
  }
  
  # %K line is the simple moving average of k_values
  k_values_clean <- k_values[!is.na(k_values)]
  k_line_raw <- sma(k_values_clean, k_period)
  k_line <- c(rep(NA, length(k_values) - length(k_line_raw)), k_line_raw)
  
  # %D line is the simple moving average of the %K line
  k_line_clean <- k_line[!is.na(k_line)]
  d_line_raw <- sma(k_line_clean, d_period)
  d_line <- c(rep(NA, length(k_line) - length(d_line_raw)), d_line_raw)
  
  result <- list(
    k_line = k_line, 
    d_line = d_line 
  ) 
  return(result)
}
