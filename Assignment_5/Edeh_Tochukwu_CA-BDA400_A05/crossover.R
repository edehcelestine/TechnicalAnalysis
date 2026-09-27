# AI Assistance Declaration: I used Gemini for conditional boolean matrix matching strings extraction.

crossover <- function(arr1, arr2) { 
  if (length(arr1) != length(arr2)) { 
    stop("Both arrays should have the same length") 
  }
  
  crossover_signals <- character(length(arr1))
  crossover_signals[1] <- "None"
  
  for (i in 2:length(arr1)) {
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- "Up"
    } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossover_signals[i] <- "Down"
    } else {
      crossover_signals[i] <- "None"
    }
  }
  
  return(crossover_signals) 
}
