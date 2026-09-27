# AI Assistance Declaration: I used Gemini for string vector matching loops for cross-under boundary maps.

crossunder <- function(arr1, arr2) { 
  if (length(arr1) != length(arr2)) { 
    stop("Both arrays should have the same length") 
  }
  
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"
  
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True" 
    } else { 
      crossunder_signals[i] <- "False" 
    }
  }
  
  return(crossunder_signals)
}
