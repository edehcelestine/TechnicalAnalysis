# ==============================================================================
# BDA400 - Assignment 2: Technical Analysis Script (Preliminary Stage)
# ==============================================================================

# 1. AUTOMATED PACKAGE SETUP & INSTALLATION
required_packages <- c("quantmod", "TTR")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(quantmod)
library(TTR)

# Custom helper function to calculate statistical Mode (Base R lacks a built-in mode function)
get_mode <- function(x) {
  uniq_x <- unique(na.omit(x))
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# 2. FUNCTION TO IMPORT AND LOAD STOCKS DATA (Step 5)
load_stock_data <- function(filepath = "portfolio.txt") {
  if (!file.exists(filepath)) {
    stop(paste("Critical Error: The configuration file", filepath, "was not found."))
  }
  
  # Read stock symbols from text file
  symbols <- readLines(filepath)
  symbols <- trimws(symbols) # Clean whitespace
  symbols <- symbols[symbols != ""] # Remove empty rows
  
  cat("--- Fetching market data for assets:", paste(symbols, collapse = ", "), "---\n")
  
  # Fetch data into environment environment
  stock_env <- new.env()
  getSymbols(symbols, src = "yahoo", from = "2026-01-01", env = stock_env)
  
  # Extract variables to a clean list of standalone data frames
  data_list <- list()
  for (sym in symbols) {
    if (exists(sym, envir = stock_env)) {
      data_list[[sym]] <- as.data.frame(get(sym, envir = stock_env))
    }
  }
  return(data_list)
}

# 3. FUNCTION TO COMPUTE BASIC STATISTICS (Step 6)
calculate_statistics <- function(stock_df, column_suffix = "Close") {
  # Dynamically target the requested metrics column (defaults to Closing price)
  col_name <- grep(column_suffix, names(stock_df), value = TRUE)[1]
  price_series <- stock_df[[col_name]]
  
  # Build structured statistical summary matrix
  stats <- list(
    Mean = mean(price_series, na.rm = TRUE),
    Median = median(price_series, na.rm = TRUE),
    Mode = get_mode(price_series),
    Std_Dev = sd(price_series, na.rm = TRUE),
    Simple_Moving_Avg_20d = tail(SMA(price_series, n = 20), 1) # Latest 20-day trend metric
  )
  return(as.data.frame(stats))
}

# 4. RUN PIPELINE & UTILITIES TO DISPLAY OUTPUT (Step 7)
run_analysis_pipeline <- function() {
  # Step A: Load the portfolios
  portfolio_data <- load_stock_data("portfolio.txt")
  
  # Step B: Loop data displays and process statistics matrices
  all_metrics <- list()
  
  for (stock_name in names(portfolio_data)) {
    cat("\n=======================================================\n")
    cat("MARKET DATA DISPLAY PROFILE FOR ASSIGNMENT 2:", stock_name, "\n")
    cat("=======================================================\n")
    
    df <- portfolio_data[[stock_name]]
    
    # Visual Layout Formats: Interactive Data Frame Previews
    print(head(df, 5)) 
    
    # Calculate analytical matrices
    stock_stats <- calculate_statistics(df)
    rownames(stock_stats) <- stock_name
    all_metrics[[stock_name]] <- stock_stats
  }
  
  # Combine data tables for direct side-by-side metric comparison layouts
  cat("\n=======================================================\n")
  cat("CONSOLIDATED PORTFOLIO METRICS SUMMARY TABLE\n")
  cat("=======================================================\n")
  summary_table <- do.call(rbind, all_metrics)
  print(summary_table)
}

# Execute the final calculation routine
run_analysis_pipeline()
