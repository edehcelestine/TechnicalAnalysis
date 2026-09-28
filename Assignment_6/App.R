# ==============================================================================
# BDA400 - ASSIGNMENT 6: INTERACTIVE PORTFOLIO VISUALIZATION DASHBOARD
# FRAMEWORK: R SHINY, GGPLOT2, AND QUANTMOD API INGESTION TRACKER
# FILENAME: app.R
# ==============================================================================

# AI Assistance Declaration: I used Gemini for fixing fluidPage theme syntax,
# resolving missing arrow assignment operators, and verifying ggplot layer piping.
# I verified all execution outputs and signal layer overlays manually using 
# live RStudio local web application ports. All final calculations are done by myself.

# 1. ENVIRONMENT STAGING & ENVIRONMENT VERIFICATION
required_packages <- c("shiny", "ggplot2", "quantmod", "dplyr")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages, dependencies = TRUE)

library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)

# 2. USER INTERFACE ARCHITECTURE (UI COMPONENT)
ui <- fluidPage(
  titlePanel("Quantitative Stock Analysis & Portfolio Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Acquisition Parameters"),
      textInput("ticker", "Enter Stock Symbol:", value = "AAPL"),
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2025-01-01", 
                     end = as.character(Sys.Date())),
      
      hr(),
      h4("Visualization Settings"),
      selectInput("chart_type", "Select Chart Type:", 
                  choices = c("Line Graph" = "line", "Area Chart" = "area")),
      
      checkboxGroupInput("technical_indicators", "Overlay Technical Indicators:",
                         choices = c("Simple Moving Average (20d)" = "SMA20",
                                     "Exponential Moving Average (50d)" = "EMA50")),
      
      checkboxInput("show_signals", "Display Active Trading Strategy Signals", value = TRUE),
      helpText("Trading Rule: Signal triggers on a classic 20-Day SMA vs 50-Day EMA breakout matrix overlay boundary.")
    ),
    
    mainPanel(
      plotOutput("stock_chart", height = "600px")
    )
  )
)

# 3. COMPUTATIONAL WORKSPACE ENGINE (SERVER LOGIC COMPONENT)
server <- function(input, output) {
  
  # Reactive block to securely fetch data and handle API errors cleanly
  reactive_stock_data <- reactive({
    req(input$ticker, input$date_range)
    
    # Graceful exception boundary validation loop
    tryCatch({
      symbol_clean <- toupper(trimws(input$ticker))
      raw_data <- getSymbols(symbol_clean, src = "yahoo", 
                             from = input$date_range[1], 
                             to = input$date_range[2], 
                             auto.assign = FALSE)
      
      # Convert xts object matrices to a standard, clean data frame layout
      df <- data.frame(Date = index(raw_data), coredata(raw_data))
      colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      return(df)
    }, error = function(e) {
      return(NULL) # Fail gracefully if API times out or ticker is wrong
    })
  })
  
  # Render plot panel
  output$stock_chart <- renderPlot({
    data_matrix <- reactive_stock_data()
    
    # Guard logic to verify if data array exists
    if (is.null(data_matrix) || nrow(data_matrix) == 0) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "Error: Invalid Ticker Symbol or Missing Connection Data.", 
                        color = "red", size = 6) + theme_void())
    }
    
    # 4. COMPUTE TECHNICAL INDICATORS (BASE VECTOR LOGIC)
    # Simple Moving Average (SMA 20)
    data_matrix$SMA20 <- NA
    if(nrow(data_matrix) >= 20) {
      for(i in 20:nrow(data_matrix)) {
        data_matrix$SMA20[i] <- mean(data_matrix$Close[(i-19):i])
      }
    }
    
    # Exponential Moving Average (EMA 50)
    data_matrix$EMA50 <- NA
    if(nrow(data_matrix) >= 50) {
      weight <- 2 / (50 + 1)
      # Calculate simple average as initial baseline seed
      data_matrix$EMA50[50] <- mean(data_matrix$Close[1:50])
      for(i in 51:nrow(data_matrix)) {
        data_matrix$EMA50[i] <- (data_matrix$Close[i] - data_matrix$EMA50[i-1]) * weight + data_matrix$EMA50[i-1]
      }
    }
    
    # 5. IMPLEMENT TRADING SIGNALS & STRATEGY RULES MATRIX
    data_matrix$Signal <- "Hold"
    if(nrow(data_matrix) >= 51) {
      for(i in 51:nrow(data_matrix)) {
        if(!is.na(data_matrix$SMA20[i]) && !is.na(data_matrix$EMA50[i]) && 
           !is.na(data_matrix$SMA20[i-1]) && !is.na(data_matrix$EMA50[i-1])) {
          if(data_matrix$SMA20[i] > data_matrix$EMA50[i] && data_matrix$SMA20[i-1] <= data_matrix$EMA50[i-1]) {
            data_matrix$Signal[i] <- "Buy"
          } else if(data_matrix$SMA20[i] < data_matrix$EMA50[i] && data_matrix$SMA20[i-1] >= data_matrix$EMA50[i-1]) {
            data_matrix$Signal[i] <- "Sell"
          }
        }
      }
    }
    
    # 6. GRAPHICS PIPELINE VISUALIZATION VIA GGPLOT2
    # Base layout assignment
    p <- ggplot(data_matrix, aes(x = Date, y = Close))
    
    # Fixed syntax assignment down below
    if (input$chart_type == "line") {
      p <- p + geom_line(color = "#2c3e50", size = 1)
    } else {
      p <- p + geom_area(fill = "#3498db", alpha = 0.3) + geom_line(color = "#2980b9", size = 1)
    }
    
    # Dynamic toggling layers allocation
    if ("SMA20" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = SMA20), color = "#e67e22", size = 1, linetype = "dashed")
    }
    if ("EMA50" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = EMA50), color = "#9b59b6", size = 1, linetype = "dotdash")
    }
    
    # Dynamic signal labels annotations overlay mapping configuration
    if (input$show_signals) {
      signal_events <- filter(data_matrix, Signal %in% c("Buy", "Sell"))
      if(nrow(signal_events) > 0) {
        p <- p + geom_point(data = signal_events, aes(x = Date, y = Close, color = Signal), size = 4) +
          geom_text(data = signal_events, aes(x = Date, y = Close, label = Signal), vjust = -1.2, 
                    fontface = "bold", size = 4.5) +
          scale_color_manual(values = c("Buy" = "#27ae60", "Sell" = "#c0392b"))
      }
    }
    
    # Final plot layout presentation formatting theme
    p <- p + labs(title = paste("Historical Closing Matrix Summary for", toupper(input$ticker)),
                  subtitle = "Integrated Technical Analysis Verification Interface Model",
                  x = "Timeline Calendar Vector Date", y = "Adjusted Market Closing Valuations ($)") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 16),
            axis.title = element_text(face = "bold"))
    
    print(p)
  })
}

# 4. RUN TIME LIFECYCLE DEPLOYMENT CORE TRIGGER
shinyApp(ui = ui, server = server)
