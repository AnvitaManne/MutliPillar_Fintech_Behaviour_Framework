library(forecast)
library(plotly)
library(scales)

# Load and prepare monthly household spending data

p1_monthly <- household_clean %>%
  
  filter(
    Transaction_Type == "Expense",
    Amount_INR > 0
  ) %>%
  
  mutate(
    YearMonth = floor_date(Clean_Date, "month")
  ) %>%
  
  group_by(YearMonth) %>%
  
  summarise(
    Total_Spend = sum(Amount_INR, na.rm = TRUE),
    Tx_Count = n(),
    .groups = "drop"
  ) %>%
  
  arrange(YearMonth)

# Train-test split

holdout_n <- 5

n <- nrow(p1_monthly)

p1_train <- p1_monthly[1:(n - holdout_n), ]

p1_test <- p1_monthly[(n - holdout_n + 1):n, ]

# Create time series

ts_train <- ts(
  
  p1_train$Total_Spend,
  
  start = c(
    year(min(p1_train$YearMonth)),
    month(min(p1_train$YearMonth))
  ),
  
  frequency = 12
)

# Fit SARIMA model

p1_model <- auto.arima(
  
  ts_train,
  
  seasonal = TRUE,
  
  stepwise = FALSE,
  approximation = FALSE,
  trace = FALSE
)

print(p1_model)

# Validation forecast

p1_fc <- forecast(
  
  p1_model,
  
  h = holdout_n,
  
  level = c(80, 95)
)

p1_val <- p1_test %>%
  
  mutate(
    
    Forecast = as.numeric(p1_fc$mean),
    
    Lo80 = as.numeric(p1_fc$lower[,1]),
    Hi80 = as.numeric(p1_fc$upper[,1]),
    
    Lo95 = as.numeric(p1_fc$lower[,2]),
    Hi95 = as.numeric(p1_fc$upper[,2]),
    
    Error = Total_Spend - Forecast,
    
    APE = abs(Error / Total_Spend) * 100,
    
    In80 = Total_Spend >= Lo80 &
      Total_Spend <= Hi80,
    
    In95 = Total_Spend >= Lo95 &
      Total_Spend <= Hi95,
    
    Month_Label = format(YearMonth, "%b %Y")
  )

# Remove incomplete months from evaluation

p1_val_clean <- p1_val %>%
  
  filter(
    YearMonth < as.Date("2018-08-01")
  )

# Evaluation metrics

mape_clean <- mean(p1_val_clean$APE)

rmse_clean <- sqrt(mean(p1_val_clean$Error^2))

mae_clean <- mean(abs(p1_val_clean$Error))

cat("\nP1 Validation Metrics\n")

cat(
  "MAPE:",
  round(mape_clean, 1),
  "%\n"
)

cat(
  "RMSE:",
  comma(round(rmse_clean)),
  "\n"
)

cat(
  "MAE:",
  comma(round(mae_clean)),
  "\n"
)

# Mark complete vs incomplete months

p1_val_annotated <- p1_val %>%
  
  mutate(
    Complete = YearMonth < as.Date("2018-08-01")
  )

# Final validation plot

p1_plot_final <- plot_ly() %>%
  
  add_trace(
    data = p1_val_clean,
    
    x = ~YearMonth,
    y = ~Hi95,
    
    type = "scatter",
    mode = "lines",
    
    line = list(color = "transparent"),
    
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p1_val_clean,
    
    x = ~YearMonth,
    y = ~Lo95,
    
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(231,76,60,0.10)",
    
    line = list(color = "transparent"),
    
    name = "95% CI",
    
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p1_val_clean,
    
    x = ~YearMonth,
    y = ~Hi80,
    
    type = "scatter",
    mode = "lines",
    
    line = list(color = "transparent"),
    
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p1_val_clean,
    
    x = ~YearMonth,
    y = ~Lo80,
    
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(231,76,60,0.22)",
    
    line = list(color = "transparent"),
    
    name = "80% CI",
    
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p1_train,
    
    x = ~YearMonth,
    y = ~Total_Spend,
    
    type = "scatter",
    mode = "lines",
    
    line = list(
      color = "#BDC3C7",
      width = 2
    ),
    
    name = "Training"
  ) %>%
  
  add_trace(
    data = p1_val_annotated %>%
      filter(Complete),
    
    x = ~YearMonth,
    y = ~Total_Spend,
    
    type = "scatter",
    mode = "lines+markers",
    
    line = list(
      color = "#2C3E50",
      width = 3
    ),
    
    marker = list(
      color = "#2C3E50",
      size = 9
    ),
    
    name = "Actual"
  ) %>%
  
  add_trace(
    data = p1_val_annotated %>%
      filter(!Complete),
    
    x = ~YearMonth,
    y = ~Total_Spend,
    
    type = "scatter",
    mode = "lines+markers",
    
    line = list(
      color = "#95A5A6",
      width = 2,
      dash = "dot"
    ),
    
    marker = list(
      color = "#95A5A6",
      size = 9,
      symbol = "x"
    ),
    
    name = "Incomplete Month"
  ) %>%
  
  add_trace(
    data = p1_val_annotated,
    
    x = ~YearMonth,
    y = ~Forecast,
    
    type = "scatter",
    mode = "lines+markers",
    
    line = list(
      color = "#E74C3C",
      width = 3,
      dash = "dash"
    ),
    
    marker = list(
      color = "#E74C3C",
      size = 9,
      symbol = "diamond"
    ),
    
    name = "SARIMA Forecast"
  ) %>%
  
  add_segments(
    x = as.numeric(as.POSIXct("2018-05-01")),
    xend = as.numeric(as.POSIXct("2018-05-01")),
    
    y = 0,
    yend = max(p1_monthly$Total_Spend) * 1.1,
    
    line = list(
      color = "#2C3E50",
      width = 1.5,
      dash = "dash"
    ),
    
    showlegend = FALSE
  ) %>%
  
  add_annotations(
    x = as.numeric(as.POSIXct("2018-04-01")),
    y = max(p1_monthly$Total_Spend) * 1.08,
    
    text = "← Train | Test →",
    
    showarrow = FALSE,
    
    font = list(
      size = 11,
      color = "#2C3E50"
    )
  ) %>%
  
  layout(
    
    title = list(
      
      text = paste0(
        "<b>SARIMA-Based Validation Forecast for Household Spending Behavior</b>",
        "<br>",
        "<sup>",
        "MAPE: ",
        round(mape_clean, 1),
        "% | ",
        "Validation performed on complete monthly observations only | ",
        "Grey × markers indicate incomplete months excluded from evaluation",
        "</sup>"
      ),
      
      font = list(size = 15)
    ),
    
    xaxis = list(
      title = "",
      showgrid = FALSE,
      range = c("2015-01-01", "2018-09-30")
    ),
    
    yaxis = list(
      title = "Monthly Spend (INR)",
      tickprefix = "₹",
      tickformat = ",",
      gridcolor = "#ECF0F1"
    ),
    
    plot_bgcolor = "#FDFEFE",
    paper_bgcolor = "#FDFEFE",
    
    legend = list(
      orientation = "h",
      y = -0.15
    ),
    
    hovermode = "x",
    
    margin = list(
      t = 100,
      b = 80
    )
  )

p1__validation_plot