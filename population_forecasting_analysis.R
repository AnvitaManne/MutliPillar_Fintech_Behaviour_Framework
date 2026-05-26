# Aggregate monthly spending activity

p3_agg <- spending_habits_clean %>%
  
  filter(
    Amount_INR > 0,
    !is.na(Transaction_Date)
  ) %>%
  
  mutate(
    YearMonth = floor_date(Transaction_Date, "month")
  ) %>%
  
  group_by(YearMonth, User_ID) %>%
  
  summarise(
    User_Monthly = sum(Amount_INR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  group_by(YearMonth) %>%
  
  summarise(
    Total_Spend = median(User_Monthly),
    Avg_Spend = mean(User_Monthly),
    User_Count = n_distinct(User_ID),
    .groups = "drop"
  ) %>%
  
  arrange(YearMonth) %>%
  
  mutate(
    Month_Label = format(YearMonth, "%b %Y")
  )

# Remove incomplete final month

if (
  tail(p3_agg$User_Count, 1) <
  mean(p3_agg$User_Count) * 0.7
) {
  
  cat("Final month removed\n")
  
  p3_agg <- p3_agg %>%
    slice(1:(n() - 1))
}

# Train-test split

holdout_n <- 4

n <- nrow(p3_agg)

p3_train <- p3_agg[1:(n - holdout_n), ]

p3_test <- p3_agg[(n - holdout_n + 1):n, ]

cat(
  "Train months:",
  nrow(p3_train),
  "| Test months:",
  nrow(p3_test),
  "\n"
)

# Build time series object

ts_train <- ts(
  p3_train$Total_Spend,
  
  start = c(
    year(min(p3_train$YearMonth)),
    month(min(p3_train$YearMonth))
  ),
  
  frequency = 12
)

# Train SARIMA model

p3_model <- auto.arima(
  ts_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = FALSE
)

print(p3_model)

# Generate validation forecasts

p3_fc <- forecast(
  p3_model,
  h = holdout_n,
  level = c(80, 95)
)

p3_val <- p3_test %>%
  
  select(
    YearMonth,
    Month_Label,
    Actual = Total_Spend
  ) %>%
  
  mutate(
    Forecast = as.numeric(p3_fc$mean),
    
    Lo80 = as.numeric(p3_fc$lower[,1]),
    Hi80 = as.numeric(p3_fc$upper[,1]),
    
    Lo95 = as.numeric(p3_fc$lower[,2]),
    Hi95 = as.numeric(p3_fc$upper[,2]),
    
    Error = Actual - Forecast,
    
    APE = abs(Error / Actual) * 100,
    
    In80 = Actual >= Lo80 &
      Actual <= Hi80
  )

# Validation metrics

mape <- mean(p3_val$APE)

rmse <- sqrt(mean(p3_val$Error^2))

mae <- mean(abs(p3_val$Error))

cat("\nValidation Metrics\n")

cat("MAPE:", round(mape, 1), "%\n")

cat("RMSE:", comma(round(rmse)), "\n")

cat("MAE :", comma(round(mae)), "\n")

# Validation forecast plot

p3_validation_plot <- plot_ly() %>%
  
  add_trace(
    data = p3_val,
    x = ~YearMonth,
    y = ~Hi95,
    type = "scatter",
    mode = "lines",
    line = list(color = "transparent"),
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p3_val,
    x = ~YearMonth,
    y = ~Lo95,
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(52,152,219,0.12)",
    
    line = list(color = "transparent"),
    
    name = "95% CI",
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p3_val,
    x = ~YearMonth,
    y = ~Hi80,
    type = "scatter",
    mode = "lines",
    line = list(color = "transparent"),
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p3_val,
    x = ~YearMonth,
    y = ~Lo80,
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(52,152,219,0.28)",
    
    line = list(color = "transparent"),
    
    name = "80% CI",
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = p3_train,
    
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
    data = p3_val,
    
    x = ~YearMonth,
    y = ~Actual,
    
    type = "scatter",
    mode = "lines+markers",
    
    line = list(
      color = "#2C3E50",
      width = 3
    ),
    
    marker = list(
      color = "#2C3E50",
      size = 10
    ),
    
    name = "Actual"
  ) %>%
  
  add_trace(
    data = p3_val,
    
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
      size = 10,
      symbol = "diamond"
    ),
    
    name = "Forecast"
  ) %>%
  
  layout(
    title = list(
      text = paste0(
        "<b>Validation of Population-Level Spending Forecast Using Aggregated User Transactions</b><br>",
        "<sup>MAPE: ",
        round(mape, 1),
        "%</sup>"
      )
    ),
    
    xaxis = list(
      title = "",
      showgrid = FALSE,
      range = c("2023-01-01", "2024-12-31")
    ),
    
    yaxis = list(
      title = "Median Monthly Spend per User (INR)",
      tickprefix = "₹",
      tickformat = ","
    ),
    
    plot_bgcolor = "#FDFEFE",
    paper_bgcolor = "#FDFEFE",
    
    legend = list(
      orientation = "h",
      y = -0.15
    ),
    
    hovermode = "x"
  )

p3_validation_plot

# Refit model on full dataset

ts_full <- ts(
  p3_agg$Total_Spend,
  
  start = c(
    year(min(p3_agg$YearMonth)),
    month(min(p3_agg$YearMonth))
  ),
  
  frequency = 12
)

p3_model_full <- auto.arima(
  ts_full,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = FALSE
)

# Generate future forecasts

future_horizon <- 12

p3_future_fc <- forecast(
  p3_model_full,
  h = future_horizon,
  level = c(80,95)
)

future_dates <- seq(
  max(p3_agg$YearMonth) %m+% months(1),
  
  by = "month",
  
  length.out = future_horizon
)

future_df <- tibble(
  
  YearMonth = future_dates,
  
  Forecast = as.numeric(p3_future_fc$mean),
  
  Lo80 = as.numeric(p3_future_fc$lower[,1]),
  Hi80 = as.numeric(p3_future_fc$upper[,1]),
  
  Lo95 = as.numeric(p3_future_fc$lower[,2]),
  Hi95 = as.numeric(p3_future_fc$upper[,2]),
  
  Month_Label = format(future_dates, "%b %Y")
)

# Future forecast plot

p3_future_plot <- plot_ly() %>%
  
  add_trace(
    data = future_df,
    x = ~YearMonth,
    y = ~Hi95,
    
    type = "scatter",
    mode = "lines",
    
    line = list(color = "transparent"),
    
    showlegend = FALSE,
    hoverinfo = "skip"
  ) %>%
  
  add_trace(
    data = future_df,
    
    x = ~YearMonth,
    y = ~Lo95,
    
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(231,76,60,0.12)",
    
    line = list(color = "transparent"),
    
    name = "95% CI"
  ) %>%
  
  add_trace(
    data = future_df,
    x = ~YearMonth,
    y = ~Hi80,
    
    type = "scatter",
    mode = "lines",
    
    line = list(color = "transparent"),
    
    showlegend = FALSE
  ) %>%
  
  add_trace(
    data = future_df,
    
    x = ~YearMonth,
    y = ~Lo80,
    
    type = "scatter",
    mode = "lines",
    
    fill = "tonexty",
    fillcolor = "rgba(231,76,60,0.28)",
    
    line = list(color = "transparent"),
    
    name = "80% CI"
  ) %>%
  
  add_trace(
    data = p3_agg,
    
    x = ~YearMonth,
    y = ~Total_Spend,
    
    type = "scatter",
    mode = "lines+markers",
    
    line = list(
      color = "#2C3E50",
      width = 2
    ),
    
    marker = list(
      color = "#2C3E50",
      size = 5
    ),
    
    name = "Historical"
  ) %>%
  
  add_trace(
    data = future_df,
    
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
      size = 10,
      symbol = "diamond"
    ),
    
    name = "2026 Forecast"
  ) %>%
  
  add_segments(
    x = as.numeric(as.POSIXct("2025-01-01")),
    xend = as.numeric(as.POSIXct("2025-01-01")),
    y = 0,
    yend = 50000,
    
    line = list(
      color = "#7F8C8D",
      dash = "dash"
    ),
    showlegend = FALSE
  ) %>%
  
  layout(
    title = list(
      text = "<b>Projected Population-Level Spending Trends for 2026</b><br>"
    ),
    
    xaxis = list(
      title = "",
      showgrid = FALSE,
      range = c("2023-01-01", "2025-12-31")
    ),
    
    yaxis = list(
      title = "Median Monthly Spend per User (INR)",
      tickprefix = "₹",
      tickformat = ","
    ),
    
    plot_bgcolor = "#FDFEFE",
    paper_bgcolor = "#FDFEFE",
    
    legend = list(
      orientation = "h",
      y = -0.15
    ),
    
    hovermode = "x"
  )

p3_future_plot