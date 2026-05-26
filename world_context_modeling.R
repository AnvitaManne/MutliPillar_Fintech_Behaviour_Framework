library(tidyverse)
library(lubridate)
library(caret)
library(themis)
library(randomForest)
library(xgboost)
library(Matrix)

# Population-level income reference

world_avg_inc <- mean(
  unified_master$Annual_Income[
    unified_master$Source == "Pillar_2_Risk_Profiles"
  ],
  na.rm = TRUE
)

# Feature engineering and model dataset preparation

ml_final_data <- unified_master %>%
  filter(Source != "Pillar_2_Risk_Profiles") %>%
  mutate(
    Log_Amount = log10(Amount_INR + 1),
    
    Income_Impact = Amount_INR / (world_avg_inc / 12),
    
    Day_of_Month = mday(Date),
    
    Is_Weekend = as.factor(
      ifelse(wday(Date) %in% c(1, 7), 1, 0)
    ),
    
    Time_Slot = as.factor(case_when(
      hour(Date) %in% 6:11  ~ "Morning",
      hour(Date) %in% 12:14 ~ "Lunch",
      hour(Date) %in% 18:22 ~ "Evening",
      TRUE ~ "Night"
    )),
    
    Intent_Group = as.factor(case_when(
      Category %in% c(
        "Food",
        "Groceries",
        "Health",
        "Transportation",
        "Housing and Utilities"
      ) ~ "Survival",
      
      Category %in% c(
        "Shopping",
        "Entertainment",
        "Travel",
        "Gifts",
        "Hobbies"
      ) ~ "Discretionary",
      
      Category %in% c(
        "Investment",
        "Savings",
        "Education",
        "Salary",
        "Bonus"
      ) ~ "Growth_Income",
      
      TRUE ~ "Noise_Other"
    )),
    
    Target = as.factor(Budget_Type)
  ) %>%
  select(
    Target,
    Log_Amount,
    Income_Impact,
    Day_of_Month,
    Is_Weekend,
    Time_Slot,
    Intent_Group
  ) %>%
  drop_na()

# Train-test split and class balancing

set.seed(42)

trainIndex <- createDataPartition(
  ml_final_data$Target,
  p = 0.8,
  list = FALSE
)

train_final <- ml_final_data[trainIndex, ]

test_final <- ml_final_data[-trainIndex, ]

balanced_recipe <- recipe(
  Target ~ .,
  data = train_final
) %>%
  step_upsample(Target, over_ratio = 0.4) %>%
  prep()

train_balanced_final <- juice(balanced_recipe)

# Random Forest model

rf_ultra <- randomForest(
  Target ~ .,
  data = train_balanced_final,
  ntree = 500,
  mtry = 3,
  classwt = c(
    Essential = 1,
    Income = 5,
    Lifestyle = 1,
    Other = 0.2
  )
)

rf_preds <- predict(
  rf_ultra,
  test_final
)

rf_cm <- confusionMatrix(
  rf_preds,
  test_final$Target
)

# XGBoost model

train_x <- model.matrix(
  Target ~ . - 1,
  data = train_balanced_final
)

test_x <- model.matrix(
  Target ~ . - 1,
  data = test_final
)

train_y <- as.numeric(
  train_balanced_final$Target
) - 1

dtrain <- xgb.DMatrix(
  data = train_x,
  label = train_y
)

xgb_model <- xgb.train(
  params = list(
    booster = "gbtree",
    objective = "multi:softmax",
    num_class = 4,
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  
  data = dtrain,
  
  nrounds = 500,
  
  evals = list(
    train = dtrain
  ),
  
  early_stopping_rounds = 20,
  
  verbose = 0
)

xgb_preds_raw <- predict(
  xgb_model,
  test_x
)

xgb_preds <- factor(
  xgb_preds_raw,
  levels = c(0, 1, 2, 3),
  labels = levels(test_final$Target)
)

xgb_cm <- confusionMatrix(
  xgb_preds,
  test_final$Target
)

# Model comparison results

cat("\nRandom Forest F1-Scores\n")

print(rf_cm$byClass[, "F1"])

cat("\nXGBoost F1-Scores\n")

print(xgb_cm$byClass[, "F1"])

cat("\nOverall Accuracy Comparison\n")

cat(
  "Random Forest:",
  rf_cm$overall["Accuracy"],
  "\n"
)

cat(
  "XGBoost:      ",
  xgb_cm$overall["Accuracy"],
  "\n"
)