library(tidyverse)
library(lubridate)
library(caret)
library(themis)
library(randomForest)

# Feature engineering and model dataset preparation

ml_final_data <- unified_master %>%
  filter(Source != "Pillar_2_Risk_Profiles") %>%
  mutate(
    Log_Amount = log10(Amount_INR + 1),
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
    Day_of_Month,
    Is_Weekend,
    Time_Slot,
    Intent_Group
  ) %>%
  drop_na()

# Train-test split

set.seed(42)

trainIndex <- createDataPartition(
  ml_final_data$Target,
  p = 0.8,
  list = FALSE
)

train_final <- ml_final_data[trainIndex, ]
test_final  <- ml_final_data[-trainIndex, ]

# Handle class imbalance using upsampling

balanced_recipe <- recipe(Target ~ ., data = train_final) %>%
  step_upsample(Target, over_ratio = 0.4) %>%
  prep()

train_balanced_final <- juice(balanced_recipe)

# Random Forest model training

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

# Generate predictions

ultra_preds <- predict(rf_ultra, test_final)

# Model evaluation

confusionMatrix(
  ultra_preds,
  test_final$Target
)

# Extract class-wise F1-scores

f1_results <- confusionMatrix(
  ultra_preds,
  test_final$Target
)$byClass[, "F1"]

cat("\nClass-wise Random Forest F1-Scores\n")

print(f1_results)