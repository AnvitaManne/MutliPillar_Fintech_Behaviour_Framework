library(xgboost)
library(Matrix)

# Prepare feature matrices

train_x <- model.matrix(
  Target ~ . - 1,
  data = train_balanced_final
)

test_x <- model.matrix(
  Target ~ . - 1,
  data = test_final
)

# Convert target labels to numeric format

train_y <- as.numeric(train_balanced_final$Target) - 1

test_y <- as.numeric(test_final$Target) - 1

# Create DMatrix objects

dtrain <- xgb.DMatrix(
  data = train_x,
  label = train_y
)

dtest <- xgb.DMatrix(
  data = test_x,
  label = test_y
)

# XGBoost model parameters

params <- list(
  booster = "gbtree",
  objective = "multi:softmax",
  num_class = 4,
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train XGBoost model

set.seed(42)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 500,
  watchlist = list(
    val = dtest,
    train = dtrain
  ),
  early_stopping_rounds = 20,
  print_every_n = 50
)

# Generate predictions

xgb_preds <- predict(
  xgb_model,
  test_x
)

# Convert predictions to factor labels

final_xgb_preds <- factor(
  xgb_preds,
  levels = c(0, 1, 2, 3),
  labels = levels(test_final$Target)
)

# Model evaluation

confusionMatrix(
  final_xgb_preds,
  test_final$Target
)

# Extract class-wise F1-scores

cm_xgb <- confusionMatrix(
  final_xgb_preds,
  test_final$Target
)

f1_scores <- cm_xgb$byClass[, "F1"]

cat("\nClass-wise XGBoost F1-Scores\n")

print(f1_scores)