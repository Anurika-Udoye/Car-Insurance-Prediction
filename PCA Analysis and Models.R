# Set working directory (adjust path if necessary)
setwd("/Users/Anurika/Desktop/AIS Casestudy/Predictive Analytics Project Pack")

#========================
#Principal Component Analysis
#========================
# ==== ENCODE BINARY COLUMNS ====
train <- read.csv("train.csv")
library(dplyr)

binary_cols <- c(
  "is_esc", "is_tpms", "is_parking_camera",
  "is_parking_sensors", "is_adjustable_steering",
  "is_power_steering", "is_driver_seat_height_adjustable",
  "is_day_night_rear_view_mirror", "is_ecw", "is_speed_alert",
  "is_brake_assist", "is_power_door_locks", "is_front_fog_lights",
  "is_rear_window_wiper", "is_rear_window_washer",
  "is_rear_window_defogger", "is_central_locking"
)

train <- train %>%
  mutate(across(all_of(binary_cols), ~ ifelse(. == "Yes", 1, 0)))

# ==== EXTRACT TORQUE AND POWER ====
library(dplyr)
library(stringr)
extract_power_torque_ratios <- function(df) {
  df %>%
    mutate(
      # Extract numeric values before '@' in Nm (e.g., 113Nm@4200rpm)
      torque_value = as.numeric(str_extract(max_torque, "\\d+(\\.\\d+)?")),
      torque_rpm   = as.numeric(str_extract(max_torque, "(?<=@)\\d+")),
      torque_to_rpm_ratio = torque_value / torque_rpm,
      
      # Extract power values similarly
      power_value = as.numeric(str_extract(max_power, "\\d+(\\.\\d+)?")),
      power_rpm   = as.numeric(str_extract(max_power, "(?<=@)\\d+")),
      power_to_rpm_ratio = power_value / power_rpm
    ) %>%
    # Keep only the ratio columns and all other columns except the originals
    select(-max_power, -max_torque, -power_value, -power_rpm, -torque_value, -torque_rpm)
}

train <- extract_power_torque_ratios(train)


# ==== SCALING ====
library(dplyr)

# Columns to scale
cols_to_scale <- c("population_density", "displacement", "cylinder", "gear_box", "turning_radius", "gross_weight", 
                   "torque_to_rpm_ratio","power_to_rpm_ratio", "airbags", "volume", "ncap_rating")

# Step 1: Compute min and max from train only
minmax_params <- train %>%
  summarise(across(all_of(cols_to_scale), list(min = ~min(., na.rm = TRUE),
                                               max = ~max(., na.rm = TRUE))))

# Step 2: Define min-max scaling function
scale_with_minmax <- function(df, params) {
  for (col in cols_to_scale) {
    min_val <- params[[paste0(col, "_min")]]
    max_val <- params[[paste0(col, "_max")]]
    df[[col]] <- (df[[col]] - min_val) / (max_val - min_val)
  }
  return(df)
}

# Step 3: Apply to train only
train <- scale_with_minmax(train, minmax_params)


# ==== DROPPING UNWANTED COLUMNS ====
cols_to_drop <- c("policy_id", "width", "height", "length")
train <- train %>% select(-all_of(cols_to_drop))

# ==== ONE HOT ENCODING ====
library(fastDummies)
library(dplyr)

# Columns to one-hot encode
cat_columns <- c("model", "segment", "fuel_type", "engine_type",
                 "transmission_type", "steering_type", "rear_brakes_type", "make", "area_cluster")

# One-hot encode only the train data set
train <- dummy_cols(
  train,
  select_columns = cat_columns,
  remove_first_dummy = TRUE,       # avoid dummy trap
  remove_selected_columns = TRUE   # drop original categorical columns
)


# Get numeric columns only (excluding target variable)
numeric_cols <- sapply(train, is.numeric)
numeric_cols["is_claim"] <- FALSE  # exclude target if present
train_numeric <- train[, numeric_cols]

# Perform PCA using prcomp
pca_result <- prcomp(train_numeric, center = TRUE, scale. = TRUE)

# Proportion of variance explained by each component
summary(pca_result)$importance


#keep pc1-15, A common rule is to keep enough PCs to explain at least 70–80% of the total variance.
train_pca_reduced <- as.data.frame(pca_result$x[,1:15])
train_pca_reduced$is_claim <- train$is_claim  # Add back target




#========================
#Models
#========================
# 1. Random forest
# 📦 Load Libraries
library(caret)
library(smotefamily)
library(randomForest)
library(dplyr)

# 🎯 Step 1: Split the PCA Data
set.seed(123)
train_index <- createDataPartition(train_pca_reduced$is_claim, p = 0.7, list = FALSE)
train_pca <- train_pca_reduced[train_index, ]
test_pca  <- train_pca_reduced[-train_index, ]

# 🎯 Step 2: Apply SMOTE to the Training Data
X_train <- train_pca[,1:15]
Y_train <- as.numeric(as.character(train_pca$is_claim))

smote_out <- SMOTE(X_train, Y_train, K = 5)
train_bal <- smote_out$data
colnames(train_bal)[ncol(train_bal)] <- "is_claim"
train_bal$is_claim <- as.factor(train_bal$is_claim)

# Make sure test set is formatted
test_pca$is_claim <- as.factor(test_pca$is_claim)




# 📊 Function to Extract Metrics
evaluate_model <- function(probabilities, actual, threshold, model_desc) {
  preds <- ifelse(probabilities > threshold, 1, 0)
  cm <- confusionMatrix(as.factor(preds), actual, positive = "1")
  
  data.frame(
    Trees = model_desc$trees,
    Threshold = threshold,
    Accuracy = cm$overall["Accuracy"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1_Score = cm$byClass["F1"]
  )
}

# 🔁 Loop over Tree Counts and Thresholds
tree_counts <- c(10, 100, 500)
thresholds <- c(0.5)

rf_results <- data.frame()

for (n_tree in tree_counts) {
  cat("\n🌲 Training Random Forest with", n_tree, "trees...\n")
  
  rf_model <- randomForest(is_claim ~ ., data = train_bal, ntree = n_tree)
  probs <- predict(rf_model, newdata = test_pca, type = "prob")[, 2]
  
  for (thresh in thresholds) {
    cat("- Threshold:", thresh, "\n")
    metrics <- evaluate_model(probs, test_pca$is_claim, thresh, model_desc = list(trees = n_tree))
    rf_results <- rbind(rf_results, metrics)
  }
}

# ✅ View Result Table
print(rf_results)

rf_model <- randomForest(is_claim ~ ., data = train_bal, ntree = n_tree)

if (n_tree == 500) {
  importance_500 <- randomForest::importance(rf_model)
  
  importance_500 <- data.frame(Feature = rownames(importance_500),
                               MeanDecreaseGini = importance_500[, "MeanDecreaseGini"])
  
  print("📌 Feature Importance (500 trees):")
  print(importance_500[order(-importance_500$MeanDecreaseGini), ])
  
  # Optional: Plot
  varImpPlot(rf_model, main = "Feature Importance (Random Forest - 500 Trees)")
}




# ===============================
# 🔍 Confusion Matrices & ROC Curves for Each Model
# ===============================
library(pROC)

for (n_tree in tree_counts) {
  cat("\n🌲 Evaluating Random Forest with", n_tree, "trees\n")
  
  # Re-train the model
  rf_model <- randomForest(is_claim ~ ., data = train_bal, ntree = n_tree)
  
  # Predict probabilities on test set
  probs <- predict(rf_model, newdata = test_pca, type = "prob")[, 2]
  preds <- ifelse(probs > 0.5, 1, 0)
  
  # Confusion Matrix
  cat("📦 Confusion Matrix:\n")
  cm <- confusionMatrix(as.factor(preds), test_pca$is_claim, positive = "1")
  print(cm)
  
  # ROC & AUC
  roc_obj <- roc(test_pca$is_claim, probs)
  auc_val <- auc(roc_obj)
  cat("📈 AUC for", n_tree, "trees:", round(auc_val, 4), "\n")
  
  # Plot ROC
  plot(roc_obj, col = "blue", main = paste("ROC Curve -", n_tree, "Trees"))
  legend("bottomright", legend = paste("AUC =", round(auc_val, 4)), col = "blue", lwd = 2)
}









#2. Logisticregression
log_model <- glm(is_claim ~ ., data = train_bal, family = "binomial")

# Predict
test_probs <- predict(log_model, newdata = test_pca, type = "response")
test_preds <- ifelse(test_probs > 0.5, 1, 0)

# Evaluate
conf_matrix <- confusionMatrix(as.factor(test_preds), test_pca$is_claim, positive = "1")
print(conf_matrix)

# Generate ROC object
roc_logit <- roc(response = test_pca$is_claim, predictor = test_probs)

# Plot ROC curve
plot(roc_logit,
     main = "ROC Curve - Logistic Regression",
     col = "darkred", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate AUC
auc_logit <- auc(roc_logit)
cat("AUC (Logistic Regression):", round(auc_logit, 4), "\n")






#. 3 Decision tree
library(rpart)

tree_model <- rpart(is_claim ~ ., data = train_bal, method = "class")
tree_probs <- predict(tree_model, newdata = test_pca, type = "prob")[,2]
tree_preds <- ifelse(tree_probs > 0.5, 1, 0)

conf_matrix_tree <- confusionMatrix(as.factor(tree_preds), test_pca$is_claim, positive = "1")
print(conf_matrix_tree)

library(pROC)

# Generate ROC object for decision tree
roc_tree <- roc(response = test_pca$is_claim, predictor = tree_probs)

# Plot ROC curve
plot(roc_tree,
     main = "ROC Curve - Decision Tree",
     col = "darkgreen", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Calculate AUC
auc_tree <- auc(roc_tree)
cat("AUC (Decision Tree):", round(auc_tree, 4), "\n")




summary(log_model)

library(rpart)
library(rpart.plot)

# Assuming your model is named: tree_model
tree_model <- rpart(is_claim ~ ., data = train_bal, method = "class")

# View the tree
rpart.plot(tree_model)

# Extract readable rules
library(rattle)  # If not installed: 
install.packages("asRules")
asRules(tree_model)

# Show numeric importance values
importance(rf_model)
