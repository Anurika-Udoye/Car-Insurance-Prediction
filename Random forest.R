# 📦 Load libraries
library(randomForest)
library(caret)
library(dplyr)
library(ROSE)

# 🎯 Select important variables
important_vars <- c("age_of_car", "policy_tenure", "displacement", 
                    "power_to_rpm_ratio", "torque_to_rpm_ratio", "is_claim")

# 🧹 Subset training and test sets
train_rf <- train_data %>% select(all_of(important_vars))
test_rf  <- test_data %>% select(all_of(important_vars))

# Ensure target is a factor
train_rf$is_claim <- as.factor(train_rf$is_claim)
test_rf$is_claim <- as.factor(test_rf$is_claim)

# 📊 Store all results
results <- data.frame()

# 🔁 Loop over number of trees and thresholds
tree_counts <- c(10, 100, 500)
thresholds <- c(0.5, 0.4, 0.3)

for (n_trees in tree_counts) {
  cat("\n🌲 Training with", n_trees, "trees\n")
  
  # 🧪 Rebalance training data with ROSE
  balanced_train <- ROSE(is_claim ~ ., data = train_rf, seed = 42)$data
  
  # 📚 Train Random Forest
  rf_model <- randomForest(is_claim ~ ., data = balanced_train, ntree = n_trees)
  
  # 🔍 Predict probabilities on test set
  pred_probs <- predict(rf_model, newdata = test_rf, type = "prob")[,2]
  
  for (thresh in thresholds) {
    cat("- Evaluating at threshold:", thresh, "\n")
    
    # Predict class using threshold
    pred_class <- ifelse(pred_probs > thresh, 1, 0)
    
    # Evaluate
    conf_mat <- confusionMatrix(factor(pred_class), test_rf$is_claim, positive = "1")
    
    # Extract metrics
    acc <- conf_mat$overall["Accuracy"]
    precision <- conf_mat$byClass["Precision"]
    recall <- conf_mat$byClass["Recall"]
    f1 <- conf_mat$byClass["F1"]
    
    # Store result
    results <- rbind(results, data.frame(
      Trees = n_trees,
      Threshold = thresh,
      Accuracy = acc,
      Precision = precision,
      Recall = recall,
      F1_Score = f1
    ))
  }
}

# ✅ Final comparison table
print(results)

# 📦 Optional: Save to CSV
# write.csv(results, "rf_rose_results.csv", row.names = FALSE)
