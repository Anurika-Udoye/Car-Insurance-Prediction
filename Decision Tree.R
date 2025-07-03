library(rpart)
library(rpart.plot)
library(caret)

# Use your already prepared data
tree_model <- rpart(is_claim ~ ., data = train_data_bal, method = "class")

#rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE)

# Predict class probabilities
tree_probs <- predict(tree_model, newdata = test_data, type = "prob")[,2]

# Convert to binary classes (threshold 0.5)
tree_preds <- ifelse(tree_probs > 0.3, 1, 0)

# Confusion matrix
conf_matrix_tree <- confusionMatrix(as.factor(tree_preds), test_data$is_claim, positive = "1")
print(conf_matrix_tree)

# Extract metrics
precision <- conf_matrix_tree$byClass["Precision"]
recall <- conf_matrix_tree$byClass["Recall"]
f1 <- conf_matrix_tree$byClass["F1"]
accuracy <- mean(tree_preds == as.numeric(as.character(Y_test)))

# Summary report
report <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score", "Accuracy"),
  Value = c(precision, recall, f1, accuracy)
)
print(report)

png("tree_plot.png", width = 1200, height = 800)
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE)
dev.off()

printcp(tree_model)  # Shows splits and complexity
summary(tree_model)  # Shows variables used and their importance
library(rpart.plot)
rpart.plot(tree_model, type = 2, extra = 104)  # Visualize splits

# Or check importance numerically
tree_model$variable.importance






# 📦 Load required libraries
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# 🎯 Top 5 most important variables + target
top_vars <- c("age_of_car", "policy_tenure", "displacement",
              "power_to_rpm_ratio", "torque_to_rpm_ratio", "is_claim")

# 📂 Subset your balanced train data and test data
train_top <- train_data_bal %>% select(all_of(top_vars))
test_top  <- test_data %>% select(all_of(top_vars))

# 🌲 Train Decision Tree Model
tree_model_top <- rpart(is_claim ~ ., data = train_top, method = "class")

# 🔍 Predict probabilities
tree_probs <- predict(tree_model_top, newdata = test_top, type = "prob")[, 2]

# 🧾 Class prediction using threshold (default 0.5, you can tune this)
tree_preds <- ifelse(tree_probs > 0.5, 1, 0)

# 🧮 Confusion Matrix
conf_matrix <- confusionMatrix(
  as.factor(tree_preds),
  as.factor(test_top$is_claim),
  positive = "1"
)
print(conf_matrix)

# 📊 Extract performance metrics
report <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score", "Accuracy"),
  Value = c(
    conf_matrix$byClass["Precision"],
    conf_matrix$byClass["Recall"],
    conf_matrix$byClass["F1"],
    mean(tree_preds == as.numeric(as.character(test_top$is_claim)))
  )
)
print(report)

# 📈 Visualize Tree
png("decision_tree_top5.png", width = 1200, height = 800)
rpart.plot(tree_model_top, type = 2, extra = 104, fallen.leaves = TRUE)
dev.off()













library(ggplot2)

importance <- data.frame(Variable = names(tree_model_small$variable.importance),
                         Importance = tree_model_small$variable.importance)
print(importance)

top_vars <- c("age_of_car", "policy_tenure", "displacement", 
              "power_to_rpm_ratio", "torque_to_rpm_ratio", "is_claim")

train_top <- train_data_bal %>% select(all_of(top_vars))
test_top <- test_data %>% select(all_of(top_vars))

tree_model_top <- rpart(is_claim ~ ., data = train_top, method = "class")

# Prediction and evaluation as usual...



top_5_vars <- c(
  "age_of_car",
  "policy_tenure",
  "displacement",
  "power_to_rpm_ratio",
  "torque_to_rpm_ratio"
)
train_data_small <- train_data_bal %>% select(all_of(top_5_vars), is_claim)
test_data_small  <- test_data %>% select(all_of(top_5_vars), is_claim)
library(rpart)

tree_model_small <- rpart(is_claim ~ ., data = train_data_small, method = "class")
library(rpart.plot)
#rpart.plot(tree_model_small, type = 2, extra = 104, fallen.leaves = TRUE)
#png("tree_top5.png", width = 1200, height = 800)
#rpart.plot(tree_model_small, type = 2, extra = 104, fallen.leaves = TRUE)
#dev.off()

# Predict probabilities for class 1 (i.e., is_claim = 1)
tree_probs <- predict(tree_model_small, newdata = test_data_small, type = "prob")[, 2]

# Convert probabilities to class predictions (threshold = 0.5)
tree_preds <- ifelse(tree_probs > 0.5, 1, 0)

# Evaluate
library(caret)
conf_matrix <- confusionMatrix(as.factor(tree_preds), test_data_small$is_claim, positive = "1")
print(conf_matrix)

# Extract metrics
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1 <- conf_matrix$byClass["F1"]
accuracy <- mean(tree_preds == as.numeric(as.character(test_data_small$is_claim)))

# Summary table
report <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score", "Accuracy"),
  Value = c(precision, recall, f1, accuracy)
)
print(report)
