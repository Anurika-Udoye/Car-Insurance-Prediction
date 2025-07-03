
# Load required libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# Load the dataset
data <- read.csv("Simplified_CarInsuranceDataset.csv", stringsAsFactors = TRUE)

# Remove rows with missing target values (likely from test set)
data <- data[!is.na(data$is_claim), ]

# Convert categorical variables
data$fuel_type <- as.factor(data$fuel_type)
data$segment <- as.factor(data$segment)
data$transmission_type <- as.factor(data$transmission_type)
data$is_claim <- as.factor(data$is_claim)

# Optional: Check structure
str(data)

# Train-test split
set.seed(123)
trainIndex <- createDataPartition(data$is_claim, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Logistic Regression Model
log_model <- glm(is_claim ~ . - policy_id, data = train, family = "binomial")
log_pred <- predict(log_model, test, type = "response")
log_class <- ifelse(log_pred > 0.5, 1, 0)
log_cm <- confusionMatrix(as.factor(log_class), test$is_claim, positive = "1")

# Decision Tree Model
tree_model <- rpart(is_claim ~ . - policy_id, data = train, method = "class")
tree_pred <- predict(tree_model, test, type = "class")
tree_cm <- confusionMatrix(tree_pred, test$is_claim, positive = "1")

# Print comparison
print("Logistic Regression Results:")
print(log_cm)

print("Decision Tree Results:")
print(tree_cm)

# Plot the decision tree
rpart.plot(tree_model)
