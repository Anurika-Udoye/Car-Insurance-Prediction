# Data manipulation and visualization
library(tidyverse)      # Includes ggplot2, dplyr, etc.
library(data.table)     # For fast data reading and handling

# Machine learning workflow
library(caret)          # For train/test splitting, modeling, evaluation

# SMOTE or oversampling (you might need one of these depending on what works)
# install.packages("smotefamily")     # Uncomment if needed
# library(smotefamily)               # For SMOTE handling (already used in earlier code)

# Plotting and visualization
# install.packages("scales")         # Uncomment if needed for plotting scales
# library(scales)

# install.packages("ggplot2")        # Included in tidyverse
# library(ggplot2)

# If required for additional modeling (lava package)
install.packages("lava")             # For structural equation modeling, if needed
# Suppress warning messages
options(warn = -1)

# Show all columns when printing data frames
options(dplyr.width = Inf)
# Set minimal theme for all ggplot plots
theme_set(theme_minimal())

# Set default point color in plots to black
update_geom_defaults("point", list(colour = "black"))

# Optional: plot backgrounds are already white by default with `theme_minimal()`
# Load readr for reading CSV files (already in tidyverse)
library(readr)

# Set working directory (adjust path if necessary)
setwd("/Users/Anurika/Desktop/AIS Casestudy/Predictive Analytics Project Pack")

# Confirm current working directory
getwd()


# Install skimr: Provides an overview of your data (like pandas' df.describe())
install.packages("skimr")

# Install dplyr: For data manipulation (already included in tidyverse)
install.packages("dplyr")
# Load the libraries
library(skimr)    # For data summary
library(dplyr)    # For data wrangling



#CLEANING DATA
# 1. Counts missing values in each column and reshapes to a long format table
missing_values <- train %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count")

# Print missing values per column
print(missing_values)

# 2. Counts total number of duplicate rows in the dataset
sum(duplicated(train))

# 3. Count Unique Values per Column
unique_counts <- train %>%
  summarise(across(everything(), ~n_distinct(.))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "unique_count")

print(unique_counts)

# 4. Shape of datasets
cat("Shape of train dataset is", dim(train)[1], "rows and", dim(train)[2], "columns\n")
cat("Shape of test dataset is", dim(test)[1], "rows and", dim(test)[2], "columns\n")

head(train)
View(train)

# 5. Prepare data: count values of is_claim
is_claim_counts <- train %>%
  count(is_claim) %>%
  mutate(prop = n / sum(n),
         label = paste0(round(prop * 100, 2), "%"))

# 6. Pie chart using ggplot2
ggplot(is_claim_counts, aes(x = "", y = prop, fill = as.factor(is_claim))) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(fill = "is_claim", title = "Proportion of Claims") +
  theme_void()

# Convert all character columns to factors and then to numeric labels
#train_encoded <- train %>%
  #mutate(across(where(is.character), ~ as.integer(as.factor(.))))

str(train)
library(purrr)

# 7. Summary table: column type and count of missing values
data.frame(
  Column = names(train),
  Class = map_chr(train, ~ class(.x)[1]),
  NonMissing = map_int(train, ~ sum(!is.na(.x))),
  Total = nrow(train)
)

# 8. Data Description
data_description <- data.frame(
  Variable = c("policy_id", "policy_tenure", "age_of_car", "age_of_policyholder", "area_cluster", 
               "population_density", "make", "segment", "model", "fuel_type", "max_torque", "max_power",
               "engine_type", "airbags", "is_esc", "is_adjustable_steering", "is_tpms", "is_parking_sensors",
               "is_parking_camera", "rear_brakes_type", "displacement", "cylinder", "transmission_type", 
               "gear_box", "steering_type", "turning_radius", "length", "width", "height", "gross_weight", 
               "is_front_fog_lights", "is_rear_window_wiper", "is_rear_window_washer", "is_rear_window_defogger",
               "is_brake_assist", "is_power_door_lock", "is_central_locking", "is_power_steering", 
               "is_driver_seat_height_adjustable", "is_day_night_rear_view_mirror", "is_ecw", "is_speed_alert",
               "ncap_rating", "is_claim"),
  Description = c(
    "Unique ID of policyholder",
    "Policy duration (normalized)",
    "Age of the car (normalized)",
    "Age of policyholder (normalized)",
    "Encoded area cluster",
    "City population density",
    "Car make (encoded)",
    "Car segment (encoded)",
    "Car model (encoded)",
    "Fuel type (encoded)",
    "Max torque (encoded)",
    "Max power (encoded)",
    "Engine type (encoded)",
    "Number of airbags",
    "ESC present (1/0)",
    "Adjustable steering (1/0)",
    "TPMS present (1/0)",
    "Parking sensors present (1/0)",
    "Parking camera present (1/0)",
    "Rear brakes type (encoded)",
    "Engine displacement",
    "Engine cylinder count",
    "Transmission type (encoded)",
    "Number of gears",
    "Steering type (encoded)",
    "Turning radius in meters",
    "Car length (mm)",
    "Car width (mm)",
    "Car height (mm)",
    "Gross vehicle weight (kg)",
    "Front fog lights (1/0)",
    "Rear wiper (1/0)",
    "Rear washer (1/0)",
    "Rear defogger (1/0)",
    "Brake assist (1/0)",
    "Power door lock (1/0)",
    "Central locking (1/0)",
    "Power steering (1/0)",
    "Driver seat adjustable (1/0)",
    "Day-night mirror (1/0)",
    "ECW present (1/0)",
    "Speed alert (1/0)",
    "NCAP rating (0–5)",
    "Target: Was a claim filed? (1 = Yes)"
  ),
  stringsAsFactors = FALSE
)

# View
head(data_description)

# Reload data
train <- read.csv("train.csv")

# Load required libraries
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

categorical_cols <- c("fuel_type", "segment", "transmission_type", 
                      "engine_type", "steering_type", "rear_brakes_type", 
                      "area_cluster", "model")

train <- train %>%
  mutate(across(all_of(categorical_cols), as.factor))


# Step 1: Drop only 'policy_id, lenght, width, heght,  column
train <- train %>% select( -policy_id, -width, -height, -length)

# ==== ENCODE BINARY COLUMNS ====
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

# ==== ONE-HOT ENCODING ====
train <- dummy_cols(train,
                    select_columns = c("model", "segment", "fuel_type", "engine_type",
                                       "transmission_type", "steering_type", "rear_brakes_type", "area cluster"),
                    remove_first_dummy = TRUE,
                    remove_selected_columns = TRUE
)

# ==== EXTRACT TORQUE AND POWER ====
train <- train %>%
  mutate(
    torque_value = as.numeric(str_extract(max_torque, "^[0-9.]+")),
    torque_rpm   = as.numeric(str_extract(max_torque, "(?<=@)[0-9]+")),
    power_value  = as.numeric(str_extract(max_power, "^[0-9.]+")),
    power_rpm    = as.numeric(str_extract(max_power, "(?<=@)[0-9]+"))
  ) %>%
  select(-max_torque, -max_power)




#--------------------
#Corrolatiom
#--------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2)

# 1. Define binary columns
binary_cols <- c(
  "is_esc", "is_tpms", "is_parking_camera",
  "is_parking_sensors", "is_adjustable_steering",
  "is_power_steering", "is_driver_seat_height_adjustable",
  "is_day_night_rear_view_mirror", "is_ecw", "is_speed_alert",
  "is_brake_assist", "is_power_door_locks", "is_front_fog_lights",
  "is_rear_window_wiper", "is_rear_window_washer",
  "is_rear_window_defogger", "is_central_locking"
)

# 2. Filter numeric features excluding binary and is_claim
numeric_data <- train %>%
  select(where(is.numeric)) %>%
  select(-all_of(binary_cols), -is_claim)

# 3. Compute correlation of each numeric feature with is_claim
correlation_df <- numeric_data %>%
  summarise(across(everything(),
                   ~ cor(.x, train$is_claim, use = "complete.obs"))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "correlation")

# 4. Plot vertical correlation bar chart
ggplot(correlation_df, aes(x = correlation, y = fct_reorder(feature, correlation))) +
  geom_col(fill = "#0073C2FF") +
  geom_text(aes(label = round(correlation, 2)),
            hjust = ifelse(correlation_df$correlation > 0, -0.1, 1.1)) +
  theme_minimal() +
  labs(title = "Correlation with is_claim (Excluding Binary Columns)",
       x = "Correlation", y = "Feature") +
  xlim(-1, 1)

# Print correlation table to console
correlation_df %>%
  arrange(desc(abs(correlation))) %>%
  print(n = Inf)

# 5. Full correlation matrix of numeric features (excluding binary)
cor_matrix <- cor(numeric_data, use = "complete.obs")

# 6. Melt matrix for heatmap
cor_melt <- melt(cor_matrix)

# 7. Heatmap plot
ggplot(cor_melt, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap (Non-Binary Numeric Features)", x = "", y = "")


# Histogram of age_of_car
ggplot(train, aes(x = age_of_car)) +
  geom_histogram(binwidth = 0.05, fill = "#0073C2FF", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Age of Car", x = "Age of Car", y = "Count")

# Boxplot before filtering
ggplot(train, aes(y = age_of_car)) +
  geom_boxplot(fill = "#0073C2FF") +
  theme_minimal() +
  labs(title = "Boxplot of Age of Car", y = "Age of Car")

# Remove top 0.5% outliers from age_of_car
train <- train %>%
  filter(age_of_car <= quantile(age_of_car, 0.995, na.rm = TRUE))

# Boxplot after filtering
ggplot(train, aes(y = age_of_car)) +
  geom_boxplot(fill = "#0073C2FF") +
  theme_minimal() +
  labs(title = "Boxplot of Age of Car (Outliers Removed)", y = "Age of Car")

# Stacked density plot of age_of_car by is_claim
ggplot(train, aes(x = age_of_car, fill = factor(is_claim))) +
  geom_density(alpha = 0.6, position = "stack") +
  scale_fill_manual(values = c("0" = "#0073C2FF", "1" = "#EFC000FF"),
                    name = "Is Claim", labels = c("No", "Yes")) +
  theme_minimal() +
  labs(title = "Stacked Density Plot of Age of Car by Claim Status",
       x = "Age of Car", y = "Density")

# Stacked density plot of policy_tenure by is_claim
ggplot(train, aes(x = policy_tenure, fill = factor(is_claim))) +
  geom_density(alpha = 0.6, position = "stack") +
  theme_minimal() +
  labs(title = "Density of Policy Tenure by Claim Status", fill = "Is Claim")



#--------------------
#PCA
#--------------------
# ✅ Scale selected columns
cols_to_scale <- c("policy_tenure", "gross_weight", "population_density", "volume", "torque_value", "torque_rpm", "power_value", "power_rpm", "airbags", "displacement", "turning radius")

train <- train %>%
  mutate(across(all_of(cols_to_scale), ~ (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))))

library(fastDummies)

# One-hot encode 'make' and 'area_cluster'
train <- dummy_cols(train,
                    select_columns = c("make", "area_cluster"),
                    remove_first_dummy = TRUE,       # avoid dummy variable trap
                    remove_selected_columns = TRUE)  # remove original columns



# Load required packages
install.packages("caret")
install.packages("fastDummies")
install.packages("smotefamily")
library(caret)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(smotefamily)
library(stringr)

# Set seed for reproducibility
set.seed(42)


# ==== SPLIT DATA ====
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_data <- train[split_index, ]
test_data  <- train[-split_index, ]

X_train <- train_data %>% select(-is_claim)
Y_train <- train_data$is_claim

X_test <- test_data %>% select(-is_claim)
Y_test <- test_data$is_claim

# ==== VISUALIZE TARGET CLASS ====
ggplot(train, aes(x = factor(is_claim))) +
  geom_bar(fill = c("#0073C2FF", "#EFC000FF")) +
  labs(title = "Class Distribution: is_claim",
       x = "Is Claim (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

# ==== ENCODE BINARY COLUMNS ====
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

# ==== ONE-HOT ENCODING ====
train <- dummy_cols(train,
                    select_columns = c("model", "segment", "fuel_type", "engine_type",
                                       "transmission_type", "steering_type", "rear_brakes_type"),
                    remove_first_dummy = TRUE,
                    remove_selected_columns = TRUE
)

# ==== EXTRACT TORQUE AND POWER ====
train <- train %>%
  mutate(
    torque_value = as.numeric(str_extract(max_torque, "^[0-9.]+")),
    torque_rpm   = as.numeric(str_extract(max_torque, "(?<=@)[0-9]+")),
    power_value  = as.numeric(str_extract(max_power, "^[0-9.]+")),
    power_rpm    = as.numeric(str_extract(max_power, "(?<=@)[0-9]+"))
  ) %>%
  select(-max_torque, -max_power)

# === OPTIONAL: Impute NAs in extracted columns ===
train <- train %>%
  mutate(across(c(torque_value, torque_rpm, power_value, power_rpm),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# === SPLIT DATA AGAIN AFTER CLEANING ===
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_data <- train[split_index, ]
test_data  <- train[-split_index, ]

X_train <- train_data %>% select(-is_claim)
Y_train <- as.factor(train_data$is_claim)

X_test <- test_data %>% select(-is_claim)
Y_test <- as.factor(test_data$is_claim)


#
# ==== SMOTE ====
# Make sure data is all numeric
X_train <- X_train %>% mutate(across(everything(), as.numeric))

smote_result <- SMOTE(X_train, Y_train, K = 5)

X_train_bal <- smote_result$data[, -ncol(smote_result$data)]
Y_train_bal <- smote_result$data$class

# Impute missing values in SMOTE result
X_train_bal <- X_train_bal %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# ==== CHECK BALANCED OUTPUT ====
table(Y_train_bal)



#
#LOGISTIC REGRESSSION#
#

# 1. Combine X and Y into one training data frame
train_data_bal <- cbind(X_train_bal, is_claim = Y_train_bal)
test_data <- cbind(X_test, is_claim = Y_test)

# 2.Convert target to factor
train_data_bal$is_claim <- as.factor(train_data_bal$is_claim)
test_data$is_claim <- as.factor(test_data$is_claim)

# 3. Fit logistic regression model
log_model <- glm(is_claim ~ ., data = train_data_bal, family = "binomial")

# 4. Predict on training set
train_preds <- predict(log_model, newdata = train_data_bal, type = "response")
train_preds_class <- ifelse(train_preds > 0.5, 1, 0)

# 5. Predict on test set
test_preds <- predict(log_model, newdata = test_data, type = "response")
test_preds_class <- ifelse(test_preds > 0.5, 1, 0)

# 6. Accuracy on training and test sets
train_acc <- mean(train_preds_class == as.numeric(as.character(train_data_bal$is_claim)))
test_acc <- mean(test_preds_class == as.numeric(as.character(test_data$is_claim)))

cat("Training Accuracy:", round(train_acc, 4), "\n")
cat("Test Accuracy:", round(test_acc, 4), "\n")



#(NOT PART OF THE MAIN CODE)
#MAKE PREDICTIONS ON TEST SET

# --- STEP 1: Make predictions ---
# Assume `log_model` is your trained logistic regression model
# and test_data is your test set

# Predict probabilities
test_probs <- predict(log_model, newdata = X_test, type = "response")

# Convert to class (threshold 0.5)
test_preds_class <- ifelse(test_probs >= 0.5, 1, 0)

# --- STEP 2: Confusion Matrix ---
# Generate confusion matrix comparing predicted vs actual
conf_matrix <- confusionMatrix(as.factor(test_preds_class), as.factor(Y_test))
print(conf_matrix)

# --- STEP 3: ROC Curve + AUC ---
#Calculate ROC curve and AUC
roc_obj <- roc(Y_test, test_probs)

# Plot ROC Curve
plot(roc_obj, col = "#0073C2FF", lwd = 2, main = "ROC Curve - Test Set")

#Add AUC calue to the plot
auc_val <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "#0073C2FF", lwd = 2)

# --- STEP 4: Actual vs Predicted Bar Chart ---
#Create a data frame of Actual vs Predicted
conf_data <- data.frame(
  Actual = as.factor(Y_test),
  Predicted = as.factor(test_preds_class)
)

# Plot Actual vs Predicted as a bar chart
ggplot(conf_data, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#0073C2FF", "1" = "#EFC000FF")) +
  labs(
    title = "Actual vs Predicted Claims (Test Set)",
    x = "Actual is_claim",
    y = "Count",
    fill = "Predicted"
  ) +
  theme_minimal()




#PART OF THE MAIN CODE
# 4.1 Advanced XGBoost Classification using DMatrix
#trains the model and prints the training and test accuracy
# ============================================
# 📦 1. Load Required Libraries
# ============================================
library(xgboost)
library(caret)
library(Matrix)

# 1. Split the dataset into Train/Test sets
widthseed(42)
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_data <- train[split_index, ]
test_data  <- train[-split_index, ]

# 2. Separate features and target variable
X_train <- train_data %>% select(-is_claim)
Y_train <- train_data$is_claim
X_test  <- test_data %>% select(-is_claim)
Y_test  <- test_data$is_claim

# 3. Ensure numeric format
X_train <- X_train %>% mutate(across(everything(), as.numeric))
X_test  <- X_test %>% mutate(across(everything(), as.numeric))

# 4. Convert data to xgb.DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = Y_test)

# 5. Set XGBoost parameters
params <- list(
  objective = "binary:logistic",  # for classification
  eval_metric = "error"           # classification error
)

# 6. Train the model
model <- xgboost(
  data = dtrain,
  params = params,
  nrounds = 100,
  verbose = 0
)

# 7. Predict
train_pred <- predict(model, dtrain)
test_pred  <- predict(model, dtest)

# 8. Convert to binary class
train_pred_class <- ifelse(train_pred > 0.5, 1, 0)
test_pred_class  <- ifelse(test_pred > 0.5, 1, 0)

# 9. Accuracy
train_acc <- mean(train_pred_class == Y_train)
test_acc  <- mean(test_pred_class == Y_test)

# 10. Print results
cat("Training Accuracy:", round(train_acc, 4), "\n")
cat("Test Accuracy:", round(test_acc, 4), "\n")



#
#NOT PART OF MAIN
#
# Convert to factors
Y_test <- as.factor(Y_test)
test_pred_class <- as.factor(test_pred_class)

# Confusion matrix
conf_matrix <- confusionMatrix(test_pred_class, Y_test)
print(conf_matrix)

# Plot confusion matrix as a heatmap
library(ggplot2)
cm_table <- as.data.frame(conf_matrix$table)

ggplot(data = cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "white", high = "#0073C2FF") +
  theme_minimal() +
  labs(title = "Confusion Matrix (Test Data)")

# Print the confusion matrix (it includes Accuracy, Kappa, etc.)
print(conf_matrix)

# For binary classification, these are directly available as named elements
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1 <- conf_matrix$byClass["F1"]

# Display in one summary table
report <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score"),
  Value = c(precision, recall, f1)
)

print(report)


# ============================================
# Decision Tree Classifier. 
# ============================================
# Load required libraries
library(rpart)
library(caret)

# 1. Combine features and target into a training and test dataset
train_data <- cbind(X_train, is_claim = Y_train)
test_data <- cbind(X_test, is_claim = Y_test)

# 2. Convert target variable to factor (required by rpart for classification)
train_data$is_claim <- as.factor(train_data$is_claim)
test_data$is_claim <- as.factor(test_data$is_claim)

# 3. Train Decision Tree model
tree_model <- rpart(is_claim ~ ., data = train_data, method = "class")

# 4. Predict on training data
train_preds <- predict(tree_model, newdata = train_data, type = "class")

# 5. Predict on test data
test_preds <- predict(tree_model, newdata = test_data, type = "class")

# 6. Calculate accuracy
train_acc <- mean(train_preds == train_data$is_claim)
test_acc <- mean(test_preds == test_data$is_claim)

# 7. Print results
cat("Training Accuracy:", round(train_acc, 4), "\n")
cat("Test Accuracy:", round(test_acc, 4), "\n")

#
# ============================================
# 🚀 Decision Tree Classifier with Full Evaluation
# ============================================

library(rpart)
library(caret)
library(ggplot2)

# 1. Combine training features and target
train_data <- cbind(X_train, is_claim = Y_train)
test_data <- cbind(X_test, is_claim = Y_test)

# 2. Convert target to factor
train_data$is_claim <- as.factor(train_data$is_claim)
test_data$is_claim <- as.factor(test_data$is_claim)

# 3. Train model
tree_model <- rpart(is_claim ~ ., data = train_data, method = "class")

# 4. Predictions
train_preds <- predict(tree_model, train_data, type = "class")
test_preds  <- predict(tree_model, test_data, type = "class")

# 5. Accuracy
cat("Training Accuracy:", round(mean(train_preds == train_data$is_claim), 4), "\n")
cat("Test Accuracy:", round(mean(test_preds == test_data$is_claim), 4), "\n")

# 6. Confusion Matrix and Metrics
levels_combined <- union(levels(factor(test_preds)), levels(factor(test_data$is_claim)))
test_preds_factor <- factor(test_preds, levels = levels_combined)
actual_factor     <- factor(test_data$is_claim, levels = levels_combined)

cm <- confusionMatrix(test_preds_factor, actual_factor, positive = "1")
print(cm)

# 7. (Optional) Save or Plot Confusion Matrix
# cm_table <- as.data.frame(cm$table)
# ggplot(...)  # for plotting


# ================================================
# Decision Tree Modified with overfitting
# ================================================
library(rpart)
library(caret)
library(ggplot2)

# 1. Train the decision tree with overfitting (very deep tree, no pruning)
dtree_model_overfit <- rpart(is_claim ~ ., 
                             data = train_data_bal, 
                             method = "class",
                             control = rpart.control(cp = 0, minsplit = 2, maxdepth = 30))

# 2. Predict on test set
dtree_preds_overfit <- predict(dtree_model_overfit, newdata = test_data, type = "class")

# 3. Ensure factors have same levels
levels_combined <- union(levels(factor(dtree_preds_overfit)), levels(factor(test_data$is_claim)))
dtree_preds_factor <- factor(dtree_preds_overfit, levels = levels_combined)
actual_factor <- factor(test_data$is_claim, levels = levels_combined)

# 4. Generate confusion matrix with detailed stats
cm_overfit <- confusionMatrix(dtree_preds_factor, actual_factor, positive = "1")

# 5. Print confusion matrix and stats
print(cm_overfit)

# 6. Plot confusion matrix
#cm_table <- as.data.frame(cm_overfit$table)

#ggplot(data = cm_table, aes(x = Reference, y = Prediction)) +
  #geom_tile(aes(fill = Freq), color = "white") +
  #geom_text(aes(label = Freq), vjust = 1.5, size = 6, color = "white") +
  #scale_fill_gradient(low = "skyblue", high = "steelblue") +
  #labs(title = "Confusion Matrix: Overfitted Decision Tree",
       #x = "Actual Label",
       #y = "Predicted Label") +
  #theme_minimal()



library(caret)
library(randomForest)
library(ggplot2)

set.seed(42)  # reproducibility

# 1. Split train into train and validation (70% / 30%)
train_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_data <- train[train_index, ]
validation_data <- train[-train_index, ]

# 2. Separate features and target, exclude 'policy_id'
X_train <- train_data[, !(names(train_data) %in% c("is_claim", "policy_id"))]
Y_train <- as.factor(train_data$is_claim)

X_val <- validation_data[, !(names(validation_data) %in% c("is_claim", "policy_id"))]
Y_val <- as.factor(validation_data$is_claim)

# 3. Train Random Forest model
set.seed(132)
rf_model <- randomForest(x = X_train, y = Y_train, ntree = 100)

# 4. Predict on validation set
val_pred <- predict(rf_model, X_val)

# 5. Validation accuracy
cat("Validation Accuracy:", mean(val_pred == Y_val), "\n")

# 6. Confusion matrix and metrics
conf_mat <- confusionMatrix(val_pred, Y_val, positive = "1")  # Assuming "1" is the positive class label
print(conf_mat)

# 7. Plot confusion matrix heatmap
cm_df <- as.data.frame(conf_mat$table)
colnames(cm_df) <- c("Prediction", "Reference", "Freq")

ggplot(data = cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Confusion Matrix: Random Forest Validation") +
  theme_minimal()

library(caret)
library(MLmetrics)

# Assuming Y_train, Y_test, train_pred, test_pred already defined as factors

# Calculate accuracies
acc_train <- mean(train_pred == Y_train)
acc_test  <- mean(test_pred == Y_test)

# Calculate recall and precision for test set (positive class assumed "1")
recall_test <- Recall(y_pred = test_pred, y_true = Y_test, positive = "1")
precision_test <- Precision(y_pred = test_pred, y_true = Y_test, positive = "1")

cat("Random Classification model's metrics:\n\n")
cat("Accuracy on Training Data:", round(acc_train, 2), "\n")
cat("Accuracy on Test Data:", round(acc_test, 2), "\n")
cat("Recall Score:", round(recall_test, 2), "\n")
cat("Precision Score:", round(precision_test, 2), "\n")




library(randomForest)
library(rpart)
library(caret)

set.seed(42)

# Sample train/test split
train_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_data <- train[train_index, ]
validation_data <- train[-train_index, ]

# Separate features and labels
X_train <- train_data[, !(names(train_data) %in% c("is_claim", "policy_id"))]
Y_train <- as.factor(train_data$is_claim)

X_val <- validation_data[, !(names(validation_data) %in% c("is_claim", "policy_id"))]
Y_val <- as.factor(validation_data$is_claim)

# Train Random Forest model
rf_model <- randomForest(x = X_train, y = Y_train, ntree = 100)

# Train Decision Tree model
dt_model <- rpart(is_claim ~ ., data = train_data[, !(names(train_data) %in% c("policy_id"))], method = "class")

# Predict with both models on validation data
rf_pred <- predict(rf_model, X_val)
dt_pred <- predict(dt_model, X_val, type = "class")

# Combine predictions by majority voting
# Convert factors to characters to avoid factor level issues
combined_preds <- data.frame(rf = as.character(rf_pred), dt = as.character(dt_pred), stringsAsFactors = FALSE)

# Simple majority voting function
majority_vote <- function(x) {
  votes <- table(x)
  names(which.max(votes))
}

ensemble_pred <- apply(combined_preds, 1, majority_vote)
ensemble_pred <- factor(ensemble_pred, levels = levels(Y_val))

# Evaluate ensemble
conf_mat <- confusionMatrix(ensemble_pred, Y_val, positive = "1")
print(conf_mat)

cat("Ensemble Accuracy:", mean(ensemble_pred == Y_val), "\n")
