# Data manipulation and visualization
library(tidyverse)      # Includes ggplot2, dplyr, etc.
library(data.table)     # For fast data reading and handling
library(caret)          # For train/test splitting, modeling, evaluation

install.packages("lava")             # For structural equation modeling, if needed
# Suppress warning messages
options(warn = -1)

# Show all columns when printing data frames
options(dplyr.width = Inf)
# Set minimal theme for all ggplot plots
theme_set(theme_minimal())

# Set default point color in plots to black
update_geom_defaults("point", list(colour = "black"))
library(readr)

# Set working directory (adjust path if necessary)
setwd("/Users/Anurika/Desktop/AIS Casestudy/Predictive Analytics Project Pack")

# Confirm current working directory
getwd()
library(skimr)    # For data summary
library(dplyr)    # For data wrangling


install.packages("tidyr")

# ================================
# Cleaning Data
# ================================
library(dplyr)
library(tidyr)
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

str(train)
library(purrr)

# 7. Summary table: column type and count of missing values
data.frame(
  Column = names(train),
  Class = map_chr(train, ~ class(.x)[1]),
  NonMissing = map_int(train, ~ sum(!is.na(.x))),
  Total = nrow(train)
)


# ================================
# Encoding the data
# ================================
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

# Apply to test
test <- test %>%
  mutate(across(all_of(binary_cols), ~ ifelse(. == "Yes", 1, 0)))


#test <- read.csv("test.csv")
#train <- read.csv("train.csv")



# ================================
# Exploratory Data Analysis
# ================================
# Install and load libraries
install.packages("tidyverse")
install.packages("corrplot")

library(tidyverse)
library(corrplot)

# ==== Load your dataset here ====
# train <- read_csv("path/to/train.csv")
# === Structure and summary ===
str(train)
summary(train)
glimpse(train)

# === Target variable distribution ===
cat("Target distribution:\n")
print(table(train$is_claim))
print(prop.table(table(train$is_claim)))

ggplot(train, aes(x = factor(is_claim))) +
  geom_bar(fill = "skyblue") +
  labs(x = "Is Claim", y = "Count", title = "Distribution of Claim Status")

# === Numerical Variables ===
num_cols <- train %>% select(where(is.numeric)) %>% names()

# Summary stats
train %>%
  select(all_of(num_cols)) %>%
  summary()

# One histogram per numerical feature
for (col in num_cols) {
  if (col %in% names(train) && !all(is.na(train[[col]]))) {
    p <- ggplot(train, aes_string(x = col)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = paste("Distribution of", col), x = col, y = "Count") +
      theme_minimal()
    print(p)
  } else {
    cat("Skipped numeric column:", col, "\n")
  }
}

# Boxplot by claim status
for (col in num_cols) {
  if (col %in% names(train) && !all(is.na(train[[col]]))) {
    p <- ggplot(train, aes_string(x = "factor(is_claim)", y = col, fill = "factor(is_claim)")) +
      geom_boxplot() +
      labs(title = paste(col, "by Claim Status"), x = "Is Claim") +
      theme_minimal()
    print(p)
  } else {
    cat("Skipped numeric boxplot for:", col, "\n")
  }
}

# === Categorical Variables ===
cat_cols <- train %>%
  select(where(is.factor)) %>%
  select(-is_claim) %>%
  names()

# Frequency tables
cat("Categorical frequencies:\n")
print(lapply(train[cat_cols], table))

# Bar plots
for (col in cat_cols) {
  if (col %in% names(train) && !all(is.na(train[[col]]))) {
    p <- ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "coral") +
      labs(title = paste("Distribution of", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  } else {
    cat("Skipped categorical bar plot for:", col, "\n")
  }
}

# Stacked bar (proportion) by claim
for (col in cat_cols) {
  if (col %in% names(train) && !all(is.na(train[[col]]))) {
    p <- ggplot(train, aes_string(x = col, fill = "factor(is_claim)")) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste("Claim Rate by", col), x = col, y = "Proportion") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  } else {
    cat("Skipped stacked plot for:", col, "\n")
  }
}

# === Correlation Plot ===
if (length(num_cols) > 1) {
  cor_matrix <- cor(train[num_cols], use = "complete.obs")
  corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8)
} else {
  cat("Not enough numeric columns for correlation plot.\n")
}




# ================================
# Correlation
# ================================
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
test  <- extract_power_torque_ratios(test)

# ==== ONE-HOT ENCODING ====
library(fastDummies)
library(dplyr)

# 1. Add a column to distinguish train/test
train$data_split <- "train"
test$data_split <- "test"

# 2. Bind rows together
combined_data <- bind_rows(train, test)

# 3. Create dummy variables
combined_data <- dummy_cols(
  combined_data,
  select_columns = c("model", "segment", "fuel_type", "engine_type",
                     "transmission_type", "steering_type", "rear_brakes_type", "make", "area_cluster"),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

# 4. Split back into train and test
train <- combined_data %>% filter(data_split == "train") %>% select(-data_split)
test  <- combined_data %>% filter(data_split == "test")  %>% select(-data_split)


#Drop only 'policy_id, lenght, width, heght,  column
cols_to_drop <- c("policy_id", "width", "height", "length")

train <- train %>% select(-all_of(cols_to_drop))
test  <- test  %>% select(-all_of(cols_to_drop))


#Scaling
library(dplyr)

# Columns to scale
cols_to_scale <- c("policy_tenure", "age_of_car", "age_of_policyholder", "population_density",
                   "displacement", "cylinder", "gear_box", "turning_radius", "gross_weight", 
                   "torque_to_rpm_ratio","power_to_rpm_ratio", "airbags", "volume", "ncap_rating")

# Step 1: Compute mean and sd from train only
scaling_params <- train %>%
  summarise(across(all_of(cols_to_scale), list(mean = ~mean(., na.rm = TRUE),
                                               sd = ~sd(., na.rm = TRUE))))

# Step 2: Define z-score scaling function
scale_with_zscore <- function(df, params) {
  for (col in cols_to_scale) {
    mean_val <- params[[paste0(col, "_mean")]]
    sd_val   <- params[[paste0(col, "_sd")]]
    df[[col]] <- (df[[col]] - mean_val) / sd_val
  }
  return(df)
}

# Step 3: Apply to both train and test
train <- scale_with_zscore(train, scaling_params)
test  <- scale_with_zscore(test, scaling_params)



library(dplyr)
library(ggplot2)

# 1. Identify binary columns (0/1 only)
binary_cols <- train %>%
  select(where(is.numeric)) %>%
  select(-is_claim) %>%
  select_if(~ all(na.omit(.) %in% c(0, 1))) %>%
  names()

# 2. Select numeric columns excluding binary and target
numeric_cols <- train %>%
  select(where(is.numeric)) %>%
  select(-is_claim, -one_of(binary_cols))

# 3. Perform PCA using correlation matrix (standardized data)
pca_result <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)

# 4. Scree Plot: Variance explained by each PC
scree_data <- data.frame(
  PC = paste0("PC", 1:length(pca_result$sdev)),
  Variance = (pca_result$sdev)^2 / sum((pca_result$sdev)^2)
)

ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_col(fill = "#00BFC4") +
  geom_text(aes(label = round(Variance, 2)), vjust = -0.5, size = 3) +
  labs(title = "PCA - Variance Explained by Each Component",
       y = "Proportion of Variance Explained", x = "Principal Component") +
  theme_minimal()

# 5. Loadings (rotation matrix shows variable contributions)
loadings <- pca_result$rotation

# ---- TOP CONTRIBUTORS ----

# Top 10 contributing variables for PC1
top10_PC1 <- sort(abs(loadings[, 1]), decreasing = TRUE)[1:10]
top10_PC1_df <- data.frame(
  Variable = names(top10_PC1),
  Contribution = loadings[names(top10_PC1), 1]
)

# Top 10 contributing variables for PC2
top10_PC2 <- sort(abs(loadings[, 2]), decreasing = TRUE)[1:10]
top10_PC2_df <- data.frame(
  Variable = names(top10_PC2),
  Contribution = loadings[names(top10_PC2), 2]
)

cat("🔹 Top 10 PC1 Contributors:\n"); print(top10_PC1_df)
cat("\n🔹 Top 10 PC2 Contributors:\n"); print(top10_PC2_df)

# ---- FILTERED VARIABLES ----
# Combine PC1 + PC2 contributions and filter high influencers
var_df <- data.frame(
  Variable = rownames(loadings),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2]
)

var_df$Total_Contribution_PC1_PC2 <- rowSums(abs(var_df[, c("PC1", "PC2")]))

# Filter based on mean contribution
filtered_vars <- var_df %>%
  filter(Total_Contribution_PC1_PC2 > mean(Total_Contribution_PC1_PC2)) %>%
  arrange(desc(Total_Contribution_PC1_PC2))

cat("\n🔍 Filtered Variables (High Total Contribution to PC1 + PC2):\n")
print(filtered_vars)



#========================
#LOGISTIC REGRESSSION#
#========================

# 1️⃣ Split Data into Train/Test
set.seed(123)
split_index <- createDataPartition(train$is_claim, p = 0.8, list = FALSE)
train_data <- train[split_index, ]
test_data  <- train[-split_index, ]

X_train <- train_data %>% select(-is_claim)
Y_train <- train_data$is_claim
X_test  <- test_data %>% select(-is_claim)
Y_test  <- test_data$is_claim

# Ensure numeric (SMOTE requires it)
X_train <- X_train %>% mutate(across(everything(), as.numeric))
X_test  <- X_test %>% mutate(across(everything(), as.numeric))


# 3️⃣ Apply SMOTE to Training Set
# Ensure target is numeric
Y_train <- as.numeric(as.character(Y_train))

# Combine for SMOTE
train_smote <- data.frame(X_train, class = Y_train)

# Apply SMOTE
smote_result <- SMOTE(X = train_smote[, -ncol(train_smote)],
                      target = train_smote$class,
                      K = 5)

# Extract balanced data
X_train_bal <- smote_result$data[, -ncol(smote_result$data)]
Y_train_bal <- smote_result$data$class

# Impute missing values
preproc <- preProcess(X_train_bal, method = "medianImpute")
X_train_bal <- predict(preproc, X_train_bal)

# Convert to data frame and set column names
X_train_bal <- as.data.frame(X_train_bal)
colnames(X_train_bal) <- colnames(X_train)


#(Optional) Visualize Class Balance

Y_train_bal <- as.factor(Y_train_bal)

ggplot(data.frame(class = Y_train_bal), aes(x = class)) +
  geom_bar(fill = c("#0073C2FF", "#EFC000FF")) +
  labs(title = "Balanced Class Distribution After SMOTE",
       x = "Is Claim (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

cat("🔸 Class distribution BEFORE SMOTE:\n")
print(table(Y_train))
cat("\n🔹 Class distribution AFTER SMOTE:\n")
print(table(Y_train_bal))



# 4 Logistic regression
library(caret)
library(dplyr)
library(smotefamily)
library(ggplot2)
library(pROC)
library(reshape2)
train_data_bal <- cbind(X_train_bal, is_claim = Y_train_bal)
test_data <- cbind(X_test, is_claim = Y_test)

train_data_bal$is_claim <- as.factor(train_data_bal$is_claim)
test_data$is_claim <- as.factor(test_data$is_claim)

log_model <- glm(is_claim ~ ., data = train_data_bal, family = "binomial")

# ================================
# 5️⃣ Predictions
# ================================
train_preds <- predict(log_model, newdata = train_data_bal, type = "response")
train_preds_class <- ifelse(train_preds > 0.5, 1, 0)

test_preds <- predict(log_model, newdata = test_data, type = "response")
test_preds_class <- ifelse(test_preds > 0.5, 1, 0)

# 6️⃣ Evaluation Metrics
train_acc <- mean(train_preds_class == as.numeric(as.character(train_data_bal$is_claim)))
test_acc <- mean(test_preds_class == as.numeric(as.character(test_data$is_claim)))

cat("Training Accuracy:", round(train_acc, 4), "\n")
cat("Test Accuracy:", round(test_acc, 4), "\n")

conf_matrix <- confusionMatrix(as.factor(test_preds_class), test_data$is_claim, positive = "1")
print(conf_matrix)

precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1 <- conf_matrix$byClass["F1"]
accuracy <- mean(test_preds_class == as.numeric(as.character(Y_test)))

report <- data.frame(
  Metric = c("Precision", "Recall", "F1 Score", "Accuracy"),
  Value = c(precision, recall, f1, accuracy)
)

print(report)

# 7️⃣ ROC Curve
roc_obj <- roc(Y_test, test_preds)
plot(roc_obj, col = "#0073C2FF", lwd = 2, main = "ROC Curve - Test Set")
auc_val <- auc(roc_obj)
legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "#0073C2FF", lwd = 2)

# 8️⃣ Actual vs Predicted Plot
conf_data <- data.frame(
  Actual = as.factor(Y_test),
  Predicted = as.factor(test_preds_class)
)

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

# 9️⃣ Confusion Matrix Plot
cm_table <- table(Actual = Y_test, Predicted = test_preds_class)
cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Actual", "Predicted", "Freq")

ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6, color = "black") +
  scale_fill_gradient(low = "#E0F7FA", high = "#006064") +
  labs(title = "Confusion Matrix - Logistic Regression",
       x = "Predicted Class",
       y = "Actual Class") +
  theme_minimal()

# ================================
# Xgboost
# ================================
# 📦 Load Required Libraries
library(xgboost)
library(caret)
library(pROC)
library(dplyr)
library(Matrix)
library(ggplot2)

# 1️⃣ Split the Data
set.seed(123)
split_index <- createDataPartition(train$is_claim, p = 0.8, list = FALSE)
train_data <- train[split_index, ]
test_data  <- train[-split_index, ]

# 2️⃣ Separate Features and Target
X_train <- train_data %>% select(-is_claim)
Y_train <- train_data$is_claim
X_test  <- test_data %>% select(-is_claim)
Y_test  <- test_data$is_claim

# 3️⃣ Ensure Features are Numeric
X_train <- X_train %>% mutate(across(everything(), as.numeric))
X_test  <- X_test %>% mutate(across(everything(), as.numeric))

# 4️⃣ Create DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = Y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = Y_test)

# 5️⃣ Calculate Scale_Pos_Weight
scale_pos_weight <- sum(Y_train == 0) / sum(Y_train == 1)

# 6️⃣ Define Parameters (Tuned)
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  scale_pos_weight = scale_pos_weight,
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 7️⃣ Train the Model with Early Stopping
watchlist <- list(train = dtrain, eval = dtest)
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  watchlist = watchlist,
  early_stopping_rounds = 10,
  print_every_n = 10
)

# 8️⃣ Predict Probabilities
test_pred_probs <- predict(model, dtest)

# 9️⃣ Try a Lower Threshold (e.g. 0.3)
threshold <- 0.5
test_pred_class <- ifelse(test_pred_probs > threshold, 1, 0)

# 🔟 Confusion Matrix
conf_matrix <- confusionMatrix(
  factor(test_pred_class, levels = c(0, 1)),
  factor(Y_test, levels = c(0, 1)),
  positive = "1"
)

# 🔁 ROC Curve & AUC
roc_obj <- roc(Y_test, test_pred_probs)
auc_val <- auc(roc_obj)

# 📈 Plot ROC
plot(roc_obj, col = "darkgreen", lwd = 2, main = paste("ROC Curve (XGBoost, thresh=", threshold, ")"))
legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "darkgreen", lwd = 2)

# 📊 Actual vs Predicted
ggplot(data.frame(Actual = as.factor(Y_test), Predicted = as.factor(test_pred_class)), 
       aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#0073C2FF", "1" = "#EFC000FF")) +
  labs(title = paste("Actual vs Predicted (Threshold =", threshold, ")"),
       x = "Actual is_claim", y = "Count", fill = "Predicted") +
  theme_minimal()

# 📋 Print Metrics
cat("\nConfusion Matrix and Statistics\n\n")
print(conf_matrix)

# 📌 Performance Report
report <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"),
  Value = c(
    mean(test_pred_class == Y_test),
    conf_matrix$byClass["Precision"],
    conf_matrix$byClass["Recall"],
    conf_matrix$by_))
    
    
  
    
    
    
# ================================
# Decision Tree
# ================================#


# Set seed and split data
set.seed(123)
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_set <- train[split_index, ]
val_set <- train[-split_index, ]

# Load required libraries
library(rpart)
library(caret)
library(rpart.plot)
library(pROC)

# 📌 1. Default Decision Tree
default.ct <- rpart(is_claim ~ ., data = train_set, method = "class")
rpart.plot(default.ct)

# Predict and evaluate on validation set
pred_default <- predict(default.ct, val_set, type = "class")
conf_default <- confusionMatrix(pred_default, as.factor(val_set$is_claim), positive = "1")
print(conf_default)

# 📌 2. Deeper Tree (Overfitting model)
deeper.ct <- rpart(is_claim ~ ., data = train_set, method = "class",
                   control = rpart.control(cp = 0.0001, minsplit = 2))
rpart.plot(deeper.ct)

# Evaluate on training set
pred_deeper <- predict(deeper.ct, train_set, type = "class")
conf_deeper <- confusionMatrix(pred_deeper, as.factor(train_set$is_claim), positive = "1")
print(conf_deeper)

# 📌 3. Cross-Validation + Pruned Tree
printcp(default.ct)  # Check cross-validation cp table
plotcp(default.ct)   # Optional: Visualize to select cp

# Prune tree (you can adjust cp here if needed)
pruned.ct <- prune(default.ct, cp = 0.01)
rpart.plot(pruned.ct)

# Evaluate on validation set
pred_pruned <- predict(pruned.ct, val_set, type = "class")
conf_pruned <- confusionMatrix(pred_pruned, as.factor(val_set$is_claim), positive = "1")
print(conf_pruned)

# 📈 ROC & AUC Evaluation
# Default tree
roc_default <- roc(val_set$is_claim, predict(default.ct, val_set, type = "prob")[,2])
plot(roc_default, main = "ROC - Default Tree")
auc_default <- auc(roc_default)
cat("AUC - Default Tree:", auc_default, "\n")

# Pruned tree
roc_pruned <- roc(val_set$is_claim, predict(pruned.ct, val_set, type = "prob")[,2])
plot(roc_pruned, main = "ROC - Pruned Tree", col = "blue")
auc_pruned <- auc(roc_pruned)
cat("AUC - Pruned Tree:", auc_pruned, "\n")







# Load required libraries
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# Load data
train <- read.csv("train.csv")

# 1. Convert character columns to factors
train <- train %>%
  mutate(across(where(is.character), as.factor))

# 2. Convert target variable to factor
train$is_claim <- as.factor(train$is_claim)

# 3. Drop unwanted columns
drop_cols <- c("policy_id", "height", "width", "length")
train <- train %>% select(-all_of(drop_cols))

# 4. Check class imbalance
print(prop.table(table(train$is_claim)))

# 5. Stratified split (70% train, 30% validation)
set.seed(123)
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_set <- train[split_index, ]
val_set   <- train[-split_index, ]

# 6. Upsample minority class in training set
set.seed(123)
train_balanced <- upSample(x = train_set %>% select(-is_claim),
                           y = train_set$is_claim,
                           yname = "is_claim")

# Confirm new class balance
print(prop.table(table(train_balanced$is_claim)))

# ------------------ Default Tree ------------------ #
default.ct <- rpart(is_claim ~ ., data = train_balanced, method = "class")
rpart.plot(default.ct, main = "Default Tree")

# Predict & Evaluate on Validation Set
pred_default <- predict(default.ct, val_set, type = "class")
conf_default <- confusionMatrix(pred_default, val_set$is_claim, positive = "1")
print(conf_default)

# ------------------ Deeper Tree ------------------ #
deeper.ct <- rpart(is_claim ~ ., data = train_balanced, method = "class",
                   control = rpart.control(cp = 0.0001, minsplit = 2))
rpart.plot(deeper.ct, main = "Deeper Tree")

# Evaluate on Training Set
pred_deeper <- predict(deeper.ct, train_balanced, type = "class")
conf_deeper <- confusionMatrix(pred_deeper, train_balanced$is_claim, positive = "1")
print(conf_deeper)

# ------------------ Pruned Tree ------------------ #
printcp(default.ct)
plotcp(default.ct)

best_cp <- default.ct$cptable[which.min(default.ct$cptable[, "xerror"]), "CP"]
pruned.ct <- prune(default.ct, cp = best_cp)
rpart.plot(pruned.ct, main = "Pruned Tree")

# Predict & Evaluate on Validation Set
pred_pruned <- predict(pruned.ct, val_set, type = "class")
conf_pruned <- confusionMatrix(pred_pruned, val_set$is_claim, positive = "1")
print(conf_pruned)



library(rpart)

# Fit the model
default.ct <- rpart(is_claim ~ ., data = train_balanced, method = "class")
library(caret)

importance <- varImp(default.ct)
print(importance)
# Convert to data frame
importance_df <- as.data.frame(importance)

# Add feature names
importance_df$Feature <- rownames(importance_df)

# Sort by importance
top_features <- importance_df %>%
  arrange(desc(Overall)) %>%
  slice(1:10)

print(top_features)


# Load libraries
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# Load your dataset
train <- read.csv("train.csv")

# Drop unwanted columns
train <- train %>% select(-policy_id, -height, -width, -length)

# Convert character columns to factors
train <- train %>%
  mutate(across(where(is.character), as.factor))

# Ensure target variable is a factor
train$is_claim <- as.factor(train$is_claim)

# Drop factor levels not present in both train/test
train <- droplevels(train)

# Stratified split
set.seed(123)
split_index <- createDataPartition(train$is_claim, p = 0.7, list = FALSE)
train_set <- train[split_index, ]
val_set   <- train[-split_index, ]

# OPTIONAL: Use ROSE to balance the training set (if you're not using SMOTE)
# install.packages("ROSE")
library(ROSE)
train_balanced <- ROSE(is_claim ~ ., data = train_set, seed = 123)$data

# Select only top important features based on previous result
top_features <- c("policy_tenure", "age_of_car", "area_cluster", 
                  "engine_type", "max_power", "max_torque", 
                  "model", "age_of_policyholder", "population_density")

# Subset training and validation set
train_top <- train_balanced %>% select(all_of(top_features), is_claim)
val_top   <- val_set %>% select(all_of(top_features), is_claim)

# Train a decision tree with top features
tree_top <- rpart(is_claim ~ ., data = train_top, method = "class")
rpart.plot(tree_top, main = "Decision Tree (Top Features)")

# Predict on validation set
pred_top <- predict(tree_top, val_top, type = "class")

# Evaluate
conf_top <- confusionMatrix(pred_top, val_top$is_claim, positive = "1")
print(conf_top)
