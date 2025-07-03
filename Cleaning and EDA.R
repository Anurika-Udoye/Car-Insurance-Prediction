# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load your data
train <- read.csv("train.csv")

# Structure of the dataset 
str(train)

# Shape of the dataset (rows and columns)
cat("Rows:", nrow(train), "\n")
cat("Columns:", ncol(train), "\n")

# Column data types
sapply(train, class)

# Identify character columns (object types)
object_cols <- names(train)[sapply(train, is.character)]
print("Object columns:")
print(object_cols)

# Summary of missing values
missing_vals <- sapply(train, function(x) sum(is.na(x)))
missing_vals <- missing_vals[missing_vals > 0]
print("Missing values:")
print(missing_vals)

# Count and plot pie chart of 'is_claim'
# Prepare data: count values of is_claim
is_claim_counts <- train %>%
  count(is_claim) %>%
  mutate(prop = n / sum(n),
         label = paste0(round(prop * 100, 2), "%"))

# Pie chart using ggplot2
ggplot(is_claim_counts, aes(x = "", y = prop, fill = as.factor(is_claim))) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(fill = "is_claim", title = "Proportion of Claims") +
  theme_void()

#categorical Columns
cat_cols <- c(
 "area_cluster", "segment", "model", "fuel_type",
  "max_torque", "max_power", "engine_type", "rear_brakes_type",
  "transmission_type", "steering_type"
)

# Get numeric columns (excluding is_claim)
numeric_cols <- train %>%
  select(where(is.numeric)) %>%
  select(-is_claim) %>%
  names()

print(numeric_cols)

# Check numeric binary columns
binary_num_cols <- train %>%
  select(where(is.numeric)) %>%
  select(-is_claim) %>%
  select(where(~ all(unique(.) %in% c(0,1)))) %>%
  names()

print("Binary Numeric Columns:")
print(binary_num_cols)

#UNIVARIANT ANALYSIS
library(ggplot2)
library(dplyr)

# Get numeric columns (excluding target)
numeric_cols <- train %>% select(where(is.numeric)) %>% select(-is_claim)

# Summary statistics
summary(numeric_cols)

# Histograms for numeric features
for (col in names(numeric_cols)) {
  print(
    ggplot(train, aes_string(x = col)) +
      geom_histogram(fill = "steelblue", bins = 30, color = "black") +
      labs(title = paste("Histogram of", col), x = col, y = "Count") +
      theme_minimal()
  )
}

# Get categorical columns (excluding policy_id)
cat_cols <- train %>% select(where(character)) %>% select(-policy_id)

# Frequency plots
for (col in names(cat_cols)) {
  print(
    ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "tomato") +
      labs(title = paste("Bar Plot of", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# Assuming binary columns are either character with Yes/No or numeric 0/1

# Binary categorical columns
binary_cat_cols <- names(cat_cols)[sapply(cat_cols, function(x) n_distinct(x) == 2)]

# Plot each binary categorical column
for (col in binary_cat_cols) {
  print(
    ggplot(train, aes_string(x = col)) +
      geom_bar(fill = "darkgreen") +
      labs(title = paste("Bar Plot of", col), x = col, y = "Count") +
      theme_minimal()
  )
}



#Bivriate Analysis


# Top relevant numeric variables (feel free to edit)
numeric_vars <- c("policy_tenure", "age_of_car", "age_of_policyholder", 
                  "population_density", "displacement")

# Top relevant categorical variables
cat_vars <- c("area_cluster", "segment", "fuel_type", "engine_type", "steering_type")


# -----------------------------
# B. Bivariate Analysis with is_claim
# -----------------------------

# 1. Numeric Variables vs is_claim
for (var in numeric_vars) {
  print(
    ggplot(train, aes_string(x = "factor(is_claim)", y = var)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = paste(var, "vs is_claim"), x = "is_claim", y = var) +
      theme_minimal()
  )
}

# 2. Categorical Variables vs is_claim (Claim Rate Plot)
for (var in cat_vars) {
  claim_rate_df <- train %>%
    group_by(!!sym(var)) %>%
    summarise(Claim_Rate = mean(is_claim), Count = n()) %>%
    arrange(desc(Claim_Rate))
  
  print(
    ggplot(claim_rate_df, aes_string(x = var, y = "Claim_Rate")) +
      geom_bar(stat = "identity", fill = "tomato") +
      labs(title = paste("Claim Rate by", var), x = var, y = "Claim Rate") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}
