library(dplyr)
all_data <- read.csv("KaggleData.csv")

#------------ Code section for first round processing, eg age category

all_data_clean <- all_data %>%
  # Remove rows without IDs
  filter(!is.na(PassengerId)) %>%
  # Model can handle NA, not empty string
  mutate(Embarked = na_if(Embarked, "")) %>%
  filter(!is.na(Embarked)) %>%
  # Create the categorical age column
  mutate(AgeGroup = case_when(
    is.na(Age) ~ "Missing",
    Age <= 12  ~ "Child",
    Age <= 19  ~ "Teen",
    Age <= 30  ~ "Young Adult",
    Age <= 55  ~ "Adult",
    TRUE       ~ "Senior"
  )) %>%
  # Ensure it is treated as a factor for modeling
  mutate(AgeGroup = as.factor(AgeGroup)) %>%
  mutate(Embarked = as.factor(Embarked)) # Easiest handling of 2 empty entries

# 2. CHECK DISTRIBUTION (This will work now!)
table(all_data_clean$AgeGroup)


#---------------------splitting data to train&validate vs test
# randomize, split
# set seed to get same result
set.seed(2)

# Create a randomized index
n <- nrow(all_data_clean)
shuffled_indices <- sample(n) # Randomly reorders row numbers 1 through n
# Reorder the data based on the shuffled indices
all_data_shuffled <- all_data_clean[shuffled_indices, ]

# ASSIGN TO SETS
# Use folds, section out 15% for test
test_prop <- 0.15
train_val_prop <- (1 - test_prop)
train_val_idx <- floor(train_val_prop * n)

# Use slice to create the data frames
train_val_set <- all_data_shuffled %>% slice(1:train_val_idx)
test_set   <- all_data_shuffled %>% slice((train_val_idx + 1):n)


#------------Split into folds, fill missing fare, log fare, build model per fold
set.seed(3)
k <- 5

# Assign Fold IDs (1 to k) to the train_val_set
train_val_set <- train_val_set %>%
  mutate(fold_id = sample(rep(1:k, length.out = n())))

# Verify that the folds are roughly equal in size
table(train_val_set$fold_id)

# Create a list or vector to store the results of each fold
fold_accuracies <- numeric(k)
# Create a list to store the significant factors for each fold
fold_significant_vars <- list()
predictor_vars_list <- c("Pclass", "Sex", "AgeGroup", "SibSp", "Parch", "LogFare", "Embarked")
# Removed factors like id etc

for (i in 1:k) {
  cat("\nProcessing Fold:", i, "\n")
  
  # --- A. Split into Fold-Specific Train and Validation ---
  # Train on all folds EXCEPT 'i', Validate on fold 'i'
  fold_train <- train_val_set %>% filter(fold_id != i)
  fold_val   <- train_val_set %>% filter(fold_id == i)
  
  # --- B. Data Cleaning / Feature Engineering (Within the Fold) ---
  # 1. Calculate median fare from Training only to avoid data leakage
  # Verified, appears close to global median
  median_fare <- median(fold_train$Fare, na.rm = TRUE)
  
  # 2. Fill missing fare and create Log Fare for both sets
  fold_train <- fold_train %>%
    mutate(Fare = if_else(Fare == 0, median_fare, Fare),
           LogFare = log(Fare)) # generally add +1 for 0 value
  
  fold_val <- fold_val %>%
    mutate(Fare = if_else(Fare == 0, median_fare, Fare),
           LogFare = log(Fare))
  
  # --- C. Model Training & Evaluation ---
  # Create input to model in desired format
  formula_string <- paste("Survived ~", paste(predictor_vars_list, collapse = " + "))
  model_formula <- as.formula(formula_string)
  
  # 1. Fit full Logistic Regression
  full_model <- glm(model_formula, data = fold_train, family = "binomial")
  
  # Summarize factor significance
  # Extract coefficients and p-values
  model_summary <- as.data.frame(summary(full_model)$coefficients)
  # Identify significant factors (p < 0.05)
  significant_factors <- rownames(model_summary)[model_summary$`Pr(>|z|)` < 0.05]
  # Clean the factor names for reporting (remove factor's name)
  unique_sig_vars <- unique(gsub("Child|Teen|Young Adult|Adult|Senior|male|female|", "", significant_factors))
  unique_sig_vars <- unique_sig_vars[unique_sig_vars != "(Intercept)"]
  
  print("The following variables showed statistical significance:")
  print(unique_sig_vars)
  print(summary(full_model)) # Has p-vals, and which sub-factors significant
  
  # 2. Generate Predictions on the Validation Fold
  # 'type = "response"' returns probabilities between 0 and 1
  prob_preds <- predict(full_model, newdata = fold_val, type = "response")
  
  # 3. Convert probabilities to binary classification (Threshold = 0.5)
  class_preds <- if_else(prob_preds > 0.5, 1, 0)
  
  # 4. Calculate Accuracy for this fold
  fold_accuracies[i] <- mean(class_preds == fold_val$Survived)
  
  # Store results (placeholder for your logic)
  # fold_accuracies[i] <- calculate_metric_function(model, fold_val)
}

# View average performance across all folds
# mean(fold_accuracies)

#----------------------------------Model v1
final_median_fare <- median(train_val_set$Fare, na.rm = TRUE)

train_val_final <- train_val_set %>%
  mutate(Fare = if_else(Fare == 0, final_median_fare, Fare),
         LogFare = log(Fare)) # generally add +1 for 0 value

predictor_vars_list_definitive <- c("Pclass", "Sex", "AgeGroup", "SibSp")
formula_definitive <- as.formula(paste("Survived ~", paste(predictor_vars_list_definitive, collapse = " + ")))
model_definitive <- glm(formula_definitive, data = train_val_final, family = "binomial")
summary(model_definitive)
