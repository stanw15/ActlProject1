library(dplyr)
all_data <- read.csv("KaggleData.csv")

#------------ Code section for processing, splitting data to train&validate vs test
# 1 PRE-SPLIT CLEANING
# Remove specific entries you don't want
all_data_clean <- all_data %>%
  filter(!is.na(PassengerId)) # Example: remove rows without IDs

# 2 randomize, split
# set seed to get same result
set.seed(2)

# Create a randomized index
n <- nrow(all_data_clean)
shuffled_indices <- sample(n) # Randomly reorders row numbers 1 through n
# Reorder the data based on the shuffled indices
all_data_shuffled <- all_data_clean[shuffled_indices, ]

# 3. ASSIGN TO SETS
# Use folds, section out 15% for test
test_prop <- 0.15
train_val_prop <- (1 - test_prop)
train_val_idx <- floor(train_val_prop * n)

# Use slice to create the data frames
train_val_set <- all_data_shuffled %>% slice(1:train_val_idx)
test_set   <- all_data_shuffled %>% slice((train_val_idx + 1):n)


#------------Split into folds and fill missing fare, then convert to log fare
set.seed(3)
k <- 5

# Assign Fold IDs (1 to k) to the train_val_set
train_val_set <- train_val_set %>%
  mutate(fold_id = sample(rep(1:k, length.out = n())))

# Verify that the folds are roughly equal in size
table(train_val_set$fold_id)

# Create a list or vector to store the results of each fold
fold_accuracies <- numeric(k)

for (i in 1:k) {
  cat("\nProcessing Fold:", i, "\n")
  
  # --- A. Split into Fold-Specific Train and Validation ---
  # Train on all folds EXCEPT 'i', Validate on fold 'i'
  fold_train <- train_val_set %>% filter(fold_id != i)
  fold_val   <- train_val_set %>% filter(fold_id == i)
  
  # --- B. Data Cleaning / Feature Engineering (Within the Fold) ---
  # 1. Calculate median fare from Training only to avoid data leakage
  median_fare <- median(fold_train$Fare, na.rm = TRUE)
  
  # 2. Fill missing fare and create Log Fare for both sets
  fold_train <- fold_train %>%
    mutate(Fare = if_else(is.na(Fare), median_fare, Fare),
           LogFare = log(Fare + 1)) # +1 to handle 0 values
  
  fold_val <- fold_val %>%
    mutate(Fare = if_else(is.na(Fare), median_fare, Fare),
           LogFare = log(Fare + 1))
  
  # --- C. Model Training & Evaluation ---
  # Example: Fit a simple linear model (replace with your chosen model)
  # model <- lm(Survived ~ LogFare + Pclass, data = fold_train)
  
  # Store results (placeholder for your logic)
  # fold_accuracies[i] <- calculate_metric_function(model, fold_val)
}

# View average performance across all folds
# mean(fold_accuracies)