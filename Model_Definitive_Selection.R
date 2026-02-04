library(dplyr)
all_data <- read.csv("KaggleData.csv")

# Feature Engineer

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


# Split data --------------------------------------------------
set.seed(3)


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
test_set_base   <- all_data_shuffled %>% slice((train_val_idx + 1):n)


# Fill null values --------------------------------------------------
median_fare_train_val <- median(train_val_set$Fare, na.rm = TRUE)

train_val_final <- train_val_set %>%
  mutate(Fare = if_else(Fare == 0, final_median_fare, Fare),
         LogFare = log(Fare)) # generally add +1 for 0 value

median_fare_test <- median(test_set$Fare, na.rm = TRUE)
test_set_definitive <- test_set_base %>%
  mutate(Fare = if_else(Fare == 0, median_fare_test, Fare))


# Model S ----------------------------------------------------final_median_fare <- median(train_val_set$Fare, na.rm = TRUE)
predictor_vars_list_definitive <- c("Pclass", "Sex", "AgeGroup", "SibSp")
formula_definitive <- as.formula(paste("Survived ~", paste(predictor_vars_list_definitive, collapse = " + ")))
model_definitive <- glm(formula_definitive, data = train_val_final, family = "binomial")
summary(model_definitive)