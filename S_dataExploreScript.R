library(dplyr)
all_data <- read.csv("KaggleData.csv")

# check data structure, all values make sense
glimpse(all_data)
summary(all_data)

# check for NA
colSums(is.na(all_data))
colSums(all_data == "", na.rm = TRUE) #catch empty string

#Fare data check
sum(all_data$Fare < 3)
all_data$Fare[all_data$Fare < 6]

#Check Ids are unique
length(unique(all_data$PassengerId))

#Check categorical data entries are correct (standardized)
categorical_cols <- c("Survived", "Pclass", "Sex", "Embarked")
categorical_subset <- all_data %>%
  select(all_of(categorical_cols))
# For each column, get its categories and counts in each
category_summaries <- lapply(categorical_subset, function(col) {
  # Convert to a data frame so dplyr can count it
  as.data.frame(col) %>% 
    count(col, sort = TRUE) %>%
    rename(Category = col, n = n)
})
# Name the list elements so you know which table is which
names(category_summaries) <- categorical_cols


#------------ Code section for processing, splitting data
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
# Define your proportions --- use train x%, test & validate y%
train_prop <- 0.5
test_val_prop <- (1 - train_prop) / 2
train_idx <- floor(train_prop * n)
test_idx   <- floor((train_prop + test_val_prop) * n) # eg 0.6 + 0.2

# Use slice to create the data frames
train_set <- all_data_shuffled %>% slice(1:train_idx)
test_set   <- all_data_shuffled %>% slice((train_idx + 1):test_idx)
val_set  <- all_data_shuffled %>% slice((test_idx + 1):n)

# 4. VERIFY
nrow(train_set) + nrow(test_set) + nrow(val_set) == n