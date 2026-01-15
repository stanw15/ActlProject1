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
