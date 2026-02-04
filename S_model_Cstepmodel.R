#using s_model variables

library(dplyr)
library(MASS)   # for stepAIC
library(pROC)   # for AUC
predictor_vars_list <- c(
  "Pclass", "Sex", "AgeGroup",
  "SibSp", "Parch", "LogFare", "Embarked"
)
cv_auc <- numeric(k)
selected_vars <- list()
for (i in 1:k) {
  
  cat("\nOuter Fold:", i, "\n")
  
  # --- Outer split ---
  outer_train <- train_val_set %>% filter(fold_id != i)
  outer_val   <- train_val_set %>% filter(fold_id == i)
  
  # --- Fold-safe preprocessing ---
  median_fare <- median(outer_train$Fare, na.rm = TRUE)
  
  outer_train <- outer_train %>%
    mutate(
      Fare = if_else(Fare == 0, median_fare, Fare),
      LogFare = log(Fare)
    )
  
  outer_val <- outer_val %>%
    mutate(
      Fare = if_else(Fare == 0, median_fare, Fare),
      LogFare = log(Fare)
    )
  
  # --- Model selection on OUTER TRAIN only ---
  null_model <- glm(null_formula, data = outer_train, family = "binomial")
  full_model <- glm(full_formula, data = outer_train, family = "binomial")
  
  step_model <- stepAIC(
    object = null_model,
    scope = list(lower = null_formula, upper = full_formula),
    direction = "both",
    trace = FALSE
  )
  
  # Store selected variables
  selected_vars[[i]] <- names(coef(step_model))[-1]
  
  # --- Evaluate on outer validation ---
  val_probs <- predict(step_model, outer_val, type = "response")
   
  roc_obj <- roc(outer_val$Survived, val_probs, quiet = TRUE)
  cv_auc[i] <- auc(roc_obj)
  
  cat("Fold AUC:", round(cv_auc[i], 4), "\n")
  cat("Selected variables:\n")
  print(selected_vars[[i]])
}
