library(dplyr)
library(MASS)   # stepAIC
library(pROC)   # AUC

# Median Fare from training data
median_fare <- median(train_val_set$Fare, na.rm = TRUE)

train_val_set <- train_val_set %>%
  mutate(
    Fare = if_else(Fare == 0, median_fare, Fare),
    LogFare = log(Fare)
  )

null_model <- glm(
  null_formula,
  data = train_val_set,
  family = binomial
)

full_model <- glm(
  full_formula,
  data = train_val_set,
  family = binomial
)

step_model <- stepAIC(
  object = null_model,
  scope = list(lower = null_formula, upper = full_formula),
  direction = "both",
  trace = FALSE
)

# AIC
final_aic <- AIC(step_model)
cat("Final model AIC:", final_aic, "\n")

# AUC (in-sample)
train_probs <- predict(step_model, train_val_set, type = "response")
roc_obj <- roc(train_val_set$Survived, train_probs, quiet = TRUE)
train_auc <- auc(roc_obj)

cat("Training AUC:", round(train_auc, 4), "\n")

selected_vars <- names(coef(step_model))[-1]
cat("Selected variables:\n")
print(selected_vars)

final_formula <- formula(step_model)

final_model <- glm(
  final_formula,
  data = train_val_set,
  family = binomial
)

summary(final_model)

# All coefficients except the intercept
coef_names <- names(coef(step_model))[-1]

# Map dummies back to original variables
get_var_base <- function(x, original_vars) {
  # If numeric variable, keep as is
  if (x %in% original_vars) return(x)
  
  # Otherwise, find which original factor variable contains this dummy
  # Match by starting substring
  match_var <- original_vars[sapply(original_vars, function(v) startsWith(x, v))]
  if (length(match_var) == 1) return(match_var)
  return(NA)
}

# Original variables used in model
original_vars <- predictor_vars_list

# Get unique set of variables in model (numeric + factors)
step_vars <- unique(sapply(coef_names, get_var_base, original_vars = original_vars))
cat("Variables in stepwise model:\n")
print(step_vars)
test_remove_var <- function(var_to_remove, model_vars, data, outcome = "Survived") {
  
  vars_keep <- setdiff(model_vars, var_to_remove)
  vars_keep_backtick <- paste0("`", vars_keep, "`", collapse = " + ")
  formula_new <- as.formula(paste(outcome, "~", vars_keep_backtick))
  
  model_new <- glm(formula_new, data = data, family = binomial)
  
  # Metrics
  aic_diff <- AIC(model_new) - AIC(step_model)
  probs <- predict(model_new, data, type = "response")
  auc_val <- auc(roc(data[[outcome]], probs, quiet = TRUE))
  
  return(list(model = model_new, AIC_diff = aic_diff, AUC = auc_val))
}
results <- lapply(step_vars, function(v) {
  res <- test_remove_var(v, step_vars, train_val_set)
  c(Variable = v, AIC_diff = res$AIC_diff, AUC = as.numeric(res$AUC))
})

results_df <- do.call(rbind, results)
results_df <- as.data.frame(results_df)
results_df$AIC_diff <- as.numeric(as.character(results_df$AIC_diff))
results_df$AUC <- as.numeric(as.character(results_df$AUC))

# Sort by AIC increase
results_df <- results_df[order(results_df$AIC_diff), ]
print(results_df)

