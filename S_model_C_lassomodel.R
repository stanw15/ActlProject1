library(dplyr)
library(glmnet)
library(pROC)
train_data <- train_val_set

median_fare <- median(train_data$Fare, na.rm = TRUE)

train_data <- train_data %>%
  mutate(
    Fare = if_else(Fare == 0, median_fare, Fare),
    LogFare = log(Fare)
  )
predictor_vars_list <- c(
  "Pclass", "Sex", "AgeGroup",
  "SibSp", "Parch", "LogFare", "Embarked"
)

x <- model.matrix(
  as.formula(paste("~", paste(predictor_vars_list, collapse = " + "))),
  data = train_data
)[, -1]

y <- train_data$Survived
set.seed(123)

lasso_cv <- cv.glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = 1,            # LASSO
  nfolds = 5,
  type.measure = "deviance"
)
lasso_cv$lambda.min
lasso_cv$lambda.1se
best_lambda <- lasso_cv$lambda.1se

coef_mat <- coef(lasso_cv, s = best_lambda)

selected_vars <- rownames(coef_mat)[coef_mat[, 1] != 0]
selected_vars <- setdiff(selected_vars, "(Intercept)")

selected_vars
