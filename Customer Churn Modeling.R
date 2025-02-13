# Loading required libraries
library(tidyverse)
library(tidymodels)
library(discrim)

# Data has already being split in the preprocessing R script

# Review train data
churn.train

# Review test data
churn.test

# First, lets create a preprocessing recipe
churn.recipe <-
recipe(Churn ~ ., data = churn.train) %>%
  step_string2factor(all_nominal_predictors()) %>%
  update_role(customerID, new_role = "id") %>%
  step_impute_knn(TotalCharges) %>%
  step_normalize(MonthlyCharges, TotalCharges) %>%
  step_dummy(all_nominal_predictors())
churn.recipe

# Creating resampling folds
churn.folds <- vfold_cv(churn.train, v = 10)
churn.folds

# Lets create a workflow set of the following model specifications:
# Logistic models
# Linear discriminant models
# Simple Random forest
# Simple Boosted tree (XGBoost)

# Making an overall workflow set
simple.workflow.set <-
workflow_set(
  preproc = list(
    churn.recipe = churn.recipe,
    discrim.recipe = recipe(Churn ~ ., data = churn.train) %>%
      step_string2factor(all_nominal_predictors()) %>%
      update_role(customerID, new_role = "id") %>%
      step_impute_knn(TotalCharges) %>%
      step_normalize(MonthlyCharges, TotalCharges) %>%
      step_corr(all_numeric_predictors(), threshold = 0.9) %>%
      step_dummy(all_nominal_predictors()),
    churn.recipe = churn.recipe,
    churn.recipe = churn.recipe
  ),
  models = list(
    logistic.spec = logistic_reg(),
    linear.discrim = discrim_linear(mode = "classification", engine = "MASS"),
    rf.spec = rand_forest(mode = "classification", engine = "ranger"),
    xgb.spec = boost_tree(mode = "classification", engine = "xgboost")
  ),
  cross = FALSE
)
simple.workflow.set

# Now Lets create a metric function for sensistivity and specificity
my.metrics <- metric_set(sensitivity, specificity, roc_auc)
my.metrics

# Get fitting results
set.seed(1233)
simple.results <-
workflow_map(
  object = simple.workflow.set,
  fn = "fit_resamples",
  verbose = TRUE,
  resamples = churn.folds,
  metrics = my.metrics
)
simple.results

# Check out result of fitting
simple.results %>%
  collect_metrics()
# We see that logistic models do better than others

# Lets see if regularization can increase the performance of logistic regression

# Firstly, create a grid for testing
logistic.grid <-
grid_regular(mixture(), penalty(range = c(-10, 0.3), trans = scales::transform_log10()), levels = c(4, 10))
logistic.grid

# Now create a logistic spec
logistic.spec <- logistic_reg(mixture = tune(), penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
logistic.spec

# Create workflow
logistic.workflow <- workflow(
  preprocessor = churn.recipe,
  spec = logistic.spec
)
logistic.workflow

# Fit workflow into resamples
logistic.results <-
logistic.workflow %>%
  tune_grid(
    resamples = churn.folds,
    grid = logistic.grid,
    control = control_resamples(save_pred = TRUE),
    metrics = metric_set(sensitivity, specificity, roc_auc)
  )
logistic.results

# Plot results
logistic.results %>%
  autoplot()
# It seems like the lesser the regularization, the better

# Lets see the best fit via roc_auc
logistic.results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  arrange(-mean)

# Get out best performing model by roc_auc
logistic.parameters <- select_best(logistic.results, metric = "roc_auc")
logistic.parameters

# The best performing model seems to be one that affects the model as little as possible. The penalty can as well be zero

# Create spec for last fit
final.spec <-
logistic_reg(mixture = logistic.parameters$mixture, penalty = logistic.parameters$penalty) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
final.spec

# Combining spec to workflow and fitting to entire training data
final.fit <- workflow(
  preprocessor = churn.recipe,
  spec = final.spec
) %>%
  fit(
    data = churn.train
  )
final.fit

# Checking out models decision making process
final.fit %>%
  tidy() %>%
  arrange(-abs(estimate))

# Lets test on the test data
churn.test["Churn"] %>%
  bind_cols(
    predict(final.fit, churn.test, type = "prob")
  ) %>%
  mutate(
    Churn = as.factor(Churn),
    .pred_class = as.factor(if_else(.pred_No >= 0.5, "No", "Yes"))
  ) %>%
  metrics(
    truth = Churn,
    estimate = .pred_class,
    .pred_No
  )
# This model shows moderately impresive performance.

# Recalculate metrics
churn.test["Churn"] %>%
  bind_cols(
    predict(final.fit, churn.test, type = "prob")
  ) %>%
  mutate(
    Churn = as.factor(Churn),
    .pred_class = as.factor(if_else(.pred_No >= 0.5, "No", "Yes"))
  ) %>%
  my.metrics(
    truth = Churn,
    estimate = .pred_class,
    .pred_No
  )

# Lets do one more thing; a neural network

# Creating a neural spec
neural.spec <- mlp(hidden_units = c(17, 10, 6)) %>%
  set_engine("keras") %>%
  set_mode("classification")
neural.spec

# Lets then create a workflow
neural.workflow <- workflow(
  preprocessor = churn.recipe,
  spec = neural.spec
)
neural.workflow

# Fit neural workflow on resamples
neural.results <- neural.workflow %>%
  fit_resamples(
    resamples = churn.folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )
neural.results

# Check out neural network results
neural.results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc")
# The neural networks results are moderate and do not surpass the logistic model in terms of roc_auc
# For this reason, logistic models would be used

# Lets vary the cutoff mark to capture more number of potentially churning individuals
bind_cols(
  Churn = as.factor(churn.test[["Churn"]]),
  predict(final.fit, churn.test, type = "prob")
) %>%
  mutate(
    .pred_class = as.factor(if_else(.pred_Yes >= 0.3, "Yes", "No"))
  ) %>%
  my.metrics(
    truth = Churn,
    estimate = .pred_class,
    .pred_No
  )
# Varying the cut off probability has the ability to capture more or less of the amount of people to be churned at the expense of increasing false positives