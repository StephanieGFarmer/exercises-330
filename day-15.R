# Stephanie Farmer
# Daily Assignment 16 for ESS 330

library(tidymodels)
library(tidyverse)
library(workflowsets)
library(magrittr)


data(penguins, package = "palmerpenguins")

set.seed(123)


data_split <- initial_split(penguins, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)


cv_folds <- vfold_cv(train_data, v = 10)

# Model Fitting and Workflow


log_reg_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")


rand_forest_model <- rand_forest() %>%
  set_mode("classification")  # Using default engine (ranger)


model_set <- workflow_set(
  preproc = list(simple = species ~ .),  # Use all predictors
  models = list(log_reg = log_reg_model, rand_forest = rand_forest_model)
)


results <- model_set %>%
  workflow_map(
    "fit_resamples",
    resamples = cv_folds,
    metrics = metric_set(accuracy)
  )

ranked_results <- collect_metrics(results) %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean))


cat("\nModel Accuracy Results:\n")
print(ranked_results)


autoplot(results, metric = "accuracy")


print(results)
