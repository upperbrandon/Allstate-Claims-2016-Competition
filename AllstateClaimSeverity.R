
# Allstate < 1240

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(vroom)
library(skimr)
library(DataExplorer)
library(patchwork)
library(glmnet)
library(ranger)
library(ggmosaic)
library(embed)
library(tensorflow)
library(themis) 
library(bonsai)
library(lightgbm)

set.seed(1213)

# WD ----------------------------------------------------------------------

setwd("~/GitHub/Allstate-Claims-2016-Competition")

# Read Files --------------------------------------------------------------

Train <- vroom("train.csv")
Test <- vroom("test.csv")

# Recipe ------------------------------------------------------------------

my_recipe <- recipe(loss ~ ., data = Train) %>%
  step_mutate_at(all_numeric_predictors(), fn = as.factor) %>% 
  step_other(all_nominal_predictors(), threshold = 0.001) %>% 
  step_lencode(all_nominal_predictors(), outcome = vars(loss), smooth = FALSE) %>%
  step_zv(all_predictors())


# Model -------------------------------------------------------------------

rf_mod <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 100
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")


rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod)

folds <- vfold_cv(Train, v = 2, strata = loss)

rf_grid <- grid_regular(
  mtry(range = c(1, 6)),
  min_n(range = c(5, 25)),
  levels = 2
)

rf_tuned <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(mae)
)

rf_best <- select_best(rf_tuned, metric = "mae")
rf_final_wf <- finalize_workflow(rf_wf, rf_best)
rf_final_fit <- fit(rf_final_wf, data = Train)

rf_predictions <- predict(rf_final_fit, new_data = Test, type = "numeric") %>%
rename(loss = .pred)

kaggle_rf_submission <- Test %>%
  select(id) %>%
  bind_cols(rf_predictions)

vroom_write(
  kaggle_rf_submission,
  file = "./Forest.csv",
  delim = ","
)
2
# 50 trees Score 1228.259 (Nothing)
# 100 trees Score 1226.30 (Seed 1213) 
# 100 trees Score 1225.69 (MAE instead of RMSE)


# Linear Model ------------------------------------------------------------


my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

Linear_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data = Train)

lin_preds <- predict(Linear_workflow, new_data = Test) %>%
  rename(loss = .pred)

kaggle_submission_lm <- Test %>%
  select(id) %>%
  bind_cols(lin_preds)

vroom_write(x = kaggle_submission_lm, file = "./LinearPreds.csv", delim = ",") 



# Boosting  ---------------------------------------------------------------

boost_model <- boost_tree(tree_depth = tune(),
                          trees = tune(),
                          learn_rate = tune()) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")


boost_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(boost_model)

folds <- vfold_cv(Train, v = 2, repeats=1)

grid_of_tuning_params <- grid_regular(
  tree_depth(range = c(1, 10)),
  learn_rate(range = c(-3, -0.1)),
  trees(range = c(10, 50)),
  levels = 2
)

CV_results <- boost_workflow %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse, mae))

bestTune <- CV_results %>%
  select_best(metric="rmse")

final_wf <-
  boost_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=Train)

boost_preds <- predict(final_wf, new_data = Test) %>%
  rename(loss = .pred)

kaggle_submission_lm <- Test %>%
  select(id) %>%
  bind_cols(boost_preds)

vroom_write(x = kaggle_submission_lm, file = "./BoostPreds.csv", delim = ",")

# 1256.40

# EDA ---------------------------------------------------------------------

# Searching columns for Dep variable

colnames(Train)
pull(Train,loss)

b <- -Inf  # start with the smallest possible number

for (i in 1:14) {
  a <- paste0("cont", i)
  col_max <- max(Train[[a]], na.rm = TRUE)
  if (col_max > b) {
    b <- col_max
  }
}

b 

