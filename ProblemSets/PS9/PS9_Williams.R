library(tidyverse)
library(tidymodels)
library(glmnet)
library(magrittr)

housing_data <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing_data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

set.seed(123456)

housing_split <- initial_split(housing_data, prop = 0.8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

#Created a recipe
housing_recipe <- recipe (medv ~ . ,data = housing_data) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0 / 1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                      ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,
                lstat,dis,nox,degree =6)
# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train,retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)
# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

#find dimensions of training data
dim(housing_train)

#find how many more X variables we have compared to original data
original_predictors <- ncol(housing_data) - 1

#Number of predictors after preprocessing
new_predictors <- ncol(housing_train_x)

#How many more variables
additional_predictors <- new_predictors - original_predictors
cat("Original Predictors:", original_predictors, "\n",
    "New Predictors after preprocessing:", new_predictors, "\n",
    "Additional Predictors created:", additional_predictors, "\n"
)

#Setting Up Lasso
tune_spec <- linear_reg(
  penalty = tune(),
  mixture = 1
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")
lambda_grid <- grid_regular(penalty(), levels = 50)

#6 fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

#Creating workflow to do k-fold
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

#Tuning the model
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

#optimal lambda
top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

#Train with finetuned
final_lasso <- finalize_workflow(rec_wf, best_rmse)

last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print

top_rmse %>% print(n=1)

# RIDGE REGRESSION MODEL
ridge_tune_spec <- linear_reg(
  penalty = tune(),
  mixture = 0
) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Can reuse same Lambda for this
ridge_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(ridge_tune_spec)

ridge_res <- ridge_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

ridge_top_rmse <- show_best(ridge_res, metric = "rmse")
ridge_best_rmse <- select_best(ridge_res, metric = "rmse")

final_ridge <- finalize_workflow(ridge_wf, ridge_best_rmse)
ridge_fit <- last_fit(final_ridge, split = housing_split)
ridge_fit %>% collect_metrics() %>% print
ridge_top_rmse %>% print(n=1)


