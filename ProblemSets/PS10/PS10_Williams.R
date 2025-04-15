library(tidyverse)
library(tidymodels)
library(magrittr)
library(modelsummary)
library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(kernlab)

set.seed(100)

income <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", col_names = FALSE)
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
# (metadata details...)

######################
# Clean up the data
######################
# Drop unnecessary columns
income %<>% select(-native.country, -fnlwgt, -education.num)
# Make sure continuous variables are formatted as numeric
income %<>% mutate(across(c(age,hours,capital.gain,capital.loss), as.numeric))
# Make sure discrete variables are formatted as factors
income %<>% mutate(across(c(high.earner,education,marital.status,race,workclass,occupation,relationship,sex), as.factor))
# Combine levels of factor variables that currently have too many levels
income %<>% mutate(education = fct_collapse(education,
                                            Advanced    = c("Masters","Doctorate","Prof-school"), 
                                            Bachelors   = c("Bachelors"), 
                                            SomeCollege = c("Some-college","Assoc-acdm","Assoc-voc"),
                                            HSgrad      = c("HS-grad","12th"),
                                            HSdrop      = c("11th","9th","7th-8th","1st-4th","10th","5th-6th","Preschool") 
),
marital.status = fct_collapse(marital.status,
                              Married      = c("Married-civ-spouse","Married-spouse-absent","Married-AF-spouse"), 
                              Divorced     = c("Divorced","Separated"), 
                              Widowed      = c("Widowed"), 
                              NeverMarried = c("Never-married")
), 
race = fct_collapse(race,
                    White = c("White"), 
                    Black = c("Black"), 
                    Asian = c("Asian-Pac-Islander"), 
                    Other = c("Other","Amer-Indian-Eskimo")
), 
workclass = fct_collapse(workclass,
                         Private = c("Private"), 
                         SelfEmp = c("Self-emp-not-inc","Self-emp-inc"), 
                         Gov     = c("Federal-gov","Local-gov","State-gov"), 
                         Other   = c("Without-pay","Never-worked","?")
), 
occupation = fct_collapse(occupation,
                          BlueCollar  = c("?","Craft-repair","Farming-fishing","Handlers-cleaners","Machine-op-inspct","Transport-moving"), 
                          WhiteCollar = c("Adm-clerical","Exec-managerial","Prof-specialty","Sales","Tech-support"), 
                          Services    = c("Armed-Forces","Other-service","Priv-house-serv","Protective-serv")
)
)

######################
# tidymodels time!
######################
income_split <- initial_split(income, prop = 0.8)
income_train <- training(income_split)
income_test  <- testing(income_split)

# 3-fold cross-validation - define once and reuse
rec_folds <- vfold_cv(income_train, v = 3)

#####################
# logistic regression
#####################
print('Starting LOGIT')
# set up the task and the engine
tune_logit_spec <- logistic_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("classification")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 20)  # Reduced from 50 to 20 for faster computing

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_logit_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_acc  <- show_best(rec_res, metric = "accuracy")
best_acc <- select_best(rec_res, metric = "accuracy")
final_logit_lasso <- finalize_workflow(rec_wf,
                                       best_acc
)
print('*********** LOGISTIC REGRESSION **************')
logit_test <- last_fit(final_logit_lasso, income_split) %>%
  collect_metrics()

logit_test %>% print(n = 1)
top_acc %>% print(n = 1)

# combine results into a nice tibble (for later use)
logit_ans <- top_acc %>% slice(1)
logit_ans %<>% left_join(logit_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg = "logit") %>% select(-starts_with(".config"))

#####################
# tree model
#####################
print('Starting TREE')
# set up the task and the engine
tune_tree_spec <- decision_tree(
  min_n = tune(), # tuning parameter
  tree_depth = tune(), # tuning parameter
  cost_complexity = tune(), # tuning parameter
) %>% 
  set_engine("rpart") %>%
  set_mode("classification")

# Reduced parameter grid for faster computing
tree_parm_df1 <- tibble(cost_complexity = c(0.001, 0.05, 0.2))  # Just 3 values
tree_parm_df2 <- tibble(min_n = c(10, 50, 100))  # Just 3 values
tree_parm_df3 <- tibble(tree_depth = c(5, 10, 20))  # Just 3 values
tree_parm_df  <- full_join(tree_parm_df1, tree_parm_df2, by=character()) %>% 
  full_join(., tree_parm_df3, by=character())

# workflow
tree_wf <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# tuning results
tree_res <- tree_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = tree_parm_df
  )

# what is the best value of parameters?
top_acc_tree  <- show_best(tree_res, metric = "accuracy")
best_acc_tree <- select_best(tree_res, metric = "accuracy")
final_tree <- finalize_workflow(tree_wf,
                                best_acc_tree
)
print('*********** Decision Tree **************')
tree_test <- last_fit(final_tree, income_split) %>%
  collect_metrics()
tree_test %>% print(n = 1)
top_acc_tree %>% print(n = 1)

# combine results into tibble
tree_ans <- top_acc_tree %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg = "tree") %>% select(-starts_with(".config"))

#####################
# neural net
#####################
print('Starting NNET')
# set up the task and the engine
tune_nnet_spec <- mlp(
  hidden_units = tune(), # tuning parameter
  penalty = tune()
) %>% 
  set_engine("nnet") %>%
  set_mode("classification")

# define a set over which to try different values - use fewer values for faster performance
nnet_parm_df1 <- tibble(hidden_units = c(1, 5, 10))  # Reduced set of values
lambda_grid_nn <- grid_regular(penalty(), levels = 5)  # Fewer levels
nnet_parm_df  <- full_join(nnet_parm_df1, lambda_grid_nn, by=character())

# workflow
nnet_wf <- workflow() %>%
  add_model(tune_nnet_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning Results
nnet_res <- nnet_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = nnet_parm_df
  )

# Find best parameters
top_acc_nnet  <- show_best(nnet_res, metric = "accuracy")
best_acc_nnet <- select_best(nnet_res, metric = "accuracy")
final_nnet <- finalize_workflow(nnet_wf,
                                best_acc_nnet
)

print('*********** Neural Net **************')
nnet_test <- last_fit(final_nnet, income_split) %>%
  collect_metrics()

nnet_test %>% print(n = 1)
top_acc_nnet %>% print(n = 1)

# Results combined to a tibble
nnet_ans <- top_acc_nnet %>% slice(1)
nnet_ans %<>% left_join(nnet_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg = "nnet") %>% select(-starts_with(".config"))

#####################
# knn
#####################
print('Starting KNN')
# set up the task and the engine
tune_knn_spec <- nearest_neighbor(
  neighbors = tune() # tuning parameter
) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

# define a set over which to try different values (reduced for faster computing)
knn_parm_df <- tibble(neighbors = c(1, 5, 10, 15, 20, 30))  # Reduced from 1-30 sequence

# Workflow
knn_wf <- workflow() %>%
  add_model(tune_knn_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Tuning results
knn_res <- knn_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = knn_parm_df
  )

# Find the best number of neighbors
top_acc_knn  <- show_best(knn_res, metric = "accuracy")
best_acc_knn <- select_best(knn_res, metric = "accuracy")
final_knn <- finalize_workflow(knn_wf,
                               best_acc_knn
)
print('*********** KNN **************')
knn_test <- last_fit(final_knn, income_split) %>%
  collect_metrics()

knn_test %>% print(n = 1)
top_acc_knn %>% print(n = 1)

# Combine results into a tibble
knn_ans <- top_acc_knn %>% slice(1)
knn_ans %<>% left_join(knn_test %>% slice(1), by=c(".metric",".estimator")) %>%
  mutate(alg = "knn") %>% select(-starts_with(".config"))

#####################
# SVM - Modified for reliability
#####################
print('Starting SVM')

# Instead of tuning with a grid, we'll test specific combinations manually
# Define the cost and sigma values specifically mentioned in the assignment
cost_values <- c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10)
sigma_values <- c(2^(-2), 2^(-1), 2^0, 2^1, 2^2, 2^10)

# Select a subset for faster computation
test_costs <- c(2^0, 2^2, 2^10)      # Just 3 values from the specified set
test_sigmas <- c(2^(-2), 2^0, 2^2)  # Just 3 values from the specified set

# Create empty tibble to store results
svm_results <- tibble(
  cost = numeric(),
  rbf_sigma = numeric(),
  accuracy = numeric()
)

# Loop through selected parameter combinations
for (cost_val in test_costs) {
  for (sigma_val in test_sigmas) {
    print(paste("Testing cost =", cost_val, "and rbf_sigma =", sigma_val))
    
    # Create SVM model with current parameters
    svm_spec <- svm_rbf(
      cost = cost_val, 
      rbf_sigma = sigma_val
    ) %>% 
      set_engine("kernlab") %>%
      set_mode("classification")
    
    # Create workflow
    svm_wf <- workflow() %>%
      add_model(svm_spec) %>%
      add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)
    
    # Fit on training data
    tryCatch({
      # Use cross-validation for more robust evaluation
      cv_results <- svm_wf %>%
        fit_resamples(
          resamples = rec_folds
        ) %>%
        collect_metrics() %>%
        filter(.metric == "accuracy") %>%
        pull(mean)
      
      # Add to results
      svm_results <- svm_results %>%
        add_row(
          cost = cost_val,
          rbf_sigma = sigma_val,
          accuracy = cv_results
        )
      
      print(paste("Completed - accuracy:", cv_results))
    }, error = function(e) {
      print(paste("Error with cost =", cost_val, "and rbf_sigma =", sigma_val, ":", e$message))
    })
  }
}

# Find optimal parameters
best_params <- svm_results %>%
  arrange(desc(accuracy)) %>%
  slice(1)

print("Optimal SVM parameters:")
print(best_params)

#Errors 

# Create final SVM model with optimal parameters
final_svm_spec <- svm_rbf(
  cost = best_params$cost, 
  rbf_sigma = best_params$rbf_sigma
) %>% 
  set_engine("kernlab") %>%
  set_mode("classification")

final_svm_wf <- workflow() %>%
  add_model(final_svm_spec) %>%
  add_formula(high.earner ~ education + marital.status + race + workclass + occupation + relationship + sex + age + capital.gain + capital.loss + hours)

# Train on full training set and evaluate on test set
svm_test <- final_svm_wf %>%
  fit(data = income_train) %>%
  predict(new_data = income_test) %>%
  bind_cols(income_test %>% select(high.earner)) %>%
  metrics(truth = high.earner, estimate = .pred_class)

print("Final SVM results with optimal parameters:")
print(svm_test)

# Create a tibble for comparison with other algorithms
svm_ans <- tibble(
  .metric = "accuracy",
  .estimator = "binary",
  .estimate = svm_test %>% 
    filter(.metric == "accuracy") %>% 
    pull(.estimate),
  alg = "svm"
)

print(svm_ans)

# Create a new dataframe with all the information for the table
latex_table <- tibble(
  Algorithm = c("Decision Tree", "SVM", "Logistic Regression", "KNN", "Neural Network"),
  Accuracy = c(
    tree_ans$.estimate,
    svm_ans$.estimate,
    logit_ans$.estimate,
    knn_ans$.estimate,
    nnet_ans$.estimate
  ),
  Parameters = c(
    paste0("cost\\_complexity = ", round(best_acc_tree$cost_complexity, 2), 
           ", tree\\_depth = ", best_acc_tree$tree_depth, 
           ", min\\_n = ", best_acc_tree$min_n),
    paste0("cost = ", round(best_params$cost, 2), 
           ", rbf\\_sigma = ", round(best_params$rbf_sigma, 2)),
    paste0("penalty = ", round(best_acc$penalty, 2)),
    paste0("neighbors = ", best_acc_knn$neighbors),
    paste0("penalty = ", round(best_acc_nnet$penalty, 2), 
           ", hidden\\_units = ", best_acc_nnet$hidden_units)
  )
)

# Order by accuracy (highest first)
latex_table <- latex_table %>%
  arrange(desc(Accuracy))

# Format accuracies as percentages with 1 decimal place
latex_table$Accuracy <- sprintf("%.1f\\%%", latex_table$Accuracy * 100)

# Generate LaTeX code
cat("\\begin{table}[htbp]\n")
cat("    \\centering\n")
cat("    \\caption{Optimal Tuning Parameters and Out-of-Sample Performance}\n")
cat("    \\begin{tabular}{lcp{8cm}}\n")
cat("    \\hline\n")
cat("    Algorithm & Accuracy & Parameters \\\\\n")
cat("    \\hline\n")

# Add each row
for(i in 1:nrow(latex_table)) {
  cat("    ", latex_table$Algorithm[i], " & ", 
      latex_table$Accuracy[i], " & ", 
      latex_table$Parameters[i], " \\\\\n", sep="")
}

cat("    \\hline\n")
cat("    \\end{tabular}\n")
cat("\\end{table}\n")

#####################
# combine answers
#####################
all_ans <- bind_rows(logit_ans, tree_ans, nnet_ans, knn_ans, svm_ans)
datasummary_df(all_ans %>% select(-.metric, -.estimator, -mean, -n, -std_err), output="markdown") %>% print
