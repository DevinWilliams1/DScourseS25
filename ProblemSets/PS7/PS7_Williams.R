library(mice)
library(modelsummary)
library(tidyverse)
library(naniar)
library(broom)
library(kableExtra)

# Load Data
wages_df <- read.csv("wages.csv")

# Drop observations with missing hgc or tenure
wages_clean <- wages_df %>% 
  filter(!is.na(hgc), !is.na(tenure))

# Lots of issues with LaTex code printed. This was used to get it to capture something that used the tabular function. 
latex_output <- capture.output({
  datasummary_skim(wages_clean, 
                   type = "numeric",
                   output = "latex")
})
# Print this LaTex code to be copied into the report. 
cat(paste(latex_output, collapse = "\n"))

#College indicator variable 
wages_clean$college <- ifelse(wages_clean$hgc >= 16, 1, 0)
#Tenure squared variable
wages_clean$tenure2 <- wages_clean$tenure^2

#Listwise deletion (MCAR)
complete_cases <- wages_clean %>%
  filter(!is.na(logwage))
#Estimate regression model for listwise
model_complete <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, 
                     data = complete_cases)

#Mean imputation
wages_mean_imp <- wages_clean
mean_logwage <- mean(wages_clean$logwage, na.rm = TRUE)
wages_mean_imp$logwage[is.na(wages_mean_imp$logwage)] <- mean_logwage

#Estimate Regression for Mean imputation
model_mean <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, 
                 data = wages_mean_imp)

#Regression Imputation (MAR)
# Create a copy for regression imputation
wages_reg_imp <- wages_clean

# Run regression on complete cases
model_for_predict <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married,
                        data = complete_cases)

# Predict missing values
missing_indices <- which(is.na(wages_clean$logwage))
predicted_values <- predict(model_for_predict, newdata = wages_clean[missing_indices, ])

# Impute missing values with predictions
wages_reg_imp$logwage[missing_indices] <- predicted_values

# Estimate regression model
model_reg <- lm(logwage ~ hgc + college + tenure + tenure2 + age + married, 
                data = wages_reg_imp)

#Multiple Imputation with MICE
# Set seed for reproducibility
set.seed(123)

# Create imputation model
# Using only predictors from your regression model
imp_model <- mice(wages_clean[, c("logwage", "hgc", "college", "tenure", "tenure2", "age", "married")], 
                  m = 5,  # Create 5 imputed datasets
                  method = "pmm",  # Predictive mean matching
                  maxit = 50,  # Number of iterations
                  printFlag = FALSE)

# Fit model to each imputed dataset
model_mice <- with(imp_model, 
                   lm(logwage ~ hgc + college + tenure + tenure2 + age + married))

# Pool results from all imputed datasets
model_mice_pooled <- pool(model_mice)

# Model list for modelsummary
model_list <- list(
  "Complete Cases" = model_complete,
  "Mean Imputation" = model_mean,
  "Regression Imputation" = model_reg
)

# Create a modelsummary table, with more LaTex friendly names
ms_table <- modelsummary(
  model_list,
  stars = TRUE,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "hgc" = "Years of Education ($\\beta_1$)",
    "college" = "College Graduate",
    "tenure" = "Tenure", 
    "tenure2" = "Tenure Squared",
    "age" = "Age",
    "married" = "Married"
  ),
  output = "latex"
)

# Print the modelsummary table
cat(ms_table)

# Create a separate table for mice results with LaTeX-friendly beta
mice_coefs <- summary(model_mice_pooled)
mice_table <- data.frame(
  Term = c("Intercept", "Years of Education ($\\beta_1$)", "College Graduate", 
           "Tenure", "Tenure Squared", "Age", "Married"),
  Estimate = mice_coefs$estimate,
  `Std.Error` = mice_coefs$std.error,
  `t value` = mice_coefs$statistic,
  `Pr(>|t|)` = 2 * pnorm(abs(mice_coefs$statistic), lower.tail = FALSE)
)

# Format the mice table
mice_latex <- kable(mice_table, format = "latex", 
                    caption = "Multiple Imputation Results (MICE)",
                    booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

cat(mice_latex)

# Create a comparison table of β₁ values with LaTeX-friendly beta
beta1_complete <- coef(model_complete)["hgc"]
beta1_mean <- coef(model_mean)["hgc"]
beta1_reg <- coef(model_reg)["hgc"]
beta1_mice <- mice_coefs$estimate[mice_coefs$term == "hgc"]
true_beta1 <- 0.093

beta1_table <- data.frame(
  Method = c("Complete Cases", "Mean Imputation", "Regression Imputation", "Multiple Imputation", "True Value"),
  Estimate = c(beta1_complete, beta1_mean, beta1_reg, beta1_mice, true_beta1),
  SE = c(
    summary(model_complete)$coefficients["hgc", "Std. Error"],
    summary(model_mean)$coefficients["hgc", "Std. Error"],
    summary(model_reg)$coefficients["hgc", "Std. Error"],
    mice_coefs$std.error[mice_coefs$term == "hgc"],
    NA
  ),
  Bias = c(
    beta1_complete - true_beta1,
    beta1_mean - true_beta1,
    beta1_reg - true_beta1,
    beta1_mice - true_beta1,
    0
  )
)

# Improve presentation through rounding
beta1_table$Estimate <- round(beta1_table$Estimate, 4)
beta1_table$SE <- round(beta1_table$SE, 4)
beta1_table$Bias <- round(beta1_table$Bias, 4)

# Create LaTeX for comparison table
beta1_latex <- kable(beta1_table, format = "latex", 
                     caption = "Comparison of Returns to Schooling ($\\beta_1$) Across Methods. Note: True value of $\\beta_1$ = 0.093",
                     col.names = c("Method", "Estimate", "Std. Error", "Bias from True Value"),
                     booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))

# Print the β₁ comparison table to input into write-up
cat(beta1_latex)