library(nloptr)
library(tidyverse)
library(modelsummary)

# Set seed for reproducibility
set.seed(100)

# Set dimensions
N <- 100000
K <- 10

cat("Step 1: Generate data\n")

# Create the matrix with random normal values
X <- matrix(rnorm(N * (K-1)), nrow = N, ncol = K-1)

# Add a column of 1's as the first column
X <- cbind(rep(1, N), X)

# Define sigma
sigma <- 0.5
sigma_squared <- sigma^2  # 0.25

# Generate the error term
eps <- rnorm(N, mean = 0, sd = sigma)

# Define the beta vector
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate
Y <- X %*% beta + eps

cat("Matrix X dimensions:", dim(X), "\n")
cat("Vector Y length:", length(Y), "\n")
cat("True beta values:", beta, "\n")
cat("True sigma:", sigma, "\n\n")

cat("Step 2: Compute OLS using closed-form solution\n")

# Compute beta_OLS using the closed-form solution
beta_OLS_closed <- solve(t(X) %*% X) %*% t(X) %*% Y

# Display results
cat("OLS estimates (closed-form):", beta_OLS_closed, "\n")
cat("Difference from true beta:", beta - beta_OLS_closed, "\n\n")

cat("Step 3: Compute OLS using gradient descent\n")

# Initialize parameters
learning_rate <- 0.0000003
max_iterations <- 10000
tolerance <- 1e-6

# Initialize beta to zeros
beta_GD <- rep(0, K)

# Pre-compute X'X and X'Y to speed up gradient calculations
XtX <- t(X) %*% X
XtY <- t(X) %*% Y

# Iterate
for (i in 1:max_iterations) {
  # Compute gradient
  gradient <- -2 * (XtY - XtX %*% beta_GD)
  
  # Update beta
  beta_GD_new <- beta_GD - learning_rate * gradient
  
  # Check convergence
  if (sum((beta_GD_new - beta_GD)^2) < tolerance) {
    cat("Converged after", i, "iterations\n")
    break
  }
  
  # Update beta for next iteration
  beta_GD <- beta_GD_new
  
  # Print progress every 1000 iterations
  if (i %% 1000 == 0) {
    cat("Iteration", i, "- Current beta:", beta_GD[1:3], "...\n")
  }
}

cat("OLS estimates (gradient descent):", beta_GD, "\n")
cat("Difference from true beta:", beta - beta_GD, "\n")
cat("Difference from closed-form OLS:", beta_OLS_closed - beta_GD, "\n\n")

cat("Step 4: Compute OLS using L-BFGS and Nelder-Mead algorithms\n")

# Define the OLS objective function: sum of squared residuals
ols_objective <- function(beta_params, X, Y) {
  residuals <- Y - X %*% beta_params
  return(sum(residuals^2))
}

# Define the gradient of the objective function
ols_gradient <- function(beta_params, X, Y) {
  residuals <- Y - X %*% beta_params
  return(-2 * t(X) %*% residuals)
}

# Initial guess for beta
beta_init <- rep(0, K)

# L-BFGS optimization
opts_lbfgs <- list(
  algorithm = "NLOPT_LD_LBFGS",
  xtol_rel = 1.0e-8,
  maxeval = 1000
)

result_lbfgs <- nloptr(
  x0 = beta_init,
  eval_f = ols_objective,
  eval_grad_f = ols_gradient,
  opts = opts_lbfgs,
  X = X, 
  Y = Y
)

beta_lbfgs <- result_lbfgs$solution

# Nelder-Mead optimization
opts_nm <- list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_rel = 1.0e-8,
  maxeval = 10000
)

result_nm <- nloptr(
  x0 = beta_init,
  eval_f = ols_objective,
  opts = opts_nm,
  X = X, 
  Y = Y
)

beta_nm <- result_nm$solution

cat("OLS estimates (L-BFGS):", beta_lbfgs, "\n")
cat("OLS estimates (Nelder-Mead):", beta_nm, "\n")
cat("Difference L-BFGS vs true beta:", beta - beta_lbfgs, "\n")
cat("Difference Nelder-Mead vs true beta:", beta - beta_nm, "\n")
cat("Difference L-BFGS vs closed-form OLS:", beta_OLS_closed - beta_lbfgs, "\n")
cat("Difference Nelder-Mead vs closed-form OLS:", beta_OLS_closed - beta_nm, "\n")
cat("Difference L-BFGS vs Nelder-Mead:", beta_lbfgs - beta_nm, "\n\n")

cat("Step 5: Compute MLE using L-BFGS algorithm\n")

# Define the negative log-likelihood function for normal linear regression
neg_log_likelihood <- function(theta, Y, X) {
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  
  n <- length(Y)
  residuals <- Y - X %*% beta
  
  # Negative log-likelihood
  ll <- -(-n/2 * log(2 * pi) - n/2 * log(sig^2) - 
            sum(residuals^2)/(2 * sig^2))
  
  return(ll)
}

# Define the gradient function
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X) %*% (Y - X %*% beta) / (sig^2)
  grad[length(theta)] <- dim(X)[1] / sig - crossprod(Y - X %*% beta) / (sig^3)
  return(grad)
}

# Initial guess for theta (beta and sigma)
theta_init <- c(rep(0, K), 1)  # Initial guess for beta values and sigma

# L-BFGS optimization
opts_lbfgs <- list(
  algorithm = "NLOPT_LD_LBFGS",
  xtol_rel = 1.0e-8,
  maxeval = 1000
)

result_mle <- nloptr(
  x0 = theta_init,
  eval_f = neg_log_likelihood,
  eval_grad_f = gradient,
  opts = opts_lbfgs,
  Y = Y, 
  X = X
)

# Extract results
beta_mle <- result_mle$solution[1:K]
sigma_mle <- result_mle$solution[K+1]

cat("MLE beta estimates:", beta_mle, "\n")
cat("MLE sigma estimate:", sigma_mle, "\n")
cat("Difference MLE beta vs true beta:", beta - beta_mle, "\n")
cat("Difference MLE sigma vs true sigma:", sigma - sigma_mle, "\n")
cat("Difference MLE beta vs closed-form OLS:", beta_OLS_closed - beta_mle, "\n\n")

cat("Step 6: Compute OLS using lm() function\n")


# Use lm() to compute OLS estimates
model_lm <- lm(Y ~ X - 1)

# Display results
cat("OLS estimates (lm):", coef(model_lm), "\n")
cat("Difference from true beta:", beta - coef(model_lm), "\n")
cat("Difference from closed-form OLS:", beta_OLS_closed - coef(model_lm), "\n\n")

cat("Step 7: Export regression output comparing multiple methods\n")

#Data frame with all our coefficient estimates
coef_df <- data.frame(
  term = paste0("X", 1:K),
  "TrueValues" = beta,
  "OLSlm" = as.numeric(coef(model_lm)),
  "ClosedForm" = as.numeric(beta_OLS_closed),
  "GradientDescent" = beta_GD,
  "LBFGS" = beta_lbfgs,
  "NelderMead" = beta_nm,
  "MLE" = beta_mle
)

write.csv(coef_df, "coefficient_comparison.csv", row.names = FALSE)

# Create a LaTeX table directly, done because I was having issues copying table into overleaf, and having trouble with modelsummary here.
cat("\\begin{table}[ht]\n", file = "coefficient_comparison.tex")
cat("\\centering\n", file = "coefficient_comparison.tex", append = TRUE)
cat("\\caption{Comparison of Estimation Methods}\n", file = "coefficient_comparison.tex", append = TRUE)
cat("\\begin{tabular}{l", paste(rep("r", ncol(coef_df)-1), collapse = ""), "}\n", 
    file = "coefficient_comparison.tex", append = TRUE)
cat("\\toprule\n", file = "coefficient_comparison.tex", append = TRUE)

# Write the header
header <- paste(names(coef_df), collapse = " & ")
cat(header, "\\\\\n", file = "coefficient_comparison.tex", append = TRUE)
cat("\\midrule\n", file = "coefficient_comparison.tex", append = TRUE)

# Write the data rows
for(i in 1:nrow(coef_df)) {
  row_data <- paste(sprintf("%.3f", unlist(coef_df[i, -1])), collapse = " & ")
  cat(coef_df$term[i], " & ", row_data, "\\\\\n", file = "coefficient_comparison.tex", append = TRUE)
}

cat("\\bottomrule\n", file = "coefficient_comparison.tex", append = TRUE)
cat("\\end{tabular}\n", file = "coefficient_comparison.tex", append = TRUE)
cat("\\end{table}\n", file = "coefficient_comparison.tex", append = TRUE)

#This allows for comparisons between the different methods as well as the "Easy Way" to the ground truth. 
cat("LaTeX table saved to 'coefficient_comparison.tex'\n\n")