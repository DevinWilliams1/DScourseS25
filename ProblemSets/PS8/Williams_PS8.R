library(nloptr)
library(tidyverse)

#Set random number generator
set.seed(100)

#Set dimensions of the problem
N <- 100000
K <- 10

#Create matrix with random normal values
X <- matrix(rnorm(N * (K-1)), nrow = N, ncol = K-1)

#A columns of 1 as the first column
X <- cbind(rep(1, N), X)

#Define sigma
sigma <- 0.5
sigma_squared <- sigma^2 #will be 0.25

#Generate error term eps ~ N (0, sigma^2)
eps <- rnorm(N, mean = 0 , sd = sigma)

#Verify length
length(eps)

#Look at first few rows
head(eps)

#Check variance (want it to be close to 0.25)
var(eps)

#UP TO PART 4, 3RD LINE