# Application of multi-class logistic to letters data

# Load the letter data
#########################
# Training data
letter_train <- read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[, 1]
X <- as.matrix(letter_train[, -1])

# Testing data
letter_test <- read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[, 1]
Xt <- as.matrix(letter_test[, -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
X <- cbind(1, X)
Xt <- cbind(1, Xt)


# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out <- LRMultiClass(...)
out <- LRMultiClass(X, Y, Xt, Yt)

# # Alternative inputs:
# # High lambda (regularization)
# out <- LRMultiClass(X, Y, Xt, Yt, lambda = 5)
# # Low lambda
# out <- LRMultiClass(X, Y, Xt, Yt, lambda = 0.1)
# # High eta (step size)
# out <- LRMultiClass(X, Y, Xt, Yt, eta = 0.8)
# # Low eta
# out <- LRMultiClass(X, Y, Xt, Yt, eta = 0.001)


# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, type = 'o')
plot(out$error_train, type = 'o')
plot(out$error_test, type = 'o')


# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors

# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.
library(microbenchmark)
check_runtime <- function(num_times) {
  benchmark <- microbenchmark(
    
    multinom_log_reg <- LRMultiClass(X, Y, Xt, Yt),
    times = num_times
    
  )
  return(benchmark)
}
check_runtime(num_times = 5L)

# [ToDo] Report the median time of your code from microbenchmark above in the comments below

# Median time: 8.142604 seconds (Intel chip)





