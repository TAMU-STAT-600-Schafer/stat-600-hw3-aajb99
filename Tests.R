# This is a script to save your own tests for the function
source("FunctionsLR.R")

##################
# Checks testing #
##################

# Check 1: check if first column are 1's:

X_good <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
X_bad <- matrix(c(1, 2, 3, 3, 1, 4), nrow = 3, byrow = TRUE)
!all(X_good[,1] == 1)
!all(X_bad[,1] == 1)

# Check LRMultiClass:
LRMultiClass(X = X_good, y = c(1,2,1), Xt = X_bad, yt = c(1,2,1))
LRMultiClass(X = X_good, y = c(1,2,1), Xt = X_good, yt = c(1,2,1))


# Check 2: check if X and y are dim compatible:

X_good <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
y_good <- c(1, 2, 3)
y_bad <- c(1,2,3,4,4)
dim(X_good)[1] != length(y_good)
dim(X_good)[1] != length(y_bad)

# Check LRMultiClass:
LRMultiClass(X_good, y_bad, Xt = X_good, yt = y_good)


# Check 3: X and Xt are dim compatible:
X <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
Xt_bad <- matrix(c(1, 2, 1, 3, 1, 4, 1, 5), nrow = 4, byrow = TRUE)

# Check LRMultiClass:
LRMultiClass(X, y_good, Xt = Xt_bad, yt = c(1, 2, 3, 4))

if (dim(X)[1] != dim(Xt_bad)[1] | dim(X)[2] != dim(Xt_bad)[2]) {
  
  stop(paste("Test Works."))
  
}


# Check 4: eta is positive:
X <- matrix(c(1, 2, 1, 2, 1, 4), nrow = 3, byrow = TRUE)
Xt <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
y <- c(1, 2, 3)
yt <- c(1, 2, 4)

# Check LRMultiClass:
LRMultiClass(X, y, Xt, yt, eta = 1)

# Check 5: lambda is non-negative:
X <- matrix(c(1, 2, 1, 2, 1, 4), nrow = 3, byrow = TRUE)
Xt <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
y <- c(1, 2, 3)
yt <- c(1, 2, 4)

# Check LRMultiClass:
LRMultiClass(X, y, Xt, yt, lambda = -1)


# Check 5: beta_init:
X <- matrix(c(1, 2, 1, 2, 1, 4), nrow = 3, byrow = TRUE)
Xt <- matrix(c(1, 2, 1, 3, 1, 4), nrow = 3, byrow = TRUE)
y <- c(0, 1, 2)
yt <- c(1, 2, 4)

K <- length(unique(y))

beta_init0 <- matrix(NaN, nrow = 2, ncol = 2)
beta_init0 <- NULL
all(is.null(beta_init0)) | all(is.na(beta_init0))

beta_init <- matrix(0, nrow = dim(X)[2], ncol = K)
beta_init
beta_init_badp <- matrix(c(1, 2, 3, 4, 5, 6, 6, 7, 4), nrow = 3)
beta_init_badk <- matrix(c(1, 2, 3, 4, 5, 6, 6, 7), nrow = 2)
beta_init_good <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)

# Check LRMultiClass:
LRMultiClass(X, y, Xt, yt, beta_init = NULL)
LRMultiClass(X, y, Xt, yt, beta_init = beta_init_badp)
LRMultiClass(X, y, Xt, yt, beta_init = beta_init_badk)
LRMultiClass(X, y, Xt, yt, beta_init = beta_init_good)



###############
# p_k testing #
###############
# X: also used in later testing
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2, 4, 3, 5, 4, 1, 1, 8, 4, 1, 1, 3, 8), nrow = 5, byrow = FALSE)
beta_init <- matrix(c(2, 3, 2, 3, 5, 6, 7, 6), nrow = 4, ncol = 2, byrow = FALSE)
beta_init2 <- matrix(c(1, 2, 4, 4, 1, 3, 4, 1, 1, 2, 7, 7), nrow = 4, ncol = 3, byrow = FALSE)
beta_init3 <- matrix(rnorm(12, sd = 0.1), nrow = 4, ncol = 3)

# p_k configuration (1):
# Numerator
Xb <- X %*% beta_init
exp_Xb <- exp(Xb)

# Denominator
sum_exp_Xb <- rowSums(exp_Xb)

# p_k estimate
p_k <- exp_Xb / rowSums(exp_Xb)

###

# p_k configuration (test 2):
Xb2 <- X %*% beta_init2
exp_Xb2 <- exp(Xb2)

# Denominator
sum_exp_Xb2 <- rowSums(exp_Xb2)

# p_k estimate (2)
p_k2 <- exp_Xb2 / sum_exp_Xb2

###

# p_k configuration (test 3):
Xb3 <- X %*% beta_init3
exp_Xb3 <- exp(Xb3)

# Denominator
sum_exp_Xb3 <- rowSums(exp_Xb3)

# p_k estimate (2)
p_k3 <- exp_Xb3 / sum_exp_Xb3

# Test LRMultiClass:
# Results in p_k3 (add return statement after p_k computation in LRMultiClass)
LRMultiClass(X, y = c(1, 2, 3, 3, 3), Xt = X, yt = c(1, 2, 3, 3, 3), beta_init = beta_init3)


##############################
# Objective Function testing #
##############################
# lambda:
lambda1 = 2

# log(p):
log(p_k3)

# Indicator function:
y_p_k3 = c(1, 3, 2, 2, 1)
model.matrix(~ y_p_k3 - 1)

# Full Objective Function: #
y_p_k3_factor <- as.factor(y_p_k3)
y_indicator <- model.matrix(~ y_p_k3_factor - 1)

-(y_indicator %*% t(log(p_k3))) + ((lambda / 2) * sum(colSums(beta_init3^2))) # Multinomial Logistic
# Method to sum above matrix (trace method)
-sum(diag(y_indicator %*% t(log(p_k3))))

# objective function in full:
-sum(diag(y_indicator %*% t(log(p_k3)))) + ((lambda1 / 2) * sum(colSums(beta_init3^2)))

# Simplify objective function for optimal runtime:
log_p <- log(p_k3)
neg_log_lik <- -sum(y_indicator * log_p)
ridge_reg <- (lambda1 / 2) * sum(beta_init3^2)
neg_log_lik + ridge_reg

# Check LRMultiClass:
# Results using p_k3 (add return statement after obj_val computation in LRMultiClass)
# (Results match above full objective function)
LRMultiClass(X, y = y_p_k3, Xt = X, yt = y_p_k3, beta_init = beta_init3, lambda = lambda1)


###########################
# Newton's Method Testing #
###########################

# Test solve() function:

a <- matrix(c(1, 2, 3, 4), nrow = 2)
a_inv <- solve(a) %*% a
b <- matrix(c(1, 2), nrow = 2)
a_inv %*% b
solve(a, b)

###

eta1 <- 1

# Test W term (Hessian calculation):
W_test <- diag(p_k3 %*% t(1 - p_k3))
p_k3[, 1] * (1 - p_k3[, 1])

p_k3[1,] %*% (1 - p_k3)[1,]

# W by row:
W_test <- vector()
for (j in 1:nrow(p_k3)){
  row_prod <- p_k3[j,] %*% (1 - p_k3)[j,]
  W_test <- append(W_test, row_prod)
}
W_test

# W by col:
W_test <- vector()
for (j in 1:nrow(p_k3)){
  row_prod <- p_k3[j,] %*% (1 - p_k3)[j,]
  W_test <- append(W_test, row_prod)
}
W_test

# Damped Newtons Update:
Identity1 <- diag(1, nrow = ncol(X))
beta_new_test <- beta_init3 - ((eta1 * solve(t(X * W_test) %*% X + (lambda1 * Identity1))) %*% 
  (t(X) %*% (p_k3 - y_indicator) + (lambda1 * beta_init3)))

# Check LRMultiClass:
LRMultiClass(X, y = y_p_k3, Xt = X, yt = y_p_k3, beta_init = beta_init3, lambda = lambda1, eta = eta1)


#### Newton's Method: looping through K clusters ####

############################
# Objects:
X <- matrix(c(1, 1, 1, 1, 1, 2, 3, 2, 4, 3, 5, 4, 1, 1, 8, 4, 1, 1, 3, 8), nrow = 5, byrow = FALSE)
beta_init3 <- matrix(rnorm(12, sd = 0.1), nrow = 4, ncol = 3)
Xb3 <- X %*% beta_init3
exp_Xb3 <- exp(Xb3)
# Denominator
sum_exp_Xb3 <- rowSums(exp_Xb3)
# p_k estimate (2)
p_k3 <- exp_Xb3 / sum_exp_Xb3
y_p_k3 = c(1, 3, 2, 2, 1)
y_p_k3_factor <- as.factor(y_p_k3)
y_indicator <- model.matrix(~ y_p_k3_factor - 1)

lambda1 = 2
eta1 <- 1
Identity1 <- diag(1, nrow = ncol(X))

############################
# Inner for loop (by class)

for (k in 1:ncol(p_k3)){
    
  # W term configuration:
  W_test <- p_k3[, k] * (1 - p_k3[, k])
  
  # Generating Function Update:
  g <- t(X) %*% (p_k3[, k] - y_indicator[, k]) + (lambda1 * beta_init3[, k])
  # Hessian Update:
  h <- t(X * W_test) %*% X + (lambda1 * Identity1)
  # Damped Newton's Update:
  beta_init3[, k] <- beta_init3[, k] -eta1 * (solve(h) %*% g)
    
}

Xb <- X %*% beta_init3
exp_Xb <- exp(Xb)
# Denom
sum_exp_Xb <- rowSums(exp_Xb)
# pk:
p_k3_new <- exp_Xb / rowSums(exp_Xb)


#######################################################
# Check LRMultiClass with implemented Newton's Updater:
p_k_test <- LRMultiClass(X, y = y_p_k3, Xt = X, yt = y_p_k3, beta_init = beta_init3, lambda = lambda1, eta = eta1)



####################
# Test/Train Error #
####################
Xt <- matrix(c(1, 1, 1, 1, 2, 3, 2, 4, 3, 5, 4, 1, 1, 8, 4, 1), nrow = 4, byrow = FALSE)
yt <- matrix(c(1, 3, 2, 2)) - 1

# Training/Testing Error:
p_k_test

y_preds <- apply(p_k_test, 1, which.max) - 1
y_p_k3 <- y_p_k3 - 1

# Compute percent
mean(y_preds == y_p_k3)
# Deviating vectors:
y_preds[2] <- 0
y_preds_dev <- y_preds
(1 - mean(y_preds_dev == y_p_k3)) * 100

#######################################################
# Test first iteration errors:

# Ensure y objects below are range 0 - K-1:
yt <- as.vector(yt)
y_p_k3
# Check LRMultiClass with Errors implemented:
LRMultiClass(X, y = y_p_k3, Xt = Xt, yt = yt, beta_init = beta_init3, lambda = lambda1, eta = eta1)
# Check if matches the following computation:
Xb <- X %*% beta_init3
exp_Xb <- exp(Xb)
# Denom
sum_exp_Xb <- rowSums(exp_Xb)
# pk:
p_k3 <- exp_Xb / sum_exp_Xb

# For Xt:
# Num
Xtb <- Xt %*% beta_init3
exp_Xtb <- exp(Xtb)
# Denom
sum_exp_Xtb <- rowSums(exp_Xtb)
# pk:
p_kt <- exp_Xtb / sum_exp_Xtb
# Errors:
y_preds <- apply(p_k3, 1, which.max) - 1
yt_preds <- apply(p_kt, 1, which.max) - 1
# Compute percent
mean(y_preds == y_p_k3) # MATCH
mean(yt_preds == yt) # MATCH

######################################
# Test final iteration errors:
# (after implementing errors in loops)
LRMultiClass(X, y = y_p_k3, Xt = Xt, yt = yt, beta_init = beta_init3, lambda = lambda1, eta = eta1)


#######################################################
# Test Xt and yt (testing) objects in p_kt computation:

# For Xt:
# Num
Xtb <- Xt %*% beta_init3
exp_Xtb <- exp(Xtb)
# Denom
sum_exp_Xtb <- rowSums(exp_Xtb)
# pk:
p_kt <- exp_Xtb / sum_exp_Xtb

# For X:
# Num
Xb <- X %*% beta_init3
exp_Xb <- exp(Xb)
# Denom
sum_exp_Xb <- rowSums(exp_Xb)
# pk:
p_k <- exp_Xb / sum_exp_Xb



# Additional Testing #
# Vector
a <- vector()
a <- append(a, 1)



###############################
# Garbage Collection Analysis #
###############################

# (Applied using objects from above from various tests)
library(profvis)

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

# Profile:
#   GC (garbage collection: freeing memory from objects no longer in use)
profvis(
  {
    LRMultiClass(X, Y, Xt, Yt)
  }
)












