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
-sum(diag(y_indicator %*% t(log(p_k3)))) + ((lambda / 2) * sum(colSums(beta_init3^2)))








