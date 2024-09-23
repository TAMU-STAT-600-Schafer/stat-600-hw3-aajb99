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

