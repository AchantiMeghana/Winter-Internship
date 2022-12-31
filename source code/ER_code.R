# For Achanti
rm(list=ls())
setwd("C:/Users/Meghana Achanti/Documents/R/R/Regression/Linear Regression/Essential Regression")
source("SupLOVE.R")
source("K-CV.R")
source("Helper.R")
source("Other_algorithms.R")
library(readr)

## Input

## User provide X and Y separately

## User provide the method between dantzig and LS

## User provide lambda and delta

### Output
# res, Z_hat and if
# Method=dantzig selected order_coef_beta_DZ
# If method=LS is selected order_coef_beta_LS

data <- read_delim("C:/Users/Meghana Achanti/Documents/R/R/Regression/Linear Regression/Essential Regression/age_all_days_imputed_5NN.txt", " ",
                   escape_double = FALSE, trim_ws = TRUE)
Y <- data$age
X <- data[,-(1:2)]

feature_names <- names(X)

################################################################################
#######                       ER on the whole dataset                    #######
################################################################################

X <- scale(X, T, T)
Y <- Y - mean(Y)
#
# set.seed(20200316)
# delta_grid <- seq(0.35, 0.75, 0.01)
#
# res <- ER(Y, X, delta_grid, pred = F, beta_est = "NULL", rep_CV = 50, verbose = T)

# res$K
# selected delta = 0.38

delta = 0.38 ## User provide this
lbd = 0.5 ## User privide this

res <- ER(Y, X, delta = delta, pred = F, beta_est = "LS", rep_CV = 50, verbose = T,
          merge = F, CI = T, correction = NULL, lbd = lbd)
res$K
### extract estimated beta, CIs and p-vals

# extract beta_hat and its standardized version
est_beta_LS <- res$beta
est_beta_LS_scaled <- diag(sqrt(diag(res$C))) %*% est_beta_LS
# return the ordered ranking based on the absolute values
order_coef_beta_LS <- order(abs(est_beta_LS_scaled), decreasing = T)
# 45  1 19 41  2 49 46 15  7 24 44 17 42  5 53  9 52 36

round(res$beta_CIs, 3)
p_vals <- 2 * pnorm(abs(res$beta / sqrt(res$beta_var / length(Y))), lower.tail = F)

res_dz <- ER(Y, X, delta = delta, pred = F, beta_est = "Dantzig", rep_CV = 50, verbose = T,
             merge = F, CI = T, correction = NULL, lbd = lbd)
round(res_dz$beta, 3)
Z_ind <- which(res_dz$beta != 0)

# extract beta_hat and its standardized version
est_beta_DZ <- res_dz$beta
est_beta_DZ_scaled <- diag(sqrt(diag(res_dz$C))) %*% est_beta_DZ
# return the ordered ranking based on the absolute values
order_coef_beta_DZ <- order(abs(est_beta_DZ_scaled), decreasing = T)

A_hat = res$A

Z_hat <- X %*% A_hat %*% solve(crossprod(A_hat))
Z_tilde <- Pred_Z_BLP(X, A_hat, res$C, res$Gamma, 1:res$K)
