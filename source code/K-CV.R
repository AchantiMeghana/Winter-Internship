################################################################################
######                                                                    ######
######                       K-fold Cross Validation                      ######
######                                                                    ######
################################################################################
library(pls)

######## Kfold for tenary classification


KfoldCV_tenary = function(k, Y, X, methods, pt_option = "perm_X", 
                          Y_type = "factor", standardize = F, delta_grid = NULL,
                          ER_sparse = F) {
  indicesPerGroup = extract(sample(1:nrow(X)), partition(nrow(X), k))
  
  Y_label <- Y
  
  if (pt_option == "perm_X")
    perm_col_ind <- sample(1:ncol(X))
  else if (pt_option == "perm_Y_before_split")
    perm_row_ind <- sample(1:nrow(X))
  else if (pt_option == "perm_X_Y") {
    perm_col_ind <- sample(1:ncol(X))
    perm_row_ind <- sample(1:nrow(X))
  }
  
  
  ACCs <- matrix(NA, k, length(methods))
  pred_mat <- matrix(NA, length(Y), length(methods))
  for (i in 1:k) {
    valid_ind <- indicesPerGroup[[i]]
    
    trainY <- Y[-valid_ind];    validY <- Y_label[valid_ind]
    trainY_label <- Y_label[-valid_ind]
    trainX <- X[-valid_ind,];   validX <- matrix(X[valid_ind,], ncol = ncol(X))
    
    for (j in 1:length(methods)) {
      method_j <- methods[j]
      if (method_j == "Perm-Lasso" | method_j == "Perm-ER") {
        if (pt_option == "perm_X") {
          perm_train_X <- trainX[,perm_col_ind]
          res_ij <- calAcc(validY, validX, perm_train_X, trainY, method_j, standardize,
                           trainY_label, Y_type)
        } else if (pt_option == "perm_X_Y") {
          perm_train_X <- trainX[,perm_col_ind]
          perm_train_Y <- Y[perm_row_ind][-valid_ind]
          perm_train_Y_label <- Y_label[perm_row_ind][-valid_ind]
          
          res_ij <- calAcc(validY, validX, perm_train_X, perm_train_Y, method_j, standardize,
                           perm_train_Y_label, Y_type)
        } else {
          if (pt_option == "perm_Y_before_split") {
            perm_train_Y <- Y[perm_row_ind][-valid_ind]
            perm_train_Y_label <- Y_label[perm_row_ind][-valid_ind]
          } else {
            perm_ind <- sample(1:nrow(trainX))
            perm_train_Y <- trainY[perm_ind]
            perm_train_Y_label <- trainY_label[perm_ind]
          }
          res_ij <- calAcc(validY, validX, trainX, perm_train_Y, method_j, standardize,
                           perm_train_Y_label, Y_type)
        }
      } else if (method_j == "ER-Lasso-LS" | method_j == "ER-Lasso-Dantzig") {
        
        if (!"ER" %in% methods)
          selected_delta = delta_grid
        
        res_ij <- calAcc(validY, validX, trainX, trainY, method_j, standardize, 
                         trainY_label, Y_type, selected_delta, ER_sparse)  
        
      } else {
        res_ij <- calAcc(validY, validX, trainX, trainY, method_j, standardize,
                         trainY_label, Y_type, delta_grid, ER_sparse)
        if (method_j == "ER")
          selected_delta = res_ij$delta
        
      }
      ACCs[i,j] <- res_ij$metric
      pred_mat[valid_ind, j] <- res_ij$pred
    }
  }
  colnames(ACCs) <- methods
  cat("Finishing one replciate ...\n")
  return(list(metric = apply(ACCs, 2, mean), pred = pred_mat))
}

pred_svm <- function(Y_label, trainX, validX, kernel = "linear", tune_flag = F, prob_flag = F) {
  if (tune_flag) {
    if (kernel == "linear")
      res_tune <- tune.svm(trainX, Y_label, cost = 2 ^ seq(-3, 3, 1))
    else if (kernel == "radial")
      res_tune <- tune.svm(trainX, Y_label, gamma = 2 ^ seq(-3, 3, 2), cost = 2 ^ seq(-3, 3, 1))
    else if (kernel == "polynomial") 
      res_tune <- tune.svm(trainX, Y_label, degree = 1:4, cost = 2 ^ seq(-3, 3, 1))
    return(predict(res_tune$best.model, validX))
  } else {
    fit_svm <- svm(trainX, Y_label, kernel = kernel)
    return(predict(fit_svm, validX, probability = prob_flag))
  }
}

calAcc = function(validY, validX, trainX, trainY, method, standardize, 
                  trainY_label, Y_type, delta_grid = NULL, ER_sparse = F) {
  if (Y_type == "factor")
    trainY <- trainY_label
  
  if (standardize) {
    train_X_stand <- scale(trainX, T, T)
    centers_X <- attr(train_X_stand, "scaled:center")
    scales_X <- attr(train_X_stand, "scaled:scale")
    test_X_stand <- t((t(validX) - centers_X) / scales_X)
  } else {
    train_X_stand <- scale(trainX, T, F)
    centers_X <- attr(train_X_stand, "scaled:center")
    test_X_stand <- t((t(validX) - centers_X))
  }
  train_Y_stand <- trainY - mean(trainY)
  train_Y_label <- as.factor(trainY_label)
  
  if (method == "Lasso" || method == "Perm-Lasso") {
    
    if (nrow(trainX) / 10 < 3) 
      cvfit = cv.glmnet(trainX, trainY, alpha = 1, nfolds = 5, standardize = F)
    else 
      cvfit = cv.glmnet(trainX, trainY, alpha = 1, nfolds = 10, standardize = F)
    
    beta_hat <- coef(cvfit, s = cvfit$lambda.min)[-1]
    supp_beta_hat <- which(beta_hat != 0)
    
    if (length(supp_beta_hat) == 0) 
      supp_beta_hat <- sample(1:ncol(trainX), 5)  # if lasso selects no variable, randomly pick 5 features instead
    
    res_pred <- pred_svm(train_Y_label, trainX[,supp_beta_hat], 
                         validX[,supp_beta_hat, drop = F])
    
  } else if (method == "PFR") {
    
    res_PCR <- PCR(train_X_stand, train_Y_stand, option = "ratio")
    res_pred <- pred_svm(train_Y_label, res_PCR$Z, test_X_stand %*% res_PCR$A)
    
  } else if (method == "ER" || method == "Perm-ER") {
    
    if (is.null(delta_grid))
      delta_grid <- seq(0.25, 0.7, 0.01)
    if (!ER_sparse) {
      res <- ER(train_Y_stand, train_X_stand, delta_grid, verbose = F)
      res_pred <- pred_svm(train_Y_label, train_X_stand %*% res$pred$Theta, test_X_stand %*% res$pred$Theta)
    } else {
      res <- ER(train_Y_stand, train_X_stand, delta_grid, verbose = F, CI = F, beta = "Dantzig")
      res_pred <- pred_svm(train_Y_label, train_X_stand %*% res$Q %*% res$beta, test_X_stand %*% res$Q %*% res$beta)
    }
    
    delta_grid <- res$optDelta
    
  } else if (method == "ER-Lasso-Dantzig" || method == "ER-Lasso-LS") {
    if (is.null(delta_grid))
      delta_grid <- seq(0.25, 0.7, 0.02)
    
    if (method == "ER-Lasso-Dantzig") {
      res <- ER(train_Y_stand, train_X_stand, delta_grid, verbose = F, CI = F, beta = "Dantzig")
      non_zero_beta <- which(res$beta != 0)
    } else {
      res <- ER(train_Y_stand, train_X_stand, delta_grid, verbose = F, CI = T, beta = "LS")
      p_vals <- 2 * pnorm(abs(res$beta) / sqrt(res$beta_var / length(train_Y_stand)), lower.tail = F)
      non_zero_beta <- which(p_vals <= 0.1)
    }
    if (length(non_zero_beta) == 0) {
      cat("There is no selected significant factor. All factors are used.\n")
      non_zero_beta <- 1:res$K
    }
    
    A_hat <- res$A
    feature_ind <- which(rowSums(abs(A_hat[,non_zero_beta, drop = F])) != 0)
    
    if (nrow(trainX) / 10 < 3) 
      cvfit = cv.glmnet(trainX[,feature_ind,drop = F], trainY, standardize = F,
                        nfolds = 5)
    else 
      cvfit = cv.glmnet(trainX[,feature_ind,drop = F], trainY, standardize = F,
                        nfolds = 10)
    
    beta_hat <- coef(cvfit, s = "lambda.min")[-1]
    supp_beta_hat <- which(beta_hat != 0)
    if (length(supp_beta_hat) == 0) 
      supp_beta_hat <- sample(1:ncol(trainX), 5)  # if lasso selects no variable, randomly pick 5 features instead
    
    res_pred <- pred_svm(train_Y_label, trainX[,supp_beta_hat], validX[,supp_beta_hat, drop = F])
    
  } else if (method == "PLS") {
    
    # whole_data <- data.frame(Y = c(trainY, validY), X = rbind(trainX, validX), row.names = NULL)
    # training_index <- 1:nrow(trainX)
    # fit_pls <- plsr(Y ~ ., data = whole_data[training_index, ], validation = "CV", segments = 5)
    # n_comp <- selectNcomp(fit_pls, method = "randomization")
    # n_comp <- ifelse(n_comp == 0, 1, n_comp)
    # A_hat <- fit_pls$loadings[, 1:n_comp]
    # res_pred <- pred_svm(train_Y_label, trainX %*% A_hat, validX %*% A_hat)
    
    fit_pls <- plsr(trainY ~ trainX, validation = "CV", segments = 5)
    n_comp <- selectNcomp(fit_pls, method = "randomization")
    n_comp <- ifelse(n_comp == 0, 1, n_comp)
    A_hat <- fit_pls$loadings[, 1:n_comp]
    res_pred <- pred_svm(train_Y_label, trainX %*% A_hat, validX %*% A_hat)
    
  } 
  list(metric = mean(validY == res_pred), pred = res_pred, delta = delta_grid)
}



KfoldCV_tenary_parallel = function(k, Y, X, methods, pt_option = "perm_X", 
                                   Y_type = "factor", standardize = F, delta_grid = NULL,
                                   ER_sparse = F) {
  # indicesPerGroup = extract(sample(1:nrow(X)), partition(nrow(X), k))
  indicesPerGroup = 1:nrow(X)
  
  Y_label <- Y
  
  if (pt_option == "perm_X")
    perm_col_ind <- sample(1:ncol(X))
  else if (pt_option == "perm_Y_before_split")
    perm_row_ind <- sample(1:nrow(X))
  else if (pt_option == "perm_X_Y") {
    perm_col_ind <- sample(1:ncol(X))
    perm_row_ind <- sample(1:nrow(X))
  }
  
  no_cores <- 10
  registerDoParallel(cores=no_cores) 
  cl <- makeCluster(no_cores)
  
  result <- foreach(i = 1:k, .combine = "rbind") %dopar% {
    valid_ind <- indicesPerGroup[[i]]
    
    trainY <- Y[-valid_ind];    validY <- Y_label[valid_ind]
    trainY_label <- Y_label[-valid_ind]
    trainX <- X[-valid_ind,];   validX <- matrix(X[valid_ind,], ncol = ncol(X))
    
    pred_vec <- c()
    
    for (j in 1:length(methods)) {
      method_j <- methods[j]
      if (method_j == "Perm-Lasso" | method_j == "Perm-ER") {
        if (pt_option == "perm_X") {
          perm_train_X <- trainX[,perm_col_ind]
          res_ij <- calAcc(validY, validX, perm_train_X, trainY, method_j, standardize,
                           trainY_label, Y_type)
        } else if (pt_option == "perm_X_Y") {
          perm_train_X <- trainX[,perm_col_ind]
          perm_train_Y <- Y[perm_row_ind][-valid_ind]
          perm_train_Y_label <- Y_label[perm_row_ind][-valid_ind]
          
          res_ij <- calAcc(validY, validX, perm_train_X, perm_train_Y, method_j, standardize,
                           perm_train_Y_label, Y_type)
        } else {
          if (pt_option == "perm_Y_before_split") {
            perm_train_Y <- Y[perm_row_ind][-valid_ind]
            perm_train_Y_label <- Y_label[perm_row_ind][-valid_ind]
          } else {
            perm_ind <- sample(1:nrow(trainX))
            perm_train_Y <- trainY[perm_ind]
            perm_train_Y_label <- trainY_label[perm_ind]
          }
          res_ij <- calAcc(validY, validX, trainX, perm_train_Y, method_j, standardize,
                           perm_train_Y_label, Y_type)
        }
      } else if (method_j == "ER-Lasso-LS" | method_j == "ER-Lasso-Dantzig") {
        
        if (!"ER" %in% methods)
          selected_delta = delta_grid
        
        res_ij <- calAcc(validY, validX, trainX, trainY, method_j, standardize, 
                         trainY_label, Y_type, selected_delta, ER_sparse)  
        
      } else {
        res_ij <- calAcc(validY, validX, trainX, trainY, method_j, standardize,
                         trainY_label, Y_type, delta_grid, ER_sparse)
        if (method_j == "ER")
          selected_delta = res_ij$delta
        
      }
      pred_vec[j] <- res_ij$pred
    }
    pred_vec
  }
  
  stopCluster(cl)  
  
  return(result)
}


Compute_metric <- function(pred_val, Y_label) {
  methods <- colnames(pred_val)
  error_list <- auc_vec <- c()
  for (l in 1:length(methods)) {
    pred_roc <- prediction(pred_val[,l], Y_label)
    perf <- performance(pred_roc, "tpr", "fpr")
    error_list[[l]] <- rbind("tpr" = perf@y.values[[1]], "fpr" = perf@x.values[[1]])
    perf_auc <- performance(pred_roc, "auc")
    auc_vec[l] <- as.numeric(perf_auc@y.values)
  }
  names(error_list) <- methods
  return(list(error = error_list, auc = auc_vec))
}



Plot_ROC <- function(mean_errors, methods, legend_loc = "topleft") {
  par(mar = c(4,4,1,1))
  n_method <- length(mean_errors)
  sel_names <- c()
  for (i in 1:n_method) {
    if (! names(mean_errors)[i] %in% methods)
      next
    sel_names <- c(sel_names, names(mean_errors)[i])
    count <- length(sel_names)
    Y_i <- mean_errors[[i]]['tpr',]
    X_i <- mean_errors[[i]]['fpr',]
    if (count == 1)
      plot(X_i, Y_i, type = "l", col = count + 1, lty = count, main = "", xlab = "False positive rate",
           ylab = "True positive rate", ylim = c(0, 1), xlim = c(0, 1), lwd = 2)
    else
      points(X_i, Y_i, type = "l", col = count + 1, lty = count, lwd = 2)
  }
  abline(a = 0, b = 1, col = 1, lty = 2, lwd = 1)
  legend(legend_loc, legend = sel_names, col = 2:(count+1), lty = 1:count, lwd = rep(2, count), bty = "n")
}











partition = function(totalNumb, numbGroup) {
  # This function returns a vector of numbers for each group given the total number of 
  # observations and the total number of groups
  # eg: divide 9 obs into 4 groups should give c(3:2:2:2)
  # args: {@code totalNumb} "integer", # of obs
  # return: a vector of length equal to {@code numbGroup}
  
  remainder = totalNumb %% numbGroup # get the remainder
  numbPerGroup = totalNumb %/% numbGroup
  rep(numbPerGroup,numbGroup) + c(rep(1,remainder),rep(0,numbGroup-remainder))
}


extract = function(preVec, indices) {
  # Extract the indices from the previous vector and return as a list with length equal to length of {@code indices}
  # args: preVec: a vector
  # args: indices: contains the length of each group to be extracted
  # return: list[length=length(indices)]
  
  newVec = vector("list",length(indices))
  newVec[[1]] = preVec[1:indices[1]]
  for (i in 2:length(indices)) {
    tmpIndices = sum(indices[1:(i-1)]) + 1
    newVec[[i]] = preVec[tmpIndices:(tmpIndices+indices[i]-1)] 
  }
  return(newVec)
}










# #### KfoldCV for Essential Regression
# 
# 
# KfoldCV_ER = function(k, Y, X, delta_grid, standardize) {
#   ## Input: Both Y and X are centered
# 
#   indicesPerGroup = extract(sample(1:nrow(X)), partition(nrow(X), k))
# 
#   MSEs <- c()
#   for (i in 1:k) {
#     valid_ind <- indicesPerGroup[[i]]
# 
#     trainY <- Y[-valid_ind];    validY <- Y[valid_ind]
#     trainX <- X[-valid_ind,];   validX <- X[valid_ind,]
# 
#     if (standardize) {
#       train_X_stand <- scale(trainX, T, T)
#       centers_X <- attr(train_X_stand, "scaled:center")
#       scales_X <- attr(train_X_stand, "scaled:scale")
#       test_X_stand <- t((t(validX) - centers_X) / scales_X)
#     } else {
#       train_X_stand <- scale(trainX, T, F)
#       centers_X <- attr(train_X_stand, "scaled:center")
#       test_X_stand <- t((t(validX) - centers_X))
# 
#       # train_X_stand <- trainX
#       # test_X_stand <- validX
#     }
#     train_Y_stand <- trainY - mean(trainY)
#     test_Y_stand <- validY - mean(trainY)
# 
#     MSEs <- rbind(MSEs, sapply(delta_grid, calMSE_ER, validY = test_Y_stand, validX = test_X_stand,
#                                trainX = train_X_stand, trainY = train_Y_stand))
# 
#   }
#   apply(MSEs, 2, mean)
# }
# 
# 
# calMSE_ER = function(validY, validX, trainX, trainY, delta) {
#   res <- ER(trainY, trainX, delta)
#   Mse(validY, validX %*% res$pred$theta)
# }