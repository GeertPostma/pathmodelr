cross_validate_node_PLS <- function(node, max_n_LVs, k_folds=10, error_function=MSE){
  
  train_errors <- matrix(0, nrow=k_folds, ncol=max_n_LVs)
  test_errors  <- matrix(0, nrow=k_folds, ncol=max_n_LVs)
  
  test_indices <- createFolds(1:nrow(node$X_data), k = k_folds) #indices of test set
  
  for(i in 1:k_folds){
    combined_and_masked <- combine_and_mask(node, test_indices[[i]])
    X_train <- as.matrix(combined_and_masked$X_train)
    X_test  <- as.matrix(combined_and_masked$X_test)
    
    Y_train <- as.matrix(combined_and_masked$Y_train)
    Y_test  <- as.matrix(combined_and_masked$Y_test)
    
    covariance_mask <- combined_and_masked$covariance_mask
    
    SIMPLS_result <- SIMPLS(X_train, Y_train, max_n_comp=max_n_LVs, minimal=TRUE, covariance_mask=covariance_mask)
    B <- SIMPLS_result$coefficients
    
    for(j in 1:max_n_LVs){
      Y_train_pred <- X_train %*% B[, , j]
      Y_test_pred  <- X_test %*% B[, , j]
      
      train_errors[i,j] <- error_function(Y_train, Y_train_pred)
      test_errors[i,j] <- error_function(Y_test, Y_test_pred)
    }
  }

  return(list("train_errors"=train_errors, "test_errors"=test_errors))
}