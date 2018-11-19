#' Cross validates Node based Partial Least Squares Regression
#'
#' This functions calculates errors given a function for k-fold cross validation
#' for each number of LVs up to a maximum number of LVs.
#'
#' When the number is higher than the number of variables in the internal X
#' matrix, the function might display unwanted behaviour.
#'
#' @param node An object of the R6Class Node which is initialised and estimated
#'   at least once. The node needs to be connected to at least one target node
#'   for the Y matrices to exist.
#' @param max_n_LVs An integer indicating the max number of LVs that are to be
#'   evaluated.
#' @param k_folds An integer indicating the number of folds that are to be
#'   evaluated for the k-fold cross validation. It can not be higher than the
#'   number of samples.
#' @param error_function A function handle for an error function which takes two
#'   equal size matrices and returns a single double.
#' @return A list containing the test error and training error, both a Matrix of
#'   errors for each fold and each number of LVs. The rows indicate the fold,
#'   the columns indicate the max number of LVs.
#' @importFrom caret createFolds
#' @import parallel
#' @export
cross_validate_node_PLS <- function(node, max_n_LVs, k_folds=10, error_function=MSE, n_cores=1, scale_blocks=FALSE, variance_scale=FALSE, minimal=TRUE){

  if(!minimal){
    train_errors <- matrix(0, nrow=k_folds, ncol=max_n_LVs)
  }

  test_errors  <- matrix(0, nrow=k_folds, ncol=max_n_LVs)

  test_indices <- createFolds(1:nrow(node$X_data), k = k_folds) #indices of test set

  # Internal help function for cross validation for node_PLS
  get_errors <- function(test_indices){
    combined_and_masked <- combine_and_mask(node, test_indices, scale_blocks=scale_blocks, variance_scale=variance_scale)
    X_train <- as.matrix(combined_and_masked$X_train)
    X_test  <- as.matrix(combined_and_masked$X_test)

    Y_train <- as.matrix(combined_and_masked$Y_train)
    Y_test  <- as.matrix(combined_and_masked$Y_test)

    covariance_mask <- combined_and_masked$covariance_mask

    SIMPLS_result <- SIMPLS(X_train, Y_train, max_n_comp=max_n_LVs, minimal=TRUE, covariance_mask=covariance_mask)
    B <- SIMPLS_result$coefficients

    train_errors <- matrix(0, nrow=1, ncol=max_n_LVs)
    test_errors  <- matrix(0, nrow=1, ncol=max_n_LVs)

    for(j in 1:max_n_LVs){
      Y_train_pred <- X_train %*% B[, , j]
      Y_test_pred  <- X_test %*% B[, , j]

      train_errors[1,j] <- error_function(Y_train, Y_train_pred)
      test_errors[1,j] <- error_function(Y_test, Y_test_pred)
    }

    return(list("train_errors"=train_errors, "test_errors"=test_errors))
  }

  get_error <- function(test_indices){
    combined_and_masked <- combine_and_mask(node, test_indices, scale_blocks=scale_blocks, variance_scale=variance_scale)
    X_train <- as.matrix(combined_and_masked$X_train)
    X_test  <- as.matrix(combined_and_masked$X_test)

    Y_train <- as.matrix(combined_and_masked$Y_train)
    Y_test  <- as.matrix(combined_and_masked$Y_test)

    covariance_mask <- combined_and_masked$covariance_mask

    SIMPLS_result <- SIMPLS(X_train, Y_train, max_n_comp=max_n_LVs, minimal=TRUE, covariance_mask=covariance_mask)
    B <- SIMPLS_result$coefficients

    test_errors  <- matrix(0, nrow=1, ncol=max_n_LVs)

    for(j in 1:max_n_LVs){
      Y_test_pred  <- X_test %*% B[, , j]

      test_errors[1,j] <- error_function(Y_test, Y_test_pred)
    }

    return(list("test_errors"=test_errors))
  }

  #parallelise when multiple cores should be used.
  if(n_cores > 1){
    cl <- makeCluster(n_cores)
    if(minimal){
      errors <- parLapply(cl, test_indices, get_error)
    }
    else{
      errors <- parLapply(cl, test_indices, get_errors)
    }

    stopCluster(cl)
  }
  else{
    if(minimal){
      errors <- lapply(test_indices, get_error)
    }
    else{
      errors <- lapply(test_indices, get_errors)
    }
  }

  if(minimal){
    for(i in 1:k_folds){
      test_errors[i,] <- errors[[i]]$test_errors
    }
    return(list("test_errors"=test_errors))
  }
  else{
    for(i in 1:k_folds){
      train_errors[i,] <- errors[[i]]$train_errors
      test_errors[i,] <- errors[[i]]$test_errors
    }
    return(list("train_errors"=train_errors, "test_errors"=test_errors))
  }


}


