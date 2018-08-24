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
#' @return A Matrix of errors for each fold and each number of LVs. The rows
#'   indicate the fold, the columns indicate the max number of LVs.
#' @export
cross_validate_node_PLS <- function(node, max_n_LVs, k_folds=10, error_function=MSE){

  train_errors <- matrix(0, nrow=k_folds, ncol=max_n_LVs)
  test_errors  <- matrix(0, nrow=k_folds, ncol=max_n_LVs)

  test_indices <- caret::createFolds(1:nrow(node$X_data), k = k_folds) #indices of test set

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