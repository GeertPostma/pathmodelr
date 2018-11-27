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
cross_validate_node_PLS <- function(node, max_n_LVs, k_folds=10, error_function=MSE, n_cores=1, end_node=FALSE, manifest=FALSE){

  train_errors <- matrix(0, nrow=k_folds, ncol=max_n_LVs)
  test_errors  <- matrix(0, nrow=k_folds, ncol=max_n_LVs)

  test_indices <- createFolds(1:nrow(node$X_data), k = k_folds) #indices of test set

  # Internal help function for cross validation for node_PLS
  get_errors <- function(test_indices){

    if(end_node){
      preprocessed_Y_sets <- node$preprocess_train_test(test_indices)

      Y_train <- preprocessed_Y_sets$train_data
      Y_test  <- preprocessed_Y_sets$test_data

      n_X_cols <- 0
      X_indices <- list()
      for(i in seq_along(node$previous_nodes)){
        X_indices[[i]] <- (n_X_cols+1):(n_X_cols + dim(node$previous_nodes[[i]]$preprocessed_X)[2])
        n_X_cols <- n_X_cols + dim(node$previous_nodes[[i]]$preprocessed_X)[2]
      }
      n_samples_train <- nrow(node$X_data[-test_indices, , drop=FALSE])
      n_samples_test  <- nrow(node$X_data[test_indices, , drop=FALSE])

      X_train <- matrix(0, nrow=n_samples_train, ncol=n_X_cols)
      X_test  <- matrix(0, nrow=n_samples_test,  ncol=n_X_cols)

      for(i in seq_along(node$previous_nodes)){
        preprocessed_X_sets <- node$previous_nodes[[i]]$preprocess_train_test(test_indices)

        X_train[,X_indices[[i]]] <- preprocessed_X_sets$train_data
        X_test[,X_indices[[i]]]  <- preprocessed_X_sets$test_data
      }

    }
    else{
      preprocessed_X_sets <- node$preprocess_train_test(test_indices)

      X_train <- preprocessed_X_sets$train_data
      X_test  <- preprocessed_X_sets$test_data

      if(manifest){ #manifest+start/middle node
        n_Y_cols <- 0
        Y_indices <- list()
        for(i in seq_along(node$next_nodes)){
          Y_indices[[i]] <- (n_Y_cols+1):(n_Y_cols + dim(node$next_nodes[[i]]$preprocessed_X)[2])
          n_Y_cols <- n_Y_cols + dim(node$next_nodes[[i]]$preprocessed_X)[2]
        }
        n_samples_train <- nrow(node$X_data[-test_indices,])
        n_samples_test  <- nrow(node$X_data[test_indices,])

        Y_train <- matrix(0, nrow=n_samples_train, ncol=n_Y_cols)
        Y_test  <- matrix(0, nrow=n_samples_test,  ncol=n_Y_cols)

        for(i in seq_along(node$next_nodes)){
          preprocessed_Y_sets <- node$next_nodes[[i]]$preprocess_train_test(test_indices)

          Y_train[,Y_indices[[i]]] <- preprocessed_Y_sets$train_data
          Y_test[,Y_indices[[i]]]  <- preprocessed_Y_sets$test_data
        }
      }
      else{ #LV+start/middle node
        n_Y_cols <- 0
        Y_indices <- list()
        for(i in seq_along(node$next_nodes)){
          Y_indices[[i]] <- (n_Y_cols+1):(n_Y_cols + dim(node$next_nodes[[i]]$LVs)[2])
          n_Y_cols <- n_Y_cols + dim(node$next_nodes[[i]]$LVs)[2]
        }
        n_samples_train <- nrow(node$X_data[-test_indices,])
        n_samples_test  <- nrow(node$X_data[test_indices,])

        Y_train <- matrix(0, nrow=n_samples_train, ncol=n_Y_cols)
        Y_test  <- matrix(0, nrow=n_samples_test,  ncol=n_Y_cols)

        for(i in seq_along(node$next_nodes)){
          Y_train[,Y_indices[[i]]] <- node$next_nodes[[i]]$LVs[-test_indices,]
          Y_test[,Y_indices[[i]]]  <- node$next_nodes[[i]]$LVs[test_indices,]
        }
      }
    }

    SIMPLS_result <- SIMPLS(X_train, Y_train, max_n_comp=max_n_LVs, minimal=TRUE)
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

  #parallelise when multiple cores should be used.
  if(n_cores > 1){
    cl <- makeCluster(n_cores)
    errors <- parLapply(cl, test_indices, get_errors)

    stopCluster(cl)
  }
  else{
    errors <- lapply(test_indices, get_errors)
  }


  for(i in 1:k_folds){
    train_errors[i,] <- errors[[i]]$train_errors
    test_errors[i,] <- errors[[i]]$test_errors
  }
  return(list("train_errors"=train_errors, "test_errors"=test_errors))


}


