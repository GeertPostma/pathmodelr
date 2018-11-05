#' Estimate a node using PLS regression
#'
#' Estimates the LVs of the given node, and all nodes on the same level, those
#' used as predictors, using PLS regression with all next level nodes as
#' targets.
#'
#' The algorithm determines which nodes should be used as predictors, and which
#' as targets given the connection matrix of the path model. The data of these
#' nodes is concatenated into a single X data block, and a single Y data block.
#'
#' The cross product matrix used in SIMPLS regression is set to zero at the
#' connections between the target and predictor variables which are not defined
#' as being connected in the path model specification.
#'
#' The function has no explicit return value, instead the R6Class node object,
#' and all same level connected nodes, are updated.
#'
#' The X_loadings which are assigned to the node are equal to the part of the R
#' matrix, the X_weights resulting from the SIMPLS algorithm, and are subsetted
#' to only use that part which corresponds to the manifest variables
#' corresponding to that block. The estimated LVs are then defined as the
#' projection of the data in the to be estimated node projected onto the partial
#' X_weights.
#'
#' The scaling in the original SIMPLS algorithm normalizes each LV, meaning that
#' the LV is scaled to have a length of 1. Because the R matrix is subsetted,
#' the resulting LVs per node are of shorter length than 1.
#'
#' @param node An object of the R6Class Node which is initialised and estimated
#'   at least once. The node needs to be connected to at least one target node
#'   for the Y matrices to exist.
#' @export
PLS_estimator <- function(node, parallelise=FALSE, n_cores=NULL){

  #Combine the data from the nodes and mask the covariance matrix accordingly
  combined_and_masked <- combine_and_mask(node)
  X <- combined_and_masked$X
  Y <- combined_and_masked$Y
  covariance_mask <- combined_and_masked$covariance_mask
  cols_per_X_node <- combined_and_masked$cols_per_X_node
  cols_per_Y_node <- combined_and_masked$cols_per_Y_node
  same_level_nodes <- combined_and_masked$same_level_nodes

  #determine max_n_LVs: after first selection, only allow shrinking
  max_n_LVs <- ifelse(node$iteration > 1, dim(node$previous_LVs)[2], dim(X)[2])

  if(parallelise){
    if(!is.null(n_cores)){
      test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=MSE, n_cores=n_cores)$test_errors
    }
    else{
      stop("parallelise is set to TRUE, but n_cores was not set.")
    }
  }
  else{
    test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=MSE)$test_errors
  }

  #n_LV selection: take lowest complexity model within 1 std of the lowest error
  avg_test_error <- colSums(test_errors)
  std_test_error <- apply(test_errors, 2, sd)

  min_error_index <- which.min(avg_test_error)

  ref_error <- avg_test_error[min_error_index] + std_test_error[min_error_index]

  n_LVs <- which((avg_test_error - ref_error) < 0 )[1] #selects lowest #LVs within 1 std of the error of the minimum error value

  #final result step:
  SIMPLS_result <- SIMPLS(X,Y, max_n_comp=n_LVs, minimal=FALSE, covariance_mask=covariance_mask)
  X_weights  <- SIMPLS_result$X_weights
  rownames(X_weights) <- colnames(X)
  Y_loadings <- SIMPLS_result$Y_loadings
  B          <- SIMPLS_result$coefficients[, , n_LVs]
  P          <- SIMPLS_result$X_loadings_unorthogonalized

  for(i in seq_along(same_level_nodes)){
    update_node <- same_level_nodes[[i]]
    node_cols <- cols_per_X_node[[i]]

    node_weights <- X_weights[node_cols, , drop = FALSE]
    node_LVs <- update_node$preprocessed_X %*% node_weights

    node_Y_loadings <- list()

    #Calculate variance explained per node
    variance_explained <- vector(mode="numeric", length=n_LVs)

    for(k in 1:n_LVs){
      node_P <- P[node_cols, , drop=FALSE]

      variance_explained[k] <- diag(t(node_P[,k, drop=FALSE]) %*% node_P[,k, drop=FALSE]) / (dim(X)[1]-1)
    }
    #correct for total variance in block
    variance_explained <- variance_explained / length(node_cols)

    for(j in seq_along(update_node$next_nodes)){

      next_node_name <- update_node$next_nodes[[j]]$node_name

      node_Y_loadings[[next_node_name]] <- Y_loadings[cols_per_Y_node[[next_node_name]], , drop = FALSE]
    }

    update_node$add_estimate(n_LVs, node_LVs, node_weights, Y_loadings=node_Y_loadings, variance_explained=variance_explained)
  }

}

#' Estimate a node using PCA
#'
#' Estimates the LVs of the given node using PCA estimation.
#'
#' The number of LVs or the \code{rank} can be set explicitly, or it will be
#' automatically determined.
#'
#' The function has no explicit return value, instead the R6Class node object is
#' updated.
#'
#' @param node An object of the R6Class Node which is initialised.
#' @param rank An integer indicating the rank, or maximum number of LVs/PCs. If left as \code{NULL}, the rank is automatically estimated.
#' @export
#' @import stats
PCA_estimator <- function(node, rank=NULL){


  if(is.null(rank)){
    rank = dim(node$preprocessed_X)[2] #TODO: Change to meaningful number based on Heuristic, bootstrapping, or cross validation
  }

  PCA_object <- prcomp(node$preprocessed_X, scale. = FALSE, center = FALSE, rank = rank)

  LVs <- PCA_object$x
  n_LVs <- rank
  X_loadings <- PCA_object$rotation
  variance_explained <- (PCA_object$sdev^2) / sum(PCA_object$sdev^2)

  node$add_estimate(n_LVs, LVs, X_loadings, variance_explained)
}

#' Estimate a node using its full data matrix
#'
#' Estimates the LVs of the given node by setting it to be equal to the full
#' data matrix.
#'
#' The function has no explicit return value, instead the R6Class node object is
#' updated.
#'
#' @param node An object of the R6Class Node which is initialised.
#'
#' @export
full_estimator <- function(node){

  LVs <- node$X_data
  n_LVs <- dim(node$preprocessed_X)[2]
  X_loadings <- diag(dim(node$preprocessed_X)[2])

  node$add_estimate(n_LVs, LVs, X_loadings)
}
