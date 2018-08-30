#' Estimate a node using PLS regression
#'
#' Estimates the LVs of the given node, and all nodes on the same level, using
#' PLS regression with all next level nodes as targets.
#'
#' The cross product matrix used in SIMPLS regression is set to zero at the
#' connections between the target and predictor variables which are not defined
#' as being connected in the path model specification.
#'
#' The function has no explicit return value, instead the R6Class node object,
#' and all same level connected nodes, are updated.
#'
#' @param node An object of the R6Class Node which is initialised and estimated
#'   at least once. The node needs to be connected to at least one target node
#'   for the Y matrices to exist.
#' @export
#' @import ggplot2
PLS_estimator <- function(node){

  max_n_LVs <- node$previous_n_LVs

  test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=MSE)$test_errors

  #TODO: implement more intricate methods of n_LVs selection
  n_LVs <- which.min(colSums(test_errors))

  #plot(ggplot2::ggplot(data=data.frame(errors=colSums(test_errors), n_LVs=1:length(colSums(test_errors))), ggplot2::aes(y=errors,x=n_LVs)) + ggplot2::geom_line())

  #final result step:
  combined_and_masked <- combine_and_mask(node)

  X <- combined_and_masked$X
  Y <- combined_and_masked$Y
  covariance_mask <- combined_and_masked$covariance_mask
  cols_per_X_node <- combined_and_masked$cols_per_X_node
  same_level_nodes <- combined_and_masked$same_level_nodes

  X_weights <- SIMPLS(X,Y, max_n_comp=n_LVs, minimal=TRUE, covariance_mask=covariance_mask)$X_weights

  # bla <- reshape2::melt(X_loadings)
  #
  # p <- ggplot2::ggplot(data = bla, ggplot2::aes(x=Var1, y=Var2)) +
  #        ggplot2::geom_tile(aes(fill = value)) +
  #        ggplot2::geom_text(aes(label = round(value, 1))) +
  #        ggplot2::scale_fill_gradient(low = "white", high = "red")
  #        ggplot2::ggtitle(node$node_name)
  # methods::show(p)


  for(i in seq_along(same_level_nodes)){
    update_node <- same_level_nodes[[i]]
    node_cols <- cols_per_X_node[[i]]

    node_weights <- X_weights[node_cols,]
    node_LVs <- update_node$preprocessed_X %*% node_weights

    update_node$add_estimate(n_LVs, node_LVs, node_weights)
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

  node$add_estimate(n_LVs, LVs, X_loadings)
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
