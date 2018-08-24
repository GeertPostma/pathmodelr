#' Initialises a node using PCA
#'
#' Initialises the LVs of the given node using PCA.
#'
#' The number of LVs or the \code{rank} can be set explicitly, or it will be
#' automatically determined.
#'
#' The function has no explicit return value, instead the R6Class node object is
#' updated.
#'
#' @param node An object of the R6Class Node.
#' @param rank An integer indicating the rank, or maximum number of LVs/PCs. If left as \code{NULL}, the rank is automatically estimated.
#' @export
#' @import stats
PCA_initializer <- function(node, rank=NULL){


  if(is.null(rank)){
    rank = dim(node$preprocessed_X)[2] #TODO: Change to meaningful number based on Heuristic, bootstrapping, or cross validation
  }

  PCA_object <- prcomp(node$preprocessed_X, scale. = FALSE, center = FALSE, rank = rank)

  LVs <- PCA_object$x
  n_LVs <- rank
  X_loadings <- PCA_object$rotation

  node$add_estimate(n_LVs, LVs, X_loadings)
}

#' Initialise a node using its full data matrix
#'
#' Initialises the LVs of the given node by setting it to be equal to the full
#' data matrix.
#'
#' The function has no explicit return value, instead the R6Class node object is
#' updated.
#'
#' @param node An object of the R6Class Node.
#'
#' @export
full_initializer <- function(node){ #Simple Full estimation (generally used for end-nodes)

  LVs <- node$preprocessed_X
  n_LVs <- dim(node$preprocessed_X)[2]
  X_loadings <- diag(dim(node$preprocessed_X)[2])

  node$add_estimate(n_LVs, LVs, X_loadings)
}
