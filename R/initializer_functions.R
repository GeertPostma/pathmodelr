#' @export
normal_SOPLS_initializer <- function(node, n_LVs=NULL, parallelise=FALSE, n_cores=NULL, error_function=SSE, LV_selection_method="minimum_mean"){

  #X <- node$preprocessed_X
  #Y <-

  #When called, must optimize prediction to -this- node, thereby finding sets of LVs for previous nodes.
  #In the cross-validation procedure, ensure that 0 LVs in previous predictor nodes is possible.
  #Note that Node abstract structure is not particularly ideal for SO-PLS, as no single set of LVs is estimated for each block.
  #Edge case is, in contrast to process PLS, not the end node, but the start node, as it is not being predicted.

  #What to save in node structure: mage plot info.
  # -dataframe of all combinations and corresponding RMSECV,

  #Make mage plot function

  #Rough algorithm:
  # - for each node:
  #   - construct ordered list of X's (note preprocessing when reconstructing them) of preceding nodes- construct ordered list of X's of preceding nodes
  #   - Make dataframe for plotting with all to-be-evaluated combinations
  #   - recursive procedure of orthogonalizing each X's with all previous X's according to how many LV's are being evaluated. residuals of Y are fitted after the first regression step.
  #   - save each result in dataframe. (RMSECV) ( + separate dataframe for exp. variance )

  #NOTE: memoization will not work for large matrices due to complexity of algorithm. 2 versions of algorithm should be implemented.

  #node$add_estimate(...)

}


#' @export
normal_PLS_initializer <- function(node, n_LVs=NULL, block_scale=TRUE, variance_scale=TRUE, parallelise=FALSE, n_cores=NULL, error_function=SSE, LV_selection_method="minimum_mean"){

  X <- node$preprocessed_X
  Y <- combine_target_manifest_variables(node)$Y

  if(is.null(n_LVs)){
    #determine max_n_LVs
    max_n_LVs <- ifelse(node$iteration > 1, dim(node$previous_LVs)[2], dim(X)[2])

    if(parallelise){
      if(!is.null(n_cores)){
        test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, n_cores=n_cores, manifest=TRUE)$test_errors
      }
      else{
        stop("parallelise is set to TRUE, but n_cores was not set.")
      }
    }
    else{
      test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, manifest=TRUE)$test_errors
    }

    #Select n_LVs
    if(tolower(LV_selection_method)=="minimum_mean"){
      avg_test_error <- colSums(test_errors)

      n_LVs <- which.min(avg_test_error)

    }
    else if(tolower(LV_selection_method)=="minimum_median"){
      median_test_error <-apply(test_errors, 2, median)

      n_LVs <- which.min(median_test_error)
    }
    else if(tolower(LV_selection_method)=="conservative"){

      #Take lowest complexity model within 1 std of the lowest error
      avg_test_error <- colMeans(test_errors)

      std_test_error <- apply(test_errors, 2, sd)

      min_error_index <- which.min(avg_test_error)

      ref_error <- avg_test_error[min_error_index] + std_test_error[min_error_index]

      n_LVs <- which((avg_test_error - ref_error) < 0 )[1] #selects lowest #LVs within 1 std of the error of the minimum error value

    }
    else{
      stop("No valid LV selection method was supplied.")
    }
  }

  SIMPLS_result <- SIMPLS(X, Y, max_n_comp = n_LVs, sign_stable=TRUE)

  LVs <- SIMPLS_result$X_scores
  variance_explained <- SIMPLS_result$X_variance_explained
  X_weights <- SIMPLS_result$X_weights

  scale_vec <- get_scale_vec(LVs, variance_explained, block_scale=block_scale, variance_scale=variance_scale)

  node$add_estimate(n_LVs, rescale_LVs(LVs, scale_vec), rescale_X_weights(X_weights, scale_vec), variance_explained = variance_explained)

}

#' @export
end_PLS_initializer <- function(node, n_LVs=NULL, block_scale=TRUE, variance_scale=TRUE, parallelise=FALSE, n_cores=NULL, error_function=SSE, LV_selection_method="minimum_mean"){

  X <- combine_previous_manifest_variables(node)$X
  Y <- node$preprocessed_X

  if(is.null(n_LVs)){
    #determine max_n_LVs: after first selection, only allow shrinking
    max_n_LVs <- dim(Y)[2]

    if(parallelise){
      if(!is.null(n_cores)){
        test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, n_cores=n_cores, manifest=TRUE, end_node=TRUE)$test_errors
      }
      else{
        stop("parallelise is set to TRUE, but n_cores was not set.")
      }
    }
    else{
      test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, manifest=TRUE, end_node=TRUE)$test_errors
    }

    #Select n_LVs
    if(tolower(LV_selection_method)=="minimum_mean"){
      avg_test_error <- colSums(test_errors)

      n_LVs <- which.min(avg_test_error)

    }
    else if(tolower(LV_selection_method)=="minimum_median"){
      median_test_error <-apply(test_errors, 2, median)

      n_LVs <- which.min(median_test_error)
    }
    else if(tolower(LV_selection_method)=="conservative"){

      #Take lowest complexity model within 1 std of the lowest error
      avg_test_error <- colMeans(test_errors)

      std_test_error <- apply(test_errors, 2, sd)

      min_error_index <- which.min(avg_test_error)

      ref_error <- avg_test_error[min_error_index] + std_test_error[min_error_index]

      n_LVs <- which((avg_test_error - ref_error) < 0 )[1] #selects lowest #LVs within 1 std of the error of the minimum error value

    }
    else{
      stop("No valid LV selection method was supplied.")
    }
  }

  SIMPLS_result <- SIMPLS(X, Y, max_n_comp = n_LVs)

  LVs <- SIMPLS_result$Y_scores
  variance_explained <- SIMPLS_result$Y_variance_explained
  Y_weights <- SIMPLS_result$Y_loadings

  scale_vec <- get_scale_vec(LVs, variance_explained, block_scale=block_scale, variance_scale=variance_scale)

  node$add_estimate(n_LVs, rescale_LVs(LVs, scale_vec), rescale_X_weights(Y_weights, scale_vec), variance_explained = variance_explained)
}

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
#' The PCA initializer suffers from the sign indeterminacy. This is an issue
#' that will be adressed in future releases.
#'
#' @param node An object of the R6Class Node.
#' @param rank An integer indicating the rank, or maximum number of LVs/PCs. If
#'   left as \code{NULL}, the rank is equal to the number of variables in that
#'   block.
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
  variance_explained <- (PCA_object$sdev^2) / sum(PCA_object$sdev^2)

  node$add_estimate(n_LVs, LVs, X_loadings, variance_explained=variance_explained)
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
  stdevs <- apply(LVs, 2, sd)
  variance_explained <- (stdevs^2) / sum(stdevs^2)

  node$add_estimate(n_LVs, LVs, X_loadings, variance_explained=variance_explained)
}
