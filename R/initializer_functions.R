#' n_LVs should be a list of n_LVs per block
#' @export
normal_SOPLS_initializer <- function(node, n_LVs_per_block=NULL, parallelise=FALSE, n_cores=NULL, error_function=SSE, LV_selection_method="minimum_mean"){


  if(is.null(n_LVs_per_block)){
    max_n_LVs_per_block <- lapply(node$previous_nodes, function(n) dim(n$X_data)[2])
    names(max_n_LVs_per_block) <- lapply(node$previous_nodes, function(n) n$node_name)

    #construct all LV combinations and order them
    combination_grid <- expand.grid(lapply(max_n_LVs_per_block, function(e) 0:e))
    combination_grid <- combination_grid[do.call(order,combination_grid), ,drop=FALSE]

    k_folds <- 10

    test_indices <- createFolds(1:nrow(node$X_data), k = k_folds)

    train_errors <- matrix(0, nrow=dim(combination_grid)[1], ncol=k_folds)
    test_errors <- train_errors

    for(k in seq_along(test_indices)){ #each cross validation fold

      test_index_set <- test_indices[[k]]

      X_blocks <- lapply(node$previous_nodes, function(n) n$preprocess_train_test(test_index_set))

      X_train_blocks <- lapply(X_blocks, function(x) x$train_data)
      X_test_blocks <- lapply(X_blocks, function(x) x$test_data)

      Y <- node$preprocess_train_test(test_index_set)
      Y_train <- Y$train_data
      Y_test <- Y$test_data

      X_orth_train_blocks <- rep_len(list(NULL), length(max_n_LVs_per_block))
      X_orth_test_blocks <- rep_len(list(NULL), length(max_n_LVs_per_block))

      T_train_blocks <- rep_len(list(NULL), length(max_n_LVs_per_block))
      T_test_blocks <- rep_len(list(NULL), length(max_n_LVs_per_block))

      #Calculates error for when no paths are used for prediction <-----
      train_errors[1,] <- error_function(Y_train, matrix(0, nrow=dim(Y_train)[1], ncol=dim(Y_train)[2]))
      test_errors[1,] <- error_function(Y_test, matrix(0, nrow=dim(Y_test)[1], ncol=dim(Y_test)[2]))

      #Y is deflated, even though it is not used in every paper description!
      deflated_Y_train_blocks <- list()
      deflated_Y_test_blocks <- list()

      #Account for empty blocks (0 LVs) when orthogonalizing and regressing, they need to be skipped
      for(i in 2:dim(combination_grid)[1]){ #each row in combination_grid

        changed_blocks <- combination_grid[i,] != combination_grid[i-1,]

        for(j in (1:length(max_n_LVs_per_block))[changed_blocks]){ #each (in size) changed X_block

          list_of_previous_used_blocks <- lapply(X_orth_train_blocks[1:(j-1)], function(x) !is.null(x[[1]]))

          #If no previous blocks are used for prediction: initialize block fully without orthogonalization
          if(Reduce("+",list_of_previous_used_blocks)==0 || j==1){
            X_orth_train_blocks[[j]] <- X_train_blocks[[j]]
            X_orth_test_blocks[[j]] <- X_test_blocks[[j]]
            PLS_model <- SIMPLS(X_orth_train_blocks[[j]], Y_train, max_n_comp = combination_grid[[i,j]])

            T_train_blocks[[j]] <- PLS_model$X_scores
            T_test_blocks[[j]] <- X_orth_test_blocks[[j]] %*% PLS_model$X_weights

            B <- PLS_model$coefficients[, , combination_grid[[i,j]]]
            deflated_Y_train_blocks[[j]] <- Y_train - X_orth_train_blocks[[j]] %*% B
            deflated_Y_test_blocks[[j]] <- Y_test - X_orth_test_blocks[[j]] %*% B
          }
          else if(combination_grid[[i,j]]==0){ #if a block connection is skipped due to 0 LVs
            X_orth_train_blocks[[j]] <- X_orth_train_blocks[[j-1]]
            X_orth_test_blocks[[j]] <- X_orth_test_blocks[[j-1]]

            T_train_blocks[[j]] <- T_train_blocks[[j-1]]
            T_test_blocks[[j]] <- T_test_blocks[[j-1]]

            deflated_Y_train_blocks[[j]] <- deflated_Y_train_blocks[[j-1]]
            deflated_Y_test_blocks[[j]] <- deflated_Y_test_blocks[[j-1]]
          }
          else{
            last_used_index <- max(which(list_of_previous_used_blocks==TRUE))

            orth_results <- SOPLS_orthogonalize(X_train_blocks[[j]], T_train_blocks[[last_used_index]])
            X_orth_train_blocks[[j]] <- orth_results$X_2_orth
            X_orth_test_blocks[[j]] <- SOPLS_orthogonalize(X_test_blocks[[j]], T_test_blocks[[last_used_index]], orth_results$D)$X_2_orth #Can fail for specific matrices which are singular, this mostly happens in small sample sizes.

            PLS_model <- SIMPLS(X_orth_train_blocks[[j]], deflated_Y_train_blocks[[j-1]], max_n_comp = combination_grid[[i,j]])

            T_train_blocks[[j]] <- PLS_model$X_scores
            T_test_blocks[[j]] <- X_orth_test_blocks[[j]] %*% PLS_model$X_weights

            B <- PLS_model$coefficients[, , combination_grid[[i,j]]]
            deflated_Y_train_blocks[[j]] <- deflated_Y_train_blocks[[j-1]] - X_orth_train_blocks[[j]] %*% B
            deflated_Y_test_blocks[[j]] <- deflated_Y_test_blocks[[j-1]] - X_orth_test_blocks[[j]] %*% B

          }
        }

        train_errors[i,k] <- error_function(Y_train, Y_train-deflated_Y_train_blocks[[j]])
        test_errors[i,k] <- error_function(Y_test, Y_test-deflated_Y_test_blocks[[j]])

      }

    }
    best_model_settings <- combination_grid[which.min(rowMeans(test_errors)),]
    #Use depth first search for memory efficiency!
    #Rough algorithm:
    # - for each dependent node: (so for every call of normal_SOPLS_initializer)
    #   - construct ordered list of X's (note preprocessing when reconstructing them) of preceding nodes- construct ordered list of X's of preceding nodes
    #   - Make dataframe for plotting with all to-be-evaluated combinations
    #   - recursive procedure of orthogonalizing each X's with all previous X's according to how many LV's are being evaluated. residuals of Y are fitted after the first regression step.
    #   - save each result in dataframe. (RMSECV/SSECV) ( + separate dataframe for exp. variance )

    #NOTE: memoization will not work for large matrices due to complexity of algorithm. 2 versions of algorithm should be implemented.

    #Make DF/matrix for CV errors
      #What to save in node structure: mage plot info.
      # -dataframe/matrix of all combinations and corresponding RMSECV (or SSECV)

    #n_LVs_per_block <- ...
  }




  #Calculate final model

  #When called, must optimize prediction to -this- node, thereby finding sets of LVs for previous nodes.
  #In the cross-validation procedure, ensure that 0 LVs in previous predictor nodes is possible.




  #Make mage plot function



  #node$add_estimate(...)

}


#' @export
normal_PLS_initializer <- function(node, n_LVs=NULL, block_scale=TRUE, variance_scale=TRUE, parallelise=FALSE, n_cores=NULL, error_function=SSE, LV_selection_method="minimum_mean"){

  X <- node$preprocessed_X
  Y <- combine_target_manifest_variables(node)$Y

  if(is.null(n_LVs)){
    #determine max_n_LVs
    max_n_LVs <- dim(X)[2]

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

#' @export
no_initializer <- function(node){

}

