#' Combines data from predictor and target nodes and constructs a mask for the covariance matrix accordingly
#'
#' This function combines all the X_data from nodes on the same level as the entered node into a matrix X. The data is preprocessed with respect to a proper train and test split if test_indices is set.
#' All the previous_LVs from all target nodes are combined, although they are not processed further, into Y.
#' A covariance mask is also calculated, so that not all predictors are only predictive for targets that are connected according to the graph structure.
#'
#' When test_indices are supplied, X and Y are split into a separate training and test set and are preprocessed accordingly.
#'
#' combine_and_mask() was made for use with an iterative procedure, it constructs the targets based on the previously estimated LVs.
#'
#' @param node An object of the R6Class Node which is initialised and estimated at least once. The node needs to be connected to at least one target node for the Y matrices to exist.
#' @param test_indices a vector of indices indicating the row indices of the samples that belong to the test set. The training set is assumed to be the complementary set of data.
#' @return list("X"=X,
#'              "Y"=Y,
#'              "covariance_mask"=covariance_mask,
#'              "cols_per_X_node"=cols_per_X_node,
#'              "cols_per_y_node"=cols_per_Y_node,
#'              "same_level_nodes"=same_level_nodes,
#'              "next_level_nodes"=next_level_nodes))
#'
#' Or, when test_indices is supplied:
#'
#'         list("X_train"=X_train,
#'              "X_test"=X_test,
#'              "Y_train"=Y_train,
#'              "Y_test"=Y_test,
#'              "covariance_mask"=covariance_mask,
#'              "cols_per_X_node"=cols_per_X_node,
#'              "cols_per_y_node"=cols_per_Y_node,
#'              "same_level_nodes"=same_level_nodes,
#'              "next_level_nodes"=next_level_nodes))
#'
#' This is named list containing X, Y, the covariance mask, all nodes on the same level (predictors), all nodes on the next level (targets), and which columns of X and Y belong to which node originally. When test_indices is supplied, X and Y are split into X_train, X_test, Y_train, and Y_test
#'
combine_and_mask <- function(node, test_indices=NULL){

  layered_nodes <- get_nodes_by_level(node)

  same_level_nodes <- layered_nodes[[1]]
  next_level_nodes <- layered_nodes[[2]]

  cols_per_X_node <- list()
  cols_per_Y_node <- list()
  total_X_cols <- 0
  total_Y_cols <- 0

  for(i in seq_along(same_level_nodes)){
    cols_per_X_node[[i]] <- (total_X_cols + 1):(total_X_cols+ncol(same_level_nodes[[i]]$X_data))
    total_X_cols <- total_X_cols + ncol(same_level_nodes[[i]]$X_data)
  }

  for(i in seq_along(next_level_nodes)){
    cols_per_Y_node[[i]] <- (total_Y_cols + 1):(total_Y_cols+ncol(next_level_nodes[[i]]$previous_LVs))
    total_Y_cols <- total_Y_cols + ncol(next_level_nodes[[i]]$previous_LVs)
  }

  #Fill in covariance mask
  covariance_mask <- matrix(0, nrow=total_X_cols, ncol=total_Y_cols)

  for(i in seq_along(same_level_nodes)){

    X_node <- same_level_nodes[[i]]

    for(j in seq_along(next_level_nodes)){

      Y_node <- next_level_nodes[[j]]

      for(k in seq_along(X_node$next_nodes)){

        temp_node <- X_node$next_nodes[[k]]

        if(Y_node$node_name == temp_node$node_name){
          covariance_mask[cols_per_X_node[[i]], cols_per_Y_node[[j]]] <- 1
        }
      }
    }
  }

  if(is.null(test_indices)){
    n_samples <- nrow(node$X_data)

    X <- matrix(0, nrow=n_samples, ncol=total_X_cols)
    Y <- matrix(0, nrow=n_samples, ncol=total_Y_cols)

    for(i in seq_along(same_level_nodes)){
      X[,cols_per_X_node[[i]]] <- as.matrix(same_level_nodes[[i]]$preprocessed_X)
    }

    for(i in seq_along(next_level_nodes)){
      Y[,cols_per_Y_node[[i]]] <- as.matrix(next_level_nodes[[i]]$previous_LVs)
    }

    return(list("X"=X,
                "Y"=Y,
                "covariance_mask"=covariance_mask,
                "cols_per_X_node"=cols_per_X_node,
                "cols_per_y_node"=cols_per_Y_node,
                "same_level_nodes"=same_level_nodes,
                "next_level_nodes"=next_level_nodes))

  }
  else{
    n_samples_train <- nrow(node$X_data[-test_indices,])
    n_samples_test  <- nrow(node$X_data[test_indices,])

    X_train <- matrix(0, nrow=n_samples_train, ncol=total_X_cols)
    X_test  <- matrix(0, nrow=n_samples_test,  ncol=total_X_cols)
    Y_train <- matrix(0, nrow=n_samples_train, ncol=total_Y_cols)
    Y_test  <- matrix(0, nrow=n_samples_test,  ncol=total_Y_cols)

    for(i in seq_along(same_level_nodes)){
      preprocessed_X_sets <- same_level_nodes[[i]]$preprocess_train_test(test_indices)

      X_train[,cols_per_X_node[[i]]] <- as.matrix(preprocessed_X_sets$train_data)
      X_test[,cols_per_X_node[[i]]]  <- as.matrix(preprocessed_X_sets$test_data)
    }

    for(i in seq_along(next_level_nodes)){
      Y <- next_level_nodes[[i]]$previous_LVs

      Y_train[,cols_per_Y_node[[i]]] <- as.matrix(Y[-test_indices,])
      Y_test[,cols_per_Y_node[[i]]]  <- as.matrix(Y[test_indices,])
    }

    return(list("X_train"=X_train,
                "X_test"=X_test,
                "Y_train"=Y_train,
                "Y_test"=Y_test,
                "covariance_mask"=covariance_mask,
                "cols_per_X_node"=cols_per_X_node,
                "cols_per_y_node"=cols_per_Y_node,
                "same_level_nodes"=same_level_nodes,
                "next_level_nodes"=next_level_nodes))
  }
}
