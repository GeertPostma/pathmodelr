# Calculates path coefficients after convergence of the LV calculation algorithm.
#' @export
PLS_regression_connection <- function(node, n_LVs=NULL, parallelise=FALSE, n_cores=NULL, error_function=MSE){

  combined_X <- combine_previous_LVs(node)
  X <- combined_X$X
  X_indices <- combined_X$X_indices
  Y <- node$LVs

  if(node$node_type == "Start"){
    #Start nodes have no incoming coefficients
  }
  else{

    if(is.null(n_LVs)){
      #determine max_n_LVs: after first selection, only allow shrinking
      max_n_LVs <- dim(X)[2]

      if(parallelise){
        if(!is.null(n_cores)){
          test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, n_cores=n_cores, connection_PLS=TRUE)$test_errors
        }
        else{
          stop("parallelise is set to TRUE, but n_cores was not set.")
        }
      }
      else{
        test_errors <- cross_validate_node_PLS(node, max_n_LVs, k_folds=10, error_function=error_function, connection_PLS=TRUE)$test_errors
      }

      #n_LV selection: take lowest complexity model within 1 std of the lowest error
      avg_test_error <- colSums(test_errors)
      std_test_error <- apply(test_errors, 2, sd)

      min_error_index <- which.min(avg_test_error)

      ref_error <- avg_test_error[min_error_index] + std_test_error[min_error_index]

      n_LVs <- which((avg_test_error - ref_error) < 0 )[1] #selects lowest #LVs within 1 std of the error of the minimum error value

    }

    SIMPLS_result <- SIMPLS(X,Y,n_LVs)

    B <- SIMPLS_result$coefficients[,,n_LVs, drop=FALSE]
    B <- array(B, dim=c(dim(B)[1], dim(B)[2]))

    for(i in seq_along(node$previous_nodes)){
      previous_node <- node$previous_nodes[[i]]

      previous_node$add_path_coefficients(B[X_indices[[previous_node$node_name]], ,drop=FALSE], node$node_name)
    }
  }


}
