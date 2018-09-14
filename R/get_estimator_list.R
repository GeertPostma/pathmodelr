#' Converts list of strings indicating node types to list of estimator function
#' handles
#'
#' This is an internal function used to make a list of estimators for each type
#' of node.
#'
#' @param node_connection_types A list of strings indicating the type of the node
#' @param start_node_estimator A handle to a function which takes a node as an
#'   argument and is able to estimate a Start type node
#' @param middle_node_estimator A handle to a function which takes a node as an
#'   argument and is able to estimate a Middle type node
#' @param end_node_estimator A handle to a function which takes a node as an
#'   argument and is able to estimate an End type node
#' @return A list of function handles to an estimator function which takes a
#'   node as an argument.
get_estimator_list <- function(node_connection_types, start_node_estimator, middle_node_estimator, end_node_estimator){

  #Convert input strings to corresponding functions
  if(typeof(start_node_estimator) == "character"){
    start_node_estimator <- estimator_string_to_function(start_node_estimator)
  }
  if(typeof(middle_node_estimator) == "character"){
    middle_node_estimator <- estimator_string_to_function(middle_node_estimator)
  }
  if(typeof(end_node_estimator) == "character"){
    end_node_estimator <- estimator_string_to_function(end_node_estimator)
  }

  estimators <- list()

  for(i in 1:length(node_connection_types)){
    if(node_connection_types[[i]] == "Middle"){
      estimators[[i]] <- middle_node_estimator
    }
    else if(node_connection_types[[i]] == "Start"){
      estimators[[i]] <- start_node_estimator
    }
    else if(node_connection_types[[i]] == "End"){
      estimators[[i]] <- end_node_estimator
    }
    else{
      stop("Node type unrecognised.")
    }
  }

  return(estimators)
}
