#' Converts list of strings indicating node types to list of Node class types
#'
#' This is an internal function used to make a list of class types for each
#' connection type of node.
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
#' @import listenv
get_node_class_types <- function(node_connection_types, start_node_estimator, middle_node_estimator, end_node_estimator){
  if(typeof(start_node_estimator) == "character"){
    start_node_class_type <- estimator_string_to_node_class_type(start_node_estimator)
  }
  if(typeof(middle_node_estimator) == "character"){
    middle_node_class_type <- estimator_string_to_node_class_type(middle_node_estimator)
  }
  if(typeof(end_node_estimator) == "character"){
    end_node_class_type <- estimator_string_to_node_class_type(end_node_estimator)
  }

  node_class_types <- listenv()

  for(i in 1:length(node_connection_types)){
    if(node_connection_types[[i]] == "Middle"){
      node_class_types[[i]] <- middle_node_class_type
    }
    else if(node_connection_types[[i]] == "Start"){
      node_class_types[[i]] <- start_node_class_type
    }
    else if(node_connection_types[[i]] == "End"){
      node_class_types[[i]] <- end_node_class_type
    }
    else{
      stop("Node type unrecognised.")
    }
  }

  return(node_class_types)
}
