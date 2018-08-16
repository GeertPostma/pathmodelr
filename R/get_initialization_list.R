#' Converts list of strings indicating node types to list of initializer
#' function handles
#'
#' This is an internal function used to make a list of initializers for each
#' type of node.
#'
#' @param node_types A list of strings indicating the type of the node
#' @param start_node_estimator A handle to a function which takes a node as an
#'   argument and is able to initialize a Start type node
#' @param middle_node_estimator A handle to a function which takes a node as an
#'   argument and is able to initialize a Middle type node
#' @param end_node_estimator A handle to a function which takes a node as an
#'   argument and is able to initialize an End type node
#' @return A list of function handles to an initializers function which takes a
#'   node as an argument.
get_initialization_list <- function(node_types, start_node_initialization, middle_node_initialization, end_node_initialization){
  #Convert input strings to corresponding functions
  if(typeof(start_node_initialization) == "character"){
    start_node_initialization <- initializer_string_to_function(start_node_initialization)
  }
  if(typeof(middle_node_initialization) == "character"){
    middle_node_initialization <- initializer_string_to_function(middle_node_initialization)
  }
  if(typeof(end_node_initialization) == "character"){
    end_node_initialization <- initializer_string_to_function(end_node_initialization)
  }

  initializers <- list()

  for(i in 1:length(node_types)){
    if(node_types[[i]] == "Middle"){
      initializers[[i]] <- middle_node_initialization
    }
    else if(node_types[[i]] == "Start"){
      initializers[[i]] <- start_node_initialization
    }
    else if(node_types[[i]] == "End"){
      initializers[[i]] <- end_node_initialization
    }
    else{
      stop("Node type unrecognised.")
    }
  }

  return(initializers)
}
