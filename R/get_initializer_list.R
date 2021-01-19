#' Converts list of strings indicating node types to list of initializer
#' function handles
#'
#' This is an internal function used to make a list of initializers for each
#' type of node.
#'
#' @param node_connection_types A list of strings indicating the type of the node
#' @param start_node_initializer A handle to a function which takes a node as an
#'   argument and is able to initialize a Start type node
#' @param middle_node_initializer A handle to a function which takes a node as an
#'   argument and is able to initialize a Middle type node
#' @param end_node_initializer A handle to a function which takes a node as an
#'   argument and is able to initialize an End type node
#' @return A list of function handles to an initializers function which takes a
#'   node as an argument.
get_initializer_list <- function(node_connection_types, start_node_initializer, middle_node_initializer, end_node_initializer){
  #Convert input strings to corresponding functions
  if(typeof(start_node_initializer) == "character"){
    start_node_initializer <- initializer_string_to_function(start_node_initializer)
  }
  if(typeof(middle_node_initializer) == "character"){
    middle_node_initializer <- initializer_string_to_function(middle_node_initializer)
  }
  if(typeof(end_node_initializer) == "character"){
    end_node_initializer <- initializer_string_to_function(end_node_initializer)
  }

  initializers <- list()

  for(i in 1:length(node_connection_types)){
    if(node_connection_types[[i]] == "Middle"){
      initializers[[i]] <- middle_node_initializer
    }
    else if(node_connection_types[[i]] == "Start"){
      initializers[[i]] <- start_node_initializer
    }
    else if(node_connection_types[[i]] == "End"){
      initializers[[i]] <- end_node_initializer
    }
    else{
      stop("Node connection type unrecognised.")
    }
  }

  return(initializers)
}
