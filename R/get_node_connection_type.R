#' Determines the type of a node
#'
#' Determines whether a node is a Start, Middle, or End type node.
#'
#' @param connection_matrix A matrix where the non zero elements what
#'   connections exist. The rows indicate the node where the edge is going to,
#'   and the columns indicates the node where the edge is coming from.
#' @param node_index a number indicating of which node the type is being
#'   determined.
#' @return A string indicating the type of node using "Start", "End", or
#'   "Middle"
get_node_connection_type <- function(connection_matrix, node_index){

  in_sum  <- sum(connection_matrix[node_index,])
  out_sum <- sum(connection_matrix[,node_index])

  if( (in_sum == 0) & (out_sum > 0) ){ #Start node
    return("Start")
  }
  else if( (in_sum > 0) & (out_sum == 0) ){ #End node
    return("End")
  }
  else if( (in_sum > 0) & (out_sum > 0) ){ #Middle node
    return("Middle")
  }
  else{ #Something went wrong and the node is unconnected.
    stop("The Node is unconnected. A warning should have already been given when checking the arguments.")
  }

}
