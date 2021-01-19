#' Construct a list of all node types in a connection matrix
#'
#' Constructs a list of node types for all nodes in a given connection matrix.
#'
#' @param connection_matrix A matrix where the non zero elements what
#'   connections exist. The rows indicate the node where the edge is going to,
#'   and the columns indicates the node where the edge is coming from.
#' @return A list of strings indicating the node type for each node specified in
#'   the \code{connection_matrix}
get_all_node_connection_types <- function(connection_matrix){

  node_types <- list()

  for(i in 1:dim(connection_matrix)[1]){
    node_types[[i]] <- get_node_connection_type(connection_matrix, i)
  }

  return(node_types)
}
