#' Gets the indices of the next nodes in a connection matrix given a node_index
#'
#' Gets the integer indices of the next nodes given a connection_matrix and a
#' starting index.
#'
#' @param connection_matrix A matrix where the non zero elements what
#'   connections exist. The rows indicate the node where the edge is going to,
#'   and the columns indicates the node where the edge is coming from.
#' @param node_index a number indicating of which node the type is being
#'   determined.
#' @return A list of indices of all next level nodes
get_next_node_indices <- function(connection_matrix, node_index){

  result <- (1:dim(connection_matrix)[1])[connection_matrix[,node_index] == 1]
}
