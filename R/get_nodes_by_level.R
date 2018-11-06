#' Gets a listenv of listenvs of same level, and next level nodes
#'
#' Constructs a listenv with length 2 which contains a listenv of all nodes on
#' the same level, and a listenv of all nodes on the next level.
#'
#' @param node An object of the R6Class Node which is initialised.
#' @return A listenv with two listenvs of nodes on the same level, and nodes on
#'   the next level.
#' @export
#' @import listenv
get_nodes_by_level <- function(node){

  unique_next_level_nodes <- listenv()
  unique_same_level_nodes <- listenv(node)
  n_same <- 0
  temp <- -1

  #currently uses rough method because R6 classes lack equals() method, may be interesting to update when a performance issue is encountered (unlikely)
  while(n_same != temp){

    all_same_level_nodes <- listenv()
    for(i in seq_along(unique_same_level_nodes)){
      temp_node <- unique_same_level_nodes[[i]]
      temp_same_level_nodes <- get_same_level_connected_nodes(temp_node)

      all_same_level_nodes[(length(all_same_level_nodes)+1):(length(all_same_level_nodes)+length(temp_same_level_nodes))] <- temp_same_level_nodes
    }
    unique_same_level_nodes <- get_unique_nodes(all_same_level_nodes)

    temp <- n_same
    n_same <- length(unique_same_level_nodes)
  }
  names(unique_same_level_nodes) <- lapply(unique_same_level_nodes, function(node) node$node_name)

  all_next_level_nodes <- listenv()
  for(i in seq_along(unique_same_level_nodes)){
    temp_node <- unique_same_level_nodes[[i]]
    temp_next_level_nodes <- temp_node$next_nodes

    all_next_level_nodes[(length(all_next_level_nodes)+1):(length(all_next_level_nodes)+length(temp_next_level_nodes))] <- temp_next_level_nodes
  }
  unique_next_level_nodes <- get_unique_nodes(all_next_level_nodes)

  names(unique_next_level_nodes) <- lapply(unique_next_level_nodes, function(node) node$node_name)

  return(listenv("unique_same_level_nodes" = unique_same_level_nodes, "unique_next_level_nodes" = unique_next_level_nodes))
}
