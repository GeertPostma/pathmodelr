#' Create a listenv of all unique nodes given a listenv of nodes
#'
#' constructs a list of all unique nodes in the input, which may contain
#' duplicates.
#'
#' @param all_nodes A listenv of nodes
#' @return A listenv of nodes. It contains a single reference to each unique
#'   node in all_nodes.
#' @export
#' @import listenv
get_unique_nodes <- function(all_nodes){

  unique_nodes <- listenv()
  unique_node_names <- list()

  n_unique_nodes <- 0

  all_node_names <- list()

  for(i in seq_along(all_nodes)){
    all_node_names <- c(all_node_names, all_nodes[[i]]$node_name)
  }

  unique_node_names <- unique(all_node_names)

  while(length(unique_node_names) > 0){

    for(j in seq_along(all_nodes)){

      for(k in seq_along(unique_node_names)){

        if(all_nodes[[j]]$node_name == unique_node_names[[k]]){

          n_unique_nodes <- n_unique_nodes + 1
          unique_nodes[[n_unique_nodes]] <- all_nodes[[j]]

          unique_node_names[[k]] <- NULL #remove from list

          #k <- k - 1#indexing only works normally when nothing is removed, so lower k for each match
          break
        }
      }
    }
  }

  return(unique_nodes)
}
