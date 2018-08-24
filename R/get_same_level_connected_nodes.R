#' Gets a listenv same level nodes
#'
#' Constructs a listenv which contains  all nodes on
#' the same level
#'
#' @param node An object of the R6Class Node which is initialised.
#' @return A listenv of nodes on the same level.
#' @export
#' @import listenv
get_same_level_connected_nodes <- function(node){
  #Code is highly verbose and unoptimized, consider replacing when performance issues are observed.

  next_nodes <- node$next_nodes

  same_level_nodes <- listenv()
  same_level_node_names <- list()
  n_same_level_nodes <- 0



  for(i in seq_along(next_nodes)){
    temp_same_level_nodes <- next_nodes[[i]]$previous_nodes

    for(j in seq_along(temp_same_level_nodes)){
      same_level_node_names <- c(same_level_node_names, temp_same_level_nodes[[j]]$node_name)
    }
  }

  unique_same_level_node_names <- unique(same_level_node_names)

  while(length(unique_same_level_node_names) > 0){

    for(i in seq_along(next_nodes)){

    temp_same_level_nodes <- next_nodes[[i]]$previous_nodes

      for(j in seq_along(temp_same_level_nodes)){

        for(k in seq_along(unique_same_level_node_names)){

          if(temp_same_level_nodes[[j]]$node_name == unique_same_level_node_names[[k]]){

            n_same_level_nodes <- n_same_level_nodes + 1
            same_level_nodes[[n_same_level_nodes]] <- temp_same_level_nodes[[j]]

            unique_same_level_node_names[[k]] <- NULL #remove from list

            k <- k - 1#indexing only works normally when nothing is removed, so lower k for each match
            break
          }
        }
      }
    }
  }

  return(same_level_nodes)
}
