#' @export
find_paths_to_node <- function(node){

  incoming_paths <- listenv()

  node_names <- list()
  for(i in seq_along(node$previous_nodes)){

    if(!node$previous_nodes[[i]]$paths_found){

      find_paths_to_node(node$previous_nodes[[i]])

    }

    node_names[[i]] <- node$previous_nodes[[i]]$node_name
    incoming_paths[[i]] <- node$previous_nodes[[i]]$paths_to_node
  }

  names(incoming_paths) <- node_names

  node$paths_found <- TRUE
  node$paths_to_node <- incoming_paths

}
