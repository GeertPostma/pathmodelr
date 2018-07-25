#' @include node_classes.R
make_nodes <- function(blocked_data, connection_matrix, block_names){
  
  node_list <- listenv()
  
  for(i in 1:length(blocked_data)){
    
    node_type <- get_node_type(connection_matrix, i)

    if(node_type == "Middle"){
      next_node_indices     <- get_next_nodes(connection_matrix, i)
      previous_node_indices <- get_previous_nodes(connection_matrix, i)
      
      node <- MiddleNode$new(node_name               = block_names[[i]],
                             next_node_indices       = next_node_indices,
                             previous_node_indices   = previous_node_indices,
                             X_data                  = blocked_data[[i]])
    }
    else if(node_type == "Start"){
      next_node_indices <- get_next_nodes(connection_matrix, i)
      
      node <- StartNode$new(node_name         = block_names[[i]],
                            next_node_indices = next_node_indices,
                            X_data            = blocked_data[[i]])      
    }
    else if(node_type == "End"){
      previous_node_indices <- get_previous_nodes(connection_matrix, i)
      
      node <- EndNode$new(node_name             = block_names[[i]],
                          previous_node_indices = previous_node_indices,
                          X_data                = blocked_data[[i]])           
    }
    else{
      stop("Node type unrecognised.")
    }
    
    node_list[[i]] <- node
  }
   return(node_list)
}