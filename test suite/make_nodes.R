#' @include node_classes.R
make_nodes <- function(blocked_data, connection_matrix, block_names){
  
  node_list <- list()
  
  for(i in 1:length(blocked_data)){
    
    node_type <- get_node_type(connection_matrix, i)

    if(node_type == "Middle"){
      downstream_node_indices <- get_upstream_nodes(connection_matrix, i)
      upstream_node_indices <- get_downstream_nodes(connection_matrix, i)
      
      node <- MiddleNode$new(node_name               = block_names[[i]],
                             downstream_node_indices = downstream_node_indices,
                             upstream_node_indices   = upstream_node_indices,
                             X_data                  = blocked_data[[i]])
    }
    else if(node_type == "Start"){
      upstream_node_indices <- get_downstream_nodes(connection_matrix, i)
      
      node <- StartNode$new(node_name             = block_names[[i]],
                            upstream_node_indices = upstream_node_indices,
                            X_data                = blocked_data[[i]])      
    }
    else if(node_type == "End"){
      downstream_node_indices <- get_upstream_nodes(connection_matrix, i)
      
      node <- EndNode$new(node_name               = block_names[[i]],
                          downstream_node_indices = downstream_node_indices,
                          X_data                  = blocked_data[[i]])           
    }
    else{
      stop("Node type unrecognised.")
    }
    
    node_list[i] <- node
  }
   return(node_list)
}