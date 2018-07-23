make_nodes <- function(blocked_data, connection_matrix, block_names){
  
  setClass("node",
           slots = list(node_type                = 'character',
                        node_name                = 'character',
                        downstream_node_indices  = 'integer',
                        upstream_node_indices    = 'integer',
                        X_data                   = 'list',
                        n_LVs                    = 'numeric',
                        LVs                      = 'list',
                        X_loadings               = 'list',
                        coefficients             = 'list'))
  
  node_list <- list()
  
  for(i in 1:length(blocked_data)){
    
    
    node_type <- get_node_type(connection_matrix, i)
    downstream_node_indices <- get_upstream_nodes(connection_matrix, i)
    upstream_node_indices <- get_downstream_nodes(connection_matrix, i)
    
    
    node <- new("node",
                node_type               = node_type,
                node_name               = block_names[[i]],
                downstream_node_indices = downstream_node_indices,
                upstream_node_indices   = upstream_node_indices,
                X_data                  = blocked_data[[i]])
    
    node_list[i] <- node
  }
   return(node_list)
}