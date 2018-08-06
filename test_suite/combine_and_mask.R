combine_and_mask <- function(node){
  
  layered_nodes <- get_nodes_by_level(node)
  
  same_level_nodes <- layered_nodes[[1]]
  next_level_nodes <- layered_nodes[[2]]
  
  n_samples <- nrow(node$X_data)
  
  cols_per_X_node <- list()
  cols_per_Y_node <- list()
  total_X_cols <- 0
  total_Y_cols <- 0
  
  for(i in seq_along(same_level_nodes)){
    cols_per_X_node[[i]] <- (total_X_cols + 1):(total_X_cols+ncol(same_level_nodes[[i]]$X_data))
    total_X_cols <- total_X_cols + ncol(same_level_nodes[[i]]$X_data)
  }
  
  for(i in seq_along(next_level_nodes)){
    cols_per_Y_node[[i]] <- (total_Y_cols + 1):(total_Y_cols+ncol(next_level_nodes[[i]]$LVs))
    total_Y_cols <- total_Y_cols + ncol(next_level_nodes[[i]]$LVs)
  }
  X <- matrix(0, nrow=n_samples, ncol=total_X_cols)
  Y <- matrix(0, nrow=n_samples, ncol=total_Y_cols)
  
  for(i in seq_along(same_level_nodes)){
    X[,cols_per_X_node[[i]]] <- as.matrix(same_level_nodes[[i]]$X_data)
  }
  
  for(i in seq_along(next_level_nodes)){
    Y[,cols_per_Y_node[[i]]] <- as.matrix(next_level_nodes[[i]]$LVs)
  }
  
  covariance_mask <- matrix(0, nrow=ncol(X), ncol=ncol(Y))
  
  #Fill in covariance mask
  for(i in seq_along(same_level_nodes)){
    
    X_node <- same_level_nodes[[i]]
    
    for(j in seq_along(next_level_nodes)){
      
      Y_node <- next_level_nodes[[j]]
      
      for(k in seq_along(X_node$next_nodes)){
        
        temp_node <- X_node$next_nodes[[k]]
        
        if(Y_node$node_name == temp_node$node_name){
          covariance_mask[cols_per_X_node[[i]], cols_per_Y_node[[j]]] <- 1
        }
      }
    }
  }
  return(list("X"=X, "Y"=Y, "covariance_mask"=covariance_mask, "cols_per_X_node"=cols_per_X_node, "cols_per_y_node"=cols_per_Y_node))
}