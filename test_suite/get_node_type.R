get_node_type <- function(connection_matrix, node_index){
  
  in_sum  <- sum(connection_matrix[node_index,])
  out_sum <- sum(connection_matrix[,node_index])
  
  if( (in_sum == 0) & (out_sum > 0) ){ #Start node
    return("Start")
  }
  else if( (in_sum > 0) & (out_sum == 0) ){ #End node
    return("End")
  }
  else if( (in_sum > 0) & (out_sum > 0) ){ #Middle node
    return("Middle")
  }
  else{ #Something went wrong and the node is unconnected.
    stop("The Node is unconnected. A warning should have already been given when checking the arguments.")
  }
    
}