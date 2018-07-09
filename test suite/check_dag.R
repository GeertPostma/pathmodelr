check_dag <- function(DAG, node_name, visited_nodes = c()){
  # print(node_name)
  # print(visited_nodes)
  # print(DAG)
  # 
  visited_nodes <- cbind(visited_nodes, node_name)
  
  
  if (sum(DAG) == 0){
    return(TRUE)
  }
  else{
    connected_nodes <- rownames(DAG)[which(DAG[, node_name] != 0, arr.ind = TRUE)]
    
    if (any(connected_nodes %in% visited_nodes)){ #Exits when cycle is detected)
      return(FALSE)
    }
    
    for(i in connected_nodes){ #Check all connected_nodes
      
      numerical_node_number <- which(row.names(DAG) == node_name)
      if (!check_dag(DAG[-numerical_node_number,-numerical_node_number], toString(i), visited_nodes)){
        return(FALSE)
      }
    }
    return(TRUE)
    
  }
  
}