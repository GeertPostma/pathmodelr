get_initialization_list <- function(node_types, start_node_initialization, middle_node_initialization, end_node_initialization){
  initializers <- list()
  
  for(i in 1:length(node_types)){
    if(node_types[[i]] == "Middle"){
      initializers[[i]] <- middle_node_initialization
    }
    else if(node_types[[i]] == "Start"){
      initializers[[i]] <- start_node_initialization
    }
    else if(node_types[[i]] == "End"){
      initializers[[i]] <- end_node_initialization
    }
    else{
      stop("Node type unrecognised.")
    }
  }
  
  return(initializers)
}