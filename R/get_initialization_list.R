get_initialization_list <- function(node_types, start_node_initialization, middle_node_initialization, end_node_initialization){
  #Convert input strings to corresponding functions
  if(typeof(start_node_initialization) == "character"){
    start_node_initialization <- initializer_string_to_function(start_node_initialization)
  }
  if(typeof(middle_node_initialization) == "character"){
    middle_node_initialization <- initializer_string_to_function(middle_node_initialization)
  }
  if(typeof(end_node_initialization) == "character"){
    end_node_initialization <- initializer_string_to_function(end_node_initialization)
  }
  
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