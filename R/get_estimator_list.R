get_estimator_list <- function(node_types, start_node_estimator, middle_node_estimator, end_node_estimator){
  
  #Convert input strings to corresponding functions
  if(typeof(start_node_estimator) == "character"){
    start_node_estimator <- estimator_string_to_function(start_node_estimator)
  }
  if(typeof(middle_node_estimator) == "character"){
    middle_node_estimator <- estimator_string_to_function(middle_node_estimator)
  }
  if(typeof(end_node_estimator) == "character"){
    end_node_estimator <- estimator_string_to_function(end_node_estimator)
  }
  
  estimators <- list()
  
  for(i in 1:length(node_types)){
    if(node_types[[i]] == "Middle"){
      estimators[[i]] <- middle_node_estimator
    }
    else if(node_types[[i]] == "Start"){
      estimators[[i]] <- start_node_estimator
    }
    else if(node_types[[i]] == "End"){
      estimators[[i]] <- end_node_estimator
    }
    else{
      stop("Node type unrecognised.")
    }
  }
  
  return(estimators)
}