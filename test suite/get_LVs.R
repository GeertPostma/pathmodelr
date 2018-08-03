get_LVs <- function(nodes, max_iterations){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1
  
  
  #TODO: Add iterative loop
  i <- 0
  converged <- FALSE
  while(i < max_iterations && converged == FALSE){
    i <- i + 1
    
    #Reset state loop:
    for(ii in seq_along(nodes)){
      node <- nodes[[ii]]
      
      if(node$is_iterative){
        node$prepare_next_estimation()
      }
      
    }
    
    #Estimation loop:
    for(ii in seq_along(nodes)){
      node <- nodes[[ii]]
      
      if(node$is_initialized){
        if(!node$is_estimated && node$is_iterative){ #Only add estimate if it needs to be updated (this allows for having both iterative and non iterative nodes.)
          node$estimate()
        }
      }
      else{
        stop("Node is not initialized.")
      }
      
    }
    
    #TODO: Check convergence criterion:
    converged = FALSE #placeholder
  }

  return(nodes)
  
}