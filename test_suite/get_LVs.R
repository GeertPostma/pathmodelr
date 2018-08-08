get_LVs <- function(nodes, max_iterations, loggers){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1
  
  
  #TODO: Add iterative loop
  i <- 0
  converged <- FALSE
  threshold <- 0.0001
  
  while(i < max_iterations && !converged){
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
    #log everything required by loggers
    for(l in seq_along(loggers)){
      logger <- loggers[[l]]
      logger$log_status(nodes,i)
    }
    
    #TODO: Check convergence criterion:
    total_error <- 0
    for(ii in seq_along(nodes)){
      total_error <- total_error + nodes[[ii]]$error
    }
    
    if(total_error < threshold){
      converged <- TRUE
    }
  }

  return(listenv("nodes"=nodes, "loggers"=loggers))
  
}