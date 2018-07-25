library(pls)

get_LVs <- function(nodes){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1
  

  #Default mode: predict all upstream blocks from the block that is being estimated. Start with setting end-node LV's to the raw data
  for(i in length(nodes):1){ #loop backwards to ensure faster convergence (the last node is more likely to be)
    
    if(is_initialized(nodes[[i]])){
      
    }

  }
  
  #TODO: Add other modes
  
  
  return(nodes)
  
}