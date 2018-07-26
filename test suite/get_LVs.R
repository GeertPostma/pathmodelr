library(pls)

get_LVs <- function(nodes){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1
  

  for(i in length(nodes):1){ #loop backwards to ensure faster convergence (the last node is more likely to be)
    
    if(is_initialized(nodes[[i]])){
      
    }

  }
  
  return(nodes)
  
}