make_nodes <- function(blocked_data, connection_matrix){
  
  setClass("node",
           slots = list(node_type        = 'character',
                        downstream_nodes = 'list',
                        n_LVs            = 'numeric',
                        X_data           = 'list',
                        LVs              = 'list',
                        X_loadings       = 'list',
                        coefficients     = 'list'))
  
  node_list <- list()
  
  for(i in 1:length(blocked_data)){
    
    
    node_type <-
    downstream_nodes <-
    
    
    node <- new("node",
                node_type        = ,
                downstream_nodes = ,
                X_data           = blocked_data[[i]])
  }
   
}