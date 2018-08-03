#Each logger must implement a log_status method

#Logs the number of components per node per iteration
ComponentLogger <- R6Class("ComponentLogger",
  public = list(
    #Fields
    n_LVs_per_node = NULL,
    
    #Methods
    initialize = function(){},
    
    log_status = function(nodes, iteration){
      if(is.null(self$n_LVs_per_node)){
        self$n_LVs_per_node <- data.frame(matrix(nrow=length(nodes), ncol=0))
      }
      n_LVs <- data.frame(matrix(nrow=length(nodes), ncol=1))
      
      colnames(n_LVs) <- iteration
      for(i in seq_along(nodes)){
        node <- nodes[[i]]
        
        n_LVs[i,] <- node$n_LVs
        rownames(n_LVs)[[i]] <- node$node_name
      }
      self$n_LVs_per_node <- cbind(self$n_LVs_per_node,n_LVs)
    }#,
    
    #show = function(){
      
    #}
  )
)