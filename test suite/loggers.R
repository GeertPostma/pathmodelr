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
        self$n_LVs_per_node <- data.frame(matrix(nrow=0, ncol=length(nodes)))
      }
      n_LVs <- data.frame(matrix(nrow=1, ncol=length(nodes)+1))
      
      colnames(n_LVs) <- iteration
      for(i in seq_along(nodes)){
        node <- nodes[[i]]
        
        n_LVs[,i] <- node$n_LVs
        colnames(n_LVs)[[i]] <- node$node_name
      }
      n_LVs[,i+1] <- iteration
      colnames(n_LVs)[[i+1]] <- "iteration"
      self$n_LVs_per_node <- rbind(self$n_LVs_per_node,n_LVs)
    },
    
    show = function(){
      d <- melt(self$n_LVs_per_node, id.vars = "iteration")
      colnames(d)[colnames(d) == "variable"] <- "Node"
      colnames(d)[colnames(d) == "value"] <- "n_LVs"
      p = ggplot(d, aes(x = iteration, y=n_LVs, group = Node)) + 
        geom_line(aes(color=Node)) 
      p
        
    }
  )
)