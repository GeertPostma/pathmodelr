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

IterationReporter <- R6Class("IterationLogger",
  public = list(
    #Fields
    
    #Methods
    log_status = function(nodes, iteration){
      cat("iteration:", iteration, "\n")
    }
  )
)

DurationLogger <- R6Class("DurationLogger",
  public = list(
   #Fields
   durations = NULL,
   last_time = NA_integer_,
   report = NA,
   
   #Methods
   initialize = function(report=FALSE){
     self$durations <- setNames(data.frame(matrix(nrow=0, ncol=2)), c("iteration", "elapsed time"))
     self$report <- report
   },
   
   log_status = function(nodes, iteration){
     if(iteration == 1){
       self$last_time <- Sys.time()
     }
     else{
       new_time <- Sys.time()
       elapsed_time <- as.numeric(new_time - self$last_time)
       
       self$last_time <- new_time
       
       duration <- data.frame(matrix(nrow=1, ncol=2))
       duration[1,] <- c(iteration, elapsed_time)
       self$durations <- rbind(self$durations, duration)
       
       if(self$report){
         cat("iteration took:", elapsed_time, "seconds\n")
       }
     }
   },
   
   show = function(){
     p = ggplot(d, aes(x = iteration, y="elapsed time", group = Node)) + 
       geom_line(aes(color=Node)) 
     p
   }
  )
)