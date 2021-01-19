#Each logger must implement a log_status method

#' Logs the number of components, or LVs, per node per iteration.
#'
#' The logger show() method produces a plot of the iterations, on the X-axis, vs
#' the number of components, or LVs, on the Y-axis.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
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
      p <- ggplot(d, aes(x = iteration, y=n_LVs, group = Node)) +
        geom_line(aes(color=Node))
      p

    }
  )
)
#' Reports the number of each iteration.
#' @export
IterationReporter <- R6Class("IterationLogger",
  public = list(
    #Fields

    #Methods
    initialize = function(){},

    log_status = function(nodes, iteration){
      cat("iteration:", iteration, "\n")
    }
  )
)

#' Logs the duration of each iteration
#'
#' The logger show() method produces a plot of the iterations, on the X-axis, vs
#' the duration of each iteration, on the Y-axis.
#'
#' When \code{report} is set to \code{TRUE} the duration is also reported each
#' iteration.
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
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
     d <- melt(self$durations, id.vars = "iteration")
     colnames(d)[colnames(d) == "value"] <- "elapsed time"
     p <- ggplot(d, aes(x = iteration, y="elapsed time")) +
       geom_line(aes(color=Node))
     p
   }
  )
)

#' Logs the error per node per iteration
#'
#' The logger show() method produces a plot of the iterations, on the X-axis, vs
#' the error of each iteration, on the Y-axis.
#'
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
ConvergenceLogger <- R6Class("ConvergenceLogger",
  public = list(
   #Fields
   error_per_node = NULL,

   #Methods
   initialize = function(){},

   log_status = function(nodes, iteration){
     if(is.null(self$error_per_node)){
       self$error_per_node <- data.frame(matrix(nrow=0, ncol=length(nodes)))
     }
     errors <- data.frame(matrix(nrow=1, ncol=length(nodes)+1))

     colnames(errors) <- iteration
     for(i in seq_along(nodes)){
       node <- nodes[[i]]

       errors[,i] <- node$error
       colnames(errors)[[i]] <- node$node_name
     }
     errors[,i+1] <- iteration
     colnames(errors)[[i+1]] <- "iteration"
     self$error_per_node <- rbind(self$error_per_node,errors)
   },

   show = function(){
     d <- melt(self$error_per_node, id.vars = "iteration")
     colnames(d)[colnames(d) == "variable"] <- "Node"
     colnames(d)[colnames(d) == "value"] <- "error"
     p = ggplot(d, aes(x = iteration, y=error, group = Node)) +
       geom_line(aes(color=Node))
     p

   }
 )
)

#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
VarianceLogger <- R6Class("VarianceLogger",
  public = list(
   #Fields
   total_variance_per_node = NULL,

   #Methods
   initialize = function(){},

   log_status = function(nodes, iteration){
     if(is.null(self$total_variance_per_node)){
       self$total_variance_per_node <- data.frame(matrix(nrow=0, ncol=length(nodes)))
     }
     total_variance <- data.frame(matrix(nrow=1, ncol=length(nodes)+1))

     colnames(total_variance) <- iteration
     for(i in seq_along(nodes)){
       node <- nodes[[i]]

       total_variance[,i] <- sum(node$variance_explained)
       colnames(total_variance)[[i]] <- node$node_name
     }
     total_variance[,i+1] <- iteration
     colnames(total_variance)[[i+1]] <- "iteration"
     self$total_variance_per_node <- rbind(self$total_variance_per_node,total_variance)
   },

   show = function(){
     d <- melt(self$total_variance_per_node, id.vars = "iteration")
     colnames(d)[colnames(d) == "variable"] <- "Node"
     colnames(d)[colnames(d) == "value"] <- "total_variance"
     p <- ggplot(d, aes(x = iteration, y=total_variance, group = Node)) +
       geom_line(aes(color=Node))
     p

   }
  )
)
