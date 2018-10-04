#' Calculates the latent variables using the estimators for each node in an
#' iterative manner.
#'
#' This is an internal function used for the iterative estimation of the Latent
#' Variables.
#'
#' A convergence threshold of a difference of 0.0001 between the Sum of Squared
#' Errors of two subsequent iterations is used as a default.
#'
#' @param nodes A listenv of objects of the R6Class Node which are initialised.
#'   The nodes also need to be connected.
#' @param max_iterations An integer indicating the maximum number of iterations
#'   before execution of LV estimation is halted when the convergence criterion
#'   is not met beforehand.
#' @param loggers A listenv of R6Class based loggers and/or reporters. All
#'   loggers or reports must implement a \code{log_status()} method. Implemented
#'   loggers or reporters are ComponentLogger, IterationReporter,
#'   DurationLogger, and ConvergenceLogger.
#' @param convergence_threshold A double indicating the maximum error before the
#'   iterations are assumed to have converged. It is compared to the difference
#'   between the latent variables of the current and previous iteration. If this
#'   difference is less, the algorithm is considered to have converged. A
#'   convergence threshold of a difference of 0.0001 between the Sum of Squared
#'   Errors of two subsequent iterations is used as a default.
#' @return A listenv of connected, initialized, and estimated nodes
#' @import listenv
get_LVs <- function(nodes, max_iterations, loggers, convergence_threshold=0.0001){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1

  i <- 0
  converged <- FALSE

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

    #Check convergence criterion:
    total_error <- 0
    for(ii in seq_along(nodes)){
      total_error <- total_error + nodes[[ii]]$error
    }

    if(total_error < convergence_threshold){
      converged <- TRUE
    }
  }

  #Post processing loop:
  for(i in seq_along(nodes)){
   nodes[[i]]$post_process()
  }

  return(listenv("nodes"=nodes, "loggers"=loggers))

}
