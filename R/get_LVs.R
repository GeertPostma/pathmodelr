#' Calculates the latent variables using the estimators for each node in an
#' iterative manner.
#'
#' This is an internal function used for the iterative estimation of the Latent
#' Variables.
#'
#' A convergence threshold of 0.0001 differnece between the Sum of Squared
#' Errors of two subsequent iterations is used.
#'
#' @param nodes A listenv of objects of the R6Class Node which are initialised.
#'   The nodes also need to be connected.
#' @param max_iterations An integer indicating the maximum number of iterations
#'   before execution is halted
#' @param loggers A listenv of R6Class based loggers and/or reporters
#' @return A listenv of connected, initialized, and estimated nodes
#' @import listenv
get_LVs <- function(nodes, max_iterations, loggers){ #Add options for different methods (SO-PLS, Multicomponent regression) in addition to just 1-1

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

    #Check convergence criterion:
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
