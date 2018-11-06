#' Estimate a process PLS model and calculate the effects of variables on blocks.
#'
#' @param data A dataframe or matrix containing the data belonging to the
#'   process that is being modelled. The rows contain the samples, the columns
#'   contain the variables.
#' @param connection_matrix A lower triangular matrix where the non zero
#'   elements what connections exist. The rows indicate the node where the edge
#'   is going to, and the columns indicates the node where the edge is coming
#'   from.
#' @param variables_in_block A list of lists indicating the variables
#'   corresponding to each block. The ordering of the upper list should be the
#'   same as that of the \code{connection_matrix}. The inner list values can be
#'   integers indicating the column number, or the name when column names are
#'   set in \code{data}.
#' @param block_names An ordered list containing the names that should be
#'   assigned to each block. The ordering of the list should be the same as that
#'   of the \code{connection_matrix}. When the argument is not set, names are
#'   extracted from \code{data}, or set to dummy names based on ordering when
#'   \code{data} contains no names.
#' @param loggers A listenv of R6Class based logger and/or reporter objects. All
#'   loggers or reports must implement a \code{log_status()} method. Implemented
#'   loggers or reporters are ComponentLogger, IterationReporter, DurationLogger,
#'   and ConvergenceLogger.
#' @param max_iterations An integer indicating the maximum number of iterations
#'   before execution of LV estimation is halted when the convergence criterion
#'   is not met beforehand.
#' @param global_preprocessors A list of preprocessing functions when
#'   unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
#'   functions when unique_node_preprocessing is \code{TRUE}. The ordering of
#'   the outer list should be the same as that of the \code{connection_matrix}.
#'   The order of the inner list is only important when the function results
#'   differ when their order of application is changed. The funtions will be
#'   applied from beginning to end. The supplied function must be invariant to
#'   subsampling. User-implemented functions must take a Matrix as input, and
#'   return the preprocessed matrix.
#' @param local_preprocessors A list of preprocessing functions when
#'   unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
#'   functions when unique_node_preprocessing is \code{TRUE}. The ordering of
#'   the outer list should be the same as that of the \code{connection_matrix}.
#'   The order of the inner list is only important when the function results
#'   differ when their order of application is changed. The funtions will be
#'   applied from beginning to end. The supplied function is assumed to be
#'   influenced by subsampling, and therefore can be applied on subsets when
#'   (cross-)validating. User-implemented functions must take a Matrix as input,
#'   and return the preprocessed matrix. Implemented functions are: (block_scale,
#'   standardize, and mean_center)
#' @param convergence_threshold A double indicating the maximum error before the
#'   iterations are assumed to have converged. It is compared to the difference
#'   between the latent variables of the current and previous iteration. If this
#'   difference is less, the algorithm is considered to have converged. A
#'   convergence threshold of a difference of 0.0001 between the Sum of Squared
#'   Errors of two subsequent iterations is used as a default.
#' @export
#' @import listenv
process_PLS <- function(data,
                        connection_matrix,
                        variables_in_block,
                        block_names           = NULL,
                        loggers               = listenv(IterationReporter$new(), DurationLogger$new(report=TRUE)),
                        max_iterations        = 20,
                        global_preprocessors  = list(),
                        local_preprocessors   = list(standardize, normalize),
                        postprocessor         = NULL,
                        convergence_threshold = 0.0001,
                        parallelise           = FALSE,
                        n_cores               = NULL,
                        n_LVs                 = NULL,
                        bootstrap             = FALSE,
                        bootstrap_iter        = 100,
                        bootstrap_ci          = 0.95){

  #Calculate standard PLS path model


  #Calculate single PLS path model
  if(bootstrap){
    #Use monte carlo style bootstrapping
    for(i in 1:bootstrap_iter){

      bootstrapped_data <- data[sample.int(dim(data)[1], replace=TRUE), ]

      p <- parallelise
      n <- n_cores
      l <- n_LVs
      model <- path_model(data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_estimator  = "PLS",
                          middle_node_estimator = "PLS",
                          end_node_estimator    = "Full",
                          loggers               = loggers,
                          max_iterations        = max_iterations,
                          global_preprocessors  = global_preprocessors,
                          local_preprocessors   = local_preprocessors,
                          convergence_threshold = convergence_threshold,
                          parallelise           = p,
                          n_cores               = n,
                          n_LVs                 = n_LVs)

      #Calculate all path effects, direct effects, and indirect effects
      model$path_effects <- get_all_path_effects(model)

      model$path_variances_explained <- calculate_PLS_variances_explained(model, scaling = "partial_variance")

      model$inner_effects <- calculate_inner_effects(model, scaling="variance")

      model$outer_effects <- calculate_outer_effects(model)
    }
  }
  else{
    p <- parallelise
    n <- n_cores
    l <- n_LVs
    model <- path_model(data,
                        connection_matrix,
                        variables_in_block,
                        block_names,
                        start_node_estimator  = "PLS",
                        middle_node_estimator = "PLS",
                        end_node_estimator    = "Full",
                        loggers               = loggers,
                        max_iterations        = max_iterations,
                        global_preprocessors  = global_preprocessors,
                        local_preprocessors   = local_preprocessors,
                        convergence_threshold = convergence_threshold,
                        parallelise           = p,
                        n_cores               = n,
                        n_LVs                 = n_LVs)

    #Calculate all path effects, direct effects, and indirect effects
    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model, scaling = "partial_variance")

    model$inner_effects <- calculate_inner_effects(model, "variance")

    model$outer_effects <- calculate_outer_effects(model)
  }


  return(model)
}

