#' Performs Latent Variable estimation given data and path model specifications
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
#' @param estimators A list of functions indicating which estimator should be
#'   used for which node. The ordering of the list should be the same as that of
#'   the \code{connection_matrix}. It is advised to only use this option when
#'   you want different estimators within a level of estimation (Start, Middle,
#'   End). All estimators must implement a common interface, taking only a
#'   R6Class Node object as input, and returning nothing. Instead, this function
#'   should update the node directly.
#' @param start_node_estimator An estimator function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", "PLS", or
#'   "Full" ) meant for estimation of a start type Node. All estimators must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
#' @param middle_node_estimator An estimator function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", "PLS", or
#'   "Full" ) meant for estimation of a middle type Node. All estimators must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
#' @param end_node_estimator An estimator function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", "PLS", or
#'   "Full" ) meant for estimation of an end type Node. All estimators must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
#' @param initializers A list of functions indicating which initializer should be
#'   used for which node. The ordering of the list should be the same as that of
#'   the \code{connection_matrix}. It is advised to only use this option when
#'   you want different initializers within a level of nodes (Start, Middle,
#'   End). All initializers must implement a common interface, taking only a
#'   R6Class Node object as input, and returning nothing. Instead, this function
#'   should update the node directly.
#' @param start_node_initializer An initializer function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", or
#'   "Full" ) meant for initializing a start type Node. All initializers must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
##' @param middle_node_initializer An initializer function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", or
#'   "Full" ) meant for initializing a middle type Node. All initializers must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
#' @param end_node_initializer An initializer function which estimates a R6Class
#'   Node object, or a string indicating the estimation method ("PCA", or
#'   "Full" ) meant for initializing an end type Node. All initializers must
#'   implement a common interface, taking only a R6Class Node object as input,
#'   and returning nothing. Instead, this function should update the node
#'   directly.
#' @param max_iterations An integer indicating the maximum number of iterations
#'   before execution of LV estimation is halted when the convergence criterion
#'   is not met beforehand.
#' @param loggers A listenv of R6Class based logger and/or reporter objects. All
#'   loggers or reports must implement a \code{log_status()} method. Implemented
#'   loggers or reporters are ComponentLogger, IterationReporter, DurationLogger,
#'   and ConvergenceLogger.
#' @param unique_node_preprocessing A Boolean indicating whether different nodes
#'   should be preprocesed differently. If \code{TRUE}, the global_preprocessors
#'   and local_preprocessors lists should be entered as a list of list of
#'   preprocessing functions.
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
#' @param unique_node_postprocessing A Boolean indicating whether different
#'   nodes should be postprocesed differently. If \code{TRUE}, postprocessor
#'   should be a list of postprocessing functions.
#' @param post_processor A postprocessing function which takes a node as an input
#'   and returns a scaling setting vector.
#' @param convergence_threshold A double indicating the maximum error before the
#'   iterations are assumed to have converged. It is compared to the difference
#'   between the latent variables of the current and previous iteration. If this
#'   difference is less, the algorithm is considered to have converged. A
#'   convergence threshold of a difference of 0.0001 between the Sum of Squared
#'   Errors of two subsequent iterations is used as a default.
#' @param node_class_types A listenv of R6Class Node classes, or R6Classes which
#'   inherit the Node class. This option should only be set when the estimators
#'   argument is provided and other Node types are needed, or when custom
#'   path_modeling methods require new Node inheriting types.
#' @param parallelise
#' @param n_cores
#' @param n_LVs
#' @return A listenv of connected, initialized, and estimated nodes
#' @export
#' @import listenv
#' @importFrom dplyr select
path_model <- function(data, connection_matrix, variables_in_block,
                       block_names                = NULL,
                       estimators                 = NULL,
                       start_node_estimator       = "PLS",
                       middle_node_estimator      = "PLS",
                       end_node_estimator         = "Full",
                       initializers               = NULL,
                       start_node_initializer     = "Full",
                       middle_node_initializer    = "Full",
                       end_node_initializer       = "Full",
                       max_iterations             = 100,
                       loggers                    = NULL,
                       unique_node_preprocessing  = FALSE,
                       global_preprocessors       = list(),
                       local_preprocessors        = list(standardize),
                       unique_node_postprocessing = FALSE,
                       post_processor             = NULL,
                       convergence_threshold      = 0.0001,
                       node_class_types           = NULL,
                       parallelise                = FALSE,
                       n_cores                    = NULL,
                       n_LVs                      = NULL
                     ){

  ##CHECK INPUT
  check_arguments(data, connection_matrix, variables_in_block,
                  block_names,
                  estimators)

  ##Construct data constructs:

  #Construct list of block_names and assign to connection_matrix
  if(is.null(block_names)){
    #get blocknames from connection matrix

    if(is.null(rownames(connection_matrix))){
      if(is.null(colnames(connection_matrix))){
        print("block_names weren't provided directly or contained in connection_matrix. block_names are set to numerical.")
        rownames(connection_matrix) <- 1:nrow((connection_matrix))
        colnames(connection_matrix) <- 1:nrow((connection_matrix))
        block_names <- 1:nrow((connection_matrix))
      }
      else{
        rownames(connection_matrix) <- colnames(connection_matrix)
        block_names <- colnames(connection_matrix)
      }
    }
    else{
      if(is.null(colnames(connection_matrix))){
        colnames(connection_matrix) <- rownames(connection_matrix)
        block_names <- rownames(connection_matrix)
      }
    }
  }
  else{
    rownames(connection_matrix) <- block_names
    colnames(connection_matrix) <- block_names
  }

  #Convert possible matrices to dataframes
  data <- as.data.frame(data)

  #Calculate number of blocks
  n_blocks <- length(block_names)

  #make blocked data
  blocked_data <- list()

  for(i in 1:n_blocks){ #index block_names and variables_in_block (also convert back to matrix)

    blocked_data[[i]] <- data.matrix(select(data, variables_in_block[[i]]))
  }
  names(blocked_data) <- block_names

  ##Get node types:
  node_connection_types <- get_all_node_connection_types(connection_matrix)

  ##Make preprocessor list:
  if(!unique_node_preprocessing){
    temp_local_functions <- local_preprocessors
    temp_global_functions <- global_preprocessors

    local_preprocessors <- list()
    global_preprocessors <- list()

    for(i in 1:n_blocks){
      local_preprocessors[[block_names[[i]]]] <- temp_local_functions
      global_preprocessors[[block_names[[i]]]] <- temp_global_functions
    }
  }

  ##Make postprocessor list:
  if(!unique_node_postprocessing){
    post_processors <- list()

    for(i in 1:n_blocks){
      post_processors[[block_names[[i]]]] <- post_processor
    }
  }

  ##Make estimator list:
  if(is.null(estimators)){
    p <- parallelise
    n <- n_cores
    l <- n_LVs
    estimators <- get_estimator_list(node_connection_types, start_node_estimator, middle_node_estimator, end_node_estimator, parallelise=p, n_cores=n, n_LVs=l)

  }

  ##Make initializer list:
  if(is.null(initializers)){
    initializers <- get_initializer_list(node_connection_types, start_node_initializer, middle_node_initializer, end_node_initializer)
  }

  ##Make listenv of node class types based on estimators and initializers:
  if(is.null(node_class_types)){
    node_class_types <- get_node_class_types(node_connection_types, start_node_estimator, middle_node_estimator, end_node_estimator, start_node_initializer, middle_node_initializer, end_node_initializer)
  }

  #make node structure graph
  nodes <- make_nodes(blocked_data, connection_matrix, block_names, estimators, initializers, local_preprocessors, global_preprocessors, node_class_types, post_processors)

  names(nodes) <- block_names

  ## Estimate LVs:

  model <- get_LVs(nodes, max_iterations, loggers, convergence_threshold=convergence_threshold)

  model$connection_matrix <- connection_matrix

  model$block_names <- block_names

  return(model)
}
