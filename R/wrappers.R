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
LV_process_PLS <- function(data,
                        connection_matrix,
                        variables_in_block,
                        block_names           = NULL,
                        loggers               = listenv(IterationReporter$new(), DurationLogger$new(report=TRUE)),
                        max_iterations        = 20,
                        global_preprocessors  = list(),
                        local_preprocessors   = list(standardize, block_scale),
                        post_processor        = NULL,
                        convergence_threshold = 0.0001,
                        parallelise           = FALSE,
                        n_cores               = NULL,
                        n_LVs                 = NULL,
                        bootstrap             = FALSE,
                        bootstrap_iter        = 100,
                        bootstrap_ci          = 0.95){

  if(bootstrap){
    #Use monte carlo style bootstrapping

    #n_lvs_per_bootstrap_iter <- list()

    bootstrap_process_PLS <- function(i){
      bootstrapped_data <- data[sample.int(dim(data)[1], replace=TRUE), ]

      l <- n_LVs
      tempmodel <- path_model(bootstrapped_data,
                              connection_matrix,
                              variables_in_block,
                              block_names,
                              start_node_initializer = "PLS",
                              middle_node_estimator  = "PLS",
                              end_node_estimator     = "PLS",
                              start_node_estimator   = "PLS",
                              middle_node_estimator  = "PLS",
                              end_node_estimator     = "PLS",
                              max_iterations         = max_iterations,
                              global_preprocessors   = global_preprocessors,
                              local_preprocessors    = local_preprocessors,
                              post_processor         = post_processor,
                              convergence_threshold  = convergence_threshold,
                              n_LVs                  = n_LVs)

      tempmodel$path_effects <- get_all_path_effects(tempmodel)

      #Calculate all path effects, direct effects, and indirect effects
      inner_bootstrap_results <- list()

      #inner_bootstrap_results$path_effects <- get_all_path_effects(model)

      inner_bootstrap_results$path_variances_explained <- calculate_PLS_variances_explained(tempmodel)
      inner_bootstrap_results$inner_effects <- calculate_inner_effects(tempmodel)$effects
      inner_bootstrap_results$outer_effects <- calculate_outer_effects(tempmodel)$outer_effects

      return(inner_bootstrap_results)
    }

    cl <- makeCluster(n_cores)

    bootstrap_results <- parLapply(cl, 1:bootstrap_iter, bootstrap_process_PLS)

    #bootstrap_results <- lapply(1:bootstrap_iter, bootstrap_process_PLS)

    stopCluster(cl)

    bootstrapped_path_variances <- simplify2array(lapply(bootstrap_results, function(bs_res) bs_res$path_variances_explained))
    inner_effects <- lapply(bootstrap_results, function(bs_res) bs_res$inner_effects)
    outer_effects <- lapply(bootstrap_results, function(bs_res) bs_res$outer_effects)


    bootstrapped_inner_effects <- list()
    e <- inner_effects[[1]]
    for(name in names(e)){
      bootstrapped_inner_effects[[name]] <- simplify2array(lapply(inner_effects, function(inner_effect) inner_effect[[name]]))
      if(is.null(dim(bootstrapped_inner_effects[[name]]))){
        bootstrapped_inner_effects[[name]] <- array(bootstrapped_inner_effects[[name]], dim = c(1,length(bootstrapped_inner_effects[[name]])))
      }
    }
    bootstrapped_outer_effects <- list()
    e <- outer_effects[[1]]
    for(name in names(e)){
      bootstrapped_outer_effects[[name]] <- simplify2array(lapply(outer_effects, function(outer_effect) outer_effect[[name]]))
      if(is.null(dim(bootstrapped_outer_effects[[name]]))){
        bootstrapped_outer_effects[[name]] <- array(bootstrapped_outer_effects[[name]], dim = c(1,length(bootstrapped_outer_effects[[name]])))
      }
    }


    mean_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), mean)
    mean_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, rowMeans)
    mean_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, rowMeans)

    median_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), median)
    median_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, function(x) apply(x, 1, median))
    median_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, function(x) apply(x, 1, median))

    ci_bootstrapped_path_variances <- get_path_ci(bootstrapped_path_variances, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_inner_effects <- get_effects_ci(bootstrapped_inner_effects, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_outer_effects <- get_effects_ci(bootstrapped_outer_effects, bootstrap_iter, bootstrap_ci)


    bootstrap_results <-
      list(
        "path_variances" = list(
          "mean"   = mean_bootstrapped_path_variances,
          "median" = median_bootstrapped_path_variances,
          "ci"     = ci_bootstrapped_path_variances),
        "inner_effects" = list(
          "mean"   = mean_bootstrapped_inner_effects,
          "median" = median_bootstrapped_inner_effects,
          "ci"     = ci_bootstrapped_inner_effects),
        "outer_effects" = list(
          "mean"   = mean_bootstrapped_outer_effects,
          "median" = median_bootstrapped_outer_effects,
          "ci"     = ci_bootstrapped_outer_effects))

    #calculate original model
    l <- n_LVs
    model <- path_model(data,
                        connection_matrix,
                        variables_in_block,
                        block_names,
                        start_node_initializer = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        start_node_estimator   = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        loggers                = loggers,
                        max_iterations         = max_iterations,
                        global_preprocessors   = global_preprocessors,
                        local_preprocessors    = local_preprocessors,
                        post_processor         = post_processor,
                        convergence_threshold  = convergence_threshold,
                        n_LVs                  = n_LVs)

    #Calculate all path effects, direct effects, and indirect effects
    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)

    model$bootstrap_results <- bootstrap_results

    }

  else{
    p <- parallelise
    n <- n_cores
    l <- n_LVs
    model <- path_model(data,
                        connection_matrix,
                        variables_in_block,
                        block_names,
                        start_node_initializer = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        start_node_estimator   = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        loggers                = loggers,
                        max_iterations         = max_iterations,
                        global_preprocessors   = global_preprocessors,
                        local_preprocessors    = local_preprocessors,
                        post_processor         = post_processor,
                        convergence_threshold  = convergence_threshold,
                        parallelise            = p,
                        n_cores                = n,
                        n_LVs                  = n_LVs)

    #Calculate all path effects, direct effects, and indirect effects
    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)
  }


  return(model)
}

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
manifest_process_PLS <- function(data,
                                 connection_matrix,
                                 variables_in_block,
                                 block_names           = NULL,
                                 loggers               = listenv(IterationReporter$new(), DurationLogger$new(report=TRUE)),
                                 max_iterations        = 20,
                                 global_preprocessors  = list(),
                                 local_preprocessors   = list(standardize, block_scale),
                                 post_processor        = NULL,
                                 convergence_threshold = 0.0001,
                                 parallelise           = FALSE,
                                 n_cores               = NULL,
                                 n_LVs                 = NULL,
                                 bootstrap             = FALSE,
                                 bootstrap_iter        = 100,
                                 bootstrap_ci          = 0.95){

  if(bootstrap){
    #Use monte carlo style bootstrapping

    #n_lvs_per_bootstrap_iter <- list()

    bootstrap_process_PLS <- function(i){
      bootstrapped_data <- data[sample.int(dim(data)[1], replace=TRUE), ]

      l <- n_LVs
      tempmodel <- path_model(bootstrapped_data,
                              connection_matrix,
                              variables_in_block,
                              block_names,
                              start_node_initializer = "PLS",
                              middle_node_estimator  = "PLS",
                              end_node_estimator     = "PLS",
                              start_node_estimator   = "None",
                              middle_node_estimator  = "None",
                              end_node_estimator     = "None",
                              max_iterations         = max_iterations,
                              global_preprocessors   = global_preprocessors,
                              local_preprocessors    = local_preprocessors,
                              post_processor         = post_processor,
                              convergence_threshold  = convergence_threshold,
                              n_LVs                  = n_LVs)

      tempmodel$path_effects <- get_all_path_effects(tempmodel)

      #Calculate all path effects, direct effects, and indirect effects
      inner_bootstrap_results <- list()

      #inner_bootstrap_results$path_effects <- get_all_path_effects(model)

      inner_bootstrap_results$path_variances_explained <- calculate_PLS_variances_explained(tempmodel)
      inner_bootstrap_results$inner_effects <- calculate_inner_effects(tempmodel)$effects
      inner_bootstrap_results$outer_effects <- calculate_outer_effects(tempmodel)$outer_effects

      return(inner_bootstrap_results)
    }

    cl <- makeCluster(n_cores)

    bootstrap_results <- parLapply(cl, 1:bootstrap_iter, bootstrap_process_PLS)

    #bootstrap_results <- lapply(1:bootstrap_iter, bootstrap_process_PLS)

    stopCluster(cl)

    bootstrapped_path_variances <- simplify2array(lapply(bootstrap_results, function(bs_res) bs_res$path_variances_explained))
    inner_effects <- lapply(bootstrap_results, function(bs_res) bs_res$inner_effects)
    outer_effects <- lapply(bootstrap_results, function(bs_res) bs_res$outer_effects)


    bootstrapped_inner_effects <- list()
    e <- inner_effects[[1]]
    for(name in names(e)){
      bootstrapped_inner_effects[[name]] <- simplify2array(lapply(inner_effects, function(inner_effect) inner_effect[[name]]))
      if(is.null(dim(bootstrapped_inner_effects[[name]]))){
        bootstrapped_inner_effects[[name]] <- array(bootstrapped_inner_effects[[name]], dim = c(1,length(bootstrapped_inner_effects[[name]])))
      }
    }
    bootstrapped_outer_effects <- list()
    e <- outer_effects[[1]]
    for(name in names(e)){
      bootstrapped_outer_effects[[name]] <- simplify2array(lapply(outer_effects, function(outer_effect) outer_effect[[name]]))
      if(is.null(dim(bootstrapped_outer_effects[[name]]))){
        bootstrapped_outer_effects[[name]] <- array(bootstrapped_outer_effects[[name]], dim = c(1,length(bootstrapped_outer_effects[[name]])))
      }
    }


    mean_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), mean)
    mean_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, rowMeans)
    mean_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, rowMeans)

    median_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), median)
    median_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, function(x) apply(x, 1, median))
    median_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, function(x) apply(x, 1, median))

    ci_bootstrapped_path_variances <- get_path_ci(bootstrapped_path_variances, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_inner_effects <- get_effects_ci(bootstrapped_inner_effects, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_outer_effects <- get_effects_ci(bootstrapped_outer_effects, bootstrap_iter, bootstrap_ci)


    bootstrap_results <-
      list(
        "path_variances" = list(
          "mean"   = mean_bootstrapped_path_variances,
          "median" = median_bootstrapped_path_variances,
          "ci"     = ci_bootstrapped_path_variances),
        "inner_effects" = list(
          "mean"   = mean_bootstrapped_inner_effects,
          "median" = median_bootstrapped_inner_effects,
          "ci"     = ci_bootstrapped_inner_effects),
        "outer_effects" = list(
          "mean"   = mean_bootstrapped_outer_effects,
          "median" = median_bootstrapped_outer_effects,
          "ci"     = ci_bootstrapped_outer_effects))

    #calculate original model
    l <- n_LVs
    model <- path_model(data,
                        connection_matrix,
                        variables_in_block,
                        block_names,
                        start_node_initializer = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        start_node_estimator   = "None",
                        middle_node_estimator  = "None",
                        end_node_estimator     = "None",
                        loggers                = loggers,
                        max_iterations         = max_iterations,
                        global_preprocessors   = global_preprocessors,
                        local_preprocessors    = local_preprocessors,
                        post_processor         = post_processor,
                        convergence_threshold  = convergence_threshold,
                        n_LVs                  = n_LVs)

    #Calculate all path effects, direct effects, and indirect effects
    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)

    model$bootstrap_results <- bootstrap_results

  }

  else{
    p <- parallelise
    n <- n_cores
    l <- n_LVs
    model <- path_model(data,
                        connection_matrix,
                        variables_in_block,
                        block_names,
                        start_node_initializer = "PLS",
                        middle_node_estimator  = "PLS",
                        end_node_estimator     = "PLS",
                        start_node_estimator   = "None",
                        middle_node_estimator  = "None",
                        end_node_estimator     = "None",
                        loggers                = loggers,
                        max_iterations         = max_iterations,
                        global_preprocessors   = global_preprocessors,
                        local_preprocessors    = local_preprocessors,
                        post_processor         = post_processor,
                        convergence_threshold  = convergence_threshold,
                        parallelise            = p,
                        n_cores                = n,
                        n_LVs                  = n_LVs)

    #Calculate all path effects, direct effects, and indirect effects
    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)
  }


  return(model)
}

