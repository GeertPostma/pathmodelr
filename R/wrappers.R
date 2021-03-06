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

#' @export
#' @import listenv
#' @import parallel
process_PLS <- function(data,
                        connection_matrix,
                        variables_in_block,
                        block_names           = NULL,
                        global_preprocessors  = list(),
                        local_preprocessors   = list(standardize, block_scale),
                        parallelise           = FALSE,
                        n_cores               = NULL,
                        bootstrap             = FALSE,
                        bootstrap_iter        = 200,
                        bootstrap_ci          = 0.95,
                        n_LVs                 = NULL,
                        max_n_LVs             = NULL){

  if(bootstrap){

    if(!is.null(max_n_LVs) | !is.null(n_LVs)){

      #TODO: build in check if n_LV matrix is valid.

      initializers <- listenv()
      node_class_types <- listenv()


      if(!is.null(max_n_LVs)){
        #Loop over max_n_LV vector
        for(i in 1:length(max_n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }

      }
      else if(!is.null(n_LVs)){
        #Loop over  n_LV vector
        for(i in 1:length(n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }
      }
    }
    bootstrap_process_PLS <- function(i){
      bootstrapped_data <- data[sample.int(dim(data)[1], replace=TRUE), ]


      if(!is.null(max_n_LVs) | !is.null(n_LVs)){
        tempmodel <- path_model(data,
                            connection_matrix,
                            variables_in_block,
                            block_names,
                            start_node_estimator    = "None",
                            middle_node_estimator   = "None",
                            end_node_estimator      = "None",
                            global_preprocessors    = global_preprocessors,
                            local_preprocessors     = local_preprocessors,
                            initializers            = initializers,
                            node_class_types        = node_class_types)
      }
      else{
        tempmodel <- path_model(bootstrapped_data,
                                connection_matrix,
                                variables_in_block,
                                block_names,
                                start_node_initializer  = "normalPLS",
                                middle_node_initializer = "normalPLS",
                                end_node_initializer    = "endPLS",
                                start_node_estimator    = "None",
                                middle_node_estimator   = "None",
                                end_node_estimator      = "None",
                                global_preprocessors    = global_preprocessors,
                                local_preprocessors     = local_preprocessors)
      }


      calculate_node_PLS_coefficients(tempmodel)

      tempmodel$path_effects <- get_all_path_effects(tempmodel)

      #Calculate all path effects, direct effects, and indirect effects
      inner_bootstrap_results <- list()

      inner_bootstrap_results$path_variances_explained <- calculate_PLS_variances_explained(tempmodel)
      temp_effects <- get_effects(inner_bootstrap_results$path_variances_explained)
      inner_bootstrap_results$variance_effects <- as.matrix(temp_effects[,2:4])
      rownames(inner_bootstrap_results$variance_effects) <- as.character(temp_effects$relationships)
      inner_bootstrap_results$inner_effects <- calculate_inner_effects(tempmodel)$effects
      inner_bootstrap_results$outer_effects <- calculate_outer_effects(tempmodel)$outer_effects

      return(inner_bootstrap_results)
    }

    if(parallelise){
      if(is.null(n_cores)){
        stop("If parallelisation is set to TRUE, n_cores must be set as well.")
      }

      cl <- makeCluster(n_cores)
      clusterExport(cl, c("data", "connection_matrix", "variables_in_block", "block_names", "global_preprocessors", "local_preprocessors", "convergence_threshold"), envir=environment())

      bootstrap_results <- parLapply(cl, 1:bootstrap_iter, bootstrap_process_PLS)

      stopCluster(cl)
    }
    else{
      bootstrap_results <- lapply(1:bootstrap_iter, bootstrap_process_PLS)
    }

    bootstrapped_path_variances <- simplify2array(lapply(bootstrap_results, function(bs_res) bs_res$path_variances_explained))
    bootstrapped_variance_effects <- simplify2array(lapply(bootstrap_results, function(bs_res) bs_res$variance_effects))
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
    mean_bootstrapped_variance_effects <- apply(bootstrapped_variance_effects, c(1,2), mean)
    mean_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, rowMeans)
    mean_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, rowMeans)

    median_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), median)
    median_bootstrapped_variance_effects <- apply(bootstrapped_variance_effects, c(1,2), median)
    median_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, function(x) apply(x, 1, median))
    median_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, function(x) apply(x, 1, median))

    ci_bootstrapped_path_variances <- get_path_ci(bootstrapped_path_variances, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_variance_effects <- get_path_ci(bootstrapped_variance_effects, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_inner_effects <- get_effects_ci(bootstrapped_inner_effects, bootstrap_iter, bootstrap_ci)
    ci_bootstrapped_outer_effects <- get_effects_ci(bootstrapped_outer_effects, bootstrap_iter, bootstrap_ci)

    stdev_bootstrapped_path_variances <- apply(bootstrapped_path_variances, c(1,2), sd)
    stdev_bootstrapped_variance_effects <- apply(bootstrapped_variance_effects, c(1,2), sd)
    stdev_bootstrapped_inner_effects <- lapply(bootstrapped_inner_effects, function(x) apply(x, 1, sd))
    stdev_bootstrapped_outer_effects <- lapply(bootstrapped_outer_effects, function(x) apply(x, 1, sd))


    bootstrap_results <-
      list(
        "path_variances" = list(
          "mean"   = mean_bootstrapped_path_variances,
          "median" = median_bootstrapped_path_variances,
          "ci"     = ci_bootstrapped_path_variances,
          "stdev"  = stdev_bootstrapped_path_variances),
        "variance_effects" = list(
          "mean"   = mean_bootstrapped_variance_effects,
          "median" = median_bootstrapped_variance_effects,
          "ci"     = ci_bootstrapped_variance_effects,
          "stdev"  = stdev_bootstrapped_variance_effects),
        "inner_effects" = list(
          "mean"   = mean_bootstrapped_inner_effects,
          "median" = median_bootstrapped_inner_effects,
          "ci"     = ci_bootstrapped_inner_effects,
          "stdev"  = stdev_bootstrapped_inner_effects),
        "outer_effects" = list(
          "mean"   = mean_bootstrapped_outer_effects,
          "median" = median_bootstrapped_outer_effects,
          "ci"     = ci_bootstrapped_outer_effects,
          "stdev"  = stdev_bootstrapped_inner_effects))

    #calculate original model
    if(!is.null(max_n_LVs) | !is.null(n_LVs)){

      #TODO: build in check if n_LV matrix is valid.

      initializers <- listenv()
      node_class_types <- listenv()


      if(!is.null(max_n_LVs)){
        #Loop over max_n_LV vector
        for(i in 1:length(max_n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }

      }
      else if(!is.null(n_LVs)){
        #Loop over  n_LV vector
        for(i in 1:length(n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }
      }

      model <- path_model(data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_estimator    = "None",
                          middle_node_estimator   = "None",
                          end_node_estimator      = "None",
                          global_preprocessors    = global_preprocessors,
                          local_preprocessors     = local_preprocessors,
                          initializers            = initializers,
                          node_class_types        = node_class_types)

    }
    else{
      model <- path_model(data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_initializer  = "normalPLS",
                          middle_node_initializer = "normalPLS",
                          end_node_initializer    = "endPLS",
                          start_node_estimator    = "None",
                          middle_node_estimator   = "None",
                          end_node_estimator      = "None",
                          global_preprocessors    = global_preprocessors,
                          local_preprocessors     = local_preprocessors)
    }

    #Calculate all path effects, direct effects, and indirect effects
    #Backwards regression pass
    calculate_node_PLS_coefficients(model)

    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)

    model$bootstrap_results <- bootstrap_results

  }

  else{


    p <- parallelise
    n <- n_cores


    #TODO: Check if not both n_LVs and max_n_LVs have been set

    if(!is.null(max_n_LVs) | !is.null(n_LVs)){

      #TODO: build in check if n_LV matrix is valid.

      initializers <- list()
      node_class_types <- listenv()


      if(!is.null(max_n_LVs)){
        #Loop over max_n_LV vector
        for(i in 1:length(max_n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, max_n_LVs=max_n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }

      }
      else if(!is.null(n_LVs)){
        #Loop over  n_LV vector
        for(i in 1:length(n_LVs)){
          if(sum(connection_matrix[,i]) > 0){
            in_function <- function(i) {force(i); function(node) normal_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          else{
            in_function <- function(i) {force(i); function(node) end_PLS_initializer(node, n_LVs=n_LVs[[i]])}
            initializers[[i]] <- in_function(i)
          }
          node_class_types[[i]] <- PLSNode
        }
      }

      model <- path_model(data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_estimator    = "None",
                          middle_node_estimator   = "None",
                          end_node_estimator      = "None",
                          global_preprocessors    = global_preprocessors,
                          local_preprocessors     = local_preprocessors,
                          parallelise             = p,
                          n_cores                 = n,
                          initializers            = initializers,
                          node_class_types        = node_class_types)

    }
    else{
      model <- path_model(data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_initializer  = "normalPLS",
                          middle_node_initializer = "normalPLS",
                          end_node_initializer    = "endPLS",
                          start_node_estimator    = "None",
                          middle_node_estimator   = "None",
                          end_node_estimator      = "None",
                          global_preprocessors    = global_preprocessors,
                          local_preprocessors     = local_preprocessors,
                          parallelise             = p,
                          n_cores                 = n)
    }

    #Calculate all path effects, direct effects, and indirect effects
    calculate_node_PLS_coefficients(model)

    model$path_effects <- get_all_path_effects(model)

    model$path_variances_explained <- calculate_PLS_variances_explained(model)

    model$inner_effects <- calculate_inner_effects(model)

    model$outer_effects <- calculate_outer_effects(model)
  }


  return(model)
}


#' @export
soplspm <- function(input_data,
                    connection_matrix,
                    variables_in_block,
                    block_names           = NULL,
                    global_preprocessors  = list(),
                    local_preprocessors   = list(standardize, block_scale),
                    n_LVs = NULL){


  #check connection matrix
  #TODO: Also allow for non fully-connected matrices which still satisfy requirements.
  if(!is_valid_soplspm_matrix(connection_matrix)){
    stop("The specified connection matrix is not at least a 2-by-2 square lower triangular and/or fully connected. Please respecify")
  }


 if(!is.null(n_LVs)){

   #TODO: build in check if n_LV matrix is valid.

   initializers <- listenv()
   node_class_types <- listenv()

   #Loop over dimensions of n_LV connection matrix
   for(i in 1:dim(n_LVs)[[2]]){

     if(sum(n_LVs[i,]) > 0){
       #TODO: check if passed function arguments are evaluated properly instead of errors due to lazy evaluation
       initializers[[i]] <- function(node) normal_SOPLS_initializer(node, n_LVs_per_block=force(n_LVs[i,n_LVs[i,] > 0]))
       node_class_types[[i]] <- SOPLSNode
      }
     else{
       initializers[[i]] <- no_initializer
       node_class_types[[i]] <- DataNode
     }

   }

   sopls_model <- path_model(input_data,
                             connection_matrix,
                             variables_in_block,
                             block_names,
                             start_node_estimator  = "None",
                             middle_node_estimator = "None",
                             end_node_estimator    = "None",
                             global_preprocessors  = global_preprocessors,
                             local_preprocessors   = local_preprocessors,
                             initializers          = initializers,
                             node_class_types      = node_class_types)
 }
  sopls_model <- path_model(input_data,
                          connection_matrix,
                          variables_in_block,
                          block_names,
                          start_node_initializer  = "startSOPLS",
                          middle_node_initializer = "normalSOPLS",
                          end_node_initializer    = "normalSOPLS",
                          start_node_estimator    = "None",
                          middle_node_estimator   = "None",
                          end_node_estimator      = "None",
                          global_preprocessors    = global_preprocessors,
                          local_preprocessors     = local_preprocessors)


  #calculate essential statistics

  #Return model
}
