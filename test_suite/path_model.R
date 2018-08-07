library(dplyr)
library(ggplot2) #suggest for ComponentLogger
library(reshape2) #suggest for ComponentLogger
library(R6)
library(listenv)
path_model <- function(data, connection_matrix, variables_in_block,
                       block_names,
                       estimators = NULL,
                       start_node_estimator  = "PLS",
                       middle_node_estimator = "PLS",
                       end_node_estimator    = "PLS",
                       initializers = NULL,
                       start_node_initialization  = "Full",
                       middle_node_initialization = "Full",
                       end_node_initialization    = "Full",
                       max_iterations = 100,
                       loggers = NULL, #listenv of R6 loggers
                       unique_node_preprocessing = FALSE, #If TRUE => list of list of functions should be supplied for global and local preprocessors
                       global_preprocessors = NULL, #preprocessing methods that can be applied on the full data (such as log scaling)
                       local_preprocessors = NULL #preprocessing methods that can only be applied locally on training and test sets seperately (such as standardizing or mean-centering)
                     #component_selection="auto", n_comps=NULL, (can be list or set number per node)
                     #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                                          #input_variable_type: assigns what type each different variable has
                     #bootstrap="FALSE", bootstrap_iter=NULL
                     ){

  ##CHECK INPUT
  check_arguments(data, connection_matrix, variables_in_block,
                  block_names,
                  estimators
                  #start_node_estimator,
                  #middle_node_estimator,
                  #end_node_estimator
                  #use_modes,
                  #component_selection="auto", n_comps=NULL,
                  #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                  #preprocessing settings: standardizing, mean-centering, {Assign per block, include scaling for categorical variables and spectra}
                  #input_variable_type: assigns what type each different variable has
                  #bootstrap="FALSE", bootstrap_iter=NULL
  )
  
  ##Construct data constructs:
  
  #Construct list of block_names and assign to connection_matrix
  #TODO simplify if else structure
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
  
  #TODO: Determine whether sfubblocking structure should be assigned
  #Proposed subblocking structure: Block > {Xin, Xout} > subblock division
  for(i in 1:n_blocks){ #index block_names and variables_in_block
    
    blocked_data[[i]] <- select(data, variables_in_block[[i]])
  }
  names(blocked_data) <- block_names
  
  ##Get node types:
  node_types <- get_all_node_types(connection_matrix)
  
  ##Make estimator list:
  if(is.null(estimators)){
    estimators <- get_estimator_list(node_types, start_node_estimator, middle_node_estimator, end_node_estimator)
  }
  
  ##Make initializer list:
  if(is.null(initializers)){
    initializers <- get_initialization_list(node_types, start_node_initialization, middle_node_initialization, end_node_initialization)
  }
  
  #make node structure graph
  nodes <- make_nodes(blocked_data, connection_matrix, block_names, estimators, initializers, node_types)
  
  ## Estimate LVs:
  result <- get_LVs(nodes, max_iterations, loggers)#, connection_matrix
  #        ) #TODO: Add additional options after minimal working version
  
  #4# Calculate Inner Model
  #Statistics: Inner model regressiion (PLSR), GoF, R^2, etc.
  
  


}
