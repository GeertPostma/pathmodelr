library(dplyr)
library(R6)
library(listenv)
path_pls <- function(data, connection_matrix, variables_in_block,
                     block_names
                     #last_block_estimation = {direct, PCA, Y_loadings}
                     #use_modes = FALSE, #Determines whether or not Reflective and Formative modes should be used.
                     #component_selection="auto", n_comps=NULL,
                     #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                     #preprocessing settings: standardizing, mean-centering, {Assign per block, include scaling for categorical variables and spectra}
                     #input_variable_type: assigns what type each different variable has
                     #bootstrap="FALSE", bootstrap_iter=NULL
                     ){

  ##CHECK INPUT
  check_arguments(data, connection_matrix, variables_in_block,
                  block_names
                  #last_block_estimation,
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
  
  #TODO: Determine whether subblocking structure should be assigned
  #Proposed subblocking structure: Block > {Xin, Xout} > subblock division
  for(i in 1:n_blocks){ #index block_names and variables_in_block
    
    blocked_data[[i]] <- select(data, variables_in_block[[i]])
  
  }
  names(blocked_data) <- block_names
  

  
  
  ##Pre-Process:
  preprocessed_blocked_data <- blocked_data
  
  
  #make node structure graph
  nodes <- make_nodes(preprocessed_blocked_data, connection_matrix, block_names)
  
  ## MAIN ALGORITHM:
  #get_LVs(pre_processed_blocked_data#, connection_matrix
  #        ) #TODO: Add additional options after minimal working version
  
  #TODO: UPDATE REQUIREMENTS BASED ON NOTES, OPTIONS, AND EDGE CASES.
  #1# Initialisation:
  #PLSR (use x_out and y_in as predictors for y_out. use y_scores for end node, x_scores for predictors. See [?] at step 2 for edge case handling.)
  #Others methods are technically possible
  
  #2# Connection Estimation:
  #use PLSR to estimate #LVs per block and calculate scores for each block. use precursor block scores and input to predict block output.
  #[?] What if initial start block has only output variables -> use output as input for predicting upward connected block scores.
  
  #3# Recalculate Loadings:
  #Recalculate loadings using PLSR or Ridge Regression (less sensible), to only use within block variables for predicting LVs. (Either in or out variables)
  
  #3.5# Convergence: (Optional)
  # Iterate 2 and 3 until convergence.
  
  #4# Calculate Inner Model
  #Statistics: Inner model regressiion (PLSR), GoF, R^2, etc.

  ##Comments:
  #USE SIMPLS instead of NIPALS for interpretation purposes when possible -> careful with NaNs. Still use NIPALS for NaN data?
  #Inquire with Geert P about the pros and cons of kernel-PLS.



}
