path_pls <- function(data, connection_matrix, vars_in_block
                     #component_selection="auto", n_comps=NULL,
                     #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                     #preprocessing settings: standardizing, mean-centering, {Assign per block, include scaling for categorical variables and spectra}
                     #input_variable_type: assigns what type each different variable has
                     #bootstrap="FALSE", bootstrap_iter=NULL
                     ){

  ##CHECK INPUT
  check_arguments(data, connection_matrix, vars_in_block
                  #component_selection="auto", n_comps=NULL,
                  #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                  #preprocessing settings: standardizing, mean-centering, {Assign per block, include scaling for categorical variables and spectra}
                  #input_variable_type: assigns what type each different variable has
                  #bootstrap="FALSE", bootstrap_iter=NULL
  )

  ##Pre-Process:

  ## MAIN ALGORITHM:
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
