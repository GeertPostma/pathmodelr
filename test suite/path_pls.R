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

  ##Pre-Process

  ##MAIN ALGORITHM

  #For each block:
  #Regress dependent block variables on predictors from connected blocks or from predictors from the same block.
  #Cross validate number of components per block -> use method from elements of statistical learning?

  #USE SIMPLS instead of NIPALS for interpretation purposes when possible -> careful with NaNs. Still use NIPALS for NaN data?
  #Inquire with Geert P about the pros and cons of kernel-PLS.

  ##OUTPUT STATISTICS


}
