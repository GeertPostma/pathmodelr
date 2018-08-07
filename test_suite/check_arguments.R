check_arguments <- function(data, connection_matrix, variables_in_block,
                            block_names,
                            estimators
                            #component_selection="auto", n_comps=NULL,
                            #sub_blocks=FALSE, sub_block_assignment=NULL, sub_block_scaling_method=NULL
                            #preprocessing settings: standardizing, mean-centering, {Assign per block, include scaling for categorical variables and spectra}
                            #input_variable_type: assigns what type each different variable has
                            #bootstrap="FALSE", bootstrap_iter=NULL
){
  error_list <- c()
  warning_list <- c()
  
  #Is the graph a DAG?
  dag_results <- verify_dag(connection_matrix)
  error_list <- c(error_list, dag_results["error_list"])
  warning_list <- c(warning_list, dag_results["warning_list"])
  
  #Is data numeric?
  if(!is.numeric(data)){
    error_list <- c(error_list, "The data matrix contains non-numerical data.")
  }
  
  #Give warning if both block_names or rownames and/or colnames connection_matrix are not supplied.
  
  #Check if data is matrix, dataframe or tidyverse equivalents
  if(!(is.matrix(data) || is.data.frame(data))){
    error_list <- c(error_list, "Input data must be a matrix, dataframe or a tidyverse dataframe equivalent.") 
  }
  
  
  #Check whether connection_matrix, variables_in_block and block_names refer to the same number of blocks.
  
  #NaN checking and warnings
  
  
  #Are all variables assigned? Are all named variables in the data?
  
  
  #Are all paired optional parameters set if one is set?
  
  
  are_valid = TRUE
}
  