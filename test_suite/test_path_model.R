source('~/OFFON/MCPM_package/test_suite/path_model.R')
source('~/OFFON/MCPM_package/test_suite/make_nodes.R')
source('~/OFFON/MCPM_package/test_suite/check_arguments.R')
source('~/OFFON/MCPM_package/test_suite/check_dag.R')
source('~/OFFON/MCPM_package/test_suite/verify_dag.R')
source('~/OFFON/MCPM_package/test_suite/get_node_type.R')
source('~/OFFON/MCPM_package/test_suite/get_next_nodes.R')
source('~/OFFON/MCPM_package/test_suite/get_previous_nodes.R')
source('~/OFFON/MCPM_package/test_suite/Node.R')
source('~/OFFON/MCPM_package/test_suite/get_all_node_types.R')
source('~/OFFON/MCPM_package/test_suite/get_estimator_list.R')
source('~/OFFON/MCPM_package/test_suite/estimator_string_to_function.R')
source('~/OFFON/MCPM_package/test_suite/estimator_functions.R')
source('~/OFFON/MCPM_package/test_suite/get_initialization_list.R')
source('~/OFFON/MCPM_package/test_suite/initializer_string_to_function.R')
source('~/OFFON/MCPM_package/test_suite/initializer_functions.R')
source('~/OFFON/MCPM_package/test_suite/get_LVs.R')
source('~/OFFON/MCPM_package/test_suite/loggers_reporters.R')
source('~/OFFON/MCPM_package/test_suite/get_nodes_by_level.R')
source('~/OFFON/MCPM_package/test_suite/get_unique_nodes.R')
source('~/OFFON/MCPM_package/test_suite/get_same_level_connected_nodes.R')
source('~/OFFON/MCPM_package/test_suite/combine_and_mask.R')
source('~/OFFON/MCPM_package/test_suite/local_preprocessing_functions.R')
source('~/OFFON/MCPM_package/test_suite/regression_functions.R')
source('~/OFFON/MCPM_package/test_suite/calculate_SSE_for_matrices.R')
source('~/OFFON/MCPM_package/test_suite/MSE.R')
source('~/OFFON/MCPM_package/test_suite/cross_validate_node_PLS.R')

test_path_model <- function(){
  
  data <- data.frame(replicate(10,MASS::mvrnorm(n=1000,0,1)))
  
  connection_matrix <- t(matrix(c(0,0,0,0,
                                  1,0,0,0,
                                  1,0,0,0,
                                  0,1,1,0),
                                  nrow=4, ncol=4))
  
  variables_in_block <- list(1:2, 3:5, 6:8, 9:10)
  
  block_names <- list("y1", "y2", "y3", "y4")
  
  result <- path_model(data, connection_matrix, variables_in_block, block_names, start_node_estimator = "PCA", middle_node_estimator = "PCA", end_node_estimator = "PCA", loggers=listenv(ComponentLogger$new(), IterationReporter$new(), DurationLogger$new(report=TRUE)))
}
