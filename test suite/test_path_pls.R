source('~/OFFON/MCPM_package/test suite/path_pls.R')
source('~/OFFON/MCPM_package/test suite/make_nodes.R')
source('~/OFFON/MCPM_package/test suite/check_arguments.R')
source('~/OFFON/MCPM_package/test suite/check_dag.R')
source('~/OFFON/MCPM_package/test suite/verify_dag.R')
source('~/OFFON/MCPM_package/test suite/get_node_type.R')
source('~/OFFON/MCPM_package/test suite/get_next_nodes.R')
source('~/OFFON/MCPM_package/test suite/get_previous_nodes.R')
source('~/OFFON/MCPM_package/test suite/node_classes.R')

library(MASS)
test_path_pls <- function(){
  
  data <- data.frame(replicate(10,MASS::mvrnorm(n=1000,0,1)))
  
  connection_matrix <- t(matrix(c(0,0,0,0,
                                  1,0,0,0,
                                  1,0,0,0,
                                  0,1,1,0),
                                  nrow=4, ncol=4))
  
  variables_in_block <- list(1:2, 3:5, 6:8, 9:10)
  
  block_names <- list("y1", "y2", "y3", "y4")
  
  result <- path_pls(data, connection_matrix, variables_in_block, block_names)
}
