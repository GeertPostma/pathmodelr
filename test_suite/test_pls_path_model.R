test_pls_path_model <- function(){
  
  data <- data.frame(replicate(12,MASS::mvrnorm(n=1000,0,1)))
  
  connection_matrix <- t(matrix(c(0,0,0,0,0,
                                  1,0,0,0,0,
                                  1,0,0,0,0,
                                  0,1,1,0,0,
                                  0,0,1,0,0),
                                nrow=5, ncol=5))
  
  variables_in_block <- list(1:2, 3:5, 6:8, 9:10, 11:12)
  
  block_names <- list("y1", "y2", "y3", "y4", "y5")
  
  model <- path_model(data, connection_matrix, variables_in_block, block_names, start_node_estimator = "PLS", middle_node_estimator = "PLS", end_node_estimator = "PCA", loggers=listenv(ComponentLogger$new(), IterationReporter$new(), DurationLogger$new(report=TRUE)))
  
}