library(MASS)
test_path_pls <- function(){
  
  data <- data.frame(replicate(10,mvrnorm(n=1000,0,1)))
  
  connection_matrix <- t(matrix(c(0,0,0,0,
                                  1,0,0,0,
                                  1,0,0,0,
                                  0,1,1,0),
                                  nrow=4, ncol=4))
  
  variables_in_block <- list(1:2, 3:5, 6:8, 9:10)
  
  block_names <- list("y1", "y2", "y3", "y4")
  
  result <- path_pls(data, connection_matrix, variables_in_block, block_names)
}