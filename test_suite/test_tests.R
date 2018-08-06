test_tests <- function(){
  #DAGs are transposed to show the matrix structure more clearly.
  
  
  ##Legal DAGs
  DAG1 <- t(matrix(c(0,0,0,0,
                     1,0,0,0,
                     1,0,0,0,
                     0,1,1,0),
                  nrow=4, ncol=4))
  
  DAG1_results <- verify_dag(DAG1)
  
  DAG2 <- t(matrix(c(0,0,0,0,
                     1,0,0,0,
                     0,1,0,0,
                     0,0,1,0),
                   nrow=4, ncol=4))
  
  DAG2_results <- verify_dag(DAG2)
  
  DAG3 <- t(matrix(c(0,0,0,0,
                     0,0,0,0,
                     1,0,0,0,
                     1,1,1,0),
                   nrow=4, ncol=4))
  
  DAG3_results <- verify_dag(DAG3)
  
  ##Illegal DAGs
  
  
  return(TRUE)
}