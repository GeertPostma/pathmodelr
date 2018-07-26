simple_PLS_estimator <- function(node){ #Simple PLS estimation (Does not work for end nodes)
  
}

PCA_estimator <- function(node){ #Simple PCA estimation
  
  
  
}

full_estimator <- function(node){ #Simple Full estimation (generally used for end-nodes)

  node$LVs <- node$X_data
  node$n_LVs <- dim(node$X_data)[2]
  node$X_loadings <- diag(dim(node$X_data)[2])
  
}