#All initializers must call add_estimate!

PCA_initializer <- function(node){ #Simple PCA estimation
  
  rank = dim(node$preprocessed_X)[2] #TODO: Change to meaningful number based on Heuristic, bootstrapping, or cross validation
  
  PCA_object <- prcomp(node$preprocessed_X, scale. = FALSE, center = FALSE, rank = rank)
  
  LVs <- PCA_object$x
  n_LVs <- rank
  X_loadings <- PCA_object$rotation
  
  node$add_estimate(n_LVs, LVs, X_loadings)
}

full_initializer <- function(node){ #Simple Full estimation (generally used for end-nodes)
  
  LVs <- node$preprocessed_X
  n_LVs <- dim(node$preprocessed_X)[2]
  X_loadings <- diag(dim(node$preprocessed_X)[2])
  
  node$add_estimate(n_LVs, LVs, X_loadings)
}