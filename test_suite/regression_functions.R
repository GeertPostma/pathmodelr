#Regression functions

#follows original algorithm described by De Jong (uses same nomenclature) (T and t are replaced by TT and tt to avoid overloading of TRUE and transpose alias in R)
#assumes all relevant preprocessing is already done (including mean-centering!)
#Includes minimal version for when only predictions and loadings are needed
SIMPLS <- function(X,Y, max_n_comp=ncol(X), minimal=FALSE, covariance_mask=NULL){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  
  V <- matrix(0, nrow=ncol(X), ncol=max_n_comp)
  B <- array(0 , dim=c(ncol(X), ncol(Y), max_n_comp))
  Q <- matrix(0, nrow=ncol(Y), ncol=max_n_comp)
  Y_pred <- array(0, dim=c(nrow(X), ncol(Y), max_n_comp))
  
  if(!minimal){
    TT <- U <- matrix(0, nrow=nrow(X), ncol=max_n_comp)
    P <- R <- V
  }

  S <- crossprod(X,Y)     #Cross product
  
  if(!is.null(covariance_mask)){
    S <- S * covariance_mask
  }
  
  for(a in 1:max_n_comp){
    q <- svd(S)$v[,1]
    
    r <- S %*% q
    tt <- X %*% r
    tt <- mean_center(tt)$preprocessed_data
    tt_norm <- sqrt(crossprod(tt))[1, 1]
    tt <- tt / tt_norm
    r <- r / tt_norm
    
    p <- crossprod(X, tt)
    q <- crossprod(Y, tt)
    
    v <- p
    
    if(a > 1){
      v <- v - V %*% crossprod(V, p)
    }
    
    v <- v / sqrt(crossprod(v))[1, 1]
    S <- S - v %*% crossprod(v, S)
    
    V[, a] <- v
    Q[, a] <- q
    R[, a] <- r
    B[, , a] <- R[, 1:a, drop=FALSE] %*% t(Q)[1:a, , drop=FALSE]
    
    
    if(!minimal){
      
      TT[, a] <- tt
      u <- Y %*% q
      
      if(a > 1){
        u <- u - TT %*% crossprod(TT, u)
      }
      
      P[, a] <- p
      U[, a] <- u
      
      Y_pred <- X %*% B[, ,a]
    }

  }
  
  if(minimal){
    return(list("X_loadings"=V, 
                "Y_loadings"=Q, 
                "coefficients"=B))
  }
  else{
    return(list("X_loadings"=V, 
                "Y_loadings"=Q, 
                "coefficients"=B,
                "X_scores"=TT,
                "Y_scores"=U,
                "X_loadings_unorthogonalized"=P,
                "X_weights"=R
                ))
  }
}