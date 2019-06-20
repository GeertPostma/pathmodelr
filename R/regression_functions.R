#Regression functions

SOPLS <- function(X_list, Y, n_LVs_per_block){

}

#follows original algorithm described by De Jong (uses same nomenclature) (T and t are replaced by TT and tt to avoid overloading of TRUE and transpose aliases in R)
#assumes all relevant preprocessing is already done (including mean-centering!)
#Includes minimal version for when only predictions and loadings are needed
SIMPLS <- function(X,Y, max_n_comp=ncol(X), minimal=FALSE, sign_stable=FALSE){

  V <- R <- matrix(0, nrow=ncol(X), ncol=max_n_comp)
  B <- array(0 , dim=c(ncol(X), ncol(Y), max_n_comp))
  Q <- matrix(0, nrow=ncol(Y), ncol=max_n_comp)

  if(!minimal){
    TT <- U <- matrix(0, nrow=nrow(X), ncol=max_n_comp)
    P <- V
  }

  S <- crossprod(X,Y)     #Cross product

  for(a in 1:max_n_comp){
    if(sign_stable){
      q <- sign_stable_svd(S, minimal=TRUE)$v[,1]
    }
    else{
      q <- svd(S)$v[,1]
    }


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
    }

  }
  if(!minimal){
    X_variance_explained <- diag(crossprod(P, P)) / (sum(X^2))
    Y_variance_explained <- diag(crossprod(Q, Q)) / (sum(Y^2))
  }
  if(minimal){
    return(list("X_loadings"=V,
                "Y_loadings"=Q,
                "coefficients"=B,
                "X_weights"=R))
  }
  else{
    return(list("X_loadings"=V,
                "Y_loadings"=Q,
                "coefficients"=B,
                "X_scores"=TT,
                "Y_scores"=U,
                "X_loadings_unorthogonalized"=P,
                "X_weights"=R,
                "X_variance_explained"=X_variance_explained,
                "Y_variance_explained"=Y_variance_explained))
  }
}
