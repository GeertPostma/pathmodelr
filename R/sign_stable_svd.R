#Minimal only computes the first column of V (for PLSR).
#Based on: Bro, Rasmus, Evrim Acar, and Tamara G. Kolda. "Resolving the sign ambiguity in the singular value decomposition." Journal of Chemometrics 22.2 (2008): 135-140.
#' @export
sign_stable_svd <- function(X, minimal=FALSE){

  svd_res <- svd(X)

  if(minimal){
    u <- svd_res$u[,1, drop=FALSE]
    d <- svd_res$d[[1]]
    v <- svd_res$v[,1, drop=FALSE]
  }
  else{
    u <- svd_res$u
    d <- svd_res$d
    v <- svd_res$v
  }


  if(minimal){
    #Step 1

    s_left_parts <- vector(mode="numeric", length=dim(Y)[2])

    for(j in 1:dim(X)[2]){

      temp_prod <- t(u) %*% X[,j]

      s_left_parts[[j]] <- sign(temp_prod) * temp_prod^2
    }

    s_left <- sum(s_left_parts)

    #Step 2
    s_right_parts <- vector(mode="numeric", length=dim(Y)[1])

    for(i in 1:dim(X)[1]){

      temp_prod <- t(v) %*% t(X[i,,drop=FALSE])

      s_right_parts[[i]] <- sign(temp_prod) * temp_prod^2
    }

    s_right <- sum(s_right_parts)

    #Step 3
    if((s_right * s_left) < 0){
      if(s_left < s_right){
        s_left <- -s_left
      }
      else{
        s_right <- -s_right
      }
    }
    vn <- v * sign(s_right)
    return(list("v"=vn))

  }
  else{ #Follows mostly original, severely unoptimized, procedure. Only matrix calculation optimization in calculation of Y was done.
    K <- length(d)

    #Step 1
    s_left <- vector(mode="numeric", length=K)

    for(k in 1:K){
      Y <- X - u[,-k] %*% diag(d[-k], length(d[-k])) %*% t(v[,-k])

      s_left_parts <- vector(mode="numeric", length=dim(Y)[2])

      for(j in 1:dim(Y)[2]){

        temp_prod <- t(u[,k,drop=FALSE]) %*% Y[,j,drop=FALSE]

        s_left_parts[[j]] <- sign(temp_prod) * temp_prod^2
      }

      s_left[k] <- sum(s_left_parts)
    }

    #Step 2
    s_right <- vector(mode="numeric", length=K)

    for(k in 1:K){
      Y <- X - u[,-k] %*% diag(d[-k], length(d[-k])) %*% t(v[,-k])

      s_right_parts <- vector(mode="numeric", length=dim(Y)[1])

      for(i in 1:dim(Y)[1]){

        temp_prod <- t(v[,k,drop=FALSE]) %*% t(Y[i,,drop=FALSE])

        s_right_parts[[i]] <- sign(temp_prod) * temp_prod^2
      }

      s_right[k] <- sum(s_right_parts)
    }

    #Step 3
    for(k in 1:K){
      if((s_right[[k]] * s_left[[k]]) < 0){
        if(s_left[[k]] < s_right[[k]]){
          s_left[[k]] <- -s_left[[k]]
        }
        else{
          s_right[[k]] <- -s_right[[k]]
        }
      }
    }
    un <- u %*% diag(sign(s_left))
    vn <- v %*% diag(sign(s_right))

    return(list("d"=d,"u"=un,"v"=vn))

  }
}
