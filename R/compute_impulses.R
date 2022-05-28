#' Compute impulse responses
#' 
#' Taking the draws of parameter matrix and covariance matrix and compute impulse
#' responses for a predefined number of steps.
#' 
#' @param Bh A draw of the parameter matrix
#' @param swish A draw of the covariance matrix
#' @param nn A vector containging the amount of variables in the sytem, number
#' of lags and prediction horizon.
#'
#' @return A matrix with impulse responses


compute_impulses <-
  function(Bh,swish,nn){
    # extract some basic information from input
    nvar <- nn[1]
    lags <- nn[2]
    imstep <- nn[3]
    #
    ll <- lags + 1
    n1 <- nvar + 1
    
    nl <- nvar*lags
    #
    Ah <- t(Bh)
    # Define the matrix to save impulse responses to
    imf <- matrix(nrow=imstep, ncol=nvar*nvar)
    # D
    M <- matrix(nrow=nvar*imstep, ncol=nvar)
    #
    M[1:nvar,] <- t(swish)
    Mtem <- M[1:nvar,]
    #
    imf[1,] <- t(as.vector(Mtem))
    #
    ims2 <- imstep - 1
    ims1 <- min(c(ims2, lags))
    t <- 1
    while(t <=ims1){
      nt <- nvar*t
      ntt <- nvar*(t+1)
      tt <- t+1
      Mtem <- Ah[,1:nt] %*% M[1:nt,]
      M[n1:ntt,] <- M[1:nt,]
      M[1:nvar,] <- Mtem
      imf[tt,] <- t(as.vector(Mtem))
      t <- t+1
    }
    #
    for(t in ll:ims2){
      nt <- nvar*t
      ntt <- nvar*(t+1)
      tt <- t+1
      Mtem <- Ah[,1:nl] %*% M[1:nl,]
      M[n1:ntt,] <- M[1:nt,]
      M[1:nvar,] <- Mtem
      imf[tt,] <- t(as.vector(Mtem))
    }
    return(imf)
  }
