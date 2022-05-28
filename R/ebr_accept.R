#' Check whether elasticity bounds hold
#'
#' @param EBR Object of ebt_setup
#' @param impulses Computed impulses based on parameter draws
#' @param Q A orthogonal matrix
#'
#' @return A Boolean whether elasticity bounds are fulfilled
#'
#' @examples
#' 
ebr_accept <- function(
  EBR = NULL,
  impulses = NULL,
  Q = NULL,
  allshocknames
){
  if (is.null(EBR)){
    check <- 1
    return(check)
  }
  
  E <- matrix(0,nrow(EBR),1)
  
  for (b in 1:nrow(EBR)){
    
    
    
    q <- Q[,which(allshocknames == EBR$shock_names[b]),drop=FALSE]
    
    ik_1 <- impulses[EBR$horizon[b], ,]%*%q
    ik_1 <- ik_1[EBR$first_variable[b],]
    
    ik_2 <- impulses[EBR$horizon[b], ,]%*%q
    ik_2 <- ik_2[EBR$second_variable[b],]
    
    elasticity <- ik_1/ik_2
    E[b,] <- elasticity < EBR$bounds[b]
  }
  
  check <- prod(E)
  
  return(check)
  
}
