#' Check whether elasticity bounds are satisfied
#' 
#' This function take the response of the first variable for the given horizon
#' and divides it by the response of the second variable. Then this 
#'
#' @param EBR Object of ebr_setup. 
#' @param impulses Computed impulses based on parameter draws. 
#' @param Q An orthogonal matrix. 
#' @param allshocknames Vector of all shocknames which are identified in the 
#' the system.
#' @param data A data frame or tibble containg the data
#'
#' @return A Boolean whether elasticity bounds are fulfilled
#'

ebr_accept <- function(EBR = NULL,
                       impulses = NULL,
                       Q = NULL,
                       allshocknames,
                       data = data) {
  # If no elasticity bounds need to be considered. Exit function here
  if (is.null(EBR)) {
    check <- 1
    return(check)
  }
  

  # Define elasticity matrix which stores which of the elasticity bounds
  # are satisfieed
  E <- matrix(NA, nrow(EBR), 1)

  for (b in 1:nrow(EBR)) {
    # Pick the column which corresponds to the shock in question
    q <- Q[, which(allshocknames == EBR$shock_names[b]), drop = FALSE]
    
    # compute impulse response
    ik_1 <- impulses[EBR$horizon[b], , ] %*% q
    # extract value for the first relevant variable
    ik_1 <- ik_1[EBR$first_variable[b], ]

    # compute impulse response
    ik_2 <- impulses[EBR$horizon[b], , ] %*% q
    # extract value for the second relevant variable
    ik_2 <- ik_2[EBR$second_variable[b], ]

    # compute elasticity as share of response of first variable divided by the 
    # response of the second variable
    elasticity <- ik_1 / ik_2
    
    # Check whether the computed elasticity falls into the respective bound and
    # save results as a boolean value
    E[b, ] <- elasticity < EBR$bounds[b]
  }
  
  # Check whether all of the elasticities fall into their respective bounds
  check <- prod(E)

  check
}
