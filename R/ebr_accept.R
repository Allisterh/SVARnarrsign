#' Check whether elasticity bounds hold
#'
#' @param EBR Object of ebt_setup
#' @param impulses Computed impulses based on parameter draws
#' @param Q A orthogonal matrix
#'
#' @return A Boolean whether elasticity bounds are fulfilled
#'

ebr_accept <- function(EBR = NULL,
                       impulses = NULL,
                       Q = NULL,
                       allshocknames,
                       worldprod = NULL,
                       data = data) {
  if (is.null(EBR)) {
    check <- 1
    return(check)
  }
  
  # here different way to check elasticities in case we want to do a kilian 2014
  # like model
  if (EBR == "kilian_murphy"){
    ProdMBPM <- worldprod*(30/1000)
    OECDCrudeDif <- data[,4]
    
    E <- matrix(NA,3,1)
    
    q <- Q[, which(allshocknames == "Flow demand shock"), drop = FALSE]
    
    ik_1 <- impulses[1, , ] %*% q
    ik_1 <- ik_1[1, ]
    
    ik_2 <- impulses[1, , ] %*% q
    ik_2 <- ik_2[3, ]
    elasticity <- ik_1 / ik_2
    E[1, ] <- elasticity < 0.04
    
    
    q <- Q[, which(allshocknames == "Speculative demand shock"), drop = FALSE]
    ik_1 <- impulses[1, , ] %*% q
    ik_1 <- ik_1[1, ]
    
    ik_2 <- impulses[1, , ] %*% q
    ik_2 <- ik_2[3, ]
    
    elasticity <- ik_1 / ik_2
    E[2, ] <- elasticity < 0.04
    
    
    q <- Q[, which(allshocknames == "Flow supply shock"), drop = FALSE]
    
    ik_1 <- impulses[1, , ] %*% q
    ik_1 <- ik_1[1, ]
    
    ik_2 <- impulses[1, , ] %*% q
    ik_2 <- ik_2[4, ]
    
    ik_3 <- impulses[1, , ] %*% q
    ik_3 <- ik_3[3, ]
    
    FlowNew=ProdMBPM*(1+ik_1)-mean(OECDCrudeDif)-ik_2
    Flow=ProdMBPM-mean(OECDCrudeDif)
    PctChange=100*(FlowNew-Flow)/Flow
    ElasUseSeries=PctChange/ik_3
    
    
    
    elasticity <- mean(ElasUseSeries) <= 0
    #elasticity <- TRUE
    
    E[3, ] <- elasticity 
    
    
    check <- prod(E)
    
    return(check)
  }

  E <- matrix(NA, nrow(EBR), 1)

  for (b in 1:nrow(EBR)) {
    q <- Q[, which(allshocknames == EBR$shock_names[b]), drop = FALSE]

    ik_1 <- impulses[EBR$horizon[b], , ] %*% q
    ik_1 <- ik_1[EBR$first_variable[b], ]

    ik_2 <- impulses[EBR$horizon[b], , ] %*% q
    ik_2 <- ik_2[EBR$second_variable[b], ]

    elasticity <- ik_1 / ik_2
    E[b, ] <- elasticity < EBR$bounds[b]
  }

  check <- prod(E)

  return(check)
}
