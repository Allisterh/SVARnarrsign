#' Check whether traditional sign restrictions hold
#'
#' Takes impulse responses and Q draw to check whether traditional sign restrictions
#' are fulfilled
#'
#' @param first A numeric which is the first period in which the restrictions
#' should hold
#' @param last A numeric which is the last period in which the restrictions
#' should hold
#' @param constrained A K X 1 vector containing the traditional restrictions
#' @param impulses A matrix containging the computed impulse responses
#' @param Q An orthogonal matrix
#' @param signshocks Which column of the Q matrix is considered
#'
#' @return A list numeric (1 if accepted, 0 if not accpeted)

tradsign_accept <-
  function(first,
           last,
           constrained,
           impulses,
           Q,
           signshocks,
           cum) {
    Q_out <- Q

    Q <- Q[, signshocks, drop = FALSE]


    constrained <- constrained[!is.na(constrained)]

    for (k in first:last) {
      ik <- impulses[k, , ] %*% Q

      # some variabels are understood as cumlative
      iks <- array(NA, dim = c(k, 1, ncol(Q_out)))
      for (l in 1:k) {
        iks[l, , ] <- impulses[l, , ] %*% Q
      }

      for (i in 1:length(cum)) {
        if (cum[i] == 1) {
          ik[i, ] <- sum(iks[, , i])
        }
      }




      for (i in 1:length(constrained)) {
        if (constrained[i] < 0) {
          value <- ik[-1.0 * constrained[i]]
        } else {
          value <- -1.0 * ik[constrained[i]]
        }
        #
        if (value < 0.0) {
          if (k == first & i == 1) {
            Q <- 1.0 * Q
            ik <- 1.0 * ik
            Q_out <- 1.0 * Q_out
          } # comment this one out and uncomment bracket below.
        } else {
          acc <- 0
          rwz <- list(
            Q = Q_out,
            acc = acc,
            ika = ik
          )
          return(rwz)
          # } # comment out this
        }
      }
    }
    acc <- 1
    rwz <- list(Q = Q_out, acc = acc, ika = ik)
    return(rwz)
  }
