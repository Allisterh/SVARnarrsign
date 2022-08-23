#' Estimate a SVAR model with narrative restrictions
#'
#' @param data A tibble or ts object containing the data
#' @param lags A numeric with the amound of lags to include
#' @param trad_signs An object as output from tradsign_setup
#' @param narr_restr An object as output from narrsign_setup. NULL if no narrative
#'  restrictions should be considered
#' @param nkeep Amount of draws to keep for the traditional sign restrictions
#' @param draws Amount of draws to make for parameter and covariance matrix
#' @param subdraws Amount of subdraws for orthogonal matrix Q for each
#' parameter matrix and covariance matrix
#' @param elasticitybounds An object as output from ebr_setup. NULL if no elasticity
#' bound should be considered
#' @param const Boolean whether constant should be considered
#' @param steps Amount of steps for which to compute the impulse responses
#' @param narrweightdraws Amount of draws for the importance sampler
#'
#' @return A list with outputs differentiating between model identified with 
#' traditional sign restrictions and narrative sign restictions.
#' 
#' @export
#'
#' @examples
narrsign <- function(data = NULL,
                     lags = 12,
                     trad_signs = NULL,
                     narr_restr = NULL,
                     nkeep = 1000,
                     draws = 300,
                     subdraws = 300,
                     elasticitybounds = NULL,
                     const = TRUE,
                     steps = 30,
                     narrweightdraws = 1000) {
  trad_m <- tradsign_estim(
    Y = data,
    nlags = lags,
    draws = draws,
    subdraws = subdraws,
    tradsign_setup = trad_signs,
    constant = const,
    steps = steps,
    EBR = elasticitybounds,
    nkeep = nkeep,
    # oil prod for kilian like restriction
    oil_production = oil_production
  )
  

  if (!is.null(narr_restr)) {
    # always removing the last entry as this is returned as NA for some reaosn
    # have not yet figured out why
    narr_m <- narrsign_accept(
      narrative_restr = narr_restr,
      irfs = trad_m$IRFS[1:(dim(trad_m$IRFS)[1] - 1), , , ],
      shocks = trad_m$SHOCKS[1:(dim(trad_m$IRFS)[1] - 1), , ],
      fevds = trad_m$FEVDS[1:(dim(trad_m$IRFS)[1] - 1), , , ],
      ndrawweights = narrweightdraws,
      data = data,
      lag = lags
    )
  }

  if (!is.null(narr_restr)){
    ret <- list(
      trad = trad_m, narr = narr_m, trad_restrict = trad_signs, narr_restrict = narr_restr,
      elasticity_bounds = elasticitybounds, savednarrative = dim(narr_m$IRFS)[1], data = data,
      varnames = colnames(data), shocknames = trad_signs$shocknames, dates = narr_restr$dates, lags = lag
    )
  } else {
    ret <- list(
      trad = trad_m, narr = NULL, trad_restrict = trad_signs, narr_restrict = NULL,
      elasticity_bounds = elasticitybounds, savednarrative = NULL, data = data,
      varnames = colnames(data), shocknames = trad_signs$shocknames, dates = NULL, lags = lag
    )
  }


  ret
}
