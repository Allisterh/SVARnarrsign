#' Setup the elasticitiy bound restrictions
#'
#' This function sets up the elasticity bound restrictions.
#'
#' @param shock_names A vector of shock names on which elasticity bounds are
#' placed.
#' @param first_variable A vector of variable names of length shock names
#' @param second_variable A vector of variables names of length shock names
#' @param horizon A vector of horizons for which elasticity bound restrictions
#' should hold
#' @param bounds A vectir of numerics which specifiy the elasticity bounds
#'
#' @return
#' @export
#'
#' @examples
ebr_setup <- function(shock_names = NULL,
                      first_variable = NULL,
                      second_variable = NULL,
                      horizon = NULL,
                      bounds = NULL) {
  ebr <- dplyr::tibble(shock_names, first_variable, second_variable, horizon, bounds)

  ebr
}
