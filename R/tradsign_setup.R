#' Setup up of traditional sign restrictions.
#'
#' This function sets up the traditional sign restrictions. Notice that
#' these restrictions can even be more than 1 for each structural shock.
#'
#' @param data Orignal data used for the model.
#' @param shock_names A vector of shock names on which traditional sign
#' restrictions. Notice this can be greather than the number of variables in
#' case one wants to place different kinds of restrictions with different horizons
#' on each structural shock.
#' @param restr_matrix A matrix of dimensions shock names x K where each row
#' represent one sign restriction.
#' @param hor_matrix A matrix which defines the hoirzons for which the restrictions
#' are supposed to hold
#'
#' @return A list which contains restrictions hoirzons and shock names
#' @export
#'
#' @examples
tradsign_setup <- function(data = NULL,
                           shock_names = NULL,
                           restr_matrix = NULL,
                           hor_matrix = NULL,
                           cum = NULL) {
  # number of varibles in the system
  nvar <- ncol(data)

  # if(!all(dim(restr_matrix) == c(nvar,nvar))){
  #  stop("The restriction matrix has incorrect dimensions. For a system with
  #       K variables the restrictions matrix should have KxK dimensions. In
  #       case you only want to partially identify a system you need to submit
  #       coumns of NA to the unidentified shocks.")
  # }

  # if(!all(dim(hor_matrix) == c(2,nvar))){
  #  stop("The horizon matrix has incorrect dimensions. For a system with K
  #       variables the horizon matrix should have 2xK dimensions. In case you
  #       only want to partially identify a system you need to submit columns
  #       of NA to the unidentified shocks.")
  # }

  # if (!length(unique(shock_names)) == nvar){
  #  stop("The vector of shock_names has incorrect dimensions. For a system
  #       with K variables the shock names should be a vector with at
  #       most K unique entries.
  #       In case you only want to partially identify a system you need to submit
  #       NA values to the unidentified shocks.")
  # }



  tradSign_setup <- list(
    restrictions = restr_matrix,
    horizons = hor_matrix,
    shocknames = shock_names,
    cum = cum
  )

  tradSign_setup
}
