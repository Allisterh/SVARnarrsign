#' Setup narrative sign restrictions
#'
#' This functions takes the user input about narrative sign restrictions and
#' sets up the arrays necessary for evaluation. Two types of restrictions are
#' possible and should be noted carefully. One can either place a restriction
#' on the sign of a specific shock or on the historic contribution. See package
#' documention and the examples which should be self explanatory.
#'
#' @param shock_names A vector of shock names on which narrative restrictions
#' are placed
#' @param shock_type A vector of type of shocks which corresponds to the name
#' of shocks
#' @param shock_dates A matrix of dimensions 2 x (length of shock_names) which
#' contains start and end date of narrative restriction
#' @param dates A vector of dates based on the original data set
#' @param relevant_variable An integer or string
#'  the name of variable on which the narrative restriction is placed
#' @param shock_size A string which further specifies the kind of shock
#' @param allshocknames A vector containing all named shocks in the system
#' @param shock_sign Either 1 or -1 depdning on the sign of the shock
#' @param lag A numeric for the amount of lags in the system
#' @param data Data used for estimation. Note: Without the dates!
#'
#' @return A list of arrays which contains the narrative restrictions
#' @export
#'
#' @examples
narrsign_setup <- function(allshocknames = NULL,
                           shock_names = NULL,
                           shock_type = NULL,
                           shock_dates = NULL,
                           dates = NULL,
                           relevant_variable = NULL,
                           shock_sign = NULL,
                           shock_size = NULL,
                           lag = NULL,
                           data = NULL) {
  varnames <- colnames(data)


  Ns <- matrix(0, dim(data)[1], dim(data)[2])
  Nc <- array(0, dim = c(dim(data)[1], dim(data)[1], dim(data)[2], dim(data)[2]))


  for (shock in 1:length(shock_names)) {
    if (shock_type[shock] == "sign") {
      Ns[which(dates == shock_dates[shock, 1]), which(allshocknames == shock_names[shock])] <- as.numeric(shock_sign[shock])
    } else {
      if (shock_size[shock] == "strong") {
        if (shock_sign[shock] == 1) {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- 2
        } else {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- -1
        }
      } else {
        if (sign(NSR[[rest]][[6]] == 1)) {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- 1
        } else {
          Nc[which(dates == shock_dates[shock, 1]), which(dates == shock_dates[shock, 2]), which(allshocknames == shock_names[shock]), which(varnames == relevant_variable[shock])] <- -2
        }
      }
    }
  }

  Ns <- Ns[(lag + 1):nrow(Ns), ]
  Nc <- Nc[(lag + 1):dim(Nc)[1], (lag + 1):dim(Nc)[2], , ]

  narrative_restr <- list(narrative_sign = Ns, narrative_contribution = Nc, dates = dates)

  narrative_restr
}
