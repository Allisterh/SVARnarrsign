#' Plot structural shocks for all identified shocks
#'
#' @param narrSign_model A model object of class narrsign
#' @param prob Argument for quantile function 
#' @param whichShock A string with the name of shock to be plotted or a numeric
#' with the position of the shock within the system
#'
#' @return A ggplot object
#' @export
#'
#' @examples
two_shock_plot <- function(narrSign_model = NULL,
                           whichShock = NULL,
                           prob = 0.5) {
  dates <- narrSign_model$dates
  lag <- narrSign_model$lag


  # median shocks
  shocks <- narrSign_model$trad$SHOCKS[, , whichShock]
  shocks_med <- apply(shocks, 2, quantile, probs = c(prob))
  shocks_lower <- apply(shocks, 2, quantile, probs = 0.16)
  shocks_upper <- apply(shocks, 2, quantile, probs = 0.84)
  
  shocks_nar <- narrSign_model$narr$SHOCKS_narr[, , whichShock]
  shocks_nar_med <- apply(shocks_nar, 2, quantile, probs = c(prob))
  shocks_nar_lower <- apply(shocks_nar, 2, quantile, probs = 0.16)
  shocks_nar_upper <- apply(shocks_nar, 2, quantile, probs = 0.84)

  shocks <- data.frame(dates = dates[-c(1:lag), ], shocks = shocks_med, shocks_nar = shocks_nar_med,
                       shocks_nar_lower = shocks_nar_lower, shocks_nar_upper = shocks_nar_upper,
                       shocks_lower = shocks_lower, shocks_upper = shocks_upper)

  ggplot(shocks, aes(x = dates, y = shocks_med)) +
    geom_line() +
    ylab(paste("Shock:", narrSign_model$shocknames[whichShock])) +
    xlab("Time") +
    geom_line(aes(x = dates, y = shocks_nar_med), col = "#F8766D")
}
