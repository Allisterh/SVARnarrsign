#' Plot structural shocks for all identified shocks
#'
#' @param narrSign_model A model object of class narrsign
#' @param whichShock A string with the name of shock to be plotted
#' @param narr A Boolean whether shocks identified by narrative restrictions
#' should be used. If False only traditional
#' 
#' @return A ggplot object
#' @export
#'
#' @examples
two_shock_plot <- function( narrSign_model = NULL,
                        whichShock = NULL,
                        prob = 0.5
){

  
  dates <- narrSign_model$dates
  lag <- narrSign_model$lag
  
  
  # median shocks
  shocks <- narrSign_model$trad$SHOCKS[,,whichShock]
  shocks <- apply(shocks,2,quantile,probs=c(prob))
  
  shocks_nar <- narrSign_model$narr$SHOCKS_narr[,,whichShock]
  shocks_nar <- apply(shocks_nar,2,quantile,probs=c(prob))
  
  shocks <- data.frame(dates = dates[-c(1:lag),], shocks = shocks, shocks_nar = shocks_nar)
  
  ggplot(shocks, aes(x=dates,y=shocks)) + geom_line() + ylab(paste("Shock:",narrSign_model$shocknames[whichShock])) + xlab("Time")+
    geom_line(aes(x=dates,y=shocks_nar),col="#F8766D")
  
}
