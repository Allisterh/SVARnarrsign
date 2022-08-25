#' Plot IRFs
#'
#' Plots all IRFs for a specified shock
#'
#' @param narrSign_model A model object of class narrsign
#' @param whichShock A string with the name of shock to be plotted or a numeric indicating the number
#' @param cumulative A Boolean whether IRFs should be cumulative or not
#' @param narr A Boolean whether IRFs should be plotted which fulfill narrative or
#' only traditional restrictions
#'
#' @return A ggplot object
#' @export
#'
#' @examples
irf_plot <- function(narrSign_model = NULL,
                     whichShock = NULL,
                     cumulative = NULL,
                     narr = FALSE,
                     type = "median",
                     bands = c(0.16, 0.84),
                     scaling = 1,
                     varnames = NULL,
                     steps = dim(narrSign_model$trad$IRFS)[2]) {
  if (narr == TRUE) {
    mod <- narrSign_model$narr
  } else {
    mod <- narrSign_model$trad
  }

  if (is.null(varnames)) {
    varnames <- narrSign_model$varnames
  }

  IRFS <- mod$IRFS[,1:steps , whichShock, ]
  irfdraws <- IRFS

  goodresp <- IRFS
  irftype <- type #  0== median, 1== mean response
  # gridgrph <- grid # grid in irf plots 0== none, 1== adds grid to plots
  bndtest <- is.null(bands)
  if (bndtest != TRUE) {
    ebupp <- bands[2] # error bands for irf plots
    eblow <- bands[1]
  } else {
    ebupp <- 0.84 # error bands for irf plots
    eblow <- 0.16
  }
  varlbl <- labels
  nstep <- dim(irfdraws)[2]
  nvar <- dim(irfdraws)[3]

  if (!is.null(cumulative)) {
    # for every shock
    # for all variables where we need to cumsum
    for (b in 1:length(cumulative)) {
      # for every draw
      for (j in 1:dim(goodresp)[1]) {
        # for the entire horizon
        goodresp[j, , cumulative[b]] <- cumsum(goodresp[j, , cumulative[b]])
      }
    }
  }

  goodresp <- scaling * goodresp

  if (irftype == "mean") {
    imp_responses <- array(NA, dim = c(3, nstep, nvar))
    irfbands <- apply(goodresp, c(2, 3), quantile, probs = c(eblow, ebupp), na.rm = TRUE)
    irfmean <- array(apply(goodresp, c(2, 3), mean), dim = c(1, nstep, nvar))
    dimnames(imp_responses) <- list(c("irf", "lower", "upper"), 1:nstep, varlbl)
    imp_responses[1, , ] <- irfmean
    imp_responses[2:3, , ] <- irfbands
    dimnames(imp_responses) <- list(c("irf", "lower", "upper"), 1:nstep, varlbl)
  } else {
    imp_responses <- apply(goodresp, c(2, 3), quantile, probs = c(0.5, eblow, ebupp), na.rm = TRUE)
    # dimnames(imp_responses) <- list(c("irf", "lower", "upper"),1:nstep, varlbl)
  }
  impt <- imp_responses
  impt <- aperm(impt, c(3, 2, 1))

  plot_list <- list()

  for (i in 1:nvar) {
    plot_list[[i]] <- local({
      i <- i
      ggplot(data = data.frame(horizon = 1:dim(impt)[2], median = impt[i, , 1]), aes(x = horizon, y = median)) +
        geom_ribbon(aes(ymin = impt[i, , 2], ymax = impt[i, , 3]), alpha = 0.4, fill = "#F8766D") +
        geom_line(col = "#F8766D", size = 1.5) +
        ylab(varnames[i]) +
        geom_line(aes(x = horizon, y = rep(0, length(horizon))))
    })
  }

  gridExtra::grid.arrange(grobs = plot_list, top = textGrob(paste("Shock:", narrSign_model$shocknames[whichShock])), gp = grid::gpar(fontsize = 20, font = 3))
}
