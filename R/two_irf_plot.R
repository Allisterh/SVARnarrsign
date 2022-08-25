#' Plot IRFs for traditional and narrative restrictions
#'
#' @param narrSign_model A model of class narrsign
#' @param whichShock A numeric indicating which shock to plot. Based on the order in
#' narrSign_model
#' @param cumulative A Boolean whether cumulative IRFs should be plotted
#'
#' @import ggplot2 grid
#'
#' @return A ggplot object
#' @export
#'
#' @examples
two_irf_plot <- function(narrSign_model = NULL,
                         whichShock = NULL,
                         cumulative = NULL,
                         scaling_trad = 1,
                         scaling_nar = 1,
                         varnames = NULL,
                         type = "median",
                         bands = c(0.16, 0.84),
                         steps = dim(narrSign_model$trad$IRFS)[2] ) {
  if (is.null(varnames)) {
    varnames <- narrSign_model$varnames
  }

  IRFS <- narrSign_model$trad$IRFS[,1:steps , whichShock, ]
  IRFS_nar <- narrSign_model$narr$IRFS_narr[,1:steps , whichShock, ]
  irfdraws <- IRFS
  irfdraws_nar <- IRFS_nar

  # traditional IRFS
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

  goodresp <- scaling_trad * goodresp

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


  # narrative IRFS
  goodresp <- IRFS_nar
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
  nstep <- dim(irfdraws_nar)[2]
  nvar <- dim(irfdraws_nar)[3]

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

  goodresp <- scaling_nar * goodresp

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
  impt_nar <- imp_responses
  impt_nar <- aperm(impt_nar, c(3, 2, 1))



  plot_list <- list()

  for (i in 1:nvar) {
    plot_list[[i]] <- local({
      i <- i
      ggplot(data = data.frame(horizon = 1:dim(impt)[2], median = impt[i, , 1]), aes(x = horizon, y = median)) +
        geom_ribbon(aes(ymin = impt[i, , 2], ymax = impt[i, , 3]), alpha = 0.4, fill = "#00BFC4") +
        geom_line(col = "#00BFC4", size = 1.5) +
        ylab(varnames[i]) +
        geom_line(aes(x = horizon, y = rep(0, length(horizon)))) +
        geom_ribbon(aes(ymin = impt_nar[i, , 2], ymax = impt_nar[i, , 3]), alpha = 0.8, fill = "#F8766D") +
        geom_line(aes(x = horizon, y = impt_nar[i, , 1]), col = "#F8766D", size = 1.5)
        
    })
  }

  gridExtra::grid.arrange(grobs = plot_list, top = textGrob(paste("Shock:", narrSign_model$shocknames[whichShock])), gp = grid::gpar(fontsize = 20, font = 3))
}
