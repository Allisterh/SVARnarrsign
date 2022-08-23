# Application of Uhlig (2005) - Monetary policy shock

# load data
data("uhlig")
# set up traditional sign restrictions
shock_names_uhlig <- c("Monetary Policy")

restr_matrix_uhlig <- matrix(NA,6,6)
restr_matrix_uhlig[1,] <- c(NA,-2,-3,NA,-5,6)
hor_matrix_uhlig <- matrix(NA,6,2)
hor_matrix_uhlig[1,] <- c(1,6)

uhlig_tradsign <- tradsign_setup(shock_names = shock_names_uhlig,
                                 restr_matrix = restr_matrix_uhlig,
                                 hor_matrix = hor_matrix_uhlig,
                                 cum = rep(0,6))
# set up narrative sign restrictions
# uhlig 
allshocknames <- c("MP")
shock_names <- c("MP","MP")
shock_type <- c("sign","contribution")
shock_dates <- matrix(c("1979-10-01","1979-10-01","1979-10-01","1979-10-01"),2,2)
dates <- uhlig[,1]
relevant_variable <- c(NA,varnames_uhlig[7])
# need to explain how to select shock_sign variable
shock_sign <- c(1,1)
# strong or weak
shock_size <- c(NA,"strong")
lag <- 12
data <- uhlig[,-1]

uhlig_narr_rest <- narrsign_setup(allshocknames = allshocknames,
                                  shock_names = shock_names,
                                  shock_type = shock_type,
                                  shock_dates = shock_dates,
                                  dates = dates,
                                  relevant_variable = relevant_variable,
                                  shock_sign =shock_sign,
                                  shock_size = shock_size,
                                  lag = lag,
                                  data = data)

# estimate the model with traditional and narrative sign restrictions
uhlig_m <- narrsign(data=ts(uhlig[,-1]),
                    lags = 12,
                    trad_signs = uhlig_tradsign,
                    narr_restr = uhlig_narr_rest,
                    nkeep = 1000,
                    draws = 300,
                    subdraws = 300,
                    elasticitybounds = NULL,
                    const = FALSE,
                    steps = 30)

# plot the irfs for monetary policy shock. Notice the scaling component

scaling_trad <- 0.25/median(uhlig_m$trad$IRFS[,1,1,6],na.rm = TRUE)
scaling_narr <- 0.25/median(uhlig_m$narr$IRFS[,1,1,6],na.rm = TRUE)

two_irf_plot(uhlig_m,whichShock = 1,scaling_trad = scaling_trad,scaling_nar = scaling_narr,
             varnames = c("Output","GDP Deflator","Commodity Prices","Total Reserves","Non-Borrowed Reserves",
                          "Federal Funds"))

# plot of FEVDS

two_fevd_plot(uhlig_m,whichShock = 1, varnames = c("Output","GDP Deflator","Commodity Prices","Total Reserves","Non-Borrowed Reserves",
                           "Federal Funds"))


# plot distribution of shock around volcker reform for narrative and traditional

hist(uhlig_m$narr$SHOCKS[,which(uhlig$dates=="1979-10-01")-12,1])
hist(uhlig_m$trad$SHOCKS[,which(uhlig$dates=="1979-10-01")-12,1])




