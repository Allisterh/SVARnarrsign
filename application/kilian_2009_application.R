# Applifcation of Kilian (2009)

data("kilian_2009")

# set up traditional sign restrictions
shock_names_kilian_2009 <- c("Oil Supply","Aggregate Demand","Oil specific Demand")

restr_matrix_kilian_2009 <- matrix(NA,3,3)
restr_matrix_kilian_2009[1,] <- c(-1,-2,3)
restr_matrix_kilian_2009[2,] <- c(1,2,3)
restr_matrix_kilian_2009[3,] <- c(1,-2,3)


hor_matrix_kilian_2009 <- matrix(NA,3,2)
hor_matrix_kilian_2009[1,] <- c(1,1)
hor_matrix_kilian_2009[2,] <- c(1,1)
hor_matrix_kilian_2009[3,] <- c(1,1)


kilian_2009_tradsign <- tradsign_setup(shock_names = shock_names_kilian_2009,
                                       restr_matrix = restr_matrix_kilian_2009,
                                       hor_matrix = hor_matrix_kilian_2009, 
                                       cum = rep(0,3))

## elasticity bound restrictions
varnames_kilian_2009 <- colnames(kilian_2009)

#EBR_kilian <- data.frame(shock =c(2,3),variableone = c(1,1), variabletwo = c(3,3),horizon = c(1,1),bound=c(0.0258,0.0258))

ebr_kilian_2009_shocknames <- c("Aggregate Demand","Oil specific Demand")
ebr_kilian_2009_first <- c(varnames_kilian_2009[2],varnames_kilian_2009[2])
ebr_kilian_2009_second <- c(varnames_kilian_2009[3],varnames_kilian_2009[3])

ebr_kilian_2009_first <- c(1,1)
ebr_kilian_2009_second <- c(3,3)

ebr_kilian_2009_horizon <- c(1,1)
ebr_kilian_2009_maxbounds <- c(0.0258,0.0258)


ebr_kilian_2009 <- ebr_setup(shock_names = ebr_kilian_2009_shocknames,
                             first_variable = ebr_kilian_2009_first,
                             second_variable = ebr_kilian_2009_second,
                             horizon = ebr_kilian_2009_horizon,
                             bounds = ebr_kilian_2009_maxbounds)

# set up narrative sign restrictions
allshocknames <- c("Oil supply", "Aggregate Demand", "Oil specific demand")
shock_names <- c("Aggregate Demand")
shock_type <- c("contribution")
shock_dates <- matrix(c("1990-08-01","1990-08-01"),1,2)
dates <- kilian_2009[,1]
relevant_variable <- varnames_kilian_2009[4]
shock_sign <- c(-1)
shock_size <- c("strong")
lag <- 24
data <- kilian_2009[,-1]





kilian_narr_rest <- narrsign_setup(allshocknames = allshocknames,
                                   shock_names = shock_names,
                                   shock_type = shock_type,
                                   shock_dates = shock_dates,
                                   dates = dates,
                                   relevant_variable = relevant_variable,
                                   shock_sign =shock_sign,
                                   shock_size = shock_size,
                                   lag = lag,
                                   data = data)


kilian_m_2009 <- narrsign(data=ts(kilian_2009[,-1]),
                          lags = 24,
                          trad_signs = kilian_2009_tradsign,
                          narr_restr = kilian_narr_rest,
                          nkeep = 1000,
                          draws = 10000,
                          subdraws = 3000,
                          elasticitybounds = ebr_kilian_2009,
                          const = TRUE,
                          steps = 30)


two_irf_plot(kilian_m_2009,3,cumulative = 1)

dim(kilian_m_2009$narr$IRFS_narr)
