#' Narrative restrictions accept and reweight
#'
#' Checks narrative restrictions and reweights them accordingly.
#'
#' Will in the long run change this such that this all comes from the model
#'
#' @param narrative_restr Narrative restriction as output from narrsign_setup
#' @param irfs A 4 dimensional array which contains IRFs for all sstructural shocks
#' @param shocks A 3 dimensional array of structural shocks
#' @param ndrawweights A numeric with the amount of draws for the fake shocks
#' @param data Data used to estimate the model
#' @param lag A numeric with lag used in the system
#' @param fevds 
#'
#' @return
#'

narrsign_accept <- function(narrative_restr,
                            irfs,
                            shocks,
                            ndrawweights,
                            data,
                            lag,
                            fevds) {
  Ns <- narrative_restr$narrative_sign
  Nc <- narrative_restr$narrative_contribution
  narrative_contributions <- Nc
  p <- lag
  Time <- nrow(data)

  # Need to check whetehr this also works withh more than contributional narrative
  nar <- arrayInd(which(Nc != 0), dim(Nc))
  startperiod <- nar[1]
  endperiod <- nar[2]
  shock <- nar[3]
  variable <- nar[4]

  findnarrativecontrib <- which(Nc != 0)



  # define matrix to capture which of the models is accepted
  accept <- rep(NA, dim(shocks)[1])
  weights <- matrix(0, dim(shocks)[1], 1)
  n <- ncol(data)
  ## sampling







  for (b in 1:dim(shocks)[1]) {
    shocks_temp <- shocks[b, , ]
    irfs_temp <- irfs[b, , , ]



    narrative_restrictions <- Ns[Ns != 0]

    if (!all(Ns == 0)) {
      check_narrative_sign <- as.numeric(sum(narrative_restrictions == sign(shocks_temp[Ns != 0])) == length(Ns[Ns != 0]))
    } else {
      check_narrative_sign <- 1
    }


    for (restriction in 1:length(findnarrativecontrib)) {
      Tstart <- startperiod[restriction]
      Tend <- endperiod[restriction]

      shocks_temp_temp <- shocks_temp[Tstart:Tend, , drop = FALSE]

      whichVariable <- variable[restriction]

      hmax <- dim(shocks_temp_temp)[1] - 1
      EYE <- diag(n)
      HD <- matrix(NaN, n, hmax + 1)

      for (h in 0:hmax) {
        for (j in 1:n) {
          # added transpose for first one
          HD[j, h + 1] <- t(EYE[, whichVariable]) %*% t(irfs_temp[h + 1, , ]) %*% EYE[, j] %*% t(EYE[, j]) %*% t(shocks_temp_temp[(nrow(shocks_temp_temp) - h), , drop = FALSE])
        }
      }

      HD <- matrix(rowSums(HD), n, 1)
      contributions_restr <- HD
      check_narrative_contrib_temp <- matrix(0, length(findnarrativecontrib), 1)

      if (narrative_contributions[findnarrativecontrib[restriction]] == 1) {
        check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) == max(abs(contributions_restr))
      } else if (narrative_contributions[findnarrativecontrib[restriction]] == -1) {
        check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) == min(abs(contributions_restr))
      } else if (narrative_contributions[findnarrativecontrib[restriction]] == 2) {
        check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) > sum(abs(contributions_restr[-shock]))
      } else {
        check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) < sum(abs(contributions_restr[-shock]))
      }



      check_narrative_contrib <- as.numeric(sum(check_narrative_contrib_temp) == length(findnarrativecontrib))

      check <- check_narrative_sign * check_narrative_contrib

      accept[b] <- check
    }


    if (check) {
      check_simulated <- matrix(NaN, ndrawweights, 1)
      for (rep in 1:ndrawweights) {
        fake_shocks <- matrix(stats::rnorm((Time - p) * n), Time - p, n)

        if (!all(Ns == 0)) {
          check_narrative_sign <- as.numeric(sum(narrative_restrictions == sign(fake_shocks[Ns != 0])) == length(Ns[Ns != 0]))
        } else {
          check_narrative_sign <- 1
        }

        # check_narrative_sign <- 1

        # check_narrative_sign_df[b] <- check_narrative_sign

        for (restriction in 1:length(findnarrativecontrib)) {
          Tstart <- startperiod[restriction]
          Tend <- endperiod[restriction]
          # contributions_restr <- getHDs_fast(irfs_temp,n,shocks_temp[Tstart:Tend,,drop=FALSE],variable[restriction])

          fake_shocks_temp_temp <- fake_shocks[Tstart:Tend, , drop = FALSE]

          # real oil price
          whichVariable <- variable[restriction]

          hmax <- dim(fake_shocks_temp_temp)[1] - 1
          EYE <- diag(n)
          HD <- matrix(NaN, n, hmax + 1)

          for (h in 0:hmax) {
            for (j in 1:n) {
              # added transpose for first one
              HD[j, h + 1] <- t(EYE[, whichVariable]) %*% t(irfs_temp[h + 1, , ]) %*% EYE[, j] %*% t(EYE[, j]) %*% t(fake_shocks_temp_temp[(nrow(fake_shocks_temp_temp) - h), , drop = FALSE])
            }
          }

          HD <- matrix(rowSums(HD), n, 1)
          contributions_restr <- HD
          check_narrative_contrib_temp <- matrix(0, length(findnarrativecontrib), 1)

          if (narrative_contributions[findnarrativecontrib[restriction]] == 1) {
            check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) == max(abs(contributions_restr))
          } else if (narrative_contributions[findnarrativecontrib[restriction]] == -1) {
            check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) == min(abs(contributions_restr))
          } else if (narrative_contributions[findnarrativecontrib[restriction]] == 2) {
            check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) > sum(abs(contributions_restr[-shock]))
          } else {
            check_narrative_contrib_temp[restriction] <- (abs(contributions_restr[shock[restriction]])) < sum(abs(contributions_restr[-shock]))
          }



          check_narrative_contrib <- as.numeric(sum(check_narrative_contrib_temp) == length(findnarrativecontrib))


          check <- check_narrative_sign * check_narrative_contrib



          check_simulated[rep] <- check
        }
      }
      weights[b] <- ndrawweights / (sum(check_simulated))
    }
  }

  irfs_nar <- irfs[which(accept != 0), , , ]
  shocks_nar <- shocks[which(accept != 0), , ]
  fevds_nar <- fevds[which(accept != 0), , , ]
  numSavedNarrative <- length(which(accept != 0))
  sample <- sample(numSavedNarrative, numSavedNarrative, TRUE, prob = 1 / weights[which(weights != 0)])
  irfs_nar_resampled <- irfs_nar[sample, , , ]
  shocks_nar_resampled <- shocks_nar[sample, , ]
  fevds_nar_resampled <- fevds_nar[sample, , , ]
  # fevd resampled still need to include this one here
  ret <- list(IRFS_narr = irfs_nar_resampled, SHOCKS_narr = shocks_nar_resampled, FEVDS_narr = fevds_nar_resampled)
  ret
}
