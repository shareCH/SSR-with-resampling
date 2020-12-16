#-------------------------------------
# Observed conditional power approach
#-------------------------------------

recalculateSampleSize_ocp <- function(design, t1, w1, w2, nmax, t1_boot) {
  n <- design$n1 * (1 + ((qnorm(design$beta) - qnorm(1 - design$alpha_1) *
                            sqrt(w1^2 + w2^2)/w2)/t1_boot + w1/w2)^2)

  n[t1_boot < qnorm(1 - design$alpha_0) | t1_boot >= qnorm(1 - design$alpha_1)] <- design$n1  # not in recalculation area

  n[n > nmax] <- nmax

  n
}


#------------------------------------------------
# Restricted observed conditional power approach
#------------------------------------------------

recalculateSampleSize_rocp <- function(design, t1, w1, w2, nmax, beta_0, t1_boot) {
  n <- design$n1 * (1 + ((qnorm(design$beta) - qnorm(1 - design$alpha_1) *
                            sqrt(w1^2 + w2^2)/w2)/t1_boot + w1/w2)^2)

  n[t1_boot < qnorm(1 - design$alpha_0) | t1_boot >= qnorm(1 - design$alpha_1)] <- design$n1  # not in recalculation area

  n[n > nmax] <- nmax
  n[conditionalPower(t1_boot, n, design$n1, design$alpha_1, w1, w2) < 1 - beta_0] <- design$n1  # restriction

  n
}


#-------------------------------
# Promising Zone approach
#-------------------------------

recalculateSampleSize_pz <- function(design, t1, w1, w2, nmax, n_ini, beta_tilde, t1_boot) {
  n <- design$n1 * (1 + ((qnorm(design$beta) - qnorm(1 - design$alpha_1) *
                            sqrt(w1^2 + w2^2)/w2)/t1_boot + w1/w2)^2)

  n[t1 < qnorm(1 - design$alpha_0) | t1 >= qnorm(1 - design$alpha_1)] <- design$n1  # not in recalculation area

  n[(conditionalPower(t1_boot, n_ini, design$n1, design$alpha_1, w1, w2) < 1 - beta_tilde |
       conditionalPower(t1_boot, n_ini, design$n1, design$alpha_1, w1, w2) >= 1 - design$beta) &
      qnorm(1 - design$alpha_0) <= t1 &
      t1_boot < qnorm(1 - design$alpha_1)] <- n_ini  # not in promising zone

  n[n > nmax] <- nmax

  n
}


#--------------------------------
# Optimization function approach
#--------------------------------

recalculateSampleSize_of <- function(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma) {
  n <- optimize(f, interval = c(design$n1, nmax), t1 = t1, n1 = design$n1,
                alpha_1 = design$alpha_1, w1 = w1, w2 = w2,
                gamma = gamma, n_ini = n_ini,
                maximum = TRUE, tol = 1)$maximum
  n[t1 < qnorm(1 - design$alpha_0) | t1 >= qnorm(1 - design$alpha_1)] <- design$n1  # not in recalculation area

  n[(conditionalPower(t1, n_ini, design$n1, design$alpha_1, w1, w2) < 1 - beta_tilde |
       conditionalPower(t1, n_ini, design$n1, design$alpha_1, w1, w2) >= 1 - design$beta) &
      qnorm(1 - design$alpha_0) <= t1 &
      t1 < qnorm(1 - design$alpha_1)] <- n_ini  # not in promising zone

  n[n > nmax] <- nmax

  n
}
