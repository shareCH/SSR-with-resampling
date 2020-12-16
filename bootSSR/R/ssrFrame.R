ssrFrame <- function(control, treatment, design, delta, bootstrap, B,
                     ssrMethod, beta_0, beta_tilde, gamma) {

  w1 <- sqrt(design$n1)
  w2 <- sqrt(design$n2)  # weights for final test statistic

  nmax <- design$b * design$n1  # maximum sample size
  n_ini <- design$n1 + design$n2  # initial total sample size

  t1 <- getTestStatistic(control, treatment, design)

  if(is.null(bootstrap)) {
    bootstrap <- "no"
  }

  if(t1 >= qnorm(1 - design$alpha_0) &
     t1 < qnorm(1 - design$alpha_1)) {  # in recalculation area

    if(bootstrap == "t") {
      t1_boot <- boot(B, control, treatment, design$n1, design$alpha_0, design$alpha_1)

      n <- recalculateSampleSize(ssrMethod, design, t1_boot, w1, w2,
                                 nmax, n_ini, beta_0, beta_tilde, gamma)
    } else if(bootstrap == "n") {
      t1_boot <- boot_n_preparation(B, control, treatment, design, w1, w2, nmax, n_ini,
                                    beta_0, beta_tilde, gamma, ssrMethod)
      n <- boot_n(t1, t1_boot, design, w1, w2, nmax, n_ini, beta_0, beta_tilde, gamma,
                  ssrMethod)
    } else {
      n <- recalculateSampleSize(ssrMethod, design, t1, w1, w2,
                                 nmax, n_ini, beta_0, beta_tilde, gamma)
    }

  } else{
    n <- recalculateSampleSize(ssrMethod, design, t1, w1, w2,
                               nmax, n_ini, beta_0, beta_tilde, gamma)
  }

  conditionalPower <- conditionalPower(t1, n, design$n1, design$alpha_1, w1, w2)
  conditionalPower_true <- conditionalPower_true(delta, t1, n, design$n1, design$alpha_1, w1, w2)

  out <- list(
    "n" = n,
    "n1" = design$n1,
    "n2" = n - design$n1,
    "delta_obs" = mean(treatment) - mean(control),
    "t1" = t1,
    "delta_ass" = delta,
    "condPow" = conditionalPower,
    "condPow_true" = conditionalPower_true,
    "method" = ssrMethod
  )

  class(out) <- "ssr"

  out
}
