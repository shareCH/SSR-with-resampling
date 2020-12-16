#------------------------------------
# Conditional power
#------------------------------------

conditionalPower <- function(t1, n, n1, alpha_1, w1, w2){
  if(n == n1){  # no increment in sample size
    x <- 0
  } else{
    x <- 1 - pnorm(qnorm(1 - alpha_1) * sqrt(w1^2 + w2^2)/w2 -
                     t1 * (w1/w2 + sqrt((n - n1)/n1)))
  }

  x
}



#-------------------------------------
# True conditional power
#-------------------------------------

conditionalPower_true <- function(delta, t1, n, n1, alpha_1, w1, w2) {
  if(n == n1){  # no increment in sample size
    x <- 0
  } else{
    x <- 1 - pnorm(qnorm(1 - alpha_1) * sqrt(w1^2 + w2^2)/w2 - t1 * w1/w2 -
                     delta * sqrt((n - n1)/2))
  }

  x
}



#-----------------------
# Test statistic
#-----------------------

getTestStatistic <- function(control, treatment, design) {

  delta_obs <- mean(treatment) - mean(control)  # observed treatment effect
  t1 <- delta_obs * sqrt(design$n1/2)  # observed t-statistic

  t1

}



#------------------------
# Bootstrap functions
#------------------------

boot <- function(B, control, treatment, n1, alpha_0, alpha_1) {

  delta_boot <- replicate(B, mean(sample(treatment, n1, replace = TRUE)) -
                            mean(sample(control, n1, replace = TRUE)))  # bootstrapped treatment effect

  t1_boot <- purrr::map_dbl(delta_boot, ~ . * sqrt(n1/2))  # bootstrapped test statistic

  t1_boot <- t1_boot[(t1_boot > qnorm(1 - alpha_0)) &
                       (t1_boot <= qnorm(1 - alpha_1))]  # cut the recalculation area

  if (length(t1_boot) == 0){  # no t1 in recalculation area
    t1_boot <- qnorm(1 - alpha_0)
  }

  t1 <- median(t1_boot)

  # Instead of median, use the mean:
  #t1 <- mean(t1_boot)

  t1
}


boot_n_preparation <- function(B, control, treatment, design, w1, w2, nmax, n_ini,
                               beta_0, beta_tilde, gamma, ssrMethod) {

  t1_boot <- purrr::map_dbl(1:B, ~ getTestStatistic(sample(control, design$n1, replace = TRUE),
                                                    sample(treatment, design$n1, replace = TRUE),
                                                    design))
  t1_boot
}

boot_n <- function(t1, t1_boot, design, w1, w2, nmax, n_ini, beta_0, beta_tilde, gamma,
                   ssrMethod) {

  # If recalculation area shall be excluded:
  # if(t1_boot >= qnorm(1 - design$alpha_0) &
  #    t1_boot < qnorm(1 - design$alpha_1)) {
  #   n <- recalculateSampleSize(ssrMethod, design, t1_boot, w1, w2,
  #                              nmax, n_ini, beta_0, beta_tilde, gamma)
  #
  # } else {
  #   n <- NULL
  # }

  n <- purrr::map_dbl(t1_boot, ~ recalculateSampleSize(ssrMethod, design, t1,
                                                       w1, w2, nmax, n_ini, beta_0,
                                                       beta_tilde, gamma, .))

  n <- mean(n)
  #n <- mean(n) + sd(n)

  n <- ceiling(n)

  n[n > nmax] <- nmax

  n
}


#-------------------------------------------------------
# Optimization function (Jennison and Turnbull (2015))
#-------------------------------------------------------

f <- function(n, t1, n1, alpha_1, w1, w2, gamma = .005/4, n_ini){
  x <- conditionalPower(t1, n, n1, alpha_1, w1, w2) - gamma * (n - n_ini)
  unlist(x)
}



#-----------------------------
# Recalculate sample size
#-----------------------------

recalculateSampleSize <- function(ssrMethod, design, t1, w1, w2,
                                  nmax, n_ini, beta_0, beta_tilde, gamma, t1_boot = t1) {
  if(ssrMethod == "ocp"){
    n <- recalculateSampleSize_ocp(design, t1, w1, w2, nmax, t1_boot)
  } else if(ssrMethod == "rocp") {
    n <- recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0, t1_boot)
  } else if(ssrMethod == "pz") {
    n <- recalculateSampleSize_pz(design, t1, w1, w2, nmax, n_ini, beta_tilde, t1_boot)
  } else if(ssrMethod == "of") {
    n <- recalculateSampleSize_of(design, t1, w1, w2, nmax, n_ini, beta_tilde, gamma)
  }
  n
}
