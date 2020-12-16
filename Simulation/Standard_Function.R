stand_recalc <- function(t1, design, ssrMethod, beta_0, beta_tilde) {
  
  w1 <- sqrt(design$n1)
  w2 <- sqrt(design$n2)
  nmax <- design$b * design$n1
  n_ini <- design$n1 + design$n2
  
  if(t1 < qnorm(1 - design$alpha_0) | t1 >= qnorm(1 - design$alpha_1)) {  # not in recalculation area
    n <- design$n1
  } else {
    
    if(ssrMethod == "ocp") {
      n <- purrr::map_dbl(t1,
                          ~ bootSSR::recalculateSampleSize_ocp(design, t1, w1, w2, nmax, .))
    } else if(ssrMethod == "rocp") {
      n <- purrr::map_dbl(t1,
                          ~ bootSSR::recalculateSampleSize_rocp(design, t1, w1, w2, nmax, beta_0, .))
    } else if(ssrMethod == "pz") {
      n <- purrr::map_dbl(t1,
                          ~ bootSSR::recalculateSampleSize_pz(design, t1, w1, w2, nmax,
                                                              n_ini, beta_tilde, .))
    } else if(ssrMethod == "gsd") {
      n <- design$n1 + design$n2
    }
  }
  
  if(n > nmax) {
    n <- nmax
  }
  
  cp <- bootSSR::conditionalPower(t1, n, design$n1, design$alpha_1, w1, w2)
  
  out <- list()
  out$n <- n
  out$condPow <- cp
  
  out
}
