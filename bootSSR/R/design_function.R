#' Design Function for sample size recalculation
#'
#' @description Specifies the design of an adaptive two-stage clinical trial.
#' Provides a frame to conduct sample size recalculation.
#'
#' @param n1 An integer. Interim sample size per group at stage one.
#' @param alpha_glob Numeric. Global significance level (range [0, 1]).
#' @param n2 An integer. Initial incremental sample size per group for stage two.
#' @param alpha_0 Numeric. Futility bound for stage one (range [0, 1]).
#' @param beta Numeric. Desired type II error (range [0, 1]).
#' @param b Numeric. Factor for maximal sample size (nmax = b * n1), b > 1.
#'
#' @details This function is necessary for the recalculation functions of this package
#' (\code{ocp_design, rocp_design, pz_design, opt_design}). It calculates
#' the local significance level according to Pocock (1977) and provides the frame that
#' each recalculation function of \code{bootSSR} needs.
#'
#' @return The function returns an object of class \code{"TwoStageDesign"}, which is a list with
#' the following components:
#' \itemize{n1} The interim size per group at stage one (as given to the function).
#' \itemize{alpha_1} The local efficacy bound for a two stage design according to Pocock.
#' \itemize{n2} The initial incremental sample size per group for stage two (as given to the function).
#' \itemize{alpha_0} The futility bound for stage one (as given to the function).
#' \itemize{b} The factor for maximal sample size (as given to the function).
#' \itemize{alpha_glob} The global significance level (as given to the function).
#' \itemize{beta} The desired type II error (as given to the function).
#'
#' @export
#'
#' @examples design <- design(50, .05, 50, .5, .2, 4)


design <- function(n1, alpha_glob, n2, alpha_0, beta, b){

  #--------------------------------------------------------------
  # Specify correlation between interim and final test statistic
  #--------------------------------------------------------------

  w1 <- sqrt(n1)
  w2 <- sqrt(n2)
  r <- matrix(c(1, w1/sqrt(w1^2 + w2^2), w1/sqrt(w1^2 + w2^2), 1), ncol = 2)


  #--------------------------------------------------------------
  # Calculate local significance levels according to Pocock
  #--------------------------------------------------------------

  glob <- 1
  alpha_loc <- alpha_glob/2
  while(glob > (1 - alpha_glob)){
    alpha_loc <- alpha_loc + 0.00001
    glob <- mvtnorm::pmvnorm(lower = c(-Inf, -Inf),
                             upper = c(qnorm(1 - alpha_loc), qnorm(1 - alpha_loc)),
                             mean = c(0, 0), sigma = r)
    glob <- glob[1]
  }
  alpha_loc <- alpha_loc - 0.00001


  #-----------------------------------
  # Output
  #-----------------------------------

  out <- list(
    "n1" = n1,
    "alpha_1" = alpha_loc,
    "n2" = n2,
    "alpha_0" = alpha_0,
    "b" = b,
    "alpha_glob" = alpha_glob,
    "beta" = beta
  )
  class(out) <- "TwoStageDesign"
  return(out)
}
