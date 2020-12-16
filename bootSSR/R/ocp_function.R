#' Sample size recalculation
#'
#' @name ssr_method
#'
#' @description Conducts sample size recalculation according to different methods, namely
#' the observed conditional power approach (\code{ocp}), the restricted observed conditional
#' power approach (\code{rocp}), the promising zone approach by Mehta and Pocock (2011) (\code{pz})
#' and the optimization function approach by Jennison and Turnbull (2015) (\code{of}).
#'
#' @aliases rocp_design pz_design of_design
#'
#' @param control Numeric vector. Standardized data of the control group.
#' @param treatment Numeric vector. Standardized data of the treatment group.
#' @param design An object of class \code{TwoStageDesign} (see function \code{design}).
#' @param delta Numeric. Assumed standardized treatment effect.
#' @param bootstrap Character. Specifies if bootstrapping shall be conducted
#' (default: no bootstrap (\code{NULL})) and if the test statistic (\code{"t"}) or the
#' sample size (\code{"n"}) shall be bootstrapped.
#' @param B Integer. If \code{bootstrap = "t"} or \code{bootstrap = "n"},
#' specify the number of bootstrap samples.
#' @param beta_tilde Numeric. The lower bound for the promising zone (range [0, 1]).
#' @param beta_0 Numeric. The lower bound for the restricted observed conditional power approach (range[0, 1]).
#' @param gamma Numeric. The penalty parameter for the optimization function of Jennison and Turnbull (2015).
#'
#' @details The function calculates the new total sample size per group in a two-stage design
#' (see function \code{design}) according to the different methods. In the observed conditional power
#' approach (\code{ocp}), the function chooses \code{n2} such that the observed conditional power
#' for the second stage is at least \code{1 - beta} if the realized (bootstrapped) test statistic
#' lies in the recalculation area (defined by \code{alpha_0} and \code{alpha_1}) and if the maximum
#' sample size is not exceeded (\code{b*n1}).
#'
#' In the restricted observed power approach (\code{rocp}), the function chooses \code{n2} such
#' that the observed conditional power for the second stage is at least \code{1 - beta} if the
#' realized (bootstrapped) test statistic lies in the recalculation area (defined by \code{alpha_0}
#' and \code{alpha_1}) and if the maximum sample size is not exceeded (\code{b*n1}). If the maximum size is
#' exceeded in the recalculation area, the function uses only a recalculated \code{n2} if the
#' conditional power is at least \code{1 - beta_0}.
#'
#' In the promising zone approach by Mehta and Pocock (2011) (\code{pz}), the function chooses
#' \code{n2} such that the conditional power is at least \code{1 - beta} if the realized
#' (bootstrapped) test statistic lies in the recalculation area (defined by \code{alpha_0} and
#' \code{alpha_1}) and if the maximum sample size is not exceeded (\code{b*n1}). Additionally, the conditional
#' power, calculated by the initial sample size (\code{n1 + n2}), must lie in the promising zone which
#' has the lower bound \code{1 - beta_tilde}.
#'
#' In the optimization function approach by Jennison and Turnbull (2015) (\code{of}), a penalty
#' function is maximized over \code{n2} if the realized (bootstrapped) test statistic lies in the
#' recalculation area (defined by \code{alpha_0} and \code{alpha_1}) and if the maximum
#' sample size is not exceeded (\code{b*n1}). Also the conditional power calculated by the initial sample size
#' (\code{n1 + n2}) must lie in the promising zone bounded by \code{1 - beta_tilde}.
#'
#' @return The function returns an object of class \code{"ssr"}, which is a list with the following components:
#' \itemize{\code{n}} {The total sample size per group according the approach.}
#' \itemize{\code{n1}} {First stage sample size per group (as defined in \code{design}).}
#' \itemize{\code{n2}} {Recalculated second stage sample size per group according to the method.}
#' \itemize{\code{delta_obs}} {Observed standardized treatment effect according to the given data.}
#' \itemize{\code{t1}} {Realized test statistic (without bootstrapping) at the interim analysis according to the given data.}
#' \itemize{\code{delta_ass}} {Assumed standardized treatment effect (as given to the function).}
#' \itemize{\code{condPow}} {The observed conditional power.}
#' \itemize{\code{condPow_true}} {The "true" conditional power according to the initially
#' assumed treatment effect (\code{delta}).}
#' \itemize{\code{method}} {Method that was used for the recalculation.}
#'
#' @export
#'
#' @examples # Simulate data
#' control <- rnorm(50)
#' treatment <- rnorm(50, .3)
#'
#' # Specify design
#' design <- design(50, .05, 50, .5, .2, 4)
#'
#' # Sample size recalculation
#' ocp <- ocp_design(control, treatment, design, .4, bootstrap = "n")
#' rocp <- rocp_design(control, treatment, design, .4, bootstrap = "n")
#' pz <- pz_design(control, treatment, design, .4, bootstrap = "n")
#' of <- of_design(control, treatment, design, .4, bootstrap = "n")
#' ocp_rocp <- ocp_rocp_design(control, treatment, design, .4, bootstrap = "n")

ocp_design <- function(control, treatment, design, delta,
                       bootstrap = NULL, B = 100) {

  out <- ssrFrame(control, treatment, design, delta, bootstrap, B,
                  ssrMethod = "ocp")

  out

}
