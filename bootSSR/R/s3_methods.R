#' @export
print.ssr <- function(x, ...) {
  if(x$method == "ocp") {
    method <- "observed conditional power"
  } else if(x$method == "rocp") {
    method <- "restricted observed conditional power"
  } else if(x$method == "pz") {
    method <- "promising zone"
  } else if(x$method == "of") {
    method <- "optimization function"
  } else if(x$method == "ocp_rocp") {
    method <- "observed conditional power with restriction"
  }

  cat("Sample size recalculation with the", method, "approach.\n\n")
  cat("Recalculated total sample size:", x$n, "\n")
  cat("Conditional power:", x$condPow, "\n")
  cat("Conditional power under Delta =", x$delta_ass, ":", x$condPow_true)
}

#--------------------------------------------------------------------------------------


#' @export
summary.ssr <- function(object, ...) {
  if(object$method == "ocp") {
    method <- "observed conditional power"
  } else if(object$method == "rocp") {
    method <- "restricted observed conditional power"
  } else if(object$method == "pz") {
    method <- "promising zone"
  } else if(object$method == "of") {
    method <- "optimization function"
  } else if(x$method == "ocp_rocp") {
    method <- "observed conditional power with restriction"
  }

  cat("Sample size recalculation with the", method, "approach.\n\n")
  cat("Recalculated total sample size:", object$n, "\n")
  cat("Conditional power:", object$condPow, "\n")
  cat("Conditional power under Delta =", object$delta_ass, ":", object$condPow_true, "\n")
  cat("First stage sample size:", object$n1, "\n")
  cat("Second stage sample size:", object$n2, "\n")
  cat("Observed treatment effect:", object$delta_obs)
}

