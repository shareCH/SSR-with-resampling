#' @rdname ssr_method
#' @export


pz_design <- function(control, treatment, design, delta,
                        beta_tilde = .64, bootstrap = NULL, B = 100) {

  out <- ssrFrame(control, treatment, design, delta, bootstrap, B,
                  ssrMethod = "pz", beta_tilde = beta_tilde)

  out
}
