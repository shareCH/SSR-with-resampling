#' @rdname ssr_method
#' @export


of_design <- function(control, treatment, design, delta, beta_tilde = .64,
                       gamma = .005/4, bootstrap = NULL, B = 100) {

  out <- ssrFrame(control, treatment, design, delta, bootstrap, B,
                  ssrMethod = "of", beta_tilde = beta_tilde, gamma = gamma)

  out

}
