#' @rdname ssr_method
#' @export



rocp_design <- function(control, treatment, design, delta,
                          beta_0 = .4, bootstrap = NULL, B = 100) {

  out <- ssrFrame(control, treatment, design, delta, bootstrap, B,
                  ssrMethod = "rocp", beta_0)

  out
}
