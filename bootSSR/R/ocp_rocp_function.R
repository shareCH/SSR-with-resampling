#' @rdname ssr_method
#' @export



ocp_rocp_design <- function(control, treatment, design, delta,
                            beta_0 = .36, bootstrap = NULL, B = 100) {

  out <- ssrFrame(control, treatment, design, delta, bootstrap, B,
                  ssrMethod = "ocp_rocp", beta_0)

  out
}
