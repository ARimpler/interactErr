#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  dgm<-mod_dgm_server("dgm_1")
  mod_sticky_params_server("sticky_params_1", dgm$params)
  infinite<-mod_infinite_server("infinite_1", dgm)
  mod_finite_server("finite_1", dgm, infinite$df_estimates)
  mod_uncertainty_server("uncertainty_1", dgm)
}
