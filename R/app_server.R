#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_server <- function(input, output, session) {
  observeEvent(input$nav, {
    bslib::toggle_sidebar(id = "sb", open = input$nav != "Home")
  }, ignoreInit = FALSE)

  dgm<-mod_dgm_server("dgm_1")
  infinite<-mod_infinite_server("infinite_1", dgm)
  finite<-mod_finite_server("finite_1", dgm, infinite$df_estimates)
 # mod_uncertainty_server("uncertainty_1", dgm, infinite)
  surface<-mod_surface_vis_server("surface_vis_1",
                         b_f=infinite$bstar_f,  b_r=infinite$bstar_r,
                         beta_f=infinite$beta_f, beta_r=infinite$beta_r,
                         sim_results=finite$sim_results,
                         dgm=dgm)
  mod_residual_surface_server("residual_1", dgm=dgm, beta_f = infinite$beta_f,
                              b_f = infinite$bstar_f, b_r = infinite$bstar_r,surface=surface)
  #mod_report_server("report_1", dgm=dgm, infinite=infinite)
  }
