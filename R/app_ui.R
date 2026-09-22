#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
# app_ui <- function(request) {
#   tagList(
#     # Leave this function for adding external resources
#     golem_add_external_resources(),
#     # Your application UI logic
#     page_navbar(
#       theme = bs_theme(version = 5, bootswatch = "flatly"),
#         nav_panel("Home",fluidRow(card(HTML(intro)))),
#         nav_panel("Explore", mod_dgm_ui("dgm_1")),
#       # nav_panel("Visualization",
#       #             mod_surface_vis_ui("surface_vis_1")),
#
#
#         #nav_panel("Visualization", mod_dgm_vis_ui("dgm_vis_1")),
#         nav_panel("Population Level", mod_infinite_ui("infinite_1")),
#         nav_panel("Sample Level", mod_finite_ui("finite_1")),
#         nav_panel("Sign Flips", mod_uncertainty_ui("uncertainty_1")),
#         nav_panel("Report", mod_report_ui("report_1"))
#       )
#       #nav_panel("Empirical Model",mod_dgm_emp_ui("dgm_emp_1"))
#       #nav_panel("Visualization", mod_dgm_vis_ui("dgm_vis_2")),
#
#     )
# }

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      id="nav",
      theme = bs_theme(version = 5, bootswatch = "flatly"),
      sidebar=sidebar(id="sb",title="Specifications",width="350px",open="closed", mod_dgm_ui("dgm_1")),
    nav_panel("Home",
              #sidebar(open=FALSE),
              fluidRow(card(HTML(intro)))),
    nav_panel("Explore",
              fillable=FALSE,
              div(
                style = "position: relative; height: 100%; overflow-y: auto;",
                mod_sticky_params_ui("sticky_params_1"),
                div(id="surface", mod_surface_vis_ui("surface_vis_1")),
                div(id="sim_settings", mod_finite_ui_sim_settings("finite_1")),
                div(id="weight_comparison",
                    h3("Regression Weights and Bias"),
                    card(
                      id="reg_weights",
                      accordion(
                        open=FALSE,
                        accordion_panel("Instructions", HTML(INS_infinite)),
                        accordion_panel("Background Information", HTML(BACKINF_infinite)),
                        accordion_panel("Assumptions", HTML(ASSUMPTIONS_infinite))
                      ),
                    layout_columns(
                        col_widths=c(6,6),
                        div(tags$b("Infinite Sample Results"),
                            mod_infinite_ui_bias_weights_combined("infinite_1")),
                        div(tags$b("Simulation Results"),
                            mod_finite_ui_estimate_summary("finite_1"),
                            mod_finite_ui_estimate_plot("finite_1"))
                      )
                    )),
                div(id="fit_info",
                    h3("Model Fit"),
                    card(
                      id="fit",
                      accordion(
                        open=FALSE,
                        accordion_panel("Instructions", HTML(INS_accuracy_pop)),
                        accordion_panel("Background Information", HTML(BACKINF_accuracy_pop))
                      ),
                      layout_columns(
                        col_widths=c(6,6),
                        div(tags$b("Infinite Sample Results"),
                            mod_infinite_ui_fit_table("infinite_1")),
                        div(tags$b("Simulation Results"),
                            mod_finite_ui_fit_summary("finite_1"))
                      )
                    )),
                div(id="fit_difference",
                    h3("Fit Difference"),
                    card(
                      id="fit_diff",
                      accordion(
                        open=FALSE,
                        accordion_panel("Instructions", HTML("SPACEHOLDER")),
                        accordion_panel("Background Information", HTML("SPACEHOLDER"))
                      ),
                      layout_columns(
                        col_widths=c(6,6),
                        div(tags$b("Infinite Sample Results"),
                            mod_infinite_ui_fit_diff_plots("infinite_1")),
                        div(tags$b("Simulation Results"),
                            mod_finite_ui_fit_diff_plot("finite_1"))
                      )
                    )),
                div(id = "uncertainty",mod_uncertainty_ui("uncertainty_1"))
              )
    ),
    nav_panel("Report",
              mod_report_ui("report_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny bslib
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon("loki_ico"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "m.error"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
