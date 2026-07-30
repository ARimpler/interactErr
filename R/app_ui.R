#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      theme = bs_theme(version = 5, bootswatch = "flatly"),
        nav_panel("Home",fluidRow(card(HTML(intro)))),
        nav_panel("Data Generating Model", mod_dgm_ui("dgm_1")),
        #nav_panel("Visualization", mod_dgm_vis_ui("dgm_vis_1")),
        nav_panel("Population Level", mod_infinite_ui("infinite_1")),
        nav_panel("Sample Level", mod_finite_ui("finite_1")),
        nav_panel("Sign Flips", mod_uncertainty_ui("uncertainty_1")),
        nav_panel("Report", mod_report_ui("report_1"))
      )
      #nav_panel("Empirical Model",mod_dgm_emp_ui("dgm_emp_1"))
      #nav_panel("Visualization", mod_dgm_vis_ui("dgm_vis_2")),

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
