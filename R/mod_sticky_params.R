#' sticky_params UI Function
#'
#' @description sticky parameter cards at top of UI page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card layout_columns
mod_sticky_params_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      title = "Current Parameters",
      fill = FALSE,
      full_screen = FALSE,
      style = "position: sticky; top: 0px; z-index: 999;
  background-color: #f8f9fa; border-bottom: 1px solid #dee2e6;
      min-height: 80px; display: block;",
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          h6("Variable X"),
          textOutput(ns("summary_x"))
        ),
        div(
          h6("Variable Z"),
          textOutput(ns("summary_z"))
        ),
        div(
          h6("True Regression Weights"),
          textOutput(ns("summary_effects"))
        )
      )
    )
  )
}

#' sticky_params Server Functions
#'
#' @noRd
mod_sticky_params_server <- function(id, params){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$summary_x <- renderText({
      p<-params()
      sprintf("Mean: %g, Var: %g, Rel: %g", p$mean_x, p$var_x, p$rel_x)  })
    output$summary_z <- renderText({
      p<-params()
      sprintf("Mean: %g, Var: %g, Rel: %g", p$mean_z, p$var_z, p$rel_z)  })
    output$summary_effects <- renderText({
      p<-params()
      sprintf("β₀: %g, β₁: %g, β₂: %g ,β₃: %g",  p$b0, p$b1, p$b2, p$b3)
    })
  })
}

## To be copied in the UI
# mod_sticky_params_ui("sticky_params_1")

## To be copied in the server
# mod_sticky_params_server("sticky_params_1")
