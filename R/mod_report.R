#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import tinytex
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_report"), "Download PDF Summary")
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, dgm, infinite){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report_summary_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # copy template to a temp dir so rendering doesn't write into your package's inst/ folder
        tmp_rmd <- file.path(tempdir(), "report_template.Rmd")
        file.copy(app_sys("app/report/report_template.Rmd"), tmp_rmd, overwrite = TRUE)
        #browser()

        rmarkdown::render(
          tmp_rmd,
          output_file = file,
          params = list(
            dgm_params  = dgm$params(),
            coef_table  = infinite$df_estimates(),
            bias_table  = infinite$bias_table(),
            fit_table   = infinite$fit_table(),
            flip_text   = sprintf("text")
          ),
          envir = new.env(parent = globalenv())  # isolates rendering environment
        )
      }
    )
  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
