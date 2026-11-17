#' dgm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
mod_dgm_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .sticky-params-card { position: sticky; top: 0px; z-index: 999;
        background-color: #f8f9fa; border-bottom: 1px solid #dee2e6; }
      .sticky-params-card table td .shiny-input-container { width: 100%; margin-bottom: 0; }
      .sticky-params-card table td .form-control { padding: 2px 4px; width:100%; height: auto; }
      .sticky-params-card table { table-layout: fixed; width: 100%; }
      .sticky-params-card table th, .sticky-params-card table td {
        vertical-align: middle; text-align: center;
      }
    ")),
    card(
      #card_header("Current Parameters"),
      fill = FALSE,
      full_screen = FALSE,
      class = "sticky-params-card",
      layout_columns(
        col_widths = c(5,1,5,1),
        div(
          #style = "max-width: 640px;",
          h6("Input Parameters"),
          tags$table(
            class = "table table-borderless w-auto align-middle",
            tags$thead(tags$tr(
              tags$th(""), tags$th("Mean"), tags$th("Variance"), tags$th("Reliability")
            )),
            tags$tbody(
              tags$tr(
                tags$td("x"),
                tags$td(numericInput(ns("mean_x"), NULL, value = 5, updateOn = "blur")),
                tags$td(numericInput(ns("var_x"),  NULL, value = 1, min = 0, updateOn = "blur")),
                tags$td(numericInput(ns("rel_x"),  NULL, value = 0.8, min = 0, max = 1, updateOn = "blur"))
              ),
              tags$tr(
                tags$td("z"),
                tags$td(numericInput(ns("mean_z"), NULL, value = 3, updateOn = "blur")),
                tags$td(numericInput(ns("var_z"),  NULL, value = 1, min = 0, updateOn = "blur")),
                tags$td(numericInput(ns("rel_z"),  NULL, value = 0.8, min = 0, max = 1, updateOn = "blur"))
              )
            )
          )
        ),
        div(#style = "max-width: 160px;",
            h6(HTML("<br>")),
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(tags$th("Cov(x, z)"))),
              tags$tbody(tags$tr(tags$td(numericInput(ns("cov_xz"),NULL, value = 0, updateOn = "blur")))))
            ),
        div(
          #style = "max-width: 640px;",
          h6("Input Regression Weights"),
          tooltip(
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(
                tags$th(HTML("&beta;<sub>0</sub>")), tags$th(HTML("&beta;<sub>x</sub>")),
                tags$th(HTML("&beta;<sub>z</sub>")), tags$th(HTML("&beta;<sub>xz</sub>"))
              )),
              tags$tbody(tags$tr(
                tags$td(numericInput(ns("b0"), NULL, value = 0, updateOn = "blur")),
                tags$td(numericInput(ns("b1"), NULL, value = 1, updateOn = "blur")),
                tags$td(numericInput(ns("b2"), NULL, value = 1, updateOn = "blur")),
                tags$td(numericInput(ns("b3"), NULL, value = -0.75, updateOn = "blur"))
              ))
            ),
            HTML("&beta;<sub>0</sub>=intercept;<br> &beta;<sub>x</sub>=linear effect x;<br>
                 &beta;<sub>z</sub>=linear effect z;<br> &beta;<sub>xz</sub>=interaction effect")
          )
        ),
        div(#style = "max-width: 160px;",
            h6(HTML("<br>")),
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(tags$th(HTML("True R<sup>2</sup>")))),
              tags$tbody(tags$tr(tags$td(numericInput(ns("R2_true"), NULL, value = 0.66, min = 0, max = 1, step = 0.05, updateOn = "blur")))))
        )
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel("Instructions", HTML(INS_descriptives)),
      accordion_panel("Instructions effects", HTML(INS_effects))
    )
  )
}

#' dgm Server Functions
#'
#' @noRd
mod_dgm_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    params_raw <- reactive({
      list(
        mean_x = input$mean_x, var_x = input$var_x, rel_x = input$rel_x,
        mean_z = input$mean_z, var_z = input$var_z, rel_z = input$rel_z,
        cov_xz = input$cov_xz,
        b0 = input$b0, b1 = input$b1, b2 = input$b2, b3 = input$b3,
        R2_true = input$R2_true
      )
    })

    params_d <- debounce(params_raw, 600)

    validation <- reactive({
      p <- params_d()
      if (any(vapply(p, function(x) is.null(x) || is.na(x), logical(1))))
        return("na")
      errs <- character()
      if (p$var_x <= 0 || p$var_z <= 0)
        errs["var"] <- "The predictor variance has to be larger than 0."
      if (p$rel_x <= 0 || p$rel_z <= 0 || p$rel_x > 1 || p$rel_z > 1)
        errs["rel"] <- "Reliability must be > 0 and <= 1."
      if (!is.na(p$var_x) && p$var_x > 0 && p$var_z > 0 &&
          abs(p$cov_xz) > sqrt(p$var_x * p$var_z)) # Error cauchy schwartz
        errs["cov"] <- sprintf("|Cov(X,Z)| must be <= %.3f", sqrt(p$var_x * p$var_z))
      errs
    })

    params <- reactive({
      v <- validation()
      req(!identical(v, "na"), length(v) == 0)
      params_d()
    })

    observe({
      v <- validation()
      if (identical(v, "na")) return()
      for (k in c("var", "rel", "cov")) {
        id <- paste0(k, "_error")
        if (k %in% names(v)) showNotification(v[[k]], id = id, type = "error", duration = NULL)
        else removeNotification(id)
      }
    })

# reliability to error variance -------------------------------------------

    v_ex<-reactive({
      p<-params()
      ((1-p$rel_x)/p$rel_x)*p$var_x # if var_x true variance (not observed)
    })

    v_ez<-reactive({
      p<-params()
      ((1-p$rel_z)/p$rel_z)*p$var_z # if var_x true variance (not observed)
    })


    v_ey <- reactive({
      p<-params()
      error_variance_y(beta(), QXs(), p$R2_true)
    })

    # observe({
    #   print(paste("var y", v_ey()))
    # })

    matrices <- reactive({
      p    <- params()
      QX_m <- QX(mean_x = p$mean_x, mean_z = p$mean_z,
                 var_x = p$var_x, var_z = p$var_z, cov_xz = p$cov_xz)
      QD_m <- QD(var_error_x = v_ex(), var_error_z = v_ez(),
                 var_x = p$var_x, var_z = p$var_z,
                 mean_x = p$mean_x, mean_z = p$mean_z)
      ok <- tryCatch({
        qr.solve(QX_m)
        qr.solve(QX_m + QD_m)
        TRUE
      }, error = function(e) FALSE)
      list(QX = QX_m, QD = QD_m, ok = ok)
    })

    matrix_has_error <- reactive(!matrices()$ok)


    beta<-reactive({

      p <- params()
      rbind(p$b0, p$b1, p$b2, p$b3)
    })

# Error: near-singular moment matrix -----------------------------------
    observe({
      req(input$var_x > 0, input$var_z > 0)
      req(abs(input$cov_xz) <= sqrt(input$var_x * input$var_z))


      if (matrix_has_error()) {
        showNotification(
          "Means are extreme relative to the variances, making the moment matrix numerically unstable. Try smaller means or larger variances.",
          id = "mean_cond_error", type = "error", duration = NULL
        )
      } else {
        removeNotification(id = "mean_cond_error")
      }
    })


    QXs<-reactive({
      m <- matrices(); req(m$ok); m$QX
      })

    Xy<-reactive({E_Xy(beta=beta(), signal_matrix = QXs())})

    QDs<-reactive({
      m <- matrices(); req(m$ok); m$QD
    })

# beta reduced ------------------------------------------------------------
beta_r<-reactive({
  inputs_ok()
  p <- params()
  b0<-beta()[1]+beta()[4]*(p$cov_xz/(p$var_x*p$var_z)-p$mean_x*p$mean_z)
  b1<-beta()[2]+beta()[4]*p$mean_z
  b2<-beta()[3]+beta()[4]*p$mean_x
  c(b0,b1,b2) })

 # return parameters for other modules -------------------------------------
    state <- reactive({
      list(
        params = params(),
        beta   = beta(),
        v_ex   = v_ex(),
        v_ey   = v_ey(),
        v_ez   = v_ez()
      )
    })

    return(list(
      params=params,
      state=state,
      QXs=QXs,
      QDs=QDs,
      beta=beta,
      beta_r=beta_r,
      Xy=Xy,
      v_ey=v_ey,
      v_ex=v_ex,
      v_ez=v_ez,
      matrix_has_error=matrix_has_error)
    )

    })

}

## To be copied in the UI
# mod_dgm_ui("dgm_1")

## To be copied in the server
# mod_dgm_server("dgm_1")
