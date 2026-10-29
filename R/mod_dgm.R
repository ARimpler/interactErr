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
                tags$td(numericInput(ns("mean_x"), NULL, value = 5)),
                tags$td(numericInput(ns("var_x"),  NULL, value = 1, min = 0)),
                tags$td(numericInput(ns("rel_x"),  NULL, value = 0.8, min = 0, max = 1))
              ),
              tags$tr(
                tags$td("z"),
                tags$td(numericInput(ns("mean_z"), NULL, value = 3)),
                tags$td(numericInput(ns("var_z"),  NULL, value = 1, min = 0)),
                tags$td(numericInput(ns("rel_z"),  NULL, value = 0.8, min = 0, max = 1))
              )
            )
          )
        ),
        div(#style = "max-width: 160px;",
            h6(HTML("<br>")),
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(tags$th("Cov(x, z)"))),
              tags$tbody(tags$tr(tags$td(numericInput(ns("cov_xz"),NULL, value = 0)))))
            ),
        div(
          #style = "max-width: 640px;",
          h6("Input Regression Weights"),
          tooltip(
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(
                tags$th("\u03b2\u2080"), tags$th("\u03b2\u2081"),
                tags$th("\u03b2\u2082"), tags$th("\u03b2\u2083")
              )),
              tags$tbody(tags$tr(
                tags$td(numericInput(ns("b0"), NULL, value = 0)),
                tags$td(numericInput(ns("b1"), NULL, value = 1)),
                tags$td(numericInput(ns("b2"), NULL, value = 1)),
                tags$td(numericInput(ns("b3"), NULL, value = -0.75))
              ))
            ),
            HTML("\u03b2\u2080=intercept;<br> \u03b2\u2081=linear effect x;<br> \u03b2\u2082=linear effect z;<br> \u03b2\u2083=interaction effect")
          )
        ),
        div(#style = "max-width: 160px;",
            h6(HTML("<br>")),
            tags$table(
              class = "table table-borderless w-auto align-middle",
              tags$thead(tags$tr(tags$th("True R\u00b2"))),
              tags$tbody(tags$tr(tags$td(numericInput(ns("R2_true"), NULL, value = 0.8, min = 0, max = 1, step = 0.05)))))
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

# reliability to error variance -------------------------------------------
    v_ex<-reactive({
      inputs_ok()
      ((1-input$rel_x)/input$rel_x)*input$var_x # if var_x true variance (not observed)
    }) |> debounce(500)

    v_ez<-reactive({
      inputs_ok()
      ((1-input$rel_z)/input$rel_z)*input$var_z # if var_x true variance (not observed)
    }) |> debounce(500)


    v_ey <- reactive({
      inputs_ok()
      error_variance_y(beta(), QXs(), input$R2_true)
    }) |>  debounce(500)
    observe({
      print(paste("var y", v_ey()))
    })


    # v_ey <- reactive({
    #   inputs_ok()
    #     # center-consistent explained (signal) variance:
    #     S_matrix <- QXs() - QXs()[,1] %*% t(QXs()[,1])
    #     signal_var_y <- as.numeric(t(beta()) %*% S_matrix %*% beta())
    #     # clamp R2 to avoid division-by-zero / extremes
    #     R2 <- max(min(input$R2_true, 0.999999), 1e-12)
    #     # noise variance implied by R2: E_y = S * (1 - R2) / R2
    #     noise_y <- signal_var_y * (1 - R2) / R2
    #     noise_y
    # })
    # observe({
    #   print(v_ey())
    # })

    beta<-reactive({
      inputs_ok()
      rbind(input$b0, input$b1, input$b2, input$b3)
    }) |>  debounce(500)

# Error cauchy schwartz  --------------------------------------------------------

    observe({
      inputs_ok()
      max_cov <- sqrt(input$var_x * input$var_z)

      if (abs(input$cov_xz) > max_cov) {
        showNotification(
          paste0(
            "Covariance must satisfy |Cov(X,Z)| \u2264 ",
            round(max_cov, 3)
          ),
          id="cov_error",
          type = "error",
          duration = NULL
        )
      } else {
        removeNotification(id = "cov_error")
      }
    })

# Error reliability 0 -----------------------------------------------------
    observe({
      inputs_ok()

      if (input$rel_x ==0|| input$rel_z ==0||input$rel_x>1||input$rel_z>1) {
        showNotification(
          paste0(
            "The reliability hast to be larger 0 and smaller 1"
          ),
          id="rel_error",
          type = "error",
          duration = NULL
        )
      } else {
        removeNotification(id = "rel_error")
      }
    })



# Error var= 0 ------------------------------------------------------------

    observe({
      inputs_ok()
      if (input$var_x ==0|| input$var_z ==0) {
        showNotification(
          paste0(
            "The predictor variance has to be larger 0."
          ),
          id="var_error",
          type = "error",
          duration = NULL
        )
      } else {
        removeNotification(id = "var_error")
      }
    })


# Error inputs NA ---------------------------------------------------------

    inputs_ok <- reactive({
      vals <- list(input$mean_x, input$var_x, input$rel_x,
                   input$mean_z, input$var_z, input$rel_z,
                   input$cov_xz, input$b0, input$b1, input$b2, input$b3, input$R2_true)
      req(all(!vapply(vals, function(x) is.null(x) || is.na(x), logical(1))))
      TRUE
    })

    # Error: near-singular moment matrix -----------------------------------
    M1_error <- reactiveVal(FALSE)
    M2_error <- reactiveVal(FALSE)
    matrix_has_error <- reactive({ M1_error() || M2_error() })
    observe({print(c(M1_error(), M2_error(),matrix_has_error() ))})

    observe({
      inputs_ok()
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
      inputs_ok()
      req(input$var_x > 0&&input$var_z>0)
      req(abs(input$cov_xz) <= sqrt(input$var_x * input$var_z))
      M<-QX(mean_x=input$mean_x, mean_z=input$mean_z,
       var_x = input$var_x, var_z = input$var_z, cov_xz = input$cov_xz)
      result <- tryCatch({
        qr.solve(M)
        M
      }, error = function(e) NULL)

      M1_error(is.null(result))
      result
      })

    # QXs<-reactive({
    #   inputs_ok()
    #   req(input$var_x > 0&&input$var_z>0)
    #   req(abs(input$cov_xz) <= sqrt(input$var_x * input$var_z))
    #   QX(mean_x=input$mean_x, mean_z=input$mean_z,
    #         var_x = input$var_x, var_z = input$var_z, cov_xz = input$cov_xz)
    # })

    Xy<-reactive({E_Xy(beta=beta(), signal_matrix = QXs())})


    QDs<-reactive({
      inputs_ok()
      req(input$rel_x>0&&input$rel_z>0)
      M<-QD(var_error_x = v_ex(),var_error_z = v_ez(), var_x = input$var_x , var_z = input$var_z, mean_x = input$mean_x,
         mean_z = input$mean_z)
      result <- tryCatch({
        qr.solve(M+QXs())
        M
      }, error = function(e) NULL)

      M2_error(is.null(result))
      result
    })


# beta reduced ------------------------------------------------------------

beta_r<-reactive({
  inputs_ok()
  b0<-beta()[1]+beta()[4]*(input$cov_xz/(input$var_x*input$var_z)-input$mean_x*input$mean_z)
  b1<-beta()[2]+beta()[4]*input$mean_z
  b2<-beta()[3]+beta()[4]*input$mean_x
  c(b0,b1,b2) })
  # test --------------------------------------------------------------------


    # return parameters for other modules -------------------------------------
    params <- reactive({
      list(
        mean_x = input$mean_x,
        var_x = input$var_x,
        rel_x = input$rel_x,
        mean_z = input$mean_z,
        var_z = input$var_z,
        rel_z = input$rel_z,
        cov_xz=input$cov_xz,
        b0 = input$b0,
        b1 = input$b1,
        b2 = input$b2,
        b3 = input$b3,
        R2_true=input$R2_true
      )
    })
    return(list(
      params=params,
      QXs=QXs,
      QDs=QDs,
      beta=beta,
      beta_r=beta_r,
      Xy=Xy,
      v_ey=v_ey,
      v_ex=v_ex,
      v_ez=v_ez,
      inputs_ok=inputs_ok,
      matrix_has_error=matrix_has_error)
    )

    })

}

## To be copied in the UI
# mod_dgm_ui("dgm_1")

## To be copied in the server
# mod_dgm_server("dgm_1")
