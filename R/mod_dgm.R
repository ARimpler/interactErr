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
  tagList(fluidRow(
    card_header("Descriptive Statistics"),
    card(
      accordion(open = FALSE, accordion_panel(
        "Instructions", HTML(INS_descriptives)
      )),
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          h6("Variable X"),
          numericInput(ns("mean_x"), "Mean", value = 5),
          numericInput(ns("var_x"), "Variance", value = 1, min = 0),
          numericInput(
            ns("rel_x"),
            "Reliability",
            value = 0.8,
            min = 0,
            max = 1
          )
        ),
        div(
          h6("Variable Z"),
          numericInput(ns("mean_z"), "Mean", value = 3),
          numericInput(ns("var_z"), "Variance", value = 1, min = 0),
          numericInput(
            ns("rel_z"),
            "Reliability",
            value = 0.8,
            min = 0,
            max = 1
          )
        ),
        div(
          h6("Covariance"),
          numericInput(ns("cov_xz"), "Cov(X, Z)", value = 0)
        )
      )
    )
  ), fluidRow(card(
    title = "True Effects",
    accordion(open = FALSE, accordion_panel(
      "Instructions effects", HTML(INS_effects)
    )),
    layout_columns(
      col_widths = c(2, 2, 2, 2, 4),
      numericInput(ns("b0"), "β₀ intercept", value = 0),
      numericInput(ns("b1"), "β₁ main X", value = 1),
      numericInput(ns("b2"), "β₂ main Z", value = 1),
      numericInput(ns("b3"), "β₃ interaction", value = -0.75),
      tooltip(
        numericInput(
          ns("R2_true"),
          "True R²",
          value = 0.8,
          min = 0,
          max = 1,
          step = 0.05
        ),
        "R² without measurement error, assuming correct model specification"
      )
    )
)),
fluidRow(
  mod_dgm_vis_ui("dgm_vis_1")
))
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
    })

    v_ez<-reactive({
      inputs_ok()
      ((1-input$rel_z)/input$rel_z)*input$var_z # if var_x true variance (not observed)
    })

    v_ey <- reactive({
      inputs_ok()
        # center-consistent explained (signal) variance:
        S_matrix <- QXs() - QXs()[,1] %*% t(QXs()[,1])
        signal_var_y <- as.numeric(t(beta()) %*% S_matrix %*% beta())
        # clamp R2 to avoid division-by-zero / extremes
        R2 <- max(min(input$R2_true, 0.999999), 1e-12)
        # noise variance implied by R2: E_y = S * (1 - R2) / R2
        noise_y <- signal_var_y * (1 - R2) / R2
        noise_y
    })

    beta<-reactive({
      inputs_ok()
      rbind(input$b0, input$b1, input$b2, input$b3)
    })

# Error cauchy schwartz  --------------------------------------------------------

    observe({
      inputs_ok()
      max_cov <- sqrt(input$var_x * input$var_z)

      if (abs(input$cov_xz) > max_cov) {
        showNotification(
          paste0(
            "Covariance must satisfy |Cov(X,Z)| ≤ ",
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



    QXs<-reactive({
      inputs_ok()
      req(input$var_x > 0&&input$var_z>0)
      req(abs(input$cov_xz) <= sqrt(input$var_x * input$var_z))
      QX(mean_x=input$mean_x, mean_z=input$mean_z,
       var_x = input$var_x, var_z = input$var_z, cov_xz = input$cov_xz)
    })

    Xy<-reactive({E_Xy(beta=beta(), signal_matrix = QXs())})
    # observe({print(Xy())})
    # observe({print(Xy()[1:3])})
    QDs<-reactive({
      inputs_ok()
      req(input$rel_x>0&&input$rel_z>0)
      QD(var_error_x = v_ex(),var_error_z = v_ez(), var_x = input$var_x , var_z = input$var_z, mean_x = input$mean_x,
         mean_z = input$mean_z)
    })


# beta reduced ------------------------------------------------------------

beta_r<-reactive({
  inputs_ok()
  b0<-beta()[1]+beta()[4]*(input$cov_xz/(input$var_x*input$var_z)-input$mean_x*input$mean_z)
  b1<-beta()[2]+beta()[4]*input$mean_z
  b2<-beta()[3]+beta()[4]*input$mean_x
  c(b0,b1,b2) })
  # test --------------------------------------------------------------------

    # output$testOUT<-renderPrint({
    #   list(
    #     betas=beta(),
    #     QX=QXs(),
    #     QD=QDs()
    #   )
    # })

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
        b3 = input$b3
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
      v_ez=v_ez)
    )

    })

}

## To be copied in the UI
# mod_dgm_ui("dgm_1")

## To be copied in the server
# mod_dgm_server("dgm_1")
