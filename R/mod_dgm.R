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
          numericInput(ns("var_x"), "Variance", value = 1, min = 3),
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
)))
}

#' dgm Server Functions
#'
#' @noRd
mod_dgm_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

# reliability to error variance -------------------------------------------
    v_ex<-reactive({
      ((1-input$rel_x)/input$rel_x)*input$var_x # if var_x true variance (not observed)
    })

    v_ez<-reactive({
      ((1-input$rel_z)/input$rel_z)*input$var_z # if var_x true variance (not observed)
    })

    v_ey <- reactive({
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
      rbind(input$b0, input$b1, input$b2, input$b3)
    })
    QXs<-reactive({
      QX(mean_x=input$mean_x, mean_z=input$mean_z,
       var_x = input$var_x, var_z = input$var_z, cov_xz = input$cov_xz)
    })
    Xy<-reactive({E_Xy(beta=beta(), signal_matrix = QXs())})
    observe({print(Xy())})
    observe({print(Xy()[1:3])})
    QDs<-reactive({
      QD(var_error_x = v_ex(),var_error_z = v_ez(), var_x = input$var_x , var_z = input$var_z, mean_x = input$mean_x,
         mean_z = input$mean_z)
    })

  # test --------------------------------------------------------------------

    output$testOUT<-renderPrint({
      list(
        betas=beta(),
        QX=QXs(),
        QD=QDs()
      )
    })

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
