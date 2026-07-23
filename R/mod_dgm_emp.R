#' dgm_emp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dgm_emp_ui <- function(id) {
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
  ), fluidRow(
    card_header("Regression Weights"),
    card(
    accordion(open = FALSE, accordion_panel(
      "Instructions effects", HTML(INS_effects)
    )),
    layout_columns(
      col_widths = c(2, 2, 2, 2),
      numericInput(ns("b0"), "b₀ intercept", value = -0.11),
      numericInput(ns("b1"), "b₁ main X", value = 0.72),
      numericInput(ns("b2"), "b₂ main Z", value = 0.72),
      numericInput(ns("b3"), "b₃ interaction", value = -0.59)
    ),
    card_header("Uncertainty"),
    layout_columns(
      col_widths = c(2,2,2,2,2,2),
      div(numericInput(ns("b0_se"), "SE β₀", value = 2.81)),
      div(numericInput(ns("b1_se"), "SE β₁", value = 0.56)),
      div(numericInput(ns("b2_se"), "SE β₂", value = 0.88)),
      div(numericInput(ns("b3_se"), "SE β₃", value = 0.18)),
      div(numericInput(ns("R2_emp"), "Empirical R^2", value = 0.57, max=1)),
      div(numericInput(ns("var_y_emp"), "Empirical variance  y", value = 13.57))
    ),
    card_header("Samplesize and Parameters"),
    layout_columns(
      col_widths=c(2,3),
      div(
        numericInput(ns("n_emp"), "Original Samplesize", value=250, min = 5)),
      div(
        tooltip(numericInput(ns("p_emp"), "Original number of estimates",
                         value=3, min=3),
    "The number of parameters in the regression model excluding the intercept")
      )
    ),
    fluidRow(
      card_header("Out"),
      card(
        tableOutput(ns("sigma"))
      )
    )
  )))
}

#' dgm_emp Server Functions
#'
#' @noRd
mod_dgm_emp_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    v_ex<-reactive({
      ((1-input$rel_x)/input$rel_x)*input$var_x # if var_x true variance (not observed)
    })

    v_ez<-reactive({
      ((1-input$rel_z)/input$rel_z)*input$var_z # if var_x true variance (not observed)
    })

    beta<-reactive({
      rbind(input$b0, input$b1, input$b2, input$b3)
    })

    QXs<-reactive({
      N_correction<-(input$n_emp-1)/input$n_emp
      QXX<-QX(mean_x=input$mean_x, mean_z=input$mean_z,
         var_x = input$var_x, var_z = input$var_z,
         cov_xz = input$cov_xz)
      })
    QDs<-reactive({
      N_correction<-(input$n_emp-1)/input$n_emp
      QD(var_error_x = v_ex(),var_error_z = v_ez(),
         var_x = input$var_x , var_z = input$var_z,
         mean_x = input$mean_x,
         mean_z = input$mean_z)
    })

    QXD_inv<-reactive({
      solve((QXs()+QDs())*input$n_emp)
      })
 observe({
   print(
   c(input$b0_se, input$b1_se, input$b2_se, input$b3_se)^2/diag(solve(QXs()*input$n_emp))
   )
 })
# sigma hat estimates -----------------------------------------------------
    sigma<-reactive({
      M<-QXD_inv()
      sigma2_R2 <- (1 - input$R2_emp) * ((input$n_emp - 1)*input$var_y_emp) /
        (input$n_emp - input$p_emp -1)

      d <- diag(M)
      se <- c(input$b0_se, input$b1_se, input$b2_se, input$b3_se)
      sigma2 <- se^2 / d

      out <- data.frame(
        Parameter = c("β₀", "β₁", "β₂", "β₃", "R²-based"),
        SE      = c(se,NA),
        sigma.2 = c(sigma2,sigma2_R2),
        sigma   = c(sqrt(sigma2), sqrt(sigma2_R2))
      )
      out
    })
    output$sigma<-renderTable({sigma()})


# return ------------------------------------------------------------------
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
  sigma=sigma,
  params=params,
  beta=beta
))
  })
}

## To be copied in the UI
# mod_dgm_emp_ui("dgm_emp_1")

## To be copied in the server
# mod_dgm_emp_server("dgm_emp_1")
