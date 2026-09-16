#' infinite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
mod_infinite_ui <- function(id) {
  ns <- NS(id)
  tagList(
             # fluidRow(
             #   h3("Regression Weights and Bias"),
             #   card(
             #     id="reg_weights",
             #     accordion(
             #       open=FALSE,
             #       accordion_panel("Instructions", HTML(INS_infinite)),
             #       accordion_panel("Background Information", HTML(BACKINF_infinite)),
             #       accordion_panel("Assumptions", HTML(ASSUMPTIONS_infinite))
             #     ),
             #     layout_columns(
             #       col_widths = c(12),
             #       # tableOutput(ns("coef_table")),
             #       # tableOutput(ns("bias_table")),
             #       #tableOutput(ns("combined_table"))
             #     )
             #   )),

             fluidRow(
               h3("Fit, Prediction Error, and their Differences"),
               card(card_header("Expected Model Fit and Prediction Accuracy on Population Level"),
                    id="model_fit_pop",
                    accordion(
                      open=FALSE,
                      accordion_panel("Instructions", HTML(INS_accuracy_pop)),
                      accordion_panel("Background Information", HTML(BACKINF_accuracy_pop)),
                    ),
                    #tableOutput(ns("model_fit")),
                    layout_columns(
                      widths=c(4,4,4),
                      plotOutput(ns("r2_diff_heatmap"), height = "500px"),
                      plotOutput(ns("mse_diff_heatmap"), height = "500px"),
                      plotOutput(ns("rmse_diff_heatmap"), height = "500px")
                    )
               )
             )

  )
}

#' @noRd
mod_infinite_ui_fit_diff_plots <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      widths=c(4,4,4),
      plotOutput(ns("r2_diff_heatmap"), height = "500px"),
      plotOutput(ns("rmse_diff_heatmap"), height = "500px"),
      plotOutput(ns("mse_diff_heatmap"), height = "500px")
    )
  )
}

#' @noRd
mod_infinite_ui_bias_weights_combined <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("combined_table"))
  )
}

#' @noRd
mod_infinite_ui_fit_table <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("model_fit"))
  )
}

#' infinite Server Functions
#'
#' @noRd
mod_infinite_server <- function(id,dgm){
  moduleServer(id, function(input, output, session){
  ns <- session$ns

  mod_sticky_params_server(
    "sticky_params",
    params = dgm$params
  )
# preparation -------------------------------------------------------------


  r<-1:3
  p<-reactive({dgm$params()})
  QXX<-reactive({dgm$QXs()})
  Xy<-reactive({dgm$Xy()})
  beta<-reactive({dgm$beta()})
  QX_f<-reactive({QXX()})
  QX_r<-reactive({QXX()[r,r]})
  Xy<-reactive({dgm$Xy()})
  QDD<-reactive({dgm$QDs()})
  QD_f<-reactive({QDD()})
  QD_r<-reactive({QDD()[r,r]})

  Qst_f<-reactive({QX_f()+QD_f()})
  Qst_r<-reactive({Qst_f()[r,r]})

  v_ey<-reactive({dgm$v_ey()})
# Computation --------------------------------------------------------------


  T_model<-reactive({beta()})#qr.solve(QX_f(), Xy())
  OVB_model<-reactive({qr.solve(QX_r(), Xy()[r])})
  ME_model<-reactive({qr.solve(Qst_f(), Xy())})
  ME_OVB_model<-reactive({qr.solve(Qst_r(), Xy()[r])})


# coefficent table --------------------------------------------------------

  df_estimates<-reactive({
    df <- data.frame(
      Model = c("Full, No Error", "Full, Error", "Reduced, No Error", "Reduced, Error"),
      b0 = c(beta()[1], ME_model()[1], OVB_model()[1], ME_OVB_model()[1]),
      b1 = c(beta()[2], ME_model()[2], OVB_model()[2], ME_OVB_model()[2]),
      b2 = c(beta()[3], ME_model()[3], OVB_model()[3], ME_OVB_model()[3]),
      b3 = c(beta()[4], ME_model()[4], NA, NA)  # pad with NA where beta3 doesn't exist
    )
    df
  })


# bias table --------------------------------------------------------------
  bias_T<-reactive({
    df<-data.frame(
      Model=c("Full, No Error", "Full, Error", "Reduced, No Error", "Reduced, Error"),
      B0=df_estimates()[,2]-df_estimates()[1,2],
      B1=df_estimates()[,3]-df_estimates()[1,3],
      B2=df_estimates()[,4]-df_estimates()[1,4],
      B3=df_estimates()[,5]-df_estimates()[1,5]
    )
  })




  df_combined <- reactive({
    est <- df_estimates()
    bias <- bias_T()

    # Function to format cell as "Estimate (Bias)"
    format_cell <- function(est_val, bias_val) {
      if (is.na(est_val)) return("-")

      # Format bias with a plus sign if positive
      bias_str <- if (bias_val > 0) sprintf("+%.3f", bias_val) else sprintf("%.3f", bias_val)

      # If it's the benchmark model (bias is 0), just show the estimate
      if (abs(bias_val) < 1e-5) return(sprintf("%.3f", est_val))

      sprintf("%.3f (%s)", est_val, bias_str)
    }

    data.frame(
      Model = est$Model,
      `β₀ (B0)` = mapply(format_cell, est$b0, bias$B0),
      `β₁ (B1)` = mapply(format_cell, est$b1, bias$B1),
      `β₂ (B2)` = mapply(format_cell, est$b2, bias$B2),
      `β₃ (B3)` = mapply(format_cell, est$b3, bias$B3),
      check.names = FALSE
    )
  })


# fit table ---------------------------------------------------------------
  tss<-reactive({ tss_est(beta=beta(),signal_matrix=QX_f(), E_y=v_ey())})
  rse<-reactive({residual_measures(beta=beta(), signal_matrix = QX_f(),
                                   XE=Qst_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)[4]})


 fit_T<- reactive({
    fit_no_error<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=QX_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)
    fit_ME<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=Qst_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)
    fit_ovb<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=QX_r(), EXY = Xy()[r], E_y=v_ey(), interaction=FALSE)
    fit_joint<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=Qst_r(), EXY = Xy()[r], E_y=v_ey(), interaction=FALSE)


    df <- data.frame(
      Model = c("Full, No Error", "Full, Error","Reduced, No Error" ,"Reduced, Error"),
      R2 = c(fit_no_error[2], fit_ME[2],fit_ovb[2], fit_joint[2]),
      RMSE = c(fit_no_error[3], fit_ME[3],fit_ovb[3], fit_joint[3]),
      MSE = c(fit_no_error[1],  fit_ME[1],fit_ovb[1], fit_joint[1])
    )
    df
  })



# fit heat maps -----------------------------------------------------------
  rse_grid <- reactive({
    rel_x_vals <- seq(0.1, 1, length.out = 10)
    rel_z_vals <- seq(0.1, 1, length.out = 10)
    expand.grid(rel_x = rel_x_vals, rel_z = rel_z_vals)
  })

  res_results <- reactive({
    grid <- rse_grid()
    Q_XX_m <-QX_f()
    res_matrix <-mapply(function(rel_x, rel_z, Q_XX=Q_XX_m) {
      var_ex<-((1-rel_x)/rel_x)*p()$var_x
      var_ez<-((1-rel_z)/rel_z)*p()$var_z
      Q_DD_m <- QD(var_error_x = var_ex,
                   var_error_z = var_ez,
                   var_x = p()$var_x,
                   var_z = p()$var_z,
                   mean_x = p()$mean_x,
                   mean_z = p()$mean_z)
      Q_st_m <-Q_XX_m+Q_DD_m
      res_f<- residual_measures(beta = beta(),
                                    signal_matrix = Q_XX_m,
                                    XE = Q_st_m, EXY = Xy(),E_y=v_ey(), interaction=TRUE)
      res_r <- residual_measures(beta = beta(),
                                    signal_matrix = Q_XX_m,
                                    XE = Q_st_m, EXY = Xy(),E_y=v_ey(), interaction=FALSE)
      res_diff<-res_f-res_r

      res_diff
    }, grid$rel_x, grid$rel_z)
    res_matrix<-t(res_matrix)
    data.frame(grid, E_res_d=res_matrix[,1],E_R2_d=res_matrix[,2], E_RMSE_d=res_matrix[,3], E_RSE_d=res_matrix[,4])
  })




# # output ------------------------------------------------------------------
#
#   output$coef_table <- renderTable({
#     df_estimates()
#   }, digits = 3)
#
#   output$bias_table<-renderTable({
#     bias_T()
#   }, digits=3)
#
  output$combined_table <- renderTable({
    df_combined()
  }, align = 'lcccc', bordered=TRUE)
#
  output$model_fit <- renderTable({
    fit_T()
  }, digits = 3)
#
#
#
  output$r2_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_R2_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154", mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(x = "Reliability X", y = "Reliability Z",
           fill = "Δ R²")+
      theme(legend.position = "bottom")
  })

  output$mse_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_RSE_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154", mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(x = "Reliability X", y = "Reliability Z",
           fill = "Δ MSE")+
      theme(legend.position = "bottom")
  })

  output$rmse_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_RMSE_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154",mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(x = "Reliability X", y = "Reliability Z",
           fill = "Δ RMSE")+
      theme(legend.position = "bottom")
  })
 return(list(df_estimates=df_estimates,bstar_f=ME_model, bstar_r=ME_OVB_model,
             beta_f=T_model, beta_r=OVB_model , Qstar_f=Qst_f, rse=rse, tss=tss,
             bias_table=bias_T, ests_bias_combined=df_combined, residual_results=res_results,
             fit_table=fit_T))
  })
}

## To be copied in the UI
# mod_infinite_ui("infinite_1")

## To be copied in the server
# mod_infinite_server("infinite_1")
