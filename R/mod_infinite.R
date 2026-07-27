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
           mod_sticky_params_ui(ns("sticky_params")), # add the sticky summary card
             fluidRow(
               card(
                 title="Effects on Population Level",
                 id="pop",
                 layout_columns(
                   col_widths = c(6, 6),
                   tableOutput(ns("coef_table")),
                   tableOutput(ns("bias_table"))
                 ),
                 accordion(
                   open=FALSE,
                   accordion_panel("Table Explanation", HTML(EXPL_weights_pop))
                 )
               )),
           fluidRow(
             div(HTML(vis_applied)),
             mod_dgm_vis_ui("dgm_vis_2")),
             fluidRow(
               card(card_header("Expected Model Fit and Prediction Accuracy on Population Level"),
                    id="model_fit_pop",
                    tableOutput(ns("model_fit")),
                    layout_columns(
                      widths=c(4,4,4),
                      plotOutput(ns("r2_diff_heatmap"), height = "500px"),
                      plotOutput(ns("mse_diff_heatmap"), height = "500px"),
                      plotOutput(ns("rmse_diff_heatmap"), height = "500px")
                    ),
                    accordion(
                      open=FALSE,
                      accordion_panel("Explanation of Heatmaps", HTML(EXPL_accuracy_pop))
                    )
               )
             )

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

  observe({
    print(Xy()[r])
  })
  T_model<-reactive({beta()})#qr.solve(QX_f(), Xy())
  OVB_model<-reactive({qr.solve(QX_r(), Xy()[r])})
  ME_model<-reactive({qr.solve(Qst_f(), Xy())})
  ME_OVB_model<-reactive({qr.solve(Qst_r(), Xy()[r])})


# coefficent table --------------------------------------------------------

  df_estimates<-reactive({
    df <- data.frame(
      Model = c("Full, No Error", "Full, Error", "Misspecified, No Error", "Misspecified, Error"),
      b0 = c(beta()[1], ME_model()[1], OVB_model()[1], ME_OVB_model()[1]),
      b1 = c(beta()[2], ME_model()[2], OVB_model()[2], ME_OVB_model()[2]),
      b2 = c(beta()[3], ME_model()[3], OVB_model()[3], ME_OVB_model()[3]),
      b3 = c(beta()[4], ME_model()[4], NA, NA)  # pad with NA where beta3 doesn't exist
    )
    df
  })
  output$coef_table <- renderTable({
    df_estimates()
  }, digits = 3)

# bias table --------------------------------------------------------------
  output$bias_table<-renderTable({
    df<-data.frame(
      Model=c("Full, No Error", "Full, Error", "Misspecified, No Error", "Misspecified, Error"),
      B0=df_estimates()[,2]-df_estimates()[1,2],
      B1=df_estimates()[,3]-df_estimates()[1,3],
      B2=df_estimates()[,4]-df_estimates()[1,4],
      B3=df_estimates()[,5]-df_estimates()[1,5]
    )
  }, digits=3)
# fit table ---------------------------------------------------------------
  tss<-reactive({ tss_est(beta=beta(),signal_matrix=QX_f(), E_y=v_ey())})
  rse<-reactive({residual_measures(beta=beta(), signal_matrix = QX_f(),
                                   XE=Qst_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)[4]})

  output$model_fit <- renderTable({
    fit_no_error<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=QX_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)
    fit_ME<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=Qst_f(), EXY = Xy(), E_y=v_ey(), interaction = TRUE)
    fit_ovb<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=QX_r(), EXY = Xy()[r], E_y=v_ey(), interaction=FALSE)
    fit_joint<-residual_measures(beta=beta(), signal_matrix = QX_f(), XE=Qst_r(), EXY = Xy()[r], E_y=v_ey(), interaction=FALSE)


    df <- data.frame(
      Model = c("Full, No Error", "Full, Error","Misspecified, No Error" ,"Misspecified, Error"),
      R2 = c(fit_no_error[2], fit_ME[2],fit_ovb[2], fit_joint[2]),
      RMSE = c(fit_no_error[3], fit_ME[3],fit_ovb[3], fit_joint[3]),
      MSE = c(fit_no_error[1],  fit_ME[1],fit_ovb[1], fit_joint[1])
    )
    df
  }, digits = 3)


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


  output$r2_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_R2_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154", mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(title = "Difference in R² (Misspecified - Full)",
           x = "Reliability X", y = "Reliability Z",
           fill = "Δ R²")
  })

  output$mse_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_RSE_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154", mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(title = "Difference in MSE (Misspecified - Full)",
           x = "Reliability X", y = "Reliability Z",
           fill = "Δ MSE")
  })

  output$rmse_diff_heatmap <- renderPlot({

    df <- res_results()
    ggplot(df, aes(x = rel_x, y = rel_z, fill = E_RMSE_d)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0, low = "#440154",mid = "white", high = "#addc30") +
      theme_minimal() +
      labs(title = "Difference in RMSE (Misspecified - Full)",
           x = "Reliability X", y = "Reliability Z",
           fill = "Δ RMSE")
  })
 return(list(df_estimates=df_estimates,bstar_f=ME_model, bstar_r=ME_OVB_model ,Qstar_f=Qst_f, rse=rse, tss=tss))
  })
}

## To be copied in the UI
# mod_infinite_ui("infinite_1")

## To be copied in the server
# mod_infinite_server("infinite_1")
