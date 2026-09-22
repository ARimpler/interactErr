#' finite UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom mvtnorm rmvnorm
#' @importFrom tidyr pivot_longer
#' @importFrom ggh4x facet_grid2
#' @importFrom bslib  input_task_button
#' @import ggplot2 viridis
#' @importFrom sn cp2dp rmsn
mod_finite_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
    h3("Simulation Settings"),
    card_header("Settings"),
    card(id="setting",
         accordion(
           open=FALSE,
           accordion_panel("Table Explanation", HTML(INS_finite)),
           accordion_panel("Background Information", HTML(BACKINF_finite)),
           accordion_panel("Assumptions", HTML(ASSUMPTIONS_finite))
         )
      )
    ),
  # div(
  #     h3("Simulation Regression Weights"),
  #     card_header("Weights"),
  #     card(id="results_weight",
  #          accordion(
  #            open=FALSE,
  #            layout_columns(
  #              col_widths = c(8,4),
  #              div(
  #              accordion_panel("Instructions", HTML(INS_est_precision_sample)),
  #              accordion_panel("Background Information", HTML(BACKINF_est_precision_sample))
  #              ),
  #              div(
  #                layout_columns(
  #                  col_widths = c(12,12,12),
  #                  div(
  #                    numericInput(ns("seed"), "Seed", value = as.numeric(Sys.Date()))),
  #                  div(numericInput(ns("iter"), "Iterations", value = 1000, max=5000)),
  #                  #actionButton(ns("run"), "Run Simulation", class = "btn-primary")#,
  #                  div(
  #                    checkboxInput(ns("use_skew"), "Skewed predictors", value = FALSE),
  #                    conditionalPanel(
  #                      condition = sprintf("input['%s']", ns("use_skew")),
  #                      numericInput(ns("skew_x"), "Skewness (x, γ1)", value = 0, min = -0.99, max = 0.99, step = 0.05),
  #                      numericInput(ns("skew_z"), "Skewness (z, γ1)", value = 0, min = -0.99, max = 0.99, step = 0.05)
  #                    )
  #                  ),
  #                  div(input_task_button(ns("run"), "Run Simulation")
  #                      #numericInput(ns("n"), "Samplesizes", value = 1, min = 0)
  #                  )
  #                )
  #              )
  #              )
  #          ),
  #       layout_columns(
  #         col_widths = c(5,5,2),
  #         div(
  #           h6("Regression Estimates"),
  #           plotOutput(ns("plot_estimates"), height = "500px")
  #         ),
  #         div(
  #           h6("Summary Table"),
  #           uiOutput(ns("simulation_estimate_summ"))
  #         )
  #       )
  #       )),
  div(
      h3("Results Fit and Differentiatability"),
      card_header("Fit"),
      card(id="results_fit",
           accordion(
             open=FALSE,
             accordion_panel("Instructions", HTML(INS_fit_sample)),
             accordion_panel("Background Information", HTML(BACKINF_fit_sample))
           ),
           layout_columns(
          col_widths = c(6, 6),
          div(h6("Fit Differences"), plotOutput(ns("plot_fit_diff"), height = "500px")),
          div(h6("Fit Summary"), uiOutput(ns("fit_summ")))
        )
      )
    )
  )
}

mod_finite_ui_sim_settings<-function(id){
  ns<-NS(id)
  tagList(
    div(
        card(id="sim_settings",
             accordion(
               open=FALSE,
               layout_columns(
                 col_widths = c(4,4,4),
                 div(
                   accordion_panel("Instructions", HTML(INS_est_precision_sample)),
                   accordion_panel("Background Information", HTML(BACKINF_est_precision_sample))
                 ),
                 div(
                   layout_columns(
                     col_widths = c(12,12,12),
                     div(
                       numericInput(ns("seed"), "Seed", value = as.numeric(Sys.Date()))),
                     div(numericInput(ns("iter"), "Iterations", value = 1000, max=5000)),
                     div(
                       checkboxInput(ns("use_skew"), "Skewed predictors", value = FALSE),
                       conditionalPanel(
                         condition = sprintf("input['%s']", ns("use_skew")),
                         numericInput(ns("skew_x"), "Skewness (x, γ1)", value = 0, min = -0.99, max = 0.99, step = 0.05),
                         numericInput(ns("skew_z"), "Skewness (z, γ1)", value = 0, min = -0.99, max = 0.99, step = 0.05)
                       )
                     ),
                     div(input_task_button(ns("run"), "Run Simulation")
                         #numericInput(ns("n"), "Samplesizes", value = 1, min = 0)
                     )
                   )
                 ),
                 div(plotOutput(ns("predictor_preview"), height = "220px"))               )
             )
        )
    )
  )
}

#' #' @noRd
#' mod_finite_ui_predictor_preview <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     div(plotOutput(ns("predictor_preview"), height = "220px"))
#'   )
#' }
#' @noRd
mod_finite_ui_estimate_summary <- function(id) {
  ns <- NS(id)
  tagList(
    div(tableOutput(ns("sim_summary")))
  )
}
#' @noRd
mod_finite_ui_estimate_plot <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      plotOutput(ns("plot_estimates"), height = "500px")
)
  )
}
#' @noRd
mod_finite_ui_fit_summary <- function(id) {
  ns <- NS(id)
  tagList(
    div(tableOutput(ns("fit_summary")))
  )
}

#' @noRd
mod_finite_ui_fit_diff_plot <- function(id) {
  ns <- NS(id)
  tagList(
    div(plotOutput(ns("plot_fit_diff"), height = "500px"))
  )
}

#' finite Server Functions
#'
#' @noRd
mod_finite_server <- function(id, dgm, df_estimates){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    observe({
      req(input$active_tab == "Explore")
    })




# view predictor distributions --------------------------------------------

    skew_x_d <- reactive(input$skew_x) |> debounce(300)
    skew_z_d <- reactive(input$skew_z) |> debounce(300)

    preview_sample <- reactive({
      mx     <- p()$mean_x
      mz     <- p()$mean_z
      var_x  <- p()$var_x
      var_z  <- p()$var_z
      cov_xz <- p()$cov_xz

      skew_x <- if (isTRUE(input$use_skew)) skew_x_d() else 0
      skew_z <- if (isTRUE(input$use_skew)) skew_z_d() else 0

      n_preview <- 3000  # single draw
      dp <- get_dp_safe(c(mx, mz), var_x, var_z, cov_xz, c(skew_x, skew_z))
      xz <- rmvsn_safe(n_preview, dp, c(mx, mz), var_x, var_z, cov_xz)
      data.frame(x = xz[,1], z = xz[,2])
    })

    output$predictor_preview <- renderPlot({
      df <- preview_sample()
      df_long <- tidyr::pivot_longer(df, cols = c(x, z), names_to = "predictor", values_to = "value")

      ggplot(df_long, aes(x = value, fill = predictor, color = predictor)) +
        geom_density(alpha = 0.3) +
        scale_fill_manual(values = c(x = "#440154", z = "#addc30")) +
        scale_color_manual(values = c(x = "#440154", z = "#addc30")) +
        labs(x = NULL, y = "Density", fill = NULL, color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
# preparation -------------------------------------------------------------

    r<-1:3
    p<-reactive({dgm$params()})
    ovb<-reactive({df_estimates()[3,]})

    iterations<- reactive({
      min(max(input$iter, 1), 5000)  # clamp silently to 5000
    })

# simulation loop ---------------------------------------------------------
sim_results <- eventReactive(input$run, {
      set.seed(input$seed)

      mx     <- p()$mean_x
      mz     <- p()$mean_z
      var_x  <- p()$var_x
      var_z  <- p()$var_z
      cov_xz <- p()$cov_xz
      var_ey <- dgm$v_ey()
      var_ex <-dgm$v_ex()
      var_ez <-dgm$v_ez()
      beta   <- dgm$beta()

      skew_x <- if (isTRUE(input$use_skew)) input$skew_x else 0
      skew_z <- if (isTRUE(input$use_skew)) input$skew_z else 0
      iter   <- iterations()


    results <- list()
    ns<-c(25,50,100, 200,500,1000)
    dp <- get_dp_safe(c(mx, mz), var_x, var_z, cov_xz, c(skew_x, skew_z))
    for(n in ns){
      big_xz <- rmvsn_safe(n * iter, dp, c(mx, mz), var_x, var_z, cov_xz)
      big_ex <- matrix(rnorm(n * iter, 0, sqrt(var_ex)), nrow = n, ncol = iter)
      big_ez <- matrix(rnorm(n * iter, 0, sqrt(var_ez)), nrow = n, ncol = iter)
      big_ey <- matrix(rnorm(n * iter, 0, sqrt(var_ey)), nrow = n, ncol = iter)

      x_all <- matrix(big_xz[,1], nrow = n, ncol = iter)
      z_all <- matrix(big_xz[,2], nrow = n, ncol = iter)

      fit    <- matrix(NA, iter, 9)
      b_sim_r  <- matrix(NA, iter, length(r))
      b_sim_f  <- matrix(NA, iter, 4)

      for(i in 1:iter){
#      tmp<-rmvnorm(n, c(mx,mz), matrix(c(var_x,cov_xz,cov_xz,var_z),2,2))


      # tmp <- rmvsn_safe(n, c(mx, mz), var_x, var_z, cov_xz, c(skew_x, skew_z))
      # x=tmp[,1]
      # z=tmp[,2]
      # x_bar[i] <- mean(x)
      # z_bar[i] <- mean(z)

      # ex<-rnorm(n, 0, sqrt(var_ex))
      # ez<-rnorm(n, 0, sqrt(var_ez))

      x <- x_all[,i]; z <- z_all[,i]
      ex <- big_ex[,i]; ez <- big_ez[,i]; ey <- big_ey[,i]


      Xf<-cbind(1,x,z,x*z)
      #ey<-rnorm(n, 0, sqrt(var_ey))
      y<-Xf%*%beta+ey
      Xf_st<-cbind(1,x+ex,z+ez,(x+ex)*(z+ez))
      QXX<-crossprod(Xf,Xf)
      QXX_st<-crossprod(Xf_st, Xf_st)
      b_r<-qr.solve(QXX_st[r,r])%*%(QXX%*%beta)[r]
      b_f<-qr.solve(QXX_st)%*%(QXX%*%beta)
      b_sim_r[i,]<-t(b_r)
      b_sim_f[i,]<-t(b_f)

      m_y<-mean(y)

      y_hat_r<-Xf_st[,r]%*%b_r
      y_hat_f<-Xf_st%*%b_f
      res1<-y-y_hat_r
      res2<-y-y_hat_f
      ete_r<-t(res1)%*%res1
      ete_f<-t(res2)%*%res2
      tss<-t(y-m_y)%*%(y-m_y)

      fit[i,1]<-sqrt(ete_r/(n-3))
      fit[i,2]<-sqrt(ete_f/(n-4))
      fit[i,3]<-1-ete_r/tss
      fit[i,4]<-1-ete_f/tss
      fit[i,5]<-(t(res1)%*%res1)/n
      fit[i,6]<-(t(res2)%*%res2)/n
      fit[i,7]<-aic(ete_r, n, 4)-aic(ete_f, n, 5)
      fit[i,8]<-bic(ete_r, n, 4)-bic(ete_f, n, 5)
      fit[i,9]<-(1 - (ete_f/(n-4))/(tss/(n-1))) -
        (1 - (ete_r/(n-3))/(tss/(n-1)))
    }
    results[[as.character(n)]] <- list(fit = fit, b_sim_r = b_sim_r, b_sim_f=b_sim_f)
    }
    list(results = results, beta = beta)
})


# max iteration  error ----------------------------------------------------

    observe({
      req(input$iter)

      if (input$iter< 0 ||input$iter>5000) {
        showNotification(
          paste0(
            "Please choose 5000 iterations or fewer. The following results are based on 5000 iterations."
          ),
          id="iter_error",
          type = "error",
          duration = NULL
        )
      } else {
        removeNotification(id = "iter_error")
      }
    })


    df_sim <- reactive({
      sr      <- sim_results()
      results <- sr$results
# reg estimates
      df_r <- do.call(rbind, lapply(names(results), function(n){
        b_sim <- results[[n]]$b_sim_r
        data.frame(n = as.integer(n), model = "Reduced",
                   b0 = b_sim[,1], b1 = b_sim[,2], b2 = b_sim[,3])
      }))
      df_f <- do.call(rbind, lapply(names(results), function(n){
        b_sim <- results[[n]]$b_sim_f
        data.frame(n = as.integer(n), model = "Full",
                   b0 = b_sim[,1], b1 = b_sim[,2], b2 = b_sim[,3], b3 = b_sim[,4])
      }))

      df_r$b3 <- NA_real_
      df_b <- rbind(df_r, df_f)

      df_b_long <- tidyr::pivot_longer(
        df_b, cols = c(b0, b1, b2, b3),
        names_to = "parameter", values_to = "estimate"
      )
      df_b_long <- df_b_long[!(df_b_long$model == "Reduced" & df_b_long$parameter == "b3"), ]


# fit
      df_fit <- do.call(rbind, lapply(names(results), function(n){
        fit_sim <- results[[n]]$fit
        data.frame(n = as.integer(n),
                   r2_r = fit_sim[,3], r2_f = fit_sim[,4], rse_r=fit_sim[,1], rse_f=fit_sim[,2],
                   mse_r=fit_sim[,5], mse_f=fit_sim[,6],
                   d_aic=fit_sim[,7], d_bic=fit_sim[,8], d_r2adj=fit_sim[,9])
      }))
      df_fit_long<-tidyr::pivot_longer(
        df_fit, cols = c(d_aic, d_bic, d_r2adj),
        names_to = "parameter", values_to = "estimate"
      )
      return(list(df_b=df_b, df_b_long=df_b_long,df_fit=df_fit, df_fit_long=df_fit_long))
    })

# Regression Estimate Plot ------------------------------------------------

    output$plot_estimates <- renderPlot({
      df_long <- df_sim()$df_b_long
      beta    <- sim_results()$beta

      df_means <- aggregate(estimate ~ n + model + parameter, data = df_long, FUN = mean)

      df_true <- data.frame(parameter = c("b0","b1","b2","b3"), true_val = beta)
      df_ovb  <- data.frame(parameter = c("b0","b1","b2"),
                            ovb_val   = c(ovb()$b0, ovb()$b1, ovb()$b2))

      ggplot(df_long, aes(x = estimate, fill = model, color = model)) +
        geom_density(alpha = 0.3) +
        facet_grid(n ~ parameter, scales = "free") +
        geom_vline(data = df_true, aes(xintercept = true_val),
                   color = "#440154", linetype = "solid") +
        geom_vline(data = df_means[df_means$model == "Full", ],
                   aes(xintercept = estimate), color = "#440154", linetype = "dashed") +
        geom_vline(data = df_ovb, aes(xintercept = ovb_val),
                   color = "#addc30", linetype = "solid") +
        geom_vline(data = df_means[df_means$model == "Reduced", ],
                   aes(xintercept = estimate), color = "#addc30", linetype = "dashed") +
        scale_fill_manual(values = c(Full = "#440154", Reduced = "#addc30")) +
        scale_color_manual(values = c(Full = "#440154", Reduced = "#addc30")) +
        labs(x = "Estimate", y = "Density", fill = "Model", color = "Model") +
        theme_minimal()+
        theme(legend.position="bottom")
    })

# Regression Estimate Summary Table -------------------------------------------------------------------


    sim_summary <- reactive({
      df_long <- df_sim()$df_b_long
      df_long <- df_long[!is.na(df_long$estimate), ]

      # Aggregate to find numeric means and standard deviations
      agg <- aggregate(estimate ~ n + model + parameter, data = df_long,
                       FUN = function(x) c(mean = mean(x), sd = sd(x)))
      agg <- do.call(data.frame, agg)

      # formatting
      format_sim_cell <- function(mean_val, sd_val) {
        if (is.na(mean_val)) return("-")
        sprintf("%.2f (%.2f)", mean_val, sd_val) # 2 or 3 decimals to match the other table
      }
      agg$cell <- mapply(format_sim_cell, agg$estimate.mean, agg$estimate.sd)

      # align parameters as columns
      wide <- tidyr::pivot_wider(
        agg[, c("n", "model", "parameter", "cell")],
        names_from = parameter, values_from = cell
      )



      # sort ttables
      full_tbl <- wide[wide$model == "Full", c("n", "b0", "b1", "b2", "b3")]
      red_tbl  = wide[wide$model == "Reduced", c("n", "b0", "b1", "b2", "b3")]

      full_tbl <- full_tbl[order(full_tbl$n), ]
      red_tbl  <- red_tbl[order(red_tbl$n), ]
      red_tbl$b3 <- "-"

      full_tbl <- cbind(Model = c(sprintf("Full, &rho;<sub>x</sub>=%.2f, &rho;<sub>z</sub>=%.2f",
                                          p()$rel_x,p()$rel_z), rep("", nrow(full_tbl) - 1)), full_tbl)
      red_tbl  <- cbind(Model = c(sprintf("Reduced, &rho;<sub>x</sub>=%.2f, &rho;<sub>z</sub>=%.2f",
                                          p()$rel_x,p()$rel_z), rep("", nrow(red_tbl) - 1)), red_tbl)


      combined_tbl <- rbind(full_tbl, red_tbl)

      colnames(combined_tbl) <- c("Model", "n", "b₀ (sd)", "b₁ (sd)", "b₂ (sd)", "b₃ (sd)")

      combined_tbl

    })


    output$sim_summary <- renderTable({
      sim_summary()
    }, align = 'lccccc', striped = FALSE, bordered = TRUE,
    sanitize.text.function = function(x) x)


# simulation fit summary --------------------------------------------------



    fit_summary <- reactive({
      df_fit <- df_sim()$df_fit

      df_fit_long <- tidyr::pivot_longer(
        df_fit,
        cols = c(r2_f, rse_f, mse_f, r2_r, rse_r, mse_r),
        names_to = "metric_model", values_to = "estimate"
      )
      df_fit_long$model  <- ifelse(grepl("_f$", df_fit_long$metric_model), "Full", "Reduced")
      df_fit_long$metric <- sub("_[fr]$", "", df_fit_long$metric_model)

      # aggregate
      agg <- aggregate(estimate ~ n + model + metric, data = df_fit_long,
                       FUN = function(x) c(mean = mean(x), sd = sd(x)))
      agg <- do.call(data.frame, agg)

      format_sim_cell <- function(mean_val, sd_val) {
        if (is.na(mean_val)) return("-")
        sprintf("%.3f (%.3f)", mean_val, sd_val)
      }
      agg$cell <- mapply(format_sim_cell, agg$estimate.mean, agg$estimate.sd)


      wide <- tidyr::pivot_wider(
        agg[, c("n", "model", "metric", "cell")],
        names_from = metric, values_from = cell
      )

      # split, order, and label like sim_summary
      full_tbl <- wide[wide$model == "Full", c("n", "r2", "rse", "mse")]
      red_tbl  <- wide[wide$model == "Reduced", c("n", "r2", "rse", "mse")]

      full_tbl <- full_tbl[order(full_tbl$n), ]
      red_tbl  <- red_tbl[order(red_tbl$n), ]

      full_tbl <- cbind(Model = c(sprintf("Full, &rho;<sub>x</sub>=%.2f, &rho;<sub>z</sub>=%.2f",
                                          p()$rel_x,p()$rel_z), rep("", nrow(full_tbl) - 1)), full_tbl)
      red_tbl  <- cbind(Model = c(sprintf("Reduced, &rho;<sub>x</sub>=%.2f, &rho;<sub>z</sub>=%.2f",
                                          p()$rel_x,p()$rel_z), rep("", nrow(red_tbl) - 1)), red_tbl)

      combined_tbl <- rbind(full_tbl, red_tbl)

      colnames(combined_tbl) <- c("Model", "n", "R² (sd)", "RMSE (sd)", "MSE (sd)")

      combined_tbl
    })

    output$fit_summary <- renderTable({
      fit_summary()
    }, align = 'lcccc', striped = FALSE, bordered = TRUE,
    sanitize.text.function = function(x) x)


# Fit Difference Plot -----------------------------------------------------

  output$plot_fit_diff <- renderPlot({

    metric_labels <- c(
      d_aic   = "Delta~AIC",
      d_bic   = "Delta~BIC",
      d_r2adj = "Delta~R[adj]^2"
    )


      df_long <- df_sim()$df_fit_long
      df_long$n <- factor(df_long$n, levels = sort(unique(df_long$n)))

      ggplot(df_long, aes(x = estimate, fill = n, color = n)) +
        geom_density(alpha = 0.3) +
        ggh4x::facet_grid2(n~parameter, scales = "free", independent="y",
                           labeller = labeller(parameter = as_labeller(metric_labels, label_parsed))) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
        scale_fill_viridis_d(option = "viridis") +
        scale_color_viridis_d(option = "viridis") +
        labs(x = "Difference (Full − Reduced)", y = "Density", fill = "n", color = "n") +
        theme_minimal()+
        theme(legend.position="none")
    })

    return(list(
      sim_results=sim_results
    ))
# Fit Difference Table ----------------------------------------------------
    # output$fit_summary <- renderTable({
    #   df_fit <- df_sim()$df_fit
    #   n_vals <- sort(unique(df_fit$n))
    #
    #   rows <- lapply(n_vals, function(nn) {
    #     sub <- df_fit[df_fit$n == nn, ]
    #
    #     full_stats <- c(
    #       r2   = sprintf("%.3f (%.3f)", mean(sub$r2_f),  sd(sub$r2_f)),
    #       rmse = sprintf("%.3f (%.3f)", mean(sub$rse_f), sd(sub$rse_f)),
    #       mse  = sprintf("%.3f (%.3f)", mean(sub$mse_f), sd(sub$mse_f))
    #     )
    #     red_stats <- c(
    #       r2   = sprintf("%.3f (%.3f)", mean(sub$r2_r),  sd(sub$r2_r)),
    #       rmse = sprintf("%.3f (%.3f)", mean(sub$rse_r), sd(sub$rse_r)),
    #       mse  = sprintf("%.3f (%.3f)", mean(sub$mse_r), sd(sub$mse_r))
    #     )
    #     c(n = nn, full_stats, red_stats)
    #   })
    #
    #   df_wide <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    #
    #   df_wide
    #
    # })
  })
}

## To be copied in the UI
# mod_finite_ui("finite_1")

## To be copied in the server
# mod_finite_server("finite_1")
