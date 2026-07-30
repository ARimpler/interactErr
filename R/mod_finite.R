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
#' @importFrom gt cols_label tab_spanner gt
#' @importFrom ggh4x facet_grid2
#' @importFrom bslib  input_task_button
#' @import ggplot2 viridis
#' @importFrom sn cp2dp rmsn
mod_finite_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      title = "On this page",
      position = "left",
      width = 200,
      tags$nav(
        tags$a(class = "d-block mb-2", href = "#setting", "Settings"),
        tags$a(class = "d-block mb-2", href = "#results_weight", "Results Regression Weights"),
        tags$a(class = "d-block mb-2", href = "#results_fit", "Results Fit and Differentiatability")
      )
    ),
  tagList(fluidRow(
    h3("Simulation Settings"),
    card_header("Settings"),
    card(id="setting",
         accordion(
           open=FALSE,
           accordion_panel("Table Explanation", HTML(INS_finite)),
           accordion_panel("Background Information", HTML(BACKINF_finite)),
           accordion_panel("Assumptions", HTML(ASSUMPTIONS_finite))
         )
      ),
      layout_columns(
        col_widths = c(3,3,3),
        div(
          numericInput(ns("seed"), "Seed", value = as.numeric(Sys.Date()))),
        div(numericInput(ns("iter"), "Iterations", value = 1000, max=5000)),
          #actionButton(ns("run"), "Run Simulation", class = "btn-primary")#,
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
      ))
    ),
  fluidRow(
      h3("Simulation Regression Weights"),
      card_header("Weights"),
      card(id="results_weight",
           accordion(
             open=FALSE,
             accordion_panel("Instructions", HTML(INS_est_precision_sample)),
             accordion_panel("Background Information", HTML(BACKINF_est_precision_sample))
           ),
        layout_columns(
          col_widths = c(6,6),
          div(
            h6("Regression Estimates"),
            plotOutput(ns("plot_estimates"), height = "500px")
          ),
          div(
            h6("Summary Table"),
            uiOutput(ns("simulation_estimate_summ"))
          )
        )
        )),
  fluidRow(
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

#' finite Server Functions
#'
#' @noRd
mod_finite_server <- function(id, dgm, df_estimates){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
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
      # x_bar    <- numeric(iter)
      # z_bar    <- numeric(iter)
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

    output$simulation_estimate_summ <- renderUI({
      df_long <- df_sim()$df_b_long
      df_long <- df_long[!is.na(df_long$estimate), ]
      n_vals  <- sort(unique(df_long$n))

      rows <- lapply(n_vals, function(nn) {
        sub      <- df_long[df_long$n == nn, ]
        full_sub <- sub[sub$model == "Full", ]
        red_sub  <- sub[sub$model == "Reduced", ]

        full_stats <- vapply(c("b0","b1","b2","b3"), function(p) {
          v <- full_sub$estimate[full_sub$parameter == p]
          sprintf("%.2f (%.2f)", mean(v), sd(v))
        }, character(1))

        red_stats <- vapply(c("b0","b1","b2"), function(p) {
          v <- red_sub$estimate[red_sub$parameter == p]
          sprintf("%.2f (%.2f)", mean(v), sd(v))
        }, character(1))

        c(n = nn, full_stats, red_stats)
      })

      df_wide <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      colnames(df_wide) <- c("n",
                             "full_b0","full_b1","full_b2","full_b3",
                             "red_b0","red_b1","red_b2")

      gt_tbl <- gt(df_wide) |>
        tab_spanner(label = "Full model",    columns = c(full_b0, full_b1, full_b2, full_b3)) |>
        tab_spanner(label = "Reduced model", columns = c(red_b0, red_b1, red_b2)) |>
        cols_label(
          full_b0 = "b0 (sd)", full_b1 = "b1 (sd)", full_b2 = "b2 (sd)", full_b3 = "b3 (sd)",
          red_b0  = "b0 (sd)", red_b1  = "b1 (sd)", red_b2  = "b2 (sd)"
        ) |>
        cols_label(n = "n")

      gt_tbl
    })


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
# Fit Difference Table ----------------------------------------------------
    output$fit_summ <- renderUI({
      df_fit <- df_sim()$df_fit
      n_vals <- sort(unique(df_fit$n))

      rows <- lapply(n_vals, function(nn) {
        sub <- df_fit[df_fit$n == nn, ]

        full_stats <- c(
          r2   = sprintf("%.3f (%.3f)", mean(sub$r2_f),  sd(sub$r2_f)),
          rmse = sprintf("%.3f (%.3f)", mean(sub$rse_f), sd(sub$rse_f)),
          mse  = sprintf("%.3f (%.3f)", mean(sub$mse_f), sd(sub$mse_f))
        )
        red_stats <- c(
          r2   = sprintf("%.3f (%.3f)", mean(sub$r2_r),  sd(sub$r2_r)),
          rmse = sprintf("%.3f (%.3f)", mean(sub$rse_r), sd(sub$rse_r)),
          mse  = sprintf("%.3f (%.3f)", mean(sub$mse_r), sd(sub$mse_r))
        )
        c(n = nn, full_stats, red_stats)
      })

      df_wide <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      colnames(df_wide) <- c("n",
                             "full_r2","full_rmse","full_mse",
                             "red_r2","red_rmse","red_mse")

      gt(df_wide) |>
        tab_spanner(label = "Full model",    columns = c(full_r2, full_rmse, full_mse)) |>
        tab_spanner(label = "Reduced model", columns = c(red_r2, red_rmse, red_mse)) |>
        cols_label(
          full_r2 = "R²", full_rmse = "RMSE", full_mse = "MSE",
          red_r2  = "R²", red_rmse  = "RMSE", red_mse  = "MSE",
          n = "n"
        )
    })

  return(list(sim_results=sim_results))
  })
}

## To be copied in the UI
# mod_finite_ui("finite_1")

## To be copied in the server
# mod_finite_server("finite_1")
