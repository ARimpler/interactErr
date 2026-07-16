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
#' @import ggplot2 viridis
mod_finite_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    card(
      title = "Simulation Settings",
      accordion(open = FALSE, accordion_panel(
        "Instructions", HTML(INS_descriptives)
      )),
      layout_columns(
        col_widths = c(12),
        div(
          h6("Inputs"),
          numericInput(ns("seed"), "Seed", value = 2026),
          numericInput(ns("iter"), "Iterations", value = 5000),
          actionButton(ns("run"), "Run Simulation", class = "btn-primary")#,
          #numericInput(ns("n"), "Samplesizes", value = 1, min = 0)
        )
      ),
      card(
        title="Simulation Results",
        layout_columns(
          col_widths = c(12),
          #textOutput(ns("testOUT")),
          plotOutput(ns("plot_estimates"), height = "500px")
        )
      )
    )
  ))
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
      iter   <- input$iter

      ns_vec  <- c(25, 100, 200, 500, 1000)
      results <- list()
    ns<-c(25,100, 200,500,1000)
    for(n in ns){
      fit    <- matrix(NA, iter, 4)
      b_sim_r  <- matrix(NA, iter, length(r))
      b_sim_f  <- matrix(NA, iter, 4)
    for(i in 1:iter){
      tmp<-rmvnorm(n, c(mx,mz), matrix(c(var_x,cov_xz,cov_xz,var_z),2,2))
      x=tmp[,1]
      z=tmp[,2]
      ex<-rnorm(n, 0, sqrt(var_ex))
      ez<-rnorm(n, 0, sqrt(var_ez))
      Xf<-cbind(1,x,z,x*z)
      ey<-rnorm(n, 0, sqrt(var_ey))
      y<-Xf%*%beta+ey
      Xf_st<-cbind(1,x+ex,z+ez,(x+ex)*(z+ez))
      QXX<-crossprod(Xf,Xf)
      QXX_st<-crossprod(Xf_st, Xf_st)
      b_r<-qr.solve(QXX_st[r,r])%*%(QXX%*%beta)[r]
      b_f<-qr.solve(QXX_st)%*%(QXX%*%beta)
      b_sim_r[i,]<-t(b_r)
      b_sim_f[i,]<-t(b_f)
      ete<-(t(beta)%*%QXX%*%beta- t(b_r)%*%QXX_st[r,r]%*%b_r+t(ey)%*%ey)
      rse_n<-sqrt(ete/(n))
      rse_nk<-sqrt(ete/(n-3))
      r2<-1-ete/t(y-mean(y))%*%(y-mean(y))
      fit[i,1]<-rse_n
      fit[i,2]<-rse_nk
      fit[i,3]<-r2
      fit[i,4]<-ete/n
    }
    results[[as.character(n)]] <- list(fit = fit, b_sim_r = b_sim_r, b_sim_f=b_sim_f)
    }
    list(results = results, beta = beta)
})

    output$plot_estimates <- renderPlot({
      sr      <- sim_results()
      results <- sr$results
      beta    <- sr$beta

      df_r <- do.call(rbind, lapply(names(results), function(n){
        b_sim <- results[[n]]$b_sim_r
        data.frame(
          n  = as.integer(n),
          model="Reduced",
          b0 = b_sim[,1],
          b1 = b_sim[,2],
          b2 = b_sim[,3]
        )
      }))
      df_f <- do.call(rbind, lapply(names(results), function(n){
        b_sim <- results[[n]]$b_sim_f
        data.frame(
          n  = as.integer(n),
          model="Full",
          b0 = b_sim[,1],
          b1 = b_sim[,2],
          b2 = b_sim[,3]
        )
      }))

      df_b <- rbind(df_r, df_f)

      df_b_long <- tidyr::pivot_longer(
        df_b, cols = c(b0, b1, b2),
        names_to = "parameter", values_to = "estimate"
      )

      ggplot(df_b_long, aes(x = estimate, fill = model, color = model)) +
        geom_density(alpha = 0.3) +
        facet_grid(n ~ parameter, scales = "free") +
        geom_vline(
          data = data.frame(
            parameter = c("b0","b1","b2"),
            true_val  = c(beta[1], beta[2], beta[3])
          ),
          aes(xintercept = true_val), linetype = "dashed"
        ) +
        geom_vline(
          data = data.frame(
            parameter = c("b0","b1","b2"),
            true_val  = c(ovb()$b0, ovb()$b1, ovb()$b2)
          ),
          aes(xintercept = true_val), color="red", linetype = "solid"
        ) +
        scale_fill_viridis_d(option = "viridis") +
        scale_color_viridis_d(option = "viridis") +
        labs(x = "Estimate", y = "Density", fill = "n", color = "n") +
        theme_minimal()
    })

  return(list(sim_results=sim_results))
  })
}

## To be copied in the UI
# mod_finite_ui("finite_1")

## To be copied in the server
# mod_finite_server("finite_1")
