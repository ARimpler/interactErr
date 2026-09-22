#' uncertainty UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uncertainty_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
    card_header("General Info"),
    card(
      accordion(open = FALSE,
          accordion_panel("Instructions", HTML(INS_sign_flips)),
          accordion_panel("Background Information", HTML(BACKINF_sign_flips)),
          accordion_panel("Assumptions", HTML(ASSUMPTIONS_sign_flips))
      )),
    h3("Sign Flips"),
    card_header( "Predefined Samplesizes"),
    card(
      id="predef",
      accordion(open = FALSE,
                layout_columns(
                  col_widths = c(8,4),
                  div(accordion_panel("Instructions", HTML(INS_flips)),
                      accordion_panel("Background Information", HTML(BACKINF_flips))),
                  div(
                    card_header("Data Bounds"),
                    card(id="settings",
                         div(textOutput(ns("inf_flip_location"))),
                         div(HTML("After you have defined the bounds of your data click `Run' to compute the probability and location of sign flips within your specified
               data range for finite samples.")),
                         div(input_task_button(ns("run"), "Run")),
                         layout_columns(
                           col_widths=c(2,2,2,2),
                           div(numericInput(ns("lower_x"), "Lower Bound x", value = 2)),
                           div(numericInput(ns("upper_x"), "Upper Bound x", value = 8)),
                           div(numericInput(ns("lower_z"), "Lower Bound z", value = 1)),
                           div(numericInput(ns("upper_z"), "Upper Bound z", value = 6))
                         ))
                  )
                )

      ),
      div(HTML(INS_flip_figs)),
      layout_columns(col_widths=c(4,4,4),
                     div(uiOutput(ns("flip_table_by_n"))),
                     div(plotOutput(ns("flip_b1_by_n"))),
                     div(plotOutput(ns("flip_b2_by_n")))
      ))#,
      # card(
      #   card_header("Custom Sign Flips"),
      #   div(
      #     layout_columns(
      #       col_widths=c(8,4),
      #       div(textOutput(ns("flip_text"))),
      #       div(      layout_columns(
      #         col_widths=c(12,12),
      #         div(
      #           numericInput(ns("n_emp"), "Original Samplesize", value=25, min = 5))#,
      #         #div(numericInput(ns("p_emp"), "Original number of estimates", value=3, min=3)
      #         #)
      #       ))
      #       )
      #     ),
      #     layout_columns(col_widths=c(6,6),
      #     div(
      #       plotOutput(ns("flip_hist_x")),
      #       plotOutput(ns("flip_hist_z"))),
      #     div(
      #       plotOutput(ns("jn_plot_x")),
      #       plotOutput(ns("jn_plot_z")))
      #   )
      # )
  )
  )
}

#' uncertainty Server Functions
#'
#' @noRd
mod_uncertainty_server <- function(id, dgm, infinite){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observe({
      req(input$active_tab == "Explore")
    })


beta<-reactive({infinite$bstar_f()})
observe({print(beta())})
#beta<-reactive({dgm$beta()})
QXX<-reactive({infinite$Qstar_f()})
sigma<-reactive({infinite$rse()})



# flip text infinite ------------------------------------------------------

output$inf_flip_location<-renderText({
  b1b3=-beta()[2]/beta()[4]
  b2b3=-beta()[3]/beta()[4]
  sprintf("For an infinite sample the sign change of b1 relatig to predictor x
          is at z = %.2f, the sign change for b2 relating to predictor z is at x= %.2f.",
          b1b3, b2b3)
})

# generic draws -----------------------------------------------------------

gener_draws <- eventReactive(input$run, {
  ns <- c(100, 250, 500, 1000, 2000)
  do.call(rbind, lapply(ns, function(n) {
    QXX_inv <- solve(QXX() * n)
    Vcov    <- sigma() * QXX_inv
    #df      <- n - input$p_emp - 1
    df      <- n - 3 - 1
    d <- mvtnorm::rmvt(n = 10000, delta = beta(), sigma = Vcov, df = df)
    data.frame(n = n, x_critical = -d[,3]/d[,4], z_critical = -d[,2]/d[,4])
  }))
})

flip_summary_by_n <- reactive({
  gener_draws() |>
    dplyr::group_by(n) |>
    dplyr::summarise(
      x_prop = mean(input$lower_x <= x_critical & x_critical <= input$upper_x),
      z_prop = mean(input$lower_z <= z_critical & z_critical <= input$upper_z),
      .groups = "drop"
    )
})


# sign flips across sample sizes ------------------------------------------


output$flip_table_by_n<-renderUI({
  s<-flip_summary_by_n()
  s$x_prop<-round(s$x_prop*100,2)
  s$z_prop<-round(s$z_prop*100,2)

  gt::gt(s) |>
    gt::cols_label(x_prop="% Flips in x", z_prop ="% Flips in z")
})
observe({print(max(gener_draws()$x_critical))})

output$flip_b1_by_n <- renderPlot({
  ggplot(gener_draws(), ggplot2::aes(x = z_critical)) +
    geom_histogram(bins = 200,
                   aes(fill = after_stat(x >= input$lower_z & x <= input$upper_z)),
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +
    geom_vline(xintercept = c(input$lower_z, input$upper_z),
               color = "black", linetype = 1, linewidth = 0.8) +
    geom_vline(xintercept = c(input$lower_z, input$upper_z),
               color = "white", linetype = 2, linewidth = 0.6)+
    ggh4x::facet_grid2(n~. , scales = "free", independent = "all") +
    labs(x = "z value where slope of x flips sign",
         title = "Distribution of flip point across sample sizes")+
    theme_minimal()
})

output$flip_b2_by_n <- renderPlot({
  ggplot(gener_draws(), ggplot2::aes(x = x_critical)) +
    geom_histogram(bins = 200,
                   aes(fill = after_stat(x >= input$lower_x & x <= input$upper_x)),
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +
    geom_vline(xintercept = c(input$lower_x, input$upper_x),
               color = "black", linetype = 1, linewidth = 0.8) +
    geom_vline(xintercept = c(input$lower_x, input$upper_x),
               color = "white", linetype = 2, linewidth = 0.6)+
    ggh4x::facet_grid2(n~. , scales = "free", independent = "all") +
    labs(x = "x value where slope of z flips sign",
                  title = "Distribution of flip point across sample sizes")+
    theme_minimal()
})



# generate plausible draws ------------------------------------------------

 draws <- reactive({
   QXX_inv <- solve(QXX() * input$n_emp)
   sigma2_hat <- sigma()
   Vcov <- sigma2_hat * QXX_inv
   #df <- input$n_emp - input$p_emp - 1
   df <- input$n_emp - 3 - 1
   mvtnorm::rmvt(n = 10000, delta = beta(), sigma = Vcov, df = df)
 })

 flips<-reactive({
   d<-draws()
   x_critical<--d[,3]/d[,4]
   z_critical<--d[,2]/d[,4]
   x_prop<-mean(input$lower_x <= x_critical & x_critical <= input$upper_x)
   z_prop<-mean(input$lower_z <= z_critical & z_critical <= input$upper_z)
   c(x_prop, z_prop)
 })

output$flip_text<-renderText({
  sprintf(
    "In %.1f%% of plausible parameter combinations, the effect of z on y flipped sign within the observed range of x.
In %.1f%% of plausible parameter combinations, the effect of x on y flipped sign within the observed range of z.
This is based on %s plausible combinations of regression weights, simulated from the reconstructed sampling distribution.",
    flips()[1] * 100,
    flips()[2] * 100,
    format(10000, big.mark = ",")
  )
  })



# histograms for sign flips -----------------------------------------------


output$flip_hist_x <- renderPlot({
  d <- data.frame(x_crit = -draws()[,3] / draws()[,4])

  ggplot(d, aes(x = x_crit)) +
    geom_histogram(
      aes(fill = after_stat(x >= input$lower_x & x <= input$upper_x)),
      bins = 200, color = "white"
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +
    geom_vline(xintercept = c(input$lower_x, input$upper_x),
               color = "darkred", linetype = 2, linewidth = 0.8) +
    labs(x = "x value where slope of z flips sign",
         title = "Distribution of flip point for z's effect (in x)") +
    theme_minimal()
})

output$flip_hist_z <- renderPlot({
  d <- data.frame(z_crit = -draws()[,2] / draws()[,4])

  ggplot(d, aes(x = z_crit)) +
    geom_histogram(
      aes(fill = after_stat(x >= input$lower_z & x <= input$upper_z)),
      bins = 200, color = "white"
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +
    geom_vline(xintercept = c(input$lower_z, input$upper_z),
               color = "darkred", linetype = 2, linewidth = 0.8) +
    labs(x = "z value where slope of x flips sign",
         title = "Distribution of flip point for x's effect (in z)") +
    theme_minimal()
})

# johnson neyman ----------------------------------------------------------

output$jn_plot_x <- renderPlot({
  d <- draws()
  z_seq <- seq(input$lower_z, input$upper_z, length.out = 200)
  slopes <- sapply(z_seq, function(z) d[,2] + d[,4]*z)   # b1 + b3*z draws
  band <- apply(slopes, 2, quantile, probs = c(.025, .5, .975))

  plot_df <- data.frame(
    z     = z_seq,
    lower = band[1, ],
    mid   = band[2, ],
    upper = band[3, ],
    pos= band[2, ]>=0
  )

  ggplot(plot_df, aes(x = z, y = mid)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = pos),
                alpha = 0.3) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"),
                      guide = "none") +
    labs(x = "z", y = "Simple slope of x on y") +
    theme_minimal()
})

output$jn_plot_z <- renderPlot({
  d <- draws()
  x_seq <- seq(input$lower_x, input$upper_x, length.out = 200)
  slopes <- sapply(x_seq, function(x) d[,3] + d[,4]*x)   # b2 + b3*x draws
  band <- apply(slopes, 2, quantile, probs = c(.025, .5, .975))

  plot_df <- data.frame(
    x     = x_seq,
    lower = band[1, ],
    mid   = band[2, ],
    upper = band[3, ],
    pos= band[2, ]>=0
  )

  ggplot(plot_df, aes(x = x, y = mid)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = pos),
                alpha = 0.3) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"),
                      guide = "none") +
    labs(x = "x", y = "Simple slope of  z on y") +
    theme_minimal()

})

 output$plausible_combinations<-renderTable({draws()})
  })
}

## To be copied in the UI
# mod_uncertainty_ui("uncertainty_1")

## To be copied in the server
# mod_uncertainty_server("uncertainty_1")
