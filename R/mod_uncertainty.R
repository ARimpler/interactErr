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
  tagList(fluidRow(
    card(
      h3("Simulation Settings"),
      accordion(open = FALSE, accordion_panel(
        "Instructions", HTML(INS_descriptives)
      )),
      # card_header("Empirical Estimates"),
      # layout_columns(
      #   col_widths = c(2,2,2,2,2,2),
      #   div(numericInput(ns("b0_se"), "SE β₀", value = 0)),
      #   div(numericInput(ns("b1_se"), "SE β₁", value = 0)),
      #   div(numericInput(ns("b2_se"), "SE β₂", value = 0)),
      #   div(numericInput(ns("b3_se"), "SE β₃", value = 0)),
      #   div(numericInput(ns("R2_emp"), "Empirical R^2", value = 0, max=1)),
      #   div(numericInput(ns("var_y_emp"), "Empirical variance  y", value = 0))
      #   ),
      card_header("Data Bounds"),
      layout_columns(
        col_widths=c(2,2,2,2),
          div(numericInput(ns("lower_x"), "Lower Bound x", value = -8)),
          div(numericInput(ns("upper_x"), "Upper Bound x", value = 8)),
          div(numericInput(ns("lower_z"), "Lower Bound z", value = -8)),
          div(numericInput(ns("upper_z"), "Upper Bound z", value = 8))
      ),
      card_header("Samplesize and Parameters"),
      layout_columns(
        col_widths=c(2,3),
        div(
          numericInput(ns("n_emp"), "Original Samplesize", value=25, min = 5)),
        div(numericInput(ns("p_emp"), "Original number of estimates", value=3, min=3)
        )
      )
      ),
    h3("Results"),
      card(
        layout_columns(
          col_widths = c(12),
          div(
            card_header("Confidence Intervals"),
            #tableOutput(ns("CItable")),
            tableOutput(ns("sigma"))
            #tableOutput(ns("plausible_combinations"))
          )
          ),
        div(
          card_header("Sign Flips"),
          tableOutput(ns("flip_table_by_n"))
        ),
        layout_columns(col_widths=c(6,6),
                       div(plotOutput(ns("flip_x_by_n"))),
                       div(plotOutput(ns("flip_z_by_n")))
                       ),
        card_header("Custom Sign Flips"),
        div(textOutput(ns("flip_text"))),
          layout_columns(col_widths=c(6,6),
          div(
            plotOutput(ns("flip_hist_x")),
            plotOutput(ns("flip_hist_z"))),
          div(
            plotOutput(ns("jn_plot_x")),
            plotOutput(ns("jn_plot_z")))
        )
      )
  )
  )
}

#' uncertainty Server Functions
#'
#' @noRd
mod_uncertainty_server <- function(id, dgm, infinite){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
beta<-reactive({dgm$beta()})
QXX<-reactive({dgm$QXs()})
sigma<-reactive({infinite$rse()})


# generic draws -----------------------------------------------------------

gener_draws <- reactive({
  ns <- c(100, 250, 500, 1000, 2000)
  do.call(rbind, lapply(ns, function(n) {
    QXX_inv <- solve(QXX() * n)
    Vcov    <- sigma() * QXX_inv
    df      <- n - input$p_emp - 1
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

output$flip_table_by_n <- renderTable({
  s <- flip_summary_by_n()

  # paste(
  #   sprintf("n = %d: z flips in %.1f%% of draws; x flips in %.1f%% of draws.",
  #           s$n, s$x_prop * 100, s$z_prop * 100),
  #   collapse = "\n"
  # )
})
observe({print(max(gener_draws()$x_critical))})
output$flip_x_by_n <- renderPlot({
  ggplot(gener_draws(), ggplot2::aes(x = x_critical)) +
    geom_histogram(bins = 200,
                   aes(fill = after_stat(x >= input$lower_x & x <= input$upper_x)),
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +
    geom_vline(xintercept = c(input$lower_x, input$upper_x),
                        color = "darkred", linetype = 2, linewidth = 0.8) +
    ggh4x::facet_grid2(n~. , scales = "free", independent = "all") +
    labs(x = "x value where slope of z flips sign",
                  title = "Distribution of flip point across sample sizes")+
    theme_minimal()
})

output$flip_z_by_n <- renderPlot({
  ggplot(gener_draws(), ggplot2::aes(x = z_critical)) +
    geom_histogram(bins = 200,
                   aes(fill = after_stat(x >= input$lower_z & x <= input$upper_z)),
    ) +
    scale_fill_manual(values = c(`TRUE` = "#addc30", `FALSE` = "#440154"), guide = "none") +    geom_vline(xintercept = c(input$lower_z, input$upper_z),
                        color = "darkred", linetype = 2, linewidth = 0.8) +
    ggh4x::facet_grid2(n~. , scales = "free", independent = "all") +
    labs(x = "z value where slope of x flips sign",
                  title = "Distribution of flip point across sample sizes")+
    theme_minimal()
})

# generate plausible draws ------------------------------------------------

 draws <- reactive({
   QXX_inv <- solve(QXX() * input$n_emp)
   sigma2_hat <- sigma()
   Vcov <- sigma2_hat * QXX_inv
   df <- input$n_emp - input$p_emp - 1
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


# output$flip_hist_x <- renderPlot({
#   d <- draws()
#   x_crit <- -d[,2]/d[,4]
#   hist(x_crit, breaks = 200, main = "Distribution of flip point for z's effect (in x)",
#        xlab = "x value where slope of z flips sign", col = "grey85", border = "white")
#   abline(v = c(input$lower_x, input$upper_x), col = "darkred", lwd = 2, lty = 2)
# })
#
# output$flip_hist_z <- renderPlot({
#   d <- draws()
#   z_crit <- -d[,3]/d[,4]
#   hist(z_crit, breaks = 200, main = "Distribution of flip point for x's effect (in z)",
#        xlab = "z value where slope of x flips sign", col = "grey85", border = "white")
#   abline(v = c(input$lower_z, input$upper_z), col = "darkred", lwd = 2, lty = 2)
# })


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

  plot(z_seq, band[2,], type = "l", lwd = 2,
       ylim = range(band), xlab = "z", ylab = "Simple slope of x on y")
  polygon(c(z_seq, rev(z_seq)), c(band[1,], rev(band[3,])),
          col = adjustcolor("steelblue", .3), border = NA)
  abline(h = 0, lty = 2, col = "red")
})

output$jn_plot_z <- renderPlot({
  d <- draws()
  x_seq <- seq(input$lower_x, input$upper_x, length.out = 200)
  slopes <- sapply(x_seq, function(x) d[,3] + d[,4]*x)   # b2 + b3*x draws
  band <- apply(slopes, 2, quantile, probs = c(.025, .5, .975))

  plot(x_seq, band[2,], type = "l", lwd = 2,
       ylim = range(band), xlab = "x", ylab = "Simple slope of z on y")
  polygon(c(x_seq, rev(x_seq)), c(band[1,], rev(band[3,])),
          col = adjustcolor("steelblue", .3), border = NA)
  abline(h = 0, lty = 2, col = "red")
})

 output$plausible_combinations<-renderTable({draws()})
  })
}

## To be copied in the UI
# mod_uncertainty_ui("uncertainty_1")

## To be copied in the server
# mod_uncertainty_server("uncertainty_1")
