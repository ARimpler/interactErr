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
      title = "Simulation Settings",
      accordion(open = FALSE, accordion_panel(
        "Instructions", HTML(INS_descriptives)
      )),
      layout_columns(
        col_widths = c(8,4),
        div(
          h6("Standard Error of Empirical Estimates"),
          numericInput(ns("b0_se"), "SE β₀", value = 0),
          numericInput(ns("b1_se"), "SE β₁", value = 0),
          numericInput(ns("b2_se"), "SE β₂", value = 0),
          numericInput(ns("b3_se"), "SE β₃", value = 0),
          numericInput(ns("R2_emp"), "Empirical R^2 β₃", value = 0, max=1),
          numericInput(ns("var_y_emp"), "Empirical variance  y", value = 0),
          numericInput(ns("lower_x"), "Lower Bound x", value = 1),
          numericInput(ns("upper_x"), "Upper Bound x", value = 2),
          numericInput(ns("lower_z"), "Lower Bound z", value = 1),
          numericInput(ns("upper_z"), "Upper Bound z", value = 2)
        ),
        div(
          h6("Samplesize and Parameters"),
          numericInput(ns("n_emp"), "Original Samplesize", value=25, min = 5),
          numericInput(ns("p_emp"), "Original number of estimates", value=3, min=3)
        )
      )),
      card(
        title="Results",
        layout_columns(
          col_widths = c(12),
          div(
            h6("Confidence Intervals"),
            #tableOutput(ns("CItable")),
            tableOutput(ns("sigma"))
            #tableOutput(ns("plausible_combinations"))
          )
          ),
          div(
            h6("Sign Flips"),
            textOutput(ns("flip_text"))
          ),
          layout_columns(col_widths=c(6,6),
          div(
            plotOutput(ns("flip_hist_x")),
            plotOutput(ns("flip_hist_z"))),
          div(
            plotOutput(ns("jn_plot_x")),
            plotOutput(ns("jn_plot_z")))
        )
      )
  ))
}

#' uncertainty Server Functions
#'
#' @noRd
mod_uncertainty_server <- function(id, dgm){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
beta<-reactive({dgm$beta()})
QXX<-reactive({dgm$QXs()})
#tss<-reactive({infinite$tss})
 t_crit<-reactive({
   df <- input$n_emp - input$p_emp -1
   percentile<-0.975
   qt(percentile, df)
 })

 CI<-reactive({
  out<-rbind(
 c(beta()[1]-t_crit()*input$b0_se, beta()[1]+t_crit()*input$b0_se),
 c(beta()[2]-t_crit()*input$b1_se, beta()[2]+t_crit()*input$b1_se),
 c(beta()[3]-t_crit()*input$b2_se, beta()[3]+t_crit()*input$b2_se),
 c(beta()[4]-t_crit()*input$b3_se, beta()[4]+t_crit()*input$b3_se))
  rownames(out) <- c("β₀", "β₁", "β₂", "β₃")
  colnames(out) <- c("Lower", "Upper")
  out

  })

 output$CItable<-renderTable({CI()})

 sigma<-reactive({

# sigma hat estimates -----------------------------------------------------
   QXX_inv<-solve(QXX()*input$n_emp)
   sigma2_R2 <- (1 - input$R2_emp) * ((input$n_emp - 1)*input$var_y_emp) /
     (input$n_emp - input$p_emp - 1)

   d <- diag(QXX_inv)
   se <- c(input$b0_se, input$b1_se, input$b2_se, input$b3_se)
   sigma2 <- se^2 / d

   out <- data.frame(
     Parameter = c("β₀", "β₁", "β₂", "β₃", "R²-based"),
     `SE`      = c(se,NA),
     `sigma^2` = c(sigma2,sigma2_R2),
     `sigma`   = c(sqrt(sigma2), sqrt(sigma2_R2))
   )
   out
 })
 output$sigma<-renderTable({sigma()})

# generate plausible draws ------------------------------------------------

 draws <- reactive({
   QXX_inv <- solve(QXX() * input$n_emp)
   sigma2_hat <- mean(sigma()[,4])
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


output$flip_hist_x <- renderPlot({
  d <- draws()
  x_crit <- -d[,2]/d[,4]
  hist(x_crit, breaks = 80, main = "Distribution of flip point for z's effect (in x)",
       xlab = "x value where slope of z flips sign", col = "grey85", border = "white")
  abline(v = c(input$lower_z, input$upper_z), col = "darkred", lwd = 2, lty = 2)
})

output$flip_hist_z <- renderPlot({
  d <- draws()
  z_crit <- -d[,2]/d[,4]
  hist(z_crit, breaks = 80, main = "Distribution of flip point for x's effect (in z)",
       xlab = "z value where slope of x flips sign", col = "grey85", border = "white")
  abline(v = c(input$lower_z, input$upper_z), col = "darkred", lwd = 2, lty = 2)
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
