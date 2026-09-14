#' dgm_vis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plot_ly layout renderPlotly plotlyOutput add_surface add_markers
mod_dgm_vis_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    card(
      title = "Range of Predictors",
      accordion(open = FALSE,
        accordion_panel("Instructions", HTML(INS_3d_vis)),
        accordion_panel("Background Information", HTML(BACKINF_3d_vis)),
        accordion_panel("Assumptions", HTML(ASSUMPTIONS_3d_vis))
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        div(
          h6("Range of X"),
          numericInput(ns("lower_x"), "Lower Bound X", value = -8),
          numericInput(ns("upper_x"), "Upper Bound X", value = 8),
        ),
        div(
          h6("Range of Z"),
          numericInput(ns("lower_z"), "Lower Bound Z", value = -8),
          numericInput(ns("upper_z"), "Upper Bound Z", value = 8),
        ),
        div(
          h6("Evaluation Point"),
          numericInput(ns("eval_x"), "X", value = 0),
          numericInput(ns("eval_z"), "Z", value = 0),
        )
      )),
    layout_columns(
      div(
        checkboxInput(ns("show_finite_var"), "Show sampling uncertainty", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("show_finite_var")),
          selectInput(ns("unc_n"), "Sample size (n)",
                      choices = c(25, 50, 100, 200, 500, 1000), selected = 100),
          numericInput(ns("ci_level"), "CI level", value = 0.95, min = 0.5, max = 0.999, step = 0.01)
        )
      )
    ),
    card(
      title="Output",
      height= "600px",
      full_screen = TRUE,
      plotlyOutput(ns("interaction_surface"), height = "100%"),
      textOutput(ns("model_equation"))
      )
    )
  )
}

#' dgm_vis Server Functions
#'
#' @noRd
mod_dgm_vis_server <- function(id, beta_f, beta_r, sim_results=NULL){
  moduleServer(id, function(input, output, session){

    ns <- session$ns
    x_seq<-reactive({
      x_seq<-seq(input$lower_x,input$upper_x, length.out=100)
    })
    z_seq<-reactive({
      z_seq<-seq(input$lower_z,input$upper_z, length.out=100)
    })

    c_surface<-reactive({
      curved_surface(beta=beta_f(), x_seq=x_seq(), z_seq=z_seq())
      })
    p_surface<-reactive({
      plane_surface(beta=beta_r(), x_seq=x_seq(), z_seq=z_seq())
    })

  ev_point<-reactive({
    b<-beta_f()
    x<-input$eval_x
    z<-input$eval_z
    y<-b[1]+b[2]*x+b[3]*z+b[4]*x*z
    list(x=x, z=z, y=y)
  })

tangent_x<-reactive({
  ev<-ev_point()
  b<-beta_f()
    x<-ev$x
    z<-ev$z
    dy_dz<-b[3]+b[4]*x
    ev$y+dy_dz*(z_seq()-ev$z)
  })
tangent_z<-reactive({
  b<-beta_f()
  ev<-ev_point()
  x<-ev$x
  z<-ev$z
  dy_dx<-b[2]+b[4]*z
  ev$y+dy_dx*(x_seq()-ev$x)
})



# uncertainty/sample variance ---------------------------------------------

beta_draws <- reactive({
  req(input$show_finite_var, sim_results)
  sr <- sim_results()
  req(sr)
  sr$results[[as.character(input$unc_n)]]$b_sim_f   # iter x 4
})

p_dim <- reactive(ncol(beta_draws()))

Sigma_hat <- reactive(cov(beta_draws()))

chi2_q <- reactive(qchisq(input$ci_level, df = p_dim()))

design_grid <- reactive({
  xs <- x_seq(); zs <- z_seq()
  g <- expand.grid(z = zs, x = xs)   # match curved_surface's (row=z, col=x) layout
  cbind(1, g$x, g$z, g$x * g$z)
})

pointwise_var <- reactive({
  Xg <- design_grid()
  Sigma <- Sigma_hat()
  v <- rowSums((Xg %*% Sigma) * Xg)   # diag(Xg %*% Sigma %*% t(Xg)), without building the full matrix
  matrix(v, nrow = length(z_seq()), ncol = length(x_seq()))
})

band_halfwidth <- reactive(sqrt(chi2_q()) * sqrt(pointwise_var()))

upper_surface <- reactive(c_surface() + band_halfwidth())
lower_surface <- reactive(c_surface() - band_halfwidth())



# variance reduced model --------------------------------------------------

beta_draws_r <- reactive({
  req(input$show_finite_var, sim_results)
  sr <- sim_results()
  req(sr)
  sr$results[[as.character(input$unc_n)]]$b_sim_r   # iter x 3
})

Sigma_hat_r <- reactive(cov(beta_draws_r()))

p_dim_r  <- reactive(ncol(beta_draws_r()))
chi2_q_r <- reactive(qchisq(input$ci_level, df = p_dim_r()))

design_grid_r <- reactive({
  xs <- x_seq(); zs <- z_seq()
  g <- expand.grid(z = zs, x = xs)
  cbind(1, g$x, g$z)
})

pointwise_var_r <- reactive({
  Xg <- design_grid_r()
  Sigma <- Sigma_hat_r()
  v <- rowSums((Xg %*% Sigma) * Xg)
  matrix(v, nrow = length(z_seq()), ncol = length(x_seq()))
})

band_halfwidth_r <- reactive(sqrt(chi2_q_r()) * sqrt(pointwise_var_r()))

upper_surface_r <- reactive(p_surface() + band_halfwidth_r())
lower_surface_r <- reactive(p_surface() - band_halfwidth_r())


# surface plot ------------------------------------------------------------

output$interaction_surface <- renderPlotly({
  p <- plotly::plot_ly() |>
    plotly::add_surface(x = x_seq(), y = z_seq(), z = c_surface(),
                        opacity = 0.7,
                        colorscale = list(c(0,"#440154"), c(1,"#440154")),
                        showscale = FALSE) |>
    plotly::add_surface(x = x_seq(), y = z_seq(), z = p_surface(),
                        opacity = 0.7,
                        colorscale = list(c(0,"#addc30"), c(1,"#addc30")),
                        showscale = FALSE) |>
    plotly::add_trace(x = rep(input$eval_x, length(z_seq())), y = z_seq(), z = tangent_x(),
                      type = "scatter3d", mode = "lines",
                      line = list(color = "#818380", width = 4),
                      opacity = 0.9, showlegend = FALSE) |>
    plotly::add_trace(x = x_seq(), y = rep(input$eval_z, length(x_seq())), z = tangent_z(),
                      type = "scatter3d", mode = "lines",
                      line = list(color = "#3E424B", width = 4),
                      opacity = 0.9, showlegend = FALSE) |>
    plotly::add_markers(x = ev_point()$x, y = ev_point()$z, z = ev_point()$y,
                        marker = list(color = "black", size = 5), showlegend = FALSE)

  if (isTRUE(input$show_finite_var)) {
    p <- p |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_surface(),
                          opacity = 0.25,
                          colorscale = list(c(0,"#440154"), c(1,"#440154")),
                          showscale = FALSE) |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_surface(),
                          opacity = 0.25,
                          colorscale = list(c(0,"#440154"), c(1,"#440154")),
                          showscale = FALSE) |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_surface_r(),
                          opacity = 0.25,
                          colorscale = list(c(0,"#addc30"), c(1,"#addc30")),
                          showscale = FALSE) |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_surface_r(),
                          opacity = 0.25,
                          colorscale = list(c(0,"#addc30"), c(1,"#addc30")),
                          showscale = FALSE)
  }

  p |> plotly::layout(scene = list(
    xaxis = list(title = "X"), yaxis = list(title = "Z"), zaxis = list(title = "Predicted Y")
  ))
})

#
# output$interaction_surface<-renderPlotly({
#       plotly::plot_ly() |>
#         plotly::add_surface(x=x_seq(), y=z_seq(), z=c_surface(),
#                       type="surface",
#                       opacity=0.7,
#                       colorscale=list(c(0,"#440154"),c(1, "#440154")),
#                       showscale=FALSE) |>
#         plotly::add_surface(x=x_seq(), y=z_seq(), z=p_surface(),
#                         type="surface",
#                         opacity=0.7,
#                         colorscale=list(c(0,"#addc30"),c(1,"#addc30")),
#                         showscale=FALSE) |>
#         plotly::add_trace(x=rep(input$eval_x,length(z_seq())), y=z_seq(), z=tangent_x(),
#                           type="scatter3d", mode="lines",
#                           line=(list(color="#818380", width=4)),
#                         opacity = 0.9, showlegend = FALSE) |>
#       plotly::add_trace(x=x_seq(), y=rep(input$eval_z, length(x_seq())), z=tangent_z(),
#                         type="scatter3d", mode="lines",
#                         line=(list(color="#3E424B", width=4)),
#                         opacity = 0.9, showlegend = FALSE) |>
#         plotly::add_markers(x = ev_point()$x, y = ev_point()$z, z = ev_point()$y,
#                                         marker = list(color = "black", size = 5),
#                             showlegend = FALSE) |>
#         plotly::layout(scene=list(
#           xaxis=list(title="X"),
#           yaxis=list(title="Z"),
#           zaxis=list(title="Predicted Y")
#         ))
#       })

output$model_equation <- renderText({
  b <- beta_f()
  x_eval <- ev_point()$x
  z_eval <- ev_point()$z
  b0<-b[1]+b[2]*x_eval+b[3]*z_eval+b[4]*x_eval*z_eval
  b1<-b[2]+b[4]*z_eval
  b2<-b[3]+b[4]*x_eval
  b3<-b[4]
  sprintf(
    "At your specified evaluation point x= %.2f, z= %.2f the model parameters are b0=%.2f, b1= %.2f, b2=%.2f. The slope of the interaction term does not change and is b3= %.2f.",
  x_eval, z_eval, b0, b1, b2, b3
    )
})
  })
}

## To be copied in the UI
# mod_dgm_vis_ui("dgm_vis_1")

## To be copied in the server
# mod_dgm_vis_server("dgm_vis_1")
