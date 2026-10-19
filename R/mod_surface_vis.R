#' surface_vis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
#' @importFrom stats cov qchisq

mod_surface_vis_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    h3("Regression Surface Visualization"),
    layout_columns(
      col_widths = c(2,3,3,1,3),
      card(title="Plot Controls",
           height="605px",
           layout_columns(
             col_widths = c(12,12,12),
             div(tags$b("Range of X"),
                 numericInput(ns("lower_x"), "Lower Bound X", value = -8),
                 numericInput(ns("upper_x"), "Upper Bound X", value = 8)),
             div(tags$b("Range of Z"),
                 numericInput(ns("lower_z"), "Lower Bound Z", value = -8),
                 numericInput(ns("upper_z"), "Upper Bound Z", value = 8)),
             div(tags$b("Evaluation Point"),
                 numericInput(ns("eval_x"), "X", value = 0),
                 numericInput(ns("eval_z"), "Z", value = 0))
           )
      ),
      card(title = "Data-generating model (infinite sample)", height = "605px", full_screen = TRUE,
           plotly::plotlyOutput(ns("dgm_surface"), height = "100%"),
           textOutput(ns("model_equation_true"))),
      card(title = "Applied model (infinite sample)", height = "605px", full_screen = TRUE,
           plotly::plotlyOutput(ns("applied_surface"), height = "100%"),
           textOutput(ns("model_equation_applied"))),
      div(
        checkboxInput(ns("show_finite_var"), "Show sampling uncertainty", value = FALSE),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("show_finite_var")),
          selectInput(ns("unc_n"), "Sample size (n)",
                      choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
          numericInput(ns("ci_level"), "CI level", value = 0.95, min = 0.5, max = 0.999, step = 0.01)
        )
      ),
      card(title = "Estimated model (Simulated samples)", height = "605px", full_screen = TRUE,
           plotly::plotlyOutput(ns("est_surface"), height = "100%"),
           textOutput(ns("model_equation_estimated")))
    ),
    tags$script(HTML(sprintf("
(function() {
  var idA = '%s', idB = '%s', idC = '%s';
  var syncing = false;
  var cBound = false;

  function getEl(id) {
    var el = document.getElementById(id);
    return (el && el.layout) ? el : null;
  }

  function currentGroup() {
    var group = [idA, idB];
    if (getEl(idC)) group.push(idC);
    return group;
  }

  function bindGroup() {
    var ids = currentGroup();
    var els = ids.map(getEl);
    if (els.some(function(el) { return !el; })) return;

    els.forEach(function(el) {
      if (el.removeAllListeners) el.removeAllListeners('plotly_relayout');
    });

    els.forEach(function(src, i) {
      src.on('plotly_relayout', function(ed) {
        if (syncing || !ed['scene.camera']) return;
        syncing = true;
        var updates = els
          .filter(function(_, j) { return j !== i; })
          .map(function(dst) { return Plotly.relayout(dst, {'scene.camera': ed['scene.camera']}); });
        Promise.all(updates).then(function() { syncing = false; });
      });
    });

    cBound = ids.indexOf(idC) !== -1;
  }

  function trySetupAB() {
    if (!getEl(idA) || !getEl(idB)) {
      setTimeout(trySetupAB, 200);
      return;
    }
    bindGroup();
  }
  trySetupAB();

  // Watch for plot C appearing later (e.g. after simulation runs) and fold it in
  function checkForC() {
    if (!cBound && getEl(idC)) {
      bindGroup();
    }
  }

  $(document).on('shiny:value', function(ev) {
    if (ev.name === idA || ev.name === idB) {
      setTimeout(bindGroup, 150);
    }
    if (ev.name === idC) {
      setTimeout(checkForC, 150);
    }
  });

  // fallback poll in case shiny:value doesn't fire cleanly for idC's first render
  var pollC = setInterval(function() {
    checkForC();
    if (cBound) clearInterval(pollC);
  }, 500);
})();
", ns("dgm_surface"), ns("applied_surface"), ns("est_surface")))),
    card(
      title = "Range of Predictors",
      accordion(open = FALSE,
                accordion_panel("Instructions", HTML(INS_3d_vis)),
                accordion_panel("Background Information", HTML(BACKINF_3d_vis)),
                accordion_panel("Assumptions", HTML(ASSUMPTIONS_3d_vis))
      )
    )
  ))
}

#' surface_vis Server Functions
#'
#' @noRd
mod_surface_vis_server <- function(id,dgm, beta_f, beta_r, b_f, b_r, sim_results = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    matrix_has_error<-reactive({dgm$matrix_has_error()})


    x_seq <- reactive(seq(input$lower_x, input$upper_x, length.out = 100))
    z_seq <- reactive(seq(input$lower_z, input$upper_z, length.out = 100))

    y_range <- reactive({
      vals <- c(c_surface_dgm(), c_surface_applied())
      if (isTRUE(input$show_finite_var)) {
        vals <- c(vals,
                  c_surface_applied() + band_f(), c_surface_applied() - band_f())
      }
      range(vals, finite = TRUE)
    })


    design_grid_f <- reactive({
      g <- expand.grid(z = z_seq(), x = x_seq())
      cbind(1, g$x, g$z, g$x * g$z)
    })
    design_grid_r <- reactive({
      g <- expand.grid(z = z_seq(), x = x_seq())
      cbind(1, g$x, g$z)
    })

    pointwise_var <- function(Xg, Sigma) {
      matrix(rowSums((Xg %*% Sigma) * Xg), nrow = length(z_seq()), ncol = length(x_seq()))
    }

    # ---- DGM panel (beta_f/beta_r; no sampling variability by construction) ----
    c_surface_dgm <- reactive({
      req(!matrix_has_error())
      curved_surface(beta = beta_f(), x_seq = x_seq(), z_seq = z_seq())})
    p_surface_dgm <- reactive({
      req(!matrix_has_error())
      plane_surface(beta = beta_r(), x_seq = x_seq(), z_seq = z_seq())})

    # ---- Applied panel (b_f/b_r infinite) ----
    c_surface_applied <- reactive({
      req(!matrix_has_error())
      curved_surface(beta = b_f(), x_seq = x_seq(), z_seq = z_seq())})
    p_surface_applied <- reactive({
      req(!matrix_has_error())
      plane_surface(beta = b_r(), x_seq = x_seq(), z_seq = z_seq())})

# estimated panel ---------------------------------------------------------

    b_draws_f <- reactive({
      req(input$show_finite_var)
      sr <- sim_results(); req(sr)
      sr$results[[as.character(input$unc_n)]]$b_sim_f
    })
    b_draws_r <- reactive({
      req(input$show_finite_var)
      sr <- sim_results(); req(sr)
      sr$results[[as.character(input$unc_n)]]$b_sim_r
    })

    E_b_f<-reactive({
      colMeans(b_draws_f())
    })
    E_b_r<-reactive({
      colMeans(b_draws_r())
    })

    c_surface_est <- reactive(curved_surface(beta = E_b_f(), x_seq = x_seq(), z_seq = z_seq()))
    p_surface_est <- reactive(plane_surface(beta = E_b_r(), x_seq = x_seq(), z_seq = z_seq()))


    band_f <- reactive({
      Sigma <- cov(b_draws_f())
      q <-  qchisq(input$ci_level, df = ncol(b_draws_f()))
      sqrt(q) * sqrt(pointwise_var(design_grid_f(), Sigma))
    })
    band_r <- reactive({
      Sigma <- cov(b_draws_r())
      q <- qchisq(input$ci_level, df = ncol(b_draws_r()))
      sqrt(q) * sqrt(pointwise_var(design_grid_r(), Sigma))
    })

    render_panel <- function(c_surf, p_surf,b, upper_c = NULL, lower_c = NULL, upper_p = NULL, lower_p = NULL) {

      ev_point <- make_ev_point(b, input$eval_x, input$eval_z)
      tangent_x <- make_tangent_x(b, ev_point, z_seq())
      tangent_z <- make_tangent_z(b, ev_point, x_seq())

      p <- plotly::plot_ly() |>
        plotly::add_surface(x = x_seq(), y = z_seq(), z = c_surf,
                            opacity = 0.7, colorscale = list(c(0,"#440154"), c(1,"#440154")), showscale = FALSE,
                            contours = list(
                              x = list(show = TRUE, color = "#EBECF0", width = 1),
                              y = list(show = TRUE, color = "#EBECF0", width = 1)
                            )) |>
        plotly::add_surface(x = x_seq(), y = z_seq(), z = p_surf,
                            opacity = 0.7, colorscale = list(c(0,"#addc30"), c(1,"#addc30")), showscale = FALSE,
                            contours = list(
                              x = list(show = TRUE, color = "black", width = 1),
                              y = list(show = TRUE, color = "black", width = 1)
                            )) |>
        plotly::add_trace(x = rep(input$eval_x, length(z_seq())), y = z_seq(), z = tangent_x,
                          type = "scatter3d", mode = "lines", line = list(color = "#818380", width = 4),
                          opacity = 0.9, showlegend = FALSE) |>
        plotly::add_trace(x = x_seq(), y = rep(input$eval_z, length(x_seq())), z = tangent_z,
                          type = "scatter3d", mode = "lines", line = list(color = "#3E424B", width = 4),
                          opacity = 0.9, showlegend = FALSE) |>
        plotly::add_markers(x = ev_point$x, y = ev_point$z, z = ev_point$y,
                            marker = list(color = "black", size = 5), showlegend = FALSE)

      if (!is.null(upper_c) && !is.null(lower_c)) {
        p <- p |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_c, opacity = 0.25,
                              colorscale = list(c(0,"#440154"), c(1,"#440154")), showscale = FALSE) |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_c, opacity = 0.25,
                              colorscale = list(c(0,"#440154"), c(1,"#440154")), showscale = FALSE)
      }
      if (!is.null(upper_p) && !is.null(lower_p)) {
        p <- p |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_p, opacity = 0.25,
                              colorscale = list(c(0,"#addc30"), c(1,"#addc30")), showscale = FALSE) |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_p, opacity = 0.25,
                              colorscale = list(c(0,"#addc30"), c(1,"#addc30")), showscale = FALSE)
      }

      p |> plotly::layout(scene = list(
        xaxis = list(title = "X"), yaxis = list(title = "Z"),
        zaxis = list(title = "Predicted Y",
        range=y_range()),
        aspectmode = "cube",
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ))
    }
    output$dgm_surface <- plotly::renderPlotly({
      render_panel(c_surface_dgm(), p_surface_dgm(),b=beta_f(), upper_c=NULL, lower_c=NULL, upper_p=NULL, lower_p=NULL)
    })

    output$applied_surface <- plotly::renderPlotly({
      render_panel(c_surface_applied(), p_surface_applied(),b=b_f(),upper_c=NULL, lower_c=NULL, upper_p=NULL, lower_p=NULL)
    })

    output$est_surface <- plotly::renderPlotly({
      up_c <- if (isTRUE(input$show_finite_var)) c_surface_est() + band_f() else NULL
      lo_c <- if (isTRUE(input$show_finite_var)) c_surface_est() - band_f() else NULL
      up_p <- if (isTRUE(input$show_finite_var)) p_surface_est() + band_r() else NULL
      lo_p <- if (isTRUE(input$show_finite_var)) p_surface_est() - band_r() else NULL
      render_panel(c_surface_est(), p_surface_est(),b=E_b_f(), up_c, lo_c, up_p, lo_p)
    })

    output$model_equation_true <- renderText({
      req(!matrix_has_error())
      b <- beta_f()
      ev_point<-make_ev_point(b, input$eval_x, input$eval_z)
      x_eval <- ev_point$x
      z_eval <- ev_point$z
      b0<-b[1]+b[2]*x_eval+b[3]*z_eval+b[4]*x_eval*z_eval
      b1<-b[2]+b[4]*z_eval
      b2<-b[3]+b[4]*x_eval
      b3<-b[4]
      sprintf(
        "At your specified evaluation point x= %.2f, z= %.2f the model parameters are b0=%.2f, b1= %.2f, b2=%.2f. The slope of the interaction term does not change and is b3= %.2f.",
        x_eval, z_eval, b0, b1, b2, b3
      )
    })
    output$model_equation_applied <- renderText({
      req(!matrix_has_error())
      b <- b_f()
      ev_point<-make_ev_point(b, input$eval_x, input$eval_z)
      x_eval <- ev_point$x
      z_eval <- ev_point$z
      b0<-b[1]+b[2]*x_eval+b[3]*z_eval+b[4]*x_eval*z_eval
      b1<-b[2]+b[4]*z_eval
      b2<-b[3]+b[4]*x_eval
      b3<-b[4]
      sprintf(
        "At your specified evaluation point x= %.2f, z= %.2f the model parameters are b0=%.2f, b1= %.2f, b2=%.2f. The slope of the interaction term does not change and is b3= %.2f.",
        x_eval, z_eval, b0, b1, b2, b3
      )
    })
    output$model_equation_estimated <- renderText({
      b <- E_b_f()
      ev_point<-make_ev_point(b, input$eval_x, input$eval_z)
      x_eval <- ev_point$x
      z_eval <- ev_point$z
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
# mod_surface_vis_ui("surface_vis_1")

## To be copied in the server
# mod_surface_vis_server("surface_vis_1")
