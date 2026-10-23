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
  tagList(
    h3("Regression Surface Visualization"),
    layout_sidebar(
      tags$head(tags$style(HTML("
  .sidebar .table td,
  .sidebar .table th {
    vertical-align: middle !important;
  }
  .sidebar .table .form-group,
  .sidebar .table .shiny-input-container {
    margin: 0 !important;
  }
"))),
      fillable=TRUE,
      sidebar=sidebar(
        width=220,
        open="desktop",
        h6("Predictor Range"),
        tags$table(
          class = "table table-borderless w-auto align-middle",
          tags$thead(tags$tr(
            tags$th(""), tags$th("Lower"), tags$th("Upper")
          )),
          tags$tbody(
            tags$tr(
              tags$td("x", class = "align-middle fw-semibold"),
              tags$td(numericInput(ns("lower_x"), NULL, value = 2)),
              tags$td(numericInput(ns("upper_x"), NULL, value = 8))),
            tags$tr(
              tags$td("z", class = "align-middle fw-semibold"),
              tags$td(numericInput(ns("lower_z"), NULL, value = -1)),
              tags$td(numericInput(ns("upper_z"), NULL, value = 6.5)))
          )
        ),
        h6("Evaluation Points"),
        tags$table(
          class = "table table-borderless align-middle w-auto",
          tags$thead(tags$tr(
            tags$th("x"), tags$th("z")
          )),
          tags$tbody(
            tags$tr(
              tags$td(numericInput(ns("eval_x"), NULL, value = 0)),
              tags$td(numericInput(ns("eval_z"), NULL, value = 0)))
          )
        ),
        div(
          checkboxInput(ns("show_cloud"), "Data Cloud", value = FALSE)
        ),
        div(
          class = "d-flex align-items-end gap-2",
          numericInput(ns("n_cloud"), "Points", value = 300, min = 10, max = 3000),
          actionButton(ns("draw"), "Run")
        ),
        div(checkboxInput(ns("show_finite_var"), "Sampling uncertainty", value = FALSE)),
        div(conditionalPanel(
              condition = sprintf("input['%s']", ns("show_finite_var")),
              selectInput(ns("unc_n"), "Sample size (n)",
                          choices = c(25, 50, 100, 250, 500, 1000), selected = 100),
              numericInput(ns("ci_level"), "CI level", value = 0.95, min = 0.5, max = 0.999, step = 0.01)
            )
        )
      ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 4, 4)),
      card(
           full_screen = TRUE,
           card_header("Data-generating model (infinite sample)"),
           card_body(padding=0,
                     plotly::plotlyOutput(ns("dgm_surface"))),
          card_footer(textOutput(ns("model_equation_true")))
),
      card(
        full_screen = TRUE,
           card_header("Applied model (infinite sample)"),
           card_body(padding=0,
                     plotly::plotlyOutput(ns("applied_surface"))),
           card_footer(textOutput(ns("model_equation_applied")))),

      card(
        full_screen = TRUE,
           card_header("Estimated model (Simulated samples)"),
           card_body(padding=0,
                     plotly::plotlyOutput(ns("est_surface"))),
           card_footer(textOutput(ns("model_equation_estimated"))))
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
  )
)
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


      x_line <- range(c(x_seq(), ev_point$x))
      z_line <- range(c(z_seq(), ev_point$z))


      tangent_x <- make_tangent_x(b, ev_point, z_seq())
      tangent_z <- make_tangent_z(b, ev_point, x_seq())

      x_seq_tangent<-sort(c(x_seq(), ev_point$x))
      z_seq_tangent<-sort(c(z_seq(), ev_point$z))

      p <- plotly::plot_ly() |>
        plotly::add_surface(x = x_seq(), y = z_seq(), z = c_surf, # "#440154" #fde725
                            opacity = 0.8, colorscale = list(c(0,"#fdd525"), c(1,"#fdd525")),
                            showscale = FALSE,
                            contours = list(
                              x = list(show = TRUE, color = "black", width = 1), ##EBECF0
                              y = list(show = TRUE, color = "black", width = 1)
                            )) |>
        plotly::add_surface(x = x_seq(), y = z_seq(), z = p_surf, # #35b779
                            opacity = 0.8, colorscale = list(c(0,"#35b799"), c(1,"#35b799")),
                            showscale = FALSE,
                            contours = list(
                              x = list(show = TRUE, color = "black", width = 1),
                              y = list(show = TRUE, color = "black", width = 1)
                            )) |>
        plotly::add_trace(x = rep(input$eval_x, length(z_seq())),
                          y = z_seq(), z = tangent_x,
                          type = "scatter3d", mode = "lines", line = list(color = "#31688e",##818380
                                                                          width = 4),
                          opacity = 0.9, showlegend = FALSE) |>
        plotly::add_trace(x = x_seq(), y = rep(input$eval_z, length(x_seq())),
                          z = tangent_z,
                          type = "scatter3d", mode = "lines", line = list(color = "#440154",
                                                                          width = 4),
                          opacity = 0.9, showlegend = FALSE) |>
        plotly::add_markers(x = ev_point$x, y = ev_point$z, z = ev_point$y,
                            marker = list(color = "black", size = 5), showlegend = FALSE)

      if (!is.null(upper_c) && !is.null(lower_c)) {
        p <- p |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_c, opacity = 0.25,
                              colorscale = list(c(0,"#fdd525"), c(1,"#fdd525")), showscale = FALSE) |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_c, opacity = 0.25,
                              colorscale = list(c(0,"#fdd525"), c(1,"#fdd525")), showscale = FALSE)
      }
      if (!is.null(upper_p) && !is.null(lower_p)) {
        p <- p |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = upper_p, opacity = 0.25,
                              colorscale = list(c(0,"#35b799"), c(1,"#35b799")), showscale = FALSE) |>
          plotly::add_surface(x = x_seq(), y = z_seq(), z = lower_p, opacity = 0.25,
                              colorscale = list(c(0,"#35b799"), c(1,"#35b799")), showscale = FALSE)
      }

      p |> plotly::layout(scene = list(
        xaxis = list(title = "X"), yaxis = list(title = "Z"),
        zaxis = list(title = "Predicted Y",
        range=y_range()),
        aspectmode = "cube",
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
      ),
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
      autosize = TRUE)
    }

    cloud <- eventReactive(input$draw, {
      req(!matrix_has_error())
      p <- dgm$params()
      simulate_cloud(
        n = min(max(input$n_cloud, 10), 5000),
        mean = c(p$mean_x, p$mean_z),
        var_x = p$var_x, var_z = p$var_z, cov_xz = p$cov_xz,
        var_ex = dgm$v_ex(), var_ez = dgm$v_ez(), var_ey = dgm$v_ey(),
        beta = beta_f(), b_f = b_f(), b_r = b_r()
      )
    })

    output$dgm_surface <- plotly::renderPlotly({
      p<-render_panel(c_surface_dgm(), p_surface_dgm(),b=beta_f(), upper_c=NULL, lower_c=NULL, upper_p=NULL, lower_p=NULL)
      if (isTRUE(input$show_cloud)) {
        d <- cloud()
        p <- p |>
          plotly::add_trace(data = d, x = ~x, y = ~z, z = ~y,
                            type = "scatter3d", mode = "markers",
                            marker = list(size = 3, color = "#3E424B", opacity = 0.5),
                            name = "Full model", showlegend = FALSE)
      }
      p
    })

    output$applied_surface <- plotly::renderPlotly({
      p<-render_panel(c_surface_applied(), p_surface_applied(),b=b_f(),upper_c=NULL, lower_c=NULL, upper_p=NULL, lower_p=NULL)
      if (isTRUE(input$show_cloud)) {
        d <- cloud()
        p <- p |>
          plotly::add_trace(data = d, x = ~xs, y = ~zs, z = ~y,
                            type = "scatter3d", mode = "markers",
                            marker = list(size = 3, color = "#3E424B", opacity = 0.5),
                            name = "Full model", showlegend = FALSE)
      }
      p
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
      equation_text(b=beta_f(), eval_x=input$eval_x, eval_z=input$eval_z)
    })
    output$model_equation_applied <- renderText({
      req(!matrix_has_error())
      equation_text(b=b_f(), eval_x=input$eval_x, eval_z=input$eval_z)

    })
    output$model_equation_estimated <- renderText({

      equation_text(b=E_b_f(), eval_x=input$eval_x, eval_z=input$eval_z)

    })
    return(list(x_seq = x_seq, z_seq = z_seq))
  })
}

## To be copied in the UI
# mod_surface_vis_ui("surface_vis_1")

## To be copied in the server
# mod_surface_vis_server("surface_vis_1")
