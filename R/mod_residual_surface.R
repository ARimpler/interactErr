#' residual_surface UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_residual_surface_ui <- function(id) {
  ns <- NS(id)
  tagList(
         layout_sidebar(
           sidebar=sidebar(
           width = 220,
           div(checkboxInput(ns("show_cloud"), "Data Cloud", value = FALSE)),
           div(
             class = "d-flex align-items-end gap-2",
             numericInput(
               ns("n_cloud"),
               "Points",
               value = 300,
               min = 10,
               max = 3000
             ),
             actionButton(ns("draw"), "Run")
           )
         ),
         layout_columns(
           col_widths = breakpoints(sm = 12, lg = c(4,4,4)),
         div(card(
           card_header("Difference Y-\u0176"),
           card_body(padding=0,
                     plotly::plotlyOutput(ns("res_plot"), height = "100%"))))
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
  var pollC = setInterval(function() {
    checkForC();
    if (cBound) clearInterval(pollC);
  }, 500);
})();
", ns("res_plot"), ns("res_squared_plot"), ns("res_obs_plot"))))
  )
  )
}

#' residual_surface Server Functions
#'
#' @noRd
mod_residual_surface_server <- function(id, surface, dgm, beta_f, b_f, b_r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    matrix_has_error<-reactive({dgm$matrix_has_error()})
    x_seq<-reactive({surface$x_seq()})
    z_seq<-reactive({surface$z_seq()})


    r_full <- reactive({
      req(!matrix_has_error())
      residual_surface(beta_f(), b_f(), x_seq(), z_seq(), TRUE)
    })


    r_red <- reactive({
      req(!matrix_has_error())
      residual_surface(beta_f(), b_r(), x_seq(), z_seq(), FALSE)
    })
    lim <- reactive(c(min(c(r_full(), r_red()), na.rm = TRUE), max(c(r_full(), r_red()), na.rm = TRUE)))


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

    output$res_plot<-plotly::renderPlotly({
      p<-plotly::plot_ly() |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = r_full(), # #b8860b fdd525
                          opacity = 0.7, colorscale = list(c(0,"#b8860b"), c(1,"#b8860b")), showscale = FALSE,
                          contours = list(
                            x = list(show = TRUE, color = "black", width = 1),
                            y = list(show = TRUE, color = "black", width = 1)
                          )) |>
      plotly::add_surface(x = x_seq(), y = z_seq(), z = r_red(), #35b799
                          opacity = 0.7, colorscale = list(c(0,"#1b6b57"), c(1,"#1b6b57")), showscale = FALSE,
                          contours = list(
                            x = list(show = TRUE, color = "black", width = 1),
                            y = list(show = TRUE, color = "black", width = 1)
                          ))
        if (isTRUE(input$show_cloud) && input$draw > 0) {
          d <- cloud()
          p <- p |>
            plotly::add_trace(data = d, x = ~x, y = ~z, z = ~res_full,
                              type = "scatter3d", mode = "markers",
                              marker = list(size = 3, color = "#fdd525", opacity = 0.5),
                              name = "Full model", showlegend = FALSE) |>
            plotly::add_trace(data = d, x = ~x, y = ~z, z = ~res_red,
                              type = "scatter3d", mode = "markers",
                              marker = list(size = 3, color = "#35b799", opacity = 0.5),
                              name = "Reduced model", showlegend = FALSE)
        }
      p|> plotly::layout(scene = list(
                            xaxis = list(title = "X"), yaxis = list(title = "Z"),
                            zaxis = list(title = "Y-\u0176",
                                         range=lim()
                                         ),
                            aspectmode = "cube",
                            camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
                          ))
          })

  })
}

## To be copied in the UI
# mod_residual_surface_ui("residual_surface_1")

## To be copied in the server
# mod_residual_surface_server("residual_surface_1")
