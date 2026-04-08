build_3d_plot <- function(input, lamp_df, field_points, particle_traj) {
  p <- plotly::plot_ly(hoverinfo = "none", hovertemplate = NULL)

  # ------------------------------------------------------------
  # Корпус реактора
  # ------------------------------------------------------------
# ------------------------------------------------------------
# Корпус реактора
# ------------------------------------------------------------
if (isTRUE(input$show_surface)) {
  cyl <- make_cylinder_surface(
    radius = input$reactor_radius,
    length_z = input$reactor_length
  )

  theta_vals <- sort(unique(cyl$theta))
  z_vals <- sort(unique(cyl$z))

  x_mat <- outer(rep(1, length(z_vals)), cos(theta_vals)) * input$reactor_radius
  y_mat <- outer(rep(1, length(z_vals)), sin(theta_vals)) * input$reactor_radius
  z_mat <- outer(z_vals, rep(1, length(theta_vals)))

  surface_col <- matrix(1, nrow = length(z_vals), ncol = length(theta_vals))

  p <- p %>%
    plotly::add_surface(
      x = x_mat,
      y = y_mat,
      z = z_mat,
      surfacecolor = surface_col,
      colorscale = list(
        c(0, "#9a9a9a"),
        c(1, "#9a9a9a")
      ),
      cmin = 0,
      cmax = 1,
      opacity = 0.12,
      showscale = FALSE,
      hoverinfo = "none"
    )
}
  # ------------------------------------------------------------
  # Поле интенсивности
  # ------------------------------------------------------------
  if (isTRUE(input$show_field) && !is.null(field_points) && nrow(field_points) > 0) {
    p <- p %>%
      plotly::add_markers(
  data = field_points,
  x = ~x, y = ~y, z = ~z,
  marker = list(
    size = 2,
    opacity = 0.18,
    color = field_points$intensity,
    colorscale = list(
      c(0.00, "#1b0c41"),
      c(0.25, "#3b528b"),
      c(0.50, "#21918c"),
      c(0.75, "#5ec962"),
      c(1.00, "#fde725")
    ),
    cmin = 0,
    cmax = input$color_max,
    colorbar = list(title = "Intensity")
  ),
  name = "Поле",
  hoverinfo = "none"
)
  }

  # ------------------------------------------------------------
  # Лампы
  # ------------------------------------------------------------

lamp_length_eff <- min(input$lamp_length, input$reactor_length)
lamp_z_top <- input$reactor_length
lamp_z_bottom <- input$reactor_length - lamp_length_eff

if (!is.null(lamp_df) && nrow(lamp_df) > 0) {
  for (i in seq_len(nrow(lamp_df))) {
    p <- p %>%
      plotly::add_trace(
  type = "scatter3d",
  mode = "lines",
  x = c(lamp_df$x[i], lamp_df$x[i]),
  y = c(lamp_df$y[i], lamp_df$y[i]),
  z = c(lamp_z_bottom, lamp_z_top),
  line = list(
    width = 8 + input$lamp_diameter * 2,
    color = "gold"
  ),
  name = paste("Лампа", i),
  showlegend = FALSE,
  hoverinfo = "none"
)
  }
}
  # ------------------------------------------------------------
  # Срезы в 3D
  # ------------------------------------------------------------
  if (isTRUE(input$show_slices_3d)) {

  xy_mid <- make_xy_slice(
  z0 = input$reactor_length * 0.95,
  radius = input$reactor_radius,
  lamp_df = lamp_df,
  power = input$lamp_power,
  mu = input$mu,
  lamp_length = input$lamp_length,
  reactor_length = input$reactor_length,
  lamp_diameter = input$lamp_diameter,
  n = 100
)

    p <- p %>%
      plotly::add_trace(
        data = xy_mid,
        type = "scatter3d",
        mode = "markers",
        x = ~x,
        y = ~y,
        z = ~z,
        marker = list(
          size = 4,
          opacity = 0.65,
          color = xy_mid$intensity,
          colorscale = list(
            c(0.00, "#1b0c41"),
            c(0.25, "#3b528b"),
            c(0.50, "#21918c"),
            c(0.75, "#5ec962"),
            c(1.00, "#fde725")
          ),
          cmin = 0,
          cmax = input$color_max
        ),
        showlegend = FALSE,
        hoverinfo = "none"
      )

xz_mid <- make_xz_slice(
  y0 = 0,
  radius = input$reactor_radius,
  length_z = input$reactor_length,
  lamp_df = lamp_df,
  power = input$lamp_power,
  mu = input$mu,
  lamp_length = input$lamp_length,
  reactor_length = input$reactor_length,
  lamp_diameter = input$lamp_diameter,
  nx = 100,
  nz = 600
)

    p <- p %>%
      plotly::add_trace(
        data = xz_mid,
        type = "scatter3d",
        mode = "markers",
        x = ~x,
        y = ~y,
        z = ~z,
        marker = list(
          size = 4,
          opacity = 0.55,
          color = xz_mid$intensity,
          colorscale = list(
            c(0.00, "#1b0c41"),
            c(0.25, "#3b528b"),
            c(0.50, "#21918c"),
            c(0.75, "#5ec962"),
            c(1.00, "#fde725")
          ),
          cmin = 0,
          cmax = input$color_max
        ),
        showlegend = FALSE,
        hoverinfo = "none"
      )
  }

  # ------------------------------------------------------------
  # Частицы / траектории
  # ------------------------------------------------------------
  
  if (isTRUE(input$show_particles) && !is.null(particle_traj) && nrow(particle_traj) > 0) {
    ids <- unique(particle_traj$id)

    for (idv in ids) {
      one <- particle_traj %>% dplyr::filter(id == idv)

      p <- p %>%
        plotly::add_trace(
          type = "scatter3d",
          mode = "lines",
          x = one$x,
          y = one$y,
          z = one$z,
          line = list(width = 5, color = "white"),
          opacity = 0.45,
          showlegend = FALSE,
          hoverinfo = "none"
        )
    }

    p <- p %>%
      plotly::add_trace(
        type = "scatter3d",
        mode = "markers",
        x = particle_traj$x,
        y = particle_traj$y,
        z = particle_traj$z,
        marker = list(
          size = 2.5,
          color = "white",
          opacity = 0.55
        ),
        showlegend = FALSE,
        hoverinfo = "none"
      )
  }

  # ------------------------------------------------------------
  # Layout
  # ------------------------------------------------------------
  p %>%
    plotly::layout(
  hovermode = FALSE,
      paper_bgcolor = "#bdbdbd",
      plot_bgcolor  = "#bdbdbd",
      dragmode = "turntable",
      scene = list(
        bgcolor = "#bdbdbd",
        xaxis = list(
          title = "X",
          backgroundcolor = "#bdbdbd",
          gridcolor = "#8f8f8f",
          zerolinecolor = "#7a7a7a",
          showspikes = FALSE
        ),
        yaxis = list(
          title = "Y",
          backgroundcolor = "#bdbdbd",
          gridcolor = "#8f8f8f",
          zerolinecolor = "#7a7a7a",
          showspikes = FALSE
        ),
        zaxis = list(
          title = "Z",
          backgroundcolor = "#bdbdbd",
          gridcolor = "#8f8f8f",
          zerolinecolor = "#8f8f8f",
          showspikes = FALSE
        ),
        aspectmode = "data",
        camera = list(
          eye = list(x = 4, y = 1, z = 0),
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = 0),
          projection = list(type = "perspective")
        )
      ),
      margin = list(l = 0, r = 0, b = 0, t = 0)
    )
}