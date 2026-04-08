server <- function(input, output, session) {

  # ------------------------------------------------------------
  # REACTIVE: координаты ламп
  # ------------------------------------------------------------
  lamps <- reactive({
    make_lamp_positions(
      n = as.integer(input$n_lamps),
      reactor_radius = input$reactor_radius
    )
  })

  # ------------------------------------------------------------
  # REACTIVE: облако точек поля
  # ------------------------------------------------------------
  field_points <- reactive({
    pts <- sample_points_in_cylinder(
      n = input$n_points,
      radius = input$reactor_radius,
      length_z = input$reactor_length
    )

pts$intensity <- calc_intensity(
  x = pts$x,
  y = pts$y,
  z = pts$z,
  lamp_df = lamps(),
  power = input$lamp_power,
  mu = input$mu,
  lamp_length = input$lamp_length,
  reactor_length = input$reactor_length,
  lamp_diameter = input$lamp_diameter
)

    pts
  })

  # ------------------------------------------------------------
  # REACTIVE: траектории частиц
  # ------------------------------------------------------------
  particle_traj <- reactive({
    tr <- make_particle_trajectories(
      n_particles = input$n_particles,
      n_steps = 160,
      reactor_radius = input$reactor_radius,
      reactor_length = input$reactor_length,
      swirl = input$swirl,
      vz_base = input$reactor_length / 80
    )

    if (nrow(tr) > 0) {
   tr$intensity <- calc_intensity(
  x = tr$x,
  y = tr$y,
  z = tr$z,
  lamp_df = lamps(),
  power = input$lamp_power,
  mu = input$mu,
  lamp_length = input$lamp_length,
  reactor_length = input$reactor_length,
  lamp_diameter = input$lamp_diameter
)

      tr <- tr %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(
          dz = c(diff(z), 0),
          dose_step = intensity * pmax(dz, 0),
          dose_cum = cumsum(dose_step)
        ) %>%
        dplyr::ungroup()
    }

    tr
  })

  # ------------------------------------------------------------
  # REACTIVE: верхний срез (XY)
  # ------------------------------------------------------------
top_slice_data <- reactive({
  make_xy_slice(
    z0 = input$reactor_length * (input$top_slice_pos / 100),
    radius = input$reactor_radius,
    lamp_df = lamps(),
    power = input$lamp_power,
    mu = input$mu,
    lamp_length = input$lamp_length,
    reactor_length = input$reactor_length,
    lamp_diameter = input$lamp_diameter,
    n = 150
  )
})

  # ------------------------------------------------------------
  # REACTIVE: продольный срез (XZ)
  # ------------------------------------------------------------
long_slice_data <- reactive({
  make_xz_slice(
    y0 = 0,
    radius = input$reactor_radius,
    length_z = input$reactor_length,
    lamp_df = lamps(),
    power = input$lamp_power,
    mu = input$mu,
    lamp_length = input$lamp_length,
    reactor_length = input$reactor_length,
    lamp_diameter = input$lamp_diameter,
    nx = 140,
    nz = 220
  )
})
  # ------------------------------------------------------------
  # OUTPUT: 3D
  # ------------------------------------------------------------
  output$plot3d <- renderPlotly({
    build_3d_plot(
      input = input,
      lamp_df = lamps(),
      field_points = field_points(),
      particle_traj = particle_traj()
    )
  })

  # ------------------------------------------------------------
  # OUTPUT: верхний срез
  # ------------------------------------------------------------
  output$topSlice <- renderPlotly({
    build_top_slice(
      xy = top_slice_data(),
      color_max = input$color_max
    )
  })

  # ------------------------------------------------------------
  # OUTPUT: продольный срез
  # ------------------------------------------------------------
  output$longSlice <- renderPlotly({
    build_long_slice(
      xz = long_slice_data(),
      color_max = input$color_max
    )
  })

  # ------------------------------------------------------------
  # OUTPUT: сводная таблица
  # ------------------------------------------------------------
  output$summaryTable <- renderTable({
    fp <- field_points()
    tr <- particle_traj()

    traj_summary <- NULL
    if (nrow(tr) > 0) {
      traj_summary <- tr %>%
        dplyr::group_by(id) %>%
        dplyr::summarise(
          final_dose = max(dose_cum, na.rm = TRUE),
          .groups = "drop"
        )
    }

    data.frame(
      Параметр = c(
        "Количество ламп",
        "Длина реактора",
        "Радиус реактора",
        "Средняя интенсивность в облаке",
        "Мин. интенсивность в облаке",
        "Макс. интенсивность в облаке",
        "Средняя финальная доза частиц",
        "Мин. финальная доза частиц",
        "Макс. финальная доза частиц"
      ),
      Значение = c(
        input$n_lamps,
        round(input$reactor_length, 3),
        round(input$reactor_radius, 3),
        round(mean(fp$intensity, na.rm = TRUE), 4),
        round(min(fp$intensity, na.rm = TRUE), 4),
        round(max(fp$intensity, na.rm = TRUE), 4),
        if (!is.null(traj_summary)) round(mean(traj_summary$final_dose, na.rm = TRUE), 4) else NA,
        if (!is.null(traj_summary)) round(min(traj_summary$final_dose, na.rm = TRUE), 4) else NA,
        if (!is.null(traj_summary)) round(max(traj_summary$final_dose, na.rm = TRUE), 4) else NA
      ),
      check.names = FALSE
    )
  })
}