library(shiny)
library(plotly)
library(dplyr)

# ------------------------------------------------------------
# Вспомогательные функции
# ------------------------------------------------------------

# Координаты ламп внутри цилиндра
make_lamp_positions <- function(n, reactor_radius, lamp_offset_ratio = 0.55) {
  if (n <= 1) {
    return(data.frame(x = 0, y = 0))
  }
  
  r <- reactor_radius * lamp_offset_ratio
  ang <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  
  data.frame(
    x = r * cos(ang),
    y = r * sin(ang)
  )
}

# Интенсивность от линейной лампы, ориентированной вдоль оси z
# Упрощённая модель:
# I ~ P / (r^2 + eps) * exp(-mu * r)
# где r — поперечное расстояние до лампы
calc_intensity <- function(x, y, z, lamp_df, power = 1, mu = 0.1, eps = 0.01) {
  total <- rep(0, length(x))
  
  for (i in seq_len(nrow(lamp_df))) {
    dx <- x - lamp_df$x[i]
    dy <- y - lamp_df$y[i]
    r <- sqrt(dx^2 + dy^2)
    total <- total + power / (r^2 + eps) * exp(-mu * r)
  }
  
  total
}

# Случайные точки внутри цилиндра
sample_points_in_cylinder <- function(n, radius, length_z) {
  u <- runif(n)
  theta <- runif(n, 0, 2 * pi)
  rr <- radius * sqrt(u)
  
  data.frame(
    x = rr * cos(theta),
    y = rr * sin(theta),
    z = runif(n, 0, length_z)
  )
}

# Генерация траекторий частиц
# Базовый поток вдоль z + опциональная закрутка
make_particle_trajectories <- function(
    n_particles = 20,
    n_steps = 120,
    reactor_radius = 1,
    reactor_length = 10,
    swirl = 0,
    vz_base = 0.1
) {
  particles <- vector("list", n_particles)
  
  for (p in seq_len(n_particles)) {
    # Стартовая точка у входа
    theta0 <- runif(1, 0, 2*pi)
    r0 <- reactor_radius * sqrt(runif(1, 0, 0.9))
    x <- r0 * cos(theta0)
    y <- r0 * sin(theta0)
    z <- 0
    
    traj <- data.frame(
      step = 1,
      x = x,
      y = y,
      z = z,
      id = p
    )
    
    for (k in 2:n_steps) {
      r <- sqrt(x^2 + y^2)
      theta <- atan2(y, x)
      
      # скорость вдоль оси: чуть быстрее в центре, медленнее у стенки
      vz <- vz_base * (1 - 0.65 * (r / reactor_radius)^2)
      vz <- max(vz, vz_base * 0.2)
      
      # закрутка
      dtheta <- swirl * 0.08
      
      # слабое радиальное "перемешивание"
      dr <- rnorm(1, mean = 0, sd = 0.015 + 0.01 * swirl)
      
      theta <- theta + dtheta
      r <- r + dr
      
      # отражение от стенки
      if (r > reactor_radius * 0.96) {
        r <- reactor_radius * 0.96
      }
      if (r < 0) r <- abs(r)
      
      x <- r * cos(theta)
      y <- r * sin(theta)
      z <- z + vz
      
      if (z > reactor_length) break
      
      traj <- bind_rows(
        traj,
        data.frame(step = k, x = x, y = y, z = z, id = p)
      )
    }
    
    particles[[p]] <- traj
  }
  
  bind_rows(particles)
}

# Цилиндрическая поверхность для отображения корпуса
make_cylinder_surface <- function(radius, length_z, n_theta = 50, n_z = 30) {
  theta <- seq(0, 2*pi, length.out = n_theta)
  z <- seq(0, length_z, length.out = n_z)
  
  grid <- expand.grid(theta = theta, z = z)
  grid$x <- radius * cos(grid$theta)
  grid$y <- radius * sin(grid$theta)
  grid
}

make_xy_slice <- function(z0, radius, lamp_df, power, mu, n = 120) {
  xs <- seq(-radius, radius, length.out = n)
  ys <- seq(-radius, radius, length.out = n)
  g <- expand.grid(x = xs, y = ys)
  g <- g[g$x^2 + g$y^2 <= radius^2, ]
  g$z <- z0
  g$intensity <- calc_intensity(
    x = g$x, y = g$y, z = g$z,
    lamp_df = lamp_df,
    power = power,
    mu = mu
  )
  g
}

make_xz_slice <- function(y0, radius, length_z, lamp_df, power, mu, nx = 120, nz = 180) {
  xs <- seq(-radius, radius, length.out = nx)
  zs <- seq(0, length_z, length.out = nz)
  g <- expand.grid(x = xs, z = zs)
  g$y <- y0
  g <- g[g$x^2 + g$y^2 <= radius^2, ]
  g$intensity <- calc_intensity(
    x = g$x, y = g$y, z = g$z,
    lamp_df = lamp_df,
    power = power,
    mu = mu
  )
  g
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- fluidPage(
  titlePanel("UV Reactor Visual Lab — demo"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("color_max", "Максимум цветовой шкалы", min = 1, max = 100, value = 25, step = 1),
      sliderInput("reactor_length", "Длина реактора", min = 4, max = 20, value = 10, step = 1),
      sliderInput("reactor_radius", "Радиус реактора", min = 0.4, max = 3, value = 1.2, step = 0.1),
      sliderInput("n_lamps", "Количество ламп", min = 1, max = 8, value = 2, step = 1),
      sliderInput("lamp_power", "Условная мощность лампы", min = 0.5, max = 10, value = 3, step = 0.5),
      sliderInput("mu", "Ослабление в воде", min = 0.0, max = 2.0, value = 0.25, step = 0.05),
      sliderInput("swirl", "Закрутка потока", min = 0, max = 3, value = 0.8, step = 0.1),
      sliderInput("n_points", "Точек поля", min = 500, max = 5000, value = 1800, step = 100),
      sliderInput("n_particles", "Частиц потока", min = 5, max = 80, value = 25, step = 1),
      checkboxInput("show_surface", "Показать корпус реактора", TRUE),
      checkboxInput("show_field", "Показать поле интенсивности", TRUE),
      checkboxInput("show_particles", "Показать траектории", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("3D реактор", plotlyOutput("plot3d", height = "750px")),
        tabPanel("Центральный срез", plotlyOutput("slicePlot", height = "700px")),
        tabPanel("Сводка", tableOutput("summaryTable"))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  


  lamps <- reactive({
    make_lamp_positions(
      n = input$n_lamps,
      reactor_radius = input$reactor_radius
    )
  })
  
  field_points <- reactive({
    pts <- sample_points_in_cylinder(
      n = input$n_points,
      radius = input$reactor_radius,
      length_z = input$reactor_length
    )
    
    pts$intensity <- calc_intensity(
      x = pts$x, y = pts$y, z = pts$z,
      lamp_df = lamps(),
      power = input$lamp_power,
      mu = input$mu
    )
    pts
  })
  
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
        x = tr$x, y = tr$y, z = tr$z,
        lamp_df = lamps(),
        power = input$lamp_power,
        mu = input$mu
      )
      
      tr <- tr %>%
        group_by(id) %>%
        mutate(
          dz = c(diff(z), 0),
          dose_step = intensity * pmax(dz, 0),
          dose_cum = cumsum(dose_step)
        ) %>%
        ungroup()
    }
    
    tr
  })
  
  output$plot3d <- renderPlotly({
    p <- plot_ly()
    
    # Корпус реактора
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
      
      p <- p %>%
        add_surface(
          x = ~x_mat,
          y = ~y_mat,
          z = ~z_mat,
          opacity = 0.15,
          showscale = FALSE
        )
    }
    
    # Поле интенсивности
    if (isTRUE(input$show_field)) {
      fp <- field_points()
      
p <- p %>%
add_markers(
  data = fp,
  x = ~x, y = ~y, z = ~z,
  marker = list(
    size = 2,
    opacity = 0.18,
    color = ~intensity,
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
  name = "Поле"
)
    }
    
    # Лампы
    lamp_df <- lamps()
    for (i in seq_len(nrow(lamp_df))) {
      p <- p %>%
        add_trace(
          type = "scatter3d",
          mode = "lines",
          x = c(lamp_df$x[i], lamp_df$x[i]),
          y = c(lamp_df$y[i], lamp_df$y[i]),
          z = c(0, input$reactor_length),
          line = list(width = 12, color = "gold"),
          name = paste("Лампа", i),
          showlegend = FALSE
        )
    }
    
xy_mid <- make_xy_slice(
    z0 = input$reactor_length * 0.5,
    radius = input$reactor_radius,
    lamp_df = lamps(),
    power = input$lamp_power,
    mu = input$mu,
    n = 100
  )
  
  p <- p %>%
add_trace(
  data = xy_mid,
  type = "scatter3d",
  mode = "markers",
  x = ~x,
  y = ~y,
  z = ~z,
  marker = list(
    size = 3,
    opacity = 0.65,
    color = ~intensity,
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
  showlegend = FALSE
)
  
  xz_mid <- make_xz_slice(
    y0 = 0,
    radius = input$reactor_radius,
    length_z = input$reactor_length,
    lamp_df = lamps(),
    power = input$lamp_power,
    mu = input$mu,
    nx = 100,
    nz = 180
  )
  
  p <- p %>%
add_trace(
  data = xy_mid,
  type = "scatter3d",
  mode = "markers",
  x = ~x,
  y = ~y,
  z = ~z,
  marker = list(
    size = 3,
    opacity = 0.65,
    color = ~intensity,
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
  showlegend = FALSE
)
  



    # Частицы/траектории
if (isTRUE(input$show_particles)) {
  tr <- particle_traj()
  if (nrow(tr) > 0) {
    ids <- unique(tr$id)
    
    for (idv in ids) {
      one <- tr %>% filter(id == idv)
      
      # линия траектории
      p <- p %>%
        add_trace(
          type = "scatter3d",
          mode = "lines",
          x = one$x,
          y = one$y,
          z = one$z,
          line = list(width = 5, color = "white"),
          opacity = 0.45,
          showlegend = FALSE
        )
    }
    
    # точки поверх всех траекторий
    p <- p %>%
      add_trace(
        type = "scatter3d",
        mode = "markers",
        x = tr$x,
        y = tr$y,
        z = tr$z,
        marker = list(
          size = 2.5,
          color = "white",
          opacity = 0.55
        ),
        showlegend = FALSE
      )
  }
}
    
p %>%
  layout(
    paper_bgcolor = "#bdbdbd",
    plot_bgcolor  = "#bdbdbd",
    scene = list(
      bgcolor = "#bdbdbd",
      xaxis = list(
        title = "X",
        backgroundcolor = "#bdbdbd",
        gridcolor = "#8f8f8f",
        zerolinecolor = "#7a7a7a"
      ),
      yaxis = list(
        title = "Y",
        backgroundcolor = "#bdbdbd",
        gridcolor = "#8f8f8f",
        zerolinecolor = "#7a7a7a"
      ),
      zaxis = list(
        title = "Z",
        backgroundcolor = "#bdbdbd",
        gridcolor = "#8f8f8f",
        zerolinecolor = "#8f8f8f"
      ),
      aspectmode = "data",
      camera = list(
  eye = list(x = 0, y = 0, z = 2.5),
  up = list(x = 0, y = 1, z = 0),
  center = list(x = 0, y = 0, z = 0),
  projection = list(type = "orthographic")
)
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0)
  )
  })
  
  output$slicePlot <- renderPlotly({
    # Срез y ~ 0
    xs <- seq(-input$reactor_radius, input$reactor_radius, length.out = 140)
    zs <- seq(0, input$reactor_length, length.out = 220)
    grid <- expand.grid(x = xs, z = zs)
    grid$y <- 0
    
    # оставляем только точки внутри цилиндра
    grid <- grid %>% filter(x^2 + y^2 <= input$reactor_radius^2)
    
    grid$intensity <- calc_intensity(
      x = grid$x, y = grid$y, z = grid$z,
      lamp_df = lamps(),
      power = input$lamp_power,
      mu = input$mu
    )
    
    plot_ly(
      data = grid,
      x = ~z,
      y = ~x,
      z = ~intensity,
      zmin = 0,
  zmax = input$color_max,
      type = "heatmap",
      colors = colorRamp(c("#0b1f8a", "#00bcd4", "#7ad151", "#ffd54f", "#ff5722"))
    ) %>%
      layout(
        xaxis = list(title = "Длина реактора (z)"),
        yaxis = list(title = "Сечение (x)")
      )
  })
  
  output$summaryTable <- renderTable({
    fp <- field_points()
    tr <- particle_traj()
    
    traj_summary <- NULL
    if (nrow(tr) > 0) {
      traj_summary <- tr %>%
        group_by(id) %>%
        summarise(
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
      )
    )
  })
}

shinyApp(ui, server)