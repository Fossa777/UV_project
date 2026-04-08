make_lamp_positions <- function(n, reactor_radius, lamp_offset_ratio = 0.55) {
  n <- as.integer(n)

  if (is.na(n) || n < 1) {
    return(data.frame(x = numeric(0), y = numeric(0)))
  }

  # 1 лампа — в центре
  if (n == 1) {
    return(data.frame(x = 0, y = 0))
  }

  r <- reactor_radius * lamp_offset_ratio

  # 2 и 3 — по окружности
  if (n <= 3) {
    ang <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    return(data.frame(
      x = r * cos(ang),
      y = r * sin(ang)
    ))
  }

  # 4 и больше — одна в центре, остальные по окружности
  n_outer <- n - 1
  ang <- seq(0, 2 * pi, length.out = n_outer + 1)[-(n_outer + 1)]

  outer <- data.frame(
    x = r * cos(ang),
    y = r * sin(ang)
  )

  center <- data.frame(x = 0, y = 0)

  rbind(center, outer)
}

make_cylinder_surface <- function(radius, length_z, n_theta = 50, n_z = 30) {
  theta <- seq(0, 2*pi, length.out = n_theta)
  z <- seq(0, length_z, length.out = n_z)
  
  grid <- expand.grid(theta = theta, z = z)
  grid$x <- radius * cos(grid$theta)
  grid$y <- radius * sin(grid$theta)
  grid
}