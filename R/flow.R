

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