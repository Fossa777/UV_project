make_xy_slice <- function(
  z0, radius, lamp_df, power, mu,
  lamp_length, reactor_length, lamp_diameter,
  n = 120
) {
  xs <- seq(-radius, radius, length.out = n)
  ys <- seq(-radius, radius, length.out = n)
  g <- expand.grid(x = xs, y = ys)
  g <- g[g$x^2 + g$y^2 <= radius^2, ]
  g$z <- z0

  g$intensity <- calc_intensity(
    x = g$x,
    y = g$y,
    z = g$z,
    lamp_df = lamp_df,
    power = power,
    mu = mu,
    lamp_length = lamp_length,
    reactor_length = reactor_length,
    lamp_diameter = lamp_diameter
  )

  g
}

make_xz_slice <- function(
  y0, radius, length_z, lamp_df, power, mu,
  lamp_length, reactor_length, lamp_diameter,
  nx = 120, nz = 180
) {
  xs <- seq(-radius, radius, length.out = nx)
  zs <- seq(0, length_z, length.out = nz)
  g <- expand.grid(x = xs, z = zs)
  g$y <- y0
  g <- g[g$x^2 + g$y^2 <= radius^2, ]

  g$intensity <- calc_intensity(
    x = g$x,
    y = g$y,
    z = g$z,
    lamp_df = lamp_df,
    power = power,
    mu = mu,
    lamp_length = lamp_length,
    reactor_length = reactor_length,
    lamp_diameter = lamp_diameter
  )

  g
}