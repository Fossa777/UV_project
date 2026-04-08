

calc_intensity <- function(
  x, y, z,
  lamp_df,
  power = 1,
  mu = 0.1,
  eps = 0.01,
  lamp_length,
  reactor_length,
  lamp_diameter = 0
) {
  total <- rep(0, length(x))

  lamp_length_eff <- min(lamp_length, reactor_length)
  lamp_z_top <- reactor_length
  lamp_z_bottom <- reactor_length - lamp_length_eff

  for (i in seq_len(nrow(lamp_df))) {
    dx <- x - lamp_df$x[i]
    dy <- y - lamp_df$y[i]

    # учитываем радиус лампы
    r_xy <- sqrt(dx^2 + dy^2)
    r_xy_eff <- pmax(r_xy - lamp_diameter / 2, 0)

    # по z: если точка вне длины лампы, штрафуем расстоянием до ближайшего конца
    dz <- ifelse(
      z < lamp_z_bottom, lamp_z_bottom - z,
      ifelse(z > lamp_z_top, z - lamp_z_top, 0)
    )

    r_eff <- sqrt(r_xy_eff^2 + dz^2)

    total <- total + power / (r_eff^2 + eps) * exp(-mu * r_eff)
  }

  total
}