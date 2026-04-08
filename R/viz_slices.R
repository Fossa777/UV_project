build_top_slice <- function(xy, color_max) {
  if (is.null(xy) || !is.data.frame(xy) || nrow(xy) == 0) {
    return(plotly::plot_ly())
  }

  if (!("intensity" %in% names(xy))) {
    return(plotly::plot_ly())
  }

  inten <- xy[["intensity"]]

  if (length(inten) != nrow(xy)) {
    return(plotly::plot_ly())
  }

  weak_threshold <- 0.2 * max(inten, na.rm = TRUE)

  weak_df <- xy
  weak_df[["weak"]] <- as.numeric(inten < weak_threshold)
  weak_df[!is.finite(inten), "weak"] <- NA_real_

  p <- plotly::plot_ly(
    data = xy,
    x = ~x,
    y = ~y,
    z = ~intensity,
    type = "heatmap",
    hoverinfo = "none",
    zmin = 0,
    zmax = color_max,
    colors = colorRamp(c("#1b0c41", "#3b528b", "#21918c", "#5ec962", "#fde725"))
  )

  p <- p %>%
    plotly::add_trace(
      data = weak_df,
      x = ~x,
      y = ~y,
      z = ~weak,
      type = "heatmap",
      showscale = FALSE,
      zmin = 0,
      zmax = 1,
      hoverinfo = "none",
      colors = colorRamp(c(
        rgb(255, 0, 0, alpha = 0.22 * 255, maxColorValue = 255),
        rgb(255, 0, 0, alpha = 0.22 * 255, maxColorValue = 255)
      ))
    )

  p %>%
    plotly::layout(
      title = "Срез сверху",
      xaxis = list(
        title = "X",
        scaleanchor = "y",
        scaleratio = 1,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "Y",
        fixedrange = TRUE
      ),
      hovermode = FALSE
    ) %>%
    plotly::config(
      displayModeBar = FALSE,
      staticPlot = TRUE,
      scrollZoom = FALSE
    )
}

build_long_slice <- function(xz, color_max) {
  if (is.null(xz) || !is.data.frame(xz) || nrow(xz) == 0) {
    return(plotly::plot_ly())
  }

  if (!("intensity" %in% names(xz))) {
    return(plotly::plot_ly())
  }

  inten <- xz[["intensity"]]

  if (length(inten) != nrow(xz)) {
    return(plotly::plot_ly())
  }

  weak_threshold <- 0.2 * max(inten, na.rm = TRUE)

  weak_df <- xz
  weak_df[["weak"]] <- as.numeric(inten < weak_threshold)
  weak_df[!is.finite(inten), "weak"] <- NA_real_

  p <- plotly::plot_ly(
    data = xz,
    x = ~z,
    y = ~x,
    z = ~intensity,
    type = "heatmap",
    hoverinfo = "none",
    zmin = 0,
    zmax = color_max,
    colors = colorRamp(c("#1b0c41", "#3b528b", "#21918c", "#5ec962", "#fde725"))
  )

  p <- p %>%
    plotly::add_trace(
      data = weak_df,
      x = ~z,
      y = ~x,
      z = ~weak,
      type = "heatmap",
      showscale = FALSE,
      zmin = 0,
      zmax = 1,
      hoverinfo = "none",
      colors = colorRamp(c(
        rgb(255, 0, 0, alpha = 0.22 * 255, maxColorValue = 255),
        rgb(255, 0, 0, alpha = 0.22 * 255, maxColorValue = 255)
      ))
    )

  p %>%
    plotly::layout(
      title = "Продольный срез",
      xaxis = list(
        title = "Длина (z)",
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "Сечение (x)",
        fixedrange = TRUE
      ),
      hovermode = FALSE
    ) %>%
    plotly::config(
      displayModeBar = FALSE,
      staticPlot = TRUE,
      scrollZoom = FALSE
    )
}