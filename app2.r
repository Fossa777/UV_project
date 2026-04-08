library(shiny)
library(plotly)
library(dplyr)

source("R/geometry.R")
source("R/physics.R")
source("R/flow.R")
source("R/slices.R")
source("R/viz_3d.R")
source("R/viz_slices.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)