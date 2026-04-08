ui <- fluidPage(
  titlePanel("UV Reactor Visual Lab"),

  tags$head(
    tags$style(HTML("
      .well {
        padding: 10px 12px;
      }
      .control-label {
        font-size: 13px;
        margin-bottom: 4px;
      }
      .irs--shiny {
        margin-bottom: 12px;
      }
      .form-group {
        margin-bottom: 10px;
      }
    "))
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      checkboxInput("show_slices_3d", "Показать срезы в 3D", FALSE),

      sliderInput(
        "color_max",
        "Максимум цветовой шкалы",
        min = 1, max = 100, value = 100, step = 1
      ),

sliderInput(
  "top_slice_pos",
  "Позиция верхнего среза (%)",
  min = 80, max = 100, value = 95, step = 1
),


      sliderInput(
        "reactor_length",
        "Длина реактора, см",
        min = 4, max = 200, value = 100, step = 1
      ),

      sliderInput(
        "reactor_radius",
        "Радиус реактора, см",
        min = 0.4, max = 30, value = 12, step = 0.1
      ),

sliderInput(
  "lamp_length",
  "Длина лампы, см",
  min = 1, max = 200, value = 80, step = 1
),

sliderInput(
  "lamp_diameter",
  "Диаметр лампы, см",
  min = 0.5, max = 10, value = 2.5, step = 0.1
),

      sliderInput(
        "n_lamps",
        "Количество ламп",
        min = 1, max = 12, value = 4, step = 1
      ),

      sliderInput(
        "lamp_power",
        "Условная мощность лампы",
        min = 0.5, max = 10, value = 3, step = 0.5
      ),

      sliderInput(
        "mu",
        "Ослабление в воде",
        min = 0.0, max = 2.0, value = 0.25, step = 0.05
      ),

      sliderInput(
        "swirl",
        "Закрутка потока",
        min = 0, max = 3, value = 0.8, step = 0.1
      ),

      sliderInput(
        "n_points",
        "Точек поля",
        min = 500, max = 5000, value = 1000, step = 100
      ),

      sliderInput(
        "n_particles",
        "Частиц потока",
        min = 5, max = 80, value = 5, step = 1
      ),

      checkboxInput("show_surface", "Показать корпус реактора", TRUE),
      checkboxInput("show_field", "Показать поле интенсивности", TRUE),
      checkboxInput("show_particles", "Показать траектории", FALSE)
    ),

    mainPanel(
      width = 9,
      fluidRow(
        column(
          width = 6,
          plotlyOutput("plot3d", height = "750px")
        ),
        column(
          width = 6,
          fluidRow(
            column(
              width = 12,
              plotlyOutput("topSlice", height = "370px")
            )
          ),
          fluidRow(
            column(
              width = 12,
              plotlyOutput("longSlice", height = "370px")
            )
          )
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 12,
          tableOutput("summaryTable")
        )
      )
    )
  )
)