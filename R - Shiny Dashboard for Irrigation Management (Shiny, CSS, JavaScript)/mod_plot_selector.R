

all_plot_types <- c(
  "Map" = "MapPlot1",
  "Box Plot" = "BoxPlot1",
  "Bar Plot" = "BarPlot1",
  "Probability" = "ProbExcPlot1",
  "Density" = "DensityPlot1"
)

# -----------------------------------------------------------------------------

id_plot_selector_plot_type <- "PlotType"

# -----------------------------------------------------------------------------

generate_plot_selector_id <- function(paddock) {
  paste("plot_selector", paddock, sep = "_")
}

# -----------------------------------------------------------------------------

plot_selector_ui <- function(paddock, user_role, plot_types) {
  ns <- NS(generate_plot_selector_id(paddock))

  radioButtons(inputId=ns(id_plot_selector_plot_type), inline=TRUE, label=NULL, choices=all_plot_types, selected=all_plot_types[1], width='100%')
}

# -----------------------------------------------------------------------------

plot_selector_server <- function(paddock) {
  moduleServer(
    generate_plot_selector_id(paddock),

    function(input, output, session) {
      reactive({input[[id_plot_selector_plot_type]]})
    }
  )
}



