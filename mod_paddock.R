

p_load(shinyjs)

# -----------------------------------------------------------------------------

source("mod_paddock_data_tabs.R")
source("mod_paddock_settings.R")
source("mod_paddock_summary.R")
source("mod_plot_selector.R")
source("mod_plots.R")
source("mod_summary_card.R")

# -----------------------------------------------------------------------------

paddock_ui <- function(paddock, user_role, version) {
  tabPanel(
    id=paste("paddock", paddock, sep="_"),
    value=paddock,
    title=tags$strong(class="myapp-paddock__title", paste("Analysis", paddock)),
    singleton(tags$head(
      tags$link(
        rel="stylesheet",
        type="text/css",
        href="paddock/paddock.css"
      )
    )),

    paddock_settings_ui(paddock, user_role, version),

    tags$div(class="myapp-paddock",
             tags$div(
               class="myapp-paddock__content",
               paddock_data_tabs_ui(paddock, user_role)
             )
    )
  )
}

# -----------------------------------------------------------------------------

paddock_server <- function(paddock, version) {
  current_tab <- paddock_data_tabs_server(paddock)

  paddock_settings <- paddock_settings_server(paddock, current_tab, version)

  plot_types <- plot_types_server(paddock)
  plot_type  <- plot_selector_server(paddock)

  data_x <- reactive(get_scenario_data(paddock_settings()))

  paddock_summary_server(paddock, paddock_settings, data_x)

  plots_server(paddock, paddock_settings, plot_types, plot_type, data_x)
}


