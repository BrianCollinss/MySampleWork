

source("mod_paddock_summary.R")

# -----------------------------------------------------------------------------

spinner_type <- 4
id_paddock_data_tabs <- "paddock_data_tabs"

generate_paddock_data_tabs_id <- function(paddock) {
  paste("paddock_data_tabs", paddock, sep="_")
}

# -----------------------------------------------------------------------------

paddock_data_tab_ui <- function(name, ...) {
  tabPanel(
    value=name,
    tags$strong(
      style="font-size:140%; color:FireBrick;",
      name
    ),
    ...
  )
}

# -----------------------------------------------------------------------------

paddock_data_tabs_ui <- function(paddock, user_role) {
  ns <- NS(generate_paddock_data_tabs_id(paddock))

  navbarPage("", id=ns("tabs"), selected='Visualisation',
             paddock_data_tab_ui(name="Summary", paddock_summary_ui(paddock, user_role, spinner_type)),
             paddock_data_tab_ui(name="Visualisation", plots_ui(paddock, user_role))
  )
}

paddock_data_tabs_server <- function(paddock) {
  moduleServer(
    generate_paddock_data_tabs_id(paddock),

    function(input, output, session) {
      observeEvent(once=T, input$tabs, {
        updateNavbarPage(session=session, inputId="tabs", selected='Summary')
      })

      reactive({
        if (is.null(input$tabs)) {
          return("Summary")
        } else {
          return(input$tabs)
        }
      })
    }
  )
}
