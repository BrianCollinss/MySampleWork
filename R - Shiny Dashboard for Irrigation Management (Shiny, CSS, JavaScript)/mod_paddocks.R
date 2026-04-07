

p_load(shinyjs)

# -----------------------------------------------------------------------------

source("fct_data_provider.R")
source("fct_user.R")
source("fct_utils.R")
source("mod_paddock.R")

# -----------------------------------------------------------------------------

paddocks_ui <- function() {
  tags$section(
    tags$div(
      class = "container-fluid",
      tags$p(
        actionButton("addPaddockBtn", "New Analysis"),
        actionButton("remPaddockBtn", "Remove Analysis")
      )
    ),
    uiOutput("paddock_tabs")
  )
}

# -----------------------------------------------------------------------------

paddocks_server <- function(input, output, session, user_role, version) {
  paddocks <- reactiveVal(c(1))

  output$paddock_tabs <- renderUI({
    paddock_server(1, version)
    navbarPage(id="Paddocks", title="", position="static-top", paddock_ui(1, user_role, version))
  })

  # Add new paddock
  observeEvent(input$addPaddockBtn, {
    new_paddock <- max(paddocks()) + 1
    paddocks(c(paddocks(), new_paddock))
    paddock_server(new_paddock, version)
    appendTab(inputId="Paddocks", tab=paddock_ui(new_paddock, user_role, version), select=T)
  })

  # Remove last Pd
  observeEvent(input$remPaddockBtn, {
    selectedPaddock <- as.numeric(input$Paddocks)
    if (selectedPaddock!=1) {
      removeTab(inputId="Paddocks", target=input$Paddocks)
      paddocks(paddocks()[paddocks()!=selectedPaddock])
    }
  })

  # Disable remove' button when 'Paddock 1' is selected
  observeEvent(input$Paddocks, {
    selectedPaddock <- as.numeric(input$Paddocks)
    if (selectedPaddock==1) {
      shinyjs::disable("remPaddockBtn")
    } else {
      shinyjs::enable("remPaddockBtn")
    }
  })
}
