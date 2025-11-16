
version <- 'V1'

# -----------------------------------------------------------------------------

if (!require(pacman)) install.packages("pacman", repos="https://cran.ms.unimelb.edu.au/")
library(pacman)

p_load(data.table, dplyr, shinymanager, shinybrowser)

# -----------------------------------------------------------------------------

source("fct_utils.R")
source("mod_help.R")
source("mod_footer.R")
source("mod_header.R")
source("mod_auth.R")
source("mod_paddocks.R")

# -----------------------------------------------------------------------------

# key_set("R-shinymanager-key", "brianx")
#
# create_db(
#   credentials_data = credentials,
#   sqlite_path = "DB/users.sqlite", # will be created
#   passphrase = key_get("R-shinymanager-key", "brianx")
# )

# -----------------------------------------------------------------------------

function(input, output, session) {
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(credentials)
  )

  get_user_role <- reactive({
    if (!auth$result) {
      return(FALSE)
    }
    # Look up the user in the database to load all the associated data.
    user <- as.data.frame(filter(credentials, user == auth$user))[1, ]
    user$role
  })

  output$app <- renderUI({
    user_role <- get_user_role()

    if (user_role != FALSE) {
      paddocks_server(input, output, session, user_role, version)
      help_server()
      list(header_ui(version), paddocks_ui(), tags$div(class = "container-fluid", footer_ui()))

    } else {
      fluidPage(title="SSAT", app_auth_ui())
    }
  })
}


