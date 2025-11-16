

p_load(shinymanager)

app_auth_ui <- function() {
  auth_ui(
    id = "auth",
    lan = use_language("en"),
    status = "primary",
    tags_top = tags$div(
      singleton(tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "app_auth/app_auth.css"
        )
      )),
      tags$h2(
        class = "text-center",
        tags$b("WELCOME TO SSAT")
      )
    ),
    tags_bottom = tags$div(
      tags$p(
        "For any question or to request a username/password, please contact the ",
        tags$a(
          href = "mailto:brian.collins@unisq.edu.au?Subject=From%20SSAT%20App",
          target = "_top",
          "admin"
        )
      )
    ),
  )
}
