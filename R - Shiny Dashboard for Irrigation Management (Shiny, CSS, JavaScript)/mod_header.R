

header_ui <- function(version) {
  tags$header(
    class = "myapp-header",
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "header/header.css"
      )
    ),
    tags$div(
      class = "container-fluid",
      tags$h1(
        class = "myapp-header__title",
		tags$b("SSAT", style="color: GreenYellow;")
        #tags$b(
        #  help_ui(id_help_main, class = "myapp-header__link", label = "SSAT")
        #)
      ),
      tags$p(
        class = "myapp-header__subtitle",
        tags$b(sprintf("Sesame Suitability Assessment Tool (%s)", version))
      )
    )
  )
}
