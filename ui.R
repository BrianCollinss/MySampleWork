

if (!require(pacman)) install.packages("pacman", repos="https://cran.ms.unimelb.edu.au/")
library(pacman)

p_load(shinyjs)

# -----------------------------------------------------------------------------

tags$div(
  useShinyjs(),
  singleton(tags$head(
    tags$title('Sesame Suitability Assessment Tool (SSAT)'),
    tags$link(
      rel="preconnect",
      href="https://fonts.googleapis.com"
    ),
    tags$link(
      rel="preconnect",
      href="https://fonts.gstatic.com",
      crossorigin="anonymous"
    ),
    tags$link(
      rel="stylesheet",
      href="https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400;700&display=swap"
    ),
    tags$style(
      "body { font-family: 'Roboto Condensed', sans-serif !important; }"
    ),
    # @TODO: Remove this inline style when NavbarPage doesn't try to
    # render empty title as this could result in unexpected behaviour.
    tags$style(
      ".navbar-header { display: none !important }"
    )
  )),

  shinybrowser::detect(),
  uiOutput("app")
)



