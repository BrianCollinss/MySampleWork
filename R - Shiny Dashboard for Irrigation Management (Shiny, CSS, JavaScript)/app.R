

if (!require(pacman)) install.packages("pacman", repos="https://cran.ms.unimelb.edu.au/")
library(pacman)

p_load(shiny)

# -----------------------------------------------------------------------------

shinyApp(ui, server, enableBookmarking = "server")
