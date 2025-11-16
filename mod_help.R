

id_help <- "help"
id_help_main <- "main"
id_help_sugar_price <- "sugar_price"
id_help_rel_ccs <- "rel_ccs"
id_help_cane_price_const <- "cane_price_const"
id_help_planting <- "planting"
id_help_tillage <- "tillage"
id_help_burn <- "burn"
id_help_n_app <- "n_app"
id_help_a_irrig_freqg <- "a_irrig_freqg"
id_help_b_irrig_freqg <- "b_irrig_freqg"
id_help_dry_off <- "dry_off"
id_help_fallow <- "fallow"
id_help_change_thresholds <- "change_thresholds"

framework_design_pdf <- "SRAT Framework Design.pdf#page=%s&toolbar=0&view=FitH"
pricing_guide_pdf <- "MSF-Sugar-Cane-Pricing-Guide-2016.pdf#page=%s&toolbar=0&view=FitH"

skip_page <- 1

# -----------------------------------------------------------------------------

help_items <- list()

help_items[[id_help_main]] <- list(
  title = "SRAT",
  source = framework_design_pdf,
  page = 1
)
help_items[[id_help_planting]] <- list(
  title = "Population Scenarios",
  source = framework_design_pdf,
  page = 10 + skip_page
)
help_items[[id_help_tillage]] <- list(
  title = "Tillage Scenarios",
  source = framework_design_pdf,
  page = 11 + skip_page
)
help_items[[id_help_burn]] <- list(
  title = "Residue Burning",
  source = framework_design_pdf,
  page = 13 + skip_page
)
help_items[[id_help_n_app]] <- list(
  title = "N Application",
  source = framework_design_pdf,
  page = 16 + skip_page
)
help_items[[id_help_a_irrig_freqg]] <- list(
  title = "Irrigation Frequency Scenarios",
  source = framework_design_pdf,
  page = 15 + skip_page
)
help_items[[id_help_b_irrig_freqg]] <- list(
  title = "Irrigation Frequency Scenarios",
  source = framework_design_pdf,
  page = 15 + skip_page
)
help_items[[id_help_dry_off]] <- list(
  title = "Dry-off Period",
  source = framework_design_pdf,
  page = 15 + skip_page
)
help_items[[id_help_fallow]] <- list(
  title = "Fallow",
  source = framework_design_pdf,
  page = 13 + skip_page
)
help_items[[id_help_change_thresholds]] <- list(
  title = "Change Thresholds",
  source = framework_design_pdf,
  page = 21 + skip_page
)
help_items[[id_help_sugar_price]] <- list(
  title = "Sugar Price",
  source = pricing_guide_pdf,
  page = 5
)
help_items[[id_help_rel_ccs]] <- list(
  title = "Relative Commercial Cane Sugar",
  source = pricing_guide_pdf,
  page = 5
)
help_items[[id_help_cane_price_const]] <- list(
  title = "Cane Price Constant",
  source = pricing_guide_pdf,
  page = 5
)

# -----------------------------------------------------------------------------

help_ui <- function(id, ...) {
  return('')
  ns <- NS(id_help)
  actionLink(
    inputId = ns(id),
    ...
  )
}

# -----------------------------------------------------------------------------

help_server <- function() {
  moduleServer(
    id_help,
    function(input, output, session) {
      lapply(names(help_items), function(id) {
        item <- help_items[[id]]
        observeEvent(input[[id]], {
          showModal(modalDialog(
            title = item$title,
            easyClose = T,
            tags$iframe(
              style = "height:60vh; width:100%; scrolling=yes",
              src = sprintf(item$source, item$page)
            ),
            size = "l"
          ))
        })
      })
    }
  )
}
