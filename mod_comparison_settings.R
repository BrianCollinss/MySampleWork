

p_load(plotly, shinyjs)

# -----------------------------------------------------------------------------

id_comparison_settings_cropsyst   <- id_paddock_settings_cropsyst
id_comparison_settings_cultivar   <- id_paddock_settings_cultivar
id_comparison_settings_sowdate    <- id_paddock_settings_sowdate
id_comparison_settings_fert_amt   <- id_paddock_settings_fert_amt
id_comparison_settings_population <- id_paddock_settings_population
id_comparison_settings_autoirrig  <- id_paddock_settings_autoirrig

  Factors <- c(id_paddock_settings_cultivar, id_paddock_settings_sowdate, id_paddock_settings_fert_amt,
               id_paddock_settings_initsw, id_paddock_settings_population, id_paddock_settings_autoirrig)

# -----------------------------------------------------------------------------

generate_comparison_settings_id <- function(paddock) {
  paste("comparison_setting", paddock, sep = "_")
}

generate_comparison_settings_ns <- function(paddock) {
  NS(generate_comparison_settings_id(paddock))
}

# -----------------------------------------------------------------------------

comparison_settings_server <- function(paddock, paddock_settings) {
  moduleServer(
    generate_comparison_settings_id(paddock),

    function(input, output, server) {
      reactive({
        settings <- list()
        comparison_setting_ids <- c(id_comparison_settings_cultivar, id_comparison_settings_population,
                                    id_comparison_settings_fert_amt, id_comparison_settings_autoirrig)

        for (p in comparison_setting_ids) {
          settings[[id_scenario_current]][[p]] <- paddock_settings[[p]]
          settings[[id_scenario_new]][[p]]     <- input[[p]]
        }
        settings
      })
    }
  )
}


