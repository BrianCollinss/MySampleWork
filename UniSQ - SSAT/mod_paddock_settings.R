

p_load(shinyBS, shinyjs, RANN)

# -----------------------------------------------------------------------------

id_paddock_settings_version <- "Version"

id_paddock_settings_cropsyst   <- "CropSyst"
id_paddock_settings_cultivar   <- "Cultivar"
id_paddock_settings_sowdate    <- "SowDate"
id_paddock_settings_fert_amt   <- "NO3NInit"
id_paddock_settings_initsw     <- "InitSW"
id_paddock_settings_population <- "Population"
id_paddock_settings_autoirrig  <- "AutoIrrigate"
id_paddock_settings_analysis   <- "AnalysisType"
id_paddock_settings_state      <- "State"
id_paddock_settings_coords     <- "Coordinates"
id_paddock_settings_distance   <- "PointDistance"
id_paddock_settings_getcoords  <- "GetCoordinates"
id_paddock_settings_ispoint    <- 'IsPointAnalysis'

id_paddock_settings_sow_years <- "SowYears"
id_paddock_settings_harv_year <- 'HarvestYear'
id_paddock_settings_padd_size <- "PaddSize"
id_paddock_settings_surf_energy_used <- "SurfEnergyUsed"
id_paddock_settings_drip_energy_used <- "DripEnergyUsed"
id_paddock_settings_water_cost <- "WaterCost"
id_paddock_settings_energy_cost <- "EnergyCost"
id_paddock_settings_surface_op_cost <- "SurfaceOpCost"
id_paddock_settings_drip_op_cost <- "DripOpCost"
id_paddock_settings_fuel <- "Fuel"
id_paddock_settings_harvest_loss <- "HarvestLoss"
id_paddock_settings_crop_price <- "SugarPrice"

# -----------------------------------------------------------------------------
# Factors

Factors <- c(id_paddock_settings_cultivar, id_paddock_settings_sowdate, id_paddock_settings_fert_amt,
             id_paddock_settings_initsw, id_paddock_settings_population, id_paddock_settings_autoirrig)

# -----------------------------------------------------------------------------
# Initial selections

cropsysts <- c("Sesame monoculture" = "Sesame")

cultivars <- c("Fast-growing" = "Fast", "Slow-growing" = "Slow")

sowdates <- c('15-Jan', '14-Feb', '15-Mar', '15-Apr', '15-May', '15-Jun',
              '15-Jul', '15-Aug', '15-Sep', '15-Oct', '15-Nov', '15-Dec')

fert_amts <- c(40, 200)

initsws <- c(0, 40, 80)

populations <- c(25, 50, 75)

autoirrigs <- c("Rainfed"=FALSE, "Auto-Irrigated"=TRUE)

analyses <- c('Spatial Analysis', 'Point Analysis')

states <- c('Australia', 'Northern Territory', 'Queensland', 'New South Wales', 'Victoria', 'South Australia', 'Tasmania')

fuels_ef <- c(
  "Brown Coal (Lignite)" = 101000,
  "Coking Coal" = 94600,
  "Anthracite (Hard Coal)" = 98300,
  "Crude Oil" = 73300,
  "Motor Gasoline" = 69300,
  "Gas/Diesel Oil" = 74100,
  "Natural Gas" = 15600
) #IPCC (2006), V2_1_Ch1, TABLE 1.4, in kg/TJ

# -----------------------------------------------------------------------------

generate_paddock_settings_id <- function(paddock) {
  paste("paddock_settings", paddock, sep = "_")
}

# -----------------------------------------------------------------------------

generate_paddock_settings_ns <- function(paddock) {
  NS(generate_paddock_settings_id(paddock))
}

# -----------------------------------------------------------------------------

paddock_setting_ui <- function(..., class='myapp-paddock-settings__paddock') {
  tags$div(class=sprintf("col-sm-6 col-md-4 col-lg-3 %s", class), ...)
}

# -----------------------------------------------------------------------------

full_width_input <- function(input, ...) input(..., width = "100%")
half_width_input <- function(input, ...) input(..., width = "50%")

# -----------------------------------------------------------------------------

paddock_settings_ui <- function(paddock, user_role, version) {
  ns <- generate_paddock_settings_ns(paddock)

  tags$div(
    class = "myapp-paddock-settings",
    singleton(tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "paddock_settings/paddock_settings.css"
      )
    )),

    wellPanel(
      tags$h3(class="myapp-paddock-settings__title", "Settings"),
      tags$hr(.noWS='before-end'),

      tags$div(class="row",
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_cropsyst),
                   label = HTML(
                     "Cropping system",
                     as.character(help_ui(id_help_fallow, label = "(?)"))
                   ),
                   choices = cropsysts,
                   selectize = F,
                   selected = cropsysts[1]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_cultivar),
                   label = HTML(
                     "Cultivar",
                     as.character(help_ui(id_help_fallow, label = "(?)"))
                   ),
                   choices = cultivars,
                   selectize = F,
                   selected = cultivars[1]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_sowdate),
                   label = HTML(
                     "Sowing date",
                     as.character(help_ui(id_help_fallow, label = "(?)"))
                   ),
                   choices = sowdates,
                   selectize = F,
                   selected = sowdates[12]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_fert_amt),
                   label = HTML(
                     "N applied (kg/ha)",
                     as.character(help_ui(id_help_n_app, label = "(?)"))
                   ),
                   choices = fert_amts,
                   selectize = F,
                   selected = fert_amts[2]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_initsw),
                   label = HTML(
                     "Initial soil moisture (%)",
                     as.character(help_ui(id_help_n_app, label = "(?)"))
                   ),
                   choices = initsws,
                   selectize = F,
                   selected = initsws[2]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_population),
                   label = HTML(
                     "Population (plants/m2)",
                     as.character(help_ui(id_help_planting, label = "(?)"))
                   ),
                   choices = populations,
                   selectize = F,
                   selected = populations[2]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_autoirrig),
                   label = HTML(
                     "Irrigation scenario",
                     as.character(help_ui(id_help_tillage, label = "(?)"))
                   ),
                   choices = autoirrigs,
                   selectize = F,
                   selected = autoirrigs[1]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_analysis),
                   label = HTML(
                     "Type of analysis",
                     as.character(help_ui(id_help_tillage, label = "(?)"))
                   ),
                   choices = analyses,
                   selectize = F,
                   selected = analyses[1]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   selectInput,
                   inputId = ns(id_paddock_settings_state),
                   label = HTML(
                     "Spatial coverage",
                     as.character(help_ui(id_help_tillage, label = "(?)"))
                   ),
                   choices = states,
                   selectize = F,
                   selected = states[2]
                 )
               ),
               paddock_setting_ui(
                 full_width_input(
                   textInput,
                   inputId = ns(id_paddock_settings_coords),
                   label = HTML(
                     "Coordinates",
                     as.character(help_ui(id_help_tillage, label = "(?)"))
                   ),
                   placeholder = "Decimal longitude/latitude e.g. 150.3,-24.2",
                   value = '150.3,-24.2'
                 )
               )
               # paddock_setting_ui(
               #   half_width_input(
               #     actionButton,
               #     inputId = ns(id_paddock_settings_getcoords),
               #     label = "Get Data", value = ""
               #   ),
               #   class='.myapp-paddock-settings__button'
               # )
               # if (has_permission(user_role, permissions$set_advanced_paddock_settings)) paddock_setting_ui(
               #   full_width_input(
               #     sliderInput,
               #     inputId = ns(id_paddock_settings_harv_year),
               #     label = "Harvest years",
               #     min = 1981,
               #     max = 2023,
               #     value = c(1981, 2023),
               #     step = 1,
               #   )
               # )
      )
    )
  )
}

# -----------------------------------------------------------------------------

paddock_settings_server <- function(paddock, current_tab, version) {

  moduleServer(
    generate_paddock_settings_id(paddock),

    function(input, output, session) {
      toListen <- reactive({
        list(input[[id_paddock_settings_analysis]], current_tab())
      })

      observeEvent(ignoreInit=T, toListen(), {
        if (input[[id_paddock_settings_analysis]]==analyses[2]) {
          shinyjs::disable(id_paddock_settings_state)
          shinyjs::enable(id_paddock_settings_coords)

        } else {
          if (current_tab()=='Summary') shinyjs::enable(id_paddock_settings_state) else shinyjs::disable(id_paddock_settings_state)
          shinyjs::disable(id_paddock_settings_coords)
        }
      })

      reactive({
        settings <- list()
        settings[[id_paddock_settings_version]] <- version

        paddock_setting_ids <- c(
          id_paddock_settings_cropsyst,
          id_paddock_settings_cultivar,
          id_paddock_settings_sowdate,
          id_paddock_settings_fert_amt,
          id_paddock_settings_initsw,
          id_paddock_settings_population,
          id_paddock_settings_autoirrig,
          id_paddock_settings_analysis,
          id_paddock_settings_state,
          id_paddock_settings_coords
        )
        for (p in paddock_setting_ids) {
          settings[[p]] <- input[[p]]
          if (settings[[p]]==FALSE) settings[[p]] <- 'false'
          if (settings[[p]]==TRUE) settings[[p]] <- 'true'
        }

        settings[[id_paddock_settings_ispoint]] <- (settings[[id_paddock_settings_analysis]]==analyses[2])

        if (settings[[id_paddock_settings_ispoint]]==T) {
          coord <- as.numeric(trimws(strsplit(settings[[id_paddock_settings_coords]],split=',')[[1]]))
          settings[[id_paddock_settings_state]] <- 'Grid1'
          settings[[id_paddock_settings_distance]] <- 9999

          if (length(na.omit(coord))>1) {
            coord <- data.frame(Lat=coord[2],Long=coord[1])
            closest <- nn2(data=grids[,-'Grid'], query=coord, k=1)
            settings[[id_paddock_settings_coords]] <- coord
            settings[[id_paddock_settings_distance]] <- closest$nn.dists
            settings[[id_paddock_settings_state]]    <- grids[closest$nn.idx, Grid]
          }
        }

        file_name2 <- get_filename(Factors, settings[Factors], what=3)
        settings[[id_paddock_settings_sow_years]] <- range(fread(sprintf("%s/%s/Aggregated/%s", dbfolder, settings[[id_paddock_settings_version]], file_name2)))

        settings
      })
    }
  )
}

# -----------------------------------------------------------------------------

plot_types_server <- function(paddock) {
  moduleServer(
    generate_paddock_settings_id(paddock),

    function(input, output, session) {
      plot_types <- reactiveVal({all_plot_types[1:3]})

      observeEvent(input[[id_paddock_settings_analysis]], {
        x <- if (input[[id_paddock_settings_analysis]]==analyses[2]) all_plot_types[2:5] else all_plot_types[1:3]
        plot_types({x})
      })
      return(plot_types)
    }
  )
}


