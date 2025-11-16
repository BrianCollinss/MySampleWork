

p_load(dplyr, lubridate, shinycssloaders)

# -----------------------------------------------------------------------------

n2o_co2e <- 298
ch4_co2e <- 25

# -----------------------------------------------------------------------------

traits <- c('GrainDryYield', 'GrainWetYield', 'AbovegroundDW', 'Soybean.Phenology.StartFloweringDAS', 'Soybean.Phenology.MaturityDAS',
            'AveFW', 'AveWetRootFracFact', 'AvePsFW', 'AvePsFTMeanT', 'AvePsFTMinT', 'AvePsFT', 'AvePsFN', 'AveFRGR')

# -----------------------------------------------------------------------------

summarise_scenario <- function(data, settings) {
  data2 <- data.table()
  state <- settings[[id_paddock_settings_state]]

  if (nrow(data)>0) {
    if (settings[[id_paddock_settings_ispoint]]) state <- '1981-2023'
    traits  <- intersect(names(data), traits)
    indices <- c('State','StateShort')
    data2   <- data[State==state, lapply(.SD, function(x) {if (all(is.na(x))) NA else mean(x, na.rm=T)}), keyby=indices, .SDcols=traits]
  }
  return(data2)
}

# -----------------------------------------------------------------------------

do_round <- function(x, digs) {
  if (abs(x) >= 100) digs <- 0
  y <- format(round(abs(x), digs), digits=max(1,digs), nsmall=digs, big.mark=",", scientific=F)
  return(y)
}

# -----------------------------------------------------------------------------

choose_color <- function(y, is_positive_good, sign_threshold) {
  color <- "darkgreen"
  return(color)
}

sign_thr_default <- 5
sign_thr_n <- 1

# -----------------------------------------------------------------------------

create_summary_item <- function(
    conversion = 1,
    get_color = choose_color,
    is_positive_good = FALSE,
    sign_thr = sign_thr_default,
    get_number_of_digits = function(...) 1,
    ...
) list(
  conversion = conversion,
  get_color = get_color,
  is_positive_good = is_positive_good,
  sign_thr = sign_thr,
  get_number_of_digits = get_number_of_digits,
  ...
)

# -----------------------------------------------------------------------------

paddock_summary_items <- list()

paddock_summary_items[['GrainDryYield']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Dry grain yield",
  icon = "lemon",
  is_positive_good = TRUE,
  get_unit = function(...) "kg/ha",
  get_number_of_digits = function(...) 0
)
paddock_summary_items[['GrainWetYield']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Wet grain yield",
  icon = "sack-dollar",
  is_positive_good = TRUE,
  get_unit = function(...) "kg/ha",
  get_number_of_digits = function(...) 0
)
paddock_summary_items[['AbovegroundDW']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Aboveground dry matter",
  icon = "tree",
  is_positive_good = TRUE,
  get_unit = function(...) "kg/ha",
  get_number_of_digits = function(...) 0
)
paddock_summary_items[['Soybean.Phenology.StartFloweringDAS']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Days to flowering",
  icon = "calendar-plus",
  is_positive_good = TRUE,
  get_unit = function(...) "days",
  get_number_of_digits = function(...) 0
)
paddock_summary_items[['Soybean.Phenology.MaturityDAS']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Days to maturity",
  icon = "calendar-check",
  is_positive_good = TRUE,
  get_unit = function(...) "days",
  get_number_of_digits = function(...) 0
)
paddock_summary_items[['AveFW']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average water deficiency stress",
  icon = "droplet-slash",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AveWetRootFracFact']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average waterlogging stress",
  icon = "cloud-showers-water",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AvePsFW']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average overall water stress",
  icon = "water",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AvePsFTMeanT']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average mean-temperature stress",
  icon = "temperature-quarter",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AvePsFTMinT']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average frost stress",
  icon = "snowflake",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AvePsFT']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average temperature stress",
  icon = "thermometer",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AvePsFN']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average N deficiency stress",
  icon = "flask",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)
paddock_summary_items[['AveFRGR']] <- create_summary_item(
  permission = permissions$view_rel_canefw,
  name = "Average growth stress index",
  icon = "chart-simple",
  is_positive_good = TRUE,
  get_unit = function(...) '(0-1)',
  get_number_of_digits = function(...) 2
)

# -----------------------------------------------------------------------------

paddock_summary_ui <- function(paddock, user_role, spinner_type) {
  ns <- NS(paste("paddock_summary_section", paddock, sep = "_"))

  column_ui <- function(...) {
    tags$div(
      class = "col-sm-6 col-md-4 col-lg-3",
      ...
    )
  }

  section_ui <- function(title, ids) {
    with_permission <- lapply(
      ids,
      function(id) has_permission(user_role, paddock_summary_items[[id]]$permission)
    )
    viewable <- ids[with_permission == TRUE]
    property_columns <- lapply(viewable, function(id) {
      column_ui(
        withSpinner(
          uiOutput(ns(id), inline=T),
          type = spinner_type
        )
      )
    })
    tags$section(
      hr(),
      tags$h3(title),
      fluidRow(property_columns)
    )
  }

  tags$section(
    uiOutput(ns("summary_text")),
    if (has_permission(user_role, permissions$view_yield_section)) section_ui(
      "Biomass and Yield", c('GrainDryYield', 'GrainWetYield', 'AbovegroundDW')),

    if (has_permission(user_role, permissions$view_phenology_section)) section_ui(
      "Phenology", c('Soybean.Phenology.StartFloweringDAS', 'Soybean.Phenology.MaturityDAS')),

    if (has_permission(user_role, permissions$view_water_stress_section)) section_ui(
      "Water Stress", c('AveFW', 'AveWetRootFracFact', 'AvePsFW')),

    if (has_permission(user_role, permissions$view_temperature_stress_section)) section_ui(
      "Temperature Stress", c('AvePsFTMeanT', 'AvePsFTMinT', 'AvePsFT')),

    if (has_permission(user_role, permissions$view_nutrition_stress_section)) section_ui(
      "Other Stresses", c('AvePsFN', 'AveFRGR'))
  )
}

# -----------------------------------------------------------------------------

paddock_summary_server <- function(paddock, settings, data_x) {
  moduleServer(
    paste("paddock_summary_section", paddock, sep = "_"),

    function(input, output, session) {
      summary_table <- reactive({
        summarise_scenario(data_x(), settings())
      })
      output[["summary_text"]] <- renderUI({
        text <- ifelse(settings()[[id_paddock_settings_ispoint]],
                       sprintf(
                         "Summary of selected variables (averaged over %s harvest years at the closes grid cell)",
                         paste(settings()[[id_paddock_settings_sow_years]], collapse='-')
                       ),
                       sprintf(
                         "Summary of selected variables (averaged over %s harvest years across %s)",
                         paste(settings()[[id_paddock_settings_sow_years]], collapse='-'), settings()[[id_paddock_settings_state]]
                       ))

        tags$h3(
          tags$strong(text),
          help_ui(id_help_change_thresholds, label = "(?)")
        )
      })

      lapply(names(paddock_summary_items), function(id) {
        summary <- paddock_summary_items[[id]]

        output[[id]] <- renderUI({
          if (nrow(summary_table())>0) {
            value <- summary_table()[, get(id)]
            y <- round(value / summary$conversion, summary$get_number_of_digits(value))
            value <- ifelse(is.na(value), 'All crops failed', sprintf("%s %s", do_round(value/summary$conversion, summary$get_number_of_digits(value)), summary$get_unit(value)))

          } else {
            value <- 'No Data'
          }

          summary_card_ui(id=paste(paddock, id, sep = "_"), value=value, subtitle=summary$name, icon=icon(summary$icon),
            color=summary$get_color(y, summary$is_positive_good, summary$sign_thr)
          )
        })
      })
    }
  )
}
