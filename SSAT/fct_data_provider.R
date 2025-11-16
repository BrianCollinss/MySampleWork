

p_load(data.table, dplyr, tidyr)

# -----------------------------------------------------------------------------

hash_default_params <- list(Region='Burdekin', WStation='AyrDPI')

n2o_co2e <- 298
ch4_co2e <- 25
n2o_burn_factor <- 0.07 / 1000 # kg N2O / kg DM
ch4_burn_factor <- 2.70 / 1000 # kg CH4 / kg DM
mm_to_ML <- 100

is_on_brians_pc <- file.exists("C:/Users/U8019357")

appfolder <- ifelse(is_on_brians_pc, '.', '/home/ubuntu/SSATApp')
dbfolder  <- ifelse(is_on_brians_pc, '../SSATDB', '/home/ubuntu/SSATDB')

grids <- fread(file=sprintf("%s/%s/Grids.csv", dbfolder, 'V1'))

# -----------------------------------------------------------------------------

fetch_scenario_data <- function(settings) {

  type <- ifelse(settings[[id_paddock_settings_ispoint]]==T, 'Gridded', 'Aggregated')

  if (type=='Gridded') {
    file_names <- get_filename(c(Factors,'Grid'), c(settings[Factors], settings[[id_paddock_settings_state]]), what=1)

  } else {
    file_names <- get_filename(Factors, settings[Factors], what=1)
  }

  file_paths <- lapply(file_names, function(file_name) { sprintf("%s/%s/%s/%s", dbfolder, settings[[id_paddock_settings_version]], type, file_name) })

  if (length(file_paths)>1) {
    return(sapply(file_paths, fread, simplify=F))

  } else {
    return(fread(file_paths[[1]]))
  }
}

# -----------------------------------------------------------------------------

get_scenario_data <- function(settings) {
  fetcheddata <- fetch_scenario_data(settings)
  return(parse_scenario_data(data=fetcheddata, settings=settings))
}

# -----------------------------------------------------------------------------

parse_scenario_data <- function(data, settings) {
  if (is.list(data) && !is.data.table(data)) data <- rbindlist(data, fill=T)
  setDT(data)

  if (nrow(data)>0) {
    if (settings[[id_paddock_settings_ispoint]]==F) {
      data <- rbind(copy(data), copy(data)[, `:=`(State='Australia',StateShort='AUS')])

    } else {
      if (settings[[id_paddock_settings_distance]]<2) {
        data$State <- cut(data$HarvestYear, breaks=c(1981,1991,2001,2011,2024), labels=c('1981-1990','1991-2000','2001-2010','2011-2023'), right=F)
        data$StateShort <- data$State
        data <- rbind(copy(data), copy(data)[, `:=`(State='1981-2023',StateShort='1981-2023')])

      } else {
        data <- data.table()
      }
    }

    if (nrow(data)>0) {
      data[Soybean.Phenology.MaturityDAS>220, `:=`(Soybean.Phenology.StartFloweringDAS=NA, Soybean.Phenology.MaturityDAS=NA,
                                                   GrainDryYield=0, GrainWetYield=0)]
    }
  }

  x <- gc()
  return(data)
}

# -----------------------------------------------------------------------------



