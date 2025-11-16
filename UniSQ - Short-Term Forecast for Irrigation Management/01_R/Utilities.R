

#========================================================================================================

AddStressIndexesToAPSIMMet <- function(MetFile, NewMetFile=NULL, Crops=NULL) {
  # MetFile: Full path to the APSIM met file.
  # NewMetFile: The name (NOT path) of the APSIM met file.
  # Crops: Names of the Crops for which the stress indexes must be calculated. If NULL, indexes will be added for all Crops.
  
  library(apsimx)
  
  met  <- apsimx::read_apsim_met(basename(MetFile), src.dir=dirname(MetFile), verbose=F)
  maxt <- met$maxt
  mint <- met$mint
  
  if (is.null(NewMetFile)) NewMetFile <- basename(MetFile)
  
  heatStressThr <- c('peanut'=35, 'wheat'=30, 'sunflower'=35, 'maize'=38, 'sorghum'=36, 
                     'cotton'=40, 'chickpea'=35, 'mungbean'=40, 'soybean'=40, 'sesame'=40)
  # References:
  # peanut_hst    <- 35 # Vara Prasad et al 2000 JEB vol 51 pp 777 to 784
  # wheat_hst     <- 30 # Stone and Nicolas 1994 Porter and Gawith 1994  http://plantsinaction.science.uq.edu.au/book/export/html/158
  # sunflower_hst <- 35 # Kalyar et a1 2014
  # maize_hst     <- 38 # Ramadoss et a1 2004
  # sorghum_hst   <- 36 # Hammer et al 2015
  # cotton_hst    <- 40 # Singh et al 2007
  # chickpea_hst  <- 35 # Devasirvatham et al 2012
  # mungbean_hst  <- 40 # Kaushal et al 2016
  # soybean_hst   <- 40 # Kaushal et al 2016 NEED TO FIND A SOYBEAN NUMBER
  
  #### Counting heat stress days ####
  if (is.null(Crops)) Crops <- names(heatStressThr)
  Crops <- tolower(Crops)
  y     <- list()
  K     <- 28
  
  for (crop in Crops) {
    x <- zoo::rollsum(maxt>=heatStressThr[crop], k=K)
    y[[crop]] <- c(rep(x[1], K-1), x)
  }
  
  for (crop in Crops)  {
    met <- apsimx::add_column_apsim_met(met, y[[crop]], sprintf('%s_heat_stress_days',crop), "()")
  }
  
  ### Calculations of heat and frost indexes ###
  y    <- list()
  
  x <- round(zoo::rollmean(maxt, k=K), 2)
  y[['28d_maxT_av']]    <- c(rep(x[1], K-1), x)
  x <- round(zoo::rollmean(mint, k=K), 2)
  y[['28d_mint_av']]    <- c(rep(x[1], K-1), x)
  x <- zoo::rollsum(mint<=2, k=K)
  y[['28d_frost_days']] <- c(rep(x[1], K-1), x)
  
  for (index in names(y)) {
    met <- apsimx::add_column_apsim_met(met, y[[index]], index, "()")
  }
  
  ### Tomorrow's min temperature
  # It's better to repeat the last mint instead of using the first day's mint
  # (let's not assume the data contains complete record of the last year.
  tomorows_min_temp <- c(mint[-1], mint[length(mint)])
  met <- apsimx::add_column_apsim_met(met, tomorows_min_temp, 'tomorows_min_temp', '()')
  
  ### Write met data back to the file
  apsimx::write_apsim_met(met, wrt.dir=dirname(MetFile), filename=NewMetFile) 
  invisible(NULL)
}

#========================================================================================================

ARMDataToCSV <- function(Data, Keys, Location, CropName, Folder, ExportColds) {
  # state-location-crop-soil_name-X_oc-X_PAWC-X_kgNS-X_per-maturity-X_plants-rowarrangment-sow_date-X_kgNf-irrigation-X_kgNT.Report.csv
  
  filename <- sprintf('%s/%s-%s-%s-%s-%s_oc-%s_PAWC-%s_kgNS-%s_per-%s-%s_plants-%s-%s-%s_kgNf-%s-%s_kgNT.Report.csv', Folder,
                      Location$State, Location$station_name, CropName, Keys$soil_name2, '10', Keys$PAWC, Keys$initial_soil_mineral_N_kg_ha, Keys$inital_PAWC_per,
                      Keys$cult, Keys$density, Keys$row_arrangement, Keys$sow_date, Keys$sow_fert_kg_N_ha, Keys$irrigation_treat, Keys$side_fert_kg_N_ha)
  
  x <- fwrite(Data[, ExportColds, with=F], filename, append=F)
  
  return(invisible(NULL))
}

#========================================================================================================

ExportToARMFormat <- function(Data, DBFold, DBVersion, CropName, DateStr, Keys, ExportColds, OnServer, ImportFile=NULL) {
  if (OnServer) DBVersion <- NULL
  
  FUNC <- function(majorData, Location) {
    message(sprintf('Exporting data for %s', Location$station_name))
    
    Folder <- paste(DBFold, DBVersion, Location$station_name, CropName, DateStr, 'results', sep='/')
    if (!dir.exists(Folder)) dir.create(Folder, recursive=T, showWarnings=T)
    
    if (!OnServer) {
      system(paste0('subst z: ',shQuote(normalizePath(Folder))))
      Folder <- 'z:'
    }
    
    x <- majorData[, ARMDataToCSV(.SD, Keys=.BY, Location=Location, CropName=CropName, Folder=Folder, ExportColds=ExportColds), by=Keys, .SDcols=names(Data)]
    
    if (!is.null(ImportFile)) file.create(paste(DBFold,DBVersion,Location$station_name,CropName,ImportFile,sep='/'), showWarnings=F)
    if (!OnServer) system("subst z: /D")
  }
  
  x <- Data[, FUNC(.SD, Location=.BY), by=c('State','station_name'), .SDcols=names(Data)]
}

#========================================================================================================




