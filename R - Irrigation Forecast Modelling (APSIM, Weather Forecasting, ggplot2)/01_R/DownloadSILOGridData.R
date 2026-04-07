
# Automatically download weather records from SILO , output names as location or station number (option)

TempFold <- NULL
if (length(commandArgs(T))==0) {
  NJobs <- 1 # Number of jobs
  JobID <- 1 # Job ID of the current script
  LoadFromID <- 1
  SaveFromID <- 1
  
} else {
  NJobs <- as.numeric(commandArgs(T))[1] # Number of jobs is read from command line
  LoadFromID <- as.numeric(commandArgs(T))[2] # The ID output files start from
  SaveFromID <- as.numeric(commandArgs(T))[3] # The ID output files start from
  JobID <- as.numeric(commandArgs(T))[4] # Job ID of the current script is read from command line
  if (length(commandArgs(T))>4) TempFold <- commandArgs(T)[5]
  if (is.na(JobID)) JobID <- 1
  
  if (is.null(TempFold)) TempFold <- 'NULL'
  message(sprintf('Arguments: NJobs=%s, LoadFromID=%s, SaveFromID=%s, JobID=%s, TempFold=%s',NJobs,LoadFromID,SaveFromID,JobID,TempFold))
}
if (!is.null(TempFold) && TempFold=='NULL') TempFold <- NULL
OnServer  <- (length(grep(':',getwd()))==0)

#========================================================================================================
# Project Details
#========================================================================================================
{
  Project  <- '2024.04.26 Short-Term Forecast for Irrigation Management'
  RunSet   <- NULL
  RunNames <- NULL
}
#========================================================================================================
# Parameters
#========================================================================================================
{
  DownGrid     <- T
  WriteMetFile <- T
  StoreDataInR <- T
  
  Start    <- '19000101'
  Username <- 'brian.collins4@uq.edu.au'
  Mesh     <- 2 # degrees
  
  # Only for point data (i.e., DownGrid=F):
  HTMLFile <- 'SILO 2020.09.30.html' # at least 70% during 1990-2020
  S60      <- c('9037','21002','10007','77007','8025','18042','50052','51010','10536','18023','10035','41023','8039','81013','65012','35027','9631','8051','51018','78014','41038','55023','77018','10568','10579','10073','89016','10592','79028','24024','42022','10093','75142','18052','8093','53048','8095','10612','54120','10111','10619','51039','65026','25015','21043','10633','43030','23020','18075','12071','12074','74087','72150','24018','52088','76064','65028','8138','74123','10654' )
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/home/u8019357/APSIM/' else MainDir <- 'C:/Users/U8019357/OneDrive - UniSQ'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySourceProject.R',sep='/'))
  Dir <- SetupProject(MainDir=MainDir,Project=Project,Run=NULL,RunSet=NULL,Analysis=NULL,SharedScripts=c('MyFunctions.R','MyApsim.R'))
}
#========================================================================================================
Finish <- format(today()-1,format='%Y%m%d')

if (DownGrid) {
  message("Determining the coordinates.")
  
  api_url  <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php'
  if(!dir.exists(paste0(Dir$Data,sprintf('/GridData - %sD',Mesh)))) dir.create(paste0(Dir$Data,sprintf('/GridData - %sD',Mesh)))
  
  if (Mesh==1) {
    ln <- seq(111.7,153.95,Mesh)
    lt <- seq(-43.15,-10.5,Mesh)
    
  } else {
    ln <- seq(111.9,153.65,Mesh)
    lt <- seq(-43.15,-10.5,Mesh)
  }
  
  SILOGrid <- as.data.table(expand.grid(ln,lt))
  names(SILOGrid) <- c('Long','Lat')
  SILOGrid[, `:=`(IsGrid=T, NoData=F)]
  
  FilterShape <- sf::st_transform(st_read(paste0(Dir$Data,'/GIS Data/AgriFutures Study Area/'), layer='AgriFutures Study Area 2024.01.19', quiet=T), 4283)
  StatesMap <- sf::st_read(paste0(Dir$DataShared, "/Australian Statistical Geography Standard (ASGS July 2016)"), layer='STE_2016_AUST', quiet=T)
  AusMap    <- sf::st_read(paste0(Dir$DataShared, "/Australian Statistical Geography Standard (ASGS July 2016)"), layer='AUS_2016_AUST', quiet=T)
  SILOGrid  <- sf::st_set_crs(st_as_sf(SILOGrid, coords=c("Long","Lat")), 4283)
  SILOGrid  <- sf::st_intersection(SILOGrid, st_buffer(FilterShape, 0.05))
  SILOGrid  <- sf::st_intersection(SILOGrid, st_buffer(AusMap, 0.05))
  SILOGrid  <- sf::st_intersection(SILOGrid, st_buffer(StatesMap, 0.05))
  SILOGrid  <- cbind(as.data.table(st_drop_geometry(SILOGrid)), st_coordinates(SILOGrid))
  SILOGrid  <- RemoveCols(SILOGrid, cols=c('State','Name','id'))
  SILOGrid  <- Mapping_Cols(SILOGrid, data.frame(Old=c('X','Y','STE_NAME16'), New=c('Long','Lat','State')))
  SILOGrid  <- RemoveCols(SILOGrid, cols=c('AUS_CODE16','AUS_NAME16','AREASQKM16','AREASQKM16.1','STE_CODE16'))
  
  SILOGrid[is.na(State) & Long>140 & Lat> -36 & Lat< -28, State:='New South Wales']
  SILOGrid[is.na(State) & Long>140 & Lat< -39, State:='Tasmania']
  SILOGrid[is.na(State) & Long>129 & Long<138 & Lat> -26, State:='Northern Territory']
  SILOGrid[is.na(State) & Long>129 & Long<141 & Lat< -26, State:='South Australia']
  SILOGrid[is.na(State) & Long>141 & Lat< -36 & Lat> -40, State:='Victoria']
  SILOGrid[is.na(State) & Long<129, State:='Western Australia']
  SILOGrid[is.na(State) & Long>140 & Long<142 & Lat< -35 & State=='New South Wales', State:='Victoria']
  SILOGrid[is.na(State), State:='Queensland']
  
  SILOGrid[State=='New South Wales', StateShort:='NSW']
  SILOGrid[State=='Victoria', StateShort:='VIC']
  SILOGrid[State=='Queensland', StateShort:='QLD']
  SILOGrid[State=='Western Australia', StateShort:='WA']
  SILOGrid[State=='South Australia', StateShort:='SA']
  SILOGrid[State=='Northern Territory', StateShort:='NT']
  SILOGrid[State=='Tasmania', StateShort:='TAS']
  
  message(sprintf('%s grid cells were found.', nrow(SILOGrid)))
  
  for (i in seq(along=SILOGrid$Long)) {
    message(paste("Downloding data for grid", i))
    
    url <- sprintf('?lat=%s&lon=%s&format=apsim&start=%s&finish=%s&username=%s&password=gui&comment=standard',SILOGrid$Lat[i],SILOGrid$Long[i],Start,Finish,Username)
    url <- curl(paste0(api_url,url),open='r')
    new_data  <- readLines(url)
    
    SILOGrid[i, ID:=i]
    SILOGrid[i, GrName:=paste0('Grid',i)]
    
    if (length(new_data)<30 || grepl('0.0   0.0   0.0   0.0', new_data[100])) {
      new_data <- NA_character_
      SILOGrid[i, NoData:=T]
      next()
      
    } else {
      if (WriteMetFile==T) {
        writeLines(new_data, paste0(Dir$Data,sprintf('/GridData - %sD/Grid',Mesh),i,'.met'))
      }
    }
    
    if (StoreDataInR==T) SILOGrid[i, MetData:=list(new_data)]
  }
  
  SILOGrid <- SILOGrid[NoData==F]
  save(SILOGrid,file=paste0(Dir$Data,sprintf('/GridData - %sD/SILOGrid',Mesh),'.RData'))
  
} else {
  #========================================================================================================
  message("Downloading station data")
  
  api_url  <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php'
  load("C:/Users/uqbababa/OneDrive - The University of Queensland/02_Data/SimFilesFull.RData")
  
  HTML <- readLines(paste(Dir$R,HTMLFile,sep='/'))
  StationsID <- strsplit(HTML,'data-value=')[[1]][-1]
  StationsID <- lapply(StationsID, function(x) return(str_extract(x, "\\-*\\d+\\.*\\d*")))
  StationsID <- unlist(StationsID)
  StationsID <- union(StationsID,S60)
  
  Stats <- c('South Australia','Northern Territory','Queensland','Victoria','Western Australia','New South Wales','Tasmania')
  StatsPos <- str_locate(HTML,Stats)[,1]
  
  AllStates <- NULL
  for (St in StationsID) {
    if (St %in% S60) {
      Temp <- SimFilesFull[`Station Number`==St][1,]
      if (length(grep('^VIC',Temp$Region2))>0) Temp <- 'Victoria'
      else if (length(grep('^WA',Temp$Region2))>0) Temp <- 'Western Australia'
      else if (length(grep('^SA',Temp$Region2))>0) Temp <- 'South Australia'
      else if (length(grep('^NSW',Temp$Region2))>0) Temp <- 'New South Wales'
      else if (length(grep('^QLD',Temp$Region2))>0) Temp <- 'Queensland'
      AllStates <- c(AllStates,Temp)
      
    } else {
      Pos <- str_locate(HTML,paste0('"',St))[1]
      Pos <- last(which(Pos>StatsPos))
      AllStates <- c(AllStates,Stats[Pos])
    }
  }
  
  AllStations <- data.table(ID=StationsID,State=AllStates)
  setwd(Dir$Data)
  
  for (i in seq(along=StationsID)) {
    
    url <- sprintf('?start=%s&finish=%s&station=%s&format=apsim&comment=XN&username=%s',Start,Finish,StationsID[i],Username)
    url <- curl(paste0(api_url,url),open='r')
    new_data  <- readLines(url)
    
    new_name  <- gsub("!station name =","",new_data[3])	
    new_name  <- gsub("^\\s","",new_name)
    new_name  <- gsub("\\s\\s","",new_name)
    
    lat  <- as.numeric(str_extract(new_data[4], "\\-*\\d+\\.*\\d*"))
    long <- as.numeric(str_extract(new_data[5], "\\-*\\d+\\.*\\d*"))
    
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    new_name <- tolower(str_trim(new_name))
    new_name <- simpleCap(new_name)
    
    file_name <- paste0(new_name,'.met')
    Ext       <- file_ext(file_name)
    
    NameWOutExt2 <- gsub("\\.met$","",file_name)
    NameWOutExt2 <- gsub(c("\\(")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\)")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\.+")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\s\\."),".",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\s\\s")," ",NameWOutExt2)
    
    NameWOutExt2 <- Capitalize_First_Letters(NameWOutExt2,ignorePar=F)
    NameWithExt2 <- paste0(NameWOutExt2,'.',Ext)
    NameWOutExt3 <- paste0(NameWOutExt2,' - ',StationsID[i])
    NameWithExt3 <- paste0(NameWOutExt3,'.',Ext)
    
    writeLines(new_data,NameWithExt3)
    
    AllStations$Name[i]   <- NameWOutExt2
    AllStations$NameID[i] <- NameWOutExt3
    AllStations$Lat[i]    <- lat
    AllStations$Long[i]   <- long
  }
  
  Stations <- AllStations[, .(ID,Name,NameID,State,Lat,Long)]
  save(Stations,file=paste0(Dir$Data,'/AllStations.RData'))
}

message("Downloding the data completed!")


