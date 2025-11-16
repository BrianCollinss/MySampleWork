
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
  TreatFile <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/01_R/APSIMTemplates/Treatments.xlsx'
  DoSample  <- F
  Radius    <- 0.1
  Rounds    <- 3
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/home/u8019357/APSIM/' else MainDir <- 'C:/Users/U8019357/OneDrive - UniSQ'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySourceProject.R',sep='/'))
  Dir <- SetupProject(MainDir=MainDir,Project=Project,Run=NULL,RunSet=NULL,SharedScripts=c('MyFunctions.R','MyApsim.R'))
}
#========================================================================================================

suppressPackageStartupMessages(library(xml2))

Sites <- as.data.table(readxl::read_xlsx(path=TreatFile, sheet='Sites'))
SoilFile  <- paste0(Dir$Data,'/Soils/Soils.apsim')

# Downloading from World Modelers Database
{
  folderNode <- read_xml('<folder version="37" creator="Apsim 7.10-r4220" name="Soils"></folder>')
  for (i in 1:nrow(Sites)) {
    message(paste("Downloding data for location", i))
    x <- vector()
    Coord <- unlist(Sites[i, .(Long,Lat)])
    Count <- -1
    
    for (r in 1:ifelse(DoSample,Rounds,1)) {
      NewRadius <- Radius * r
      
      for (j in 0:ifelse(DoSample,8,0)) {
        if (j==0 && r>1)
          next
        
        Count <- Count + 1
        
        if (j==0 && r==1)
          CoordNew <- Coord
        if (j==1)
          CoordNew <- Coord + c(+NewRadius, -NewRadius)
        if (j==2)
          CoordNew <- Coord + c(+NewRadius, 0)
        if (j==3)
          CoordNew <- Coord + c(+NewRadius, +NewRadius)
        if (j==4)
          CoordNew <- Coord + c(0, +NewRadius)
        if (j==5)
          CoordNew <- Coord + c(-NewRadius, +NewRadius)
        if (j==6)
          CoordNew <- Coord + c(-NewRadius, 0)
        if (j==7)
          CoordNew <- Coord + c(-NewRadius, -NewRadius)
        if (j==8)
          CoordNew <- Coord + c(0, -NewRadius)
        
        x <- try(xml2::read_xml(sprintf('https://worldmodel.csiro.au/apsimsoil?lon=%s&lat=%s', CoordNew[1], CoordNew[2])), silent=F)
        
        if (class(x)[1]=='xml_document') {
          x <- xml2::xml_find_all(x, 'Soil')[[1]]
          if (DoSample)
            xml2::xml_set_attr(x, 'name', paste0(Sites$Name[i],'-',Count))
          else 
            xml2::xml_set_attr(x, 'name', paste0(Sites$Name[i]))
          xml2::xml_add_child(folderNode, x)
          
        } else {
          Count <- Count - 1
        }
      }
    }
  }
  
  # Write the APSIM Classic file
  xml2::write_html(folderNode, file=paste0(Dir$Data,'/Soils/Soils.apsim'))
}

message("Downloding the data completed!")



