
# Automatically download weather records from SILO , output names as location or station number (option)

TempFold    <- NULL
SimilarJobs <- F
if (length(commandArgs(T))==0) {
  NJobs <- 1 # Number of jobs
  JobID <- 1 # Job ID of the current script
  
} else {
  NJobs <- as.numeric(commandArgs(T))[1] # Number of jobs is read from command line
  JobID <- as.numeric(commandArgs(T))[2] # Job ID of the current script is read from command line
  if (length(commandArgs(T))>2) TempFold <- commandArgs(T)[3]
  if (length(commandArgs(T))>3) SimilarJobs <- (commandArgs(T)[4]=='TRUE' || commandArgs(T)[4]=='T')
  if (is.na(JobID)) JobID <- 1
  
  if (is.null(TempFold)) TempFold <- 'NULL'
  message(sprintf('Arguments: NJobs=%s, JobID=%s, TempFold=%s, SimilarJobs=%s',NJobs,JobID,TempFold,SimilarJobs))
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
  TreatFile <- 'Treatments.xlsx'
  SoilFile  <- 'SoilsModified_2024.05.28.apsimx'
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
{
  if (is.null(TempFold)) TempFold <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/ClusterZipFiles'
  
  setwd(TempFold)
  dir.create('Met', showWarnings=F, recursive=T)
  base::load('MetData.RData')
  
  Treats <- as.data.table(readxl::read_xlsx(path=TreatFile, sheet='Current'))
  
  ApFiles <- list.files(path=TempFold, pattern='*.apsimx$', full.names=F)
  if (SimilarJobs==T) {
    ApFiles <- MySplit(ApFiles,NGroups=NJobs,Which=JobID)
  } else {
    ApFiles <- ApFiles[grepl(paste0('Job',JobID),ApFiles)]
  }
  
  SoilNames <- GetNodeFromJSON(SoilFile, Attrs='$type', Values='Models.Soils.Soil, Models')
  SoilNames <- sapply(SoilNames, function(x) x$Name)
  
  Comms   <- NULL
  for (ApFile in ApFiles) {
    Comms <- append(Comms, sprintf('load %s', ApFile))
    Comms <- append(Comms, '#=========================================================================')
    
    for (i in 1:nrow(Treats)) {
      Site      <- Treats$Site[i]
      SowDate   <- Treats$SowDate[i]
      MetFile   <- paste0('Met/',Site,'.met')
      Soils     <- SoilNames[grep(Site, SoilNames)]
      
      if (!file.exists(MetFile)) writeLines(MetData[[Site]], con=MetFile)
      
      for (Soil in Soils) {
        NewApFile <- paste0(file_path_sans_ext(ApFile),'_',Soil,'_',SowDate,'.apsimx')
        
        Comms <- append(Comms, sprintf('[Weather].FileName = %s', MetFile))
        Comms <- append(Comms, sprintf('[Soil] = %s;[%s]', SoilFile, Soil))
        Comms <- append(Comms, sprintf('[Sowing].Script.SowDate = %s', SowDate))
        Comms <- append(Comms, sprintf('save %s', NewApFile))
        Comms <- append(Comms, sprintf('run'))
        Comms <- append(Comms, '#=========================================================================')
      }
    }
    Comms <- append(Comms, '')
  }
  writeLines(Comms, con='Config.txt')
}









