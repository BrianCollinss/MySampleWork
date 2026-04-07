
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
  # Could be file(s) or folder
  MetFiles <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/04_Data/Weather Files/Original'
  MetDir   <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/04_Data/Weather Files/Modified'
  MaxLead  <- 7 # days
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

if (dir.exists(MetFiles)) MetFiles <- list.files(path=MetFiles, pattern='*.met$', full.names=T)

MetData <- list()

for (MetFile in MetFiles) {
  WData <- APSIM_Read_Weather_Files(File=MetFile, AddDateComps=T, ExpComments=T)
  
  Data <- copy(WData$Data)
  Cols <- paste0('rain_l',1:MaxLead)
  
  for (l in 1:MaxLead) {
    Data[, (Cols[l]) := lapply(.SD,data.table::shift,l,fill=0,type="lead"), .SDcols='rain']
  }
  
  FileOut <- paste(MetDir,basename(MetFile),sep='/')
  Data[is.nan(radn), radn:=mean(Data[!is.nan(radn), round(mean(radn,na.rm=T),2)])]
  APSIM_Write_Weather_Files(Data=Data, Comments=WData$Comments, FileOut=FileOut, AddCols=Cols)
  
  Temp <- readLines(FileOut)
  MetData[[file_path_sans_ext(basename(MetFile))]] <- Temp
}

save(MetData, file=paste0(MetDir, '/MetData.RData'))













