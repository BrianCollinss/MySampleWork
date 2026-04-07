

TempFold <- NULL
if (length(commandArgs(T))==0) {
  NJobs <- 1 # Number of jobs
  JobID <- 1 # Job ID of the current script
  
} else {
  NJobs <- as.numeric(commandArgs(T))[1] # Number of jobs is read from command line
  JobID <- as.numeric(commandArgs(T))[2] # Job ID of the current script is read from command line
  if (length(commandArgs(T))>2) TempFold <- commandArgs(T)[3]
  if (is.na(JobID)) JobID <- 1
  
  if (is.null(TempFold)) TempFold <- 'NULL'
  message(sprintf('Arguments: NJobs=%s, JobID=%s, TempFold=%s',NJobs,JobID,TempFold))
}
if (!is.null(TempFold) && TempFold=='NULL') TempFold <- NULL
OnServer  <- (length(grep(':',getwd()))==0)

#========================================================================================================
# Project Details
#========================================================================================================
{
  Project <- '2024.04.26 Short-Term Forecast for Irrigation Management'
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
# Settings for Running APSIM and APSIM-NG
#========================================================================================================

Settings <- list()

Settings$Projects <- Project
Settings$RunSet   <- '2024.05.26 2Lead-4P-1Z-2DelayFrac-22Treat-3Soil-2N'
Settings$RunNames <- c('2024.05.28 Run2-50Rep')

Settings$MainDir          <- if (OnServer) '/home/u8019357/APSIM' else 'C:/Users/U8019357/OneDrive - UniSQ/'
Settings$APSIMConsolePath <- if (OnServer) {
  'singularity exec -B $TMPDIR:$TMPDIR --pwd $TMPDIR /home/uqbababa/Singularities/Apsim7.10-83039e542.sapp'
} else {
  'C:/Users/U8019357/OneDrive - UniSQ/Documents/Repository/APSIMNextGen/bin/Debug/net6.0/Models.exe' 
}

if (!OnServer) {
  system(paste0('subst z: ',shQuote(normalizePath(Settings$MainDir))))
  Settings$MainDir <- 'z:'
}

Settings$OnServer       <- OnServer
Settings$RunApsim       <- F
Settings$IsApsimNG      <- T
Settings$JustLoad       <- F
Settings$SaveInOneFile  <- F
Settings$RemOldOuts     <- F
Settings$CheckHPC       <- T

Settings$Extension      <- 'apsimx'
Settings$OutExtension   <- 'db'
Settings$SFilePatt      <- 'ReportHarvest'
Settings$DFilePatt      <- NA
Settings$SowYearCol     <- 'SowYear'
Settings$SimFilesFull   <- NA

Settings$SaveToRunFold  <- T
Settings$ReadTars       <- T
Settings$InfoInFileName <- F
Settings$Mapping        <- NA
Settings$Factors        <- NA

Settings$RunFacts       <- T
Settings$NoSumm         <- T
Settings$UseListFile    <- T

#========================================================================================================

x <- APSIM_Run_Read(Settings=Settings,NJobs=NJobs,JobID=JobID,TempFold=TempFold)

#========================================================================================================

if (!OnServer) system("subst z: /D")

if (OnServer) for (i in 1:200) print('Job is done!')




