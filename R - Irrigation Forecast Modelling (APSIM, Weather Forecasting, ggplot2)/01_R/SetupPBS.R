

HostUni <- 'USQ'
Project <- '2024.04.26 Short-Term Forecast for Irrigation Management'

UploadR           <- T  # Upload all R scripts (shared and project)?
UploadSharedData  <- F  # Upload all SharedData folder?

JobType  <- 2
DependOn <- NULL

if (JobType==1) {
  RFile   <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/01_R/ReadAPSIM.R'
  
  NJobs    <- 3
  JobArray <- T
  NCPU     <- 3
  Memor    <- 100  # GB
  WallTime <- '01:00:00'
  
} else if (JobType==2) {
  RFile   <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/01_R/RunAnalysis.R'
  
  NJobs    <- 1
  JobArray <- T
  NCPU     <- 3
  Memor    <- 150  # GB
  WallTime <- '01:00:00'
  
} else {
  stop()
}

ProjectGroup <- '459040_1008724_APSIMSesame'

TempFold <- '$TMPDIR'  # $PBS_O_WORKDIR
Email    <- 'brian.collins@unisq.edu.au'
OnServer <- (length(grep(':',getwd()))==0)

#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/home/u8019357/APSIM/' else MainDir <- 'C:/Users/U8019357/OneDrive - UniSQ'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySourceProject.R',sep='/'))
  Dir <- SetupProject(MainDir=MainDir,Project=Project,Run=NULL,RunSet=NULL,SharedScripts=c('MyFunctions.R','MyApsim.R','MyPlots.R'))
}
#========================================================================================================

x <- SetupPBS(HostUni=HostUni,Project=Project,RFile=RFile,NJobs=NJobs,JobArray=JobArray,NCPU=NCPU,Memor=Memor,WallTime=WallTime,TempFold=TempFold,
              Email=Email,ProjectGroup=ProjectGroup,MainDir=MainDir,UploadR=UploadR,UploadSharedData=UploadSharedData,DependOn=DependOn)




