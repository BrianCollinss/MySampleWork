

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
  Project  <- '2024.04.26 Short-Term Forecast for Irrigation Management'
  RunSet   <- '2024.05.26 2Lead-4P-1Z-2DelayFrac-22Treat-3Soil-2N'
  RunNames <- '2024.05.28 Run2-50Rep'
}
#========================================================================================================
# Parameters
#========================================================================================================
{
  TempFile  <- 'MaizeTemplate.apsimx'
  NumCopies <- 50
  StartRep  <- 51
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/home/u8019357/APSIM/' else MainDir <- 'C:/Users/U8019357/OneDrive - UniSQ'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySourceProject.R',sep='/'))
  Dir <- SetupProject(MainDir=MainDir,Project=Project,Run=RunNames[1],RunSet=RunSet,SharedScripts=c('MyFunctions.R','MyApsim.R'))
}
LineX()
#========================================================================================================
{
  Sim  <- readLines(TemplateX <- paste0(Dir$R,'/ApsimTemplates/',TempFile),warn=F)
  Sim2 <- copy(Sim)
  
  # Create copies
  for (Scen in 1:3) {
    Scen <- c('Base','Perf','Imperf')[Scen]
    
    if (Scen=='Base') {
      Sim3 <- copy(Sim2)
      ln <- grep('\\[AutoIrrigate\\].Script.Lead', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' 0', Sim3[ln])
      ln <- grep('\\[AutoIrrigate\\].Script.Prob', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' -1', Sim3[ln])
      ln <- grep('\\[AutoIrrigate\\].Script.MaxErr', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' -1', Sim3[ln])
      ln <- grep('\\[AutoIrrigate\\].Script.DelayFrac', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' -1', Sim3[ln])
      
    } else if (Scen=='Perf') {
      Sim3 <- copy(Sim2)
      ln <- grep('\\[AutoIrrigate\\].Script.Prob', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' 0', Sim3[ln])
      ln <- grep('\\[AutoIrrigate\\].Script.MaxErr', Sim3)
      Sim3[ln] <- gsub(str_sub(Sim3[ln], start=str_locate(Sim3[ln], pattern='=')[1]+1, end=str_locate(Sim3[ln], pattern='\\",')[1]-1), ' 0', Sim3[ln])
      
    } else if (Scen=='Imperf') {
      Sim3 <- Sim2
      
    } else {
      stop('This Scen is not defined!')
    }
    
    ln <- grep('"Name": "ReportDaily"', Sim3)
    Sim3[ln+3] <- gsub('true','false',Sim3[ln+3])
    
    for (rp in 1:if (Scen%in%c('Base','Perf')) 1 else NumCopies) {
      writeLines(Sim3, con=paste0(Dir$Run, sprintf('/%s_Rep%s.apsimx', Scen, ifelse(NumCopies==1,'',StartRep+rp-1))))
    }
  }
}
LineX()






