
p_load(char=c('base','DBI','doParallel','dplyr','DBI','foreach','lubridate','Matrix','methods','parallel',
              'plyr','RSQLite','ssh','stringr','tools','utils','XML','zip'), install=F, repos=getOption('repos'))

#==============================================================================================
#==============================================================================================

APSIM_Bundle <- function(HostUni='JCU',MainDir,Projects,RunSet,RunNames,IsApsimNG=F,Bundle=T,Upload=T,UploadR=F,TempDir=NULL,
                         SimsPerJob=100,NJobs=NULL,RunOnServ=T,NewSetts=NULL,MoreCPUs=1,BundleConsolePath,ProjectGroup=NULL,
                         NoSummary=T,UnZipLocal=F,OnLinux=F,SimFile=NULL,Email=NULL,Singularity=NULL,ApsimExec='Apsim.exe',
                         USQServer='fawkes.usq.edu.au',JCUServer='zodiac.hpc.jcu.edu.au',
                         BunyaServer='bunya.rcc.uq.edu.au',UQServer='tinaroo.rcc.uq.edu.au',
                         PreProcR=NULL,PreProcRArgs=NULL,PostProcR=NULL,PostProcRArgs=NULL,PrePostFiles=NULL,
                         SimilarJobs=F,Playlist=NULL,ConfigFile=NULL) {
  library(ssh)
  
  if (HostUni=='USQ') {
    Server     <- USQServer
    RemMainDir <- '/home/u8019357/APSIM'
    UserName   <- Sys.getenv("USQ_USERNAME")
    PassW      <- Sys.getenv("USQ_PASSWORD")
    if (is.null(Singularity)) Singularity <- '/home/u8019357/Singularities/Apsim7.10-latest.ubuntu.sapp'
    if (is.null(Email)) Email <- 'brian.collins@unisq.edu.au'
    
  } else if (HostUni=='JCU') {
    Server     <- JCUServer
    RemMainDir <- '/home/jc837076/APSIM'
    UserName   <- Sys.getenv("JCU_USERNAME")
    PassW      <- Sys.getenv("JCU_PASSWORD")
    if (is.null(Singularity)) Singularity <- '/home/jc837076/Singularities/Apsim7.10-latest.ubuntu.sapp'
    if (is.null(Email)) Email <- 'brcollins2020@gmail.com' # 'brian.collins@jcu.edu.au'
    
  } else if (HostUni=='Bunya') {
    Server     <- BunyaServer
    RemMainDir <- '/scratch/user/briancollins/APSIM'
    UserName   <- Sys.getenv("Bunya_USERNAME")
    PassW      <- Sys.getenv("Bunya_PASSWORD")
    if (is.null(Singularity)) Singularity <- '/home/briancollins/Singularities/Apsim7.10-latest.ubuntu.sapp'
    if (is.null(Email)) Email <- 'brian.collins@jcu.edu.au'
    
  } else if (HostUni=='UQ') {
    Server     <- UQServer  # 'awoonga.qriscloud.org.au' #
    RemMainDir <- '/scratch/user/uqbababa/APSIM'
    UserName   <- Sys.getenv("UQ_USERNAME")
    PassW      <- Sys.getenv("UQ_PASSWORD")
    if (is.null(Singularity)) Singularity <- '/home/uqbababa/Singularities/Apsim7.10-latest.ubuntu.sapp'
    if (is.null(Email)) Email <- 'brian.collins4@uq.edu.au'
  }
  
  #========================================================================================================
  
  if (IsApsimNG) OnLinux <- F
  RemDir <- APSIM_Paths2(Dir=list(),Project=NULL,RunSet=RunSet,Run=NULL,MainDir=RemMainDir)
  UserAtServer <- paste(UserName,Server,sep='@')
  
  if ((Upload || RunOnServ) && HostUni!='Bunya') {
    message(sprintf('Connecting to the server, %s ...',Server))
    sscon <- ssh_connect(host=UserAtServer,passwd=PassW,verbose=F,keyfile=NULL)
    
    x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                       command=c(paste('mkdir',shQuote(RemMainDir)),paste('mkdir',shQuote(RemDir$AllProjects)),
                                 paste('mkdir',shQuote(RemDir$WeatherAll)),paste('mkdir',shQuote(RemDir$Weather)),
                                 paste('mkdir',shQuote(RemDir$RShared)),paste('mkdir',shQuote(RemDir$DataShared)),
                                 paste('mkdir',shQuote(RemDir$JobSubmit))))
  }
  
  if (UploadR) {
    message('Uploading project and shared R scripts ...')
    
    for (Pr in Projects) {
      Dir    <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=NULL,Run=NULL,MainDir=MainDir,OnLinux=OnLinux)
      RemDir <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=RunSet,Run=NULL,MainDir=RemMainDir)
      
      x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                         command=c(paste('mkdir',shQuote(RemDir$Project)),paste('mkdir',shQuote(RemDir$R)),
                                   paste('mkdir',shQuote(RemDir$RShared))))
      
      Temp <- List_Files_by_Ext(Dir=Dir$R,Ext='r',AlTogether=F)
      x <- scp_upload(session=sscon,files=Temp,to=RemDir$R,verbose=F)
      
      Temp <- List_Files_by_Ext(Dir=Dir$RShared,Ext='r',AlTogether=F)
      x <- scp_upload(session=sscon,files=Temp,to=RemDir$RShared,verbose=F)
    }
  }
  #========================================================================================================
  
  for (Pr in Projects) {
    LineX()
    message(paste0("Project: ",Pr))
    
    if (is.null(RunNames)) RunNames <- list.dirs(path=Dir$RunSet,full.names=F,recursive=F)
    
    for (Rn in RunNames) {
      IndRn <- which(RunNames==Rn)
      
      LineX()
      message(sprintf("Run %s: %s", IndRn, Rn))
      LineX()
      
      Dir    <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=RunSet,Run=Rn,MainDir=MainDir,OnLinux=OnLinux)
      RemDir <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=RunSet,Run=Rn,MainDir=RemMainDir)
      
      x <- sapply(Dir, function(x) {if(!dir.exists(x)) dir.create(x,recursive=T,showWarnings=F)})
      
      message('Bundling the input files')
      ZipFile <- list.files(Dir$Run,pattern=glob2rx("*.zip"),full.names=T)
      unlink(ZipFile,recursive=T,force=T)
      
      if (is.null(SimFile)) {
        xDir <- shQuote(Dir$Run)
      } else {
        xDir <- shQuote(paste(Dir$Run,SimFile,sep="/"))
      }
      
      setwd(Dir$Run)
      
      if (Bundle) {
        if (IsApsimNG) {
          res <- APSIMNG_Bundle(Dir$Run,ModelsExePath=BundleConsolePath,NJobs=NJobs,Ext='apsimx',Singularity=Singularity,ApsimExec=ApsimExec,
                                NewSetts=NewSetts,MoreCPUs=MoreCPUs,Email=Email,ProjectGroup=ProjectGroup,Zip=T,
                                PreProcR=PreProcR,PreProcRArgs=PreProcRArgs,PostProcR=PostProcR,PostProcRArgs=PostProcRArgs,PrePostFiles=PrePostFiles,
                                SimilarJobs=SimilarJobs,Playlist=Playlist,ConfigFile=ConfigFile)
          
        } else {
          res <- system2(BundleConsolePath,stdout=T,
                         args=c(paste0('simulationSFX=',Singularity),
                                ifelse(NoSummary,'MaxOutputLines=50',''),
                                'archUnix=TRUE',paste0('simsPerJob=',as.integer(SimsPerJob)),'outputFolder=.',xDir))
        }
      } else res <- NULL
      
      xNameZip <- paste0(paste(Dir$Run,gsub('\\s+|\\.','_',Rn),sep='/'),'.zip')
      xNameFol <- paste(Dir$ClustZips,gsub('\\s+|\\.','_',Rn),sep='/')
      
      unlink(xNameZip,force=T,recursive=T)
      unlink(xNameFol,force=T,recursive=T)
      
      if (!is.null(attr(res,"status"))) {
        print(res)
        x <- ssh_disconnect(sscon)
        stop(paste('Problem in:',Rn))
        
      } else {
        ZipFile <- list.files(Dir$Run,pattern=glob2rx("*.zip"),full.names=T)
        if (length(ZipFile)>0) x <- file.rename(ZipFile,xNameZip)
        
        if (UnZipLocal) {
          unzip(xNameZip,exdir=xNameFol)
          unlink(xNameZip,recursive=T,force=T)
          
        } else {
          xNameFol <- paste0(paste(Dir$ClustZips,gsub('\\s+|\\.','_',Rn),sep='/'),'.zip')
          if (file.exists(xNameZip)) {
            file.copy(xNameZip,xNameFol,overwrite=T)
            file.remove(xNameZip)
          }
          xNameZip <- xNameFol
        }
      }
      
      # Update
      if (!IsApsimNG && (!is.null(NewSetts) && (HostUni %in% c('JCU','USQ','Bunya')))) {
        exdir <- paste0(tempdir(),'/apsim')
        if (dir.exists(exdir)) x <- unlink(exdir,recursive=T,force=T)
        
        x <- unzip(xNameZip,exdir=exdir)
        x <- unlink(xNameZip,recursive=T,force=T)
        
        message('Changing requested resources')
        Setts <- readLines(paste(exdir,'Apsim.pbs',sep='/'))
        
        if (HostUni %in% c('JCU','USQ')) {
          Pos <- grep('bin/qsub -J',Setts)
          if (length(Pos)==0) {
            Pos <- grep('qsub',Setts)
            Setts <- gsub('qsub', '/opt/pbs/bin/qsub', Setts)
          }
          
          # if (NJobs==1) {
          #   Pos <- grep('bin/qsub', Setts)
          #   Setts <- gsub('*qsub*', '/opt/pbs/bin/qsub', Setts)
          # }
          
          Pos <- grep('srcdir',Setts)
          Setts <- Setts[-Pos[1:2]]
          Setts <- gsub('srcdir', 'PBS_O_WORKDIR', Setts)
          
          Setts <- Setts[-grep('glibc',Setts)]
          Setts <- Setts[-grep('PBS -A',Setts)]
          Pos <- grep('#PBS -l select',Setts)
          if (length(Pos)>0) {
            Setts[Pos] <- sprintf('#PBS -l select=1:ncpus=%s:mem=%s',NewSetts$CPU+MoreCPUs,NewSetts$Mem)
            Setts <- Setts[-(Pos+1)]
          }
          Pos <- grep('#PBS -l walltime',Setts)
          if (length(Pos)>0) Setts[Pos]   <- sprintf('#PBS -l walltime=%s',NewSetts$WallT)
          if (length(Pos)>0) Setts <- append(Setts, sprintf('#PBS -m %s','e'), after=Pos)
          if (length(Pos)>0) Setts <- append(Setts, sprintf('#PBS -M %s',Email), after=Pos+1)
          if (length(Pos)>0) Setts <- append(Setts, sprintf('#PBS -q %s', 'default'), after=Pos+2)
          if (length(Pos)>0 & HostUni=='USQ') Setts <- append(Setts, sprintf('#PBS -P %s', ProjectGroup), after=Pos+3)
          
          Pos <- grep('chmod',Setts)
          if (length(Pos)>0 & NoSummary==T) Setts <- append(Setts,' find . -name "*.sum" -exec rm {} \\;', after=Pos+1)
          Pos <- grep('module load singularity',Setts)
          if (length(Pos)>0 & HostUni %in% c('JCU')) Setts[Pos] <- ' module load singularity'
          if (length(Pos)>0 & HostUni %in% c('USQ')) Setts[Pos] <- ' module load singularity/3.7.0-gcc8.3.1-fv5'
          Pos <- grep('module load glibc',Setts)
          if (length(Pos)>0 & HostUni=='JCU') Setts <- Setts[-Pos]
          Pos <- grep('--files-from',Setts)
          if (length(Pos)>0) Setts[Pos] <- gsub('--files-from', '--remove-files --files-from', Setts[Pos])
          Pos <- grep('TMPDIR',Setts)
          if (length(Pos)>0 & HostUni=='USQ') {
            Setts <- append(Setts, ' mkdir -p \\$TMPDIR', after=Pos-1)
            Setts <- append(Setts, ' TMPDIR=/sandisk1/u8019357/Job\\$PBS_ARRAY_INDEX', after=Pos-1)
          }
          
          Pos <- grep('SINGULARITYENV_NUMBER_OF_PROCESSORS', Setts)
          if (length(Pos)>0) {
            Setts[Pos] <- sprintf(' export SINGULARITYENV_NUMBER_OF_PROCESSORS=%s',NewSetts$CPU)
            Setts <- append(Setts, sprintf(' export NUMBER_OF_PROCESSORS=%s', NewSetts$CPU+MoreCPUs), after=Pos-1)
            Setts <- append(Setts, ' export SINGULARITY_CACHEDIR=$TMPDIR', after=Pos+1)
            Setts <- append(Setts, ' export SINGULARITY_LOCALCACHEDIR=$TMPDIR', after=Pos+1)
            Setts <- append(Setts, ' export SINGULARITY_TMPDIR=$TMPDIR', after=Pos+1)
          }
          Pos <- grep('find',Setts)[2]
          Setts <- append(Setts, ' rm -r \\$TMPDIR', after=Pos)
          
          message('Changing singularity commands')
          Files <- list.files(path=exdir,pattern='Apsim.singularity',full.names=T)
          
          for (File in Files) {
            Text <- readLines(File)
            Text[3] <- gsub(' -B /30days/\\$USER:/30days -B /90days/\\$USER:/90days ','',Text[3])
            if (!is.null(ApsimExec)) Text[3] <- gsub('Apsim.exe', ApsimExec, Text[3])
            writeLines(Text,File)
          }
          
        } else if (HostUni=='Bunya') {
          Pos <- grep('qsub',Setts)
          if (length(Pos)>0) {
            NJobs <- as.numeric(trimws(str_split(Setts[Pos], pattern='-')[[1]][2]))
            if (is.na(NJobs)) NJobs <- 1
            Setts[Pos] <- 'cat <<EOF | sbatch '
          }
          
          Pos <- grep('#PBS',Setts)
          if (length(Pos)>0) Setts <- Setts[-Pos]
          Setts <- gsub('PBSPro','SLURM',Setts)
          Setts <- gsub('PBS_ARRAY_INDEX','SLURM_ARRAY_TASK_ID',Setts)
          
          Pos <- grep('sbatch',Setts)
          Setts <- append(Setts,'#SBATCH --account=a_agtac', after=Pos+1)
          Setts <- append(Setts,'#SBATCH --partition=general', after=Pos+1)
          Setts <- append(Setts,'#SBATCH --job-name=APSIM', after=Pos+1)
          Setts <- append(Setts,sprintf('#SBATCH --time=%s',NewSetts$WallT), after=Pos+1)
          Setts <- append(Setts,sprintf('#SBATCH --mem=%s',gsub('GB','G',NewSetts$Mem)), after=Pos+1)
          Setts <- append(Setts,sprintf('#SBATCH --cpus-per-task=%s',NewSetts$CPU+MoreCPUs), after=Pos+1)
          Setts <- append(Setts,'#SBATCH --ntasks-per-node=1', after=Pos+1)
          Setts <- append(Setts,sprintf('#SBATCH --nodes=%s',NJobs), after=Pos+1)
        }
        
        writeLines(Setts,paste(exdir,'Apsim.pbs',sep='/'))
        
        if (file.exists(paste0(exdir,'/CondorApsim.xml'))) x <- file.remove(paste0(exdir,'/CondorApsim.xml'))
        x <- file.remove(list.files(path=exdir,pattern='*.bat',full.names=T))
        x <- file.remove(list.files(path=exdir,pattern='*.sub',full.names=T))
        
        x <- zip::zipr(zipfile=xNameZip,files=list.files(exdir,full.names=T))
        x <- unlink(exdir,force=T,recursive=T)
      }
      
      if (HostUni=='Bunya') {
        message('Creating batch file for Bunya')
        
        BatComm <- sprintf('scp %s %s:%s', shQuote(xNameZip,type='cmd'), UserAtServer, shQuote(RemDir$Project,type='sh'))
        
        BatComm <- append(BatComm,'')
        BatComm <- append(BatComm, paste0(sprintf('ssh %s "', UserAtServer),
                                          sprintf('rm -rf %s ; ', shQuote(RemDir$Run,type='sh')),
                                          sprintf('mkdir -p %s', shQuote(RemDir$Run,type='sh')),
                                          sprintf('cd %s ; ', shQuote(RemDir$Project,type='sh')),
                                          sprintf('unzip %s -d %s ; ', basename(xNameZip), shQuote(RemDir$Run,type='sh')),
                                          sprintf('rm -f %s ; ', shQuote(basename(xNameZip),type='sh')),
                                          sprintf('find %s -name *.bat -type f -delete ; ', shQuote(RemDir$Run,type='sh')),
                                          sprintf('cd %s ; ', shQuote(RemDir$Run,type='sh')),
                                          sprintf('dos2unix ./Apsim.pbs ; '),
                                          sprintf('dos2unix ./Apsim.singularity*.sh ; '),
                                          ifelse(RunOnServ, 'sh Apsim.pbs', ''),
                                          '"'))
        
        BatComm <- append(BatComm,'')
        BatComm <- append(BatComm,'pause')
        writeLines(BatComm,paste(Dir$Project,'BunyaRun.bat',sep='/'))
      }
      
      if (Upload) {
        if (HostUni!='Bunya') {
          message('Creating folders on the server')
          x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                             command=c(paste('rm -r',shQuote(RemDir$Run)),paste('mkdir -p',shQuote(RemDir$Run))))
          
          message('Uploading the tar file')
          x <- scp_upload(session=sscon,files=xNameZip,to=RemDir$Project,verbose=F)
          
          message('Unpacking the tar file')
          x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                             command=c(paste('cd',shQuote(RemDir$Project)),paste('rm -rf',shQuote(RemDir$Run)),
                                       paste('unzip',basename(xNameZip),'-d',shQuote(RemDir$Run)),
                                       sprintf('find %s -name "*.bat" -type f -delete',shQuote(RemDir$Run)),
                                       paste('rm -f',basename(xNameZip))))
          
          x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                             command=c(paste('cd',shQuote(RemDir$Run)),'dos2unix ./Apsim.pbs'))
          
          x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                             command=c(paste('cd',shQuote(RemDir$Run)),'dos2unix ./Apsim.singularity*.sh'))
          
        } else {
          
        }
      }
      
      if (RunOnServ && HostUni!='Bunya') {
        message('Submitting the jobs')
        x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                           command=c(paste('cd',shQuote(RemDir$Run)),'sh Apsim.pbs'))
      }
    }
  }
  
  if ((Upload || RunOnServ) && HostUni!='Bunya') {
    LineX()
    message('Closing the connection ...')
    x <- ssh_disconnect(sscon)
    LineX()
  }
}

#==============================================================================================
#==============================================================================================

APSIMNG_Bundle <- function(Dir,ModelsExePath,Singularity,ApsimExec,NewSetts,NJobs=NULL,SimsPerJob=NULL,Zip=T,MoreCPUs=0,Email,
                           ProjectGroup,Ext='apsimx',PreProcR=NULL,PreProcRArgs=NULL,PostProcR=NULL,PostProcRArgs=NULL,PrePostFiles=NULL,
                           SimilarJobs=F,Playlist=NULL,ConfigFile=NULL) {
  
  TempFold <- paste(Dir,'Temp',sep='/')
  if(!dir.exists(TempFold)) dir.create(TempFold,recursive=T,showWarnings=F)
  
  ApFiles <- list.files(Dir,pattern=sprintf('*.%s$',Ext),full.names=T,recursive=F)
  Files2  <- JobsList <- NULL; AllSimNames <- data.table()
  
  LineX()
  for (ApFile in ApFiles) {
    SimNames    <- system2(ModelsExePath, stdout=T, args=c(shQuote(ApFile), '--list-simulations'))
    # SimNames    <- paste0(paste0('^', SimNames), "$")
    AllSimNames <- rbind(AllSimNames, data.table(ApFile=ApFile, Simulations=SimNames))
    message(sprintf('%s simulations found in %s (%s/%s)', length(SimNames), basename(ApFile), which(ApFiles==ApFile), length(ApFiles)))
  }
  LineX()
  
  if (SimilarJobs==F) {
    # Regular PBS jobs
    withPlayList <- (!is.null(Playlist))
    
    if (is.null(NJobs)) {
      if (!is.null(SimsPerJob)) {
        NJobs <- nrow(AllSimNames) %/% SimsPerJob
        if (NJobs * SimsPerJob < nrow(AllSimNames)) NJobs <- NJobs + 1
        
      } else {
        stop("At least one of 'NJobs' or 'SimsPerJob' must be non-NULL")
      }
    }
    AllSimIDs <- MySplit(1:nrow(AllSimNames), NGroups=NJobs)
    names(AllSimIDs) <- as.character(seq(1,length(AllSimIDs)))
    
  } else {
    # All jobs will run all simulations but with different inputs, which are generated by PreProcR.
    if (is.null(NJobs) || is.null(PreProcR)) stop("NJobs and PreProcR cannot be NULL when SimilarJobs=TRUE!")
    
    AllSimIDs <- list()
    for (n in 1:NJobs) AllSimIDs[[as.character(n)]] <- 1:nrow(AllSimNames)
  }
  
  # Creating jobs.
  message('Creating jobs and singularity commands')
  
  for (JobID in names(AllSimIDs)) {
    JobName <- paste0('APSIMNextGen.', JobID)
    JobSims <- AllSimNames[AllSimIDs[[JobID]]]
    ApFiles <- unique(JobSims$ApFile)
    
    # This is the file with the singularity command to run Models.exe.
    CommandFile <- sprintf('Singularity.Command.%s.sh', JobID)
    
    AllFiles   <- CommandFile
    AllApFiles <- NULL
    
    for (iApFile in ApFiles) {
      ApLines    <- readLines(iApFile,warn=F)
      NewApFile  <- paste0('Job', JobID, '_', basename(file_path_sans_ext(iApFile)), '.', file_ext(iApFile))
      AllFiles   <- c(AllFiles, NewApFile)
      AllApFiles <- c(AllApFiles, NewApFile)
      ApSims     <- paste(JobSims[ApFile==iApFile, Simulations], collapse='\\\\n')
      
      if (SimilarJobs==F) {
        # Add a Playlist, if needed.
        PlLine <- grep('Models.Playlist', ApLines)
        
        if (withPlayList==T) {
          if (length(PlLine)==0) stop(sprintf('No playlist exists in %s!', basename(iApFile)))
          
          plExists <- F
          for (pl in PlLine) {
            if (any(grepl(sprintf('"Name":.*"%s"', Playlist), ApLines))) {
              plExists <- T
              break
            }
          }
          if (plExists==F) stop(sprintf('The "%s" playlist does not exist in %s!', Playlist, basename(iApFile)))
          
        } else {
          
          Playlist <- 'TemporaryPL'
          PlLine   <- grep('Children', ApLines)[1]
          
          ApLines <- append(ApLines, '    },', after=PlLine)
          ApLines <- append(ApLines, '      "ReadOnly": false', after=PlLine)
          ApLines <- append(ApLines, '      "Enabled": true,', after=PlLine)
          ApLines <- append(ApLines, '      "Children": [],', after=PlLine)
          ApLines <- append(ApLines, '      "ResourceName": null,', after=PlLine)
          ApLines <- append(ApLines, '      "Name": "TemporaryPL",', after=PlLine)
          ApLines <- append(ApLines, '      "Text": "*",', after=PlLine)
          ApLines <- append(ApLines, '      "$type": "Models.Playlist, Models",', after=PlLine)
          ApLines <- append(ApLines, '    {', after=PlLine)
          PlLine <- grep('Models.Playlist', ApLines)[1]
          
          ApLines[PlLine+1] <- gsub('"Text": .*', sprintf('"Text": "%s",',ApSims), ApLines[PlLine+1])
          ApLines[PlLine+5] <- gsub('"Enabled": .*', '"Enabled": true,', ApLines[PlLine+5])
        }
      }
      
      # Extract other referenced files and make their paths relative
      {
        OtherFiles <- system2(ModelsExePath, stdout=T, args=c(shQuote(ApFile), '--list-referenced-filenames'))
        
        for (File in OtherFiles) {
          LineFi <- grep(paste0('FileName.*',basename(File)), ApLines)
          file.copy(File, TempFold)
          File  <- basename(File)
          ApLines[LineFi] <- gsub('"FileName": .*', sprintf('"FileName": "%s",',File), ApLines[LineFi])
          AllFiles <- c(AllFiles, File)
        }
      }
      
      # Write the updated APSIM file
      writeLines(ApLines, con=paste(TempFold, NewApFile, sep='/'))
    }
    
    # Add pre-processing and post-processing R scripts, if any, to the list of input files.
    {
      if (!is.null(PreProcR)) {
        file.copy(PreProcR, TempFold)
        AllFiles <- c(AllFiles, basename(PreProcR))
      }
      if (!is.null(PostProcR)) {
        file.copy(PostProcR, TempFold)
        AllFiles <- c(AllFiles, basename(PostProcR))
      }
      if (!is.null(PrePostFiles) && (!is.null(PreProcR) || !is.null(PostProcR))) {
        file.copy(PrePostFiles, TempFold)
        AllFiles <- c(AllFiles, basename(PrePostFiles))
      }
      if (!is.null(PreProcRArgs)) {
        PreProcRArgs <- paste(as.character(PreProcRArgs))
      }
      if (!is.null(PostProcRArgs)) {
        PostProcRArgs <- paste(as.character(PostProcRArgs))
      }
    }
    
    # Write singularity commands in files.
    {
      CPUnFlag <- paste('--cpu-count', NewSetts$CPU)
      Command  <- c('echo Running on `hostname -f` at `date`', 'echo Current folder: $TMPDIR')
      
      if (is.null(ConfigFile)) {
        for (iApFile in AllApFiles) {
          Command <- append(Command, sprintf('singularity exec -B $TMPDIR:/TMPDIR --pwd /TMPDIR %s %s %s %s', Singularity, ApsimExec, iApFile, CPUnFlag))
        }
        
      } else {
        if (is.null(PreProcR)) stop("PreProcR cannot be NULL when ConfigFile is not NULL!")
        
        if (grepl('--csv',ApsimExec)) {
          Command <- append(Command, sprintf('singularity exec -B $TMPDIR:/TMPDIR --pwd /TMPDIR %s %s --apply %s --csv %s', Singularity, gsub(' --csv','',ApsimExec), ConfigFile, CPUnFlag))
        } else {
          Command <- append(Command, sprintf('singularity exec -B $TMPDIR:/TMPDIR --pwd /TMPDIR %s %s --apply %s %s', Singularity, ApsimExec, ConfigFile, CPUnFlag))
        }
      }
      
      if (!is.null(Playlist)) Command[3:length(Command)] <- paste(Command[3:length(Command)], '--playlist', Playlist)
      
      Command <- append(Command, 'echo APSIM simulations completed!')
      Command <- append(Command, 'echo Finished at `date`')
      
      writeLines(Command, con=paste(TempFold, CommandFile, sep='/'))
    }
    
    # Create one line/job and ad it to the lost of jobs.
    {
      AllFiles <- paste(AllFiles, collapse=',')
      Job      <- paste(JobName, AllFiles, CommandFile, sep='|')
      JobsList <- c(JobsList, Job)
    }
  }
  JobRandID <- as.character(as.integer(runif(1, 1e8, 1e9)))
  
  message('Creating PBS scripts')
  
  PBSScript <- c('#!/bin/bash', '# Construct a PBSPro job for each APSIM NextGen job.',
                 sprintf('cat <<EOF | /opt/pbs/bin/qsub %s', ifelse(NJobs==1,'',sprintf('-J 1-%s', NJobs))),
                 '############################################',
                 '#PBS -S /bin/bash',
                 '#PBS -N APSIMNextGen',
                 sprintf('#PBS -l select=1:ncpus=%s:mem=%s', NewSetts$CPU+MoreCPUs, NewSetts$Mem),
                 sprintf('#PBS -l walltime=%s', NewSetts$WallT),
                 '#PBS -m e',
                 sprintf('#PBS -M %s', Email),
                 '#PBS -q default',
                 sprintf('#PBS -P %s', ProjectGroup),
                 '############################################',
                 if (NJobs==1) ' PBS_ARRAY_INDEX=1' else NULL,
                 sprintf(' TMPDIR=/sandisk1/u8019357/Job_%s_\\$PBS_ARRAY_INDEX', JobRandID),
                 ' mkdir -p \\$TMPDIR',
                 ' cd \\$TMPDIR',
                 '############################################',
                 sprintf(' export NUMBER_OF_PROCESSORS=%s', NewSetts$CPU+MoreCPUs),
                 sprintf(' export SINGULARITYENV_NUMBER_OF_PROCESSORS=%s',NewSetts$CPU),
                 ' export SINGULARITY_TMPDIR=$TMPDIR',
                 ' export SINGULARITY_LOCALCACHEDIR=$TMPDIR',
                 ' export SINGULARITY_CACHEDIR=$TMPDIR',
                 ' export SINGULARITYENV_R_LIBS_USER=$HOME/R',
                 ' module load singularity/3.7.0-gcc8.3.1-fv5',
                 if (!is.null(PreProcR) || !is.null(PostProcR)) ' module load udunits/2.2.28-gcc-t3a' else NULL,
                 if (!is.null(PreProcR) || !is.null(PostProcR)) ' module load r/4.3.0-gcc-py3-jyj' else NULL,
                 '############################################',
                 " mapfile -t joblist <<'XXXXXX'",
                 JobsList,
                 'XXXXXX',
                 ' jobname=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX-1]} | cut -d\\| -f1)',
                 ' inputfiles=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX-1]} | cut -d\\| -f2)',
                 ' command=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX-1]} | cut -d\\| -f3)',
                 " IFS=','",
                 '  for x in \\$inputfiles ; do cp "\\$PBS_O_WORKDIR/\\$x" ./ ; done',
                 ' unset IFS',
                 '############################################',
                 if(!is.null(PreProcR)) sprintf(' Rscript "%s" %s \\$PBS_ARRAY_INDEX \\$TMPDIR %s &', basename(PreProcR), NJobs, PreProcRArgs) else NULL,
                 if(!is.null(PreProcR)) ' wait' else NULL,
                 if(!is.null(PreProcR)) '############################################' else NULL,
                 ' dos2unix -q \\$command',
                 ' chmod +x \\$command; touch \\$command',
                 ' ./\\$command',
                 '############################################',
                 if(!is.null(PostProcR)) sprintf(' Rscript "%s" %s \\$PBS_ARRAY_INDEX \\$TMPDIR %s &', basename(PostProcR), NJobs, PostProcRArgs) else NULL,
                 if(!is.null(PostProcR)) ' wait' else NULL,
                 if(!is.null(PostProcR)) '############################################' else NULL,
                 ' find . -name "*.sum|*.apsimx" -exec rm {} \\;',
                 ' find . -maxdepth 1 -type f -newer \\$command -print | tar cfz \\$PBS_O_WORKDIR/\\$jobname.output.tar.gz --remove-files --files-from -',
                 '############################################',
                 ' rm -r \\$TMPDIR',
                 'EOF')
  
  writeLines(PBSScript, con=paste0(TempFold,'/Apsim.pbs'))
  
  if (Zip) {
    message('Bundle files in a zip file')
    setwd(TempFold)
    zip::zipr(zipfile=paste0(Dir,'/Bundle.zip'),files=list.files(getwd(),full.names=F))
  }
  LineX()
  
  setwd(Dir)
  unlink(TempFold,recursive=T,force=T)
}

#==============================================================================================
#==============================================================================================

APSIMNG_Bundle_OLD <- function(Dir,Ext='apsimx',Zip=T) {
  
  TempFold <- paste(Dir,'Temp',sep='/')
  if(!dir.exists(TempFold)) dir.create(TempFold,recursive=T,showWarnings=F)
  
  Files  <- list.files(Dir,pattern=sprintf('*.%s$',Ext),full.names=T,recursive=F)
  Files2 <- JobsList <- NULL
  
  for (File in Files) {
    Lines <- readLines(File,warn=F)
    Job <- basename(File)
    
    # Find and modify paths to weather files and copy the files into the same folder as the APSIM-NG file.
    
    MetLines <- APSIMNG_Extract_Block_from_JSON(Input=Lines,Type='Models.Climate.Weather',Attrs=NULL,Values=NULL,ReturnLines=T)
    
    if (!is.list(MetLines)) MetLines <- list(MetLines)
    
    for (Ls in MetLines) {
      LineNs <- Ls[1]:Ls[2]
      LineFi <- LineNs[grep('FileName', Lines[LineNs])]
      AddFile <- gsub('\\"FileName\\": ([[:print:]]+)', '\\1', Lines[LineFi])
      AddFile <- trimws(gsub('"|,','',AddFile))
      file.copy(AddFile, TempFold)
      AddFile  <- basename(AddFile)
      Lines[LineFi] <- gsub('"FileName": .*', sprintf('"FileName": "%s",',AddFile), Lines[LineFi])
      Job <- paste(Job, basename(AddFile), sep='|')
    }
    JobsList <- c(JobsList, Job)
    
    File2 <- paste(TempFold, basename(File), sep='/')
    writeLines(Lines, File2)
    Files2 <- c(Files2, File2)
  }
  writeLines(JobsList,con=paste0(TempFold,'/Jobs.list'))
  
  if (Zip) {
    setwd(TempFold)
    zip::zipr(zipfile=paste0(Dir,'/Bundle.zip'),files=list.files(getwd(),full.names=F))
  }
  setwd(Dir)
  unlink(TempFold,recursive=T,force=T)
}

#==============================================================================================
#==============================================================================================

APSIM_Run_Read <- function(Settings=NULL,NJobs=1,JobID=1,TempFold=NULL) {
  
  if (is.character(Settings)) source(Settings)
  
  List <- c('OnServer','APSIMConsolePath','MainDir','IsApsimNG','Projects','RunNames','Analyses',
            'SaveToRunFold','Pattern','OutExtension','Extension','OutPattern',
            'RunSet','SFilePatt','DFilePatt','SowYearCol','SaveInOneFile','JustLoad','RunApsim','RemOldOuts','Mapping',
            'Factors','ReadTars','RunFacts','NoSumm','UseListFile','InfoInFileName','CheckHPC')
  
  if (!'RunNames'%in%names(Settings)) Settings$RunNames <- NA
  if (!'InfoInFileName'%in%names(Settings)) Settings$InfoInFileName <- F
  if (!'SaveToRunFold'%in%names(Settings)) Settings$SaveToRunFold <- T
  if (!'Analyses'%in%names(Settings)) Settings$Analyses <- NA
  if (!'RunSet'%in%names(Settings)) Settings$RunSet <- NA
  if (!'Pattern'%in%names(Settings)) Settings$Pattern <- '*'
  if (!'OutPattern'%in%names(Settings)) Settings$OutPattern <- 'Outputs'
  if (!'CheckHPC'%in%names(Settings)) Settings$CheckHPC <- T
  
  if (any(!List%in%names(Settings))) print(sprintf('%s not found in Settings!',paste(setdiff(List,names(Settings)),collapse='|')))
  
  for (Name in names(Settings))
    if (length(Settings[[Name]])==1 && is.na(Settings[[Name]])) assign(Name,NULL)
  for (Name in names(Settings))
    if (length(Settings[[Name]])>1 || (length(Settings[[Name]])==1 && !is.na(Settings[[Name]]))) assign(Name,Settings[[Name]])
  
  if (is.null(Analyses) || is.na(Analyses)) SaveToRunFold <- T
  
  if (IsApsimNG) {
    UseListFile <- F
    NoSumm      <- T
    RunFacts    <- T
    InfoInFileName <- F
  }
  OutPrefix <- sprintf('%s%s',OutPattern,ifelse(NJobs>1,JobID,''))
  
  if (OnServer && !is.null(RunSet) && !is.na(RunSet)) RunSet <- gsub('\\s+|\\.','_',RunSet)
  
  #========================================================================================================
  
  for (Pr in Projects) {
    
    if (is.null(RunNames) || is.na(RunNames)) {
      Dir <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=RunSet,Run=NULL,MainDir=MainDir,Analysis=NULL)
      RunNames <- list.dirs(path=Dir$RunSet,full.names=F,recursive=F)
      
    } else {
      if (OnServer) for (i in 1:length(RunNames)) RunNames[i] <- gsub('\\s+|\\.','_',RunNames[i])
    }
    
    if (length(RunNames)>1 && !SaveToRunFold && length(Analyses)==1) Analyses <- rep(Analyses,length(RunNames))
    
    LineX(Lines=2)
    message(paste('Project:',Pr))
    message(paste0('Output file: ',OutPrefix))
    LineX()
    
    for (Rn in RunNames) {
      LineX()
      x <- gc()
      RunID <- which(RunNames==Rn)
      message(paste0('Run',which(RunNames==Rn),': ',Rn))
      LineX()
      
      setwd(MainDir)
      Dir <- APSIM_Paths2(Dir=list(),Project=Pr,RunSet=RunSet,Run=Rn,MainDir=MainDir,Analysis=if (SaveToRunFold) NULL else Analyses[RunID])
      x <- sapply(Dir, function(x) {if(!dir.exists(x)) dir.create(x,recursive=T,showWarnings=F)})
      
      # Running the selected Sim files
      if (RunApsim) {
        
        # Identifying the Sim files and dividing them into two groups
        if (!is.null(TempFold) && file.exists(paste0(Dir$Run,'/Jobs.list'))) {
          Jobs1 <- readLines(paste0(Dir$Run,'/Jobs.list'))
          Jobs  <- MySplit(Jobs1,NGroups=NJobs,Which=JobID)
          
          if (length(Jobs)==0) {
            message('Not enough jobs found!')
            next()
            
          } else {
            Jobs <- str_split(Jobs,pattern='\\|')
            NewSimFiles <- OtherFiles <- NULL
            for (Job in Jobs) {
              NewSimFiles <- c(NewSimFiles,Job[1])
              OtherFiles  <- c(OtherFiles,Job[-1])
            }
            NewSimFiles <- paste(Dir$Run,NewSimFiles,sep='/')
            OtherFiles  <- paste(Dir$Run,OtherFiles,sep='/')
          }
          
        } else {
          NewSimFiles1 <- list.files(path=Dir$Run,pattern=sprintf('%s.%s$',ifelse(is.null(Pattern),'*',Pattern),Extension),full.names=T)
          NewSimFiles  <- MySplit(NewSimFiles1,NGroups=NJobs,Which=JobID)
          OtherFiles   <- NULL
          
          if (length(NewSimFiles)==0) {
            message('Not enough apsim files found!')
            next()
          }
        }
        message(paste(as.character(length(NewSimFiles)),
                      sprintf("%s been selected to run on this computer", ifelse(length(NewSimFiles)==1, 'simulation has', 'simulations have'))))
        
        if (RemOldOuts) {
          message("Removing old output files")
          OutFiles <- list.files(path=Dir$Run,patter='*.db$|*.csv$|.out$|*.sum*')
          unlink(OutFiles,recursive=T,force=T)
        }
        
        LineX()
        TempFoldJob <- Dir$Run
        if (!is.null(TempFold)) {
          TempFoldJob <- paste0(TempFold,'/Job',JobID)
          if (!dir.exists(TempFoldJob)) unlink(TempFoldJob,recursive=T,force=T)
          dir.create(TempFoldJob,recursive=T,showWarnings=F)
          x <- file.copy(from=NewSimFiles,to=TempFoldJob,overwrite=T)
          if (!is.null(OtherFiles)) x <- file.copy(from=OtherFiles,to=TempFoldJob,overwrite=T)
          NewSimFiles <- gsub(Dir$Run,TempFoldJob,NewSimFiles)
        }
        
        APSIM_Run_All_Sim_Files(SimFiles=NewSimFiles,Ext=Extension,NJobs=NULL,JobID=NULL,NoSumm=NoSumm,
                                OnServer=OnServer,RunFacts=RunFacts,UseListFile=UseListFile,
                                APSIMConsolePath=APSIMConsolePath,IsApsimNG=IsApsimNG)
        
        if (ReadTars) {
          if (is.null(TempFoldJob)) TempFoldJob <- Dir$Run
          setwd(TempFoldJob)
          OutFiles <- list.files(TempFoldJob,patter='*.db$|*.csv$|*.out$',full.names=F)
          if (length(OutFiles)>0) {
            x <- tar(tarfile=paste0(TempFoldJob,'/',basename(TempFoldJob),'.tar.gz'),files=OutFiles,tar='tar')
            x <- unlink(OutFiles,recursive=T,force=T)
            ReadTars <- T
          }
          if (TempFoldJob!=Dir$Run) x <- file.copy(from=list.files(TempFoldJob,patter='*.db$|*.csv$|*.out$|*.gz$',full.names=T),to=Dir$Run,overwrite=T)
        }
        LineX()
        
        OutFiles1 <- list.files(path=ifelse(RunApsim && !is.null(TempFold),TempFoldJob,Dir$Run), patter='*.db$|*.csv$|*.out$|*.gz$', full.names=T)
        OutFiles  <- OutFiles1
        
      } else {
        OutFiles1 <- list.files(path=Dir$Run,patter='*.db$|*.csv$|*.out$|*.gz$',full.names=T)
        OutFiles  <- MySplit(OutFiles1,NGroups=NJobs,Which=JobID)
      }
      
      if (CheckHPC==TRUE && JobID==1 && !IsApsimNG) {
        message("Checking the status of APSIM jobs")
        if (file.exists(paste(Dir$Run,'FailedJobs.out',sep='/'))) unlink(paste(Dir$Run,'FailedJobs.out',sep='/'), recursive=T, force=T)
        
        apsimCheck <- APSIM_Check_HPC_Outputs(Dir$Run)
        
        if (!is.null(apsimCheck$failedJobs) && length(apsimCheck$failedJobs)>0) {
          Temp <- c(paste('Number of files checked:', apsimCheck$nFiles),
                    paste('JobID of the failed jobs:', paste(apsimCheck$failedJobs, collapse=', ')),
                    '', apsimCheck$failedFiles)
          
          writeLines(Temp, con=paste(Dir$Run,'FailedJobs.out',sep='/'))
          warning(paste('Number of files checked:', apsimCheck$nFiles))
          warning(paste('JobID of the jobs that failed:', paste(apsimCheck$failedJobs, collapse=', ')))
        }
      }
      
      if (JustLoad) {
        message("Importing the seasonal and daily outputs")
        load(file=paste0(Dir$Outputs,OutFile1))
        
      } else {
        message("Reading the seasonal and daily output files")
        
        if (IsApsimNG) {
          # Read outputs of APSIM-NextGen
          HasSeas  <- HasDaily <- F
          SeasOuts <- DailyOuts <- Simulations <- list()
          Factors  <- NULL
          
          if (tolower(OutExtension)=='db') {
            # When outputs are in DB files
            Outputs <- APSIMNG_Read_All_Output_Files(OutFiles=OutFiles,Ext='db',STable=SFilePatt,DTable=DFilePatt,OnlyFullSeasons=T,
                                                     OnlyInSeason=T,SowYearCol=SowYearCol,NJobs=NULL,JobID=NULL,TempFold=TempFold)
            
            for (Fi in 1:length(Outputs)) {
              xSTable <- xDTable <- NULL
              if (!is.null(SFilePatt) && !is.na(SFilePatt) && SFilePatt!='NULL') xSTable <- intersect(SFilePatt,names(Outputs[[Fi]]))
              if (!is.null(DFilePatt) && !is.na(DFilePatt) && DFilePatt!='NULL') xDTable <- intersect(DFilePatt,names(Outputs[[Fi]]))
              
              if (!is.null(xSTable)) SeasOuts[[Fi]]  <- Outputs[[Fi]][[xSTable]]
              if (!is.null(xDTable)) DailyOuts[[Fi]] <- Outputs[[Fi]][[xDTable]]
              Simulations[[Fi]] <- Outputs[[Fi]][['_Simulations']]
              
              if ('_Factors' %in% names(Outputs[[Fi]])) Factors <- sort(c(Factors,unique(Outputs[[Fi]][['_Factors']]$FactorName)))
            }
            Simulations <- rbindlist(Simulations,fill=T)
            
          } else if (tolower(OutExtension)=='csv') {
            # When outputs are in CSV files
            OutputsSeas <- OutputsDaily <- list()
            
            if (!is.null(SFilePatt) && !is.na(SFilePatt) && SFilePatt!='NULL') {
              OutputsSeas <- APSIMNG_Read_All_Output_Files(OutFiles=OutFiles,Ext='csv',STable=SFilePatt,DTable=NULL,OnlyFullSeasons=T,
                                                           OnlyInSeason=T,SowYearCol=SowYearCol,NJobs=NULL,JobID=NULL,TempFold=TempFold)
            }
            
            if (!is.null(DFilePatt) && !is.na(DFilePatt) && DFilePatt!='NULL') {
              OutputsDaily <- APSIMNG_Read_All_Output_Files(OutFiles=OutFiles,Ext='csv',STable=NULL,DTable=DFilePatt,OnlyFullSeasons=T,
                                                            OnlyInSeason=T,SowYearCol=SowYearCol,NJobs=NULL,JobID=NULL,TempFold=TempFold)
            }
            Count <- max(length(OutputsSeas), length(OutputsDaily))
            
            for (Fi in 1:Count) {
              xSTable <- xDTable <- NULL
              if (!is.null(SFilePatt) && !is.na(SFilePatt) && SFilePatt!='NULL') xSTable <- intersect(SFilePatt,names(OutputsSeas[[Fi]]))
              if (!is.null(DFilePatt) && !is.na(DFilePatt) && DFilePatt!='NULL') xDTable <- intersect(DFilePatt,names(OutputsDaily[[Fi]]))
              
              if (!is.null(xSTable)) SeasOuts[[Fi]]  <- OutputsSeas[[Fi]][[xSTable]]
              if (!is.null(xDTable)) DailyOuts[[Fi]] <- OutputsDaily[[Fi]][[xDTable]]
            }
            
            rm(list=c('OutputsSeas','OutputsDaily'))
            x <- gc()
          }
          
          message("Putting seasonal and daily tables together")
          
          if (length(SeasOuts)>0) {
            SeasOuts  <- rbindlist(SeasOuts,fill=T)
            SeasOuts[, `:=`(Project=Pr,Run=Rn)]
            SeasOuts <- setkeyv(SeasOuts,cols=intersect(c('Project','Run','File','SowYear','SimulationID'),names(DailyOuts)))
            SeasOuts <- SeasOuts[!is.na(SimulationID)]
            Factors  <- intersect(Factors,names(SeasOuts))
          }
          
          if (length(DailyOuts)>0) {
            DailyOuts <- rbindlist(DailyOuts,fill=T)
            DailyOuts[, `:=`(Project=Pr,Run=Rn)]
            DailyOuts <- setkeyv(DailyOuts,cols=intersect(c('Project','Run','File','SimulationID','SowYear'),names(DailyOuts)))
            DailyOuts <- DailyOuts[!is.na(SimulationID)]
          }
          
          message("Saving the raw outputs")
          if (is.null(TempFold)) Folder <- ifelse(SaveToRunFold,Dir$Run,Dir$Analyses) else Folder <- TempFold
          
          SaveFiles <- MySave(Prefix=OutPrefix,Folder=Folder,What=c('SeasOuts','DailyOuts'),
                              WhatInOneFile=c('Factors','Simulations'),AllInOneFile=SaveInOneFile,Env=environment())
          
          rm(list=c('DailyOuts','SeasOuts'))
          x <- gc()
          
          if (!is.null(TempFold) && !is.null(SaveFiles)) {
            message("Moving RData files from TempFold to Run/Analyses folder")
            x <- file.copy(from=SaveFiles,to=ifelse(SaveToRunFold,Dir$Run,Dir$Analyses),overwrite=T)
            x <- unlink(SaveFiles,recursive=T,force=T)
            x <- gc()
          }
          
        } else {
          # Read outputs of classic APSIM
          SeasOuts <- DailyOuts <- Tars <- list()
          HasSeas  <- HasDaily  <- F
          
          if (ReadTars) Tars <- grep(patter='*.tar$|*.gz$',OutFiles,value=T)
          
          if (length(Tars)>0) {
            LineX()
            Tars1 <- list.files(path=Dir$Run,pattern='*.gz$',full.names=T)
            Tars  <- MySplit(Tars1,NGroups=NJobs,Which=JobID)
            
            if (is.null(Tars) || length(Tars)==0) {
              message('Not enough tar files found!')
              next()
              
            } else {
              
              FUNC <- function(trFile,Type='s') {
                if (Type=='s') message(sprintf('Processing seasonal tar file %s/%s: %s',which(Tars==trFile),length(Tars),File_Base_Name(trFile)))
                if (Type=='d') message(sprintf('Processing daily tar file %s/%s: %s',which(Tars==trFile),length(Tars),File_Base_Name(trFile)))
                
                if (is.null(TempFold)) {
                  xDir <- paste(dirname(trFile),File_Base_Name(trFile),sep='/')
                } else {
                  xDir <- paste(TempFold,File_Base_Name(trFile),sep='/')
                }
                untar(trFile,exdir=xDir)
                x <- gc()
                
                if (Type=='s') Out <- APSIM_Read_All_Output_Files(OutFiles=xDir,Ext='out',Pattern=SFilePatt,NJobs=NULL,JobID=NULL,OnlyFullSeasons=F)
                if (Type=='d') Out <- APSIM_Read_All_Output_Files(OutFiles=xDir,Ext='out',Pattern=DFilePatt,NJobs=NULL,JobID=NULL,OnlyFullSeasons=F)
                
                unlink(xDir,force=T,recursive=T)
                x <- gc()
                
                if (length(Out)>0) {
                  for (i in 1:length(Out)) Out[[i]]$TarFile <- File_Base_Name(trFile)
                }
                return(Out)
              }
              
              if (!is.null(SFilePatt) && !is.na(SFilePatt)) {
                SeasOuts  <- sapply(Tars,FUNC,Type='s',simplify=F,USE.NAMES=F)
                SeasOuts  <- do.call(c,SeasOuts)
                x <- gc(); LineX()
              }
              if (!is.null(DFilePatt) && !is.na(DFilePatt)) {
                DailyOuts <- sapply(Tars,FUNC,Type='d',simplify=F,USE.NAMES=F)
                DailyOuts <- do.call(c,DailyOuts)
                x <- gc(); LineX()
              }
            }
            
          } else {
            
            if (length(OutFiles)==0) {
              message('Not enough output files found!')
              
            } else {
              
              if (!is.null(SFilePatt) && !is.na(SFilePatt))
                SeasOuts  <- APSIM_Read_All_Output_Files(OutFiles=OutFiles,Ext='out',Pattern=SFilePatt,NJobs=NULL,JobID=NULL,OnlyFullSeasons=F)
              
              if (!is.null(DFilePatt) && !is.na(DFilePatt))
                DailyOuts <- APSIM_Read_All_Output_Files(OutFiles=OutFiles,Ext='out',Pattern=DFilePatt,NJobs=NULL,JobID=NULL,OnlyFullSeasons=F)
            }
          }
          
          if (!is.null(SFilePatt) && !is.na(SFilePatt) && !is.null(SeasOuts) && length(SeasOuts)>0) HasSeas <- T
          if (!is.null(DFilePatt) && !is.na(DFilePatt) && !is.null(DailyOuts) && length(DailyOuts)>0) HasDaily <- T
          
          if (HasSeas || HasDaily) {
            
            if (HasSeas && HasDaily) {
              
              message("Checking consistency between daily and seasonal outputs")
              for (i in 1:length(SeasOuts)) {
                
                if (InfoInFileName) {
                  x <- trimws(gsub(SFilePatt,'',names(SeasOuts)[i]))
                  
                  if (Extension=='sim')
                    y <- strsplit(x,'_')[[1]]
                  else
                    y <- strsplit(x,';')[[1]]
                  
                  if ((Extension=='sim' && length(y)==2)) Paddock <- y[2] else Paddock <- NULL
                  
                  j <- NULL
                  if (!is.null(DailyOuts) && length(DailyOuts)>0) {
                    if (Extension=='sim')
                      j <- grep(paste0('^',trimws(x),DFilePatt),names(DailyOuts))
                    else
                      j <- grep(paste0('^',trimws(x),' ',DFilePatt),names(DailyOuts))
                  }
                  
                  x <- strsplit(x,';')[[1]][1]
                  
                  SeasOuts[[i]][, `:=`(Project=Pr,Run=Rn,SimID=trimws(x))]
                  if (!is.null(Paddock)) SeasOuts[[i]][, `:=`(Paddock=Paddock)]
                  if (!'SowYear'%in%colnames(SeasOuts[[i]])) SeasOuts[[i]]$SowYear <- SeasOuts[[i]]$year
                  
                  if (length(j)==1 && HasDaily) {
                    DailyOuts[[j]][, `:=`(Project=Pr,Run=Rn,SimID=trimws(x))]
                    if (!is.null(Paddock)) DailyOuts[[j]][, `:=`(Paddock=Paddock)]
                    if (!'SowYear'%in%colnames(DailyOuts[[j]])) DailyOuts[[j]]$SowYear <- DailyOuts[[j]]$year
                    
                    # Removing the years without seasonal data from the daily outputs
                    DailyOuts[[j]] <- DailyOuts[[j]][SowYear%in%unique(SeasOuts[[i]]$SowYear),]
                    # Removing the years without daily data from the seasonal outputs
                    SeasOuts[[i]] <- SeasOuts[[i]][SowYear%in%unique(DailyOuts[[j]]$SowYear),]
                  }
                  
                } else {
                  if (HasSeas) {
                    SeasOuts[[i]][, `:=`(Project=Pr,Run=Rn)]
                    if (!'SowYear'%in%colnames(SeasOuts[[i]])) SeasOuts[[i]]$SowYear <- SeasOuts[[i]]$year
                  }
                  if (HasDaily) {
                    DailyOuts[[i]][, `:=`(Project=Pr,Run=Rn)]
                    if (!'SowYear'%in%colnames(DailyOuts[[i]])) DailyOuts[[i]]$SowYear <- DailyOuts[[i]]$year
                  }
                  
                }
              }
            }
            
            message("Putting daily and seasonal outputs in data frames")
            
            if (HasSeas) SeasOuts  <- rbindlist(SeasOuts,fill=T)
            if (HasDaily) DailyOuts <- rbindlist(DailyOuts,fill=T)
            
            if (!is.null(Mapping) && !is.na(Mapping)) {
              if (HasSeas) SeasOuts  <- Mapping_Cols(SeasOuts,Mapping)
              if (HasDaily) DailyOuts <- Mapping_Cols(DailyOuts,Mapping)
              if (!is.null(Factors) && !is.na(Factors)) Factors <- Change_Levels(Factors,Mapping[,1],Mapping[,2])
            }
            
            if (!is.null(SimFilesFull) && !is.na(SimFilesFull)) {
              message('Extracting characteristics of simulations')
              SimFilesFull[,ID:=as.character(ID)]
              if (HasSeas) SeasOuts  <- merge(SeasOuts,SimFilesFull,by.x="SimID",by.y="ID",all.x=T,sort=F)
              if (HasDaily) DailyOuts <- merge(DailyOuts,SimFilesFull[,.(ID,Site)],by.x="SimID",by.y="ID",all.x=T,sort=F)
            }
            rownames(SeasOuts) <- rownames(DailyOuts) <- What <- NULL
            
            if (HasSeas) What <- c(What,'SeasOuts')
            if (HasDaily) What <- c(What,'DailyOuts')
            
            message("Saving the raw outputs")
            if (is.null(TempFold)) Folder <- ifelse(SaveToRunFold,Dir$Run,Dir$Analyses) else Folder <- TempFold
            if (!is.null(Factors)) WhatInOneFile <- 'Factors' else WhatInOneFile <- NULL
            
            SaveFiles <- MySave(Prefix=OutPrefix,Folder=Folder,What=What,WhatInOneFile=WhatInOneFile,AllInOneFile=SaveInOneFile,Env=environment())
            rm(list=What)
            x <- gc()
            
            if (!is.null(TempFold) && !is.null(SaveFiles)) {
              x <- file.copy(from=SaveFiles,to=ifelse(SaveToRunFold,Dir$Run,Dir$Analyses),overwrite=T)
              x <- unlink(SaveFiles,recursive=T,force=T)
              x <- gc()
            }
          }
        }
      }
    }
    LineX()
  }
  LineX()
}

#==============================================================================================
#==============================================================================================

APSIM_Paths <- function(Dir=list(),Project=NULL,Run=NULL,Analysis=NULL,RemMainDir=NULL) {
  
  OnServer <- !(as.character(Sys.info()['nodename'])=='QAAFI-JPNCBH2')
  if (OnServer) { MainDir <- RemMainDir
  } else { MainDir <- 'C:/Users/uqbababa/OneDrive - The University of Queensland/' }
  
  Dir$RCodes  <- paste(MainDir,'01_RScripts',sep="/")
  Dir$AllSims <- paste(MainDir,'00_Simulations',sep="/")
  Dir$Weather <- paste(MainDir,'03_WeatherData','60 Locations',sep="/")
  Dir$Data    <- paste(MainDir,'05_Data',sep='/')
  if (!OnServer) Dir$OnClust   <- paste(MainDir,"00_Simulations",'!OnCluster',sep='/')
  
  if (!is.null(Analysis)) {
    Dir$Analysis <- paste(MainDir,'04_Analyses',Analysis,sep='/')
  }
  if (!is.null(Project) && !is.null(Run)) {
    Dir$Project <- paste(Dir$AllSims,ifelse(OnServer,gsub('\\s+','.',Project),Project),sep="/")
    Dir$Run     <- paste(Dir$Project,ifelse(OnServer,gsub('\\s+','.',Run),Run),sep="/")
    Dir$Sims    <- paste(Dir$Run,'01_Sims',sep='/')
    Dir$Outputs <- paste(Dir$Run,'02_Outputs',sep='/')
    if (!OnServer) Dir$OnClustPr <- paste(Dir$OnClust,Project,sep='/')
  }
  
  return(Dir)
}

#==============================================================================================
#==============================================================================================

APSIM_Paths2 <- function(Dir=list(),Project=NULL,RunSet=NULL,Run=NULL,Analysis=NULL,MainDir=NULL,
                         WeatherFold='60 Locations',OnLinux=F,AnalysisForRun=F) {
  
  OnServer  <- (length(grep(':',MainDir))==0)
  
  Dir$AllProjects <- paste(MainDir,'00_Projects',sep="/")
  Dir$WeatherAll  <- paste(MainDir,'01_WeatherData',sep="/")
  Dir$Weather     <- paste(MainDir,'01_WeatherData',WeatherFold,sep="/")
  Dir$DataShared  <- paste(MainDir,'02_Data',sep='/')
  Dir$RShared     <- paste(Dir$AllProjects,'SharedScripts',sep="/")
  
  if (!OnServer) Dir$ClustZips <- paste(Dir$AllProjects,'ClusterZipFiles',sep="/")
  if ( OnServer) Dir$JobSubmit <- paste(Dir$AllProjects,'JobSubmit',sep="/")
  
  if (!is.null(Project)) {
    Dir$Project   <- paste(MainDir,'00_Projects',Project,sep="/")
    Dir$R         <- paste(Dir$Project,'01_R',sep="/")
    Dir$Sims      <- paste(Dir$Project,'02_Simulations',sep="/")
    Dir$RunSet    <- paste(Dir$Sims,RunSet,sep='/')
    Dir$Analyses  <- paste(Dir$Project,'03_Analyses',sep='/')
    Dir$Data      <- paste(Dir$Project,'04_Data',sep='/')
    Dir$Docs      <- paste(Dir$Project,'05_Docs',sep='/')
    Dir$Comm      <- paste(Dir$Project,'06_Comms',sep='/')
    Dir$Soft      <- paste(Dir$Project,'07_Software',sep='/')
    Dir$Admin     <- paste(Dir$Project,'10_Admin',sep='/')
    
    if (!is.null(Run)) Dir$Run <- paste(Dir$RunSet,Run,sep='/')
    if (!is.null(Run) && OnLinux) Dir$Run <- paste(Dir$Run,'Linux',sep='/')
    if (!is.null(Analysis)) Dir$Analyses <- paste(Dir$Analyses,Analysis,sep='/')
    if (!is.null(Analysis) && !is.null(Run) && AnalysisForRun==T) Dir$Analyses <- paste(Dir$Analyses,Run,sep='/')
  }
  
  if (OnServer) for (i in 1:length(Dir)) Dir[[i]] <- gsub('\\s+|\\.','_',Dir[[i]])
  return(Dir)
}

#==============================================================================================
#==============================================================================================

APSIM_New_Project <- function(Project,RunSet=NULL,Run=NULL,Analysis=NULL,MainDir='C:/Users/uqbababa/OneDrive - The University of Queensland/') {
  
  if (length(Project)>1) {
    x <- sapply(Project,APSIM_New_Project,Run=Run,Analysis=Analysis,MainDir=MainDir)
    return(x)
  }
  
  Dir <- APSIM_Paths2(Dir=list(),Project=Project,RunSet=RunSet,Run=Run,MainDir=MainDir)
  x <- sapply(Dir, function(x) {if(!dir.exists(x)) dir.create(x,recursive=T,showWarnings=F)})
  return(x)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Sim_File <- function(SimFile,UseXML=F) {
  if (UseXML) {
    Sim <- xmlParse(SimFile)
  } else {
    Sim <- readLines(SimFile,warn=F)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Write_Sim_File <- function(Sim,SimFile,Dir=NULL) {
  if (!is.null(Dir)) SimFile <- paste(Dir,SimFile,sep="/")
  writeLines(as(Sim,"character"),SimFile,useBytes=T)
}

#==============================================================================================
#==============================================================================================

APSIM_Rep_Text_in_Sim_File <- function(Sim,TextOld,TextNew,FileOut=NULL) {
  if (length(Sim)==1 && class(Sim)!='XMLInternalDocument') Sim <- APSIM_Read_Sim_File(Sim)
  
  pos <- grep(TextOld,Sim)
  
  for (i in seq(along = pos)) {
    Sim[pos[i]] <- gsub(TextOld,TextNew,Sim[pos[i]])
  }
  
  if (!is.null(FileOut)) {
    writeLines(Sim,FileOut)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Remove_Lines_in_Sim_File <- function(Sim,Texts,AfterLine=NULL,BeforeLine=NULL,FileOut=NULL) {
  # Removes line(s) containing elements in Texts
  
  if (length(Sim)==1 && class(Sim)!='XMLInternalDocument') Sim <- APSIM_Read_Sim_File(Sim)
  
  for (i in seq(along=Texts)) {
    pos <- grep(Texts[i],Sim)
    pos <- APSIM_FindLines_in_Sim(Sim,AfterLine=AfterLine,BeforeLine=BeforeLine,Pos=pos)
    if (length(pos)>0) Sim <- Sim[-pos]
  }
  
  if (!is.null(FileOut)) {
    writeLines(Sim,FileOut)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Attr_in_Sim_File <- function(Sim,AttrName,isNumeric=T,withProp=F,AfterLine=NULL,BeforeLine=NULL,UseXML=F) {
  
  if (length(Sim)==1 && is.character(Sim)) Sim <- APSIM_Read_Sim_File(Sim)
  
  if (UseXML) {
    x <- xpathApply(Sim,paste0("//",AttrName),xmlValue)[[1]]
    
  } else {
    
    if (withProp) pos <- grep(paste0('<',AttrName,'.*>'),Sim)
    else pos <- grep(paste0('<',AttrName,'>'),Sim)
    
    pos <- APSIM_FindLines_in_Sim(Sim,AfterLine=AfterLine,BeforeLine=BeforeLine,Pos=pos)
    
    if (length(pos)>0) {
      if (isNumeric) x <- as.numeric(gsub(paste0('.*<.*>(\\d+\\.*\\d*)</.*>'),'\\1',Sim[pos]))
      else x <- gsub(paste0('.*<.*>([[:print:]]+)</.*>'),'\\1',Sim[pos])
    } else {
      x <- NA
    }
  }
  
  return(x)
}

#==============================================================================================
#==============================================================================================

APSIM_Numbers_in_a_Line <- function(Sim,Pattern,Factors=1,Replace=F,FileOut=NULL) {
  
  if (length(Sim)==1 && is.character(Sim)) Sim <- APSIM_Read_Sim_File(Sim)
  
  pos <- grep(Pattern,Sim)
  
  for (p in pos) {
    x <- y <- as.numeric(strsplit(Sim[p],"\\D+")[[1]])
    x <- y <- y[!is.na(y)]
    xFactors <- Factors
    if (!is.null(Factors)) {
      if (length(Factors)<length(x) && length(Factors)>1) xFactors <- c(Factors[1],rep(Factors[-1],length(x)-1))
      if (length(Factors)>length(x)) xFactors <- Factors[1:length(x)]
      y <- x * xFactors
    }
    if (Replace) {
      for (i in 1:length(x)) {
        Sim[p] <- gsub(x[i],y[i],Sim[p])
      }
    }
  }
  
  if (!is.null(FileOut)) {
    writeLines(Sim,FileOut)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_FindLines_in_Sim <- function(Sim,AfterLine,BeforeLine,Pos) {
  if (!is.null(AfterLine)) {
    if (!is.numeric(AfterLine)) AfterLine <- grep(AfterLine,Sim)
  } else {
    AfterLine <- 0
  }
  if (!is.null(BeforeLine)) {
    if (!is.numeric(BeforeLine)) BeforeLine <- grep(BeforeLine,Sim)
  } else {
    BeforeLine <- 1e10
  }
  
  if (length(AfterLine)>0) {
    if (length(AfterLine)>length(BeforeLine)) {
      BeforeLine <- max(BeforeLine)
      AfterLine  <- AfterLine[AfterLine<BeforeLine]
      AfterLine  <- AfterLine[1]
      
    } else if (length(BeforeLine)>length(AfterLine)) {
      AfterLine  <- min(AfterLine)
      BeforeLine <- BeforeLine[BeforeLine>AfterLine]
      BeforeLine <- BeforeLine[1]
    }
  }
  
  Pos2 <- NULL
  for (A in 1:length(AfterLine)){
    rows <- which(Pos<=BeforeLine[A] & Pos>=AfterLine[A])
    if (length(rows)>0) Pos2 <- c(Pos2,Pos[rows])
  }
  return(Pos2)
}

#==============================================================================================
#==============================================================================================

APSIM_Change_Attr_in_Sim_File <- function(Sim,AttrName,AfterLine=NULL,BeforeLine=NULL,NewNum,withProp=F,FileOut=NULL,UseXML=F) {
  if (length(Sim)==1 && is.character(Sim)) Sim <- APSIM_Read_Sim_File(Sim)
  
  if (UseXML) {
    x <- GetNodes(Sim,AttrName)
    xmlValue(x[[1]]) <- NewNum
    
  } else {
    
    if (withProp)
      pos <- grep(paste0('<',AttrName,' .*>'),Sim)
    else
      pos <- grep(paste0('<',AttrName,'>'),Sim)
    
    pos <- APSIM_FindLines_in_Sim(Sim,AfterLine=AfterLine,BeforeLine=BeforeLine,Pos=pos)
    
    if (length(NewNum)>1) NewNum <- paste(NewNum,collapse=" ")
    
    for (i in seq(along=pos)) {
      Sim[pos[i]] <- sub('>.*</',paste0('>',NewNum,'</'),Sim[pos[i]])
    }
  }
  
  if (!is.null(FileOut)) {
    writeLines(Sim,FileOut)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Append_Block_to_Sim_File <- function(Sim,Block,AfterLine=NULL,AddLine=0,FileOut=NULL) {
  if (length(Sim)==1) Sim <- APSIM_Read_Sim_File(Sim)
  
  if (is.null(AfterLine)) AfterLine <- 0
  if (!is.numeric(AfterLine)) AfterLine <- grep(AfterLine,Sim)[1]
  
  Sim <- append(Sim,Block,after=AfterLine+AddLine)
  
  if (!is.null(FileOut)) {
    writeLines(Sim,FileOut)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Block_From_Sim_File <- function(Sim,AttrName,Name=NULL,Desc=NULL,FileOut=NULL) {
  if (length(Sim)==1) Sim <- APSIM_Read_Sim_File(Sim)
  
  if (!is.null(Name)) pos2 <- grep(paste0('<',AttrName,'.*name="',Name,'".*>'),Sim)
  else if (!is.null(Desc)) pos2 <- grep(paste0('<',AttrName,'.*description="',Desc,'".*>'),Sim)
  else pos2 <- pos1;
  
  pos1 <- pos2[1]
  
  pos2 <- grep(paste0('</',AttrName,'>'),Sim)
  pos2 <- pos2[pos2>=pos1]
  pos2 <- pos2[1]
  
  if (!is.null(FileOut)) {
    writeLines(Sim[seq(pos1,pos2)],FileOut)
  }
  return(Sim[seq(pos1,pos2)])
}

#==============================================================================================
#==============================================================================================

APSIM_Run_Sim_File <- function(SimFile,StopOnErr=T,
                               APSIMConsolePath='C:/Users/uqbababa/OneDrive - The University of Queensland/07_Repository/apsimDCaPS/Model/Apsim.exe',Print=T) {
  
  if (is.null(APSIMConsolePath)) {
    PCName  <- Sys.info()['nodename']
    if (PCName=='BEHNAM-LT') {
      APSIMConsolePath <- 'G:/Program Files/Apsim78-r3867/Model/Apsim.exe'
    } else {
      APSIMConsolePath <- 'C:/Users/uqbababa/OneDrive - The University of Queensland/07_Repository/apsimDCaPS/Model/Apsim.exe'
    }
  }
  
  if (Print) message("------------------------------------------------------------------------------")
  if (Print) message(paste0("Running the Sim File: ",basename(SimFile)))
  if (Print) message("------------------------------------------------------------------------------")
  
  x <- system(command = paste0('"',APSIMConsolePath,'" "',SimFile,'"'),
              intern=T,ignore.stdout=T,wait=T)
  
  if (exists(x['status'])) {
    warning('An error occurred during running the simulations!')
    if (StopOnErr) stop(x)
  }
  return(x);
}

#==============================================================================================
#==============================================================================================

APSIM_Run_All_Sim_Files <- function(SimFiles,IsApsimNG=F,Pattern=NULL,Ext='sim',
                                    UseListFile=T,RunFacts=F,NoSumm=F,SelectList=NULL,StopOnErr=T,
                                    SimPerGroup=10,NJobs=NULL,JobID=NULL,OnServer=F,TempFold=NULL,
                                    APSIMConsolePath='C:/Program Files (x86)/APSIM710-r4158/Model/Apsim.exe') {
  
  if (is.null(TempFold)) TempFold <- getwd()
  
  if (length(SimFiles)==1 && dir.exists(SimFiles)) {
    if (is.null(Pattern)) {
      SimFiles <- List_Files_by_Ext(SimFiles,Ext=Ext,AlTogether=F)
    } else {
      SimFiles <- List_Files_by_Ext_and_Pattern(SimFiles,Ext=Ext,Pattern,AlTogether=F)
    }
  }
  
  if (!is.null(NJobs) && !is.null(NJobs)) SimFiles <- MySplit(SimFiles,NGroups=NJobs,Which=JobID)
  if (!is.null(SelectList)) SimFiles <- SimFiles[file_path_sans_ext(basename(SimFiles)) %in% SelectList]
  
  if (IsApsimNG) {
    for (i in seq(along=SimFiles)) {
      if (OnServer) {
        x <- system(command=paste0('cd ',dirname(SimFiles[i]),' && ',APSIMConsolePath,' ',shQuote((SimFiles[i]))),wait=T)
        
      } else {
        x <- system2(command=APSIMConsolePath,stdout="",stderr="",wait=T,args=shQuote(SimFiles[i]))
      }
      
      if (x!=0) {
        warning('An error occurred during running the simulations!')
        if (StopOnErr) stop(x) else x <- -999
        
      } else {
        x <- gc()
      }
    }
    
  } else if (!IsApsimNG && UseListFile) {
    SimFiles <- shQuote(SimFiles,type='cmd')
    writeLines(SimFiles,paste0(TempFold,'/Temp.list'),sep='\n')
    
    if (OnServer) {
      x <- system(command=paste(APSIMConsolePath,'@Temp.list',ifelse(RunFacts,'doAllFactors=true','doAllFactors=false')),wait=T)
      
    } else {
      x <- system2(command=APSIMConsolePath,stdout="",stderr="",wait=T,
                   args=c('@Temp.list',ifelse(RunFacts,'doAllFactors=true','doAllFactors=false')))
    }
    
    if (x!=0) {
      warning('An error occurred during running the simulations!')
      if (StopOnErr) stop(x) else x <- -999
      
    }
    unlink(paste0(TempFold,'/Temp.list'),force=T,recursive=T)
    
  } else {
    
    xSimFiles <- Split_a_Vector(Vector=SimFiles,PerGroup=SimPerGroup)
    
    for (i in seq(along=xSimFiles)) {
      
      if (OnServer) {
        x <- system(command=paste0(APSIMConsolePath,' ',paste(shQuote(xSimFiles[[i]])),' ',
                                   ifelse(RunFacts,'doAllFactors=true','doAllFactors=false')),wait=T)
        
      } else {
        x <- system2(command=APSIMConsolePath,stdout="",stderr="",wait=T,
                     args=c(paste(shQuote(xSimFiles[[i]])),ifelse(RunFacts,'doAllFactors=true','doAllFactors=false')))
      }
      
      if (x!=0) {
        warning('An error occurred during running the simulations!')
        if (StopOnErr) stop(x) else x <- -999
      }
      LineX()
    }
  }
  
  return(x)
}

#==============================================================================================
#==============================================================================================

APSIM_Summarize_Sim_File <- function(Sim) {
  
  if (length(Sim)==1) {
    BaseName1 <- File_Base_Name(Sim)
    BaseName2 <- basename(Sim)
    Sim <- APSIM_Read_Sim_File(Sim)
  }
  
  Title <- APSIM_Ext_Title_Sim_File(Sim,Split=T)
  
  return(cbind(data.frame(ID=as.numeric(BaseName1),File=BaseName2),Title))
}

#==============================================================================================
#==============================================================================================

APSIM_Summarize_All_Sim_Files <- function(Dir,Pattern=NULL,SelectList=NULL,SortBy='ID',OutFile=NULL) {
  
  if (is.null(Pattern)) {
    SimFiles <- List_Files_by_Ext(Dir,Ext='sim',AlTogether=F)
  } else {
    SimFiles <- List_Files_by_Ext_and_Pattern(Dir,Ext='sim',Pattern,AlTogether=F)
  }
  
  if (!is.null(SelectList)) SimFiles <- SimFiles[file_path_sans_ext(basename(SimFiles)) %in% SelectList]
  
  Summ <- lapply(SimFiles,APSIM_Summarize_Sim_File)
  Summ <- do.call("rbind",Summ)
  
  if (!is.null(SortBy)) Summ <- Summ[order(Summ[,SortBy]),]
  
  if (!is.null(OutFile)) write.csv(Summ,file=OutFile,quote=F,sep=",",row.names=F)
  
  return(Summ)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Block <- function(DCaPSFile=NULL) {
  x <- readLines(DCaPSFile,warn=F)
  return(x)
}

#==============================================================================================
#==============================================================================================

APSIM_DCaPS_Change <- function(DCaPSFile=NULL,Changes=c(psiVc=0.0,psiJ=0.0),FileOut=NULL) {
  
  MainDir <- APSIM_UQ_Main_Dir()
  if (is.null(DCaPSFile)) DCaPSFile <- paste(MainDir,"05_Data/(2017.11.27) Blocks to Add to Sim Files/DCaPS Block - Default79.txt",sep="/")
  if (is.null(FileOut))   FileOut   <- paste(MainDir,"05_Data/(2017.11.27) Blocks to Add to Sim Files/DCaPS Block - Changed.txt",sep="/")
  
  DCaPS <- APSIM_Read_Block(DCaPSFile=DCaPSFile)
  
  for (P in names(Changes)) {
    PVal  <- APSIM_Read_Attr_in_Sim_File(DCaPS,AttrName=P,withProp=T,isNumeric=T)
    DCaPS <- APSIM_Change_Attr_in_Sim_File(DCaPS,AttrName=P,NewNum=PVal*(1+Changes[P]),withProp=T,FileOut=FileOut)
  }
  return(DCaPS)
}

#==============================================================================================
#==============================================================================================

APSIM_60_Met_Stations <- function(MainDir=NULL,RFile='60 Met Locations.RData',
                                  csvFile="03_WeatherData/60 Locations/(2017.11.28) Met Stations 60 Locations with Wickepin.csv") {
  if (is.null(MainDir)) MainDir <- APSIM_UQ_Main_Dir()
  
  if (!is.null(csvFile) && is.null(RFile)) {
    x <- read.csv(file=paste(MainDir,csvFile,sep="/"),header=T,check.names=F)
    return(x)
    
  } else {
    load(paste(MainDir,'02_Data',RFile,sep='/'))
    return(MetLocations)
  }
}

#==============================================================================================
#==============================================================================================

APSIM_Ext_Title_Sim_File <- function(Sim,Split=T) {
  if (length(Sim)==1) Sim <- APSIM_Read_Sim_File(Sim)
  
  FullTit <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='title',withProp=F,isNumeric=F)
  
  if (Split) {
    x <- unlist(strsplit(FullTit,split="[=;]+"))
    Title <- data.frame()
    for (i in 1:(length(x)/2)) {
      Title[1,i] <- x[i*2]
      colnames(Title)[i] <- x[i*2-1]
    }
    Title$FullTit <- FullTit;
  } else {
    Title <- FullTit
  }
  
  return(Title)
}

#==============================================================================================
#==============================================================================================

APSIM_Remake_Title_Sim_File <- function(Title,Sep=';') {
  Title$FullTit <- NULL
  x <- paste(names(Title)[1],Title[1],sep='=')
  
  for (Name in names(Title[-1])[names(Title[-1])!='FullTit']) {
    x <- paste(x,paste(Name,Title[Name],sep='='),sep=Sep)
  }
  Title$FullTit <- x
  return(Title)
}

#==============================================================================================
#==============================================================================================

APSIM_Move_All_Output_Files <- function(Dir1,Dir2,SelectList=NULL,NJobs=NULL,JobID=NULL,MoveOuts=T,MoveSums=T,IsApsimNG=F) {
  # SelectList must NOT contain extensions. Only the base names of Sim files are accepted.
  
  if (IsApsimNG) Ext <- 'db' else Ext <- 'out'
  
  if (MoveOuts) {
    OutFiles <- List_Files_by_Ext(Dir1,Ext)
    if (length(OutFiles)==0) OutFiles <- List_Files_by_Ext(Dir1,'gz')
    if (length(OutFiles)>1) BaseNames <- unlist(File_Base_Name(OutFiles))
    else BaseNames <- File_Base_Name(OutFiles)
    
    if (!is.null(SelectList)) OutFiles <- OutFiles[unlist(sapply(paste0("^",SelectList,'_'),grep,BaseNames))]
    if (!is.null(NJobs) && !is.null(NJobs)) OutFiles <- MySplit(OutFiles,NGroups=NJobs,Which=JobID)
    
    x <- file.copy(OutFiles,Dir2,overwrite=T)
    x <- unlink(OutFiles,force=T,recursive=T)
  }
  
  OutFiles2 <- NULL
  if (MoveSums && !IsApsimNG) {
    OutFiles2 <- list.files(Dir1,pattern=".sum",full.names=T)
    if (!is.null(SelectList)) OutFiles2 <- OutFiles2[!is.na(match(basename(OutFiles2),paste0(SelectList,'.sum')))]
    if (!is.null(NJobs) && !is.null(NJobs)) OutFiles2 <- MySplit(OutFiles2,NGroups=NJobs,Which=JobID)
    x <- file.copy(OutFiles2,Dir2,overwrite=T)
    x <- unlink(OutFiles2,force=T,recursive=T)
  }
  return(list(Outputs=OutFiles,Summs=OutFiles2))
}

#==============================================================================================
#==============================================================================================

APSIMNG_Read_Output_File <- function(OutFile,Ext='db',STable=NULL,DTable=NULL,OnlyFullSeasons=F,OnlyInSeason=T,SowYearCol='SowYear',TempFold=NULL) {
  
  if (grepl(patter='*.gz$',OutFile)) {
    if (is.null(TempFold)) {
      xDir <- paste(dirname(OutFile),File_Base_Name(OutFile),sep='/')
    } else {
      xDir <- paste(TempFold,File_Base_Name(OutFile),sep='/')
    }
    untar(OutFile,exdir=xDir)
    Out <- APSIMNG_Read_All_Output_Files(OutFiles=xDir,Ext=Ext,STable=STable,DTable=DTable,OnlyFullSeasons=OnlyFullSeasons,
                                         OnlyInSeason=OnlyInSeason,SowYearCol=SowYearCol)
    unlink(xDir,force=T,recursive=T)
    x <- gc()
    
    if (length(Out)>0 && !is.null(STable)) for (i in 1:length(Out)) Out[[i]][[STable]]$TarFile <- File_Base_Name(OutFile)
    if (length(Out)>0 && !is.null(DTable)) for (i in 1:length(Out)) Out[[i]][[DTable]]$TarFile <- File_Base_Name(OutFile)
    return(Out)
  }
  
  Outputs <- list()
  if (tolower(Ext)=='db') {
    con <- dbConnect(drv=RSQLite::SQLite(),dbname=OutFile)
    AllTables <- DBI::dbListTables(con)
    AllTables <- setdiff(AllTables, c('_Checkpoints','_InitialConditions','_Units'))
    
    if (!is.null(STable) && (is.na(STable) && STable=='NULL')) STable <- NULL
    if (!is.null(DTable) && (is.na(DTable) && DTable=='NULL')) DTable <- NULL
    
    if (!is.na(STable) && length(STable)>1) STable <- intersect(STable,AllTables)
    if (!is.na(DTable) && length(DTable)>1) DTable <- intersect(DTable,AllTables)
    
    for (Table in AllTables) {
      Outputs[[Table]] <- data.table(dbGetQuery(con,sprintf('select * from %s',Table)))
      if ('SimulationID' %in% names(Outputs[[Table]])) setkeyv(Outputs[[Table]],col='SimulationID')
      Outputs[[Table]]$File <- File_Base_Name(OutFile)
    }
    
  } else if (tolower(Ext)=='csv') {
    AllTables <- c(STable, DTable)
    for (Table in AllTables) {
      Outputs[[Table]] <- fread(file=OutFile, sep=',', quote="")
      Outputs[[Table]]$File <- File_Base_Name(OutFile)
    }
  }
  
  if (!is.null(DTable) && DTable %in% AllTables) {
    # Remove daily outputs of incompletely simulated seasons
    
    if (!SowYearCol %in% names(Outputs[[DTable]])) SowYearCol <- 'year'
    if (!SowYearCol %in% names(Outputs[[DTable]])) SowYearCol <- 'SowYear'
    if (!SowYearCol %in% names(Outputs[[DTable]])) SowYearCol <- 'Clock.Today.Year'
    
    if (SowYearCol %in% names(Outputs[[STable]]) && SowYearCol %in% names(Outputs[[DTable]])) {
      if (OnlyFullSeasons && !is.null(STable)) {
        Outputs[[DTable]] <- merge(Outputs[[STable]][,c('SimulationID',SowYearCol),with=F],Outputs[[DTable]],by=c('SimulationID',SowYearCol),all=F,sort=T)
      }
      Outputs[[DTable]] <- Outputs[[DTable]][get(SowYearCol)>1]
      
    } else {
      warning("'SowYearCol' does not exist!")
    }
    
    Outputs[[DTable]] <- Outputs[[DTable]][, lapply(.SD,function(x){x[x=='?']<-NA;return(x)}), .SDcols=colnames(Outputs[[DTable]])]
    if ('Clock.Today'%in%colnames(Outputs[[DTable]])) Outputs[[DTable]]$Clock.Today <- as.Date(Outputs[[DTable]]$Clock.Today)
  }
  
  if (tolower(Ext)=='db') dbDisconnect(con)
  return(Outputs)
}

#==============================================================================================
#==============================================================================================

APSIMNG_Read_All_Output_Files <- function(OutFiles,Ext='db',Pattern=NULL,STable=NULL,DTable=NULL,OnlyFullSeasons=F,OnlyInSeason=T,
                                          SowYearCol='SowYear',NJobs=NULL,JobID=NULL,TempFold=NULL) {
  
  Pattern2 <- '*'
  if (tolower(Ext)=='csv') {
    if (!is.null(STable) && !is.null(DTable))
      stop('When Ext="csv", either STable or DTable must be provided, not both!')
    
    if (!is.null(STable)) Pattern2 <- paste0(Pattern2, '.', STable)
    if (!is.null(DTable)) Pattern2 <- paste0(Pattern2, '.', DTable)
  }
  
  if (length(OutFiles)==1 && dir.exists(OutFiles)) {
    # OutFiles is a folder.
    Pattern <- ifelse(is.null(Pattern), sprintf('%s.%s$',Pattern2,Ext), sprintf('*%s%s.%s$',Pattern,Pattern2,Ext))
    OutFiles <- list.files(path=OutFiles,pattern=paste0(Pattern,'|*.tar.gz$'),full.names=T)
    
  } else {
    # OutFiles is a vector.
    if (tolower(Ext)=='csv') {
      OutFiles <- OutFiles[grepl(paste0(Pattern,'|*.tar.gz$'), OutFiles) & !grepl('*.db$', OutFiles)]
    }
  }
  
  if (!is.null(NJobs) && !is.null(NJobs)) OutFiles <- MySplit(OutFiles,NGroups=NJobs,Which=JobID)
  
  if (length(OutFiles)>1) BaseNames <- unlist(File_Base_Name(OutFiles))
  else BaseNames <- File_Base_Name(OutFiles)
  
  Outputs <- lapply(OutFiles, APSIMNG_Read_Output_File, Ext=Ext,STable=STable,DTable=DTable,OnlyFullSeasons=OnlyFullSeasons,
                    OnlyInSeason=OnlyInSeason,SowYearCol=SowYearCol,TempFold=TempFold)
  
  if (ListDepth(Outputs)==2) {
    # When ReadTars=F
    names(Outputs) <- File_Base_Name(OutFiles)
  } else {
    # When ReadTars=T
    Outputs <- do.call(c, Outputs)
  }
  return(Outputs)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Output_File_DT <- function(OutFile,OnlyFullSeasons=F,ExpHead=F,NRows=-1) {
  All   <- readLines(OutFile,n=50)
  Heads <- All[grep('=',All)]
  NSkip <- max(grep('=',All))
  
  if (!is.null(Heads)) {
    FUNC <- function(x) {
      isFactors <- F
      if (length(grep('factors =',x))>0) {
        x <- gsub('factors =','',x)
        isFactors <- T
      }
      if (isFactors) x <- gsub('factors = ','',x)
      
      xOut <- NULL
      if (length(grep('Title',x))>0) {
        x <- gsub('Title = ','',x)
        xOut <- cbind.data.frame(y1='Title',y2=trimws(x))
        
      } else {
        x1 <- strsplit(x,'=')[[1]]
        x2 <- strsplit(x,';')[[1]]
        xLen <- length(x2)
        
        for (i in 1:xLen) {
          y  <- strsplit(x2[i],'=')[[1]]
          y1 <- trimws(y[1])
          y2 <- trimws(y[2])
          xOut <- rbind.data.frame(xOut,cbind.data.frame(y1,y2))
        }
      }
      return(xOut)
    }
    Heads <- rbindlist(lapply(Heads,FUNC),fill=T)
    Heads <- as.data.frame(t(Heads))
    colnames(Heads) <- unlist(Heads[1,])
    rownames(Heads) <- NULL
    Heads <- Heads[-1,]
  }
  
  VarNames <- read.table(OutFile,header=F,skip=NSkip,nrows=1,check.names=F,sep='',strip.white=T,fill=T)
  Outputs  <- fread(file=OutFile,header=F,nrows=NRows,skip=NSkip+2,check.names=F,strip.white=T,fill=T,sep=' ')
  colnames(Outputs) <- as.character(unlist(VarNames))
  
  if (OnlyFullSeasons & all(c('SowYear','stage') %in% as.matrix(VarNames))) {
    # Remove daily outputs of incompletely simulated seasons
    if (Outputs[, stage[.N]]<9) {
      Outputs <- Outputs[SowYear!=SowYear[.N]]
    }
  }
  
  Outputs$File <- file_path_sans_ext(basename(OutFile))
  Outputs <- Outputs[, lapply(.SD,function(x){x[x=='?']<-NA;return(x)}), .SDcols=colnames(Outputs)]
  
  if (ExpHead) {
    Outputs <- list(Outputs=Outputs,Heads=Heads)
  } else {
    Outputs <- cbind(Outputs,Heads)
  }
  
  return(data.table(Outputs))
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Output_File <- function(OutFile,OnlyFullSeasons=F,ExpHead=F,NRows=-1,DataTable=F) {
  
  All <- readLines(OutFile,n=100)
  Heads <- All[grep('=',All)]
  NSkip <- length(Heads)
  
  if (!is.null(Heads)) {
    FUNC <- function(x) {
      isFactors <- F
      if (length(grep('factors =',x))>0) {
        x <- gsub('factors =','',x)
        isFactors <- T
      }
      if (isFactors) x <- gsub('factors = ','',x)
      
      xOut <- NULL
      if (length(grep('Title',x))>0) {
        x <- gsub('Title = ','',x)
        xOut <- cbind.data.frame(y1='Title',y2=trimws(x))
        
      } else {
        x1 <- strsplit(x,'=')[[1]]
        x2 <- strsplit(x,';')[[1]]
        xLen <- length(x2)
        
        for (i in 1:xLen) {
          y  <- strsplit(x2[i],'=')[[1]]
          y1 <- trimws(y[1])
          y2 <- trimws(y[2])
          #if (isFactors) y1 <- paste('factor',y1,sep='_')
          xOut <- rbind.data.frame(xOut,cbind.data.frame(y1,y2))
        }
      }
      return(xOut)
    }
    Heads <- lapply(Heads,FUNC)
    Heads <- List_RBind(Heads)
    Heads <- as.data.frame(t(Heads))
    colnames(Heads) <- Heads[1,]
    rownames(Heads) <- NULL
    Heads <- Heads[-1,]
  }
  
  VarNames <- read.table(OutFile,header=F,skip=NSkip,nrows=1,check.names=F,sep='',strip.white=T,fill=T)
  if (DataTable) {
    Outputs  <- fread(file=OutFile,header=F,nrows=NRows,skip=NSkip+2,check.names=F,strip.white=T,fill=T)
  } else {
    Outputs  <- as.data.frame(read.table(OutFile,header=F,nrows=NRows,skip=NSkip+2,check.names=F,sep='',strip.white=T,fill=T))
  }
  colnames(Outputs) <- unlist(VarNames)
  
  if (OnlyFullSeasons & ('SowYear' %in% as.matrix(VarNames)) & ('stage' %in% as.matrix(VarNames))) {
    # Remove daily outputs of incompletely simulated seasons
    SYears <- unique(Outputs$SowYear)
    if (Outputs$stage[nrow(Outputs)]<9) {
      Outputs <- Outputs[-which(Outputs$SowYear==Outputs$SowYear[nrow(Outputs)]),]
    }
  }
  Outputs$File <- basename(OutFile)
  Outputs <- Replace_in_Table(Outputs,What='?',With=NA)
  
  if (ExpHead) {
    Outputs <- list(Outputs=Outputs,Heads=Heads)
  } else {
    Outputs <- cbind.data.frame(Outputs,Heads)
  }
  
  return(Outputs)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_All_Output_Files <- function(OutFiles,Ext='out',Pattern='Daily',SelectList=NULL,doParallel=T,
                                        NJobs=NULL,JobID=NULL,OnlyFullSeasons=F,ExpHead=F,AppendToNames='_',NRows=-1) {
  # SelectList must NOT contain extensions. Only the base names of Sim files are accepted.
  
  if (length(OutFiles)==1 && dir.exists(OutFiles)) {
    OutFiles <- List_Files_by_Ext_and_Pattern(OutFiles,Ext,Pattern,AlTogether=F)
  } else if (!is.null(Pattern)) {
    OutFiles <- grep(Pattern,OutFiles,value=T)
  }
  if (length(OutFiles)>0) OutFiles <- OutFiles[file.size(OutFiles)>0]
  if (length(OutFiles)==1) doParallel <- F
  
  Outputs <- NULL
  if (length(OutFiles)>0) {
    if (!is.null(NJobs) && !is.null(NJobs)) OutFiles <- MySplit(OutFiles,NGroups=NJobs,Which=JobID)
    
    if (length(OutFiles)>1) BaseNames <- unlist(file_path_sans_ext(basename(OutFiles)))
    else BaseNames <- file_path_sans_ext(basename(OutFiles))
    
    if (!is.null(SelectList)) OutFiles <- OutFiles[unlist(sapply(paste0("^",SelectList,AppendToNames),grep,BaseNames))]
    
    if (doParallel) {
      doParallel::registerDoParallel(cores=detectCores()-1)
      
      Outputs <- foreach::foreach(File=OutFiles,.packages=c('data.table','tools'),.inorder=T,.export='APSIM_Read_Output_File_DT') %dopar%
        APSIM_Read_Output_File_DT(OutFile=File,OnlyFullSeasons=OnlyFullSeasons,ExpHead=ExpHead,NRows=NRows)
      
      doParallel::stopImplicitCluster()
      
    } else {
      Outputs <- lapply(OutFiles,APSIM_Read_Output_File_DT,OnlyFullSeasons=OnlyFullSeasons,ExpHead=ExpHead,NRows=NRows)
    }
    names(Outputs) <- BaseNames
  }
  
  return(Outputs)
}

#==============================================================================================
#==============================================================================================

APSIM_Find_Stages <- function(DailyOut,LevCols=NULL,Years=NULL,Stages=c(3,6,9),StageCol='stage',DCols=NULL) {
  
  if (!('SowYear'%in%names(DailyOut))) DailyOut$SowYear <- DailyOut$year
  if (!is.null(Years)) DailyOut <- DailyOut[SowYear%in%Years]
  
  FUNC <- function(Data) {
    Output <- data.table()
    for (s in Stages) {
      RowD  <- which(Data[,get(StageCol)]>=s)[1]
      DayD  <- Data[RowD,day]
      YearD <- Data[RowD,year]
      Output <- rbind(Output,data.table(Stage=s,Year=YearD[[1]],Day=DayD[[1]],RowD=RowD))
    }
    return(Output)
  }
  Output <- DailyOut[, FUNC(.SD), keyby=c(LevCols,'SowYear')]
  return(as.data.frame(Output))
}

#==============================================================================================
#==============================================================================================

APSIM_Rows_by_TT <- function(DailyOut,LevCols=NULL,Years=NULL,Stage=6,PreTT=300,PosTT=100,StageCol='stage',TTCol='cumTT',DStages=NULL) {
  
  DailyOut <- data.table(DailyOut)
  if (!('SowYear'%in%names(DailyOut))) DailyOut$SowYear <- DailyOut$year
  if (!is.null(Years)) DailyOut <- DailyOut[SowYear%in%Years]
  
  FUNC <- function(Data) {
    if (is.na(PreTT) && is.na(PosTT)) {
      Rows   <- range(Data[get(StageCol) >= 1.9, which=T])
      X1  <- Data[Rows[1],c('year','day',StageCol),with=F]
      X2  <- Data[Rows[2],c('year','day',StageCol),with=F]
      Output <- data.table(Row1=Rows[1],Row2=Rows[2],YearD1=X1$year,DayD1=X1$day,StageD1=X1$stage,YearD2=X2$year,DayD2=X2$day,StageD2=X2$stage)
      
    } else {
      DStages <- APSIM_Find_Stages(DailyOut=Data,Years=NULL,Stages=Stage,StageCol=StageCol,DCols=NULL)
      
      if (!is.na(DStages$RowD)) {
        TTStage <- Data[DStages$RowD,get(TTCol)][[1]]
        Rows   <- range(Data[get(TTCol) >= TTStage-PreTT & get(TTCol) <= TTStage+PosTT, which=T])
        X1  <- Data[Rows[1],c('year','day',StageCol),with=F]
        X2  <- Data[Rows[2],c('year','day',StageCol),with=F]
        Output <- data.table(Row1=Rows[1],Row2=Rows[2],YearD1=X1$year,DayD1=X1$day,StageD1=X1$stage,YearD2=X2$year,DayD2=X2$day,StageD2=X2$stage)
      } else {
        Output <- data.table(Row1=NA,Row2=NA,YearD1=NA,DayD1=NA,StageD1=NA,YearD2=NA,DayD2=NA,StageD2=NA)
      }
    }
    return(Output)
  }
  
  Output <- DailyOut[, FUNC(.SD), keyby=c(LevCols,'SowYear'), .SDcols=names(DailyOut)]
  return(Output)
}

#==============================================================================================
#==============================================================================================

APSIM_Count_Spells <- function(DailyOut,WeatherTab,LevCols=NULL,WeatCol='maxt',Years=NULL,Stage=6,PreTT=300,PosTT=100,Thrsh=30,CalcSpells=F,
                               StageCol='stage',TTCol='cumTT',CompareType='above',AddCols=NULL,DStages=NULL) {
  
  DailyOut <- data.table(DailyOut)
  if (!is.null(Years)) DailyOut <- DailyOut[SowYear%in%Years]
  
  if (!is.null(WeatherTab)) {
    WeatherTab <- data.table(WeatherTab)
    if (!is.null(Years)) WeatherTab <- WeatherTab[year%in%c(Years,max(Years)+1)]
    DailyOut <- merge(DailyOut,WeatherTab,by=c('year','day'),sort=F)
  }
  
  FUNC <- function(Data) {
    Output <- list()
    DRows <- APSIM_Rows_by_TT(DailyOut=Data,Years=NULL,Stage=Stage,PreTT=PreTT,PosTT=PosTT,StageCol=StageCol,TTCol=TTCol,DStages=DStages)
    
    if (all(is.finite(unlist(DRows[,.(Row1,Row2)])))) {
      TempW <- Data[DRows$Row1:DRows$Row2,get(WeatCol)]
      if (length(TempW)<2) stop('The period must be longer than one day!')
      for (Thr in Thrsh) {
        if (CompareType=='above') Temp <- as.numeric(TempW>Thr)
        if (CompareType=='below') Temp <- as.numeric(TempW<Thr)
        if (CalcSpells) {
          TempN1 <- c(Temp[2:length(Temp)],0)
          TempP1 <- c(0,Temp[-length(Temp)])
          x1 <- which(Temp==1 & TempN1==0)
          x2 <- which(Temp==1 & TempP1==0)
          Duration <- x1-x2+1
          if (length(Duration)==0) Duration <- 0
        } else {
          Duration <- sum(Temp,na.rm=T)
        }
        Output[[length(Output)+1]] <- data.table(Thrsh=Thr,Duration=Duration)
      }
    } else {
      for (Thr in Thrsh) {
        Output[[length(Output)+1]] <- data.table(Thrsh=Thr,Duration=NA_real_)
      }
    }
    Output <- rbindlist(Output)
    return(Output)
  }
  
  Output <- DailyOut[, FUNC(.SD), keyby=c(LevCols,'SowYear'), .SDcols=names(DailyOut)]
  if (!is.null(AddCols)) Output <- cbind(data.table(AddCols),Output)
  return(Output)
}

#==============================================================================================
#==============================================================================================

APSIM_Count_Weather_Spells <- function(WeatherTab,LevCols=NULL,WeatCol='maxt',Years=NULL,Months=1:12,Thrsh=30,CalcSpells=F,CompareType='above') {
  
  FUNC <- function(Data) {
    Output <- list()
    TempW <- Data[,get(WeatCol)]
    
    for (Thr in Thrsh) {
      if (CompareType=='above') Temp <- as.numeric(TempW>Thr)
      if (CompareType=='below') Temp <- as.numeric(TempW<Thr)
      
      if (CalcSpells) {
        TempN1 <- c(Temp[2:length(Temp)],0)
        TempP1 <- c(0,Temp[-length(Temp)])
        x1 <- which(Temp==1 & TempN1==0)
        x2 <- which(Temp==1 & TempP1==0)
        Duration <- x1-x2+1
        if (length(Duration)==0) Duration <- 0
        
      } else {
        Duration <- sum(Temp,na.rm=T)
      }
      Output[[length(Output)+1]] <- data.table(Thrsh=Thr,Duration=Duration)
    }
    
    Output <- rbindlist(Output)
    return(Output)
  }
  
  WeatherTab <- cbind(WeatherTab,DOY2DateComps(WeatherTab[,.(year,day)]))
  if (!is.null(Months)) WeatherTab <- WeatherTab[Mon%in%Months]
  Output <- WeatherTab[, FUNC(.SD), keyby=c(LevCols,'Year'), .SDcols=names(WeatherTab)]
  return(Output)
}

#==============================================================================================
#==============================================================================================

APSIM_Count_Stress_Days <- function(WeatherTab,LevCols=NULL,WeatCol='maxt',Years=NULL,Months=1:12,Thrsh=30,CompareType='above') {
  
  WeatherTab <- data.table(WeatherTab)
  if (is.null(Years)) Years <- unique(as.numeric(as.character(WeatherTab$year)))
  WeatherTab <- WeatherTab[year%in%Years]
  WeatherTab <- cbind(WeatherTab,DOY2DateComps(WeatherTab[,.(year,day)]))
  if (!is.null(Months)) WeatherTab <- WeatherTab[Mon%in%Months]
  
  FUNC <- function(Data) {
    Output <- data.table()
    for (Thr in Thrsh) {
      if (CompareType=='above') Temp <- Data[get(WeatCol)>Thr, .N]
      if (CompareType=='below') Temp <- Data[get(WeatCol)<Thr, .N]
      Count  <- cbind(Thrsh=Thr,Count=Temp)
      Output <- rbind(Output,Count)
    }
    return(Output)
  }
  Output <- WeatherTab[, FUNC(.SD), keyby=c(LevCols,'Year'), .SDcols=names(WeatherTab)]
  return(Output)
}

#==============================================================================================
#==============================================================================================

APSIM_First_Last_Stress_Day <- function(WeatherTab,LevCols=NULL,WeatCol='maxt',Years=NULL,From=c(1,1),To=c(12,31),Thrsh=30,Which='first',
                                        CompareType='above',DateFormat=NULL,NAReplace=T,LevColsNARep=NULL,WhatYear=NULL,MinOccPer=NULL) {
  
  WeatherTab <- data.table(WeatherTab)
  if (is.null(Years)) Years <- unique(as.numeric(as.character(WeatherTab$year)))
  WeatherTab <- WeatherTab[year%in%Years,]
  WeatherTab <- cbind(WeatherTab,DOY2DateComps(WeatherTab[,.(year,day)]))
  
  FUNC <- function(Data) {
    Out  <- data.table()
    Row1 <- which(Data$Mon==From[1] & Data$Day==From[2])
    Row2 <- which(Data$Mon==To[1] & Data$Day==To[2])
    if (length(Row2)==0) Row2 <- nrow(Data)
    Temp  <- Data[Row1:Row2,]
    TempW <- Temp[,get(WeatCol)]
    
    for (Thr in Thrsh) {
      if (CompareType=='above') Rows <- which(TempW>Thr)
      if (CompareType=='below') Rows <- which(TempW<Thr)
      
      if (length(Rows)>0) {
        if (Which=='first') Rows <- Rows[1]
        if (Which=='last')  Rows <- Rows[length(Rows)]
      } else {
        Rows <- NA_real_
      }
      
      Temp2  <- as.Date(paste(unlist(Temp[Rows,.(Year,Mon,Day)]),collapse='-'),origin='1970-01-01',format='%Y-%m-%d')
      Count  <- setDT(cbind.data.frame(Thrsh=Thr,DOY=Date2DOY(Temp2)))
      Out <- rbind(Out,Count)
    }
    return(Out)
  }
  KeyBy <- unique(c(LevCols,LevColsNARep,'Year'))
  Output <- WeatherTab[, FUNC(.SD), keyby=KeyBy, .SDcols=names(WeatherTab)]
  
  if (NAReplace) {
    FUNC2 <- function(Data) {
      Data2 <- copy(Data)
      Data2[, Regional:=FALSE]
      if (Data[is.na(DOY), .N]>0) {
        if (Data[!is.na(DOY), .N]>0) {
          if (Which=='first') Data2[is.na(DOY), `:=`(DOY=max(Data[!is.na(DOY), DOY],na.rm=T),Regional=TRUE)]
          if (Which=='last')  Data2[is.na(DOY), `:=`(DOY=min(Data[!is.na(DOY), DOY],na.rm=T),Regional=TRUE)]
          
        } else {
          if (Which=='first') Data2[is.na(DOY), `:=`(DOY=DateComps2DOY(c(2000,To)),Regional=TRUE)]
          if (Which=='last')  Data2[is.na(DOY), `:=`(DOY=DateComps2DOY(c(2000,from)),Regional=TRUE)]
        }
      }
      return(Data2)
    }
    KeyBy <- c(LevColsNARep,'Thrsh')
    Output <- Output[, FUNC2(.SD), keyby=KeyBy, .SDcols=setdiff(names(Output),KeyBy)]
  }
  
  if (!is.null(MinOccPer)) {
    FUNC3 <- function(Data) {
      Data2 <- copy(Data)
      Per <- Data[, 100*sum(!Regional,na.rm=T)/nrow(Data)]
      if (Per<MinOccPer) Data2[, DOY:=NA]
      return(Data2)
    }
    KeyBy <- unique(c(LevCols,LevColsNARep,'Thrsh'))
    Output <- Output[, FUNC3(.SD), keyby=KeyBy, .SDcols=setdiff(names(Output),KeyBy)]
  }
  
  if (!is.null(WhatYear)) Output[, Date:=DOY2Date(DOY,WhatYear)] else Output[, Date:=DOY2Date(DOY,Year)]
  if (!is.null(DateFormat)) Output[, Date:=format(Date,format=DateFormat)]
  setkeyv(Output,col=unique(c(LevCols,LevColsNARep,'Thrsh','Year')))
  return(Output)
}

#==============================================================================================
#==============================================================================================

APSIM_Correct_Weather_File_Name <- function(WFiles) {
  
  if (length(WFiles)>1) {
    WFiles2 <- sapply(WFiles,APSIM_Correct_Weather_File_Name)
    return(WFiles2)
  }
  
  NameWithExt <- basename(WFiles)
  NameWOutExt <- File_Base_Name(WFiles)
  Ext <- file_ext(WFiles)
  Dir <- dirname(WFiles)
  
  NameWOutExt2 <- gsub("\\.met$","",NameWOutExt)
  NameWOutExt2 <- gsub(c("\\(")," ",NameWOutExt2)
  NameWOutExt2 <- gsub(c("\\)")," ",NameWOutExt2)
  NameWOutExt2 <- gsub(c("\\.+")," ",NameWOutExt2)
  NameWOutExt2 <- gsub(c("\\s\\."),".",NameWOutExt2)
  NameWOutExt2 <- gsub(c("\\s\\s")," ",NameWOutExt2)
  
  NameWOutExt2 <- Capitalize_First_Letters(NameWOutExt2,ignorePar=F)
  NameWithExt2 <- paste(NameWOutExt2,Ext,sep=".")
  
  WFiles2 <- paste(Dir,NameWithExt2,sep='/')
  file.rename(WFiles,WFiles2)
  return(WFiles2)
}

#==============================================================================================
#==============================================================================================

APSIM_UQ_Main_Dir <- function(LocMainDir='C:/Users/uqbababa/OneDrive - The University of Queensland/',RemMainDir=NULL) {
  OnServer <- (length(grep(':',getwd()))==0)
  MainDir  <- LocMainDir
  if (OnServer) MainDir <- RemMainDir
  return(MainDir)
}

#==============================================================================================
#==============================================================================================

APSIM_CenterOnAnthesis <- function(DailyOut,IDCols=NULL,Years=NULL,AnthStage=6,StageCol='stage',XAxisCol='cumTT',ClusterCols='sdr',
                                   Interval=100,MinMax=c(-2000,1000),RepAllNAs=NA) {
  
  if (is.character(DailyOut)) DailyOut <- APSIM_Read_Output_File_DT(DailyOut,OnlyFullSeasons=T,ExpHead=F)
  DailyOut <- MySort(DailyOut,unique(c(IDCols,'SowYear')))
  Daily <- NULL;
  
  Ints1 <- seq(-Interval/2,MinMax[1],-Interval);
  Ints2 <- seq(Interval/2,MinMax[2],Interval);
  Ints  <- c(rev(Ints1),Ints2);
  IntsN <- Ints[1:(length(Ints)-1)] + Interval/2;
  
  if (is.null(Years)) Years <- sort(unique(DailyOut$SowYear),decreasing=F) else
    Years <- sort(Years[Years%in%DailyOut$SowYear],decreasing=F)
  
  for (y in Years) {
    yData    <- DailyOut[DailyOut$SowYear==y,]
    GrStg    <- yData[,StageCol];
    PivotInd <- which(GrStg>=AnthStage)[1];
    
    if (!is.na(PivotInd)) {
      XData <- yData[,XAxisCol];
      XData <- XData - XData[PivotInd];
      YData <- yData[,ClusterCols];
      YData <- as.matrix(YData,ncol=length(ClusterCols),byrow=F);
      
      Cuts  <- cut(XData,Ints,labels=IntsN)
      YData <- stats::aggregate(YData,list(Range=Cuts),mean,na.rm=T,drop=F)
      if (nrow(YData)<length(IntsN)) YData <- merge(YData,data.frame(Range=IntsN),by='Range',all=T)
      
      YData <- round(YData[,-1],3)
      YData <- sapply(YData,function(x){x[is.nan(x)]<-NA;return(x)})
      YData <- t(YData)
      
    } else {
      YData <- matrix(RepAllNAs,nrow=length(ClusterCols),ncol=length(IntsN))
    }
    YData <- cbind(Var=ClusterCols,YData)
    colnames(YData) <- c('Var',IntsN)
    Daily <- rbind.data.frame(Daily,YData)
  }
  
  rownames(Daily) <- NULL
  if (!is.null(IDCols)) {
    x <- unique(DailyOut[,IDCols])
    Daily <- cbind.data.frame(Daily,x[rep(1:nrow(x),each=length(ClusterCols)),])
  }
  rownames(Daily) <- NULL
  return(Daily)
}

#==============================================================================================
#==============================================================================================

APSIM_CenterOnAnthesis_DT <- function(DailyOut,Levels=NULL,Years=NULL,AnthStage=6,
                                      SowYearCol='SowYear',StageCol='stage',TTCol='cumTT',ClusterCols='sdr',
                                      Interval=100,MinMax=c(-2000,1000),ConvWide=T) {
  
  if (is.character(DailyOut)) DailyOut <- APSIM_Read_Output_File_DT(DailyOut,OnlyFullSeasons=T,ExpHead=F)
  if (is.list(DailyOut) && !is.data.table(DailyOut)) Data <- rbindlist(DailyOut,fill=T) else Data <- data.table(DailyOut)
  if (!is.null(Years)) Data <- Data[get(SowYearCol)%in%Years]
  
  Ints1 <- seq(-Interval/2,MinMax[1],-Interval)
  Ints2 <- seq(Interval/2,MinMax[2],Interval)
  Ints  <- c(rev(Ints1),Ints2)
  IntsN <- Ints[1:(length(Ints)-1)] + Interval/2
  
  if (length(ClusterCols)>1) {
    Data <- melt(Data,id.vars=c(Levels,SowYearCol,StageCol,TTCol),measure.vars=ClusterCols,variable.name='CentVar',value.name='CentValue')
    
  } else {
    Data[, `:=`(CentVar=ClusterCols,CentValue=get(ClusterCols))]
  }
  
  Formula <- paste(paste(c('CentVar',Levels,SowYearCol),collapse='+'),'~','Period')
  
  Data <- Data[, TTAnth:=get(TTCol)[get(StageCol)>=AnthStage][1], keyby=c(Levels,SowYearCol)]
  Data[is.na(TTAnth), lapply(.SD,function(x){x<-x*NA}), keyby=c('CentVar',Levels,SowYearCol), .SDcols=c('CentValue',TTCol)]
  Data <- Data[, cumTTX:=get(TTCol)-TTAnth, keyby=c('CentVar',Levels,SowYearCol)]
  Data <- Data[, Period:=cut(cumTTX,Ints,labels=IntsN), keyby=c('CentVar',Levels,SowYearCol)]
  Data <- Data[, lapply(.SD,mean,na.rm=T), keyby=c('CentVar',Levels,SowYearCol,'Period'), .SDcols='CentValue']
  Data <- Data[!is.na(Period)]
  if (ConvWide) Data <- dcast.data.table(Data,Formula,value.var='CentValue',verbose=F)
  setkeyv(Data,cols=c('CentVar',Levels,SowYearCol))
  return(Data)
}

#==============================================================================================
#==============================================================================================

APSIM_Read_Weather_Files <- function(File,ExpComments=F,HeadPatt='^year',AddDateComps=F) {
  if (length(File)>1) {
    Data <- sapply(File,APSIM_Read_Weather_Files,ExpComments=ExpComments,AddDateComps=AddDateComps,simplify=F)
    names(Data) <- File_Base_Name(names(Data))
    return(Data)
  }
  
  Comments <- readLines(File)
  Skip     <- grep(HeadPatt,Comments) - 1
  Comments <- Comments[1:(Skip+2)]
  
  Header <- read.table(File,header=F,skip=Skip,nrow=1)
  Data   <- as.data.table(read.table(File,header=F,skip=Skip+2))
  colnames(Data) <- unlist(Header)
  
  if (AddDateComps) {
    Data <- DOY2DateComps(Data, Add=T)
  }
  
  if (ExpComments) {
    Out <- list(Data=Data,Comments=Comments)
  } else {
    Out <- Data
  }
  return(Out)
}

#==============================================================================================
#==============================================================================================

APSIM_Monthly_Weather_Data <- function(Data,Years=NULL,Cols,AddCols=NULL,AddVals=NULL,Funcs=c(Sum='sum',Mean='mean',Std='Std',CInt='ConfInt')) {
  
  if('year' %in% colnames(Data)) colnames(Data)[colnames(Data)=='year'] <- 'Year'
  if (!is.null(Years)) Data <- Data[Data$Year %in% Years,]
  Dates <- DOY2Date(Data$Year,Data$day,F)
  Data  <- cbind(Year=Data$Year,Mon=Dates$mon+1,Day=Dates$mday,Data[,which(!(colnames(Data) %in% c('year','day')))])
  Data  <- Data[,c('Year','Mon','Day',Cols)]
  
  if (is.null(AddCols)) { AddCols <- 'Station'; AddVals <- 'Unknown' }
  AddTab <- matrix(AddVals,ncol=length(AddCols))
  colnames(AddTab) <- AddCols
  
  Out <- list()
  for (Fc in Funcs) {
    FcName <- names(Funcs)[Funcs==Fc]
    for (Col in Cols) {
      Temp <- Data
      Temp <- by(Data[,Col],list(Data$Mon,Data$Year),get(Fc),na.rm=T,simplify=F)
      Temp <- Fill_by_Outputs(Temp)
      Ys   <- names(Temp[1,])
      Temp <- round(matrix(unlist(Temp),ncol=12,byrow=T),3)
      colnames(Temp) <- 1:12
      Out[[FcName]][[Col]] <- cbind.data.frame(AddTab,Var=Col,Year=Ys,Temp)
      rownames(Out[[FcName]][[Col]]) <- NULL
    }
  }
  
  return(Out)
}

#==============================================================================================
#==============================================================================================

APSIM_Analyze_Weather_Data <- function(Data,Years=NULL,Cols,CalcMonthly=T,ConfInterval=0.95,AddCols=NULL,AddVals=NULL,
                                       Funcs=c(Sum='colSums',Mean='colMeans',Std='Std',CInt='ConfInt')) {
  
  if (is.null(AddCols)) { AddCols <- 'Station'; AddVals <- 'Unknown' }
  AddTab <- matrix(AddVals,ncol=length(AddCols))
  colnames(AddTab) <- AddCols
  
  if (CalcMonthly) {
    Data <- APSIM_Monthly_Weather_Data(Data,Years=Years,Cols=Cols,AddCols=AddCols,AddVals=AddVals,Funcs=c(Sum='sum',Mean='mean',Std='Std',CInt='ConfInt'))
    
    Data2 <- list()
    for (Fc in names(Data)) {
      x <- NULL
      for (Col in names(Data[[1]])) {
        x[[Col]] <- Reshape_Table_to_Long(Table=Data[[Fc]][[Col]],StatCols=c('Station','Var','Year'),VarCols=as.character(1:12),VarName='Mon',ValName=Col,RemoveOtherCols=F)
        x[[Col]]$Mon <- Factor2Numeric(x[[Col]]$Mon)
      }
      y <- NULL
      for (Col in names(Data[[1]])) {
        y <- cbind(y,x[[Col]][,Col])
      }
      colnames(y) <- Cols
      y <- cbind(x[[1]][,c('Station','Var','Year','Mon')],y)
      y <- y[order(y[,'Year']),]
      Data2[[Fc]] <- y
    }
    Data <- Data2
    
  } else {
    
    if('year' %in% colnames(Data)) colnames(Data)[colnames(Data)=='year'] <- 'Year'
    if (!is.null(Years)) Data <- Data[Data$Year %in% Years,]
    
    Dates <- DOY2Date(Data$Year,Data$day,F)
    Data  <- cbind(Year=Data$Year,Mon=Dates$mon+1,Day=Dates$mday,Data[,which(!(colnames(Data) %in% c('year','day')))])
    Data  <- Data[,c('Year','Mon','Day',Cols)]
    Data  <- list(Data=Data)
  }
  
  Out <- list()
  for (Name in names(Data)) {
    for (Fc in Funcs) {
      FcName <- names(Funcs)[Funcs==Fc]
      Data[[Name]]$Mon <- Factor2Numeric(Data[[Name]]$Mon)
      Out[[FcName]][[Name]] <- round(unlist(by(Data[[Name]][,Cols],Data[[Name]]$Mon,get(Fc),na.rm=T)),3)
      Out[[FcName]][[Name]] <- matrix(Out[[FcName]][[Name]],ncol=length(Cols),byrow=T)
      colnames(Out[[FcName]][[Name]]) <- Cols
      rownames(Out[[FcName]][[Name]]) <- 1:12
      Out[[FcName]][[Name]] <- cbind.data.frame(AddTab,Mon=1:12,Out[[FcName]][[Name]])
    }
  }
  
  if (length(Data)==1) Out <- Out[[1]]
  return(Out)
}

#==============================================================================================
#==============================================================================================

APSIM_Analyze_Weather_Data1 <- function(Data,Years=NULL,Cols,Monthly=T,ConfInterval=0.95,Station=NULL) {
  if (!is.null(Years)) Data <- Data[Data$year %in% Years,]
  if (is.null(Station)) Station <- 'Station'
  Dates <- DOY2Date(Data$year,Data$day,F)
  Data  <- cbind(Year=Data$year,Mon=Dates$mon+1,Day=Dates$mday,Data[,-c(1,2)])
  Data  <- Data[,c('Year','Mon','Day',Cols)]
  
  if (Monthly) {
    x <- list()
    for (FUNC in c('colMeans','colSums')) {
      Temp <- Data
      Temp <- by(Data[,Cols],list(Data$Mon,Data$Year),get(FUNC),na.rm=T,simplify=F)
      Temp <- matrix(unlist(Temp),ncol=length(Cols),byrow=T)
      colnames(Temp) <- Cols
      Temp2 <- cbind.data.frame(Year=rep(unique(Data$Year),each=12),Mon=unique(Data$Mon))
      x[[FUNC]] <- cbind.data.frame(Station=Station,Temp2[1:nrow(Temp),],Temp)
    }
    Data <- x
  } else {
    Data <- list(Data=Data)
  }
  
  x <- list()
  for (Name in names(Data)) {
    x[[Name]] <- list()
    for (FUNC in c('colSums','colMeans','Std','ConfInt')) {
      if (FUNC!='ConfInt') x[[Name]][[FUNC]] <- unlist(by(Data[[Name]][,Cols],Data[[Name]]$Mon,get(FUNC),na.rm=T))
      if (FUNC=='ConfInt') x[[Name]][[FUNC]] <- unlist(by(Data[[Name]][,Cols],Data[[Name]]$Mon,get(FUNC),na.rm=T,ConfInterval=0.95))
      x[[Name]][[FUNC]] <- matrix(x[[Name]][[FUNC]],byrow=T,ncol=length(Cols))
      colnames(x[[Name]][[FUNC]]) <- paste(FUNC,Cols,sep='.')
      rownames(x[[Name]][[FUNC]]) <- 1:12
      x[[Name]][[FUNC]] <- cbind(Mon=1:12,x[[Name]][[FUNC]])
    }
  }
  return(x)
}

#==============================================================================================
#==============================================================================================

Create_Daily_File_Name <- function(SimID,Factors,Values,Sep1='_',Sep2='_') {
  x <- SimID
  for (i in 1:length(Factors)) {
    x <- paste0(x,Sep2,Factors[i],Sep1,as.character(Values[i]))
  }
  x <- paste0(x,'_Daily')
  return(x)
}

#==============================================================================================
#==============================================================================================

APSIM_Write_Weather_Files <- function(Data,Comments=NULL,FileOut, AddCols=NULL) {
  if ((!is.list(Data) || (is.list(Data) && is.data.table(Data))) && is.null(Comments)) {
    stop('"Comments" must be provided!')
    
  } else if (is.null(Comments)) {
    Comments <- Data$Comments
    Out <- Data$Data
    
  } else {
    Out <- Data
  }
  
  if (!is.null(AddCols)) {
    Comments[length(Comments)-1] <- paste(Comments[length(Comments)-1], paste(AddCols, collapse='  '), sep='  ')
    Comments[length(Comments)] <- paste(Comments[length(Comments)], paste(rep('()',length(AddCols)), collapse='  '), sep='  ')
  }
  
  Cols <- c(c('year','day','radn','maxt','mint','rain','wind','evap','vp','code'), AddCols)
  Cols <- intersect(names(Data),Cols)
  
  writeLines(Comments,FileOut)
  write.table(Data[,Cols,with=F],file=FileOut,quote=F,append=T,col.names=F,row.names=F,sep=' ')
  
  return(invisible(Data))
}

#==============================================================================================
#==============================================================================================

APSIM_Change_Weather_Files <- function(File,FileOut=NULL,Changes=rep(0,12),Cols=NULL,DiffTypes='a',
                                       ChangeVP=F,Years=NULL,OutDir=NULL) {
  if (is.character(File) && length(File)>1) {
    Data <- sapply(File,APSIM_Change_Weather,...,simplify=F)
    return(Data)
  }
  
  if (length(Changes)==1)   Changes <- matrix(rep(Changes,length(Cols)*12),nrow=12)
  if (is.null(Cols)) Cols <- colnames(Changes)
  Changes <- as.matrix(Changes)
  if (!is.null(Cols) && ncol(Changes)>length(Cols)) Changes <- cbind(Changes[,c(1,2)],Changes[,Cols])
  
  if (is.list(File) && 'Data'%in%names(File)) {
    Out <- File
  } else {
    Out <- APSIM_Read_Weather_Files(File,ExpComments=T,HeadPatt='^year')
  }
  
  Comments <- Out$Comments
  Data     <- data.table(Out$Data)
  if (!is.null(Years)) Data <- Data[Data$year %in% Years]
  
  AllYears <- unlist(Data[,year])
  Months   <- MyMonth(DOY2Date(Data$day,Data$year))
  
  if (nrow(Changes)>12) {
    # Detrend
    if (length(DiffTypes)==1) DiffTypes <- rep(DiffTypes,ncol(Changes)-2)
    
    for (Col in Cols) {
      for (Yr in unique(AllYears)) {
        for (M in 1:12) {
          Type   <- DiffTypes[Cols==Col]
          Change <- Changes[Changes[,1]==Yr & Changes[,2]==M,Col]
          Rows   <- which(AllYears==Yr & Months==M)
          
          if (length(Rows)>0) {
            if (Type=='a') Data[Rows, (Col):=round(get(Col)+Change,1)]
            else if (Type=='r') Data[Rows, (Col):=round(get(Col)*Change,1)]
            else stop('Change type must be a (absolute) or r (relative)!')
          }
        }
      }
    }
    
  } else {
    # Change each calendar month
    if (length(DiffTypes)==1) DiffTypes <- rep(DiffTypes,ncol(Changes))
    
    for (Col in Cols) {
      for (M in 1:12) {
        Type   <- DiffTypes[Cols==Col]
        Change <- Changes[M,Cols==Col]
        Rows   <- which(Months==M)
        
        if (Type=='a') Data[Rows, (Col):=round(get(Col)+Change,1)]
        else if (Type=='r') Data[Rows, (Col):=round(get(Col)*Change,1)]
        else stop('Change type must be a (absolute) or r (relative)!')
      }
    }
  }
  
  if (all(c('maxt','mint') %in% Cols)) {
    Rows <- which(Data[,mint]>Data[,maxt])
    if (length(Rows)>0) {
      Data[Rows, `:=`(mint2=mint,mint=maxt)]
      Data[Rows, maxt:=mint2]
      Data[, mint2:=NULL]
    }
  }
  
  if (all(c('maxt','mint') %in% names(Data))) {
    Tav   <- rowMeans(Data[,c('maxt','mint'),with=F],na.rm=T)
    Dates <- DOY2Date(DOYs=Data$day,Years=Data$year,ToDate=T)
    Temp  <- stats::aggregate(list(Tav=Tav),list(Mon=MyMonth(Dates)),mean,na.rm=T)
    Tav   <- round(mean(Temp$Tav),2)
    Amp   <- round(diff(range(Temp$Tav,na.rm=T)),2)
    
    Temp <- NULL
    Temp <- c(Temp,paste('tav =',Tav,'(oC) ! annual average ambient temperature'))
    Temp <- c(Temp,paste('amp =',Amp,'(oC) ! annual amplitude in mean monthly temperature'))
    pos  <- max(grep('^latitude|^longitude',Comments))
    
    Comments <- Comments[-grep('^tav*|^amp*',Comments)]
    Comments <- append(Comments,Temp,pos)
    
    if (ChangeVP) {
      # Change VP based on changes in mint and maxt
    }
  }
  
  if (is.null(FileOut)) {
    if (is.null(OutDir)) FileOut <- File
    else FileOut <- paste(OutDir,basename(File),sep='/')
    
  } else {
    if (!is.null(OutDir)) FileOut <- paste(OutDir,basename(FileOut),sep='/')
  }
  
  writeLines(Comments,FileOut)
  write.table(Data,file=FileOut,quote=F,append=T,col.names=F,row.names=F,sep=' ')
  
  return(Data)
}

#==============================================================================================
#==============================================================================================

APSIM_Detrend_Weather_Files <- function(File,FileOut=NULL,Cols=c('rain','maxt','mint','vp','radn','pan'),
                                        PThr=0.5,Years=NULL,OutDir=NULL) {
  if (is.character(File) && length(File)>1) {
    Data <- sapply(File,APSIM_Detrend_Weather_Files,...,simplify=F)
    return(Data)
  }
  
  if (is.list(File) && 'Data'%in%names(File)) {
    Out <- File
  } else {
    Out <- APSIM_Read_Weather_Files(File,ExpComments=T,HeadPatt='^year')
  }
  Comments <- Out$Comments
  Data <- data.table(Out$Data)
  rm(Out)
  
  if (!is.null(Years)) Data <- Data[year %in% Years]
  Years <- sort(unique(Data$year))
  Data$Mon <- MyMonth(DOY2Date(Data$day,Data$year))
  DataOrig <- copy(Data)
  
  # ========================================
  
  DataAnn   <- Data[, lapply(.SD,mean,na.rm=T), keyby=c('year'), .SDcols=Cols]
  DataAnnDe <- copy(DataAnn)
  DataAnnDe[, year:=year-year[1]+1]
  TrendsAnn <- Trend_Analysis(DataAnn,LevCols=NULL,TimeCol='year',MainCols=Cols,Alpha=PThr,StartTime=0,Methods=1,ToDT=T)
  TrendsAnn[,`:=`(Slope=LN_SL,Interc=LN_intercept,P=LN_P)]
  
  for (Col in Cols) {
    xTr <- TrendsAnn[TrendVar==Col]
    if (xTr$P <PThr) DataAnnDe[, (Col):=xTr$Slope*year]
    if (xTr$P>=PThr) DataAnnDe[, (Col):=0]
  }
  DataAnnDe$year <- DataAnn$year
  
  for (Y in Years) {
    for (Col in Cols) {
      Rows <- Data[,year==Y]
      Trend <- as.numeric(DataAnnDe[year==Y,..Col])
      if (!is.na(Trend) && Trend!=0) {
        if (Col=='rain') {
          Trend <- 1 - Trend * Data[Rows, .N] / Data[Rows, sum(get(Col))]
          if (is.finite(Trend)) Data[Rows, rain:=round(rain*Trend,2)]
        } else {
          if (is.finite(Trend)) Data[Rows, (Col):=round(get(Col)-Trend,2)]
        }
      }
    }
  }
  if ('rain'%in%Cols) Data[rain<0,rain:=0]
  
  # ========================================
  
  DataMon <- Data[, lapply(.SD,mean,na.rm=T), keyby=c('year','Mon'), .SDcols=Cols]
  DataMonDe <- copy(DataMon)
  DataMonDe[, year:=year-year[1]+1]
  TrendsMon <- Trend_Analysis(DataMon,LevCols='Mon',TimeCol='year',MainCols=Cols,Alpha=PThr,StartTime=0,Methods=1,ToDT=T)
  TrendsMon[,`:=`(Slope=LN_SL,Interc=LN_intercept,P=LN_P)]
  
  for (M in 1:12) {
    Rows <- DataMon[Mon==M,which=T]
    
    for (Col in Cols) {
      xTr <- TrendsMon[TrendVar==Col & Mon==M]
      if (xTr$P <PThr) DataMonDe[Rows, (Col):=xTr$Slope*year]
      if (xTr$P>=PThr) DataMonDe[Rows, (Col):=0]
    }
  }
  DataMonDe$year <- DataMon$year
  
  for (Y in Years) {
    for (M in 1:12) {
      for (Col in Cols) {
        Rows <- Data[,year==Y & Mon==M]
        Trend <- as.numeric(DataMonDe[year==Y & Mon==M,..Col])
        if (!is.na(Trend) && Trend!=0) {
          if (Col=='rain') {
            Trend <- 1 - Trend * Data[Rows, .N] / Data[Rows, sum(get(Col))]
            if (is.finite(Trend)) Data[Rows, rain:=round(rain*Trend,2)]
          } else {
            if (is.finite(Trend)) Data[Rows, (Col):=round(get(Col)-Trend,2)]
          }
        }
      }
    }
  }
  if ('rain'%in%Cols) Data[rain<0,rain:=0]
  
  # ========================================
  
  if (all(c('maxt','mint') %in% names(Data))) {
    Data[maxt<mint, `:=`(maxt=mint,mint=maxt)]
    Tav   <- rowMeans(Data[,c('maxt','mint')],na.rm=T)
    Dates <- DOY2Date(Data$day,Data$year,T)
    Temp  <- stats::aggregate(list(Tav=Tav),list(Mon=MyMonth(Dates)),mean,na.rm=T)
    Tav   <- round(mean(Temp$Tav),2)
    Amp   <- round(diff(range(Temp$Tav,na.rm=T)),2)
    
    Temp <- NULL
    Temp <- c(Temp,paste('tav =',Tav,'(oC) ! annual average ambient temperature'))
    Temp <- c(Temp,paste('amp =',Amp,'(oC) ! annual amplitude in mean monthly temperature'))
    pos  <- max(grep('^latitude|^longitude',Comments))
    
    Comments <- Comments[-grep('^tav*|^amp*',Comments)]
    Comments <- append(Comments,Temp,pos)
  }
  
  if (is.null(FileOut)) {
    if (is.null(OutDir)) FileOut <- File
    else FileOut <- paste(OutDir,basename(File),sep='/')
    
  } else {
    if (!is.null(OutDir)) FileOut <- paste(OutDir,basename(FileOut),sep='/')
  }
  
  writeLines(Comments,FileOut)
  write.table(subset(Data,select=-Mon),file=FileOut,quote=F,append=T,col.names=F,row.names=F,sep=' ')
  
  return(list(Data=Data,Trends=Trends,DataOrig=DataOrig))
}


#==============================================================================================
#==============================================================================================

CC_Scenarios <- function(CCData,Targets=2050,Vars=c('mint','maxt','rain'),DiffTypes=c('a','a','r'),
                         Scens=NULL,Models=NULL,ChangeVP=F,Years=1975:2005,
                         WFolder='C:/Users/uqbababa/OneDrive - The University of Queensland/01_WeatherData/60 Locations/Base',
                         OFolder='C:/Users/uqbababa/OneDrive - The University of Queensland/01_WeatherData/60 Locations',
                         TarCol='target',ScenCol='scenarios',ModCol='models',SiteCol='sites',
                         VarCol='variables',DiffCol='diff',MonCol='month') {
  
  CCData <- as.data.frame(CCData)
  if(!dir.exists(OFolder)) dir.create(OFolder,recursive=T,showWarnings=F)
  
  CCData[,ScenCol] <- toupper(CCData[,ScenCol])
  CCData[,ModCol]  <- toupper(CCData[,ModCol])
  
  if (!is.null(Scens))  Scens  <- toupper(Scens)
  if (!is.null(Models)) Models <- toupper(Models)
  
  if (is.null(Targets)) Targets <- unique(CCData[,TarCol])
  if (is.null(Scens))   Scens   <- unique(CCData[,ScenCol])
  if (is.null(Models))  Models  <- unique(CCData[,ModCol])
  if (is.null(Vars))    Vars    <- unique(CCData[,VarCol])
  
  if (length(DiffTypes)!=length(Vars) && length(DiffTypes)==1) DiffTypes <- rep(DiffTypes,length(Vars))
  
  WFiles <- list.files(path=WFolder,pattern='.met$',full.names=T)
  
  CCData1 <- CCData[as.character(CCData[,ScenCol])%in%Scens & as.character(CCData[,TarCol])%in%Targets &
                      as.character(CCData[,ModCol])%in%Models & as.character(CCData[,VarCol])%in%Vars,]
  
  CCData1[,MonCol] <- Change_Levels(CCData1[,MonCol],month.name,1:12)
  CCData1[,MonCol] <- Factor2Numeric(CCData1[,MonCol])
  CCData1 <- MySort(CCData1,c(ScenCol,TarCol,ModCol,VarCol,SiteCol,MonCol))
  
  CCGrid <- expand.grid(Scen=Scens,Target=Targets,Model=Models,stringsAsFactors=F)
  CCGrid <- data.frame(lapply(CCGrid,as.character),stringsAsFactors=FALSE)
  CCGrid <- Add_Key(CCGrid,c('Scen','Target','Model'),KeyCol='Name',Sep='_',JustKeepKey=F)
  
  LineX()
  message('Scenarios, Target Periods and GCMs:')
  LineX()
  print(CCGrid)
  LineX()
  
  for (File in WFiles) {
    Temp <- readLines(File,n=15)
    Row  <- str_which(Temp,'station number')
    SiteNum <- as.numeric(str_split(Temp[Row],'=')[[1]][2])
    CCData2 <- CCData1[CCData1[,SiteCol]==SiteNum,]
    
    BaseData <- APSIM_Read_Weather_Files(File,ExpComments=T,HeadPatt='^year')
    
    for (gRow in 1:nrow(CCGrid)) {
      xGrid   <- CCGrid[gRow,]
      CCData3 <- CCData2[CCData2[,ScenCol]==xGrid$Scen & CCData2[,TarCol]==xGrid$Target & CCData2[,ModCol]==xGrid$Model,]
      
      Cols    <- unique(as.character(CCData3[,VarCol]))
      Changes <- CCData3[,c(MonCol,DiffCol)]
      Changes <- as.data.frame(matrix(Changes[,DiffCol],nrow=12,byrow=F))
      colnames(Changes) <- Cols
      
      xFolder <- paste(OFolder,xGrid$Name,sep='/')
      if(!dir.exists(xFolder)) dir.create(xFolder,recursive=T,showWarnings=F)
      FileOut <- paste(xGrid$Name,basename(File),sep=' - ')
      
      x <- APSIM_Change_Weather_Files(File=BaseData,FileOut=FileOut,Cols=Cols,Changes=Changes,DiffTypes=DiffTypes,
                                      ChangeVP=ChangeVP,Years=Years,OutDir=xFolder)
    }
  }
  CCData <- setDT(CCData)
  CCGrid <- setDT(CCGrid)
  save(CCData,CCGrid,file=paste0(OFolder,'/CC Scenarios.RData'))
  return(CCGrid)
}

#==============================================================================================
#==============================================================================================

APSIM_Prep_Old_Sim_File <- function(SimOrFile,StartDate='1/1/1987',EndDate='15/11/2017',RemAddVars=T,
                                    WeatherFold=NULL,HeatFile=NULL,DCaPSFile=NULL,ClContFile=NULL,
                                    minLAI=0.3,NAppFact=1,SowDate=NULL,Cultivar=NULL,
                                    TempThrshs=NULL,DCaPSChange=NULL,
                                    Project=NULL,ChangeOutFiles=T,WinOrLinix='W',
                                    SaveFile=T,OutDir=NULL,FileOut=NULL,OutFileBase=NULL) {
  MainDir <- APSIM_UQ_Main_Dir()
  
  AddHeatLobell <- !is.null(HeatFile)
  AddDCaPS <- !is.null(DCaPSFile)
  AddClCont <- !is.null(ClContFile)
  
  if (is.null(DCaPSChange)) DCaPSChange <- data.frame(psiVc=0.00,psiJ=0.00,gm25=0.0)
  if (is.null(TempThrshs)) TempThrshs <- data.frame(HtLowThrsh=26,FrLowThrsh=0)
  
  xList <- list(SowDate=SowDate,Cultivar=Cultivar,minLAI=minLAI,NApp=NAppFact,ClScen=1:length(WeatherFold),
                TempThrshs=1:nrow(TempThrshs),DCaPSChange=1:nrow(DCaPSChange),ClCont=1:length(ClContFile))
  xGrid <- expand.grid(xList)
  
  if (length(SimOrFile)==1) {
    SimX <- APSIM_Read_Sim_File(SimOrFile)
  } else {
    SimX <- SimOrFile
  }
  TitleX <- APSIM_Ext_Title_Sim_File(SimX)
  
  MetTable <- APSIM_60_Met_Stations()
  
  for (i in 1:nrow(xGrid)) {
    message(paste0('Combination number ',i,'/',nrow(xGrid),' for ',basename(SimOrFile)))
    
    Sim <- SimX
    Title <- TitleX
    
    XSowDate <- xGrid[i,'SowDate']
    XCultivar <- xGrid[i,'Cultivar']
    XminLAI <- xGrid[i,'minLAI']
    XNApp <- xGrid[i,'NApp']
    iClScen <- xGrid[i,'ClScen']
    iClCont <- xGrid[i,'ClCont']
    iTempThrshs <- xGrid[i,'TempThrshs']
    iDCaPSChange <- xGrid[i,'DCaPSChange']
    
    # Adding components to Sim and Output file names
    Sep1 <- '.'
    Sep2 <- '.'
    Add2Name <- file_path_sans_ext(basename(SimOrFile))
    if (length(SowDate)>1) Add2Name <- paste(Add2Name,paste('SowDate',XSowDate,sep=Sep1),sep=Sep2)
    if (length(Cultivar)>1) Add2Name <- paste(Add2Name,paste('Cultivar',XCultivar,sep=Sep1),sep=Sep2)
    if (length(minLAI)>1) Add2Name <- paste(Add2Name,paste('minLAI',XminLAI,sep=Sep1),sep=Sep2)
    if (length(NAppFact)>1) Add2Name <- paste(Add2Name,paste('NAppFact',XNApp,sep=Sep1),sep=Sep2)
    if (length(NAppFact)>1) Add2Name <- paste(Add2Name,paste('NAppFact',XNApp,sep=Sep1),sep=Sep2)
    
    if (AddDCaPS) {
      XDCaPSChange <- DCaPSChange[iDCaPSChange,]
      if (nrow(DCaPSChange)>1) {
        for (Col in colnames(DCaPSChange)) {
          Add2Name <- paste(Add2Name,paste(Col,as.character(XDCaPSChange[1,Col]),sep=Sep1),sep=Sep2)
        }
      }
    }
    if (AddHeatLobell) {
      XTempThrshs <- TempThrshs[iTempThrshs,]
      if (nrow(TempThrshs)>1) {
        for (Col in colnames(TempThrshs)) {
          Add2Name <- paste(Add2Name,paste(Col,as.character(XTempThrshs[1,Col]),sep=Sep1),sep=Sep2)
        }
      }
    }
    if (AddClCont) {
      XClCont <- ClContFile[iClCont]
      if (length(ClContFile)>1) Add2Name <- paste(Add2Name,paste('ClCont',iClCont,sep=Sep1),sep=Sep2)
    }
    if (!is.null(WeatherFold)) {
      XClScen <- WeatherFold[iClScen]
      if (length(WeatherFold)>1) Add2Name <- paste(Add2Name,paste('ClScen',iClScen,sep=Sep1),sep=Sep2)
    }
    Add2Name <- gsub(' ','',Add2Name)
    
    # Adding the header for XML files
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'<?xml version="1.0" encoding="utf-8"?>',AfterLine=0,AddLine=0,FileOut=NULL)
    
    # Changing sowing date, if required
    if (!is.null(SowDate)) {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='date type="text" description="Enter sowing date',NewNum=XSowDate,withProp=T,FileOut=NULL)
      Title$SowingDate <- XSowDate
      Title <- APSIM_Remake_Title_Sim_File(Title,Sep=';')
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='title',NewNum=Title$FullTit,withProp=F,FileOut=NULL)
    }
    
    # Change the cultivar
    if (!is.null(Cultivar)) {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='cultivar type="cultivars" description="Enter cultivar',NewNum=XCultivar,withProp=T,FileOut=NULL)
    }
    
    if(!is.null(XClScen)) {
      # Replacing new weather files
      if (Title$Site=='Wandering') {
        Title$Site <- 'Wickepin'
        Title$FullTit <- gsub('Wandering',Title$Site,Title$FullTit);
        Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='title',NewNum=Title$FullTit,withProp=F,FileOut=NULL)
      }
      
      Site <- Title$Site
      Site <- as.character(MetTable[MetTable$location==Site,"location"])
      Site <- Capitalize_First_Letters(Site,ignorePar=F)
      
      if (length(Site)==0) stop(paste("No weather file exists for",Site))
      
      MetFile <- Select_Met_File(WFolder=XClScen,Site=Site)
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='filename',NewNum=MetFile,withProp=T,FileOut=NULL)
    }
    
    if (ChangeOutFiles) {
      # Changing the names of the output files
      OutFiles <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='outputfile',withProp=F,isNumeric=F)
      DailyFile <- OutFiles[grep('daily',OutFiles)]
      HarvFile <- OutFiles[grep('harvest',OutFiles)]
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld=DailyFile,TextNew=paste0(Add2Name,'_Daily.out'),FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld=HarvFile,TextNew=paste0(Add2Name,'_Harvest.out'),FileOut=NULL)
    }
    
    # Adding missing parameters and correcting the incorrect one for APSIM 7.8
    pos <- grep("<executable>%apsim%\\\\Model\\\\SoilN.dll</executable>",Sim)[1]
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,"          <fbiom_lb>0</fbiom_lb>",AfterLine=pos,AddLine=2,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,"          <fbiom_ub>1</fbiom_ub>",AfterLine=pos,AddLine=3,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,"          <finert_lb>0</finert_lb>",AfterLine=pos,AddLine=4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,"          <finert_ub>1</finert_ub>",AfterLine=pos,AddLine=5,FileOut=NULL)
    
    pos <- grep('<dnit_wf_power description="power term for calculation of water factor for denitrification">',Sim)[1]
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <dnit_k1 description="K1 parameter from Thorburn et al(2010) for N2O model">25.1</dnit_k1>',AfterLine=pos,AddLine=0,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <dnit_wfps description="WFPS for calculating n2o fraction of denitrification">21.33333 100.0 </dnit_wfps>',AfterLine=pos,AddLine=1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <dnit_n2o_factor description="WFPS factor for n2o fraction of denitrification">0.0   1.18 </dnit_n2o_factor>',AfterLine=pos,AddLine=2,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <nit_n2o_frac description="Fraction of nitrified nitrogen lost as n2o">0.0 </nit_n2o_frac>',AfterLine=pos,AddLine=3,FileOut=NULL)
    
    pos <- grep('<y_senescence_fac description="daily fraction of leaf area senesced due to frost',Sim)[1]
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <x_maxt_senescence description="minimum temperature for heat senescence table (oC)">34.0 34.1 45.0</x_maxt_senescence>',AfterLine=pos,AddLine=0,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <y_heatsenescence_fac description="daily fraction of leaf area senesced due to heat (0-1)">0  0.05  0.40</y_heatsenescence_fac>',AfterLine=pos,AddLine=1,FileOut=NULL)
    
    pos <- grep('<sw_fac_max description="soil water stress factor maximum">',Sim)[1]
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <grain_no_determinant>stem</grain_no_determinant>',AfterLine=pos,AddLine=-1,FileOut=NULL)
    
    pos <- grep('<y_sw_fac_root description="stress factor for root depth growth, This may be probably the same for all crops">',Sim)[1]
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <x_afps description="Air filled pore space for effects on root depth growth">0.0 0.02</x_afps>',AfterLine=pos,AddLine=0,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <y_afps_fac_root description="Air filled pore space factor for root depth growth">1 1</y_afps_fac_root>',AfterLine=pos,AddLine=1,FileOut=NULL)
    
    Sim <- APSIM_Rep_Text_in_Sim_File(Sim,'.Stage_name','.StageName',FileOut=NULL)
    
    # Adding DCaPS model manager block
    if (AddDCaPS) {
      DCaPSBlock <- APSIM_DCaPS_Change(DCaPSFile=DCaPSFile,Changes=XDCaPSChange,FileOut=NULL)
      pos <- grep('<component name="Sow on a fixed date" executable="%apsim%\\\\Model\\\\Manager.dll">',Sim)[1]
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,DCaPSBlock,AfterLine=pos,AddLine=-1,FileOut=NULL)
      
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='laiTrigger',NewNum=XminLAI,withProp=T,FileOut=NULL)
    }
    
    # Adding heat stress manager block (Lobell et al., 2015)
    if (AddHeatLobell) {
      Block <- APSIM_Read_Block(HeatFile)
      pos <- grep('<component name="Reset water, nitrogen and surfaceOM on sowing" executable="%apsim%\\\\Model\\\\Manager.dll">',Sim)[1]
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,Block,AfterLine=pos,AddLine=-1,FileOut=NULL)
      for (Col in names(XTempThrshs)) Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName=Col,NewNum=as.character(XTempThrshs[1,Col]),withProp=T,FileOut=NULL)
    }
    
    # Adding climate control module
    if (AddClCont) {
      if (basename(XClCont)!='NA') {
        ClContBlock <- APSIM_Read_Block(XClCont)
        pos <- grep('<system name="paddock" executable=',Sim)[1]
        Sim <- APSIM_Append_Block_to_Sim_File(Sim,ClContBlock,AfterLine=pos,AddLine=-1,FileOut=NULL)
      }
    }
    
    # Add the cultivar Axe
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <axe cultivar="yes">',AfterLine='<janz cultivar="yes">',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <derived_from>base_cultivar</derived_from>',AfterLine='<janz cultivar="yes">',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <vern_sens>1.5</vern_sens>',AfterLine='<janz cultivar="yes">',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'          <photop_sens>1.5</photop_sens>',AfterLine='<janz cultivar="yes">',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        </axe>',AfterLine='<janz cultivar="yes">',AddLine=-1,FileOut=NULL)
    
    # Removing unnecesary daily output variables
    if (RemAddVars) {
      AfterLine  <- NULL
      BeforeLine <- '<component name="outputfile_harvest"'
      
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>year</variable>',AfterLine='<component name="outputfile_daily"',BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>DaysAfterSowing</variable>',AfterLine,BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.flow_das</variable>',AfterLine,BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>cumTT</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>stage</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.hi</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.biomass_stem_to_grain</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.avgTemp_sowing_to_harvest</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.cumRadn</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.min_sdr</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.min_sdr_stage</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.min_sln</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.min_sln_stage</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.n_uptake_flow</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>nsowing_report</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n37_report</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.biomass_stem_maturity</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.biomass_stem_flow</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>pawc37</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n30_report</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>cumRain30</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>logic.biomass_flow</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine=NULL,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>zadok_stage</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>rain</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>yield</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>grain_protein</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>lai</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>biomass</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>ep</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n_uptake</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n_stress_expan</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n_stress_grain</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts="<variable>nh4",AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts="<variable>no3",AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>sw</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>nsowing_report</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n30_report</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>n37_report</variable>',AfterLine,BeforeLine,FileOut=NULL)
      Sim <- APSIM_Remove_Lines_in_Sim_File(Sim,Texts='<variable>year</variable>',AfterLine='<component name="outputfile_harvest"',BeforeLine='<outputfrequency>harvesting</outputfrequency>',FileOut=NULL)
    }
    
    # Adding additional output variables
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>year</variable>',AfterLine='<component name="outputfile_harvest"',AddLine=4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>CO2</variable>',AfterLine='<component name="outputfile_harvest"',AddLine=5,FileOut=NULL)
    
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>year</variable>',AfterLine='<component name="outputfile_daily"',AddLine=5,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>CO2</variable>',AfterLine='<component name="outputfile_daily"',AddLine=6,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>wheat.sln</variable>',AfterLine='<component name="outputfile_harvest"',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>biomass</variable>',AfterLine='<component name="outputfile_harvest"',AddLine=-4,FileOut=NULL)
    if (AddDCaPS) Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>dcap</variable>',AfterLine='<component name="outputfile_harvest"',AddLine=-4,FileOut=NULL)
    
    # Adding phenological traits
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>emergence_das</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>flowering_das</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>maturity_das</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    
    # Adding cumulative or averaged output variables
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sdrSum</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rad_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rad_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rad_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rain_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rain_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Cum_Rain_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMin_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMin_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMin_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMax_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMax_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMax_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMean_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMean_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_TMean_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_SLN_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_SLN_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>Mean_SLN_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    
    if (AddHeatLobell) {
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>GNHeatLossTiller</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>GNHeatLoss</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>GWHeatLoss</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>FrostLoss</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>FrostedYield</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>HeatedYield</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>StressedYield</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>FrostLossBell</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
      Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>HeatLossBell</variable>',AfterLine='<component name="accum" executable=',AddLine=-4,FileOut=NULL)
    }
    
    # Change Applied amounts of N by multiplying by NAppFact
    if (!is.null(NAppFact) && XNApp!=1) {
      Sim <- APSIM_Numbers_in_a_Line(Sim,Pattern='<fert_amount_sow.*>',Factors=XNApp,Replace=T,FileOut=NULL)
      Sim <- APSIM_Numbers_in_a_Line(Sim,Pattern='fertiliser apply amount',Factors=c(XNApp,1),Replace=T,FileOut=NULL)
      Sim <- APSIM_Numbers_in_a_Line(Sim,Pattern='n30_report',Factors=c(1,XNApp),Replace=T,FileOut=NULL)
      Sim <- APSIM_Numbers_in_a_Line(Sim,Pattern='n37_report',Factors=c(1,XNApp),Replace=T,FileOut=NULL)
    }
    
    # Adding the Tracker component
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'	<component name="Tracker" executable="%apsim%/Model/Tracker.dll" class="Tracker">',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'      <executable name="%apsim%/Model/Tracker.dll" version="1.0" />',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'      <initdata>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of Logic.sdr on end_of_day from sowing to maturity as sdrSum</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of radn on end_of_day from sowing to flowering as Cum_Rad_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of radn on end_of_day from flowering to maturity as Cum_Rad_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of radn on end_of_day from sowing to maturity as Cum_Rad_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of rain on end_of_day from sowing to flowering as Cum_Rain_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of rain on end_of_day from flowering to maturity as Cum_Rain_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>sum of rain on end_of_day from sowing to maturity as Cum_Rain_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of mint on end_of_day from sowing to flowering as Mean_TMin_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of mint on end_of_day from flowering to maturity as Mean_TMin_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of mint on end_of_day from sowing to maturity as Mean_TMin_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of maxt on end_of_day from sowing to flowering as Mean_TMax_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of maxt on end_of_day from flowering to maturity as Mean_TMax_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of maxt on end_of_day from sowing to maturity as Mean_TMax_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of avgTemp on end_of_day from sowing to flowering as Mean_TMean_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of avgTemp on end_of_day from flowering to maturity as Mean_TMean_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of avgTemp on end_of_day from sowing to maturity as Mean_TMean_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of wheat.sln on end_of_day from sowing to flowering as Mean_SLN_0_6</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of wheat.sln on end_of_day from flowering to maturity as Mean_SLN_6_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'        <variable>average of wheat.sln on end_of_day from sowing to maturity as Mean_SLN_0_9</variable>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'      </initdata>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    Sim <- APSIM_Append_Block_to_Sim_File(Sim,'    </component>',AfterLine='<component name="accum" executable=',AddLine=-1,FileOut=NULL)
    
    # Change the commencement and end dates of simulation
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='start_date',NewNum=StartDate,withProp=T,FileOut=NULL)
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='end_date',NewNum=EndDate,withProp=T,FileOut=NULL)
    
    # Chenge .dll extensions to .co for Linux
    if (tolower(WinOrLinix)=='l') {
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Plant.dll',TextNew='/Model/Plant.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\SoilN.dll',TextNew='/Model/SoilN.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\SurfaceOM.dll',TextNew='/Model/SurfaceOM.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\SoilWat.dll',TextNew='/Model/SoilWat.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Manager.dll',TextNew='/Model/Manager.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='/Model/Manager.dll',TextNew='/Model/Manager.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Fertiliser.dll',TextNew='/Model/Fertiliser.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Accum.dll',TextNew='/Model/Accum.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Report.dll',TextNew='/Model/Report.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\ProtocolManager.dll',TextNew='/Model/ProtocolManager.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Input.dll',TextNew='/Model/Input.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='\\\\Model\\\\Clock.dll',TextNew='/Model/Clock.so',FileOut=NULL)
      Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld='/Model/Tracker.dll',TextNew='/Model/Tracker.so',FileOut=NULL)
    }
    
    if (SaveFile) {
      if (is.null(FileOut)) FileOut2 <- paste0(OutDir,'/',gsub('-','_',Add2Name),'.sim')
      else FileOut2 <- paste0(OutDir,'/',basename(FileOut))
      APSIM_Write_Sim_File(Sim=Sim,SimFile=FileOut2,Dir=NULL)
    }
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

Select_Met_File <- function(WFolder,Site) {
  WeathFiles1 <- List_Files_by_Ext(Dir=WFolder,Ext='met',AlTogether=F)
  WeathFiles2 <- lapply(WeathFiles1,function(x){gsub('\\s+','',x)})
  Site        <- gsub('\\s+','',Site)
  MetFile     <- WeathFiles1[grep(tolower(Site),tolower(WeathFiles2))]
  return(MetFile)
}

#==============================================================================================
#==============================================================================================

APSIM_Gen_New_APSIM_Files <- function(SimOrFile,WFold=NULL,StartDate='1/1/1987',EndDate='15/11/2017',Params=NULL,OutDir=NULL,FileOut=NULL,
                                      BaseSim=NULL,Soils=NULL,SoilNames=NULL,SimNameCol=NULL,
                                      SaveFile=T,Append=F,KeepBaseSim=F,ResetSoilName=F) {
  
  Params <- as.data.frame(Params)
  Parameters <- c('Site','WScen','WFold',"StartDate","EndDate","SowDate","Cultivar","Density",'Location','MaxRootDepth',
                  "NO3Sow","UreaSow","NO3Z30SwType","NO3Z30SwMin","NO3Z30SwMax",
                  "NO3Z30CumRain","NO3Z30","NO3Z37SwType","IFESW","Pest",
                  "NO3Z37SwMin","NO3Z37SwMax","NO3Z37CumRain","NO3Z37",
                  'CO2Year1','CO2Year2','CO2Conc1','CO2Conc2','CO2Slope')
  
  # Params <- data.frame(ID=0,Site='Badgingarra',StartDate=NA,
  #                      EndDate=NA,Density=153,UreaSow=19,
  #                      NO3Sow=17,NO3Z30SwType=1,NO3Z30SwMin=62,
  #                      NO3Z30SwMax=199,NO3Z30CumRain=64,NO3Z30=12,
  #                      NO3Z37SwType=1,NO3Z37SwCrit=62,NO3Z37SwMax=199,
  #                      NO3Z37CumRain=63,NO3Z37=21)
  
  for (Par in Parameters) if(!Par%in%colnames(Params)) Params[,Par] <- NA
  
  MainDir <- APSIM_UQ_Main_Dir()
  if (is.null(WFold)) WFold <- paste(MainDir,'01_WeatherData/60 Locations/Base',sep='/')
  
  if (!is.null(StartDate) && !is.na(StartDate)) Params$StartDate <- NA
  if (!is.null(EndDate) && !is.na(EndDate))   Params$EndDate <- NA
  
  if (length(SimOrFile)==1) {
    SimX <- APSIM_Read_Sim_File(SimOrFile)
  } else {
    SimX <- SimOrFile
  }
  
  if (is.null(BaseSim)) {
    BaseSim <- str_trim(gsub('<simulation name="([[:alpha:]]+)">','\\1',SimX[grep('<simulation',SimX)[1]]))
  }
  
  Temp1 <- grep(sprintf('<simulation name="%s">',BaseSim),SimX)
  Temp2 <- grep('</simulation>',SimX)
  Temp2 <- Temp2[Temp2>Temp1][1]
  SRows <- Temp1:Temp2
  
  if (Append) {
    SimAll <- SimX
  }
  
  for (i in nrow(Params):1) {
    xParams <- Params[i,]
    
    if (Append) {
      Sim <- Sim[SRows]
    } else {
      Sim <- SimX
    }
    
    if (is.na(xParams$WFold)) xParams$WFold <- WFold
    if ('WFile' %in% names(xParams)) {
      MetFile <- paste(xParams$WFold,xParams$WFile,sep='/')
    } else {
      MetFile <- Select_Met_File(WFolder=xParams$WFold,Site=xParams$Site)
    }
    
    if (is.null(StartDate)) StartDate <- NA
    if (is.null(EndDate)) EndDate <- NA
    StartDate2 <- ifelse(is.na(xParams$StartDate),StartDate,xParams$StartDate)
    EndDate2 <- ifelse(is.na(xParams$EndDate),EndDate,xParams$EndDate)
    
    if (!is.na(StartDate2)) Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='start_date',NewNum=StartDate2,withProp=T,FileOut=NULL)
    if (!is.na(EndDate2)) Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='end_date',NewNum=EndDate2,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$Location))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Location',NewNum=xParams$Location,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$MaxRootDepth)) {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='MaxRootDepth',NewNum=xParams$MaxRootDepth,withProp=T,FileOut=NULL)
    } else {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='MaxRootDepth',NewNum=-1,withProp=T,FileOut=NULL)
    }
    
    if (!is.na(xParams$Site))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Site',NewNum=xParams$Site,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$Cultivar))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='cultivar type="cultivars"',
                                           AfterLine='<manager name="Sow on a fixed date">',
                                           BeforeLine='<row_spacing type="text"',
                                           NewNum=xParams$Cultivar,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$SowDate)) {
      if (is.Date(xParams$SowDate)) xParams$SowDate <- format(xParams$SowDate,format='%d-%b')
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='date type="text"',
                                           AfterLine='<manager name="Sow on a fixed date">',
                                           BeforeLine='<row_spacing type="text"',
                                           NewNum=xParams$SowDate,withProp=T,FileOut=NULL)
    }
    
    if ('SowDates'%in%names(xParams) && !is.na(xParams$SowDates)) {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='date',
                                           AfterLine='<factor name="Sowing">',
                                           BeforeLine='<manager name="Sow on a fixed date">',
                                           NewNum=xParams$SowDates,withProp=F,FileOut=NULL)
    }
    
    if (!is.na(MetFile))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='filename',AfterLine='<metfile',BeforeLine='</metfile>',
                                           NewNum=MetFile,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$Density))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='density type="text"',
                                           AfterLine='<manager name="Sow on a fixed date">',
                                           BeforeLine='<row_spacing type="text"',
                                           NewNum=xParams$Density,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Sow))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='fert_amount_sow type="text"',
                                           AfterLine='<manager name="Sowing fertiliser NO3_N">',
                                           BeforeLine='fert_type_sow type="list"',
                                           NewNum=xParams$NO3Sow,withProp=T,FileOut=NULL)
    if (!is.na(xParams$UreaSow))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='fert_amount_sow type="text"',
                                           AfterLine='<manager name="Sowing fertiliser Urea_N">',
                                           BeforeLine='fert_type_sow type="list"',
                                           NewNum=xParams$UreaSow,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$NO3Z30SwType))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_type type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 30 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z30SwType,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z30SwMin))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_min type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 30 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z30SwMin,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z30SwMax))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_max type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 30 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z30SwMax,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z30CumRain))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='cumrain_crit type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 30 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z30CumRain,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z30))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='fert_amount type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 30 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z30,withProp=T,FileOut=NULL)
    
    if (!is.na(xParams$NO3Z37SwType))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_type type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 37 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z37SwType,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z37SwMin))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_min type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 37 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z37SwMin,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z37SwMax))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='esw_max type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 37 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z37SwMax,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z37CumRain))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='cumrain_crit type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 37 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z37CumRain,withProp=T,FileOut=NULL)
    if (!is.na(xParams$NO3Z37))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='fert_amount type="text"',
                                           AfterLine='<manager name="Fertilise at zadok stage 37 NO3_N">',
                                           BeforeLine='<fert_type type="list"',
                                           NewNum=xParams$NO3Z37,withProp=T,FileOut=NULL)
    
    Temp <- paste(xParams$ID,'Seasonal.out')
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='filename output="yes"',AfterLine='<outputfile name="Seasonal">',
                                         BeforeLine='<variables name="Variables">',
                                         NewNum=Temp,withProp=T,FileOut=NULL)
    
    Temp <- paste(xParams$ID,'Seasonal')
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='title',AfterLine='<outputfile name="Seasonal">',
                                         BeforeLine='<variables name="Variables">',NewNum=Temp,withProp=F,FileOut=NULL)
    
    Temp <- paste(xParams$ID,'Daily.out')
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='filename output="yes"',AfterLine='<outputfile name="Daily">',
                                         BeforeLine='<variables name="Variables">',
                                         NewNum=Temp,withProp=T,FileOut=NULL)
    Temp <- paste(xParams$ID,'Daily')
    Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='title',AfterLine='<outputfile name="Daily">',
                                         BeforeLine='<variables name="Variables">',NewNum=Temp,withProp=F,FileOut=NULL)
    
    if (!is.na(xParams$CO2Year1))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Year1 type="text"',
                                           AfterLine='<manager name="CO2 Change">',
                                           BeforeLine='<event>prenewmet</event>',
                                           NewNum=xParams$CO2Year1,withProp=T,FileOut=NULL)
    if (!is.na(xParams$CO2Year2))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Year2 type="text"',
                                           AfterLine='<manager name="CO2 Change">',
                                           BeforeLine='<event>prenewmet</event>',
                                           NewNum=xParams$CO2Year2,withProp=T,FileOut=NULL)
    if (!is.na(xParams$CO2Conc1))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='C1 type="text"',
                                           AfterLine='<manager name="CO2 Change">',
                                           BeforeLine='<event>prenewmet</event>',
                                           NewNum=xParams$CO2Conc1,withProp=T,FileOut=NULL)
    if (!is.na(xParams$CO2Conc2))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='C2 type="text"',
                                           AfterLine='<manager name="CO2 Change">',
                                           BeforeLine='<event>prenewmet</event>',
                                           NewNum=xParams$CO2Conc2,withProp=T,FileOut=NULL)
    if (!is.na(xParams$CO2Slope))
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Slope type="text"',
                                           AfterLine='<manager name="CO2 Change">',
                                           BeforeLine='<event>prenewmet</event>',
                                           NewNum=xParams$CO2Slope,withProp=T,FileOut=NULL)
    if (!is.null(Soils)) {
      pos1  <- grep('<Soil>',Sim)
      pos2  <- grep('</Soil>',Sim)
      for (is in 1:length(pos1)) {
        Sim   <- Sim[-seq(pos1[is],pos2[is])]
        if ('Soil' %in% names(xParams)) {
          xSoil <- Soils[[as.character(xParams$Soil)]]
        } else {
          xSoil <- Soils[[as.character(xParams$ID)]]
        }
        xSoil <- sapply(xSoil,function(x){return(paste0('      ',x))},USE.NAMES=F)
        if (ResetSoilName) {
          pos <- grep('<Soil',xSoil)
          xSoil[pos] <- gsub('<Soil [[:print:]]+','<Soil name="Soil">',xSoil[pos])
        }
        Sim   <- APSIM_Append_Block_to_Sim_File(Sim,Block=xSoil,AfterLine=pos1[is],AddLine=-1,FileOut=NULL)
      }
    }
    
    if (!is.na(xParams$IFESW)) {
      xParams$IFESW <- round(as.numeric(xParams$IFESW),2)
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='FractionFull',AfterLine='<InitialWater>',BeforeLine='</InitialWater>',
                                           NewNum=xParams$IFESW,withProp=F,FileOut=NULL)
    }
    
    if (!is.na(xParams$Pest)) {
      Sim <- APSIM_Change_Attr_in_Sim_File(Sim,AttrName='Pest',AfterLine=NULL,BeforeLine=NULL,
                                           NewNum=xParams$Pest,withProp=T,FileOut=NULL)
    }
    
    if (is.null(SimNameCol)) SimNameCol <- 'ID'
    Sim <- APSIM_Rep_Text_in_Sim_File(Sim,TextOld=BaseSim,TextNew=as.character(xParams[,SimNameCol]))
    
    if (Append) {
      SimAll <- append(SimAll,Sim,after=max(SRows))
      
    } else {
      if (SaveFile) {
        if (is.null(FileOut)) FileOut2 <- paste0(xParams[,SimNameCol],'.apsim')
        else FileOut2 <- basename(FileOut)
        FileOut2 <- paste0(OutDir,'/',basename(FileOut2))
        APSIM_Write_Sim_File(Sim=Sim,SimFile=FileOut2,Dir=NULL)
      }
    }
  }
  
  if (SaveFile && Append) {
    if (!KeepBaseSim) SimAll <- SimAll[-SRows]
    if (is.null(FileOut)) FileOut2 <- SimOrFile
    else FileOut2 <- basename(FileOut)
    FileOut2 <- paste0(OutDir,'/',basename(FileOut2))
    APSIM_Write_Sim_File(Sim=SimAll,SimFile=FileOut2,Dir=NULL)
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Sim_Soil_to_Apsim <- function(SimFile=NULL,Crop='wheat',FileOut=NULL) {
  
  if (length(SimFile)>1) {
    Soil <- sapply(SimFile,APSIM_Sim_Soil_to_Apsim,FileOut=NULL,simplify=F)
    names(Soil) <- File_Base_Name(SimFile)
    return(Soil)
  }
  
  Sim <- APSIM_Read_Sim_File(SimFile)
  
  Arrays  <- c('dlayer','bd','air_dry','ll15','dul','sat','ll','kl','xf','swcon','oc',
               'fbiom','finert','ph','sw','no3ppm','nh4ppm')
  
  for (Var in Arrays) {
    x <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName=Var,withProp=F,isNumeric=F)
    if (is.character(x)) {
      x <- as.numeric(strsplit(x,split='\\s+')[[1]])
      if (Var!='dlayer' && length(x)<length(dlayer)) {
        x <- c(x,rep(x[length(x)],length(dlayer)-length(x)))
      }
    }
    assign(Var,x)
  }
  NL <- length(dlayer)
  
  Chars   <- c('ModifyKL','MaxRootDepth','SummerDate','WinterDate')
  
  for (Var in Chars) {
    x <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName=Var,withProp=F,isNumeric=F)
    assign(Var,x)
  }
  
  Scalars <- c('SummerCona','SummerU','WinterCona','WinterU','diffus_const','diffus_slope','salb',
               'cn2_bare','cn_red','cn_cov','root_cn','root_wt','soil_cn','enr_a_coeff','enr_b_coeff','profile_fesw')
  
  for (Var in Scalars) {
    x <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName=Var,withProp=F,isNumeric=T)
    assign(Var,x)
  }
  
  FUNC <- function(X,Name,Params) {
    X <- c(X,sprintf('    <%s>',Name))
    for (i in 1:length(Params)) X <- c(X,sprintf('      <double>%s</double>',Params[i]))
    X <- c(X,sprintf('    </%s>',Name))
    return(X)
  }
  
  Soil <- c('<Soil>')
  Soil <- c(Soil,'  <Water>')
  
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'BD',bd)
  Soil <- FUNC(Soil,'AirDry',air_dry)
  Soil <- FUNC(Soil,'LL15',ll15)
  Soil <- FUNC(Soil,'DUL',dul)
  Soil <- FUNC(Soil,'SAT',sat)
  
  Soil <- c(Soil,sprintf('    <SoilCrop name="%s">',Crop))
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'LL',ll)
  Soil <- FUNC(Soil,'KL',kl)
  Soil <- FUNC(Soil,'XF',xf)
  Soil <- c(Soil,'    </SoilCrop>')
  
  Soil <- c(Soil,'  </Water>')
  Soil <- c(Soil,'  <SoilWater>')
  
  Soil <- c(Soil,sprintf('    <SummerCona>%s</SummerCona>',SummerCona))
  Soil <- c(Soil,sprintf('    <SummerU>%s</SummerU>',SummerU))
  Soil <- c(Soil,sprintf('    <SummerDate>%s</SummerDate>',SummerDate))
  Soil <- c(Soil,sprintf('    <WinterCona>%s</WinterCona>',WinterCona))
  Soil <- c(Soil,sprintf('    <WinterU>%s</WinterU>',WinterU))
  Soil <- c(Soil,sprintf('    <WinterDate>%s</WinterDate>',WinterDate))
  Soil <- c(Soil,sprintf('    <DiffusConst>%s</DiffusConst>',diffus_const))
  Soil <- c(Soil,sprintf('    <DiffusSlope>%s</DiffusSlope>',diffus_slope))
  Soil <- c(Soil,sprintf('    <Salb>%s</Salb>',salb))
  Soil <- c(Soil,sprintf('    <CN2Bare>%s</CN2Bare>',cn2_bare))
  Soil <- c(Soil,sprintf('    <CNRed>%s</CNRed>',cn_red))
  Soil <- c(Soil,sprintf('    <CNCov>%s</CNCov>',cn_cov))
  Soil <- c(Soil,sprintf('    <Slope>%s</Slope>',NaN))
  Soil <- c(Soil,sprintf('    <DischargeWidth>%s</DischargeWidth>',NaN))
  Soil <- c(Soil,sprintf('    <CatchmentArea>%s</CatchmentArea>',NaN))
  Soil <- c(Soil,sprintf('    <MaxPond>%s</MaxPond>',NaN))
  
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'SWCON',swcon)
  Soil <- c(Soil,'  </SoilWater>')
  Soil <- c(Soil,'  <SoilOrganicMatter>')
  
  Soil <- c(Soil,sprintf('    <RootCN>%s</RootCN>',root_cn))
  Soil <- c(Soil,sprintf('    <RootWt>%s</RootWt>',root_wt))
  Soil <- c(Soil,sprintf('    <SoilCN>%s</SoilCN>',soil_cn))
  Soil <- c(Soil,sprintf('    <EnrACoeff>%s</EnrACoeff>',enr_a_coeff))
  Soil <- c(Soil,sprintf('    <EnrBCoeff>%s</EnrBCoeff>',enr_b_coeff))
  
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'OC',oc)
  Soil <- FUNC(Soil,'FBiom',fbiom)
  Soil <- FUNC(Soil,'FInert',finert)
  Soil <- c(Soil,sprintf('    <OCUnits>%s</OCUnits>','Total'))
  
  Soil <- c(Soil,'  </SoilOrganicMatter>')
  Soil <- c(Soil,'  <Analysis>')
  
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'PH',ph)
  Soil <- c(Soil,sprintf('    <PHUnits>%s</PHUnits>','Water'))
  Soil <- c(Soil,sprintf('    <BoronUnits>%s</BoronUnits>','HotWater'))
  
  Soil <- c(Soil,'  </Analysis>')
  Soil <- c(Soil,'  <InitialWater>')
  
  Soil <- c(Soil,sprintf('    <FractionFull>%s</FractionFull>',profile_fesw))
  Soil <- c(Soil,sprintf('    <DepthWetSoil>%s</DepthWetSoil>',NaN))
  Soil <- c(Soil,sprintf('    <PercentMethod>%s</PercentMethod>','FilledFromTop'))
  Soil <- c(Soil,sprintf('    <RelativeTo>%s</RelativeTo>','ll15'))
  
  Soil <- c(Soil,'  </InitialWater>')
  Soil <- c(Soil,'  <Sample name="InitialNitrogen">')
  
  Soil <- FUNC(Soil,'Thickness',dlayer)
  Soil <- FUNC(Soil,'NO3',no3ppm)
  Soil <- FUNC(Soil,'NH4',nh4ppm)
  
  Soil <- c(Soil,sprintf('    <NO3Units>%s</NO3Units>','ppm'))
  Soil <- c(Soil,sprintf('    <NH4Units>%s</NH4Units>','ppm'))
  Soil <- c(Soil,sprintf('    <SWUnits>%s</SWUnits>','Volumetric'))
  Soil <- c(Soil,sprintf('    <OCUnits>%s</OCUnits>','Total'))
  Soil <- c(Soil,sprintf('    <PHUnits>%s</PHUnits>','Water'))
  
  Soil <- c(Soil,'  </Sample>')
  Soil <- c(Soil,'</Soil>')
  
  if (!is.null(FileOut)) writeLines(Soil,FileOut)
  
  return(Soil)
}

#==============================================================================================
#==============================================================================================

APSIM_Ext_Crop_Management <- function(SimFile=NULL,FileOut=NULL) {
  
  if (length(SimFile)>1) {
    CropMan <- sapply(SimFile,APSIM_Ext_Crop_Management,FileOut=NULL,simplify=F)
    CropMan <- List_RBind(CropMan)
    return(CropMan)
  }
  
  Sim <- APSIM_Read_Sim_File(SimFile)
  
  StartDate <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='start_date',withProp=T,isNumeric=F,
                                           AfterLine='<timestep_events>',BeforeLine='<component name="met"')
  
  EndDate <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='end_date',withProp=T,isNumeric=F,
                                         AfterLine='<timestep_events>',BeforeLine='<component name="met"')
  
  SowDate <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='date',withProp=T,isNumeric=F,
                                         AfterLine='<component name="Sow on a fixed date"',
                                         BeforeLine='<component name="Harvesting rule"')
  
  Density <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='density',withProp=T,isNumeric=T,
                                         AfterLine='<component name="Sow on a fixed date',
                                         BeforeLine='<component name="Harvesting rule"')
  
  NO3Sow <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='fert_amount_sow',withProp=T,isNumeric=T,
                                        AfterLine='<component name="Sowing fertiliser NO3"',
                                        BeforeLine='<component name="Sowing fertiliser urea"')
  
  UreaSow <- APSIM_Read_Attr_in_Sim_File(Sim,AttrName='fert_amount_sow',withProp=T,isNumeric=T,
                                         AfterLine='<component name="Sowing fertiliser urea"',
                                         BeforeLine='<component name="logic" executable')
  NO3Z30SwType <- 1
  NO3Z30SwMin <- 0
  NO3Z30SwMax <- 1000
  
  pos1 <- grep('if \\(zadok_stage>=30',Sim)
  pos2 <- grep('if \\(cumRain',Sim)
  pos2 <- min(pos2[pos2>pos1])
  pos3 <- grep('n30_report=',Sim)
  pos3 <- min(pos3[pos3>pos2])
  
  NO3Z30CumRain <- gsub('.*>=(\\d+)\\)','\\1',Sim[pos2])
  NO3Z30 <- gsub('.*=(\\d+)','\\1',Sim[pos3])
  
  NO3Z37SwType <- ifelse(length(grep('if \\(10=10\\)',Sim))>0,2,1)
  NO3Z37CumRain <- 0
  
  pos1 <- grep('if \\(zadok_stage>=37',Sim)
  pos2 <- grep('if \\(pawc>=',Sim)
  pos2 <- min(pos2[pos2>pos1])
  pos3 <- grep('n37_report=',Sim)
  pos3 <- min(pos3[pos3>pos2])
  
  NO3Z37SwMin <- gsub('.*>=(\\d+) and pawc<(\\d+)\\)','\\1',Sim[pos2])
  NO3Z37SwMax <- gsub('.*>=(\\d+) and pawc<(\\d+)\\)','\\2',Sim[pos2])
  NO3Z37 <- gsub('.*=(\\d+)','\\1',Sim[pos3])
  
  CropMan <- data.frame(ID=File_Base_Name(SimFile),
                        StartDate=StartDate,
                        EndDate=EndDate,
                        SowDate=SowDate,
                        Density=Density,
                        NO3Sow=NO3Sow,
                        UreaSow=UreaSow,
                        NO3Z30SwType=NO3Z30SwType,
                        NO3Z30SwMin=NO3Z30SwMin,
                        NO3Z30SwMax=NO3Z30SwMax,
                        NO3Z30CumRain=NO3Z30CumRain,
                        NO3Z30=NO3Z30,
                        NO3Z37SwType=NO3Z37SwType,
                        NO3Z37SwMin=NO3Z37SwMin,
                        NO3Z37SwMax=NO3Z37SwMax,
                        NO3Z37CumRain=NO3Z37CumRain,
                        NO3Z37=NO3Z37)
  return(CropMan)
}

#==============================================================================================
#==============================================================================================

APSIM_Change_Version <- function(Sim,Version=37) {
  
  if (length(Sim)==1 && is.character(Sim)) Sim <- APSIM_Read_Sim_File(Sim)
  
  if (!is.null(Version)) {
    Comm <- switch(as.character(Version),
                   '36' = '<folder version="36" creator="Apsim 7.8-r3867" name="simulations">',
                   '37' = '<folder version="37" creator="Apsim 7.9-r4131" name="simulations">'
    )
    Pos <- grep('<folder version',Sim)
    Sim[Pos] <- Comm
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIMNG_Extract_Block_from_JSON <- function(Input,Type,Attrs=NULL,Values=NULL,ReturnLines=T,ReturnList=F) {
  
  if (length(Input)==1) Lines <- readLines(Input,warn=F) else Lines <- Input
  LNType <- str_which(Lines,sprintf('type": "%s,',Type))
  
  if (is.null(Attrs)) {
    LNTypeX <- LNType
    
  } else {
    
    LNTypeX <- NULL
    for (i in 1:length(LNType)) {
      Found <- 0
      for (j in setdiff((1:100)+LNType[i],LNType[i])) {
        if (str_detect(Lines[j],'type"')) if (j-LNType[i]>0) break else next
        
        for (a in 1:length(Attrs)) {
          LN1 <- str_detect(Lines[j],paste0('"',Attrs[a],'": "',Values[a]))
          LN2 <- str_detect(Lines[j],paste0('"',Attrs[a],'": ',Values[a]))
          if (LN1 && LN2) Found <- Found + 1
        }
      }
      
      if (Found==length(Attrs)) {
        LNTypeX <- c(LNTypeX,LNType[i])
        next
      }
    }
  }
  
  OutL <- OutB <- list()
  for (i in 1:length(LNTypeX)) {
    LNType <- LNTypeX[i]
    StartLine <- LNType-1
    
    LineX1 <- Lines[LNType-1]
    LineX2 <- gsub('\\{','\\}',LineX1)
    
    EndLine <- grep(paste0('^',LineX2),Lines)
    EndLine <- EndLine[EndLine>LNType][1]
    
    OutB[[i]] <- Lines[StartLine:EndLine]
    OutL[[i]] <- c(StartLine,EndLine)
  }
  
  if (i==1 && !ReturnList) {
    OutB <- OutB[[1]]
    OutL <- OutL[[1]]
  }
  
  if (ReturnLines) return(OutL) else return (OutB)
}

#==============================================================================================
#==============================================================================================

APSIMNG_Change_Block_in_JSON <- function(Input,Type,Attrs,Values,ChangeAttr,ToValues,IsNum) {
  if (length(Input)==1) Lines <- readLines(Input,warn=F) else Lines <- Input
  
  Block  <- APSIMNG_Extract_Block_from_JSON(Input=Lines,Type=Type,Attrs=Attrs,Values=Values,ReturnLines=F,ReturnList=T)
  BlockL <- APSIMNG_Extract_Block_from_JSON(Input=Lines,Type=Type,Attrs=Attrs,Values=Values,ReturnLines=T,ReturnList=T)
  
  for (i in 1:length(Block)) {
    for (Attr in ChangeAttr) {
      Ind <- which(ChangeAttr==Attr)
      Attr2 <- paste0('"',Attr,'"')
      p <- grep(Attr2,Block[[i]])
      Attr2 <- paste0('"',Attr,'": .*')
      if ( IsNum[Ind]) Block[[i]][p] <- gsub(Attr2,paste0('"',Attr,'": ',ToValues[Ind],','),Block[[i]][p])
      if (!IsNum[Ind]) Block[[i]][p] <- gsub(Attr2,paste0('"',Attr,'": "',ToValues[Ind],'",'),Block[[i]][p])
    }
    Lines[BlockL[[i]][1]:BlockL[[i]][2]] <- Block[[i]]
  }
  return(Lines)
}

#==============================================================================================
#==============================================================================================

APSIM_Create_Weather_File <- function(Data,FileOut,DateCol,RadCol,TmaxCol,TminCol,RainCol,VpCol=NULL,StName,StNumber,LatLong) {
  
  Data <- as.data.table(Data)
  if (!is.Date(Data[,get(DateCol)])) Data[, (DateCol):=as.Date(DateCol)]
  if (is.null(RainCol)) {RainCol <- 'xRainCol'; Data[, xRainCol:=0]}
  
  Data[, Year:=year(Data[,get(DateCol)])]
  Data[, Day:=yday(Data[,get(DateCol)])]
  Data[, Month:=month(Data[,get(DateCol)])]
  Data[, Code:='000']
  
  Data[, Tav:=0.5*(get(TmaxCol)+get(TminCol))]
  Temp  <- Data[, .(Tav=mean(Tav,na.rm=T)), keyby='Month']
  Tav   <- round(mean(Temp$Tav),2)
  Amp   <- round(diff(range(Temp$Tav,na.rm=T)),2)
  
  Headers <- '[weather.met.weather]'
  Headers <- c(Headers,paste0('!station number = ',StNumber))
  Headers <- c(Headers,paste0('!station name = ',StName))
  Headers <- c(Headers,sprintf('latitude = %s (DECIMAL DEGREES)',LatLong[1]))
  Headers <- c(Headers,sprintf('longitude = %s (DECIMAL DEGREES)',LatLong[2]))
  Headers <- c(Headers,paste('tav =',Tav,'(oC) ! annual average ambient temperature'))
  Headers <- c(Headers,paste('amp =',Amp,'(oC) ! annual amplitude in mean monthly temperature'))
  Headers <- c(Headers,'!')
  
  Data <- Data[, c('Year','Day',RadCol,TmaxCol,TminCol,RainCol,VpCol,'Code'), with=F]
  Data[, (TmaxCol):=round(get(TmaxCol),1)]
  Data[, (TminCol):=round(get(TminCol),1)]
  Data[, (RainCol):=round(get(RainCol),1)]
  
  if (!is.null(VpCol)) {
    Data[, (VpCol):=round(get(VpCol),1)]
    Headers <- c(Headers,c('year  day radn  maxt   mint  rain  vp      code'))
    Headers <- c(Headers,c(' ()   () (MJ/m^2) (oC) (oC)  (mm)  (hPa)     ()'))
    
  } else {
    Headers <- c(Headers,c('year  day radn  maxt   mint  rain  code'))
    Headers <- c(Headers,c(' ()   () (MJ/m^2) (oC) (oC)  (mm)  ()'))
  }
  
  writeLines(Headers,FileOut)
  write.table(Data,file=FileOut,quote=F,append=T,col.names=F,row.names=F,sep=' ')
  
  return(Data)
}

#==============================================================================================
#==============================================================================================

APSIM_CenterTTOnAnthesis <- function(DailyOut,Levels=NULL,SowYearCol='SowYear',StageCol='stage',AnthStage=6,TTCol='cumTT') {
  
  if (is.character(DailyOut)) DailyOut <- APSIM_Read_Output_File_DT(DailyOut,OnlyFullSeasons=T,ExpHead=F)
  if (is.list(DailyOut) && !is.data.table(DailyOut)) Data <- rbindlist(DailyOut,fill=T) else Data <- data.table(DailyOut)
  Levels <- setdiff(Levels,SowYearCol)
  
  Data <- Data[, TTAnth:=get(TTCol)[get(StageCol)>=AnthStage][1], keyby=c(Levels,SowYearCol)]
  Data[is.na(TTAnth), lapply(.SD,function(x){x<-x*NA}), keyby=c(Levels,SowYearCol), .SDcols=TTCol]
  Data <- Data[, cumTTX:=get(TTCol)-TTAnth, keyby=c(Levels,SowYearCol)]
  return(Data)
}

#==============================================================================================
#==============================================================================================

APSIM_Deactivate_Node <- function(Sim, Type='outputfile', Names) {
  if (is.null(Names)) {
    Sim <- gsub(sprintf('<%s enabled="yes"', Type), sprintf('<%s', Type), Sim)
    Sim <- gsub(sprintf('<%s enabled="no"', Type), sprintf('<%s', Type), Sim)
    Sim <- gsub(sprintf('<%s', Type), sprintf('<%s enabled="no"', Type), Sim)
    
  } else {
    for (Name in Names) {
      Sim <- gsub(sprintf('<%s name="%s" enabled="yes">', Type, Name), sprintf('<%s name="%s">', Type, Name), Sim)
      Sim <- gsub(sprintf('<%s name="%s" enabled="no">', Type, Name), sprintf('<%s name="%s">', Type, Name), Sim)
      Sim <- gsub(sprintf('<%s name="%s"', Type, Name), sprintf('<%s name="%s" enabled="no">', Type, Name), Sim)
    }
  }
  return(Sim)
}


APSIM_Activate_Node <- function(Sim, Type='outputfile', Names) {
  if (is.null(Names)) {
    Sim <- gsub(sprintf('<%s enabled="no">', Type), sprintf('<%s enabled="yes">', Type), Sim)
    
  } else {
    for (Name in Names) {
      Sim <- gsub(sprintf('<%s name="%s" enabled="no">', Type, Name), sprintf('<%s name="%s" enabled="yes">', Type, Name), Sim)
    }
  }
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Duplicate_Manager <- function(Sim, Name='i-RAT') {
  # Copies the first instance of a manager onto others.
  
  line1 <- grep('<manager2 name="i-RAT">', Sim)[1]
  line2 <- grep('</manager2>', Sim)
  line2 <- line2[line2>line1][1]
  line3 <- grep('<manager2 name="i-RAT">', Sim)[2]
  line4 <- grep('</manager2>', Sim)[2]
  line4 <- line4[line4>line3][1]
  
  Sim <- Sim[-seq(line3,line4)]
  Sim <- append(Sim, Sim[line1:line2], after=line3-1)
  return(Sim)
}

#==============================================================================================
#==============================================================================================

APSIM_Check_HPC_Outputs <- function(Path) {
  
  files <- list.files(path=Path, pattern='^Apsim.o', full.names=T)
  failedJobs <- failedFiles <- NULL
  
  for (file in files) {
    out <- readLines(file)
    
    if (any(grepl('\\[Fail\\]', out) & !grepl('Target: Apsim.exe', out))) {
      failedJobs <- c(failedJobs, strsplit(basename(file), split='\\.')[[1]][3])
      failedFiles <- c(failedFiles, basename(file))
    }
  }
  
  failedJobs <- sort(as.numeric(failedJobs))
  failedFiles <- sort(as.numeric(failedFiles))
  return(list(failedJobs=failedJobs, failedFiles=failedFiles, nFiles=length(files)))
}

#==============================================================================================
#==============================================================================================

APSIM_CalcMaxRootDepth <- function(soil) {
  L1 <- grep('SoilCrop name=\\"sugar\\"',soil,value=F)
  L2 <- grep('</SoilCrop>',soil,value=F)
  L2 <- L2[L2>L1][1]
  soilCrop <- soil[L1:L2]
  
  L1 <- grep('<Thickness>',soilCrop,value=F)+1
  L2 <- grep('</Thickness>',soilCrop,value=F)-1
  Thicks <- soilCrop[L1:L2]
  
  L1 <- grep('<XF>',soilCrop,value=F)+1
  L2 <- grep('</XF>',soilCrop,value=F)-1
  XFs <- soilCrop[L1:L2]
  
  Thicks <- cumsum(as.numeric(trimws(gsub('<double>|</double>', '', Thicks))))
  XFs <- as.numeric(trimws(gsub('<double>|</double>', '', XFs)))
  
  return(ifelse(any(XFs==0), Thicks[which(XFs==0)[1]-1], last(Thicks)))
}

#==============================================================================================
#==============================================================================================



