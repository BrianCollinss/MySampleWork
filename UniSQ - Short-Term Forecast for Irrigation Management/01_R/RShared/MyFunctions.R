
options(stringsAsFactors=FALSE)

p_load(char=c('base','broom','caret','cluster','dplyr','forecast','graphics','grDevices','Hmisc',    
              'lubridate','Matrix','multcompView','pdist','plyr','rjson','Rmisc','rsample',
              'sp','ssh','stats','stringr','tools','utils','XML'), install=F, repos=getOption('repos'))

#=================================================
#=================================================
#=================================================

NumericCols <- function(data) {
  return(which(sapply(data,is.numeric)))
}

#=================================================
#=================================================
#=================================================

SetupPBS <- function(HostUni='JCU',Project,RFile,NJobs=10,JobArray=T,NCPU=10,NCPUSing=NULL,Memor=50,WallTime='02:00:00',TempFold='$TMPDIR',Email=NULL,
                     ProjectGroup,MainDir='C:/Users/jc837076/OneDrive - James Cook University/',UploadR=T,UploadSharedData=T,DependOn=NULL,
                     USQServer='fawkes.usq.edu.au',JCUServer='zodiac.hpc.jcu.edu.au',BunyaServer='bunya.rcc.uq.edu.au',UQServer='tinaroo.rcc.uq.edu.au') {
  
  if (NJobs==1) JobArray <- F
  if (is.null(NCPUSing)) NCPUSing <- NCPU - 2
  
  JName <- 'BrianCollins'
  File  <- 'Run.pbs'
  
  if (HostUni=='USQ') {
    Server     <- USQServer
    RemMainDir <- '/home/u8019357/APSIM'
    UserName   <- Sys.getenv("USQ_USERNAME")
    PassW      <- Sys.getenv("USQ_PASSWORD")
    if (is.null(Email)) Email <- 'brian.collins@unisq.edu.au'
    
  } else if (HostUni=='JCU') {
    Server     <- JCUServer
    RemMainDir <- '/home/jc837076/APSIM'
    UserName   <- Sys.getenv("JCU_USERNAME")
    PassW      <- Sys.getenv("JCU_PASSWORD")
    if (is.null(Email)) Email <- 'brian.collins@jcu.edu.au'
    
  } else if (HostUni=='UQ') {
    Server     <- UQServer
    RemMainDir <- '/scratch/user/uqbababa/APSIM'
    UserName   <- Sys.getenv("UQ_USERNAME")
    PassW      <- Sys.getenv("UQ_PASSWORD")
    if (is.null(Email)) Email <- 'brian.collins4@uq.edu.au'
  }
  
  # Sources and Folders
  Dir     <- APSIM_Paths2(Dir=list(),Project=Project,Run=NULL,MainDir=MainDir)
  FileLoc <- paste(MainDir,'00_Projects/SharedScripts',File,sep='/')
  shFile  <- File
  sscon   <- ssh_connect(host=paste(UserName,Server,sep='@'),passwd=PassW,verbose=F)
  
  RemDir <- APSIM_Paths2(Dir=list(),Project=Project,Run=NULL,MainDir=RemMainDir)
  
  CommFold  <- format(Sys.time(),format=paste0('Job-','%Y%m%d-%H%M%S---',basename(RemDir$Project)))
  CommFold  <- paste(RemDir$JobSubmit,CommFold,sep='/')
  
  x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                     command=c(paste('mkdir',shQuote(RemMainDir)),paste('mkdir',shQuote(RemDir$AllProjects)),
                               paste('mkdir',shQuote(RemDir$WeatherAll)),paste('mkdir',shQuote(RemDir$Weather)),
                               paste('mkdir',shQuote(RemDir$RShared)),paste('mkdir',shQuote(RemDir$DataShared)),
                               paste('mkdir',shQuote(RemDir$JobSubmit)),paste('mkdir',shQuote(CommFold))))
  
  if (!is.null(Project)) {
    message('Creating project folders ...')
    for (Fold in RemDir) x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,command=c(paste('mkdir',shQuote(Fold))))
  }
  
  if (UploadR) {
    message('Uploading project R scripts ...')
    Temp <- List_Files_by_Ext(Dir=Dir$R,Ext='r',AlTogether=F)
    x <- scp_upload(session=sscon,files=Temp,to=RemDir$R,verbose=F)
    
    message('Uploading shared R scripts ...')
    Temp <- List_Files_by_Ext(Dir=Dir$RShared,Ext='r',AlTogether=F)
    x <- scp_upload(session=sscon,files=Temp,to=RemDir$RShared,verbose=F)
  }
  
  if (UploadSharedData) {
    message('Uploading files from shared data folder ...')
    Temp <- List_Files_by_Pattern(Dir=Dir$DataShared,Pattern='[:alpha:]',AlTogether=F)
    x <- scp_upload(session=sscon,files=Temp,to=RemDir$DataShared,verbose=F)
  }
  
  
  Comm <- '#!/bin/bash'
  Comm <- append(Comm,'')
  Comm <- append(Comm,'############################################')
  Comm <- append(Comm,'')
  
  if (NJobs>1 && JobArray==T) {
    Comm <- append(Comm,sprintf('cat <<EOF | /opt/pbs/bin/qsub -J 1-%s',NJobs))
    JobID <- '\\$PBS_ARRAY_INDEX'
    
  } else {
    Comm <- append(Comm,sprintf('cat <<EOF | /opt/pbs/bin/qsub'))
    JobID <- '1'
  }
  
  Comm <- append(Comm,'')
  Comm <- append(Comm,'############################################')
  Comm <- append(Comm,'')
  
  Comm <- append(Comm,sprintf('#PBS -S /bin/bash'))#PBS 
  Comm <- append(Comm,sprintf('#PBS -N %s', JName))
  Comm <- append(Comm,sprintf('#PBS -l select=1:ncpus=%s:mem=%sGB', NCPU, Memor))
  Comm <- append(Comm,sprintf('#PBS -l walltime=%s', WallTime))
  Comm <- append(Comm,sprintf('#PBS -m %s','e'))
  Comm <- append(Comm,sprintf('#PBS -M %s', Email))
  Comm <- append(Comm,sprintf('#PBS -q default'))
  if (!is.null(ProjectGroup)) Comm <- append(Comm,sprintf('#PBS -P %s', ProjectGroup))
  if (!is.null(DependOn)) Comm <- append(Comm, sprintf('#PBS -W depend=afterok:%s', DependOn))
  if (HostUni=='UQ') Comm <- append(Comm, sprintf('#PBS -A UQ-QAAFI'))
  
  Comm <- append(Comm,'')
  Comm <- append(Comm,'############################################')
  Comm <- append(Comm,'')
  
  JobRandID <- as.character(as.integer(runif(1, 1e8, 1e9)))
  
  if (HostUni=='USQ' || HostUni=='JCU') {
    
    if (HostUni=='USQ') {
      Comm <- append(Comm,'############################################')
      Comm <- append(Comm,sprintf('TMPDIR=/sandisk1/u8019357/Job_%s_\\$PBS_ARRAY_INDEX', JobRandID))
      Comm <- append(Comm,sprintf('mkdir -p \\%s', TempFold))
      Comm <- append(Comm,sprintf('cd \\%s', TempFold))
      Comm <- append(Comm,'############################################')
      
      Comm <- append(Comm,sprintf('export NUMBER_OF_PROCESSORS=%s', NCPU))
      Comm <- append(Comm,sprintf('export SINGULARITYENV_NUMBER_OF_PROCESSORS=%s', NCPUSing))
      Comm <- append(Comm,'export SINGULARITY_TMPDIR=$TMPDIR')
      Comm <- append(Comm,'export SINGULARITY_LOCALCACHEDIR=$TMPDIR')
      Comm <- append(Comm,'export SINGULARITY_CACHEDIR=$TMPDIR')
      Comm <- append(Comm,'export SINGULARITYENV_R_LIBS_USER=$HOME/R')
      Comm <- append(Comm,sprintf('module load singularity'))
      Comm <- append(Comm,sprintf('module load gdal'))
      Comm <- append(Comm,sprintf('module load udunits/2.2.28-gcc-t3a'))
      Comm <- append(Comm,sprintf('module load r/4.3.0-gcc-py3-jyj'))
      
    } else if (HostUni=='JCU') {
      Comm <- append(Comm,sprintf('cd \\%s', TempFold))
      Comm <- append(Comm,sprintf('export NUMBER_OF_PROCESSORS=%s', NCPU))
      Comm <- append(Comm,sprintf('export SINGULARITYENV_NUMBER_OF_PROCESSORS=%s', NCPUSing))
      Comm <- append(Comm,sprintf('module load singularity'))
      Comm <- append(Comm,sprintf('module load gdal'))
      Comm <- append(Comm,sprintf('module load conda3'))
      Comm <- append(Comm,sprintf('source \\$CONDA_PROF/conda.sh'))
      Comm <- append(Comm,sprintf('conda activate R-4.0.3'))
    }
    
    if (NJobs>1 && JobArray==T) {
      Comm <- append(Comm,sprintf('Rscript %s %s %s \\%s', shQuote(paste0('\\$PBS_O_WORKDIR/',basename(RFile))),NJobs,JobID,TempFold))
    } else {
      for (JobID in 1:NJobs) {
        Comm <- append(Comm,sprintf('Rscript %s %s %s \\%s &', shQuote(paste0('\\$PBS_O_WORKDIR/',basename(RFile))),NJobs,JobID,TempFold))
      }
      Comm <- append(Comm,'wait')
    }
    
  } else if (HostUni=='UQ') {
    Comm <- append(Comm,sprintf('cd \\%s',TempFold)) #PBS_O_WORKDIR
    Comm <- append(Comm,sprintf('export SINGULARITYENV_NUMBER_OF_PROCESSORS=%s',NCPU-1))
    Comm <- append(Comm,sprintf('export LD_LIBRARY_PATH=/home/%s/RExtra/local/lib',UserName))
    Comm <- append(Comm,'export INCLUDE_PATH=/usr/include/sys/stat.h')
    Comm <- append(Comm,sprintf('module load singularity'))
    Comm <- append(Comm,sprintf('module load proj'))
    Comm <- append(Comm,sprintf('module load gdal'))
    Comm <- append(Comm,sprintf('module load geos'))
    Comm <- append(Comm,sprintf('module load R/3.5.0'))
    
    if (NJobs>1 && JobArray==T) {
      Comm <- append(Comm,sprintf('Rscript %s %s %s \\%s', shQuote(paste0('\\$PBS_O_WORKDIR/',basename(RFile))),NJobs,JobID,TempFold))
    } else {
      for (JobID in 1:NJobs) {
        Comm <- append(Comm,sprintf('Rscript %s %s %s \\%s', shQuote(paste0('\\$PBS_O_WORKDIR/',basename(RFile),' &')),NJobs,JobID,TempFold))
      }
      Comm <- append(Comm,'wait')
    }
  }
  
  Comm <- append(Comm,'')
  Comm <- append(Comm,'############################################')
  Comm <- append(Comm,'rm -r \\$TMPDIR')
  Comm <- append(Comm,'EOF')
  
  writeLines(Comm, FileLoc)
  
  message('Uploading R and PBS scripts ...')
  
  x <- scp_upload(session=sscon,files=FileLoc,to=CommFold,verbose=F)
  x <- scp_upload(session=sscon,files=RFile,to=CommFold,verbose=F)
  x <- file.remove(FileLoc)
  
  message('Running R script ...')
  
  RunComm <- sprintf('sh %s', shFile)
  
  x <- ssh_exec_wait(session=sscon,std_out=NULL,std_err=NULL,
                     command=c(paste('cd', CommFold),
                               paste('dos2unix', shFile),
                               paste('chmod -x', shFile),
                               RunComm))
  
  {
    LineX()
    message('Closing the connection ...')
    x <- ssh_disconnect(sscon)
    LineX()
  }
  
  return(x)
}

#=================================================
#=================================================
#=================================================

GenCorrelTable <- function(data,cols=NULL,useAst=F,npdig=4) {
  if (is.null(cols)) cols <- colnames(data)
  out1 <- out2 <- data.frame(matrix(NA_real_,nrow=length(cols),ncol=length(cols)))
  rownames(out1) <- colnames(out1) <- rownames(out2) <- colnames(out2) <- cols
  
  for (c1 in 1:length(cols)) {
    for (c2 in 1:length(cols)) {
      x <- unlist(cor.test(data[,cols[c1]],data[,cols[c2]]))
      out1[c1,c2] <- round(as.numeric(x['estimate.cor']),3)
      
      pval <- as.numeric(x['p.value'])
      if (useAst==T) {
        if (pval<0.001) pval <- "***" else if (pval<0.01) pval <- "**" else if (pval<0.05) pval <- "*" else pval <- "ns"
      } else {
        pval <- round(pval,npdig)
      }
      out2[c1,c2] <- pval
    }
  }
  
  out <- data.frame(matrix(NA_real_,nrow=length(cols),ncol=2*length(cols)))
  rownames(out) <- cols
  colnames(out) <- c(cols,cols)
  colnames(out)[seq(2,ncol(out),2)] <- ifelse(useAst==T,'','P')
  
  for (c1 in 1:length(cols)) {
    out[,(c1-1)*2+1] <- out1[,c1]
    out[,(c1-1)*2+2] <- out2[,c1]
  }
  return(out)
}

#=================================================
#=================================================
#=================================================

MyPc <- function() {
  return('QAAFI-JPNCBH2')
}

#=================================================
#=================================================
#=================================================

List_Files_by_Ext <- function(Dir,Ext,AlTogether=F) {
  
  Pks <- c('tools')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  }  
  
  files1 <- list.files(path=Dir,full.names=T,recursive=AlTogether)
  files2 <- vector(mode="character")
  
  if (length(files1)>0) {
    for (file in files1) {
      ext <- tolower(file_ext(file))
      if (!is.null(ext) && length(ext)>0 && ext == tolower(Ext)) {
        files2 <- c(files2,file)
      }
    }
  }
  return(files2)
}

#=================================================
#=================================================
#=================================================

List_Files_by_Pattern <- function(Dir,Pattern,AlTogether=F) {
  
  Pks <- c('tools')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  
  files1 <- list.files(Dir,full.names=T,recursive=AlTogether);
  files2 <- vector(mode="character");
  
  if (length(Pattern)>1) Pattern <- paste(Pattern,collapse="|")
  
  if (length(files1)>0) {
    for (file in files1) {
      name <- basename(file);
      if (!is.null(name) && length(grep(Pattern,name))>0) {
        if (grep(Pattern,name)>0) files2 <- c(files2,file)
      }
    }
  }
  return(files2)
}

#=================================================
#=================================================
#=================================================

List_Files_by_Ext_and_Pattern <- function(Dir,Ext,Pattern,AlTogether=F) {
  
  Pks <- c('tools')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  
  files1 <- list.files(path=Dir,full.names=T,recursive=AlTogether);
  files2 <- vector(mode="character");
  
  if (length(Pattern)>1) Pattern <- paste(Pattern,collapse="|")
  
  if (length(files1)>0) {
    for (file in files1) {
      name <- basename(file);
      ext  <- tolower(file_ext(file));
      if (!is.null(ext) && !is.null(name) && length(ext)>0 && ext==tolower(Ext) && length(grep(Pattern,name))>0) {
        if (grep(Pattern,name)==1) files2 <- c(files2,file)
      }
    }
  }
  return(files2)
}

#=================================================
#=================================================
#=================================================

File_Base_Name <- function(FPath) {
  
  Pks <- c('tools')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  return(file_path_sans_ext(basename(FPath)))
}

#=================================================
#=================================================
#=================================================

Capitalize_First_Letters <- function(Text,ignorePar=F,ignoreTexts=NULL,toLower=F) {
  
  if (length(Text)>1) {
    Text2 <- sapply(Text,Capitalize_First_Letters,ignorePar=ignorePar,ignoreTexts=ignoreTexts,simplify=T)
    names(Text2) <- NULL
    return(Text2)
  }
  
  FUNC <- function(x) {
    if (is.null(ignoreTexts) || (!is.null(ignoreTexts) && length(grep(x,ignoreTexts))==0)) {
      if (toLower) x <- tolower(x)
      if (substring(x,1,1)=="(") {
        if (ignorePar) {
          x <- paste0("(",toupper(substring(x,2,2)),substring(x,3))
        } else {
          x <- paste0(toupper(substring(x,1,1)),substring(x,2))
        }
      } else {
        x <- paste0(toupper(substring(x,1,1)),substring(x,2))
      }
    }
    return(x)
  }
  
  Text <- unlist(strsplit(Text," "))
  Text <- paste(sapply(Text,FUNC,simplify=T),collapse=" ")
  return(Text)
}

#=================================================
#=================================================
#=================================================

Replace_in_Strings <- function(Texts,Patterns,Replaces) {
  if (length(Replaces)==1) Replaces <- rep(Replaces,length(Patterns))
  
  for (Pat in Patterns) {
    Rep <- Replaces[Patterns==Pat]
    Texts <- sapply(Texts,function(x){x <- gsub(Pat,Rep,x)},simplify=T)
    names(Texts) <- NULL
  }
  return(Texts)
}

#=================================================
#=================================================
#=================================================

Find_and_Remove_in_Names <- function(Dir,Dir2=NULL,Pattern,AlTogether=F) {
  Files <- List_Files_by_Pattern(Dir,Pattern,AlTogether);
  
  for (file in Files) {
    name  <- basename(file);
    name2 <- sub(Pattern,"",name);
    if (is.null(Dir2)) {
      file2 <- paste(Dir,name2,sep="/");
      file.rename(file,file2);
    }
    else {
      file2 <- paste(Dir2,name2,sep="/");
      file.copy(file,file2);
    }
  }
}

#=================================================
#=================================================
#=================================================

CorrTest <- function(mat,conf.level=0.95) {
  mat <- as.matrix(mat)
  n   <- ncol(mat)
  cmat <- pmat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(pmat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1;
  
  if (nrow(mat)<3) mat <- rbind(mat,mat);
  if (nrow(mat)<3) mat <- rbind(mat,mat);
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      
      tmp  <- cor.test(mat[,i],mat[,j],conf.level=conf.level,use='pairwise.complete.obs');
      
      cmat[i,j] <- cmat[j,i] <- cor(mat[,i],mat[,j],use='pairwise.complete.obs');
      pmat[i,j] <- pmat[j,i] <- ifelse(!is.na(tmp$p.value) & !is.nan(tmp$p.value),tmp$p.value,1);
      
      colnames(cmat) <- rownames(cmat) <- colnames(mat);
      colnames(pmat) <- rownames(pmat) <- colnames(mat);
      
      if (!is.null(tmp$conf.int)) {
        lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1];
        uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2];
      } else {
        lowCI.mat[i,j] <- lowCI.mat[j,i] <- pmat[i,j];
        uppCI.mat[i,j] <- uppCI.mat[j,i] <- pmat[i,j];
      }
    }
  }
  
  cmat[is.na(cmat)] <- 0;
  
  qmat <- QValues(pmat);  
  return(list(Cor=cmat,P=pmat,Q=qmat,Low=lowCI.mat,Up=uppCI.mat))
}

#=================================================
#=================================================
#=================================================

QValues <- function(PVals,l=0.05) {
  
  Pks <- c('LBE')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  
  # Pierre's code:
  # a <- LBEa(m=length(vecP),l=0.05,fig=F); # Parameter of the function LBE() to calculate FDR
  # q <- LBE(pval=vecP,a=a);
  
  vecP  <- as.vector(PVals);
  a     <- LBEa(m=length(vecP),l=l,fig=F) # Parameter of the function LBE() to calculate FDR
  LBEx  <- LBE(vecP,a=a,plot.type='none');
  qmat  <- matrix(LBEx$qvalues,ncol=ncol(PVals));
  
  return(qmat)
}


#=================================================
#=================================================
#=================================================

DeTrend <- function(Table,Method=0,NewMean=NULL,NumYrs=NULL,P=0.10,BaseInd=1,IgnoreInds=NULL) {
  # Method=0 : Just detrend
  # Method=1 : Adjust mean to the NewMean or colMean(Table)
  # Method=2 : Adjust mean to the first predictipn
  
  if (is.vector(Table)) Table <- matrix(Table,ncol=1)
  Table <- as.data.frame(Table)
  nCol <- ncol(Table)
  VarNames <- NULL
  if (!is.null(colnames(Table))) VarNames <- colnames(Table)
  
  if (is.null(NumYrs) && is.null(NewMean)) NumYrs <- nrow(Table)
  if (Method==0 || Method==2) NewMean <- rep(0,ncol(Table))
  if (Method==1 && is.null(NewMean)) NewMean <- colMeans(Table[1:NumYrs,],na.rm=T)
  if (is.null(P)) P <- Inf
  
  TableDetr <- TablePred <- Table
  Reg <- Stats <- data.table()
  
  for (i in 1:nCol) {
    if (length(na.omit(Table[,i]))>1) {
      
      TableDetr <- cbind(TableDetr,TRUE)
      names(TableDetr)[ncol(TableDetr)] <- paste(names(Table)[i],'Detrend',sep='_')
      TablePred <- cbind(TablePred,TRUE)
      names(TablePred)[ncol(TablePred)] <- paste(names(Table)[i],'Detrend',sep='_')
      
      Data1 <- Data2 <- data.frame(X=1:nrow(Table),Y=Table[,i])
      if (!is.null(IgnoreInds)) Data1 <- Data1[-IgnoreInds,]
      
      LM <- lm(Y~X,data=Data1,singular.ok=T)
      Predicts <- predict(LM,Data2)
      
      Temp <- data.table(glance(LM))
      if (!is.null(VarNames)) Temp <- cbind(variable=VarNames[i],Temp)
      Stats <- rbind(Stats,Temp)
      
      Temp <- data.table(tidy(LM))
      if (!is.null(VarNames)) Temp <- cbind(variable=VarNames[i],Temp)
      Reg <- rbind(Reg,Temp)
      
      TablePred[,i] <- Predicts
      
      if (Method==0) NewMean[i] <- 0
      if (Method==2) NewMean[i] <- Predicts[BaseInd]
      
      if (tidy(LM)[2,'p.value'] < P) {
        TableDetr[,i] <- Table[,i] - Predicts
        
      } else {
        TableDetr[,paste(names(Table)[i],'Detrend',sep='_')] <- FALSE
        TablePred[,paste(names(Table)[i],'Detrend',sep='_')] <- FALSE
      }
      
      if (Method!=0) TableDetr[,i] <- TableDetr[,i] - mean(TableDetr[,i],na.rm=T) + NewMean[i]
    }
  }
  return(list(Detrended=data.table(TableDetr),Predicts=data.table(TablePred),Reg=data.table(Reg),Stats=data.table(Stats)))
}

#=================================================
#=================================================
#=================================================

Change_All_Extensions <- function(Dir,Dir2=NULL,Ext1,Ext2,AlTogether=F) {
  
  Pks <- c('tools')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  
  Files <- List_Files_by_Ext(Dir,Ext1,AlTogether);
  
  for (file in Files) {
    name  <- basename(file);
    name2 <- sub(file_ext(name),Ext2,name);
    if (is.null(Dir2)) {
      file2 <- paste(Dir,name2,sep="/");
      file.rename(file,file2);
    }
    else {
      file2 <- paste(Dir2,name2,sep="/");
      file.copy(file,file2);
    }
  }
}

#=================================================
#=================================================
#=================================================

DOY2DateComps <- function(Table, Add=F) {
  
  Pks <- c('data.table')
  for (Pk in Pks) {
    if (!require(Pk,character.only=T)) install.packages(Pk,dependencies=T,character.only=T)
    library(Pk,character.only=T)
  } 
  
  # Converts 'Day Number" to 'Date'
  Table  <- data.table(Table)
  dates  <- as.POSIXlt(paste(Table[[1]],Table[[2]],sep='-'),tz="",format="%Y-%j")
  months <- dates$mon + 1
  mdays  <- dates$mday
  DOYs   <- dates$yday + 1
  
  Dates <- data.table(cbind(Year=Table[[1]],Mon=months,Day=mdays,DOY=DOYs))
  if (Add==TRUE) Dates <- cbind(Dates, Table)
  return(Dates)
}

#=================================================
#=================================================
#=================================================

DateComps2Date <- function(Table, Add=F) {
  if (is.vector(Table)) Table <- matrix(Table,nrow=1)
  Table <- as.data.table(Table)
  Dates <- as.Date(paste(Table[[1]],Table[[2]],Table[[3]],sep="-"),format="%Y-%m-%d")
  if (Add==T) {
    Table[, Date:=Dates]
    return(Table)
  }
  return(Dates)
}

#=================================================
#=================================================
#=================================================

DateComps2DOY <- function(Table) {
  # Converts 'Date Components' to 'Day Number'
  if (is.vector(Table)) Table <- matrix(Table,nrow=1)
  Dates <- DateComps2Date(Table)
  Dates <- cbind(Year=Table[,1],DOY=Date2DOY(Dates))
  return(Dates)
}

#=================================================
#=================================================
#=================================================

Date2DOY <- function(Dates,Year=NULL,Origin='1970-01-01') {
  if (is.data.frame(Dates) || is.matrix(Dates) || is.data.table(Dates)) {
    DOYs <- apply(Dates,2,Date2DOY,Origin=Origin,Year=Year)
    return(DOYs)
  }
  if (!is.Date(Dates)) Dates <- as.Date(Dates,origin=Origin)
  if (is.null(Year)) DOYs <- as.numeric(format(Dates,format='%j')) else
    DOYs <- as.numeric(Dates - as.Date(paste0(Year,'-01-01'),format='%Y-%m-%d') + 1)
  return(DOYs)
}

#=================================================
#=================================================
#=================================================

DateComponents <- function(Dates) {
  Years  <- as.numeric(format(Dates,format='%Y'))
  Months <- as.numeric(format(Dates,format='%m'))
  Days   <- as.numeric(format(Dates,format='%d'))
  return(data.frame(Year=Years,Mon=Months,Day=Days))
}

MyYear <- function(Dates) {
  Years <- as.numeric(format(Dates,format='%Y'))
  return(Years)
}

MyMonth <- function(Dates) {
  Months <- as.numeric(format(Dates,format='%m'))
  return(Months)
}

MyDay <- function(Dates) {
  Days <- as.numeric(format(Dates,format='%d'))
  return(Days)
}

#=================================================
#=================================================
#=================================================

DOY2Julian <- function(Years,DOYs,Origin="1970-01-01") {
  Dates <- DOY2Date(Years,DOYs);
  Julians <- julian(Dates,origin=as.Date(Origin));
  return(Julians)
}

#=================================================
#=================================================
#=================================================

DOY2Date <- function(DOYs,Years,ToDate=TRUE) {
  if (is.data.frame(DOYs) || is.matrix(DOYs)) {
    DOYs  <- as.data.frame(DOYs)
    Dates <- matrix(NA,ncol=ncol(DOYs),nrow=nrow(DOYs))
    Dates <- apply(DOYs,2,DOY2Date,Years=Years,ToDate=ToDate)
    return(Dates)
  }
  if (ToDate) Dates <- as.Date(paste0(Years,'-01-01'),format="%Y-%m-%d")+DOYs-1
  else Dates <- as.POSIXlt(paste0(Years,"-",DOYs),tz="",format="%Y-%j")
  return(Dates)
}

#=================================================
#=================================================
#=================================================

Date_2_DateClass <- function(Table) {
  # Converts 'Date Table' to 'Date Class'
  Dates <- as.Date(paste(Table[,1],Table[,2],Table[,3],sep="-"),format="%Y-%m-%d")
  return(Dates)
}

#=================================================
#=================================================
#=================================================

LineX <- function(Length=100,Lines=1) {
  for (i in 1:Lines) message(rep("-",Length))
}

#=================================================
#=================================================
#=================================================

MakeUnitSum <- function(X) {
  X <- X/sum(X,na.rm=T);
  return(X)
}

#=================================================
#=================================================
#=================================================

Convert_Dates <- function(Data) {
  options(warn=-1);
  
  if (is.vector(Data)) {
    if (is.character(Data[1])) {
      x <- Data[1];
      x <- unlist(strsplit(x, "-")); 
      if (length(x)==3) {
        Data <- as.Date(Data,tz = "",format="%Y-%m-%d");
      }
    }
  }
  else {
    for (i in 1:ncol(Data)) {
      Data[,i] <- Convert_Dates(Data[,i]);
    }
  }
  return(Data);
}

#=================================================
#=================================================
#=================================================

ParseExp <- function(x) {
  parse(text=x)[[1]]
}


#=================================================
#=================================================
#=================================================

Convert_to_Numeric <- function(Data) {
  
  if (is.vector(Data) || class(Data)=="Date") {
    if (!is.na(as.numeric(Data[1])) & class(Data)!="Date") Data <- as.numeric(Data);
  } 
  else {
    for (i in 1:ncol(Data)) {
      Data[,i] <- Convert_to_Numeric(Data[,i]);
    }
  }
  return(Data);
}

#=================================================
#=================================================
#=================================================

LinRegr <- function(y,x) {
  data <- cbind.data.frame(Y=y,x);
  data <- Factor2Numeric(data);
  lm   <- lm(Y~.,data=data);
  r2   <- round(summary(lm)$r.squared,digits=2)
  lm   <- as.data.frame(summary(lm)$coefficients,singular.ok=T);
  colnames(lm) <- c('Value','Std','t','P');
  lm$R2 <- r2
  
  return(lm);
}

#=================================================
#=================================================
#=================================================

LinReg_Factors <- function(Data,yCol,xCols,FactCols,AddToCol=NULL) {
  Data <- as.data.frame(Data)
  
  if (!is.null(AddToCol)) {
    FUNC <- function(x) {
      RegOut <- LinRegr(x[,yCol],x[,xCols])
      a <- round(RegOut[1,1],2)
      b <- round(RegOut[2,1],2)
      r2val <- RegOut$R2[1]
      pval <- round(RegOut[2,4],3)
      
      print(as.character(bquote('Y='*.(b)*'X+'*.(a)*'  R'^{2}*'='*.(r2val))))
      x$RegEq <- as.character(bquote('Y='*.(b)*'X+'*.(a)*'  R'^{2}*'='*.(r2val)))
      return(x)
    }
    RegOut <- by(Data,Data[,FactCols],FUNC)
    RegOut <- by_to_DataFrame(RegOut)
    return(RegOut)
  } else {
    FUNC <- function(x) {return(LinRegr(x[,yCol],x[,xCols]))}
    RegOut <- by(Data,Data[,FactCols],FUNC)
    RegOut <- by_to_DataFrame(RegOut)
    return(RegOut)
  }
}

#=================================================
#=================================================
#=================================================

MyCompare <- function(Stats='RMSRE',Obs,Sims,Weights=NULL,Digits=2,Probs=c(0.1,0.9),CompleteCases=F,Q=4) {
  
  if (length(Stats)==1 && Stats==F) Stats <- NULL
  if (is.null(Stats)) Stats <- c('N','Mean','SD','R','R2','R2Sp','RMSE','NRMSE','NRMSEQ','RMSRE',
                                 'MAE','NMAE','MRE','MARE','ME','NSE','P_Factor')
  
  if (length(Stats)>1) {
    value <- NULL;
    for (i in 1:length(Stats)) {
      temp  <- MyCompare(Stats[i],Obs,Sims,Weights,Digits);
      value <- cbind(value,temp);
      if (!Stats[i]%in%c('Mean','SD')) colnames(value)[ncol(value)] <- Stats[i];
    }
    return(value);
  }
  
  if (!is.null(dim(Obs))) {
    if (ncol(Obs)==ncol(Sims)) {
      value <- NULL;
      for (i in 1:ncol(Obs)) {
        value[i] <- MyCompare(Stats,Obs[,i],Sims[,i],Weights,Digits);
        names(value)[ncol(value)] <- Stats[i];
      }
      return(value)
    }
  }
  
  if (is.null(Weights))   Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  if (CompleteCases) {
    rows <- complete.cases(cbind(Obs,Sims,Weights));
    Obs  <- Obs[rows]
    Sims <- Sims[rows]
    Weights <- Weights[rows];
  }
  
  Weights[is.na(Weights)] <- 0;
  WeightsN <- Weights/sum(Weights,na.rm=T);
  
  if (length(Obs)>0) {
    if (Stats=='N') {
      value <- round(sum(!is.na(Obs) & !is.na(Sims)),0)
    }
    if (Stats=='Mean') {
      value <- cbind(Mean1=mean(Obs,na.rm=T),Mean2=mean(Sims,na.rm=T))
    }
    if (Stats=='SD') {
      value <- cbind(SD1=sd(Obs,na.rm=T),SD2=sd(Sims,na.rm=T))
    }
    if (Stats=='RMSRE') {
      re <- (Obs-Sims)/Obs*100;
      value <- sqrt(mean(WeightsN*(re^2),na.rm=T));
    }
    if (Stats=='RMSE') {
      value <- RMSE(Obs,Sims,Weights=Weights,Digits=Digits,CompleteCases=CompleteCases)
    }
    if (Stats %in% c('RRMSE','SRMSE','NRMSE')) {
      value <- RRMSE(Obs,Sims,Weights=Weights,Digits=Digits,CompleteCases=CompleteCases)
    }
    if (Stats=='NRMSEQ') {
      value <- NULL
      for (i in 1:Q) {
        Ind <- which(Obs %between% quantile(Obs,c(i-1,i)*(1/Q),na.rm=T))
        value <- c(value, RRMSE(Obs[Ind],Sims[Ind],Weights=Weights,Digits=Digits,CompleteCases=CompleteCases))
      }
      value <- mean(value,na.rm=T)
    }
    if (Stats=='MAE') {
      value <- sum(WeightsN*abs(Sims-Obs),na.rm=T)
    }
    if (Stats=='NMAE') {
      value <- round(100*sum(WeightsN*abs(Sims-Obs),na.rm=T)/abs(mean(Obs,na.rm=T)),2)
    }
    if (Stats=='MRE') {
      WeightsN <- WeightsN[Obs!=0]
      Sims2 <- Sims[Obs!=0]
      Obs2  <- Obs[Obs!=0]
      value <- sum(WeightsN*(Sims2-Obs2)/Obs2*100,na.rm=T)
    }
    if (Stats=='MARE') {
      WeightsN <- WeightsN[Obs!=0]
      Sims2 <- Sims[Obs!=0]
      Obs2  <- Obs[Obs!=0]
      value <- sum(WeightsN*abs(Sims2-Obs2)/abs(Obs2)*100,na.rm=T)
    }
    if (Stats=='ME') {
      value <- sum(WeightsN*(Sims-Obs),na.rm=T)
    }
    if (Stats=='NSE' || Stats=='NASH' || Stats=='EF') {
      E1 <- WeightsN*((Obs-Sims)^2)
      E2 <- WeightsN*((Obs-mean(Obs,na.rm=T))^2)
      value <- 1-(sum(E1,na.rm=T)/sum(E2,na.rm=T))
    }
    if (Stats=='R') {
      value <- cor(Obs,Sims,use='pairwise.complete.obs')
    }
    if (Stats=='R2') {
      value <- cor(Obs,Sims,use='pairwise.complete.obs')^2
    }
    if (Stats=='R2Sp') {
      value <- cor(Obs,Sims,method='spearman')^2
    }
    if (Stats=='P_Factor') {
      omean <- mean(Obs,na.rm=T)
      q1sim <- Quantile(Sims,Probs=Probs[1],FullRows=F)
      q2sim <- Quantile(Sims,Probs=Probs[2],FullRows=F)
      value <- as.numeric(q1sim <= omean & q2sim >= omean)
    }
  }
  return(round(value,Digits))
}

#=================================================
#=================================================
#=================================================

Zero2One <- function(x) {
  
  if (class(x)=='numeric') y <- (x-min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))
  else y <- apply(x,2,Zero2One);
  
  return(y);
}

#=================================================
#=================================================
#=================================================

ChangeMinMax <- function(Data,NewMin=0,NewMax=1,IsSymmetrical=F) {
  if (class(Data)=='numeric' || class(Data)=='integer') {
    if (IsSymmetrical) {
      Data2 <- Data
      NewMin <- 0
      NewMax <- abs(NewMax)
      IDP <- which(Data>0)
      xMin <- min(as.numeric(Data[IDP]),na.rm=T)
      xMax <- max(as.numeric(Data[IDP]),na.rm=T)
      Data2[IDP] <- (NewMax-NewMin)*(as.numeric(Data[IDP])-xMin)/(xMax-xMin)+NewMin
      
      NewMin <- -NewMax
      NewMax <- 0
      IDN <- which(Data<0)
      xMin <- min(as.numeric(Data[IDN]),na.rm=T)
      xMax <- max(as.numeric(Data[IDN]),na.rm=T)
      Data2[IDN] <- (NewMax-NewMin)*(as.numeric(Data[IDN])-xMin)/(xMax-xMin)+NewMin
      
    } else {
      xMin <- min(as.numeric(Data),na.rm=T)
      xMax <- max(as.numeric(Data),na.rm=T)
      Data2 <- (NewMax-NewMin)/(xMax-xMin) * (as.numeric(Data)-xMin) + NewMin
    }
  }
  else {
    if (length(NewMin)==1) NewMin <- rep(NewMin,ncol(Data))
    if (length(NewMax)==1) NewMax <- rep(NewMax,ncol(Data))
    if (is.data.table(Data)) {
      Data2 <- data.table(Data)
      for (C in 1:ncol(Data)) Data2[[C]] <- ChangeMinMax(Data2[[C]],NewMin[C],NewMax[C])
    } else {
      Data2 <- Data
      for (C in 1:ncol(Data)) Data2[,C] <- ChangeMinMax(Data2[,C],NewMin[C],NewMax[C])
    }
  }
  return(Data2)
}

#=================================================
#=================================================
#=================================================

RMSE <- function(Obs,Sims,Weights=NULL,Digits=3,CompleteCases=F) {
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- 1:length(Obs)
  if (CompleteCases) rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs  <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  Weights <- Weights/sum(Weights,na.rm=T);
  
  rmse <- NA;
  if (length(Obs)>0) rmse <- sqrt(sum(Weights*((Obs-Sims)^2)/sum(Weights),na.rm=T))
  
  return(round(rmse,Digits));
}

#=================================================
#=================================================
#=================================================

RRMSE <- function(Obs,Sims,Weights=NULL,Digits=3,CompleteCases=F) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- 1:length(Obs)
  if (CompleteCases) rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs  <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  Weights <- Weights/sum(Weights,na.rm=T);
  
  rrmse <- NA;
  if (length(Obs)>0) {
    rrmse <- RMSE(Obs,Sims,Weights,Digits,CompleteCases);
    rrmse <- 100*rrmse/abs(mean(Obs,na.rm=T));
  }
  
  return(round(rrmse,Digits));
}

#=================================================
#=================================================
#=================================================

RMSRE <- function(Obs,Sims,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs  <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  Weights <- Weights/sum(Weights,na.rm=T);
  
  rmsre <- NA;
  if (length(Obs)>0) {
    re <- (Obs-Sims)/Obs;
    rmsre <- sqrt(sum(Weights*(re^2)));
  }
  return(round(100*rmsre,Digits));
}

#=================================================
#=================================================
#=================================================

MAE <- function(Obs,Sims,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  
  mae <- NA;
  if (length(Obs)>0) {
    mae <- sum(Weights*abs(Obs-Sims))/sum(Weights);
  }
  
  return(round(mae,Digits));
}

#=================================================
#=================================================
#=================================================

ME <- function(Obs,Sims,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  
  me <- NA;
  if (length(Obs)>0) {
    me <- sum(Weights*(Sims-Obs))/sum(Weights);
  }
  
  return(round(me,Digits));
}

#=================================================
#=================================================
#=================================================

NASH <- function(Obs,Sims,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Sims));
  if (length(Weights)==1) Weights <- rep(Weights,length(Sims));
  
  rows <- complete.cases(cbind(Obs,Sims,Weights));
  Obs <- Obs[rows];
  Sims <- Sims[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  
  Weights <- 1;
  
  EF <- NA;
  if (length(Obs)>0) {
    E1 <- Weights*((Obs-Sims)^2);
    E2 <- Weights*((Obs-mean(Obs))^2);
    EF <- round(1-(sum(E1)/sum(E2)),Digits);
  }
  
  return(EF);
}

#=================================================
#=================================================
#=================================================

WMean <- function(Data,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Data))
  if (length(Weights)==1) Weights <- rep(Weights,length(Data))
  
  rows <- which(complete.cases(cbind.data.frame(Data,Weights)))
  Data <- Data[rows]
  Weights <- Weights[rows]
  Weights[is.na(Weights)] <- 0
  
  wmean <- NA
  if (length(Data)>0) {
    wmean <- sum(Weights*Data/sum(Weights))
    wmean <- round(wmean,Digits)
  }
  return(wmean)
}

#=================================================
#=================================================
#=================================================

WStd <- function(Data,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Data));
  if (length(Weights)==1) Weights <- rep(Weights,length(Data));
  
  rows <- complete.cases(cbind(Data,Weights));
  Data <- Data[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  
  wstd <- NA;
  if (length(Data)>0) {
    wmean <- WMean(Data,Weights=Weights,Digits=Digits);
    Temp1 <- sum(Weights*(Data - wmean) ^ 2);
    Temp2 <- (sum(Weights!=0)-1)*sum(Weights)/sum(Weights!=0);
    wstd  <- sqrt(Temp1/Temp2);
    wstd  <- round(wstd,Digits);
  }
  
  return(wstd);
}

#=================================================
#=================================================
#=================================================

WCV <- function(Data,Weights=NULL,Digits=3) {
  
  if (is.null(Weights)) Weights <- rep(1,length(Data));
  if (length(Weights)==1) Weights <- rep(Weights,length(Data));
  
  rows <- complete.cases(cbind(Data,Weights));
  Data <- Data[rows];
  Weights <- Weights[rows];
  Weights[is.na(Weights)] <- 0;
  
  wcv <- NA;
  if (length(Data)>0) {
    wmean <- WMean(Data,Weights=Weights,Digits=Digits);
    wstd  <- WStd(Data,Weights=Weights,Digits=Digits);
    wcv   <- round(wstd/wmean,Digits);
  }
  
  return(wcv);
}

#=================================================
#=================================================
#=================================================

Mean <- function(Data,Digits=3,FullRows=F) {
  Data <- as.data.frame(Data);
  if (FullRows) Data <- Data[complete.cases(Data),];
  if (length(unlist(Data))==1) {
    mean <- Data;
  }
  else {
    mean <- apply(Data,2,mean,na.rm=T);
  }
  return(round(mean,Digits));
}

#=================================================
#=================================================
#=================================================

Std <- function(Data,Digits=3,FullRows=F,na.rm=T) {
  Data <- as.data.frame(Data);
  if (FullRows) Data <- Data[complete.cases(Data),];
  if (length(unlist(Data))==1) {
    std <- 0;
  }
  else {
    std <- apply(Data,2,sd,na.rm=na.rm);
  }
  return(round(std,Digits));
}

#=================================================
#=================================================
#=================================================

SE <- function(Data,Digits=3,FullRows=F,na.rm=T) {
  Data <- as.data.frame(Data);
  if (FullRows) Data <- Data[complete.cases(Data),]
  if (length(unlist(Data))==1) {
    se <- 0;
  }
  else {
    Func <- function(x) {sd(x,na.rm=na.rm)/sqrt(sum(!is.na(x),na.rm=T))}
    se <- apply(Data,2,Func);
  }
  return(round(se,Digits));
}

#=================================================
#=================================================
#=================================================

Length <- function(Data,FullRows=F,na.rm=T) {
  Data <- as.data.frame(Data);
  if (FullRows) Data <- Data[complete.cases(Data),];
  
  Func <- function(x) {
    if (na.rm) return(sum(!is.na(x)))
    else       return(length(x))
  }
  return(apply(Data,2,Func))
}

#=================================================
#=================================================
#=================================================

CV <- function(Data,Digits=1,FullRows=F) {
  if (is.vector(Data)) Data <- as.matrix(Data)
  if (FullRows) Data <- Data[complete.cases(Data),]
  cv <- 100*Std(Data,Digits=5)/abs(Mean(Data,Digits=5))
  return(round(cv,Digits))
}

#=================================================
#=================================================
#=================================================

Quantile <- function(Data,Digits=3,Probs=0.9,FullRows=F) {
  if (is.vector(Data)) Data <- as.matrix(Data);
  if (FullRows) Data <- Data[complete.cases(Data),];
  if (!is.matrix(Data) && !is.data.frame(Data) && !is.data.table(Data)) {
    qn <- quantile(Data,na.rm=T,probs=Probs,type=8);
  }
  else {
    qn <- apply(Data,2,quantile,na.rm=T,probs=Probs,type=8);
  }
  return(round(qn,Digits));
}

#=================================================
#=================================================
#=================================================

CutAtQuantiles <- function(Data,Digits=3,Probs=c(0.05,0.95),FullRows=F) {
  
  if (is.vector(Data)) Data <- matrix(Data,ncol=1);
  #if (FullRows) Data <- Data[complete.cases(Data),];
  #if (class(Data)=='numeric') Data <- matrix(Data,ncol=1);
  
  qn1 <- apply(Data,2,quantile,na.rm=T,probs=Probs[1],type=8);
  qn2 <- apply(Data,2,quantile,na.rm=T,probs=Probs[2],type=8);
  
  for (i in 1:ncol(Data)) Data[Data[,i]<qn1[i] | Data[,i]>qn2[i],i] <- NA;
  
  return(Data);
}

#=================================================
#=================================================
#=================================================

R.Factor <- function(Data,Digits=1,Probs=c(0.1,0.9),FullRows=F,AsPerc=T) {
  q1 <- Quantile(Data,Digits,Probs=Probs[1],FullRows=FullRows);
  q2 <- Quantile(Data,Digits,Probs=Probs[2],FullRows=FullRows);
  mn <- Mean(Data,Digits,FullRows=FullRows);
  if ( AsPerc) r.factor <- 100*(q2-q1)/mn
  else         r.factor <- (q2-q1);
  return(round(r.factor,Digits));
}

#=================================================
#=================================================
#=================================================

P_Factor <- function(Obs,Sim=NULL,Probs=c(0.1,0.9)) {
  
  if (length(dim(Obs))>1 & is.null(Sim)) {
    Sim <- Obs[,2];
    Obs <- Obs[,1];
  }
  
  omean <- mean(Obs,na.rm=T);
  q1sim <- Quantile(Sim,Probs=Probs[1],FullRows=F);
  q2sim <- Quantile(Sim,Probs=Probs[2],FullRows=F);
  pf    <- as.numeric(q1sim <= omean & q2sim >= omean);
  return(pf);
}

#=================================================
#=================================================
#=================================================

Rsquare = function(Y,Fitted.Y) {
  # compute coefficient of determination
  if (class(Fitted.Y) == "numeric") {
    return(cor(Y,Fitted.Y)^2);
  } else {
    R2 = double(ncol(Y));
    for (ic in 1:ncol(Y))
      R2[ic] = cor(Y[,ic],Fitted.Y[,ic])^2;
    return(R2);
  }
}

#=================================================
#=================================================
#=================================================

Factor2Numeric <- function(Data,Exc=-1,Chr=NULL) {
  if (class(Data)=="matrix" | class(Data)=="data.frame") {
    if (is.numeric(Exc)) {
      for (i in 1:ncol(Data)) {
        if (!i %in% Exc) Data[,i] <- Factor2Numeric(Data[,i]);
        if ( i %in% Chr) Data[,i] <- as.character(Data[,i]);
      }
    }
    if (is.character(Exc)) {
      for (col in colnames(Data)) {
        if (!col %in% Exc) Data[,col] <- Factor2Numeric(Data[,col]);
        if ( col %in% Chr) Data[,col] <- as.character(Data[,col]);
      }
    }
    return(Data)
  }
  else {
    if (class(Data)=="factor") {
      Data <- as.character(Data);
    }
    Data <- as.numeric(Data);
    return(Data);
  }
}

#=================================================
#=================================================
#=================================================

RemoveAllExcept <- function(X,Paths) {
  rm(list=setdiff(ls(),X));
  source(paste(Paths$Scripts,'Source All.R',sep='/'));
  SourceAll(Paths$Scripts);
}

#=================================================
#=================================================
#=================================================

GoodHeaders <- function(Header) {
  for (i in 1:length(Header)) {
    ind <- regexpr("\\(",Header[i])[1];
    if (ind!=-1) Header[i] <- substr(Header[i],1,ind-2);
  }
  return(Header);
}

#=================================================
#=================================================
#=================================================

FreqTable <- function(Vector,Sort=T) {
  Names <- as.numeric(names(table(Vector)));
  Freq  <- as.numeric(table(Vector));
  
  if (Sort) Inds <- order(Freq,decreasing=T)
  else      Inds <- 1:length(Names);
  
  Freq  <- cbind(Names[Inds],Freq[Inds]);
  Freq  <- cbind(Freq,round(Freq[,2]/sum(Freq[,2]),3));
  Freq  <- as.data.frame(Freq);
  
  colnames(Freq) <- c("Class","Freq","Prob");
  return(Freq);
}

#=================================================
#=================================================
#=================================================

WFreqTable <- function(Data,Cols,Weights) {
  Out <- list();
  
  for (i in 1:length(Cols)) {
    xData <- Data[,Cols[i]];
    Classes <- seq(min(xData),max(xData));
    
    Counts <- NULL;
    for (c in 1:length(Classes)) {
      class  <- Classes[c];
      Counts <- cbind(Counts,(xData==class)*MakeUnitSum(Weights));
    }
    Probs <- round(colSums(Counts),3);
    Freq  <- as.data.frame(cbind(Classes,floor(Probs*length(xData)),Probs));
    colnames(Freq) <- c("Class","Freq","Prob");
    Out[[i]] <- Freq;
  }
  if (i==1) Out <- Out[[1]];
  
  return(Out);
}

#=================================================
#=================================================
#=================================================

FreqTable2D <- function(Data,xCol,yCol,Reshape=F) {
  Freq <- table(Data[,xCol],Data[,yCol])
  Freq <- cbind(Freq,'All'=rowSums(Freq))
  Freq <- as.data.frame(apply(Freq,2,function(x){round(100*x/sum(x),3)}))
  Freq <- cbind.data.frame(rownames(Freq),Freq)
  colnames(Freq)[1] <- xCol
  rownames(Freq) <- NULL
  
  if (Reshape) {
    Freq <- Reshape_Table_to_Long(Table=Freq,StatCols=xCol,VarCols=NULL,VarName=yCol,ValName='P')
    Freq$P <- Factor2Numeric(Freq$P)
  }
  return(Freq);
}

#=================================================
#=================================================
#=================================================

FreqTableXD <- function(Data,Cols=NULL,AggCol=NULL,AggUnit='All',Prob=T,IncludeZero=T,ToDT=T) {
  Data <- data.table(Data)
  if (is.null(Cols)) Cols <- names(Data)
  LastCol <- Cols[length(Cols)]
  isNum <- is.numeric(Data[,get(LastCol)])
  
  if (IncludeZero) Members <- sort(unique(Data[,get(LastCol)]))
  
  FUNC <- function(x) {
    Probs <- Freq <- NULL
    Freq  <- table(x, useNA='ifany')
    if (IncludeZero && length(Freq)<length(Members)) {
      for (Mem in setdiff(Members,names(Freq))) Freq[as.character(Mem)] <- as.double(0)
      Freq <- Freq[sort(names(Freq))]
    }
    Prob  <- round(100*Freq/sum(Freq,na.rm=T),2)
    Out   <- data.table(X=names(Freq),Freq=as.double(unclass(Freq)),Prob=as.double(unclass(Prob)))
    colnames(Out)[1] <- LastCol
    return(Out)
  }
  Freq <- Data[, FUNC(.SD), keyby=setdiff(Cols,LastCol), .SDcols=LastCol]
  setkeyv(Freq,cols=Cols)
  
  if (!is.null(AggCol)) {
    if (length(AggUnit)==1 && length(AggCol)!=1) AggUnit <- rep(AggUnit,length(AggCol))
    for (ACol in AggCol) {
      Temp <- FreqTableXD(Data=Data,Cols=setdiff(Cols,ACol),AggCol=NULL,AggUnit=NULL,
                          Prob=Prob,IncludeZero=IncludeZero,ToDT=ToDT)
      Temp[, (ACol):=AggUnit[AggCol==ACol]]
      Freq <- rbind(Freq,Temp)
    }
  }
  
  if (!ToDT) {
    if (isNum) Freq <- Sort_Factors(Freq,Col=LastCol,PutLast=NULL,isNum=T)
    Freq <- MySort(as.data.frame(Freq),AscCols=Cols,DescCols=NULL)
  }
  return(Freq);
}

#=================================================
#=================================================
#=================================================

FreqTableXDOld2 <- function(Data,Cols=NULL,AggCol=NULL,Prob=F) {
  isNum <- T
  if (is.null(Cols)) Cols <- names(Data)
  LastCol <- Cols[length(Cols)]
  if (any(!is.numeric(Data[!is.na(Data[,LastCol]),LastCol]))) isNum <- F
  
  if (length(Cols)>1) {
    Data$X12345Y <- apply(Data[,Cols],1,paste,collapse='__')
  } else {
    Data$X12345Y <- Data[,Cols]
  }
  Factors <- unique(Data[,c(Cols,'X12345Y')])
  Freq <- as.data.frame(table(Data$X12345Y))
  Freq <- merge(Factors,Freq,by.x='X12345Y',by.y='Var1',sort=F)
  Freq <- Freq[,-1]
  
  if (!is.null(AggCol)) {
    for (Agg in AggCol) {
      if (!(Agg %in% Cols)) stop('AggCol must be among Cols!')
      Freq2 <- FreqTableXD(Data,Cols=Cols[Cols!=Agg],AggCol=NULL,Prob=F)
      Freq2[,Agg] <- 'All'
      Freq <- rbind.data.frame(Freq,Freq2)
    }
  }
  
  if (isNum) Freq <- Sort_Factors(Freq,Col=LastCol,PutLast=NULL,isNum=T)
  Freq <- MySort(Freq,AscCols=Cols,DescCols=NULL)
  if (Prob) Freq <- FreqToProb(FreqTable=Freq,FreqCol='Freq',Cols=NULL)
  
  return(Freq);
}

#=================================================
#=================================================
#=================================================

FreqToProb <- function(FreqTable,FreqCol='Freq',Cols=NULL) {
  Freq <- FreqTable
  
  if (is.null(FreqCol)) FreqCol <- names(Freq)[ncol(Freq)]
  if (is.null(Cols)) Cols <- names(Freq)[names(Freq)!=FreqCol]
  LastCol <- Cols[length(Cols)]
  Cols2 <- Cols[-length(Cols)]
  
  FUNC <- function(x) {
    Prob <- cbind.data.frame(x[,1],Prob=round(100*MakeUnitSum(x[,2]),2))
    return(Prob)
  }
  
  if (length(Cols2)==0) {
    Freq$Prob <- round(MakeUnitSum(Freq[,FreqCol]),2)
    
  } else {
    if (length(Cols2)>1) Indices <- Freq[,Cols2]
    else Indices <- list(Level=Freq[,Cols2])
    
    x <- MyBy(Freq[,c(LastCol,FreqCol)],Indices,FUNC,AddLevels=T)
    names(x)[1:length(Cols)] <- Cols
    Freq <- merge(Freq,x,by=Cols,sort=F)
    
    if (any(!is.finite(unlist(Freq$Prob)))) Freq$Prob[!is.finite(unlist(Freq$Prob))] <- 0
    colnames(Freq)[1:length(Cols)] <- Cols
    rownames(Freq) <- NULL
  }
  
  return(Freq)
}

#=================================================
#=================================================
#=================================================

RemoveConstCols <- function(Data) {
  cols <- names(Data[,sapply(Data, function(v) var(v,na.rm=T)!=0)]);
  Data <- Data[,cols];
  return(Data);
}

#=================================================
#=================================================
#=================================================

KNN <- function(Xinp,Xtarg,Weights=NULL,K=10,Type='NonLin2') {
  
  if (!is.null(dim(Xtarg)) && nrow(Xtarg)>1) {
    KNNx <- function(x) {
      x <- KNN(Xinp,x,Weights=Weights,K=K,Type=Type)
      return(x)
    }
    Out <- apply(Xtarg,1,KNNx)
    if (K==1) Out <- rbindlist(Out)
    return(Out)
  }
  
  dist  <- Dist2Center(Xinp,Xtarg,Weights=Weights)
  dist[is.na(dist)] <- Inf
  dist  <- sort(as.numeric(dist),decreasing=F,index.return=T,na.last=NA,method='auto')
  index <- dist$ix[1:K]
  dist  <- dist$x[1:K]
  
  wei <- 1
  if (K>1) {
    if (Type=='NonLin')  wei <- (1/dist)/sum(1/dist,na.rm=T)
    if (Type=='NonLin2') wei <- (1/dist^2)/sum(1/dist^2,na.rm=T)
    if (Type=='NonLin4') wei <- (1/dist^4)/sum(1/dist^4,na.rm=T)
  }
  
  Out <- cbind.data.frame(index,dist,wei)
  colnames(Out) <- c("Ind","Dist","Weight")
  return(Out)
}

#=================================================
#=================================================
#=================================================

KNN_Calc <- function(Xinp,Yinp,Xtarg,K=10,Type='NonLin',Digits=3) {
  
  Types  <- class(Xinp)
  Xtarg2 <- copy(Xtarg)
  if (is.null(dim(Yinp)))  Yinp  <- matrix(Yinp,ncol=1) else Types <- sapply(Yinp,function(x) {return(class(x)[1])},simplify=T)
  if (is.null(dim(Xinp)))  Xinp  <- matrix(Xinp,ncol=1)
  if (is.null(dim(Xtarg))) Xtarg <- matrix(Xtarg,ncol=ncol(Xinp))
  
  if (is.vector(Yinp) || ncol(Yinp)==1) {
    NARows <- which(is.na(Yinp))
    if (length(NARows)>0) {
      Yinp  <- Yinp[-NARows,]
      Xinp  <- Xinp[-NARows,]
      Xtarg <- Xtarg[-NARows,]
    }
  }
  
  Y <- list()
  
  for (i in 1:nrow(Xtarg)) {
    dist  <- KNN(Xinp,Xtarg[i,],K=K,Type=Type)
    y     <- Yinp[dist$Ind,]
    
    if (K==1) {
      Y[[i]] <- y
    } else {
      #y      <- matrix(y,ncol=ncol(Yinp),byrow=T)
      Y[[i]] <- round(apply(y,2,WMean,Weights=dist$Weight),Digits) 
    }
  }
  
  if (i==1) {
    Y <- Y[[1]]
  } else { 
    if (length(Y[[1]])==1) {
      Y <- unlist(Y)
    } else {
      Y <- rbindlist(Y)
    }
  }
  
  if(!(is.matrix(Y) || is.data.frame(Y) || is.data.table(Y))) Y <- data.table(matrix(Y,ncol=ncol(Yinp)))
  colnames(Y) <- colnames(Yinp)
  return(Y)
}

#=================================================
#=================================================
#=================================================

KNN_CV <- function(Xinp,Yinp,K=10,Type='Linear') {
  
  if (is.vector(Yinp)) Yinp <- matrix(Yinp,ncol=1);
  YP <- matrix(data=NA,nrow=nrow(Xinp),ncol=ncol(Yinp));
  
  for (i in 1:nrow(Xinp)) {
    x <- Xinp[i,];
    X <- Xinp[-i,];
    y <- Yinp[i,];
    Y <- Yinp[-i,];
    YP[i,] <- KNN_Calc(X,Y,x,K,Type=Type);
  }
  return(YP);
}

#=================================================
#=================================================
#=================================================

KNN_Fill <- function(Data,K=10) {
  Data <- as.data.table(Data)
  
  KNN_Fill_Col <- function(YData,Data,K) {
    Col   <- which(apply(Data,2,identical,YData))
    XData <- Data[,-Col,with=F]
    
    Rows <- which(is.na(YData))
    if (length(Rows)>0) {
      Xinp  <- matrix(XData[-Rows,],ncol=ncol(XData),byrow=F)
      Yinp  <- matrix(YData[-Rows ],ncol=1,byrow=F)
      Xtarg <- matrix(XData[ Rows,],ncol=ncol(XData),byrow=F)
      K2    <- min(K,nrow(Xinp))
      YData[Rows] <- KNN_Calc(Xinp,Yinp,Xtarg,K=K2,Digits=3)
    }
    return(YData)
  }
  
  Data <- apply(Data,2,KNN_Fill_Col,Data,K)
  return(Data)
}

#=================================================
#=================================================
#=================================================

KNN_Fill_Row <- function(Data,K=50) {
  
  Rows <- which(apply(is.na(Data),1,any));
  if (length(Rows)>0) {
    for (row in Rows) {
      Xinp  <- Data[-row,];
      Yinp  <- Data[-row,is.na(Data[row,])];
      Xtarg <- Data[ row,];
      K2    <- min(K,nrow(Xinp));
      Data[row,is.na(Data[row,])] <- KNN_Calc(Xinp,Yinp,Xtarg,K=K2,Digits=3);
    }
  }
  
  return(Data);
}

#=================================================
#=================================================
#=================================================

Mean_Fill <- function(Data) {
  if (is.null(dim(Data))) Data <- matrix(Data,ncol=1);
  
  for(i in 1:ncol(Data)){
    Data[is.na(Data[,i]),i] <- mean(Data[,i],na.rm=TRUE);
  }
  return(Data);
}


#=================================================
#=================================================
#=================================================

KNN_Fill1 <- function(Data,K=50) {
  
  Rows <- which(apply(is.na(Data),1,any));
  if (length(Rows)>0) {
    for (row in Rows) {
      dataX <- matrix(Data[ row,],ncol=ncol(Data),byrow=F);
      dataF <- matrix(Data[-Rows,],ncol=ncol(Data),byrow=F);
      Xinp  <- matrix(dataF[,!is.na(Data[row,])],ncol=ncol(Data)-1,byrow=F);
      Yinp  <- matrix(dataF[, is.na(Data[row,])],ncol=1,byrow=F);
      Xtarg <- matrix(dataX[,!is.na(Data[row,])],ncol=ncol(Data)-1,byrow=F);
      K2    <- min(K,nrow(Xinp));
      Data[row,is.na(Data[row,])] <- KNN_Calc(Xinp,Yinp,Xtarg,K=K2,Digits=3);
    }
  }
  return(Data);
}

#=================================================
#=================================================
#=================================================

KNN_Fill2 <- function(Data,K=50) {
  
  for (i in 1:ncol(Data)) {
    
    Rows <- which(is.na(Data[,i]));
    if (length(Rows)>0) {
      dataX <- matrix(Data[ Rows,],ncol=ncol(Data),byrow=F);
      dataF <- matrix(Data[-Rows,],ncol=ncol(Data),byrow=F);
      Xinp  <- matrix(dataF[,-i],ncol=ncol(Data)-1,byrow=F);
      Yinp  <- matrix(dataF[, i],ncol=1,byrow=F);
      Xtarg <- matrix(dataX[,-i],ncol=ncol(Data)-1,byrow=F);
      K2    <- min(K,nrow(Xinp));
      Data[Rows,i] <- KNN_Calc(Xinp,Yinp,Xtarg,K=K2,Digits=3);
    }
  }
  return(Data);
}

#=================================================
#=================================================
#=================================================

KMeans <- function(Data,Centers,IterMax=10,MaxBadIter=5,NStart=1,Weights=NULL,DoStand=F) {
  
  if (is.null(Weights))   Weights <- rep(1,nrow(Data));
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data));
  
  FlagCent <- (length(Centers)!=1);
  FlagInit <- F;
  
  if (NStart>1 & length(Centers)==1) {
    FlagInit <- T;
    TotWCSS <- NULL; runs <- list();
    
    for (i in 1:NStart) {
      # message("Initial runs: ",i);
      runs[[i]]  <- KMeans(Data=Data,Centers=Centers,IterMax=1,MaxBadIter=MaxBadIter,NStart=1,Weights=Weights,DoStand=DoStand);
      TotWCSS[i] <- runs[[i]]$tot.withinss;
    }
    
    best <- which.min(TotWCSS);
    Centers <- runs[[best]]$centers;
    WCSS <- runs[[best]]$withinss
    TotWCSS <- runs[[best]]$tot.withinss;
    class <- runs[[best]]$cluster;
  }
  else TotWCSS <- Inf;
  
  Data2 <- Weights*scale(Data,center=DoStand,scale=DoStand);
  
  if (length(Centers)==1) {
    K <- Centers;
    IndCent <- sample.int(nrow(Data),K);
    Centers <- Data2[IndCent,];
  } 
  else {
    K <- nrow(Centers);
  }
  
  BadIter <- 0;
  CentersNew <- Centers;
  
  for (i in 1:IterMax) {
    Diss  <- list();
    
    Diss     <- Dist2Center(Data2,CentersNew,Weights=NULL);
    classNew <- max.col(-Diss,ties.method="first");
    Diss     <- Diss[cbind(seq_along(classNew),classNew)];
    
    WCSSNew    <- c(unlist(by(Diss^2,as.character(classNew),sum,na.rm=T)));
    CentersNew <- matrix(unlist(by(Data2,as.character(classNew),colMeans,na.rm=T)),ncol=ncol(Centers),byrow=T);
    if (nrow(CentersNew) < K) CentersNew <- rbind(CentersNew,Data2[sample.int(K-nrow(CentersNew)),]);
    
    # message("Within-cluster sum of squares = ",round(sum(WCSSNew)));
    
    if (sum(WCSSNew)/TotWCSS>=1) 
    { 
      BadIter <- BadIter + 1;
      CentersNew[sample.int(nrow(CentersNew),1),] <- Data2[sample.int(nrow(Data),1),];
    }
    else 
    { 
      TotWCSS <- sum(WCSSNew); WCSS <- WCSSNew; class <- classNew; BadIter <- 0;
      if (i>1) Centers <- CentersNew; 
    }
    if (BadIter == MaxBadIter) break;
  }
  
  return(list(cluster=class,centers=Centers,withinss=WCSS,tot.withinss=TotWCSS,iter=i,BadIter=BadIter));
}

#=================================================
#=================================================
#=================================================

Clara <- function(Data,K,SampNum=10,SampSize=50,SampSize2=1000,Weights=NULL,DoStand=F,IndMeds=NULL,Replace=F,SuppData=NULL) {
  
  if (is.null(Weights))   Weights <- rep(1,nrow(Data));
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data));
  
  DataX <- scale(Data,center=DoStand,scale=DoStand);
  
  if (!Replace) AllInds <- sample.int(nrow(Data),SampNum*SampSize);
  
  if (is.null(IndMeds)) {
    Clara <- clara(DataX[sample.int(nrow(Data),SampSize2),],K,metric="euclidean",stand=F,samples=50,sampsize=SampSize/2,trace=0,medoids.x=TRUE,keep.data=FALSE,rngR=TRUE,pamLike=TRUE);
    IndMeds <- Clara$i.med;
  }
  
  ObjectiveOld1 <- ObjectiveOld2 <- Inf;
  for (s in 1:SampNum) {
    
    if (!Replace) {
      Inds <- ((s-1)*SampSize+1):(s*SampSize);
      Inds <- c(IndMeds,AllInds[Inds]);
    }
    else Inds <- c(IndMeds,sample.int(nrow(Data),SampSize));
    
    Pam1 <- pam(DataX[Inds,],K,stand=F,diss=F);
    
    if (SampSize2==nrow(Data)) Inds2 <- 1:nrow(Data)
    else Inds2 <- sample.int(nrow(Data),SampSize2);
    
    Clustering <- Manual_Clustering(DataX[Inds2,],t(Pam1$medoids),Weights=Weights,DoStand=F,CalcStats=T,UseCenteroids=F,SortProbs=T);
    Objective  <- Pam1$objective[2];
    Objective1 <- Clustering$TWCSS;
    
    if (Objective1<ObjectiveOld1) {
      Clustering <- Manual_Clustering(DataX,t(Pam1$medoids),Weights=Weights,DoStand=F,CalcStats=T,UseCenteroids=F,SortProbs=T);
      Objective2 <- Clustering$TWCSS;
      
      if (Objective2<ObjectiveOld2) {
        IndMeds <- Inds[Pam1$id.med];
        Medoids <- Pam1$medoids;
        ObjectiveOld1 <- mean(c(Objective2*SampSize2/nrow(DataX),Objective1));
        ObjectiveOld2 <- Objective2;
        Pam <- Pam1;
      }
    }
  }
  
  Clustering <- Manual_Clustering(DataX,t(Medoids),Weights=Weights,DoStand=F,CalcStats=T,UseCenteroids=F,SortProbs=F,SuppData=SuppData);
  
  Clustering$IndMeds <- IndMeds;
  Clustering$Pam <- Pam;
  return(Clustering);
}

#=================================================
#=================================================
#=================================================

RoundTo <- function(Val,Multiplier,Type=0,IsSymm=F) {
  if (length(Type)>1) {
    for (i in 1:length(Type)) {
      Val[i] <- RoundTo(Val[i],Multiplier,Type=Type[i]);
    }
    
    if (IsSymm & length(Val)==2) Val <- c(-max(abs(Val)),max(abs(Val)));
    return(Val);
  }
  
  if (Type== 0) Val <- round(Val/Multiplier)*Multiplier;
  
  if (Type==+1) Val[Val>0] <- ceiling(Val[Val>0]/Multiplier)*Multiplier;
  if (Type==+1) Val[Val<0] <- floor(Val[Val<0]/Multiplier)*Multiplier;
  
  if (Type==-1) Val[Val>0] <- floor(Val[Val>0]/Multiplier)*Multiplier;
  if (Type==-1) Val[Val<0] <- ceiling(Val[Val<0]/Multiplier)*Multiplier;
  return(Val);
}

#=================================================
#=================================================
#=================================================

Merge_Shape_Files <- function(Folder,Layer,Table,SCols,TCols=NULL,Num=10,MinX=1000,Res=600) {
  
  ShapeFile <- readOGR(dsn=Folder,layer=Layer,verbose=F);
  
  if (is.null(TCols)) TCols <- SCols;
  Joined <- merge(ShapeFile,Table,by.x=SCols,by.y=TCols);
  x <- Joined$WHEATTOT.x;
  
  ColBands <- colorRampPalette(c("green","red"))(Num);
  Bins <- quantile(x,seq(0,1,1/Num));
  Bins <- cut(x,Bins);
  Cols <- as.numeric(Bins);
  Cols <- ColBands[Cols];
  Cols[x<MinX] <- NA;
  
  dev.set(2);
  png(file="E:/Behnam/Test.png",res=Res,width=800*Res/150,height=700*Res/150);
  par(lty=1,lwd=0.1,mar=c(0,0,0,0));
  
  plot(Joined,col=Cols,border="darkgrey");
  legend('topright',fill=ColBands,legend=levels(Bins),col=ColBands,cex=0.6,bty='n',inset=c(0.05,0.05),
         title='Wheat Cultivation (ha)',title.adj=0.3);
  
  z <- dev.off(2); graphics.off();
  
  return(Joined);
}

#=================================================
#=================================================
#=================================================

MarkovTransProbs <- function(Data) {
  Stats <- sort(unique(Data));
  Trans <- expand.grid(Stats,Stats);
  Trans <- Trans[order(Trans[,1],Trans[,2]),];
  Trans <- cbind(Trans,NA);
  
  LData <- cbind(Data[1:(length(Data)-1)],Data[2:length(Data)]);
  
  for (S1 in Stats) {
    XData <- matrix(LData[LData[,1]==S1,],ncol=ncol(LData));
    NumTr <- nrow(XData);
    
    for (S2 in Stats) {
      NumS2 <- sum(XData[,2]==S2);
      TrPr  <- ifelse(NumTr==0,1/max(Data),NumS2/NumTr);
      
      row <- which(Trans[,1]==S1 & Trans[,2]==S2);
      Trans[row,3] <- round(TrPr,3);
    }
  }
  
  colnames(Trans) <- c('S1','S2','Prob');
  return(Trans)
}

#=================================================
#=================================================
#=================================================

FindCurrentPath <- function(WithFileName=T) {
  i <- 1
  if (is.null(sys.frame(1)$ofile)) i <- 2
  if (WithFileName) return(sys.frame(i)$ofile)
  else return(dirname(sys.frame(i)$ofile))
}

#=================================================
#=================================================
#=================================================

GetNodes <- function(Doc,Node) {
  x<- getNodeSet(Doc,paste0("//",Node))
  if(length(x) == 0) {stop(paste("No nodes matched",Node))}
  return (x)
} 

#=================================================
#=================================================
#=================================================

Split_a_Vector <- function(Vector,NumGroup=NULL,PerGroup=10) {
  
  if (is.null(NumGroup)) {
    x <- split(Vector,ceiling(seq_along(Vector)/PerGroup))
  } else {
    x <- split(Vector,ceiling(seq_along(Vector)/(ceiling(length(Vector)/NumGroup))))
  }
  return(x)
  
} 

#=================================================
#=================================================
#=================================================

List_of_Drives <- function() {
  
  Names <- NULL
  for (i in LETTERS) {
    if (dir.exists(paste0(i,":/"))) Names <- c(Names,paste0(i,":"))
  }
  return(Names)
}

#=================================================
#=================================================
#=================================================

Find_a_Folder_on_HDD <- function(Folder,Drives) {
  
  if (is.null(Drives)) Drives <- List_of_Drivers()
  
  for (i in Drives) {
    Folds <- list.dirs(i,full.names=T,recursive=T)
    pos <- grep(paste0("/",Folder),Folds)
    if (length(pos)>0) return(Folds[pos])
  }
}

#=================================================
#=================================================
#=================================================

Dist2Center <- function(Data,Center,Weights=NULL,IncForNAs=T) {
  #library(pdist)
  
  if (is.null(Weights))   Weights <- matrix(rep(1,nrow(Data)),ncol=1)
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data))
  if (is.vector(Data))    Data    <- matrix(Data,nrow=1)
  if (is.vector(Center))  Center  <- matrix(as.numeric(Center),nrow=1)
  if (F) Weights <- matrix(rep(Weights,nrow(Center)),ncol=nrow(Center),byrow=F)
  
  Raise = 1
  if (IncForNAs && any(is.na(Data))) {
    # Increase the distances for the existence of NA values
    FUNC <- function(x,xCenter) {
      xCenter <- unlist(xCenter)
      return(sum(!is.na(xCenter) & !is.nan(xCenter))/sum(!is.na(x) & !is.nan(x)))
    }
    Raise <- NULL;
    for (i in 1:nrow(Center)) {
      Raise <- cbind(Raise,apply(Data,1,FUNC,Center[i,]))
    }
  }
  
  Dist <- Raise*as.matrix(pdist(Data,Center))
  #Dist <- Raise*as.matrix(dist.matrix(Data,Center,method='euclidean'))
  return(Dist)
}

#=================================================
#=================================================
#=================================================

Manual_Clustering <- function(Data,Centers,Weights=NULL,DoStand=F,CalcStats=T,SortProbs=T,SuppData=NULL) {
  
  Data <- as.data.frame(Data)
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data));
  if (is.null(Weights))   Weights <- 1
  
  Data2 <- Data[,colnames(Centers)];
  Data2 <- scale(Data2,center=DoStand,scale=DoStand)
  Data2 <- Weights * Factor2Numeric(as.data.frame(Data2))
  
  Diss  <- matrix(NA,nrow=nrow(Data),ncol=nrow(Centers));
  Diss  <- Dist2Center(Data2,Centers,Weights=NULL);
  
  Classes <- max.col(-Diss,ties.method="first");
  Freq    <- FreqTable(Classes,Sort=SortProbs);
  OrigInd <- Freq[,1];
  
  # Sorting based on the median of the supplementary data, instead of the frequencies
  if (!is.null(SuppData)) {
    
    if (length(unique(Weights))==1) MedYields <- c(by(SuppData,as.factor(as.character(Classes)),median,na.rm=T))
    else {
      MedYields <- NULL;
      for (i in sort(unique(Classes))) {
        MedYields[i] <- quantile(SuppData[Classes==i],0.5,na.rm=T);
      }
    }
    NewClasses <- order(MedYields,decreasing=T);
    
    OldClasses <- Classes;
    for (i in 1:length(NewClasses)) {
      Classes[OldClasses==NewClasses[i]] <- i;
      OrigInd[i] <- NewClasses[i];
    }
    Freq <- FreqTable(Classes,F);
  }
  
  Classes2 <- 0*Classes;
  for (i in 1:nrow(Freq)) Classes2[Classes==Freq[i,1]] <- i;
  
  Centers  <- Centers[Freq[,1],];
  Diss     <- Diss[cbind(seq_along(Classes),Classes)];
  Freq[,1] <- 1:nrow(Freq);
  Classes  <- Classes2;
  MeanDiss <- mean(Diss);
  
  if (CalcStats) {
    
    # Withing cluster sum of squares
    WCSS <- NULL;
    Centroids <- matrix(NA,nrow=ncol(Data2),ncol=nrow(Centers));
    for (i in 1:nrow(Freq)) {
      xRows <- which(Classes==i);
      if(length(xRows)==1) xRows <- rep(xRows,2);
      Centroids[,i] <- colMeans(Data2[xRows,],na.rm=T);
      Diss    <- Dist2Center(Data2[xRows,],Centroids[,i],Weights=NULL);
      WCSS[i] <- sum(Diss^2,na.rm=T);
    }
    
    # Withing cluster sum of squares
    WCSS  <- cbind(Freq[,1],WCSS,round(100*MakeUnitSum(WCSS),1));
    colnames(WCSS) <- c("Class","WCSS","WCSSP");
    TWCSS <- sum(WCSS[,"WCSS"]);
    
    # Total sum of squares and the proportion of variation explained
    XCentroid <- colMeans(Data2,na.rm=T);
    Diss  <- Dist2Center(Data2,XCentroid,Weights=NULL);
    TSS   <- sum(Diss^2,na.rm=T);
    TBCSS <- TSS-TWCSS;
    R2    <- round(TBCSS/TSS,3);
  }
  else {
    TBCSS <- TSS <- R2 <- TWCSS <- WCSS <- XCentroid <- Centroids <- NULL;
  }
  
  return(list(Classes=Classes,Freq=Freq,Centers=Centers,OrigInd=OrigInd,Centroids=Centroids,XCentroid=XCentroid,MeanDiss=MeanDiss,
              WCSS=WCSS,TWCSS=TWCSS,TBCSS=TBCSS,TSS=TSS,R2=R2));
}

#=================================================
#=================================================
#=================================================

ClusterClara <- function(Data,XAxis=NULL,K,Pref='ET',SampNum=50,SamSize=2000,Weights=NULL,DoStand=F,CusClara=F,SampSize2=25,SuppData=NULL,RepAllNAs=NA) {
  library(cluster,verbose=F,warn.conflicts=F,quietly=T);
  
  if (length(K)>1 | length(SampNum)>1) {
    for (k in K) {
      for (sampNum in SampNum) {
        Clusters[[paste(k,sampNum,sep="_")]] <- ClusterClara(Data,XAxis,k,sampNum,Weights,DoStand,SuppData=SuppData);
      }
    }
    return(Clusters);
  }
  
  if (is.null(Weights))   Weights <- c(rep(1,nrow(Data)));
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data));
  
  set.seed(7663887);
  
  #DataX  <- round(scale(Weights*Data,center=DoStand,scale=DoStand),3)
  #DataX  <- scale(Data,center=DoStand,scale=DoStand);
  DataX   <- scale(Data,center=DoStand,scale=DoStand);
  
  NARows   <- which(apply(DataX,1,function(x) all(is.na(x))))
  GoodRows <- which(apply(DataX,1,function(x) !all(is.na(x))))
  
  if (!is.na(RepAllNAs)) DataX[NARows,] <- RepAllNAs
  
  if (CusClara) {
    Clara <- Clara(Weights*DataX,K,SampNum=SampNum,SampSize2=SampSize2,SampSize=SamSize,Weights=NULL,DoStand=F,SuppData=SuppData);
    
    TypicWei <- Weights[Clara$IndMeds];
    TypicPat <- Data[Clara$IndMeds,];
    Medoids  <- Clara$Centers;
    rownames(Medoids) <- colnames(Data);
    Summary <- Clara;
    Freq <- Clara$Freq;
    Classes <- as.numeric(Clara$Classes);
  }
  else {
    Clara <- clara(x=Weights*DataX,k=K,metric="euclidean",stand=F,samples=SampNum,sampsize=SamSize,trace=0,
                   medoids.x=TRUE,keep.data=FALSE,rngR=TRUE,pamLike=TRUE,correct.d=T);
    
    Medoids  <- Clara$medoids;
    colnames(Medoids) <- colnames(Data);
    TypicPat <- Medoids;
    TypicWei <- Weights[Clara$i.med];
    
    Clara$Classes <- as.numeric(Clara$clustering);
    Clara$Freq    <- FreqTable(Clara$Classes);
    Clara$IndMeds <- Clara$i.med;
    
    # As the results of clara clustering may be different with manual clustering:
    Summary  <- Manual_Clustering(DataX,Medoids,Weights=Weights,DoStand=F,SortProbs=is.null(SuppData),SuppData=SuppData);
    
    #Classes2 <- Clara$Classes
    #for (i in 1:nrow(Clara$Freq)) {
    #  Classes2[Clara$Classes==Clara$Freq[i,1]] <- i
    #  Clara$Freq[i,1] <- i
    #}
    
    Classes  <- Summary$Classes; #Classes2; #
    Freq     <- Summary$Freq; #Clara$Freq; #
    Medoids  <- Summary$Centers; #TypicPat; #
    TypicPat <- TypicPat[Summary$OrigInd,]; #
    TypicWei <- TypicWei[Summary$OrigInd]; #
    Clara$IndMeds <- Clara$IndMeds[Summary$OrigInd]; #
  }
  
  rownames(TypicPat) <- paste0(Pref,Freq[,1]);
  colnames(TypicPat) <- colnames(Data);
  
  temp       <- Calc_Average_Cluster_Patterns(Data,Freq,Classes,Weights,XAxis);
  Averages   <- temp$Averages;
  AveragesW  <- temp$AveragesW;
  AveragesWD <- temp$AveragesWD;
  AveragesXA <- temp$AveragesXA;
  
  Out <- list(K=K,SampNum=SampNum,Summary=Summary,TypicPat=TypicPat,TypicWei=TypicWei,Averages=round(Averages,3),
              AveragesW=AveragesW,AveragesWD=AveragesWD,Medoids=Medoids,Freq=Freq,Classes=Classes,Clara=Clara,XAxis=AveragesXA,DoStand=DoStand);
  return(Out);
}

#=================================================
#=================================================
#=================================================

Calc_Average_Cluster_Patterns <- function(Data,Freq,Classes,Weights=NULL,Pref='ET',XAxis=NULL,Inds=NULL) {
  
  if (is.null(Inds)) Inds <- 1:nrow(Data)
  
  if (is.null(Weights))   Weights <- matrix(rep(1,nrow(Data)),ncol=1);
  if (length(Weights)==1) Weights <- rep(Weights,nrow(Data));
  
  Averages <- AveragesW <- AveragesWD <- AveragesXA <- Medians <- matrix(NA,nrow=nrow(Freq),ncol=ncol(Data));
  rownames(Averages) <- rownames(AveragesW) <- rownames(AveragesWD) <- rownames(AveragesXA) <- rownames(Medians) <- paste0(Pref,Freq[,1]);
  colnames(Averages) <- colnames(AveragesW) <- colnames(AveragesWD) <- colnames(AveragesXA) <- colnames(Medians) <- colnames(Data);
  
  Data <- Data[Inds,]
  Classes <- Classes[Inds]
  Weights <- Weights[Inds]
  
  for (i in 1:nrow(Freq)) {
    data    <- as.matrix(Data[which(Classes==Freq[i,1]),])
    weights <- Weights[which(Classes==Freq[i,1])];
    
    Averages[i,]   <- round(colMeans(data,na.rm=T),3)
    Dist2Ave       <- Dist2Center(data,Averages[i,],Weights=NULL);
    Dist2Ave       <- nrow(data)*MakeUnitSum(1/Dist2Ave);
    AveragesW[i,]  <- apply(data,2,WMean,Weights=1*weights,Digits=3);
    AveragesWD[i,] <- apply(data,2,WMean,Weights=Dist2Ave*weights,Digits=3);
    
    Medians[i,]   <- NA;
    for (j in 1:ncol(data)) {
      if (any(!is.na(data[,j]))) Medians[i,j] <- quantile(data[,j],probs=0.5,na.rm=TRUE);
    }
  }
  
  if (!is.null(XAxis)) {
    xaxis          <- XAxis[which(Classes==Freq[i,1]),];
    AveragesXA[i,] <- apply(xaxis,2,WMean,Weights=Dist2Ave*weights,Digits=3);
  }
  else AveragesXA <- NULL;
  
  Out <- list();
  Out$Averages   <- Averages;
  Out$AveragesW  <- AveragesW;
  Out$AveragesWD <- AveragesWD;
  Out$AveragesXA <- AveragesXA;
  Out$Medians   <- Medians;
  return(Out);
}

#=================================================
#=================================================
#=================================================

Find_Multiple_Patterns_in_Strings <- function(Strings,Patterns) {
  if (length(Strings)==1 && dir.exists(Strings)) Strings <- list.files(Strings,include.dirs=T)
  Strings2 <- Strings[grep(paste(Patterns,collapse="|"),Strings)]
  return(Strings2)
}

#=================================================
#=================================================
#=================================================

Reshape_Table_to_Long <- function(Table,StatCols=NULL,VarCols=NULL,VarName='Var',ValName='Value',RemoveOtherCols=F) {
  # StatCols: columns which will be repeated
  # VarCols: columns which will be moved to rows
  # RemoveOtherCols: should other columns be removed?
  
  if (is.null(StatCols)) StatCols <- colnames(Table)[!(colnames(Table) %in% VarCols)]
  if (is.null(VarCols)) VarCols <- colnames(Table)[!(colnames(Table)) %in% StatCols]
  if (RemoveOtherCols) Table <- Table[,c(StatCols,VarCols)]
  
  if (is.null(StatCols)) xStatCols <- 1
  else xStatCols <- StatCols
  
  x <- Table[rep(seq_len(nrow(Table)),length(VarCols)),xStatCols]
  y <- do.call('c',as.list(Table[,VarCols]))
  z <- rep(VarCols,each=nrow(Table))
  y <- cbind.data.frame(z,y)
  colnames(y) <- c(VarName,ValName)
  y <- cbind.data.frame(x,y)
  colnames(y)[1:length(xStatCols)] <- xStatCols
  rownames(y) <- NULL
  
  if (is.null(StatCols)) y[,1] <- NULL
  
  return(y)
}



#=================================================
#=================================================
#=================================================

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=0.95, .drop=TRUE) {
  
  ## Summarizes data.
  ##   Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- tapply(data[,measurevar], data[,groupvars], function(xx) {print(xx)
    c(N    = length2(xx, na.rm=na.rm),
      mean = mean   (xx, na.rm=na.rm),
      sd   = sd     (xx, na.rm=na.rm))
  },simplify=T)
  datac <- do.call(rbind,datac)
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + 0.5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#=================================================
#=================================================
#=================================================

Intersect_of_Tables <- function(List,IDCols=NULL,OnCols=F,BaseIndex=NULL,Type='Drop',DoRbind=F,ToDT=F) {
  
  ToBase <- F
  if (!is.null(BaseIndex)) ToBase <- T
  
  if (OnCols) {
    # On columns ---
    IDs <- list()
    for (i in 1:length(List)) {
      List[[i]] <- as.data.frame(List[[i]])
      IDs[[i]] <- colnames(List[[i]])
    }
    
  } else {
    # On rows ---
    if (is.null(IDCols)) IDCols <- colnames(List[[1]])
    IDs <- list()
    for (i in 1:length(List)) {
      List[[i]] <- as.data.frame(List[[i]])
      if (length(IDCols)>1) IDs[[i]] <- apply(List[[i]][,IDCols],1,paste,collapse="-") else 
        IDs[[i]] <- List[[i]][,IDCols]
      rownames(List[[i]]) <- IDs[[i]]
    }
  }
  
  List2 <- List
  if (Type=='Drop') {
    if (ToBase) {
      bIDs <- IDs[[BaseIndex]]
      for (i in 1:length(List)) {
        IDs[[i]] <- intersect(bIDs,IDs[[i]])
      }
      
    } else {
      xIDs <- IDs[[1]]
      for (i in 2:length(List)) {
        xIDs <- intersect(xIDs,IDs[[i]])
      }
    }
    
    for (i in 1:length(List)) {
      if (OnCols) {
        # On columns ---
        if (ToBase) List2[[i]] <- List[[i]][,IDs[[i]]]
        else List2[[i]] <- List[[i]][,xIDs]
        
      } else {
        # On rows ---
        if (ToBase) List2[[i]] <- List[[i]][IDs[[i]],]
        else List2[[i]] <- List[[i]][xIDs,]
      }
    }
    
  } else if (Type=='Add') {
    if (OnCols) {
      # On columns ---
      xIDs <- IDs[[1]]
      for (i in 2:length(List)) {
        xIDs <- union(xIDs,IDs[[i]])
      }
      for (i in 1:length(List)) {
        xCols <- xIDs[!(xIDs%in%IDs[[i]])]
        if (length(xCols)>0) {
          List2[[i]][,xCols] <- NA
        }
        List2[[i]] <- List2[[i]][,xIDs]
      }
      
    } else {
      # On rows ---
      stop('Not defined!!')
    }
  }
  if (ToDT) {
    if (DoRbind) {
      List2 <- rbindlist(List2,fill=T)
      rownames(List2) <- NULL
    } else {
      for (i in 1:length(List2)) List2[[i]] <- data.table(List2[[i]])
    }
  } else {
    if (DoRbind) {
      List2 <- do.call(rbind.data.frame,List2)
      rownames(List2) <- NULL
    }
  }
  
  return(List2)
}

#=================================================
#=================================================
#=================================================

RbindFill <- function(Table1,Table2) {
  if ((is.null(Table1) || nrow(Table1)==0) && (!is.null(Table2) && nrow(Table2)>0)) {
    Out <- Table2
  } else if ((is.null(Table2) || nrow(Table2)==0) && (!is.null(Table1) && nrow(Table1)>0)) {
    Out <- Table1
  } else if (identical(sort(colnames(Table1)),sort(colnames(Table2)))) {
    Out <- rbind(Table1,Table2)
  } else {
    Out <- Intersect_of_Tables(list(Table1,Table2),OnCols=T,BaseIndex=NULL,Type='Add',DoRbind=T,ToDT=T)
  }
  return(Out)
}

#=================================================
#=================================================
#=================================================

OrderTableMeans <- function(Table,Keys,Col,ReturnLevels=T) {
  Table2 <- data.table(Table)
  Table2 <- Table2[, lapply(.SD,mean,na.rm=T), keyby=Keys, .SDcols=Col]
  Table2 <- Table2[order(get(Col))]
  
  if (ReturnLevels) {
    Out <- Table2[, as.character(get(Keys[length(Keys)]))]
    
  } else {
    Out <- Table2
    class(Out) <- class(Table)
  }
  return(Out)
}

#=================================================
#=================================================
#=================================================

OutlierThr <- function(X,pMin=0.25,pMax=0.75,Coef=1.5) {
  xMin <- quantile(X,pMin,na.rm=T)-Coef*IQR(X,na.rm=T)
  xMax <- quantile(X,pMax,na.rm=T)+Coef*IQR(X,na.rm=T)
  return(c(xMin,xMax))
}

NaOutliers <- function(X) {
  Thr <- OutlierThr(X,pMin=0.25,pMax=0.75,Coef=1.5)
  Rows <- which(X<Thr[1]|X>Thr[2])
  if (length(Rows)>0) X[Rows] <- NA
  return(X)
}

#=================================================
#=================================================
#=================================================

Fill_by_Outputs <- function(Data) {
  if (!is.list(Data)) stop('Data must be a list!')
  N <- max(sapply(Data,length,simplify=T))
  for (i in 1:length(Data)) if (is.null(Data[[i]])) Data[[i]] <- rep(NA,N)
  return(Data)
}

#=================================================
#=================================================
#=================================================

Repeat_Table <- function(Table,N) {
  if (!is.matrix(Table) || !is.data.frame(Table)) Table <- data.frame(Table)
  X <- do.call("rbind",replicate(N,Table,simplify=FALSE))
  rownames(X) <- NULL
  return(as.data.frame(X))
}

#=================================================
#=================================================
#=================================================

ConvertUnits <- function(Table,Cols,ConvFacts) {
  if (length(Cols)!=length(ConvFacts)) stop('Cols and ConvFacts must be of the same size!')
  if (is.list(Table) && !is.data.frame(Table)) {
    Table <- sapply(Table,ConvertUnits,Cols,ConvFacts)
    return(Table)
  }
  Table[,Cols] <- Table[,Cols] / matrix(ConvFacts,nrow=nrow(Table),ncol=length(ConvFacts),byrow=T)
  return(Table)
}

#=================================================
#=================================================
#=================================================

CalcBoxRang_without_NA <- function(Data,xCol,yCol,Facets,Margines,pLow=0.25,pHigh=0.75,Coef=1.5) {
  Data <- as.data.frame(Data)
  FUNC <- function(x){
    if (length(Coef)==2) {
      Temp1 <- quantile(x,Coef,na.rm=T)
    } else {
      Temp1 <- quantile(x,c(pLow,pHigh),na.rm=T)+c(-1,1)*Coef*IQR(x,na.rm=T)
    }
    Temp2 <- rbind(Temp1,range(x,na.rm=T))
    return(c(max(Temp2[,1]),min(Temp2[,2])))
  }
  
  if ((is.null(Facets)) || (!is.null(Facets) && Margines==T)) {
    if (length(xCol)==1) X <- list(Data[,xCol])
    else X <- as.list(Data[,xCol])
    Temp1 <- stats::aggregate(Data[,yCol],X,FUNC)
    Temp1 <- Temp1[,-(1:length(xCol))]
    Temp1 <- c(min(Temp1[,1]),max(Temp1[,2]))
    Temp1 <- unlist(Temp1)
  } else {
    Temp1 <- NULL
  }
  
  if (!is.null(Facets)) {
    Temp2 <- stats::aggregate(Data[,yCol],as.list(Data[,c(Facets,xCol)]),FUNC)
    Temp2 <- Temp2[,(length(c(Facets,xCol))+1):ncol(Temp2)]
    Temp2 <- c(min(Temp2[,1],na.rm=T),max(Temp2[,2],na.rm=T))
    Temp2 <- unlist(Temp2)
    Temp1 <- rbind(Temp1,Temp2)
    
    if (Margines==Facets[1]) {
      Temp3 <- stats::aggregate(Data[,yCol],as.list(Data[,c(Facets[-1],xCol)]),FUNC)
      Temp3 <- Temp3$x
      Temp3 <- c(min(Temp3[,1],na.rm=T),max(Temp3[,2],na.rm=T))
      Temp3 <- unlist(Temp3)
      Temp1 <- rbind(Temp1,Temp3)
    }
    
    if (length(Facets)>1 && Margines==Facets[2]) {
      Temp4 <- stats::aggregate(Data[,yCol],as.list(Data[,c(Facets[-2],xCol)]),FUNC)
      Temp4 <- Temp4$x
      Temp4 <- c(min(Temp4[,1],na.rm=T),max(Temp4[,2],na.rm=T))
      Temp4 <- unlist(Temp4)
      Temp1 <- rbind(Temp1,Temp4)
    }
    
    if (Margines==T) {
      Temp5 <- stats::aggregate(Data[,yCol],list(Data[,c(xCol)]),FUNC)
      Temp5 <- Temp5$x
      Temp5 <- c(min(Temp5[,1],na.rm=T),max(Temp5[,2],na.rm=T))
      Temp5 <- unlist(Temp5)
      Temp1 <- rbind(Temp1,Temp5)
    }
  }
  Temp1 <- matrix(Temp1,ncol=2,byrow=F)
  yRange <- c(min(Temp1[,1],na.rm=T),max(Temp1[,2],na.rm=T))
  return(yRange)
}

#=================================================
#=================================================
#=================================================

Sort_Factors <- function(Data,Col,PutLast='All',PutFirst=NULL,isNum=F,Desc=F) {
  if (is.data.table(Data)) {
    isDT <- T
    Data <- as.data.frame(Data)
  }
  if (length(Col)>1) {
    for (C in Col) Data <- Sort_Factors(Data,Col=C,PutLast=PutLast,PutFirst=PutFirst,isNum=isNum)
    return(Data)
  }
  
  if (isNum) {
    Levels <- sort(unique(as.numeric(as.character(Data[,Col]))),decreasing=Desc)
    Data[,Col] <- Factor2Numeric(Data[,Col])
    Levels[is.na(Levels)] <- NULL
  }
  else {
    Levels <- sort(unique(as.character(Data[,Col])),decreasing=Desc)
  }
  
  if (!is.null(PutLast)) Levels <- c(Levels[Levels!=PutLast],PutLast)
  if (!is.null(PutFirst)) Levels <- c(PutLast,Levels[Levels!=PutLast])
  Data[,Col] <- factor(Data[,Col],levels=as.character(Levels))
  if (isDT) Data <- as.data.table(Data)
  return(Data)
}

#=================================================
#=================================================
#=================================================

Add_Key_DT <- function(Table,Cols,KeyCol='Key',Sep='-',MakeRowNames=F,JustKeepKey=F) {
  if (KeyCol %in% names(Table)) Table[,(KeyCol):=NULL]
  if (JustKeepKey) Table <- Table[,(Cols)]
  Table[, (KeyCol):=do.call(paste,c(.SD,sep=Sep)), .SDcols=Cols]
  if (JustKeepKey) Table <- Table[,(KeyCol)]
  if (MakeRowNames) rownames(Table) <- Table[,(KeyCol)]
  return(Table)
}

#=================================================
#=================================================
#=================================================

Add_Key <- function(Table,Cols,KeyCol='Key',Sep='-',MakeRowNames=F,JustKeepKey=F,ToDT=F) {
  if (KeyCol %in% names(Table)) Table[,KeyCol] <- NULL
  Table <- as.data.frame(Table)
  if (JustKeepKey) Table <- Table[,Cols]
  Table[,KeyCol] <- apply(Table[,Cols],1,paste,collapse=Sep)
  if (JustKeepKey) Table <- Table[,KeyCol]
  if (MakeRowNames) rownames(Table) <- Table[,KeyCol]
  if (ToDT) Table <- data.table(Table)
  return(Table)
}

#=================================================
#=================================================
#=================================================

by_to_DataFrame <- function(byOutput,LevColName=NULL,ColName=NULL) {
  Dims <- expand.grid(dimnames(byOutput))
  Output <- list()
  
  if (is.null(LevColName)) LevColName <- colnames(Dims)
  if (length(LevColName)==1 && LevColName=='Table[, LevCols]') LevColName <- 'Levels'
  
  n <- 1
  for (i in 1:nrow(Dims)) {
    if (!is.null(byOutput[[i]])) {
      AddCol <- Dims[i,]
      rownames(AddCol) <- NULL
      Output[[n]] <- cbind.data.frame(AddCol,byOutput[[i]])
      n <- n + 1
    }
  }
  Output <- do.call(rbind.data.frame,Output)
  
  if (!is.null(ColName)) colnames(Output)[ncol(Output)] <- ColName
  colnames(Output)[1:length(LevColName)] <- LevColName
  return(Output)
}

#=================================================
#=================================================
#=================================================

MySort <- function(Data,AscCols=NULL,DescCols=NULL,ToDT=F) {
  Data <- as.data.frame(Data)
  if (is.null(AscCols) && is.null(DescCols)) AscCols <- names(Data)
  n <- ncol(Data)
  X <- NULL
  if (!is.null(AscCols)) X <- c(X,data.frame(Data[,AscCols]))
  if (!is.null(DescCols)) X <- c(X,data.frame(rev(Data[,DescCols])))
  Order <- do.call(order,X)
  Data <- Data[Order,]
  rownames(Data) <- NULL
  if (ToDT) Data <- data.table(Data)
  return(Data)
}

#=================================================
#=================================================
#=================================================

List_RBind <- function(List,RemRowNames=T,AddNames=F,ColName='ListName',ToDT=T) {
  if (ToDT) {
    if (AddNames) {
      for (i in names(List)) {List[[i]] <- data.table(List[[i]]); List[[i]][, (ColName):=i]}
    }
    List <- rbindlist(List,fill=T)
  } else {
    if (AddNames) {
      for (i in 1:length(List)) List[[i]]$ListName <- names(List)[i]
    }
    List <- as.data.frame(do.call(rbind,List))
    if (RemRowNames) rownames(List) <- NULL
  }
  return(List)
}

#=================================================
#=================================================
#=================================================

MyBy <- function(Data,Indices,Func,AddLevels=T,SortCols=NULL,...) {
  
  if (is.null(dim(Data))) Data <- data.frame(Data)
  if (is.null(names(Indices))) Indices <- data.frame(Indices)
  Indices <- as.data.frame(Indices)
  
  FactCols <- colnames(Indices)
  Factors  <- as.data.frame(apply(Indices,2,as.character),stringsAsFactors=F)
  Factors  <- apply(Factors,1,paste,collapse='_-')
  Levels   <- unique(Factors)
  
  List <- by(Data,Factors,Func,...,simplify=F)
  List <- List_RBind(List,RemRowNames=T,AddNames=T)
  
  if (AddLevels) {
    Temp   <- cbind.data.frame(Levels=Levels,unique(Indices))
    Names1 <- names(Temp)[names(Temp)!='Levels']
    Names2 <- names(List)
    Temp <- Temp[,!names(Temp)%in%names(List)]
    if (!is.null(dim(Temp))) List <- merge(List,Temp,by.x='ListName',by.y='Levels',sort=F)
    List <- List[,c(Names1,Names2)]
  }
  List <- subset(List,select=-ListName)
  if (!is.null(SortCols)) List <- MySort(List,AscCols=SortCols,DescCols=NULL)
  return(List)
}

#=================================================
#=================================================
#=================================================

MyRound <- function(Numbers,Digits,Up=T) {
  Digits <- 1/(10^Digits)
  if (Up) {
    Numbers <- ceiling(Numbers/Digits)*Digits
  } else {
    Numbers <- floor(Numbers/Digits)*Digits
  }
  return(Numbers)
}

#=================================================
#=================================================
#=================================================

Round_Bin_Digits <- function(Range,MidPoint=NULL,IsInt=F,CalcRange=T) {
  if (CalcRange) Range <- range(Range,na.rm=T)
  if (!is.null(MidPoint) && In_Interval(MidPoint,Range,WithEqual=F)) Range <- c(MidPoint,max(abs(Range),na.rm=T))
  Bin <- as.numeric(diff(Range))
  Bin <- (1-IsInt)*max(0,round(-log10(Bin)+1.2))
  return(Bin)
}

#=================================================
#=================================================
#=================================================

Is_Int <- function(Data) {
  Data <- Data[!is.na(Data)]
  if (all(Data==round(Data,0))) return(T)
  else return(F)
}

#=================================================
#=================================================
#=================================================

In_Interval <- function(x,Interval,WithEqual=T){
  x <- data.table::between(x,Interval[1],Interval[2],incbounds=WithEqual)
  return(x)
}

#=================================================
#=================================================
#=================================================

Change_All_Years <- function(Dates,NewYear) {
  Dates <- as.Date(format(as.Date(Dates,origin='1970-01-01'),format=paste0('%',NewYear,'-%m-%d')))
  return(Dates)
}

#=================================================
#=================================================
#=================================================

as.percent <- function(x,Digits=2) {
  return(round(x*100,Digits))
}

#=================================================
#=================================================
#=================================================

Add_Aggregate_Unit <- function(Data,Col,AggUnit='All',ToDT=F,PutLast=T,AsFactor=F) {
  if (any(!Col %in% names(Data))) stop(sprintf('"%s" is not a column in the data!',paste(setdiff(names(Data),Col),collapse=', ')))
  if (length(AggUnit)==1) AggUnit <- rep(AggUnit,length(Col))
  
  if ('data.table'%in%class(Data)) {
    for (Cl in Col) {
      Levels <- levels(Data[,get(Cl)])
      if (is.null(Levels)) Levels <- sort(unique(as.character(Data[,get(Cl)])))
      Data2 <- copy(Data)
      AggUnitX <- AggUnit[Col==Cl]
      Data2[,(Cl):=AggUnitX]
      Data <- rbind(Data,Data2)
      if (PutLast) Levels <- unique(c(Levels,AggUnit[Col==Cl])) else Levels <- unique(c(AggUnit[Col==Cl],Levels))
      if (PutLast || AsFactor) Data[,(Cl):=factor(as.character(get(Cl)),levels=as.character(Levels))]
    }
    
  } else {
    for (Cl in Col) {
      Data2 <- as.data.frame(Data)
      Data2[,Cl] <- AggUnit[Col==Cl]
      Data <- rbind(Data,Data2)
      if (PutLast) {
        Levels <- sort(unique(as.character(Data[,Cl])))
        Levels <- c(Levels[Levels!=AggUnit[Col==Cl]],AggUnit[Col==Cl])
        Data[,Cl] <- factor(as.character(Data[,Cl]),levels=as.character(Levels))
      }
    }
    if (ToDT) Data <- data.table(Data)
  }
  return(Data)
}

#=================================================
#=================================================
#=================================================

Change_Factor_Levels <- function(Data,Col,From,To,ToDT=T) {
  xClass <- class(Data)
  Data <- data.table(Data)
  Data[, (Col):=as.character(get(Col))]
  for (j in 1:length(From)) Data[get(Col)==From[j],(Col):=To[j]]
  Data[, (Col):=factor(as.character(get(Col)),levels=To)]
  if (!ToDT)  class(Data) <- xClass
  return(Data)
}

#=================================================
#=================================================
#=================================================

PackageTest <- function(Packages,Load=T) {
  for (x in Packages) {
    if (!require(x,character.only=TRUE))
    {
      install.packages(x,dep=TRUE)
    }
    if (Load) x <- library(x,character.only=T,verbose=F,quietly=T)
  }
}

#=================================================
#=================================================
#=================================================

Find_Inside_Points <- function(MapDF,MXCol,MYCol,Points,PXCol,PYCol,Buffer=0,ToDT=F,ExpId=F) {
  Points <- as.data.frame(Points)
  Points <- Points[,c(PXCol,PYCol)]
  
  if (Buffer!=0) {
    Points1 <- Points2 <- Points3 <- Points4 <- Points
    Points1[,PXCol] <- Points[,PXCol] - Buffer
    Points2[,PXCol] <- Points[,PXCol] + Buffer
    Points3[,PYCol] <- Points[,PYCol] - Buffer
    Points4[,PYCol] <- Points[,PYCol] + Buffer
    Points2 <- rbind(Points,Points1,Points2,Points3,Points4)
    
    x <- point.in.polygon(Points2[,PXCol], Points2[,PYCol], MapDF[,MXCol], MapDF[,MYCol])
    x <- matrix(x,ncol=5,byrow=F)
    x <- apply(x,1,sum)
    if (ExpId==T) Out <- which(x>0) else Out <- Points[x>0,]
    
  } else {
    x <- point.in.polygon(Points[,PXCol], Points[,PYCol], MapDF[,MXCol], MapDF[,MYCol])
    if (ExpId==T) Out <- which(x==1) else Out <- Points[x==1,]
  }
  
  if (ToDT) Out <- data.table(Out)
  return(Out)
}

#=================================================
#=================================================
#=================================================

Regrid <- function(Grid,XYCols=c('Long','Lat'),NewCellSize,Buffer=0,MapDF=NULL,MXCol=NULL,MYCol=NULL) {
  NS <- NewCellSize
  x <- Grid[,XYCols]
  X <- range(x[,1])+c(-1,1)*Buffer
  Y <- range(x[,2])+c(-1,1)*Buffer
  Xs <- seq(X[1],X[2],NS)
  Ys <- seq(Y[1],Y[2],NS)
  if (Xs[length(Xs)]<X[2]) Xs <- c(Xs,Xs[length(Xs)]+NS)
  if (Ys[length(Ys)]<Y[2]) Ys <- c(Ys,Ys[length(Ys)]+NS)
  NewGrid <- expand.grid(Xs,Ys)
  names(NewGrid) <- XYCols
  if (!is.null(Polygon)) {
    NewGrid <- Find_Inside_Points(MapDF,MXCol,MYCol,NewGrid,PXCol=XYCols[1],PYCol=XYCols[2],Buffer=NS/2)
  }
  return(NewGrid)
}

#=================================================
#=================================================
#=================================================

PrettyBreaks <- function(Range,NumBins=5,MidPoint=NULL,Digits=2,LessDigits=NULL) {
  IsSymm <- F
  if (is.null(LessDigits)) { if (Digits<2) LessDigits <- 0 else LessDigits <- 1 }
  
  if (!is.null(MidPoint) && In_Interval(MidPoint,Range,WithEqual=F)) {
    Length <- max(abs(Range-MidPoint))
    IsSymm <- T
    NumBins <- floor(NumBins/2)
  } else {
    Length <- as.numeric(diff(Range))
  }
  
  Ints <- MyRound(Length/NumBins,Digits=Digits,Up=T)
  if (NumBins - MyRound(Length/Ints,Digits=0,Up=T) > LessDigits) {
    Digits <- Digits + 1
    Ints <- MyRound(Length/NumBins,Digits=Digits,Up=T)
  }
  NumBins <- MyRound(Length/Ints,Digits=0,Up=T)
  
  Range2 <- (0:NumBins)*Ints
  if (IsSymm) {
    Breaks <- MidPoint + Range2
    Breaks <- unique(c(-Breaks[Breaks!=MidPoint],MidPoint,Breaks))
  } else {
    Breaks <- Range[1] + Range2
  }
  Breaks <- sort(Breaks)
  return(list(Breaks=Breaks,Digits=Digits))
}

#=================================================
#=================================================
#=================================================

MyTTest <- function(Data,SharedLevels,ClassCol,Classes,TimeCol,MainCol) {
  # Example:
  # x <- MyTTest(Data=IrData,SharedLevels=c('Var','Lat','Long','Mon'),TimeCol='Year',
  # ClassCol='Period',Classes=c('1957-1986','1987-2016'),MainCol='Value')
  
  if (!is.null(Classes)) {
    Temp <- Data[get(ClassCol)%in%Classes] 
    
  } else {
    Classes <- unique(Data[,get(ClassCol)])
    Temp <- data.table(Data)
  }
  
  if (length(MainCol)>1) {
    Temp <- data.table::melt(Temp,id.vars=c(SharedLevels,ClassCol,TimeCol),measure.vars=MainCol,variable.name='TestVar',value.name='TestVarValue')
    MainCol <- 'TestVarValue'
    SharedLevels <- c(SharedLevels,'TestVar')
  }
  if (!is.null(TimeCol)) setkeyv(Temp,cols=c(SharedLevels,ClassCol,TimeCol))
  
  Formula <- paste(paste(c(SharedLevels,TimeCol),collapse='+'),'~',ClassCol)
  Temp <- dcast(Temp,Formula,value.var=MainCol,verbose=F)
  
  FUNC <- function(x) {
    Flag <- F
    if (sum(!is.na(x[[1]]))<2 || sum(!is.na(x[[2]]))<2 || nrow(unique(x))<2) Flag <- T
    
    if (Flag) {
      x1 <- mean(x[[1]],na.rm=T); if (is.na(x1) || is.nan(x1)) x1 <- NA_real_
      x2 <- mean(x[[2]],na.rm=T); if (is.na(x2) || is.nan(x2)) x2 <- NA_real_
      x <- data.table(x1,x2,x2-x1,round(100*(x2-x1)/x1,2),NA_real_)
      
    } else {
      x <- t.test(x[,1],x[,2],alternative='two.sided')
      x <- data.table(unlist(x$estimate[1]),unlist(x$estimate[2]),unlist(x$estimate[2])-unlist(x$estimate[1]),
                      round(100*(unlist(x$estimate[2])-unlist(x$estimate[1]))/unlist(x$estimate[1]),2),round(x$p.value,4))
      if (!is.finite(x[[5]]) || is.nan(!is.finite(x[[5]]))) x[,5] <- 1
    }
    colnames(x) <- c(Classes[1],Classes[2],'Diff','DiffRel','P')
    return(x)
  }
  
  Temp <- Temp[, FUNC(.SD), keyby=SharedLevels, .SDcols=Classes]
  return(Temp)
}

#=================================================
#=================================================
#=================================================

Print_Decimals <- function(x,Num=2,AddSign=F) {
  y <- format(round(x,Num),nsmall=Num)
  if (AddSign && round(x,Num)>=0) y <- paste0('+',y)
  return(y)
}

#=================================================
#=================================================
#=================================================

Select_Shared_Rows <- function(Data1,Data2,Cols1,Cols2) {
  if (nrow(Data1)>nrow(Data2)) {
    Temp1 <- Data2
    Temp2 <- Data1
    CX1 <- Cols2
    CX2 <- Cols1
    
  } else {
    Temp1 <- Data1
    Temp2 <- Data2
    CX1 <- Cols1
    CX2 <- Cols2
  }
  Out <- merge(Temp1,Temp2,by.x=CX1,by.y=CX2,all.x=T,sort=F)
}

#=================================================
#=================================================
#=================================================

Change_Levels <- function(Vector,OldLevs,NewLevs,AsFactor=F) {
  Vector2 <- as.character(Vector)
  for (Lev in OldLevs) Vector2[as.character(Vector)==Lev] <- NewLevs[OldLevs==Lev]
  NewLevs <- NewLevs[NewLevs%in%Vector2]
  if (AsFactor && length(NewLevs)>0) Vector2 <- factor(as.character(Vector2),levels=NewLevs)
  return(Vector2)
}

#=================================================
#=================================================
#=================================================

Charact_to_Date <- Char2Date <- function(Vector,Format='%d-%b',Year=NULL) {
  if (!is.null(Year)) {
    Vector <- as.Date(paste(Vector,Year,sep='-'),format=paste0(Format,'-%Y'))
  } else {
    Vector <- as.Date(Vector,format=Format)
  }
  return(Vector)
}

#=================================================
#=================================================
#=================================================

MySplit <- function(Vector,NGroups=5,Which=NULL) {
  Out <- split(Vector,ceiling(seq_along(Vector)/ceiling(length(Vector)/NGroups)))
  if (!is.null(Which)) {
    if (Which>length(Out)) Out <- NULL else Out <- Out[[Which]]
  }
  return(Out)
}

#=================================================
#=================================================
#=================================================

RenameCols <- Mapping_Cols <- function(Table, Mapping) {
  ColNames <- colnames(Table)
  for (i in 1:nrow(Mapping)) {
    Old <- as.character(Mapping[i,1])
    New <- as.character(Mapping[i,2])
    colnames(Table)[ColNames==Old] <- New
  }
  return(Table)
}

#=================================================
#=================================================
#=================================================

Replace_in_Table <- function(Table,What,With) {
  Table <- data.frame(lapply(Table,function(x){x[x==What]<-With;return(x)}),check.names=F)
  return(Table)
}

#=================================================
#=================================================
#=================================================

IsDate <- function(x,Format='%Y-%m-%d') {
  return(!is.na(as.Date(x,format=Format)))
}

#=================================================
#=================================================
#=================================================

PVal2Letters <- function(PMatrix,PThr=0.1) {
  Cols1 <- colnames(PMatrix)
  Cols2 <- rownames(PMatrix)
  Cols  <- unique(c(Cols1,Cols2))
  PMatrix <- data.table(PMatrix)
  PLeters <- data.table(Names=Cols,Letters=character(length(Cols)))
  iLetter <- 0
  Groups  <- list()
  for (Col in Cols) {
    AddNew <- T
    if (Col%in%Cols1) {
      Rows <- PMatrix[get(Col)>=PThr, which=T]
      Rows <- Cols2[Rows]
      if (length(Rows)>0) {
        NewGroup <- PLeters[Names%in%c(Rows,Col),Names]
        for (G in Groups) if (all(NewGroup%in%G)) {AddNew <- F; break}
        if (AddNew) {
          Groups[[Col]] <- NewGroup
          iLetter <- iLetter + 1
          PLeters[Names%in%c(Rows,Col), Letters:=paste0(Letters,letters[iLetter])]
        }
      }
    }
  }
  return(PLeters)
}

#           May        Jun         Jul         Aug
# Jun 1.0000000000         NA          NA          NA
# Jul 0.0002931151 0.10225483          NA          NA
# Aug 0.0001949061 0.08312222 1.000000000          NA
# Sep 1.0000000000 1.00000000 0.006969712 0.004847635

#=================================================
#=================================================
#=================================================

Select_Files_in_Groups <- function(Dir,MainPattern,Ext,NJobs,JobID) {
  Files <- List_Files_by_Ext_and_Pattern(Dir,Pattern=MainPattern,Ext=Ext)
  if (!is.null(Pattern)) {
    ExcInd <- grep(Pattern,Files)
    if (length(ExcInd)>0) Files <- Files[-ExcInd]
  }
  Files <- MySplit(Files,NGroups=NJobs,Which=JobID)
  return(Files)
}

#=================================================
#=================================================
#=================================================

FindInJason <- function(File,Attr='Name',Map='Simulations:APAD:MySim',NewValue='TestXXX') {
  library(rjson)
  
  Data <- rjson::fromJSON(file=File)
  Map <- str_split(Map,pattern=':')[[1]]
  
  Target <- Data
  Comm <- 'Data$Children'
  
  for (Name in Map[-1]) {
    Target <- Target$Children
    i <- 0
    for (Name2 in Target) {
      i <- i + 1
      if (Name2[[Attr]]==Name) {
        Target <- Name2
        if (Name!=Map[length(Map)]) Comm <- paste0(Comm,'[[',i,']]$Children')
        break
      }
    }
  }
  
  #Target[[Attr]] <- NewValue  
  #Comm <- paste0(Comm,'[[length(',Comm,')+1]] <- Target')
  #print(Comm)
  #eval(parse(text=Comm))
  x <- rjson::toJSON(Data,indent=5)
  writeLines(x, "TestJason.apsimx")
  return(Data)
}

#=================================================
#=================================================
#=================================================

Extract_from_File_or_Text <- function(Input,LineStart,LineEnd) {
  if (length(Input)==1) Lines <- readLines(Input,warn=F) else Lines <- Input
  Lines <- Lines[LineStart:LineEnd]
  return(Lines)
}

#=================================================
#=================================================
#=================================================

Add_to_File_or_Text <- function(Input,What,Replace=F,LineStart=NULL,LineEnd,Sep=',',SaveTo=NULL) {
  if (length(Input)==1) Lines <- readLines(Input,warn=F) else Lines <- Input
  if (is.list(What)) {
    if (!is.null(Sep)) {
      for (i in 1:length(What)) {
        Temp <- substr(What[[i]][length(What[[i]])],nchar(What[[i]][length(What[[i]])]),nchar(What[[i]][length(What[[i]])]))
        if (Temp!=Sep) What[[i]][length(What[[i]])] <- paste0(What[[i]][length(What[[i]])],Sep)
      }
    }
    What <- unlist(What)
  } else {
    Temp <- substr(What[length(What)],nchar(What[length(What)]),nchar(What[length(What)]))
    if (Temp!=Sep) What[length(What)] <- paste0(What[length(What)],Sep)
  }
  
  if (!Replace) {
    Lines <- append(Lines,Sep,LineEnd)
    Lines <- append(Lines,What,LineEnd+1)
    
  } else {
    Lines <- Lines[-(LineStart:LineEnd)]
    Lines <- append(Lines,What,LineStart-1)
  }
  if (!is.null(SaveTo)) {
    writeLines(Lines,SaveTo) 
  }
  else return(Lines)
}

#=================================================
#=================================================
#=================================================

MyMerge <- function(x,y,...) {
  x <- data.table(x)
  y <- data.table(y)
  x$xxxxID <- 1:nrow(x)
  out <- merge(x,y,...)
  print(x)
  print(y)
  print(out)
  out <- out[order(xxxxID)]
  print(x)
  print(out)
  out[, xxxxID:=NULL]
  return(out)
}

#=================================================
#=================================================
#=================================================

Eval_Regression <- function(Reg,X,Y) {
  YPred <- predict(Reg,X)
  pval  <- round(cor.test(Y,YPred,use='pairwise.complete.obs')$p.value,digits=3)
  Out <- list()
  Out$Summary <- summary(Reg)
  Out$Stats <- MyCompare(Stats=NULL,Obs=Y,Sims=YPred,Digits=2,Probs=c(0.1,0.9),CompleteCases=F)
  return(Out)
}

#=================================================
#=================================================
#=================================================

ColMins <- function(Data) {
  return(apply(Data,2,min,na.rm=T))
}

ColMaxs <- function(Data) {
  return(apply(Data,2,max,na.rm=T))
}

#=================================================
#=================================================
#=================================================

MySummary <- function(Data,Keys,YCols,Digits=NULL,VarName='Variable',conf.interval=0.95) {
  
  if (is.null(Digits)) Digits <- 3
  
  FUNC <- function(Vector) {
    Vector <- as.numeric(Vector)
    Temp <- as.data.frame(matrix(summary(Vector,digits=Digits,na.rm=T),nrow=1))
    names(Temp) <- c('Min','Q1','Med','Mean','Q3','Max')
    Temp <- cbind.data.frame(Temp,Count=length(Vector),CountNA=sum(is.na(Vector)),
                             CV=CV(Data=Vector,Digits=Digits),
                             STD=Std(Data=Vector,Digits=Digits),
                             SE=SE(Data=Vector,Digits=Digits),
                             CI=ConfInt(Data=Vector,ConfInterval=conf.interval,UseMean=F,Digits=Digits,FullRows=F,na.rm=T))
    Temp <- Temp[,c('Count','CountNA','Min','Q1','Med','Mean','Q3','Max','STD','SE','CI')]
    return(Temp)
  }
  
  if (length(YCols)>1) {
    Data <- data.table::melt(Data,measure.vars=YCols,variable.name=VarName)
    Out <- Data[, FUNC(get('value')), keyby=c(Keys,VarName)]
    
  } else {
    Out <- Data[, FUNC(get(YCols)), keyby=Keys]
  }
  return(Out)
}

#=================================================
#=================================================
#=================================================

CalcAbsRelChanges <- function(Data,Cols,Indices,RefIndices,RefIndicesVal,TakeMean=F,BaseIsRef=T,ReturnFull=T,RemoveBase=F,LongForm=F,PositiveDenominator=F) {
  
  Indices <- unique(c(Indices,RefIndices))
  
  Data <- data.table(Data)[, unique(c(Cols,Indices,RefIndices)), with=F]
  setkeyv(Data,cols=Indices)
  ColsBase <- paste0('Base',Cols)
  ColsRel  <- paste0('Rel',Cols)
  ColsAbs  <- paste0('Abs',Cols)
  ColsDiv  <- paste0('Div',Cols)
  ColsOrd  <- NULL
  for (i in 1:length(Cols)) ColsOrd <- c(ColsOrd,Cols[i],ColsBase[i],ColsRel[i],ColsAbs[i],ColsDiv[i])
  
  if (ColsRel[1]%in%names(Data)) Data[,(ColsRel):=NULL]
  if (ColsAbs[1]%in%names(Data)) Data[,(ColsAbs):=NULL]
  if (ColsDiv[1]%in%names(Data)) Data[,(ColsDiv):=NULL]
  
  if (TakeMean) {
    Data <- Data[, lapply(.SD,mean,na.rm=T), keyby=Indices, .SDcols=Cols]
  }
  TempB <- copy(Data)
  
  for (Ind in RefIndices) TempB <- TempB[get(Ind)==RefIndicesVal[RefIndices==Ind], c(Indices,Cols), with=F]
  TempB <- merge.data.table(Data[,..Indices],TempB[,c(setdiff(Indices,RefIndices),Cols),with=F],by=setdiff(Indices,RefIndices),sort=F,all.x=T)
  TempB <- Mapping_Cols(TempB,data.frame(Old=Cols,New=ColsBase))
  
  TempB1 <- copy(Mapping_Cols(TempB,data.frame(Old=ColsBase,New=ColsRel)))
  if (BaseIsRef) TempB1[, (ColsRel):=round(100*(Data[,Cols,with=F] - TempB1[,ColsRel,with=F]) / if (PositiveDenominator) abs(TempB1[, ColsRel, with=F]) else TempB1[,ColsRel,with=F], 3)]
  if (!BaseIsRef) TempB1[, (ColsRel):=round(100*(TempB1[,ColsRel,with=F] - Data[,Cols,with=F]) / if (PositiveDenominator) abs(Data[,Cols,with=F]) else Data[,Cols,with=F], 3)]
  TempB1 <- cbind(TempB1,TempB[,ColsBase,with=F])
  
  TempB2 <- copy(Mapping_Cols(TempB,data.frame(Old=ColsBase,New=ColsAbs)))
  if ( BaseIsRef) TempB2[, (ColsAbs):=(Data[,Cols,with=F]-TempB2[,ColsAbs,with=F])]
  if (!BaseIsRef) TempB2[, (ColsAbs):=(TempB2[,ColsAbs,with=F]-Data[,Cols,with=F])]
  TempB1 <- cbind(TempB1,TempB2[,..ColsAbs])
  
  TempB2 <- copy(Mapping_Cols(TempB,data.frame(Old=ColsBase,New=ColsDiv)))
  if (BaseIsRef) TempB2[, (ColsDiv):=round(Data[,Cols,with=F] / if (PositiveDenominator) abs(TempB2[,ColsDiv,with=F]) else TempB2[,ColsDiv,with=F], 3)]
  if (!BaseIsRef) TempB2[, (ColsDiv):=round(TempB2[,ColsDiv,with=F] / if (PositiveDenominator) abs(Data[,Cols,with=F]) else Data[,Cols,with=F], 3)]
  TempB1 <- cbind(TempB1,TempB2[,..ColsDiv])
  rm(TempB2); x <- gc()
  
  if (ReturnFull) {
    Data <- merge(Data,TempB1,by=Indices,sort=F)
  } else {
    Data <- TempB1
  }
  
  if (RemoveBase) {
    for (Ind in RefIndices) Data[,(Ind):=as.character(get(Ind))]
    Data <- Data[apply(Data[,RefIndices,with=F],1,paste,collapse='_-')!=paste(RefIndicesVal,collapse='_-')]
  }
  
  if (LongForm) {
    if (ReturnFull) ColsOrd <- setdiff(ColsOrd,Cols)
    Data <- data.table::melt(Data,measure.vars=ColsOrd,variable.name='Variable',value.name='Value')
    
  } else {
    if (!ReturnFull) ColsOrd <- setdiff(ColsOrd,Cols)
    ColsOrd <- c(setdiff(names(Data),ColsOrd),ColsOrd)
    Data <- Data[,ColsOrd,with=F]
  }
  
  FUNC <- function(x) {
    if (any(is.infinite(x))) {
      x[is.infinite(x)] <- 0
    }
    return(x)
  }
  Data <- Data[, lapply(.SD, FUNC), .SDcols=names(Data)]
  return(Data)
}

#=================================================
#=================================================
#=================================================

MyEucDist <- function(m1,m2) {
  mtm <- Matrix::tcrossprod(m1,m2)
  sq <- rowSums(m1*m2)
  out <- sqrt(outer(sq,sq,"+") - 2*mtm)
  return(out)
}

#=================================================
#=================================================
#=================================================

FixedDigits <- function(Value,Digits=2) {
  if (Digits==0) {
    out <- as.character(round(as.numeric(Value),Digits))
  } else {
    out <- sprintf(paste0('%.',Digits,'f'),round(as.numeric(Value),Digits))
  }
  return(out)
}

#=================================================
#=================================================
#=================================================

TTestP <- function(Data,PThr=0.1,Letters=letters) {
  if (is.list(Data)) {
    Names <- names(Data)
    if (is.null(names(Data))) {
      Names <- LETTERS[1:length(Data)]
      names(Data) <- Names
    }
    Means <- sapply(Data,mean,na.rm=T)
    
  } else {
    Data <- data.table(Data)
    if (is.null(colnames(Data))) Names <- LETTERS[1:ncol(Data)] else Names <- colnames(Data)
    Means <- colMeans(Data,na.rm=T)
  }
  
  Means <- sort(Means,decreasing=T)
  Data  <- Data[names(Means)]
  Names <- names(Data)
  
  Table <- matrix(1,nrow=length(Names),ncol=length(Names))
  rownames(Table) <- colnames(Table) <- Names
  
  for (Col1 in 1:(length(Data)-1)) {
    for (Col2 in (Col1+1):length(Data)) {
      x <- na.omit(Data[[Col1]])
      y <- na.omit(Data[[Col2]])
      Flag <- F
      if (length(x)<3) {x <- rnorm(3); Flag <- T}
      if (length(y)<3) {y <- rnorm(3); Flag <- T}
      z <- t.test(x=x,y=y,na.action=na.omit)
      if (Flag==T) Table[Col1,Col2] <- Table[Col2,Col1] <- 1.000
      if (Flag==F) Table[Col1,Col2] <- Table[Col2,Col1] <- z$p.value
    }
  }
  
  groups <- data.frame(Names=Names,groups=multcompLetters(Table,threshold=PThr,Letters=Letters)$Letters)
  Out  <- list(p.valye=Table,groups=groups)
  return(Out)
}


#=================================================
#=================================================
#=================================================

ConfInt <- function(Data,ConfInterval=0.95,UseMean=F,Digits=3,FullRows=F,na.rm=T) {
  Data <- as.data.frame(Data);
  if (FullRows) Data <- Data[complete.cases(Data),];
  
  se <- SE(Data,Digits=Digits,FullRows=FullRows,na.rm=na.rm)
  N  <- Length(Data,na.rm=na.rm)
  ciMult <- qt(ConfInterval/2+0.5,max(1,N-1))
  ci <- ciMult*se
  if (UseMean) ci <- Mean(Data,Digits=Digits,FullRows=FullRows)+c(-1,1)*ci
  return(round(ci,Digits))
}

#=================================================
#=================================================
#=================================================

CrossCIs <- function(Data,Groups,ColX,ColY,CL=0.95) {
  Data1 <- copy(data.table(Data))
  Data1 <- Data1[, (ColX):=mean(get(ColX),na.rm=T), keyby=Groups]
  Data1 <- data.table(Rmisc::summarySE(data=Data1,measurevar=ColY,groupvars=c(Groups,ColX),conf.interval=CL,na.rm=T))
  Data2 <- copy(data.table(Data))
  Data2 <- Data2[, (ColY):=mean(get(ColY),na.rm=T), keyby=Groups]
  Data2 <- data.table(Rmisc::summarySE(data=Data2,measurevar=ColX,groupvars=c(Groups,ColY),conf.interval=CL,na.rm=T))
  Data  <- merge(Data2,Data1,by=c(Groups,ColX,ColY),sort=F)
  return(Data)
}

#=================================================
#=================================================
#=================================================

CalcSVP <- function(temp) {
  # In kPa
  return(0.6106 * exp(17.27 * temp / (temp + 237.3)))
}

CalcHrVPD <- function(data,datecol,tcol) {
  data <- data.table(data)
  Func <- function(x) out <- CalcSVP(x[,get(tcol)]) - CalcSVP(x[,min(get(tcol),na.rm=T)])
  out  <- data[, .(VPD=Func(.SD)), keyby=datecol]
  return(out$VPD)
}

#=================================================
#=================================================
# A function to mark outliers.

MarkOutliers <- function(Data,Cols,Conf=0.95,MinCV=15,FlagCol=NULL,OutlierMark='Outlier_') { 
  Data2 <- copy(Data)
  for (Col in Cols) {
    if (is.null(FlagCol)) xFlagCol <- paste0(OutlierMark,Col) else xFlagCol <- FlagCol
    xCV <- ifelse(sum(!is.na(Data[,get(..Col)]))>2,CV(Data[,get(..Col)],Digits=4,FullRows=F),0)
    Data2[, (xFlagCol):=FALSE]
    if (is.finite(xCV) && xCV>MinCV) {
      xx <- Data[,get(..Col)]
      Data2[abs((xx-mean(xx,na.rm=T))) > ConfInt(xx,Conf,na.rm=T), (xFlagCol):=T]
    }
  }
  return(Data2)
}

#=================================================
#=================================================
# A function to show asterisks instead of p values.

RepPvalwithAst <- function(PVals) {
  Temp <- ifelse(PVals<0.001,"***", ifelse(PVals<0.01,"**", ifelse(PVals<0.05,"*", ifelse(PVals<0.10,".", "ns"))))
  return(Temp)
}

#=================================================
#=================================================
# A function to remove duplicated columns.

RemoveDupCols <- function(data) {
  if (any(duplicated(names(data)))) {
    data[,(names(data)[duplicated(names(data))]):=NULL]
  }
  return(data)
}

#=================================================
#=================================================
# A function to remove selected columns.

RemoveCols <- function(data, cols) {
  if (!is.null(cols) && length(cols)>0) {
    cols <- intersect(names(data),cols)
    if (!is.null(cols) && length(cols)>0) {
      data[,(cols):=NULL]
    }
  }
  return(data)
}

#=================================================
#=================================================
#=================================================

MySave <- function(Prefix,Folder,What,AllInOneFile=F,Sep='_',WhatInOneFile=NULL,NameOfOneFile='AllOthers',Env=parent.frame()) {
  Files <- NULL
  if (!is.null(What)) {
    if (!is.null(dim(What))) {
      Temp <- data.table(What)
      for (x in Temp[[1]]) {
        if (object.size(get(x))==0) {
          Temp <- Temp[Temp[[1]]!=x,]
        }
      }
      What <- unlist(Temp[,1])
      
      InfoFile <- paste0(Folder,'/',Prefix,Sep,'1Info','.csv')
      colnames(Temp) <- c('RDataFile','Description')
      Temp <- Temp[order(RDataFile)]
      Temp[, RDataFile:=paste0(Prefix,Sep,RDataFile)]
      if (!is.null(WhatInOneFile)) Temp <- rbind(Temp,data.frame(RDataFile=paste0(Prefix,Sep,NameOfOneFile),Description='Other variables'))
      write.csv(Temp,file=InfoFile,row.names=F,quote=F)
    }
    
  } else {
    return(Files)
  }
  
  if (AllInOneFile) {
    x <- save(list=What,file=Prefix,envir=Env)
    Files <- Prefix
    
  } else {
    if (!is.null(WhatInOneFile)) {
      What <- setdiff(What, WhatInOneFile)
      if (NameOfOneFile %in% What) NameOfOneFile <- paste0(NameOfOneFile,'2')
      File <- paste0(Folder,'/',Prefix,Sep,NameOfOneFile,'.RData')
      for (Name in WhatInOneFile) if (object.size(get(Name,envir=Env))==0) WhatInOneFile <- setdiff(WhatInOneFile,Name)
      if (length(WhatInOneFile)>0) {
        x     <- save(list=WhatInOneFile, file=File, envir=Env)
        Files <- c(Files, File)
      }
    }
    
    for (Name in What) {
      if (object.size(get(Name,envir=Env))!=0) {
        File  <- paste0(Folder, '/', Prefix ,Sep, Name, '.RData')
        x     <- save(list=Name, file=File, envir=Env)
        Files <- c(Files, File)
      }
    }
  }
  x <- gc()
  return(Files)
}

#=================================================
#=================================================
#=================================================

MyLoad <- function(File=NULL,What=NULL,Exclude=NULL,Prefix=NULL,Folder=NULL,Sep='_') {
  
  if (is.null(Prefix)) {
    Names <- base::load(File,envir=globalenv())
    if (!is.null(Exclude)) {
      Names <- setdiff(Names,Exclude)
      rm(list=Exclude,envir=.GlobalEnv)
    }
    if (!is.null(What)) {
      if (length(grep('AllOthers',File_Base_Name(File)))==0) {
        rm(list=setdiff(Names,What),envir=.GlobalEnv)
        x <- gc()
        Names <- intersect(Names,What)
      }
    }
    return(Names) 
    
  } else {
    Files <- list.files(path=Folder,pattern=paste0(Prefix,Sep,'[[:print:]]+.RData'),full.names=T)
    
    if (length(Files)==0) {
      Files <- list.files(path=Folder,pattern=Prefix,full.names=T)
      
    } else {
      if (!is.null(What)) {
        What    <- setdiff(What,Exclude)
        Pattern <- paste(What,collapse='|')
        Pattern <- sapply(Pattern, function(x) return(paste0(x,'.RData')))
        Files   <- grep(pattern=Pattern,Files,value=T)
      }
    }
    
    if (!is.null(Exclude)) {
      Pattern <- paste(paste0(Sep,Exclude,'\\.'),collapse='|')
      Exclude <- grep(pattern=Pattern,Files,value=T)
      if (length(Exclude)>0) Files <- setdiff(Files,Exclude)
    }
    
    Temp <- NULL
    for (File in Files) {
      x <- MyLoad(File=File,What=What,Prefix=NULL,Folder=NULL,Sep=Sep)
      Temp <- c(Temp,x)
    }
    return(sort(unique(Temp)))
  }
}

#=================================================
#=================================================
#=================================================

MySplitSave <- function(DTable,Keys,Prefix=NULL,Folder=getwd(),Sep='_',InFolder=F) {
  
  if (is.null(Prefix)) Prefix <- deparse(substitute(DTable))
  
  if (InFolder==TRUE) {
    Folder <- paste(Folder, Prefix, sep='/')
    dir.create(Folder, recursive=T, showWarnings=F)
  }
  
  saveFUNC <- function(data) {
    File  <- paste(unlist(data[1, Keys, with=F]), collapse=Sep)
    Path  <- paste0(Folder,'/', Prefix, Sep, File, '.RDS')
    x     <- saveRDS(data, file=Path)
    return(Path)
  }
  
  if (object.size(DTable)!=0) {
    Files <- DTable[, .(File=saveFUNC(.SD)), keyby=Keys, .SDcols=names(DTable)]
  }
  
  x <- gc()
  return(Files)
}

#=================================================
#=================================================
#=================================================

MySplitLoad <- function(Prefix,Folder=getwd(),Sep='_',Pattern=NULL) {
  Files <- list.files(path=Folder, pattern=glob2rx(paste0(Prefix,Sep,'*.RDS')), full.names=T)
  if (length(Files)==0) Files <- list.files(path=paste0(Folder,'/',Prefix), pattern=glob2rx(paste0(Prefix,Sep,'*.RDS')), full.names=T)
  if (!is.null(Pattern) && length(Files)>0) Files <- grep(Pattern, Files, value=T)
  
  Outs  <- list()
  for (File in Files) Outs[[basename(File)]] <- base::readRDS(File)
  Outs  <- rbindlist(Outs)
  
  x <- gc()
  return(Outs)
}

#=================================================
#=================================================
#=================================================

ListDepth <- function(this,thisdepth=0) {
  if(is.list(this) && !is.data.table(this)) {
    if (length(this)==0)
      return(thisdepth+1)
    else
      return(max(unlist(lapply(this,ListDepth,thisdepth=thisdepth+1))))
    
  } else {
    return(thisdepth)
  }
}

#=================================================
#=================================================
#=================================================

GetNodeFromJSON <- function(Json, Attrs, Values) {
  # Attrs: Attributes to be used to search the target node.
  # Values: Values of Attributes.
  library(jsonlite)
  
  if (!is.null(Json) && !is.list(Json) && file.exists(as.character(Json))) Json <- jsonlite::read_json(Json)
  
  if (is.list(Json)) {
    if (all(hasName(Json, Attrs)) && identical(as.vector(unlist(Json[Attrs])),Values)) {
      return(list(Json))
      
    } else  {
      Json <- lapply(Json, GetNodeFromJSON, Attrs, Values)
      if (!is.null(Json) && length(Json)>0) {
        Out <- list()
        for (j in 1:length(Json)) {
          if (!is.null(Json[[j]]) && length(Json[[j]])>0) {
            Out <- append(Out, Json[[j]])
          }
        }
        return(Out)
      }
    }
    
  } else {
    return(NULL)
  }
  return(NULL)
}

#=================================================
#=================================================
#=================================================

UpdateAttributeFromJSON <- function(Json, Attrs, Values, TargAttr, NewValue) {
  # Attrs: Attributes to be used to search the target node.
  # Values: Values of Attributes.
  # TargAttr: Target attribute. to be updated.
  # Newvalue: New value for the target attribute.
  library(jsonlite)
  
  if (!is.null(Json) && !is.list(Json) && file.exists(as.character(Json))) Json <- jsonlite::read_json(Json)
  
  if (is.list(Json)) {
    if (all(hasName(Json, Attrs)) && identical(as.vector(unlist(Json[Attrs])),Values)) {
      if (hasName(Json, TargAttr)) {
        Json[[TargAttr]] <- as(NewValue, Class=class(Json[[TargAttr]]))
      }
      
    } else  {
      Json <- lapply(Json, UpdateAttributeFromJSON, Attrs, Values, TargAttr, NewValue)
    }
  }
  return(Json)
}

#=================================================
#=================================================
#=================================================


