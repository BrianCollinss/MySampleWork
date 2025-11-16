

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

#=========================================================================================================================
# Project Details
#=========================================================================================================================
{
  Project  <- '2024.04.26 Short-Term Forecast for Irrigation Management'
  RunSet   <- '2024.05.26 2Lead-4P-1Z-2DelayFrac-22Treat-3Soil-2N'
  RunNames <- c('2024.05.28 Run1-50Rep', '2024.05.28 Run2-50Rep')
  Analysis <- c('2024.05.28 Analysis2')
}
#=========================================================================================================================
# Parameters
#=========================================================================================================================
{
  JustLoad  <- T # Just load the data or process the data?
  DoGraphs  <- T
  
  OutPrefix    <- 'Forecast'
  InpPrefix    <- 'Outputs'
  OutPrefixRaw <- paste0(OutPrefix,'Raw')
  
  Factors <- c('Scen','Lead','Confidence','MaxErr','DelayFrac','NLevel','SowDate','Site','Soil','IrrSys','Rep')
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/home/u8019357/APSIM/' else MainDir <- 'C:/Users/U8019357/OneDrive - UniSQ'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySourceProject.R',sep='/'))
  Dir <- SetupProject(MainDir=MainDir,Project=Project,Run=RunNames[1],RunSet=RunSet,Analysis=Analysis,SharedScripts=c('MyFunctions.R','MyApsim.R','MyPlots.R'))
}
LineX()
#=========================================================================================================================
{
  Traits <- list(Yield=list(Lab='Grain yield', Unit='kgDM/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 NumIrrig=list(Lab='Number of irrigations', Unit=NA, Limits=NULL, AbsRel='Abs', Colorband=MyColors$GnYlRd),
                 TotalIrrig=list(Lab='Irrigation water use', Unit='mm', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 OWUE=list(Lab='Overall water use efficiency', Unit='kgDM/ha/mm', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 IWUE=list(Lab='Irrigation water use efficiency', Unit='kgDM/ha/mm', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # NUPE=list(Lab='N uptake efficiency', Unit='kgN/kgDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # NUE=list(Lab='N use efficiency', Unit='kgN/kgDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # NHI=list(Lab='N harvest index', Unit='%', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # TE=list(Lab='Transpiration efficiency', Unit='-', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # TotalWatLoss=list(Lab='Total water loss', Unit='mm', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 # TotalNLeach=list(Lab='Total N leaching', Unit='kg/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 # TotalNUptake=list(Lab='Total N uptake', Unit='kg/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$RdYlGn),
                 # CO2RI=list(Lab='CO2 emission risk index', Unit='kg/t', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 # N2ORI=list(Lab='N2O emission risk index', Unit='kg/t', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd)
                 #--------------------------------------------------------------------------------------------------------------------
                 WLI=list(Lab='Water loss index', Unit='mm/tDM/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 NLOI=list(Lab='N loss index', Unit='kgN/tDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 NLI=list(Lab='N leaching index', Unit='kgN/tDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 TEI=list(Lab='Total emission index', Unit='kg/tDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 TotalCO2e=list(Lab='Total CO2e emission', Unit='kg/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd)
  )
  
  for (Tr in names(Traits)) {
    x <- tolower(Traits[[Tr]]$Lab)
    x <- gsub('^n ', 'N ', x)
    x <- gsub('co2 ', 'CO2 ', x)
    x <- gsub('co2e ', 'CO2e ', x)
    
    if (!is.na(Traits[[Tr]]$Unit)) {
      Traits[[Tr]]$Title    <- sprintf('%s (%s)', Traits[[Tr]]$Lab, Traits[[Tr]]$Unit)
      Traits[[Tr]]$AbsTitle <- sprintf('Absolute change in %s (%s)', x, Traits[[Tr]]$Unit)
      
    } else {
      Traits[[Tr]]$Title    <- Traits[[Tr]]$Lab
      Traits[[Tr]]$AbsTitle <- sprintf('Absolute change in %s', x)
    }
    
    Traits[[Tr]]$RelTitle <- sprintf('Relative change (%%) in %s', x)
  }
  
  TraitsAbsRel <- paste0(sapply(Traits, function(x) x$AbsRel), names(Traits))
  TraitsAbs <- paste0('Abs', names(Traits))
  TraitsRel <- paste0('Rel', names(Traits))
  
  ForcNumCols <- c('NumHits','NumMisses','NumFalseAlarms')
  
  RemColsLoad <- c('Project','Run','TarFile','Zone','CheckpointID','Experiment')
}
#=========================================================================================================================
# Execution
#=========================================================================================================================

if (JustLoad) {
  message('Importing the data from previous run')
  {  
    x <- gc()
    if (length(Analysis)==1) {
      x <- MyLoad(File=NULL,What=NULL,Prefix=OutPrefix,Folder=Dir$Analyses,Sep='_',Exclude=NULL)
      if (is.null(x)) stop("No file was found to load!")
      Seasonal <- RemoveCols(Seasonal, cols=RemColsLoad)
      
    } else {
      
      SeasonalX <- list()
      for (An in Analysis) {
        if (OnServer) An <- gsub('\\s+|\\.','_',An)   
        x <- MyLoad(File=NULL,What=NULL,Prefix=OutPrefix,Folder=paste0(Dir$Analyses,'/../',An),Sep='_',Exclude=NULL)
        SeasonalX[[An]] <- RemoveCols(Seasonal, cols=RemColsLoad)
        rm(Seasonal); x <- gc()
      }
      
      SeasonalX <- rbindlist(SeasonalX, fill=T)
      Seasonal  <- SeasonalX
      rm(SeasonalX, Temp); x <- gc()
    }
  }
  LineX()
  x <- gc()
  
} else {
  
  #=========================================================================================================================
  message('Importing and clustering raw data')
  {
    if (is.null(RunNames)) RunNames <- list.dirs(path=Dir$RunSet,full.names=F,recursive=F)
    
    LineX()
    Seasonal <- list()
    for (Rn in RunNames) {
      message(sprintf('Data from run: %s',Rn))
      if (OnServer) Rn <- gsub('\\s+|\\.','_',Rn)   
      rFound <- 0
      
      for (i in 1:200) {
        y <- MyLoad(Prefix=paste0(InpPrefix,i),Folder=paste(Dir$RunSet,Rn,sep='/'),What=NULL,Exclude='AllOthers')
        if (is.null(y)) next() else rFound <- rFound + 1
        
        Seasonal[[length(Seasonal)+1]]  <- RemoveCols(SeasOuts, cols=RemColsLoad)
        rm(SeasOuts); x <- gc()
      }
      
      if (rFound==0) {
        y <- MyLoad(Prefix=InpPrefix,Folder=paste(Dir$RunSet,Rn,sep='/'),What=NULL,Exclude='AllOthers')
        Seasonal[[length(Seasonal)+1]]  <- RemoveCols(SeasOuts, cols=RemColsLoad)
        rm(SeasOuts); x <- gc()
      }
    }
    
    Seasonal <- rbindlist(Seasonal,fill=T)
    Seasonal <- RemoveDupCols(Seasonal)
    x <- gc()
    LineX()
  }
  #=========================================================================================================================
  message('Renaming and addding variables')
  {
    # Get 'Rep' and 'Job' numbers.
    if ('File' %in% names(Seasonal) && !'Job' %in% names(Seasonal)) {
      Seasonal[, File:=gsub('Job|Rep','',File)]
      Seasonal <- as.data.table(tidyr::separate(Seasonal,col='File',sep='_',into=c('Job','Scen','Rep','Site','PAWC','Depth','SowDate'),remove=T,fill='right'))
      
      Seasonal[, `:=`(Soil=paste(PAWC,Depth,sep='-'), Job=as.numeric(as.character(Job)), Rep=as.numeric(as.character(Rep)), 
                      PAWC=as.numeric(as.character(PAWC)), Depth=as.numeric(as.character(Depth)), NLevel=as.numeric(as.character(NLevel)))]
    }
    
    Seasonal[, `:=`(Prob=as.numeric(as.character(Prob)), MaxErr=as.numeric(as.character(MaxErr)), DelayFrac=as.numeric(as.character(DelayFrac)))]
    Seasonal[is.na(Prob), `:=`(Prob=-1, MaxErr=-1)]
    Seasonal[Prob==-1, Confidence:=-1]
    Seasonal[Prob!=-1, Confidence:=100-Prob]
    
    if ('TotalNUptake' %in% names(Seasonal)) Seasonal[, TotalNUptake:=abs(TotalNUptake)]
    
    # Productivity and risk indexes.
    Seasonal[, `:=`(TotalWatLoss=TotalDrain+TotalRunoff+TotalEP, WLI=round((TotalDrain+TotalRunoff+TotalEP)/Yield*1000,2))]
    Seasonal[, `:=`(OWUE=round(Yield/(TotalIrrig+InCropRain),2), IWUE=round(Yield/TotalIrrig,2))]
    Seasonal[, `:=`(NLI=round(TotalNLeach/Yield*1000,2), NLOI=round((TotalNLeach+TotalN2Oatm)/Yield*1000,2))]
    Seasonal[, `:=`(TE=round(Yield/TotalTransp,2), ETcE=round(Yield/TotalETc,2))]
    Seasonal[, `:=`(TotalCO2e=TotalCatm*12/44 + TotalN2Oatm*273)]
    Seasonal[, `:=`(CO2RI=round((TotalCatm*12/44)/Yield*1000,2), N2ORI=round(TotalN2Oatm/Yield*1000,2), TEI=round(TotalCO2e/Yield*1000,2))]
    Seasonal[, `:=`(NUE=round(Yield/as.numeric(NLevel),2), NUPE=round(Yield/TotalNUptake,2), NHI=round(TotalGrainN/abs(TotalNUptake)*100,2))]
    Seasonal[is.infinite(IWUE), IWUE:=NA]
    
    # Adding a column for Soil x NLevel.
    SoilNLevs <- unique(Seasonal[, .(PAWC,Depth,NLevel)])
    SoilNLevs <- SoilNLevs[order(NLevel,PAWC,Depth)]
    SoilNLevs <- apply(SoilNLevs, 1, function(x) sprintf('SO=%s-%s | NAV=%s', x[1], x[2], x[3]))
    
    Seasonal[, SoilNLev:=sprintf('SO=%s | NAV=%s', Soil, NLevel)]
    Seasonal[, SoilNLev:=factor(SoilNLev, levels=SoilNLevs)]
    
    # Extract country names.
    Countries <- unique(Seasonal$Site)
    Countries <- strsplit(Countries, split='-')
    Countries <- sort(unique(sapply(Countries, function(x) x[[1]])))
  }
  #=========================================================================================================================
  message('Adding water/N balance classes')
  {
    Seasonal <- RemoveCols(Seasonal, cols=c('WBR','WBD','WBRClass','WBDClass','WBClass2','YldClass'))
    MaxPAWC  <- max(Seasonal$PAWC)
    MaxNL    <- max(Seasonal$NLevel)
    
    FUNC <- function(data, keys) {
      x <- data[Lead==0 & NLevel==MaxNL & PAWC==MaxPAWC & IrrSys=='Sprinkler']
      return(data.table(WBR=round(x$InCropRain/x$TotalETo,4), WBD=round(x$InCropRain-x$TotalETo,1)))
    }
    Keys1 <- setdiff(c(Factors,'SowYear'),c('Scen','Lead','Confidence','MaxErr','DelayFrac','NLevel','Soil','IrrSys'))
    Keys2 <- setdiff(c(Keys1,'NLevel','Soil','IrrSys'),c('Scen','Rep'))
    
    Temp <- copy(Seasonal[Scen=='Base'])
    Temp[, c('WBR','WBD'):=FUNC(.SD, .BY), keyby=Keys1, .SDcols=c('InCropRain','TotalETo','Lead','NLevel','PAWC','IrrSys')]
    Seasonal <- merge(Seasonal, Temp[, c(Keys2,'WBR','WBD'), with=F], by=Keys2, sort=F)
    
    setkeyv(Seasonal, cols=c('SimulationID',Factors,'SowYear'))
    
    for (k in 6:3) {
      x <- kmeans(Seasonal$WBR, centers=k)
      if (all(x$size/sum(x$size)>0.05)) {
        x <- kmeans(Seasonal$WBR, centers=sort(x$centers))
        break
      } 
    }
    
    WBRClasses <- paste0('CL',1:k)
    Seasonal[, WBRClass:=WBRClasses[x$cluster]]
    Seasonal[, WBRClass:=factor(WBRClass, WBRClasses)]
    
    WBRBreaks <- min(Seasonal$WBR)
    for (i in 1:(k-1)) {
      mx <- Seasonal[WBRClass==WBRClasses[i], max(WBR)]
      mn <- Seasonal[WBRClass==WBRClasses[i+1], min(WBR)]
      WBRBreaks <- c(WBRBreaks, mean(mn,mx))
    }
    WBRBreaks <- c(WBRBreaks, max(Seasonal$WBR))
    
    for (k in 6:3) {
      x <- kmeans(Seasonal$WBD, centers=k)
      if (all(x$size/sum(x$size)>0.05)) {
        x <- kmeans(Seasonal$WBD, centers=sort(x$centers))
        break
      } 
    }
    
    WBDClasses <- paste0('CL',1:k)
    Seasonal[, WBDClass:=WBDClasses[x$cluster]]
    Seasonal[, WBDClass:=factor(WBDClass, WBDClasses)]
    
    WBDBreaks <- min(Seasonal$WBD)
    for (i in 1:(k-1)) {
      mx <- Seasonal[WBDClass==WBDClasses[i], max(WBD)]
      mn <- Seasonal[WBDClass==WBDClasses[i+1], min(WBD)]
      WBDBreaks <- c(WBDBreaks, mean(mn,mx))
    }
    WBDBreaks <- c(WBDBreaks, max(Seasonal$WBD))
    
    rm(Temp); x <- gc()
  }
  #==========================================================-===============================================================
  message('ANOVA')
  {
    File <- paste(Dir$Analyses,'ANOVA.txt',sep='/')
    write('--------------------------------------------------------------------------------------------------', file=File)
    
    write('All Factors',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','MaxErr')),'SowYear'),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    
    write('All Factors with WBClass',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    
    write('All Factors with WBClass under Furrow Irrigation',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr','IrrSys')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf' & IrrSys=='Furrow']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    
    write('All Factors with WBClass under Sprinkler Irrigation',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr','IrrSys')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf' & IrrSys=='Sprinkler']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------',file=File, append=T)
    
    write('All Factors without Site, with WBClass',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr','Site')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    
    write('All Factors without Site, with WBClass under Furrow Irrigation',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr','IrrSys','Site')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf' & IrrSys=='Furrow']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    
    write('All Factors without Site, with WBClass under Sprinkler Irrigation',file=File, append=T)
    write('--------------------------------------------------------------------------------------------------', file=File, append=T)
    Vars <- paste(c(setdiff(Factors,c('Scen','Rep','SowDate','MaxErr','IrrSys','Site')),WBClassCol),collapse=' + ')
    lmmodel <- lm(as.formula(paste0('RelYield ~ ', Vars)), data=Impacts[Scen=='Imperf' & IrrSys=='Sprinkler']);
    x <- anova(lmmodel); x <- cbind(x, 'Share %'=round(x$`Sum Sq`/sum(x$`Sum Sq`)*100,1))
    utils::capture.output(x[order(x$`Share %`,decreasing=T),], file=File, append=T)
    write('--------------------------------------------------------------------------------------------------',file=File, append=T)
    
    # for (fr in setdiff(c(Factors,'WBRClass'), c('MaxErr','Scen','SowDate','Site'))) {
    #   print(fr)
    #   keys <- c(setdiff(c(Factors,'WBRClass'), c('MaxErr','Scen','SowDate','Site',fr)))
    #   print(Impacts[Scen=='Imperf', sd(RelYield, na.rm=T), keyby=keys])
    # }
  }
  #=========================================================================================================================
  message('Running diagnostic tests')
  {
    testthat::expect(Seasonal[Scen=='Base', max(Rep)==1], failure_message='Base scenario has more than one rep!')
  }
  #=========================================================================================================================
  LineX()
  message("Saving the processed results in RData files")
  {
    if (is.null(TempFold)) Folder <- Dir$Analyses else Folder <- TempFold
    
    What <- c('Seasonal')
    
    WhatInOneFile <- c('Factors','WBRBreaks','WBDBreaks','WBRClasses','WBDClasses','Countries','SoilNLevs')
    
    if (!is.null(What) && length(What)>1) What <- matrix(What,ncol=2,byrow=T)
    SaveFiles <- MySave(Prefix=OutPrefix,Folder=Folder,What=What,WhatInOneFile=WhatInOneFile)
    
    if (!is.null(TempFold)) {
      x <- file.copy(from=SaveFiles,to=Dir$Analyses,overwrite=T)
      x <- unlink(SaveFiles,recursive=T,force=T)
    }
  }
}





#=========================================================================================================================
# Graphs
#=========================================================================================================================

if (DoGraphs) {
  {
    MaxPAWC <- max(Seasonal$PAWC)
    MaxNL   <- max(Seasonal$NLevel)
    
    SoilLevs <- unique(Seasonal[, .(PAWC,Depth)])
    SoilLevs <- SoilLevs[order(PAWC,Depth)]
    SoilLevs <- apply(SoilLevs, 1, function(x) sprintf('%s-%s', x[1], x[2]))
    
    x <- gc()
    FontFamily <- 'Calibri'
    Dpi        <- 600
    
    WBColors   <- c('darkred','darkorange3','darkgreen','navy')
    WBClassCol <- 'WBRClass'
    
    # Calculates changes relative to base
    Cols <- setdiff(intersect(c(names(Traits),ForcNumCols), names(Seasonal)), c('TotalETc','TotalETo','InCropRain'))
    Keys <- c(Factors, WBClassCol, 'SowYear')
    RefIndices <- c('Rep','Lead','Confidence','MaxErr','DelayFrac')
    
    Impacts <- CalcAbsRelChanges(Seasonal, Cols=Cols, Indices=setdiff(Keys,'Scen'), RefIndices=RefIndices, RefIndicesVal=c(1,0,-1,-1,-1), TakeMean=F, RemoveBase=T)
    Impacts[, SoilNLev:=sprintf('SO=%s | NAV=%s', Soil, NLevel)]
    Impacts[, SoilNLev:=factor(SoilNLev, levels=SoilNLevs)]
    Impacts[, Scen:=c('Imperf','Perf')[(Confidence==100)+1]]
  }
  #==========================================================-===============================================================
  message('Reports')
  {
    # Temp <- Seasonal[sugarStatus=='dead', c(Factors,'IrrigAllocOrig','MaxIrrigNum','ratoon_no','sugarHarvYear'), with=F]
    # CsvPath <- paste0(Dir$Analyses,'/Rep-DeadCrops.csv')
    # fwrite(Temp,file=CsvPath)
    # rm(Temp); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Bar): WBRClasses')
  {
    for (i in 1:2) {
      xCol   <- ifelse(i==1, 'WBR', 'WBD')
      xLab   <- ifelse(i==1, 'Water balance ratio (WBR)', 'Water balance (WB)')
      iClass <- ifelse(i==1, 'WBRClass', 'WBDClass')
      iBreaks <- if (i==1) WBRBreaks[2:(length(WBRBreaks)-1)] else WBDBreaks[2:(length(WBDBreaks)-1)]
      
      Temp <- Seasonal[Scen=='Base' & NLevel==MaxNL & PAWC==MaxPAWC & IrrSys=='Sprinkler']
      
      Temp <- Add_Aggregate_Unit(Temp, Col='Site', AggUnit='All Sites', PutLast=F, AsFactor=T)
      Temp[Site=='All Sites', SowDate:='All Dates']
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/00 - Density - WB Distribution - %s.pdf', iClass))
      
      p <- DoGGPlot(Data=Temp,Type='densityarea',xCol=xCol,fCol='Site',hFacet='Site',FacetScale='free_y',
                    FacetGridAxes='all_x',FacetGridLabs='all_x',NRow=3,BinWidth=ifelse(i==1,0.1,50),
                    RemPanMarg=F,PanLw=0.15,xLab=xLab,yLab='Density',xAngle=0,yAngle=0,FColors=MyColors$BkPrYl(12),
                    VLine=iBreaks,VLineW=0.2,VLineCol='darkred',VLineType=3,RemGrids=T,RemMinGrids=T,RemStripBack=T,
                    AxTxSize=4,AxTiSize=7,Alpha=0.5,GridLwd=0.05,ExpPath=ExpPath,Size=PPSize(H=0.4,W=0.8),FontFamily=FontFamily,Dpi=Dpi)
      
      FUNC <- function(data) {
        if (nrow(data)<3) {
          Classes <- setdiff(WBRClasses,data[,get(iClass)])
          x <- data.table(XXX=Classes)
          x[[xCol]] <- 0
          colnames(x)[1] <- iClass
          data <- rbind(data, x)
        }
        return(data)
      }
      
      Temp <- Temp[, lapply(.SD, mean, na.rm=T), keyby=c('Site','SowDate',iClass), .SDcols=xCol]
      Temp <- Temp[, FUNC(.SD), keyby=c('Site','SowDate')]
      
      p1 <- DoGGPlot(Data=Temp,Type='bar',xCol='SowDate',yCol=xCol,gCol=c('SowDate',iClass),fCol=iClass,FacetScale='free_x',FacetSpace='free_x',
                     hFacet='Site',vFacetPref=T,RemPanMarg=T,PSize=0.7,PanLw=0.15,StripSizes=3.4,
                     yLab='Water balance ratio',xAngle=0,yAngle=0,CColors='black',FColors=WBColors,
                     RemGrids=T,RemMinGrids=T,RemStripBack=T,AxTxSize=c(3.1,3.8),AxTiSize=5.5,HLine=iBreaks,HLineCol='darkred',HLineType=2,
                     GridLwd=0.05,LegPos='top',LegKeyText=5,LegMarg=margin(t=0.02,b=0.2,unit='cm'),BarWidth=0.75,BarDodge=0.75,FontFamily=FontFamily,Dpi=Dpi)
      
      Temp <- Add_Aggregate_Unit(Seasonal[Scen=='Base'], Col='Site', AggUnit='All Sites', PutLast=F, AsFactor=T)
      Temp[Site=='All Sites', SowDate:='All Dates']
      Temp <- FreqTableXD(Temp, Cols=c('Site','SowDate',iClass), IncludeZero=T)
      
      p2 <- DoGGPlot(Data=Temp,Type='bar',xCol='SowDate',yCol='Prob',gCol=c('SowDate',iClass),fCol=iClass,FacetScale='free_x',FacetSpace='free_x',
                     hFacet='Site',vFacetPref=T,RemPanMarg=T,PSize=0.7,PanLw=0.15,StripSizes=3.4,
                     yLab='Frequency (%)',xAngle=0,yAngle=0,CColors='black',FColors=WBColors,
                     RemGrids=T,RemMinGrids=T,RemStripBack=T,AxTxSize=c(3.1,3.8),AxTiSize=5.5,GridLwd=0.05,
                     LegPos='top',LegKeyText=5,LegMarg=margin(t=0.02,b=0.2,unit='cm'),BarWidth=0.75,BarDodge=0.75,FontFamily=FontFamily,Dpi=Dpi)
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/00 - Bar - WB Classes - %s.pdf', iClass))
      x <- Plot_Grid(list(p1,p2), LegPos='top', NRow=2, NCol=1, RemLabs=F, CommLegend=T, ExpPath=ExpPath, Size=PPSize(H=0.3,W=0.67), FontFamily=FontFamily, Dpi=Dpi)
    }
    
    rm(Temp); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Bar): Averages')
  {
    for (Type in 1:2) {
      
      if (Type==1) {
        # For Site x SowDate Combinations
        Keys1 <- c(setdiff(Factors,'Rep'), 'SoilNLev', 'SowYear')
        
      } else if (Type==2) {
        # For WBRClasses
        Keys1 <- c(setdiff(Factors,'Rep'), 'SoilNLev', 'SowYear', WBClassCol)
      }
      
      Cols1 <- intersect(names(Traits), names(Seasonal))
      
      Temp <- Seasonal[Confidence!=100, lapply(.SD, mean, na.rm=T), keyby=Keys1, .SDcols=Cols1]
      
      Temp[, `:=`(Confidence=factor(Confidence,levels=c('-1','20','40','60','80')), MaxErr=factor(MaxErr,levels=c('-1','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
      
      Temp[is.na(DelayFrac), DelayFrac:='NA']
      Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c('NA',0.5,1)))]
      levels(Temp$DelayFrac) <- c('NA','0.5','1.0')
      
      if (Type==1) {
        xCol   <- 'SowDate'
        gCol   <- c('SowDate','SoilNLev')
        hFacet <- c('Site','DelayFrac')
        vFacet <- 'IrrSys'
        hFacetPref <- c(NA, 'DelayFrac: ')
        hFacetSuff <- NULL
        StripSizes <- 4.3
        Size  <- PPSize(H=0.3,W=0.66)
        
      } else if (Type==2) {
        xCol   <- WBClassCol
        gCol   <- c(WBClassCol,'SoilNLev')
        hFacet <- c('Scen','Lead','DelayFrac')
        vFacet <- 'IrrSys'
        hFacetPref <- T
        hFacetSuff <- c(NA,'d',NA)
        StripSizes <- 4.5
        Size  <- PPSize(H=0.3,W=0.66)
      }
      
      AxTxSize   <- 4
      LegKeyText <- 4
      
      if (Type==1) {
        
        for (Tr in Cols1) {
          Plots <- list()
          
          for (ctr in Countries) {
            if (!Tr %in% names(Temp)) next
            yLab <- Traits[[Tr]]$Title
            
            xData <- Temp[grepl(ctr,Site)]
            xData[is.infinite(get(Tr)), (Tr):=NA]
            xData[, Site:=as.factor(Site)]
            
            p <- DoGGPlot(Data=xData,Type='bar',xCol=xCol,yCol=Tr,gCol=gCol,fCol=gCol[2],FacetScale='free',
                          hFacet=hFacet,vFacet=vFacet,hFacetPref=hFacetPref,vFacetPref=T,hFacetSuff=hFacetSuff,PanLw=0.15,BarWidth=0.75,BarDodge=0.75,
                          yLab=yLab,xAngle=0,yAngle=0,CColors='black',FColors=MyColors$ET6,RemGrids='x',DropFacets=F,CheckFacets=F,
                          RemPanMarg=T,RemMinGrids=T,RemStripBack=T,StripSizes=StripSizes,AxTxSize=AxTxSize,ErrBarType='2se',
                          GridLwd=0.05,LegPos='top',LegKeyText=LegKeyText,LegNRow=2,Caption=sprintf('Country: %s', ctr),TitleSizes=4,FontFamily=FontFamily,Dpi=Dpi)
            
            Plots[[length(Plots)+1]] <- p
          }
          
          ExpPath <- paste0(Dir$Analyses, sprintf('/01 - Bar - Averages - Type %s - %s.pdf', Type, Traits[[Tr]]$Lab))
          x <- Print_Plots_to_File(Plots, File=ExpPath, Size=Size)
        }
        
      } else {
        
        Plots <- list()
        for (Tr in Cols1) {
          if (!Tr %in% names(Temp)) next
          yLab <- Traits[[Tr]]$Title
          
          xData <- Temp
          xData[is.infinite(get(Tr)), (Tr):=NA]
          
          p <- DoGGPlot(Data=xData,Type='bar',xCol=xCol,yCol=Tr,gCol=gCol,fCol=gCol[2],FacetScale='free',
                        hFacet=hFacet,vFacet=vFacet,hFacetPref=hFacetPref,hFacetSuff=hFacetSuff,vFacetPref=T,PanLw=0.15,
                        yLab=yLab,xAngle=30*(Type==1),yAngle=0,CColors='black',FColors=MyColors$ET6,RemGrids='x',
                        RemPanMarg=T,RemMinGrids=T,RemStripBack=T,StripSizes=StripSizes,AxTxSize=AxTxSize,GridLwd=0.05,LegNRow=2,
                        LegPos='top',LegKeyText=LegKeyText,ErrBarType='2se',BarWidth=0.75,BarDodge=0.75,FontFamily=FontFamily,Dpi=Dpi)
          
          Plots[[length(Plots)+1]] <- p
        }
        
        ExpPath <- paste0(Dir$Analyses, sprintf('/01 - Bar - Averages - Type %s - All Variables.pdf', Type))
        x <- Print_Plots_to_File(Plots, File=ExpPath, Size=Size)
      }
    }
    
    rm(Temp, xData); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Bar): Global Average Impacts')
  {
    Keys <- c('Scen','Lead','IrrSys')
    
    Plots <- list()
    for (Tr in names(Traits)) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      yLab <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      Temp <- list()
      for (gr in c(WBClassCol,'Soil','NLevel','DelayFrac')) {
        Temp2 <- copy(Impacts[Confidence%in%c(40,60,100), lapply(.SD, mean, na.rm=T), keyby=c(Keys,gr), .SDcols=yCol])[, Group:=gr]
        if (gr=='DelayFrac') {
          Temp2[, `:=`(DelayFrac=factor(DelayFrac, levels=c(0.5,1)))]
          levels(Temp2$DelayFrac) <- c('0.5','1.0')
        }
        Temp[[gr]] <- RenameCols(Temp2, data.table(Old=gr,new='X'))
      }
      Temp <- rbindlist(Temp)
      
      Temp[, Scen:=Change_Levels(Scen, OldLevs=c('Perf','Imperf'), NewLevs=c('Perfect','Imperfect'), AsFactor=T)]
      
      ExpPath <- paste0(Dir$Analyses, '/02 - Bar - Global Averages.pdf')
      
      p <- DoGGPlot(Data=Temp,Type='bar',xCol='X',yCol=yCol,fCol='Scen',gCol=c('X','Scen'),FacetScale='free',FacetSpace='free_x',
                    hFacet='Group',vFacet=c('Lead','IrrSys'),vFacetPref=T,vFacetSuff=c('d',NA),CColors='black',FColors=c('midnightblue','firebrick4'),HLine=0,HLineCol='black',HLineType=1,
                    yLab=yLab,StripSizes=4.2,AxTxSize=c(3.6,3.4),AxTiSize=4,PanLw=0.15,RemPanMarg=F,xAngle=0,yAngle=0,RemGrids=T,RemStripBack=T,
                    LegPos='top',LegKeyText=4,LegTitSize=5,LegTitPos='left',BarWidth=0.6,BarDodge=0.6,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, '/02 - Bar - Global Average Impacts - All Variables.pdf')
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.3,W=0.47))
    
    rm(Temp,Temp2); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Tile): Forecast Impact for WBRClasses')
  {
    Keys <- c(setdiff(Factors,c('Rep','Soil','NLevel')), 'SoilNLev', WBClassCol, 'SowYear')
    Cols <- setdiff(intersect(names(Traits), names(Seasonal)), c('TotalETc','TotalETo','InCropRain'))
    
    # Take mean across reps.
    Temp <- copy(Impacts[MaxErr%in%c(0,100), lapply(.SD, mean, na.rm=T), keyby=Keys, .SDcols=c(Cols,paste0('Rel',Cols),paste0('Abs',Cols))])
    
    Temp[, `:=`(Confidence=factor(Confidence,levels=c('100','80','60','40','20')), MaxErr=factor(MaxErr,levels=c('0','100')), 
                DelayFrac=as.factor(as.character(DelayFrac)))]
    levels(Temp$DelayFrac) <- c('0.5','1.0')
    
    # Separate perfect and imperfect scenarios.
    Temp1 <- Temp[Scen=='Perf',   lapply(.SD, mean, na.rm=T), keyby=Keys, .SDcols=c(Cols,paste0('Rel',Cols),paste0('Abs',Cols))]
    Temp2 <- Temp[, lapply(.SD, mean, na.rm=T), keyby=Keys, .SDcols=c(Cols,paste0('Rel',Cols),paste0('Abs',Cols))]
    
    # Plots.
    xCol    <- WBClassCol
    gCol    <- c(WBClassCol,'SoilNLev')
    
    Plots <- list()
    for (Tr in Cols) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp1)) next
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      if (Temp1[MaxErr==0, all(is.nan(get(yCol)) | is.infinite(get(yCol)))]) next
      Temp1[MaxErr==0 & (is.nan(get(yCol)) | is.infinite(get(yCol))), (yCol):=NA]
      
      p <- DoGGPlot(Data=Temp1[MaxErr==0],Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                    hFacet=c('Lead','DelayFrac'),vFacet=c('IrrSys'),hFacetPref=T,vFacetPref=T,hFacetSuff=c('d',NA),vFacetSuff=NULL,
                    StripSizes=5.3,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.7,TileLabDigs=1,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,RemMinGrids=T,RemStripBack=T,
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegWidth=0.25,LegLength=15,LegKeyText=5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, sprintf('/03 - Tile - Impact Perf - Type %s - All Variables.pdf', 2))
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.34,W=0.57))
    
    Plots <- list()
    for (Tr in Cols) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp2)) next
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      if (Temp2[, all(is.nan(get(yCol)) | is.infinite(get(yCol)))]) next
      Temp2[(is.nan(get(yCol)) | is.infinite(get(yCol))), (yCol):=NA]
      
      p <- DoGGPlot(Data=Temp2,Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                    hFacet=c('Lead','Confidence'),vFacet=c('IrrSys','DelayFrac'),hFacetPref=T,vFacetPref=T,hFacetSuff=c('d','%'),RemStripBack=T,
                    StripSizes=5.3,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.5,TileLabDigs=1,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegNumBreaks=10,LegWidth=0.30,LegLength=18,LegKeyText=5.5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,
                    FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, sprintf('/04 - Tile - Impact - Type %s - All Variables.pdf', 2))
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.47,W=1.0))
    
    
    Cols1 <- c('Yield','WLI','NLOI','TEI')
    Cols2 <- c(na.omit(sapply(names(Traits), function(x) {if (x %in% Cols1) str_to_title(Traits[[x]]$Lab) else NA})))
    
    Temp3 <- melt(Temp2[Lead=='7' & DelayFrac=='1.0' & Confidence%in%c(100,20)], id.vars=c(gCol,'Lead','Confidence','IrrSys','DelayFrac'), measure.vars=paste0('Rel',Cols1))
    Temp3[, variable:=gsub('Rel','',variable)]
    Temp3[, variable2:=Change_Levels(variable, OldLevs=Cols1, NewLevs=Cols2, AsFactor=T)]
    
    Plots <- list()
    for (i in 1:2) {
      p <- DoGGPlot(Data=if (i==1) Temp3[variable=='Yield'] else Temp3[variable!='Yield'],Type='tiles',xCol=xCol,yCol=gCol[2],fCol='value',
                    hFacet=c('variable2','Confidence'),vFacet='IrrSys',hFacetPref=c(NA,'Confidence: '),vFacetPref='Irrigation System: ',hFacetSuff=c(NA,'%'),
                    StripSizes=5.75*c(1,i==2),AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.56,TileLabDigs=1,RemStripBack=T,CheckFacets=F,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=if (i==1) Traits$Yield$Colorband else Traits$NLI$Colorband,RemGrids=T,RemAxesLabs=c(F,i==2,F),
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegNumBreaks=8,LegLength=9,LegWidth=0.30,LegKeyText=3.5,LegPos='top',LegDirection='horizontal',LegTitPos='top',
                    ContinLeg=T,LegTitleF='Relative Change (%)',FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)

      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, '/20 - Tile - Graphical Abstract (Old).pdf')
    x <- Plot_Grid(Plots, LegPos='top', NRow=1, NCol=2, ExpPath=ExpPath, Size=PPSize(H=0.4,W=1.2), Widths=c(1,2.3), Align='h', FontFamily=FontFamily, Dpi=Dpi)
    
    
    Temp4 <- copy(Temp3)[variable2 %in% c('Grain Yield','N Loss Index') & IrrSys %in% c('Sprinkler')]
    levels(Temp4$SoilNLev) <- Temp4[, gsub('-800','-S',levels(SoilNLev))]
    levels(Temp4$SoilNLev) <- Temp4[, gsub('-1400','-D',levels(SoilNLev))]

    Plots <- list()
    for (i in 1:2) {
      p <- DoGGPlot(Data=if (i==1) Temp4[variable=='Yield'] else Temp4[variable!='Yield'],Type='tiles',xCol=xCol,yCol=gCol[2],fCol='value',
                  hFacet=c('Confidence'),hFacetPref=c('Confidence: '),hFacetSuff=c('%'),StripSizes=15,
                  AxTxSize=c(5.2,5.5)*2,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.2,RemPanMarg=F,TileLabSize=3.7,TileLabDigs=1,RemStripBack=T,CheckFacets=F,
                  xAngle=0,yAngle=0,CColors='black',FColorsCont=if (i==1) Traits$Yield$Colorband else Traits$NLI$Colorband,RemGrids=T,RemAxesLabs=F,
                  HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                  LegNumBreaks=8,LegLength=1.4,LegWidth=14,LegKeyText=11,LegTitSize=16,LegPos='left',LegDirection='vertical',
                  LegTitPos='left',LegLabsStart=1,LegLabsGap=3,LegBoxMarg=unit(0.0,'cm'),
                  ContinLeg=T,LegTitleF=if (i==1) 'Change in yield (%)' else 'Change in N loss index (%)',FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, '/20 - Tile - Graphical Abstract.pdf')
    x <- Plot_Grid(Plots, LegPos='left', NRow=2, NCol=1, ExpPath=ExpPath, Widths=1, RemLabs=F, RemTitles=F, RemMargs=F, 
                   Align='h', Size=PPSize(H=0.72,W=0.96), PlotsPad=unit(0.3,'cm'), FontFamily=FontFamily, Dpi=Dpi)
    
    rm(Temp, Temp1, Temp2, Temp3, Temp4); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Tile): Analysis of Stability for WBRClasses')
  {
    Keys <- c(setdiff(Factors,c('Rep','Soil','NLevel','SowDate','Site')), 'SoilNLev', WBClassCol, 'SowYear')
    Cols <- setdiff(intersect(names(Traits), names(Seasonal)), c('TotalETc','TotalETo','InCropRain'))
    
    # Calculate mean across reps.
    Temp <- copy(Seasonal[, lapply(.SD, mean, na.rm=T), keyby=Keys, .SDcols=Cols])
    
    # Calculate CV across seasons.
    Temp1 <- Temp[, lapply(.SD, function(x) mean(x[x<quantile(x,0.25,na.rm=T)])), keyby=setdiff(Keys,'SowYear'), .SDcols=Cols]
    Temp2 <- Temp[, lapply(.SD, function(x) mean(x[x>quantile(x,0.75,na.rm=T)])), keyby=setdiff(Keys,'SowYear'), .SDcols=Cols]
    Temp  <- rbind(Temp1[,Range:='Lows'], Temp2[,Range:='Highs'])
    Temp[, Range:=factor(Range, levels=c('Lows','Highs'))]
    
    # Calculates changes in std relative to base.
    RefIndices <- c('Lead','Confidence','MaxErr','DelayFrac')
    Temp <- CalcAbsRelChanges(Temp, Cols=Cols, Indices=setdiff(c(Keys,'Range'),c('Scen','SowYear')), RefIndices=RefIndices, RefIndicesVal=c(0,-1,-1,-1), TakeMean=F, RemoveBase=T)
    
    Temp[, Scen:=c('Perf','Imperf')[(Confidence!=100)+1]]
    Temp[, `:=`(Confidence=factor(Confidence,levels=c('100','80','60','40','20')), MaxErr=factor(MaxErr,levels=c('0','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
    
    Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c(0.5,1)))]
    levels(Temp$DelayFrac) <- c('0.5','1.0')
    
    # Plots.
    xCol    <- WBClassCol
    gCol    <- c(WBClassCol,'SoilNLev')
    
    Plots <- list()
    for (Tr in Cols) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      p <- DoGGPlot(Data=Temp[MaxErr==0],Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                    hFacet=c('Lead','DelayFrac'),vFacet=c('IrrSys','Range'),hFacetPref=T,vFacetPref=T,hFacetSuff=c('d',NA),vFacetSuff=NULL,
                    StripSizes=5.3,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.7,TileLabDigs=1,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,RemMinGrids=T,RemStripBack=T,
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegWidth=0.25,LegLength=15,LegKeyText=5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, sprintf('/05 - Tile - Stability Perf - Type %s - All Variables.pdf', 2))
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.5,W=0.57))
    
    for (Tr in Cols) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      Plots <- list()
      for (irrSys in c('Furrow','Sprinkler')) {
        
        p <- DoGGPlot(Data=Temp[IrrSys==irrSys],Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                      hFacet=c('Lead','Confidence'),vFacet=c('DelayFrac','Range'),hFacetPref=T,vFacetPref=T,hFacetSuff=c('d','%'),vFacetSuff=NULL,RemStripBack=T,
                      StripSizes=5.3,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.5,TileLabDigs=1,
                      xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,
                      HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                      LegWidth=0.30,LegLength=18,LegKeyText=5.5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,FMidPoint=0,LegNumDigs=1,FontFamily=FontFamily,Dpi=Dpi)
        
        Plots[[length(Plots)+1]] <- p
      }
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/06 - Tile - Stability - Type %s - %s.pdf', 2, Traits[[Tr]]$Lab))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.47,W=1.0))
    }
    
    rm(Temp, Temp1, Temp2); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Point): Yield vs Yield')
  {
    Keys1 <- c(Factors, 'SoilNLev', WBClassCol, 'SowYear')
    Keys2 <- c(setdiff(Factors,'Rep'), WBClassCol, 'SowYear')
    Cols1 <- 'Yield'
    
    Temp <- copy(Impacts[DelayFrac%in%c(0.5)])
    
    Temp[, `:=`(Confidence=factor(Confidence,levels=c('20','40','60','80','100')), MaxErr=factor(MaxErr,levels=c('0','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
    Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c(0.5,1)))]
    levels(Temp$DelayFrac) <- c('0.5','1.0')
    
    Temp  <- Temp[, lapply(.SD, mean, na.rm=T), keyby=c(setdiff(Factors,c('Rep','Soil')),WBClassCol,'SoilNLev','SowYear'), .SDcols=c('BaseYield','BaseNumIrrig','Yield',TraitsAbsRel)]
    
    Temp[, (WBClassCol):=as.factor(get(WBClassCol))]
    Sites <- c('All', sort(unique(Temp$Site)))
    
    Temp2  <- Temp[, lapply(.SD, mean, na.rm=T), keyby=c('BaseNumIrrig','Confidence','Lead','NLevel'), .SDcols=c('BaseYield','Yield',TraitsAbsRel)]
    Temp2[, NAV:=as.character(NLevel)]
    
    Plots <- list()
    for (i in 1:length(TraitsAbsRel)) {
      yCol <- TraitsAbsRel[i]
      yLab <- Traits[[i]][[paste0(Traits[[i]]$AbsRel,'Title')]]
      
      p <- DoGGPlot(Data=Temp2,Type='point',xCol='BaseNumIrrig',yCol=yCol,cCol='Confidence',rgcCol='Confidence',
                    hFacet='Lead',vFacet='NAV',hFacetPref=T,vFacetPref=T,hFacetSuff='d',vFacetSuff=' kgN/ha',RemStripBack=T,StripSizes=7,
                    PSize=0.8,LegScale=2.5,AxTxSize=6.0,PanLw=0.15,RemPanMarg=F,RemGrids=T,LegDrop=F,AxTiSize=8,
                    xLab='Number of irrigations',yLab=yLab,xAngle=0,yAngle=0,CColors=rev(MyColors$ET6),
                    ShowReg=T,RegType=1,RegDegree=2,AddRegEq='Eq-R-P',RegEqCol=T,RegEqPos=ifelse(i%in%c(2,3),'topleft','bottomleft'),RegEqHjust=-0.01,
                    RegEqStat=NULL,RegEqSize=1.5,RegEqMarg=5.5,RegEqBold=T,RegDrop=F,RegEqDigs=c(3,2),LegMarg=margin(b=0.25,unit='cm'),
                    LegKeyText=7,LegPos='top',LegTitleC='Confidence (%)',LegTitleSh='Irrigation System',LegTitPos='top',
                    Caption=sprintf('DelayFrac: %s', 0.5),TitleSizes=5,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, '/07 - Point - Impact vs Number of Irrigations.pdf')
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.40,W=0.65))
    
    
    for (irrSys in c('Furrow','Sprinkler')) {
      for (pr in c(40,80)) {
        Plots <- list()
        
        for (st in Sites) {
          TempX <- if (st=='All') Temp else Temp[Site==st]
          
          p <- DoGGPlot(Data=TempX[Confidence==pr & IrrSys==irrSys],Type='point',xCol='BaseYield',yCol='Yield',cCol=WBClassCol,rgcCol=WBClassCol,
                        hFacet='Lead',vFacet='SoilNLev',hFacetPref=T,hFacetSuff='d',RemStripBack=T,StripSizes=c(7.5,6),
                        PSize=0.6,LegScale=5,AxTxSize=5.43,PanLw=0.15,RemPanMarg=F,RemGrids=T,LegDrop=F,AxTiSize=8,
                        xLab='Base yield',yLab='Yield with forecast',xAngle=0,yAngle=0,CColors=WBColors,
                        ShowReg=F,RegType=0,AddRegEq='Eq-R',RegEqCol=T,RegEqStat=c('MRE','N'),Add45L=T,RegEqSize=1.6,RegEqMarg=8,RegEqBold=T,RegDrop=F,
                        LegKeyText=7,LegPos='top',LegTitleC=NULL,LegTitPos='left',
                        Caption=sprintf('DelayFrac: %s | Site: %s', 0.5, st),TitleSizes=5,FontFamily=FontFamily,Dpi=Dpi)
          
          Plots[[length(Plots)+1]] <- p
        }
        
        ExpPath <- paste0(Dir$Analyses, sprintf('/07 - Point - Yield vs Yield - %s - Confidence %s.pdf', irrSys, pr))
        x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.74,W=0.65))
      }
    }
  }
  #=========================================================================================================================
  message('Plot (Tile): Risk of Rield Reduction')
  {
    Temp <- copy(Impacts[, c(Factors,WBClassCol,'SoilNLev','SowYear','RelYield'), with=F])
    Temp[, `:=`(Confidence=factor(Confidence,levels=c('100','80','60','40','20')), MaxErr=factor(MaxErr,levels=c('0','100')))]
    
    Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c(0.5,1)))]
    levels(Temp$DelayFrac) <- c('0.5','1.0')
    
    Plots1 <- list()
    for (ld in c(3,7)) {
      for (z in c(100)) {
        for (irrSys in c('Furrow','Sprinkler')) {
          for (pr in c(-5,-10)) {
            LegTitleF <- sprintf('Probability of yield reduction >%s%%', abs(pr))
            
            TempY <- RenameCols(FreqTableXD(Temp[Lead==ld & IrrSys==irrSys & MaxErr==z & Confidence%in%c(40)], 
                                            Cols=c('Site','SoilNLev','Confidence','DelayFrac',WBClassCol)), 
                                data.frame(Old=c('Freq','Prob'), New=c('FreqY','ProbY')))
            
            TempX <- FreqTableXD(Temp[, `:=`(X=(RelYield<pr))][Lead==ld & IrrSys==irrSys & MaxErr==z & Confidence%in%c(40)], 
                                 Cols=c('Site','SoilNLev','Confidence','DelayFrac',WBClassCol,'X'))
            
            TempX <- merge(TempX,TempY,by=c('Site','SoilNLev','Confidence','DelayFrac',WBClassCol),sort=F)[ProbY>=5 & X==TRUE]
            
            p <- DoGGPlot(Data=TempX,Type='tiles',xCol='Site',yCol='SoilNLev',fCol='Prob',FacetScale='free_x',FacetSpace='free_x',
                          hFacet=WBClassCol,vFacet='DelayFrac',hFacetPref=NULL,vFacetPref=T,hFacetSuff=NULL,vFacetSuff=NULL,RemStripBack=T,
                          StripSizes=5.3,AxTxSize=4.38,PanLw=0.15,RemPanMarg=F,TileLabSize=1.6,TileLabDigs=0,
                          xAngle=30,yAngle=0,yLab=NULL,CColors='black',FColorsCont=MyColors$RedsX[1:7],RemGrids=T,
                          LegWidth=0.25,LegLength=15,LegKeyText=4,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,LegNumDigs=0,LegLimits=c(0,100),
                          HLine=0.5+seq(1,11),VLine=0.5+seq(1,11),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                          Caption=sprintf('Confidence: %s%% | Irrigation: %s | Lead: %s | MaxErr: %s', 40, irrSys, ld, z),TitleSizes=4,FontFamily=FontFamily,Dpi=Dpi)
            
            Plots1[[length(Plots1)+1]] <- p
          }
        }
      }
    }
    
    ExpPath <- paste0(Dir$Analyses, '/08 - Tile - Yield Reduction Risk - Type 1.pdf')
    x <- Print_Plots_to_File(Plots1, File=ExpPath, Size=PPSize(H=0.35,W=0.65))
    
    Plots <- list()
    for (pr in c(-5,-10)) {
      LegTitleF <- sprintf('Probability of yield reduction >%s%%', abs(pr))
      
      TempX <- FreqTableXD(Temp[, `:=`(X=(RelYield<pr))], Cols=c(WBClassCol,'Lead','IrrSys','SoilNLev','Confidence','DelayFrac','X'))[X==T]
      
      p <- DoGGPlot(Data=TempX,Type='tiles',xCol=WBClassCol,yCol='SoilNLev',fCol='Prob',FacetScale='free_x',FacetSpace='free_x',
                    hFacet=c('Lead','Confidence'),vFacet=c('IrrSys','DelayFrac'),hFacetPref=T,vFacetPref=T,hFacetSuff=c('d','%'),RemStripBack=T,
                    StripSizes=5.3,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.5,TileLabDigs=0,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=MyColors$RedsX[1:7],RemGrids=T,
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegNumBreaks=10,LegWidth=0.30,LegLength=18,LegKeyText=5.5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,LegNumDigs=0,FontFamily=FontFamily,Dpi=Dpi)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, '/08 - Tile - Yield Reduction Risk - Type 2.pdf')
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.47,W=1.0))
    
    rm(Temp,TempX,TempY); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Point): Correlation')
  {
    for (Type in 2:2) {
      
      if (Type==1) {
        # For Site x SowDate Combinations
        Keys1 <- c(Factors, 'SowYear')
        
      } else if (Type==2) {
        # For WBRClasses
        Keys1 <- c(Factors, WBClassCol, 'SowYear')
      }
      
      Cols1 <- intersect(c(names(Traits), ForcNumCols), names(Seasonal))
      Cols1 <- setdiff(Cols1, c('TotalETc','TotalETo','InCropRain'))
      
      Temp <- copy(Impacts[get(WBClassCol)==get(paste0(WBClassCol,'es'))[c(1,3)] & DelayFrac%in%c(0.5) & MaxErr%in%c(100)])
      
      Temp[, `:=`(NLevel=as.numeric(as.character(NLevel)))]
      Temp[, `:=`(Soil=factor(Soil,levels=SoilLevs))]
      
      CorColsX <- intersect(ForcNumCols, names(Temp))
      CorColsY <- intersect(TraitsAbsRel, names(Temp))
      
      Temp <- RenameCols(Temp, data.frame(old=paste0('Abs',ForcNumCols), new=ForcNumCols))
      
      if (Type==1) {
        hFacet <- c('Site','Soil','SowDate')
        hFacetSuff <- NULL
        Size   <- PPSize(H=0.76,W=0.95)
        
      } else if (Type==2) {
        hFacet <- c('Lead','Soil',WBClassCol)
        hFacetSuff <- c('d',NA,NA)
        Size   <- PPSize(H=0.68,W=1.33)
      }
      
      Plots <- list()
      for (irrSys in c('Furrow','Sprinkler')) {
        for (z in c(100)) {
          p <- DoGGPlot(Data=Temp[MaxErr==z & IrrSys==irrSys],Type='corr',corCols=list(CorColsX,CorColsY),
                        hFacet=hFacet,vFacet='NLevel',hFacetPref=T,vFacetPref=T,hFacetSuff=hFacetSuff,RemStripBack=T,StripSizes=c(6,7.5),
                        PShape=22,PSize=7.4,BarLabSize=1.5,RemGrids=T,xAngle=25,yAngle=0,PThr=0.05,
                        PanLw=0.15,RemPanBorder=F,RemAxesLines=F,RemPanMarg=T,AxTxSize=6,
                        LegWidth=0.45,LegLength=28,LegKeyText=6,LegPos='top',LegLimits=c(-1,1),LegTitleF='Correlation',
                        Caption=sprintf('Irrigation: %s | MaxErr: %s | DelayFrac: %s', irrSys, z, 0.5),FontFamily=FontFamily,Dpi=Dpi)
          
          Plots[[length(Plots)+1]] <- p
        }
      }
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/09 - Point - Correlation - Type %s.pdf', Type))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=Size)
    }
    
    rm(Temp); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Line): Time Series')
  {
    Keys1 <- c(Factors, 'SoilNLev', WBClassCol, 'SowYear')
    Keys2 <- c(setdiff(Factors,'Rep'), WBClassCol, 'SowYear')
    Cols1 <- 'Yield'  # intersect(names(Traits), names(Seasonal))
    
    Temp <- copy(Impacts[Scen!='Perf' & Site=='AU-Emerald' & SowDate=='15-Sep' & SoilNLev=='SO=170-1400 | NAV=300' & DelayFrac==0.5])
    
    Temp[, `:=`(Confidence=factor(Confidence,levels=c('20','40','60','80')), MaxErr=factor(MaxErr,levels=c('0','100')))]
    
    Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c(0.5,1)))]
    levels(Temp$DelayFrac) <- c('0.5','1.0')
    
    ExpPath <- paste0(Dir$Analyses, '/10 - Line - Time Series.pdf')
    
    p <- DoGGPlot(Data=Temp,Type='line',xCol='SowYear',yCol='RelYield',gCol=c('Rep','Confidence'),cCol='Confidence',FacetScale='free_y',
                  hFacet='Lead',vFacet='IrrSys',hFacetPref=T,vFacetPref=T,hFacetSuff='d',CColors=rev(MyColors$ET6),Alpha=0.2,LegAlpha=1,HLine=0,HLineCol='darkred',HLineType=3,
                  xLab='Sowing year',yLab=Traits$Yield$RelTitle,StripSizes=5,AxTxSize=4.2,PanLw=0.15,RemPanMarg=F,xAngle=0,yAngle=0,RemGrids=T,RemStripBack=T,
                  LegPos='top',LegKeyText=4.8,LegScale=5.3,LegTitleC='Confidence (%)   ',LegTitPos='left',Lwd=0.13,
                  Caption=sprintf('AU-Emerald | SowDate: %s | SoilNLev: %s | DelayFrac: %s', '15-Sep', 'SO=170-1400 | NR=300', 0.5),TitleSizes=2.5,
                  ExpPath=ExpPath,Size=PPSize(H=0.25,W=0.5),FontFamily=FontFamily,Dpi=Dpi)
    
    rm(Temp); x <- gc()
  }
  #=========================================================================================================================
  #=========================================================================================================================
  #=========================================================================================================================
  LineX()
  message("Saving the selected figures")
  {
    if (is.null(TempFold)) Folder <- Dir$Analyses else Folder <- TempFold
    
    What <- NULL
    WhatInOneFile <- NULL
    
    SaveFiles <- MySave(Prefix=OutPrefix,Folder=Folder,What=What,WhatInOneFile=WhatInOneFile,NameOfOneFile='SelectedFigs')
    
    if (!is.null(TempFold)) {
      x <- file.copy(from=SaveFiles,to=Dir$Analyses,overwrite=T)
      x <- unlink(SaveFiles,recursive=T,force=T)
    }
  }
}

if (OnServer) for (i in 1:200) print('Job is done!')




