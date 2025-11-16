

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
  RunSet   <- '2024.05.25 2Lead-4P-1Z-2DelayFrac-22Treat-2Soil-2N'
  RunNames <- c('2024.05.25 Run2-30Rep')
  Analysis <- c('2024.05.25 Analysis1')
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
  
  Factors <- c('Scen','Lead','Prob','MaxErr','DelayFrac','NLevel','SowDate','Site','PAWC','IrrSys','Rep')
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
                 NumIrrig=list(Lab='Number of irrigations', Unit=NA, Limits=NULL, AbsRel='Abs', Colorband=MyColors$GnYlRd),
                 TotalIrrig=list(Lab='Total irrigation', Unit='mm', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 TERI=list(Lab='Total emission risk index', Unit='kg/tDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 WLRI=list(Lab='Water loss risk index', Unit='mm/tDM/ha', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
                 NLRI=list(Lab='N loss risk index', Unit='kgN/tDM', Limits=NULL, AbsRel='Rel', Colorband=MyColors$GnYlRd),
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
      x <- MyLoad(File=NULL,What=NULL,Prefix=OutPrefix,Folder=Dir$Analyses,Sep='_',Exclude='DailyC')
      if (is.null(x)) stop("No file was found to load!")
      Seasonal <- RemoveCols(Seasonal, cols=RemColsLoad)
      
    } else {
      
      SeasonalX <- list()
      for (An in Analysis) {
        if (OnServer) An <- gsub('\\s+|\\.','_',An)   
        x <- MyLoad(File=NULL,What=NULL,Prefix=OutPrefix,Folder=paste0(Dir$Analyses,'/../',An),Sep='_',Exclude='DailyC')
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
  }
  #=========================================================================================================================
  message('Renaming and addding variables')
  {
    # Get 'Rep' and 'Job' numbers.
    if ('File' %in% names(Seasonal) && !'Job' %in% names(Seasonal)) {
      Seasonal[, File:=gsub('Job|Rep','',File)]
      Seasonal <- as.data.table(tidyr::separate(Seasonal,col='File',sep='_',into=c('Job','Scen','Rep','Site','PAWC','SowDate'),remove=T,fill='right'))
      Seasonal[, `:=`(Job=as.numeric(as.character(Job)), Rep=as.numeric(as.character(Rep)), 
                      PAWC=as.numeric(as.character(PAWC)), NLevel=as.numeric(as.character(NLevel)))]
    }
    
    Seasonal[, `:=`(Prob=as.numeric(as.character(Prob)), MaxErr=as.numeric(as.character(MaxErr)), DelayFrac=as.numeric(as.character(DelayFrac)))]
    Seasonal[is.na(Prob), `:=`(Prob=-1, MaxErr=-1)]
    
    if ('TotalNUptake' %in% names(Seasonal)) Seasonal[, TotalNUptake:=abs(TotalNUptake)]
    
    # Productivity and risk indexes.
    Seasonal[, `:=`(TotalWatLoss=TotalDrain+TotalRunoff+TotalEP, WLRI=round((TotalDrain+TotalRunoff+TotalEP)/Yield*1000,2))]
    Seasonal[, `:=`(OWUE=round(Yield/(TotalIrrig+InCropRain),2), IWUE=round(Yield/TotalIrrig,2))]
    Seasonal[, `:=`(NLRI=round(TotalNLeach/Yield*1000,2), TE=round(Yield/TotalTransp,2), ETcE=round(Yield/TotalETc,2))]
    Seasonal[, `:=`(TotalCO2e=TotalCatm*12/44 + TotalN2Oatm*298)]
    Seasonal[, `:=`(CO2RI=round((TotalCatm*12/44)/Yield*1000,2), N2ORI=round(TotalN2Oatm/Yield*1000,2), TERI=round(TotalCO2e/Yield*1000,2))]
    Seasonal[, `:=`(NUE=round(Yield/as.numeric(NLevel),2), NUPE=round(Yield/TotalNUptake,2), NHI=round(TotalGrainN/abs(TotalNUptake)*100,2))]
    
    # Extract country names.
    Countries <- unique(Seasonal$Site)
    Countries <- strsplit(Countries, split='-')
    Countries <- sort(unique(sapply(Countries, function(x) x[[1]])))
  }
  #=========================================================================================================================
  message('Adding water/N balance classes')
  {
    Seasonal <- RemoveCols(Seasonal, cols=c('WBR','WBClass','WBClass2','YldClass'))
    MaxPAWC  <- max(Seasonal$PAWC)
    MaxNL    <- max(Seasonal$NLevel)
    
    FUNC <- function(data, keys) {
      x <- data[Lead==0 & NLevel==MaxNL & PAWC==MaxPAWC & IrrSys=='Sprinkler']
      return(round(x$InCropRain/x$TotalETo,4))
    }
    Keys1 <- setdiff(c(Factors,'SowYear'),c('Scen','Lead','Prob','MaxErr','DelayFrac','NLevel','PAWC','IrrSys'))
    Keys2 <- setdiff(c(Keys1,'NLevel','PAWC','IrrSys'),c('Scen','Rep'))
    
    Temp <- copy(Seasonal[Scen=='Base'])
    Temp[, WBR:=FUNC(.SD, .BY), keyby=Keys1, .SDcols=c('InCropRain','TotalETo','Lead','NLevel','PAWC','IrrSys')]
    Seasonal <- merge(Seasonal, Temp[, c(Keys2,'WBR'), with=F], by=Keys2, sort=F)
    
    setkeyv(Seasonal, cols=c('SimulationID',Factors,'SowYear'))
    
    WBRBreaks <- quantile(Seasonal$WBR, seq(0,1,length.out=4))
    WBRBreaks[1] <- 0; WBRBreaks[4] <- Inf
    Seasonal[, WBClass:=as.numeric(cut(WBR, breaks=WBRBreaks, right=T))]
    Seasonal[, WBClass:=c('Dry','Normal','Wet')[WBClass]]
    Seasonal[, WBClass:=factor(WBClass, c('Dry','Normal','Wet'))]
    
    WBRBreaks2 <- diff(quantile(Seasonal$WBR, c(0,0.95)))/3
    WBRBreaks2 <- quantile(Seasonal$WBR, 0) + c(1,2)*WBRBreaks2
    WBRBreaks2 <- c(0, WBRBreaks2, Inf)
    Seasonal[, WBClass2:=as.numeric(cut(WBR, breaks=WBRBreaks2, right=T))]
    Seasonal[, WBClass2:=c('Dry','Normal','Wet')[WBClass2]]
    Seasonal[, WBClass2:=factor(WBClass2, c('Dry','Normal','Wet'))]

    rm(Temp); x <- gc()
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
    WhatInOneFile <- c('Factors','WBRBreaks','WBRBreaks2','Countries')
    
    if (!is.null(What)) What <- matrix(What,ncol=2,byrow=T)
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
    
    x <- gc()
    
    WBColors <- c('darkred','chartreuse4','navy')
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
  message('Plot (Bar): WBClasses')
  {
    for (i in 1:2) {
      iClass <- ifelse(i==1, 'WBClass', 'WBClass2')
      iBreaks <- if (i==1) WBRBreaks else WBRBreaks2
      
      Temp <- Seasonal[Scen=='Base' & NLevel==MaxNL & PAWC==MaxPAWC & IrrSys=='Sprinkler']
      
      if (i==1) {
        Temp2 <- Add_Aggregate_Unit(Temp, Col='Site', AggUnit='All Sites', PutLast=F, AsFactor=T)
        
        ExpPath <- paste0(Dir$Analyses, '/00 - Density - WBR Distribution.pdf')
        
        p <- DoGGPlot(Data=Temp2,Type='densityarea',xCol='WBR',fCol='Site',hFacet='Site',FacetScale='free_y',
                      FacetGridAxes='all_x',FacetGridLabs='all_x',NRow=3,BinWidth=0.10,
                      RemPanMarg=F,PanLw=0.15,xLab='Water balance ratio (WBR)',yLab='Density',xAngle=0,yAngle=0,FColors=MyColors$BkPrYl(12),
                      VLine=quantile(Temp$WBR, c(0,0.333,0.66667,0.95,0.99)),VLineW=0.2,VLineCol='darkred',VLineType=3,
                      RemGrids=T,RemMinGrids=T,RemStripBack=T,AxTxSize=3.5,AxTiSize=7,Alpha=0.5,GridLwd=0.05,ExpPath=ExpPath,Size=PPSize(H=0.4,W=0.8))
      }
      
      FUNC <- function(data) {
        if (nrow(data)<3) {
          Classes <- setdiff(c('Dry','Normal','Wet'),data[,get(iClass)])
          x <- data.table(XXX=Classes, WBR=0)
          colnames(x)[1] <- iClass
          data <- rbind(data, x)
        }
        return(data)
      }
      
      Temp <- Temp[, lapply(.SD, median), keyby=c('Site','SowDate',iClass), .SDcols='WBR']
      Temp <- Temp[, FUNC(.SD), keyby=c('Site','SowDate')]
      
      p1 <- DoGGPlot(Data=Temp,Type='bar',xCol='SowDate',yCol='WBR',gCol=c('SowDate',iClass),fCol=iClass,FacetScale='free_x',
                     hFacet='Site',vFacet=NULL,hFacetPref=NULL,vFacetPref=T,RemPanMarg=T,PSize=0.7,PanLw=0.15,
                     yLab='Median water balance ratio',xAngle=0,yAngle=0,CColors='black',FColors=WBColors,
                     RemGrids=T,RemMinGrids=T,RemStripBack=T,AxTxSize=2.5,AxTiSize=5,StripSizes=3,HLine=iBreaks[c(2,3)],HLineCol='darkred',HLineType=2,
                     GridLwd=0.05,LegPos='top',LegWidth=0.4,LegLength=0.4,LegKeyText=4.5,LegMarg=margin(t=0.02,b=0.2,unit='cm'),BarWidth=0.75,BarDodge=0.75)
      
      Temp <- FreqTableXD(Seasonal[Scen=='Base'], Cols=c('Site','SowDate',iClass), IncludeZero=T)
      
      p2 <- DoGGPlot(Data=Temp,Type='bar',xCol='SowDate',yCol='Prob',gCol=c('SowDate',iClass),fCol=iClass,FacetScale='free_x',
                     hFacet='Site',vFacet=NULL,hFacetPref=NULL,vFacetPref=T,RemPanMarg=T,PSize=0.7,PanLw=0.15,
                     yLab='Frequency (%)',xAngle=0,yAngle=0,CColors='black',FColors=WBColors,
                     RemGrids=T,RemMinGrids=T,RemStripBack=T,AxTxSize=2.5,AxTiSize=5,StripSizes=3,GridLwd=0.05,
                     LegPos='top',LegWidth=0.4,LegLength=0.4,LegKeyText=4.5,LegMarg=margin(t=0.02,b=0.2,unit='cm'),BarWidth=0.75,BarDodge=0.75)
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/00 - Bar - WBR and WB Classes - %s.pdf', ifelse(i==1, 'Terciles', 'Uniform')))
      x <- Plot_Grid(list(p1,p2), LegPos='top', NRow=2, NCol=1, RemLabs=F, CommLegend=T, ExpPath=ExpPath, Size=PPSize(H=0.3,W=0.63))
    }
    
    rm(Temp, Temp2); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Bar): Averages')
  {
    for (Type in 1:2) {
      
      if (Type==1) {
        # For Site x SowDate Combinations
        Keys1 <- c(setdiff(Factors,'Rep'), 'SowYear')
        
      } else if (Type==2) {
        # For WBClasses
        Keys1 <- c(setdiff(Factors,'Rep'), 'SowYear', 'WBClass')
      }
      
      Cols1 <- intersect(names(Traits), names(Seasonal))
      
      Temp <- Seasonal[Prob!=0, lapply(.SD, mean, na.rm=T), keyby=Keys1, .SDcols=Cols1]
      
      Temp[, `:=`(Prob=factor(Prob,levels=c('-1','20','40','60','80')), MaxErr=factor(MaxErr,levels=c('-1','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
      
      Temp[, `:=`(PAWC=as.numeric(as.character(PAWC)))]
      Temp[, `:=`(NLevel=as.numeric(as.character(NLevel)))]
      Temp[is.na(DelayFrac), DelayFrac:='NA']
      Temp[, `:=`(DelayFrac=factor(DelayFrac, levels=c('NA',0.5,1)))]
      
      Temp[, PAWCSowN:=sprintf('PAWC=%s | NLevel=%s', PAWC, NLevel)]
      Temp[, PAWCSowN:=factor(PAWCSowN, levels=apply(Temp[, expand.grid(sort(as.numeric(unique(PAWC))),sort(as.numeric(unique(NLevel))))], 
                                                     1, function(x) sprintf('PAWC=%s | NLevel=%s', x[1], x[2])))]
      
      if (Type==1) {
        xCol   <- 'SowDate'
        gCol   <- c('SowDate','PAWCSowN')
        hFacet <- c('Site','DelayFrac')
        vFacet <- 'IrrSys'
        hFacetPref <- c(NA, 'DelayFrac: ')
        StripSizes <- 3.5
        Size  <- PPSize(H=0.3,W=0.6)
        
      } else if (Type==2) {
        xCol   <- 'WBClass'
        gCol   <- c('WBClass','PAWCSowN')
        hFacet <- c('Scen','Lead','DelayFrac')
        vFacet <- 'IrrSys'
        hFacetPref <- T
        StripSizes <- 4.2
        Size  <- PPSize(H=0.3,W=0.6)
      }
      
      AxTxSize <- 4
      LegKeyText <- 3.8
      LegWidth <- LegLength <- 0.43
      
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
                          hFacet=hFacet,vFacet=vFacet,hFacetPref=hFacetPref,vFacetPref=T,PSize=0.7,PanLw=0.15,BarWidth=0.75,BarDodge=0.75,
                          yLab=yLab,xAngle=0,yAngle=0,CColors='black',FColors=MyColors$ET4,RemGrids='x',DropFacets=F,CheckFacets=F,
                          RemPanMarg=T,RemMinGrids=T,RemStripBack=T,StripSizes=StripSizes,AxTxSize=AxTxSize,ErrBarType='2se',
                          GridLwd=0.05,LegPos='top',LegWidth=LegWidth,LegLength=LegLength,LegKeyText=LegKeyText,
                          Caption=sprintf('Country: %s', ctr),TitleSizes=5)
            
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
                        hFacet=hFacet,vFacet=vFacet,hFacetPref=hFacetPref,vFacetPref=T,PSize=0.7,PanLw=0.15,
                        yLab=yLab,xAngle=30*(Type==1),yAngle=0,CColors='black',FColors=MyColors$ET4,RemGrids='x',
                        RemPanMarg=T,RemMinGrids=T,RemStripBack=T,StripSizes=StripSizes,AxTxSize=AxTxSize,GridLwd=0.05,
                        LegPos='top',LegWidth=LegWidth,LegLength=LegLength,LegKeyText=LegKeyText,ErrBarType='2se',BarWidth=0.75,BarDodge=0.75)
          
          Plots[[length(Plots)+1]] <- p
        }
        
        ExpPath <- paste0(Dir$Analyses, sprintf('/01 - Bar - Averages - Type %s - All Variables.pdf', Type))
        x <- Print_Plots_to_File(Plots, File=ExpPath, Size=Size)
      }
    }
    
    rm(Temp, xData); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Box): Forecast Impact for WBClasses')
  {
    Keys1 <- c(Factors, 'WBClass', 'SowYear')
    Keys2 <- c(setdiff(Factors,'Rep'), 'WBClass', 'SowYear')
    
    Cols1 <- intersect(names(Traits), names(Seasonal))
    Cols2 <- setdiff(Cols1, c('TotalETc','TotalETo','InCropRain'))
    
    Temp <- CalcAbsRelChanges(Seasonal[MaxErr%in%c(-1,0,100)], Cols=Cols1, Indices=setdiff(Keys1, 'Scen'), 
                              RefIndices=c('Rep','Lead','Prob','MaxErr','DelayFrac'), RefIndicesVal=c(1,0,-1,-1,-1), TakeMean=F, RemoveBase=T)
    
    testthat::expect(all(Temp$Scen!='Base'), failure_message='Base must not be present in Temp!')
    
    Temp <- Temp[, lapply(.SD, mean), keyby=Keys2, .SDcols=c(Cols1,paste0('Rel',Cols1),paste0('Abs',Cols1))]
    
    Temp[, `:=`(Prob=factor(Prob,levels=c('0','20','40','60','80')), MaxErr=factor(MaxErr,levels=c('0','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
    
    Temp[, `:=`(PAWC=as.numeric(as.character(PAWC)))]
    Temp[, `:=`(NLevel=as.numeric(as.character(NLevel)))]
    
    Temp[, PAWCSowN:=sprintf('PAWC=%s | NLevel=%s', PAWC, NLevel)]
    Temp[, PAWCSowN:=factor(PAWCSowN, levels=apply(Temp[, expand.grid(sort(as.numeric(unique(PAWC))),sort(as.numeric(unique(NLevel))))], 
                                                   1, function(x) sprintf('PAWC=%s | NLevel=%s', x[1], x[2])))]
    
    xCol    <- 'WBClass'
    gCol    <- c('WBClass','PAWCSowN')
    
    hFacet1 <- c('Lead','DelayFrac')
    vFacet1 <- c('IrrSys')
    
    hFacet2 <- c('Prob')
    hFacetSuff2 <- '%'
    vFacet2 <- c('DelayFrac')
    vFacetSuff2 <- NULL
    
    Temp1 <- Temp[Scen=='Perf', lapply(.SD, mean), keyby=c(unique(c('Scen','Lead','IrrSys','MaxErr','DelayFrac',xCol,gCol,hFacet1,vFacet1,'SowYear'))), 
                  .SDcols=c(Cols1,paste0('Rel',Cols1),paste0('Abs',Cols1))]
    Temp1 <- Temp1[, lapply(.SD, mean), keyby=c(unique(c('Scen','Lead','IrrSys','MaxErr','DelayFrac',xCol,gCol,hFacet1,vFacet1))), 
                   .SDcols=c(Cols1,paste0('Rel',Cols1),paste0('Abs',Cols1))]
    
    Temp2 <- Temp[Scen=='Imperf', lapply(.SD, mean), keyby=c(unique(c('Scen','Lead','IrrSys','MaxErr','DelayFrac',xCol,gCol,hFacet2,vFacet2,'SowYear'))), 
                  .SDcols=c(Cols1,paste0('Rel',Cols1),paste0('Abs',Cols1))]
    Temp2 <- Temp2[, lapply(.SD, mean), keyby=c(unique(c('Scen','Lead','IrrSys','MaxErr','DelayFrac',xCol,gCol,hFacet2,vFacet2))), 
                   .SDcols=c(Cols1,paste0('Rel',Cols1),paste0('Abs',Cols1))]
    
    Plots <- list()
    for (Tr in Cols2) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp)) next
      yLab <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      p <- DoGGPlot(Data=Temp[Scen=='Perf'],Type='boxmeanpoint',xCol=xCol,yCol=yCol,gCol=gCol,fCol=gCol[2],BarLabSize=6,
                    hFacet=hFacet1,vFacet=vFacet1,hFacetPref=T,vFacetPref=T,hFacetSuff=NULL,vFacetSuff=NULL,RemPanMarg=T,PSize=0.55,PanLw=0.15,
                    xAngle=0,yAngle=0,yLab=yLab,CColors='black',FColors=MyColors$ET4,RemGrids='x',RemStripBack=T,StripSizes=4.5,AxTxSize=3.5,
                    RemMinGrids=T,HLine=0,HLineCol='darkred',HLineW=0.2,GridLwd=0.05,HVLineBack=F,LegNRow=1,LegWidth=0.6,LegKeyText=4.2,
                    BoxWidth=0.75,BoxDodge=0.75,LegPos='top')
      
      Plots[[length(Plots)+1]] <- p
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/02 - Box - Impact Perf - Type %s - All Variables.pdf', 2))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.30,W=0.62))
    }
    
    for (Tr in Cols2) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp)) next
      yLab <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      Plots <- list()
      for (ld in c(3,7)) {
        for (z in c(100)) {
          for (irrSys in c('Furrow','Sprinkler')) {
            p <- DoGGPlot(Data=Temp[Scen=='Imperf' & Lead==ld & MaxErr==z & IrrSys==irrSys],Type='boxmeanpoint',xCol=xCol,yCol=yCol,gCol=gCol,fCol=gCol[2],
                          hFacet=hFacet2,vFacet=vFacet2,hFacetPref=T,vFacetPref=T,hFacetSuff=hFacetSuff2,vFacetSuff=vFacetSuff2,RemPanMarg=T,PSize=0.55,PanLw=0.15,
                          xAngle=0,yAngle=0,yLab=yLab,CColors='black',FColors=MyColors$ET4,RemGrids='x',RemStripBack=T,StripSizes=4.5,AxTxSize=3.5,
                          RemMinGrids=T,HLine=0,HLineCol='darkred',HLineW=0.2,GridLwd=0.05,HVLineBack=F,LegNRow=1,LegWidth=0.6,LegLength=0.6,LegKeyText=5,
                          BoxWidth=0.75,BoxDodge=0.75,LegPos='top',Caption=sprintf('Irrigation: %s | Lead: %s | MaxErr: %s',irrSys,ld,z),TitleSizes=4)
            
            Plots[[length(Plots)+1]] <- p
          }
        }
      }
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/03 - Box - Impact Imperf - Type %s - %s.pdf', 2, Traits[[Tr]]$Lab))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.3,W=0.65))
    }
    
    Plots <- list()
    for (Tr in Cols2) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp1)) next
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      if (Temp1[MaxErr==0, all(is.nan(get(yCol)) | is.infinite(get(yCol)))]) next
      Temp1[MaxErr==0 & (is.nan(get(yCol)) | is.infinite(get(yCol))), (yCol):=NA]
      
      p <- DoGGPlot(Data=Temp1[MaxErr==0],Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                    hFacet=hFacet1,vFacet=vFacet1,hFacetPref=T,vFacetPref=T,hFacetSuff=NULL,vFacetSuff=NULL,
                    StripSizes=5.4,AxTxSize=4.38,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.5,TileLabDigs=1,
                    xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,RemMinGrids=T,RemStripBack=T,
                    HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                    LegWidth=0.4,LegLength=15,LegKeyText=5,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,FMidPoint=0,LegNumDigs=1)
      
      Plots[[length(Plots)+1]] <- p
    }
    
    ExpPath <- paste0(Dir$Analyses, sprintf('/04 - Tile - Impact Perf - Type %s - All Variables.pdf', 2))
    x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.35,W=0.6))
    
    for (Tr in Cols2) {
      yCol <- paste0(Traits[[Tr]]$AbsRel,Tr)
      if (!yCol %in% names(Temp2)) next
      LegTitleF <- Traits[[Tr]][[paste0(Traits[[Tr]]$AbsRel,'Title')]]
      
      Plots <- list()
      for (ld in c(3,7)) {
        for (z in c(100)) {
          for (irrSys in c('Furrow','Sprinkler')) {
            if (Temp2[Lead==ld & MaxErr==z, all(is.nan(get(yCol)) | is.infinite(get(yCol)))]) next
            Temp2[Lead==ld & MaxErr==z & (is.nan(get(yCol)) | is.infinite(get(yCol))), (yCol):=NA]
            
            p <- DoGGPlot(Data=Temp2[Lead==ld & MaxErr==z & IrrSys==irrSys],Type='tiles',xCol=xCol,yCol=gCol[2],fCol=yCol,
                          hFacet=hFacet2,vFacet=vFacet2,hFacetPref=T,vFacetPref=T,hFacetSuff=hFacetSuff2,vFacetSuff=vFacetSuff2,RemStripBack=T,
                          StripSizes=6.3,AxTxSize=5.43,ContinLegTrans=MyTrans$SymSqrt,PanLw=0.15,RemPanMarg=F,TileLabSize=1.6,TileLabDigs=1,
                          xAngle=0,yAngle=0,yLab=NULL,CColors='black',FColorsCont=Traits[[Tr]]$Colorband,RemGrids=T,
                          HLine=c(1.5,2.5,3.5,4.5,5.5),VLine=c(1.5,2.5,3.5,4.5,5.5),HVLineBack=F,HLineCol='black',VLineCol='black',ExpandAxes=F,HLineW=0.1,VLineW=0.1,
                          LegWidth=0.5,LegLength=22,LegKeyText=6,LegPos='top',ContinLeg=T,LegTitleF=LegTitleF,FMidPoint=0,LegNumDigs=1,
                          Caption=sprintf('Irrigation: %s | Lead: %s | MaxErr: %s', irrSys, ld, z))
            
            Plots[[length(Plots)+1]] <- p
          }
        }
      }
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/05 - Tile - Impact Imperf - Type %s - %s.pdf', 2, Traits[[Tr]]$Lab))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.45,W=0.80))
    }
    
    rm(Temp, Temp2); x <- gc()
  }
  #=========================================================================================================================
  message('Plot (Point): Yield vs Yield')
  {
    Keys1 <- c(Factors, 'WBClass', 'SowYear')
    Keys2 <- c(setdiff(Factors,'Rep'), 'WBClass', 'SowYear')
    Cols1 <- 'Yield'  # intersect(names(Traits), names(Seasonal))
    
    Temp <- CalcAbsRelChanges(Seasonal[Scen!='Perf' & MaxErr%in%c(-1,100) & DelayFrac%in%c(-1,0.5) & (Prob%in%c(40,80) | Prob==-1)], 
                              Cols=Cols1, Indices=setdiff(Keys1, 'Scen'), RefIndices=c('Rep','Lead','Prob','MaxErr','DelayFrac'), 
                              RefIndicesVal=c(1,0,-1,-1,-1), TakeMean=F, RemoveBase=T)
    
    Temp[, `:=`(Prob=factor(Prob,levels=c('0','20','40','60','80')), MaxErr=factor(MaxErr,levels=c('0','100')), DelayFrac=factor(DelayFrac,levels=c('0.5','1')))]
    
    Temp[, `:=`(PAWC=as.numeric(as.character(PAWC)))]
    Temp[, `:=`(NLevel=as.numeric(as.character(NLevel)))]
    
    Temp[, PAWCSowN:=sprintf('PAWC=%s | NLevel=%s', PAWC, NLevel)]
    Temp[, PAWCSowN:=factor(PAWCSowN, levels=apply(Temp[, expand.grid(sort(as.numeric(unique(PAWC))),sort(as.numeric(unique(NLevel))))], 
                                                   1, function(x) sprintf('PAWC=%s | NLevel=%s', x[1], x[2])))]
    
    Temp[, Yield2:=Yield*DivYield]
    
    Temp2 <- Temp[, lapply(.SD, mean), keyby=Keys2, .SDcols=c('Yield','Yield2')]
    
    Temp2[, PAWCSowN:=sprintf('PAWC=%s | NLevel=%s', PAWC, NLevel)]
    Temp2[, PAWCSowN:=factor(PAWCSowN, levels=apply(Temp2[, expand.grid(sort(as.numeric(unique(PAWC))),sort(as.numeric(unique(NLevel))))], 
                                                   1, function(x) sprintf('PAWC=%s | NLevel=%s', x[1], x[2])))]
    
    Temp[, WBClass:=as.factor(WBClass)]
    Temp2[, WBClass:=as.factor(WBClass)]
    Sites <- c('All', sort(unique(Temp$Site)))
    
    for (irrSys in c('Furrow','Sprinkler')) {
      for (pr in c(40,80)) {
        Plots <- list()
        
        for (st in Sites) {
          TempX <- if (st=='All') Temp2 else Temp[Site==st]
          
          p <- DoGGPlot(Data=TempX[Prob==pr & IrrSys==irrSys],Type='point',xCol='Yield',yCol='Yield2',cCol='WBClass',rgcCol='WBClass',
                        hFacet='Lead',vFacet='PAWCSowN',hFacetPref=T,hFacetSuff=' days',RemStripBack=T,StripSizes=c(7.5,6),
                        PSize=0.6,LegScale=5,AxTxSize=5.43,PanLw=0.15,RemPanMarg=F,RemGrids=T,LegDrop=F,AxTiSize=8,
                        xLab='Base yield',yLab='Yield with forecast',xAngle=0,yAngle=0,CColors=WBColors, #c('brown4','blue4')
                        ShowReg=F,RegType=0,AddRegEq='Eq-R',RegEqCol=T,RegEqStat=c('MRE','N'),Add45L=T,RegEqSize=1.6,RegEqMarg=8,RegEqBold=T,RegDrop=F,
                        LegWidth=0.5,LegLength=0.5,LegKeyText=7,LegPos='top',LegTitleC=NULL,LegTitPos='left',
                        Caption=sprintf('Site: %s', st),TitleSizes=5)
          
          Plots[[length(Plots)+1]] <- p
        }
        
        ExpPath <- paste0(Dir$Analyses, sprintf('/06 - Point - Yield vs Yield - %s - Prob %s.pdf', irrSys, pr))
        x <- Print_Plots_to_File(Plots, File=ExpPath, Size=PPSize(H=0.60,W=0.65))
      }
    }
  }
  #=========================================================================================================================
  message('Plot (Point): Correlation')
  {
    for (Type in 2:2) {
      
      if (Type==1) {
        # For Site x SowDate Combinations
        Keys1 <- c(Factors, 'SowYear')
        
      } else if (Type==2) {
        # For WBClasses
        Keys1 <- c(Factors, 'WBClass', 'SowYear')
      }
      
      Cols1 <- intersect(c(names(Traits), ForcNumCols), names(Seasonal))
      Cols1 <- setdiff(Cols1, c('TotalETc','TotalETo','InCropRain'))
      
      Temp <- CalcAbsRelChanges(Seasonal[DelayFrac%in%c(-1,0.5) & MaxErr%in%c(-1,100)], Cols=Cols1, Indices=setdiff(Keys1,'Scen'), 
                                RefIndices=c('Rep','Lead','Prob','MaxErr','DelayFrac'), RefIndicesVal=c(1,0,-1,-1,-1), TakeMean=F, RemoveBase=T)
      
      testthat::expect(all(!Temp$Scen%in%c('Base','Perf')), failure_message='Temp must only have Imperf scenario!')
      
      Temp[, `:=`(PAWC=as.numeric(as.character(PAWC)))]
      Temp[, `:=`(NLevel=as.numeric(as.character(NLevel)))]
      
      CorColsX <- intersect(ForcNumCols, names(Temp))
      CorColsY <- intersect(TraitsAbsRel, names(Temp))
      
      Temp <- RenameCols(Temp, data.frame(old=paste0('Abs',ForcNumCols), new=ForcNumCols))
      
      if (Type==1) {
        hFacet <- c('Site','PAWC','SowDate')
        Size   <- PPSize(H=0.76,W=0.95)
        
      } else if (Type==2) {
        hFacet <- c('Lead','PAWC','WBClass')
        Size   <- PPSize(H=0.63,W=1.3)
      }
      
      Plots <- list()
      for (irrSys in c('Furrow','Sprinkler')) {
        for (z in c(100)) {
          p <- DoGGPlot(Data=Temp[MaxErr==z & IrrSys==irrSys],Type='corr',corCols=list(CorColsX,CorColsY),
                        hFacet=hFacet,vFacet='NLevel',hFacetPref=T,vFacetPref=T,RemStripBack=T,StripSizes=c(6,7.5),
                        PShape=22,PSize=7.4,BarLabSize=1.5,RemGrids=T,xAngle=25,yAngle=0,PThr=0.05,
                        PanLw=0.15,RemPanBorder=F,RemAxesLines=F,RemPanMarg=T,AxTxSize=6,
                        LegWidth=0.45,LegLength=28,LegKeyText=6,LegPos='top',LegLimits=c(-1,1),LegTitleF='Correlation',
                        Caption=sprintf('Irrigation: %s | MaxErr: %s | DelayFrac: %s', irrSys, z, 0.5))
          
          Plots[[length(Plots)+1]] <- p
        }
      }
      
      ExpPath <- paste0(Dir$Analyses, sprintf('/07 - Point - Correlation - Type %s.pdf', Type))
      x <- Print_Plots_to_File(Plots, File=ExpPath, Size=Size)
    }
    
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




