

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

#=========================================================================================================================
# Project Details
#=========================================================================================================================
{
  Project    <- '2020.09.28 Compound Dry-Hot Extremes'
  Analysis   <- '2020.11.13 TMax 1D'
}
#=========================================================================================================================
# Parameters
#=========================================================================================================================
{
  Mesh     <- 1
  
  JustLoad <- F # Just load the data or process the data?
  DoGraphs <- T
  UseGrid  <- T
  
  AreaOrd  <- c('North-East','South-East','South','West','Australia')
  Vars     <- c('maxt','rain') # meantx
  DivYears <- c(1930,1960,1990)
  Periods <- c('1930-1959','1960-1989','1990-2019','1889-1989')
  YTypes  <- c('Hot Years','Dry Years','Hot and Dry Years')
  Trends  <- c('Decreasing','Insignificant','Increasing')
  Seasons <- c('Annual','DJF','MAM','JJA','SON','A2O')
  MinYear <- 0
  MaxYear <- 2019
  Alpha   <- 0.05
  
  SowingOrd   <- format(seq(as.Date('2000-03-01'),as.Date('2000-07-31'),2),format('%d-%b'))
  
  StateMap <- data.frame(Old=c('South Australia','Northern Territory','Queensland','Victoria','Western Australia','New South Wales','Tasmania','Australia'),
                         New=c('SA','NT','QLD','VIC','WA','NSW','TAS','Australia'))
  
  InpFilePat <- 'Outputs'
  OutFilePat <- 'HotDryExtr'
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  if (OnServer) MainDir <- '/gpfs1/scratch/90days/uqbababa/Apsim2/' else MainDir <- 'C:/Users/uqbababa/OneDrive - The University of Queensland/'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySource.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyFunctions.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyApsim.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyPlots.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/TrendAnalysis.R',sep='/'))
  
  Dir <- APSIM_Paths2(Dir=list(),Project=Project,RunSet=NULL,MainDir=MainDir,Analysis=Analysis)
  x <- sapply(Dir, function(x) {if(!dir.exists(x)) dir.create(x,recursive=T)})
  cat("\014")
}
LineX()
#========================================================================================================
message('Importing yield and weather station data')
{
  load(file=paste0(Dir$DataShared,'/SimFilesFull.RData'))
  load(file=paste0(Dir$Data,sprintf('/GridData - %sD/Grid.RData',Mesh)))
  
  if (!require(VineCopula)) install.packages("VineCopula")
  library(VineCopula)
  if (!require(copula)) install.packages("copula")
  library(copula)
  if (!require(GA)) install.packages("GA")
  library(GA)
  if (!require(ape)) install.packages("ape")
  library(ape)
  if (!require(spdep)) install.packages("spdep")
  library(spdep)
  if (!require(MASS)) install.packages("MASS")
  library(MASS)
  if (!require(kdecopula)) install.packages("kdecopula")
  library(kdecopula)
}
LineX()
#========================================================================================================

if (JustLoad) {
  message('Importing the data from previous run')
  LineX()
  
  x <- MyLoad(File=NULL,What=NULL,FileFormat=OutFilePat,Folder=Dir$Analyses,Sep='_',Exclude='Daily')
  
} else {
  
  #=========================================================================================================================
  message("Importing weather data and calculating annual and seasonal averages")
  {
    Files <- list.files(paste0(Dir$Data,ifelse(UseGrid,sprintf('/GridData - %sD',Mesh),NULL)),pattern='*.met',full.names=T)
    AllDataD <- APSIM_Read_Weather_Files(Files,ExpComments=F,HeadPatt='^year')
    for (St in names(AllDataD)) AllDataD[[St]][, IsGrid:=T]
    
    Files2 <- list.files(paste0(Dir$Weather,'/Base'),pattern='*.met',full.names=T)
    AllDataD2 <- APSIM_Read_Weather_Files(Files2,ExpComments=F,HeadPatt='^year')
    for (St in names(AllDataD2)) AllDataD2[[St]][, IsGrid:=F]
    AllDataD <- c(AllDataD,AllDataD2)
    
    AllData  <- list()
    for (St in names(AllDataD)) {
      iSite  <- File_Base_Name(St)
      XWData <- AllDataD[[St]]
      XWData[, `:=`(meant=0.5*(maxt+mint),meantx=(0.75)*maxt+(0.25)*mint)]
      XWData <- cbind(DOY2DateComps(XWData[,1:2]),XWData)[,-c('year','code')]
      # Seasons: Summer: DJF, Autumn: MAM, Winter: JJA and Spring: SON
      XWData[Mon!=12, Year2:=Year]
      XWData[Mon==12, Year2:=Year+1]
      XWData[, Seas:=ifelse(Mon<=2|Mon==12,1, ifelse(Mon<=5,2, ifelse(Mon<=8,3, ifelse(Mon<=11,4,0))))]
      
      # WData1 <- XWData[, lapply(.SD,function(x) round(mean(x,na.rm=T),2)), keyby=c('Year2','Seas','Mon'), .SDcols=c('radn','meant','meantx','maxt','mint','evap')]
      # WData2 <- XWData[, lapply(.SD,function(x) round(sum(x,na.rm=T),2)), keyby=c('Year2','Seas','Mon'), .SDcols=c('rain')]
      # XWData1 <- cbind.data.frame(WData1,rain=WData2$rain)
      
      WData1 <- XWData[, lapply(.SD,function(x) round(mean(x,na.rm=T),2)), keyby=c('Year2','Seas'), .SDcols=c('radn','meant','meantx','maxt','mint','evap')]
      WData2 <- XWData[, lapply(.SD,function(x) round(sum(x,na.rm=T),2)), keyby=c('Year2','Seas'), .SDcols=c('rain')]
      WData2 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData2 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData2[, `:=`(Mon=0)]
      XWData2 <- Mapping_Cols(XWData2,data.table(old='Year2',new='Year'))
      
      WData1 <- XWData[, lapply(.SD,function(x) round(mean(x,na.rm=T),2)), keyby=c('Year'), .SDcols=c('radn','meant','meantx','maxt','mint','evap')]
      WData2 <- XWData[, lapply(.SD,function(x) round(sum(x,na.rm=T),2)), keyby=c('Year'), .SDcols=c('rain')]
      WData2 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData3 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData3[, `:=`(Seas=0,Mon=0)]
      
      WData1 <- XWData[between(Mon,4,10), lapply(.SD,function(x) round(mean(x,na.rm=T),2)), keyby=c('Year'), .SDcols=c('radn','meant','meantx','maxt','mint','evap')]
      WData2 <- XWData[between(Mon,4,10), lapply(.SD,function(x) round(sum(x,na.rm=T),2)), keyby=c('Year'), .SDcols=c('rain')]
      WData2 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData4 <- cbind.data.frame(WData1,rain=WData2$rain)
      XWData4[, `:=`(Seas=5,Mon=0)]
      
      WData <- rbind(XWData2,XWData3,XWData4)
      WData <- WData[between(Year,MinYear,MaxYear,incbounds=T)]
      AllData[[St]] <- WData[, Site:=iSite]
    }
    
    AllData <- rbindlist(AllData)
    
    if (UseGrid) {
      AllData <- merge(AllData,Stations[,.(Name,ID,Lat,Long,IsGrid,State,State2)],by.x='Site',by.y='Name')
      AllData$Name <- AllData$Site
      
    } else {
      AllData <- merge(AllData,Stations,by.x='Site',by.y='NameID')
    }
    
    setkeyv(AllData,cols=c('IsGrid','Site','Year','Seas'))
  }
  #=========================================================================================================================
  LineX()
  message("Saving the raw data")
  {
    if (is.null(TempFold)) Folder <- Dir$Analyses else Folder <- TempFold
    
    What <- c('AllDataD')
    WhatInOneFile <- NULL
    
    SaveFiles <- MySave(FileFormat=OutFilePat,Folder=Folder,What=What,WhatInOneFile=WhatInOneFile)
    
    if (!is.null(TempFold)) {
      x <- file.copy(from=SaveFiles,to=Dir$Analyses,overwrite=T)
      x <- unlink(SaveFiles,recursive=T,force=T)
    }
  }
  #=========================================================================================================================
  message("Fitting copulas")
  {
    # Calculate return periods for all months, seasons and years
    FuncRP <- function(WData2) {
      # Ft <- ecdf(WData2[[1]])(WData2[[1]])
      # Fr <- ecdf(-WData2[[2]])(-WData2[[2]])
      prob_t <- pobs(WData2[[1]])
      prob_r <- pobs(-WData2[[2]])
      
      # mycop <- BiCopSelect(prob_t,prob_r,familyset=NA,selectioncrit='BIC')
      # decop <- kdecop(cbind(prob_t,prob_r),knots=50,info=F)
      # PR    <- BiCopCDF(prob_t,prob_r,obj=mycop)
      # PRD   <- pkdecop(cbind(prob_t,prob_r), decop)
      TP  <- cbind(WData2[[1]],-WData2[[2]])
      FF  <- apply(TP,1,function(x) sum(TP[,1]<=x[1] & TP[,2]<=x[2])/nrow(TP))
      RP  <- 1/(1-prob_t-prob_r+FF)
      RPt <- prob2T(prob_t)
      RPr <- prob2T(prob_r)
      if (any(is.infinite(RP))) {
        print(WData2[is.infinite(RP)])
      }
      
      x <- data.table(Year=WData2[[3]],RP=round(RP,1),RPt=round(RPt,1),RPr=round(RPr,1))
      return(x)
    }
    
    AllData[, c('Year','RP','RPt','RPr'):=FuncRP(.SD), keyby=c('IsGrid','Name','ID','Seas','Mon'), .SDcols=c(Vars,'Year')]
    
    AllData[, DryHot25:=(RP>25)]
    AllData[, DryHot50:=(RP>50)]
    AllData[, Hot25:=RPt>25]
    AllData[, Hot50:=RPt>50]
    AllData[, Dry25:=RPr>25]
    AllData[, Dry50:=RPr>50]
    
    AllDataState <- Add_Aggregate_Unit(AllData[IsGrid==T],Col='State2',AggUnit='Australia')
    AllDataState <- AllDataState[, lapply(.SD,mean,na.rm=T), keyby=c('State2','Seas','Mon','Year'), .SDcols=Vars]
    AllDataState[, c('Year','RP','RPt','RPr'):=FuncRP(.SD), keyby=c('State2','Seas','Mon'), .SDcols=c(Vars,'Year')]
    
    AllDataState[, DryHot25:=(RP>25)]
    AllDataState[, DryHot50:=(RP>50)]
    AllDataState[, Hot25:=RPt>25]
    AllDataState[, Hot50:=RPt>50]
    AllDataState[, Dry25:=RPr>25]
    AllDataState[, Dry50:=RPr>50]
  }
  #=========================================================================================================================
  message("Calculating potential yield return periods")
  {
    load("C:/Users/uqbababa/OneDrive - The University of Queensland/00_Projects/2020.07.19 Frost Insurance/03_Analyses/2020.08.13/FrostInsuranceRaw_Seasonal.RData")
    Seasonal <- Seasonal[Run=='WithoutIrr']
    Factors <- c('SowDate','HtLowThrsh')
    Seasonal$WFrostLoss <- Seasonal$`FrostLoss(2)`
    Seasonal$WNTLimitedYield <- Seasonal$HeatedYield * (1-Seasonal$WFrostLoss/100)
    if ('WNLimitedYield' %in% names(Seasonal)) Seasonal[, `:=`(WNLimitedYield=NULL,WNLimitedYield.x=NULL,WNLimitedYield.y=NULL)]
    Seasonal  <- Seasonal[HtLowThrsh==26]
    if ('BestInYear' %in% names(Seasonal)) Seasonal[, `:=`(BestInYear=NULL)]
    Cols <- c('Climate','Run','Area','Site','SimID','Lat','Long','SowYear',setdiff(Factors,'SowDate'))
    Seasonal <- Seasonal[, BestInYear:=(between(WNTLimitedYield,
                                                quantile(.SD$WNTLimitedYield,0.5,na.rm=T),
                                                quantile(.SD$WNTLimitedYield,1.0,na.rm=T),incbounds=T)), keyby=Cols]
    SowDs <- c("12-May","14-May","16-May","18-May")
    Seasonal$yield <- Seasonal$WNTLimitedYield
    Seasonal1 <- Seasonal[BestInYear==T, lapply(.SD,mean,na.rm=T), keyby=c('Area','Site','SimID','Lat','Long','SowYear'), .SDcols=c('biomass','yield')]
    Seasonal2 <- Seasonal[SowDate%in%SowDs, lapply(.SD,mean,na.rm=T), keyby=c('Area','Site','SimID','Lat','Long','SowYear'), .SDcols=c('biomass','yield')]
    Seasonal2$yield2   <- Seasonal2$yield
    Seasonal2$biomass2 <- Seasonal2$biomass
    Seasonal1 <- merge(Seasonal1,Seasonal2[,-c('yield','biomass')],by=c('Area','Site','SimID','SowYear','Lat','Long'),sort=F)
    Seasonal <- copy(Seasonal1)[SowYear>=1990]
    
    # Calculate return periods for all months, seasons and years
    FuncYRP <- function(WData2) {
      RP <- prob2T(pobs(-WData2$yield))
      x <- data.table(RP=round(RP,1))
      return(x)
    }
    
    Seasonal[, c('RPY'):=FuncYRP(.SD), keyby=c('Site','SimID'), .SDcols=c('yield','SowYear')]
    Seasonal[, LowYield25:=(RPY>25)]
    Seasonal[, LowYield50:=(RPY>50)]
    
    Seasonal$Site <- NULL
    Seasonal <- merge(Seasonal,Stations[IsGrid==F,.(Lat,Long,Name)],by=c('Lat','Long'))
    Seasonal <- merge(AllData[IsGrid==F,-c('Site','Lat','Long')],Seasonal,by.x=c('Name','Year'),by.y=c('Name','SowYear'),sort=F)
    Seasonal[, Area:=Change_Levels(Area,OldLevs='East',NewLevs='North-East')]
    Seasonal[, c('Year','RP','RPt','RPr'):=FuncRP(.SD), keyby=c('IsGrid','Name','ID','Seas','Mon'), .SDcols=c(Vars,'Year')]
  }
  #=========================================================================================================================
  message("Trend analysis of return periods")
  {
    MainCols <- c('RP','RPt','RPr',Vars)
    RPTrends <- list()
    
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows <- which(between(AllData$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      
      RPTrends[[i]] <- Trend_Analysis(AllData[Rows],LevCols=c('IsGrid','State2','Name','ID','Seas','Mon'),TimeCol='Year',MainCols=MainCols,WeightCol=NULL,
                                      Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')[,Period:=iPeriod]
    }
    RPTrends <- rbindlist(RPTrends)
    RPTrends$IsTrend <- as.numeric(RPTrends$YP_P<=Alpha)*sign(RPTrends$LN_SL)
    
    RPTrendsState <- list()
    
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows <- which(between(AllData$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      
      RPTrendsState[[i]] <- Trend_Analysis(AllDataState[Rows],LevCols=c('State2','Seas','Mon'),TimeCol='Year',MainCols=MainCols,WeightCol=NULL,
                                           Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')[,Period:=iPeriod]
    }
    RPTrendsState <- rbindlist(RPTrendsState)
    RPTrendsState$IsTrend <- as.numeric(RPTrendsState$YP_P<=Alpha)*sign(RPTrendsState$LN_SL)
    
    MainCols <- c('RP','RPt','RPr','RPY','yield','yield2')
    RPYTrends <- Trend_Analysis(Seasonal,LevCols=c('Area','State2','Name','SimID','Lat','Long','Seas'),TimeCol='Year',MainCols=MainCols,WeightCol=NULL,
                                Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    RPYTrends$IsTrend <- as.numeric(RPYTrends$YP_P<=Alpha)*sign(RPYTrends$LN_SL)
  }
  #=========================================================================================================================
  message("Trend analysis of number of stations affected")
  {
    Area <- list()
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows <- which(between(AllData$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      
      if (length(Rows)==0) next()
      Temp <- Add_Aggregate_Unit(AllData[Rows],Col='State2',AggUnit='Australia')
      Cols <- c('IsGrid','State2','Year','Seas','Mon')
      
      Area[['DryHot25']] <- rbind(Area[['DryHot25']], FreqTableXD(Temp,Cols=c(Cols,'DryHot25'),Prob=T)[,Period:=iPeriod])
      Area[['DryHot50']] <- rbind(Area[['DryHot50']], FreqTableXD(Temp,Cols=c(Cols,'DryHot50'),Prob=T)[,Period:=iPeriod])
      Area[['Hot25']] <- rbind(Area[['Hot25']], FreqTableXD(Temp,Cols=c(Cols,'Hot25'),Prob=T)[,Period:=iPeriod])
      Area[['Hot50']] <- rbind(Area[['Hot50']], FreqTableXD(Temp,Cols=c(Cols,'Hot50'),Prob=T)[,Period:=iPeriod])
      Area[['Dry25']] <- rbind(Area[['Dry25']], FreqTableXD(Temp,Cols=c(Cols,'Dry25'),Prob=T)[,Period:=iPeriod])
      Area[['Dry50']] <- rbind(Area[['Dry50']], FreqTableXD(Temp,Cols=c(Cols,'Dry50'),Prob=T)[,Period:=iPeriod])
    }
    
    ArTrends <- list()
    Cols <- c('IsGrid','State2','Seas','Mon','Period')
    
    ArTrends[['DryHot25']] <- Trend_Analysis(Area[['DryHot25']][DryHot25==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                             Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    ArTrends[['DryHot50']] <- Trend_Analysis(Area[['DryHot50']][DryHot50==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                             Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    ArTrends[['Hot25']] <- Trend_Analysis(Area[['Hot25']][Hot25==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                          Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    ArTrends[['Hot50']] <- Trend_Analysis(Area[['Hot50']][Hot50==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                          Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    ArTrends[['Dry25']] <- Trend_Analysis(Area[['Dry25']][Dry25==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                          Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    ArTrends[['Dry50']] <- Trend_Analysis(Area[['Dry50']][Dry50==T],LevCols=Cols,TimeCol='Year',MainCols='Freq',WeightCol=NULL,
                                          Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T,ChangePointMethods='SNH')
    
    for (x in names(ArTrends)) ArTrends[[x]]$IsTrend <- as.numeric(ArTrends[[x]]$YP_P<=Alpha)*sign(ArTrends[[x]]$LN_SL)
  }
  #=========================================================================================================================
  message("Calculation of Morans-I")
  {
    # Calculation of Morans-I
    FuncMor <- function(data) {
      data[[1]] <- as.numeric(data[[1]])
      Dist <- as.matrix(dist(cbind(data$Long,data$Lat)))
      DistInv <- 1/Dist
      diag(DistInv) <- 0
      DistInv[is.infinite(DistInv)] <- 0
      if (nrow(data)<2) return(data.table(Moran=0,MoranP=1))
      if (length(unique(data[[1]]))==1) return(data.table(Moran=1,MoranP=0))
      m <- Moran.I(data[[1]],DistInv)
      if (!is.nan(m$p.value)) return(data.table(Moran=round(m$observed,2),MoranP=round(m$p.value,4))) else return(data.table(Moran=0,MoranP=1))
    }
    
    Moran <- list()
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows <- which(between(AllData$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      
      for (V in c('Hot25','Dry25','DryHot25')) {
        Temp <- Add_Aggregate_Unit(AllData[Rows],Col='State2',AggUnit='Australia')
        Temp <- Temp[, FuncMor(.SD), keyby=c('IsGrid','State2','Year','Seas','Mon'), .SDcols=c(V,'Lat','Long')][,Period:=iPeriod]
        Temp[, Type:=V]
        Moran[[length(Moran)+1]] <- Temp
      }
    }
    Moran <- rbindlist(Moran)
    
    MorTrends <- Trend_Analysis(Moran,LevCols=c('Type','IsGrid','State2','Seas','Mon','Period'),TimeCol='Year',MainCols='Moran',WeightCol=NULL,
                                Alpha=Alpha,StartTime=-Inf,Methods=1:3,TimeFrom1=T)
    MorTrends$IsTrend <- as.numeric(MorTrends$YP_P<=Alpha)*sign(MorTrends$LN_SL)
  }
  #=========================================================================================================================
  message("Compare distributions")
  {
    FuncTest <- function(data,Col) {
      #d1 <- as.data.table(t(dts_test(data[Year<last(DivYears),get(Col)],data[Year>=last(DivYears),get(Col)], nboots=1000)))
      #c1 <- as.data.table(t(cvm_test(data[Year<last(DivYears),get(Col)],data[Year>=last(DivYears),get(Col)], nboots=1000)))
      #w1 <- as.data.table(t(wass_test(data[Year<last(DivYears),get(Col)],data[Year>=last(DivYears),get(Col)], nboots=1000)))
      k1 <- as.data.table(t(ks_test(data[Year<last(DivYears),get(Col)],data[Year>=last(DivYears),get(Col)], nboots=1000)))
      #d1[, Test:='DTS Test']
      #c1[, Test:='Cramer-von Mises Test']
      #w1[, Test:='Wasserstein Distance Test']
      k1[, Test:='Kolmogorov-Smirnov Test']
      x <- rbind(k1)
      x[, Var:=Col]
      return(x)
    }
    
    DistTest <- list()
    for (Col in c(Vars,'RP','RPt','RPr')) {
      DistTest[[Col]] <- AllData[, FuncTest(.SD,Col), keyby=c('IsGrid','State2','Name','ID','Seas','Mon')]
    }
    DistTest <- rbindlist(DistTest)
    
    DistTestMor <- Moran[, FuncTest(.SD,'Moran'), keyby=c('Type','IsGrid','State2','Seas','Mon')]
  }
  #=========================================================================================================================
  message("Frequency of dry and/or hot years")
  {
    Freq <- list()
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows <- which(between(AllData$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      
      if (length(Rows)==0) next()
      Cols <- c('IsGrid','State2','Name','ID','Seas','Mon')
      
      Freq[['DryHot25']] <- rbind(Freq[['DryHot25']], FreqTableXD(AllData[Rows],Cols=c(Cols,'DryHot25'),Prob=T)[,Period:=iPeriod])
      Freq[['DryHot50']] <- rbind(Freq[['DryHot50']], FreqTableXD(AllData[Rows],Cols=c(Cols,'DryHot50'),Prob=T)[,Period:=iPeriod])
      Freq[['Hot25']] <- rbind(Freq[['Hot25']], FreqTableXD(AllData[Rows],Cols=c(Cols,'Hot25'),Prob=T)[,Period:=iPeriod])
      Freq[['Hot50']] <- rbind(Freq[['Hot50']], FreqTableXD(AllData[Rows],Cols=c(Cols,'Hot50'),Prob=T)[,Period:=iPeriod])
      Freq[['Dry25']] <- rbind(Freq[['Dry25']], FreqTableXD(AllData[Rows],Cols=c(Cols,'Dry25'),Prob=T)[,Period:=iPeriod])
      Freq[['Dry50']] <- rbind(Freq[['Dry50']], FreqTableXD(AllData[Rows],Cols=c(Cols,'Dry50'),Prob=T)[,Period:=iPeriod])
    }
  }
  #=========================================================================================================================
  LineX()
  message("Saving the processed results")
  {
    if (is.null(TempFold)) Folder <- Dir$Analyses else Folder <- TempFold
    
    What <- c('Stations','AllData','AllDataState','Seasonal','Area','Freq','Moran','MorTrends','DistTestMor',
              'RPTrends','RPTrendsState','RPYTrends','ArTrends','DistTest')
    WhatInOneFile <- NULL
    
    SaveFiles <- MySave(FileFormat=OutFilePat,Folder=Folder,What=What,WhatInOneFile=WhatInOneFile)
    
    if (!is.null(TempFold)) {
      x <- file.copy(from=SaveFiles,to=Dir$Analyses,overwrite=T)
      x <- unlink(SaveFiles,recursive=T,force=T)
    }
  }
  LineX()
}

#=========================================================================================================================
# GRAPHS
#=========================================================================================================================

if (DoGraphs) {
  {
    AusMapXRange <- c(114.4,152.2)
    AusMapYRange <- c(-43,-10.4)
    
    BoxWidth <- BoxDodge <- 0.7
    BoxCoef <- c(0.1,0.9)
    
    LegKeyText <- 8
    PSize      <- if (Mesh==1) 1.77 else 2.7
    MinPSize   <- 0.55
    Stroke     <- if (Mesh==1) 0.25 else 0.2
    AxTxSize   <- 8
    LegLength  <- unit(0.7,'npc')
    VLineCol   <- 'grey20'
    
    TrCols <- c('dodgerblue4','gray50','darkred')
    YrCols <- c('orange','turquoise4','darkred')
    PrCols <- MyColors$ET4
    
    TmpCols <- MyColors$BuRd
    PcpCols <- MyColors$BrBG
    
    FigLabs <- LETTERS
    FigLabSize <- 4
    
    AusMap  <- Plot_Australia_Base_Map(Plot=F,Type=4,AsList=T,JustLoad=T,StatLwd=0.15,OzLwd=0.15,
                                       NArrowPos=NULL,StatFill='transparent',BackFill='black',MainDir=MainDir) 
    AusMapY <- Plot_Australia_Base_Map(Plot=F,Type=3,AsList=T,JustLoad=T,StatLwd=0.2,OzLwd=0.2,
                                       NArrowPos=NULL,BackFill='black',CroppingFill='grey80',MainDir=MainDir) 
  }
  #=========================================================================================================================
  message('Plot (Bar): Weather variables')
  #=========================================================================================================================
  {
    Temp <- copy(AllData[IsGrid==T])
    Temp[Year<last(DivYears), Period:='1889-1989']
    Temp[Year>=last(DivYears), Period:='1990-2019']
    Temp <- Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia')
    Temp <- Temp[, lapply(.SD,mean,na.rm=T), keyby=c('State2','Period','Site','Seas'), .SDcols=Vars]
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    
    p1 <- DoGGPlot(Data=Temp,Type='bar',xCol='Seas',yCol=Vars[1],fCol='Period',gCol=c('Period','Seas'),hFacet='State2',Lwd=0.2,
                   FColors=c('blue4','brown3'),CColors='black',LegPos='top',LegKeyText=LegKeyText,BarDodge=0.7,BarWidth=0.7,yRange=c(0,36),
                   yLab=expression('Maximum temperature ('^{o}*'C)'),RemStripBack=F,
                   AxTxSize=AxTxSize*c(0.8,0.9),xAngle=30,RemGrids=T,RemPanMarg=F,yAngle=0,ErrBarType='se')
    
    p2 <- DoGGPlot(Data=Temp,Type='bar',xCol='Seas',yCol=Vars[2],fCol='Period',gCol=c('Period','Seas'),hFacet='State2',Lwd=0.2,
                   FColors=c('blue4','brown3'),CColors='black',LegPos='top',LegKeyText=LegKeyText,BarDodge=0.7,BarWidth=0.7,yRange=c(0,1550),
                   yLab='Total precipitation (mm)',RemStripBack=F,
                   AxTxSize=AxTxSize*c(0.8,0.9),xAngle=30,RemGrids=T,RemPanMarg=F,yAngle=0,ErrBarType='se')
    
    ExpPath <- paste0(Dir$Analyses,"/0 - Bar - Weather Variables - Monthly.pdf")
    p <- Plot_Grid(list(p1,p2),CommLegend=T,LegPos=T,RemTitles=F,RemLabs=F,RemAxText=F,Labels=FigLabs,LabelXY=c(0.01,1),
                   ExpPath=ExpPath,Size=PPSize(H=0.45,W=1.1),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Line): State and national average MMT and TPR')
  #=========================================================================================================================
  {
    for (V in Vars) {
      Plots <- list()
      
      if (V==Vars[1]) yLab <- 'Average daily maximum temperature (\u00B0C)'
      if (V==Vars[2]) yLab <- 'Average total precipitation (mm)'
      
      for (S in 0:5) {
        Temp <- AllData[IsGrid==T & Seas==S]
        Temp <- Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia')
        Temp <- Temp[, lapply(.SD,mean,na.rm=T), keyby=c('State2','Year'), .SDcols=V]
        Temp$Period <- '1889-1989'
        Temp[between(Year,1990,2019,incbounds=T), Period:='1990-2019']
        # Temp <- Add_Aggregate_Unit(Temp,Col='Period',AggUnit='1889-2019')
        
        p <- DoGGPlot(Data=Temp,Type='linep',xCol='Year',yCol=V,cCol='Period',rgcCol='Period',CColors=c('navy','darkred'),
                      hFacet='State2',FacetScale='free_y',NRow=4,RemGrids=T,PSize=1.0,PShape=18,xLab='Year',yLab=yLab,
                      LegPos='top',LegKeyText=LegKeyText,AxTxSize=AxTxSize,Alpha=1,LegScale=5,yAngle=0,xBreaks=seq(2019,1889,-15),
                      ShowReg=T,RegDegree=1,RegType=3,AddRegEq='Eq-R2-Ast-Mean',RegEqPos='topleft',
                      RegEqSize=2,RegEqMarg=20,RegEqHVadj=c(0,6.5+14*(V==Vars[2])),RegXfrom1=T,RegEqBold=T,RegEqAddNames=F,RegEqDigs=c(3,2),
                      HLine=mean,HLineCol='red',HLineType=2,HLineW=0.4,VLine=1990,VLineCol='navy',VLineType=2,VLineW=0.4)
        Plots[[length(Plots)+1]] <- p
      }
      
      if (V==Vars[1]) ExpPath <- paste0(Dir$Analyses,"/0 - Line - Weather Variables - Temp.pdf")
      if (V==Vars[2]) ExpPath <- paste0(Dir$Analyses,"/0 - Line - Weather Variables - Rain.pdf")
      
      x <- Print_Plots_to_File(Plots,File=ExpPath,Size=PPSize(H=0.6,W=1))
    }
  }
  #=========================================================================================================================
  message('Plot (Bar): Correlation of temperature and rainfall')
  #=========================================================================================================================
  {
    Temp <- list()
    for (Pr in Periods[-4]) {
      Years <- as.numeric(strsplit(Pr,split='-')[[1]])
      
      for (S in 0:5) {
        Temp1 <- AllData[IsGrid==T & Seas==S & Year>=Years[1] & Year<=Years[2], .(Cor=round(cor(.SD)[2],2)), keyby=c('Site','Long','Seas','Mon','Lat','State2'), .SDcols=c(Vars)]
        Temp2 <- AllData[IsGrid==T & Seas==S & Year>=Years[1] & Year<=Years[2], .(P=round(cor.test(.SD[[1]],.SD[[2]])$p.value,4)), keyby=c('Site','Long','Seas','Mon','Lat','State2'), .SDcols=c(Vars)]
        Temp[[length(Temp)+1]] <- Temp1[, Period:=Pr]
      }
    }
    Temp <- rbindlist(Temp)
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    
    ExpPath <- paste0(Dir$Analyses,'/0 - Bar - Corr of Temp and Rain.pdf')
    
    p1 <- DoGGPlot(Data=Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia'),
                   Type='bar',xCol='Period',yCol='Cor',fCol='Period',hFacet='State2',vFacet='Seas',
                   FColors=PrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',BarLabSize=1.9,BarLabDigs=2,
                   yLab='Correlation coefficient',AxTxSize=AxTxSize,xAngle=45,yAngle=0,Lwd=0.2,BarLabGap=0.03,xyOffset=c(0.05,0.05,0.07,0.03),
                   RemGrids=T,RemPanMarg=F,ExpPath=ExpPath,Size=PPSize(H=0.65,W=1),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Bar): Correlation of return periods')
  #=========================================================================================================================
  {
    Temp <- list()
    
    FUNC <- function(data) {
      Cor <- round(cor(data)[1,2:3],2)
      Cor <- data.table(Var=c('H.Ext','D.Ext'),Cor=Cor)
      return(Cor)
    }
    
    for (Pr in Periods[-4]) {
      Years <- as.numeric(strsplit(Pr,split='-')[[1]])
      
      for (S in 0:5) {
        Temp1 <- copy(AllData[IsGrid==T & Seas==S & Year>=Years[1] & Year<=Years[2]])
        Temp1 <- Temp1[, FUNC(.SD), keyby=c('Site','Long','Seas','Mon','Lat','State2'), .SDcols=c('RP','RPt','RPr')]
        Temp[[length(Temp)+1]] <- Temp1[, Period:=Pr]
      }
    }
    Temp <- rbindlist(Temp)
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    
    ExpPath <- paste0(Dir$Analyses,'/1 - Bar - Corr of Return Periods.pdf')
    
    p1 <- DoGGPlot(Data=Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia'),
                   Type='bar',xCol='Var',yCol='Cor',fCol='Period',gCol='Period',hFacet='Seas',vFacet='State2',yBreaks=seq(0,1,0.5),
                   FColors=PrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',BarLabSize=1.8,BarLabDigs=2,
                   yLab='Linear correlation with compound hot-dry (CHD) extreems',AxTxSize=AxTxSize,xAngle=0,yAngle=0,Lwd=0.2,BarLabGap=0.03,xyOffset=c(0.05,0.05,0.05,0.07),
                   RemGrids=T,RemPanMarg=F,ExpPath=ExpPath,Size=PPSize(H=0.7,W=1.2),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Point): Correlations')
  #=========================================================================================================================
  {
    Temp <- Add_Aggregate_Unit(Seasonal,Col='Area',AggUnit='Australia')
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    
    ExpPath <- paste0(Dir$Analyses,"/2 - Point - Yield and Var PR Corr.pdf")
    
    p1 <- DoGGPlot(Data=Temp,Type='cor',corCols=c('RP','RPt','RPr','RPY',Vars,'yield','yield2'),hFacet='Area',vFacet='Seas',
                   PSize=4.2,PShape=21,Stroke=0.3,BarLabSize=1.23,LegLength=unit(16,'cm'),LegTitleF='Correlation',
                   FColors=c('gray30','darkred'),RemGrids=T,xAngle=30,yAngle=0,FColorsCont=MyColors$RdBu,
                   LegPos='top',LegKeyText=LegKeyText,AxTxSize=AxTxSize,Add45L=T,LegNumBreaks=20,LegLabsGap=1,
                   ExpPath=ExpPath,Size=PPSize(H=0.9,W=1),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Bar): Area of sig trends')
  #=========================================================================================================================
  {
    for (i in 1:2) {
      if (i==1) Cols <- c('RP','RPt','RPr')
      if (i==2) Cols <- Vars
      
      Temp <- copy(RPTrends[IsGrid==T & TrendVar%in%Cols & Period%in%last(Periods,2)])
      Temp <- merge(Temp,Stations[,-c('IsGrid','State2')],by=c('Name','ID'),sort=F)
      Temp[, IsTrend:=Trends[IsTrend+2]]
      Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
      Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
      
      if (i==1) {
        Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('RPt','RPr','RP'),NewLevs=c('Hot Years','Dry Years','Hot and Dry Years'),AsFactor=T)]
      }
      if (i==2) {
        Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=Vars,NewLevs=c('Temperature','Precipitation'),AsFactor=T)]
      }
      Temp2 <- FreqTableXD(Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia'),Cols=c('State2','TrendVar','Period','Seas','IsTrend'))
      Temp2[, IsTrend:=factor(IsTrend,levels=Trends)]
      
      if (i==1) ExpPath <- paste0(Dir$Analyses,"/1 - Bar - Area of Sig Trend of RP.pdf")
      if (i==2) ExpPath <- paste0(Dir$Analyses,"/1 - Bar - Area of Sig Trend of Vars.pdf")
      if (i==1) LegTitleF <- 'Trend analysis of return periods'
      if (i==2) LegTitleF <- 'Trend analysis of weather variables'
      
      p <- DoGGPlot(Data=Temp2,Type='bar',xCol='TrendVar',yCol='Prob',fCol='IsTrend',
                    hFacet=c('Seas','Period'),vFacet='State2',FacetScale='fixed',
                    FColors=TrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,BarLabSize=1.6,BarLabDigs=0,BarLabGap=0.03,
                    yLab='Share of area (%)',AxTxSize=AxTxSize,xAngle=30,yAngle=0,xyOffset=c(0,0,0.03,0.15),
                    RemGrids=T,RemPanMarg=F,BarDodge=0.8,BarWidth=0.8,Lwd=0.2,yRange=c(0,100),yBreaks=seq(0,100,25),
                    LegTitleF=LegTitleF,ExpPath=ExpPath,Size=PPSize(H=0.75,W=1.5),Device='pdf')
    }
  }
  #=========================================================================================================================
  message('Plot (Line): State area affected')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      Temp1 <- copy(Area$DryHot25[IsGrid==T & Seas==S & Period%in%Periods[3:4] & DryHot25==T])[, Type:='Hot and Dry Years']
      Temp2 <- copy(Area$Hot25[IsGrid==T & Seas==S & Period%in%Periods[3:4] & Hot25==T])[, Type:='Hot Years']
      Temp3 <- copy(Area$Dry25[IsGrid==T & Seas==S & Period%in%Periods[3:4] & Dry25==T])[, Type:='Dry Years']
      Temp  <- rbind(Temp1[,-'DryHot25'],Temp2[,-'Hot25'],Temp3[,-'Dry25'])
      Temp[, Type:=Change_Levels(Type,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
      N <- c(table(Stations$State2),Australia=nrow(Stations))
      Temp[, Freq:=Freq/N[State2]*100]
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Line - State Area Affected - %s.pdf",Seasons[S+1]))
      
      p1 <- DoGGPlot(Data=Temp,Type='linep',xCol='Year',yCol='Freq',cCol='Type',gCol='Type',rgcCol='Type',
                     hFacet='State2',NRow=4,CColors=YrCols,RemGrids=T,PSize=1.0,PShape=18,
                     xLab='Year',yLab='Share of area affected by climate extremes (%)',LegPos='top',
                     LegKeyText=LegKeyText,AxTxSize=AxTxSize,Alpha=1,LegScale=5,yAngle=0,
                     xyOffset=c(0.05,0.05,0.05,0.25),xBreaks=seq(2019,1889,-20),yBreaks=seq(0,100,20),yRange=c(0,100),
                     ShowReg=F,RegDegree=1,RegType=3,AddRegEq='Eq-R2-Ast-Mean',RegEqPos='topleft',
                     RegEqSize=2,RegEqMarg=10,RegEqHVadj=c(0,10),RegXfrom1=T,RegEqBold=T,RegEqAddNames=F,HLine=mean,HLineCol='red',HLineType=3,
                     Labels=FigLabs,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.04),LabelPos='topright',
                     ExpPath=ExpPath,Size=PPSize(H=0.6,W=1),Device='pdf')
    }
  }
  #=========================================================================================================================
  message('Plot (Bar): Trend of state area affected')
  #=========================================================================================================================
  {
    Temp1 <- copy(ArTrends$DryHot25[IsGrid==T & Period%in%last(Periods,2)])[, Type:='Hot and Dry Years']
    Temp2 <- copy(ArTrends$Hot25[IsGrid==T & Period%in%last(Periods,2)])[, Type:='Hot Years']
    Temp3 <- copy(ArTrends$Dry25[IsGrid==T & Period%in%last(Periods,2)])[, Type:='Dry Years']
    Temp  <- rbind(Temp1,Temp2,Temp3)
    Temp[, Type:=Change_Levels(Type,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
    Temp[, IsTrend:=c('Insignificant','Significant')[abs(IsTrend)+1]]
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    
    N <- c(table(Stations$State2),Australia=nrow(Stations))
    Temp[, LN_SL:=LN_SL/N[State2]*100]
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Bar - Trend of Area Affected.pdf")
    
    p <- DoGGPlot(Data=Temp[!is.na(IsTrend)],Type='bar',xCol='State2',yCol='LN_SL',fCol='IsTrend',hFacet=c('Type','Period'),vFacet=c('Seas'),
                  CColors='black',FColors=c('gray30','darkred'),RemGrids=T,yLab='Slope (% area / decade)',FacetScale='fixed',yConv=0.1,
                  xAngle=30,yAngle=0,BarLabSize=1.4,BarLabGap=0.05,Alpha=1,
                  LegPos='top',LegKeyText=LegKeyText,AxTxSize=AxTxSize*0.9,LegScale=4,
                  HLine=0,HLineCol='black',HLineW=0.1,Lwd=0.2,BarDodge=0.8,BarWidth=0.8,xyOffset=0.07,RemPanMarg=F,
                  ExpPath=ExpPath,Size=PPSize(H=0.6,W=1.1),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Bar): Values and trends of Moran-I')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      Temp <- copy(MorTrends[IsGrid==T & Period%in%last(Periods,2)])
      Temp[, IsTrend:=Trends[IsTrend+2]]
      Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
      Temp[, Type:=Change_Levels(Type,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Bar - Moran Trend - %s.pdf",Seasons[S+1]))
      
      p1 <- DoGGPlot(Data=Temp[Seas==S],Type='bar',xCol='State2',yCol='LN_SL',fCol='IsTrend',hFacet='Period',vFacet='Type',FacetScale='fixed',
                     FColors=TrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,yConv=0.1,
                     yLab='Slope (unit/decade)',AxTxSize=AxTxSize,xAngle=30,yAngle=0,BarLabSize=1.7,BarLabDigs=3,BarLabGap=0.05,
                     RemGrids=T,RemPanMarg=F,BarDodge=0.8,BarWidth=0.8,Lwd=0.2,xyOffset=c(0.05,0.05,0.05,0.1),
                     LegTitleF='Trend analysis of Moran-I',HLine=0,HLineCol='black',HLineW=0.1,LegDrop=F,
                     Labels=FigLabs,LabelsSize=FigLabSize*0.9,LabelsOffs=c(0.02,0.02),
                     ExpPath=ExpPath,Size=PPSize(H=0.55,W=0.8),Device='pdf')
    }
    
    Temp <- copy(Moran[IsGrid==T & Period%in%Periods[-4]])
    Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
    Temp[, IsSig:=(MoranP<=Alpha)*sign(Moran)]
    Temp[, IsSig:=Trends[IsSig+2]]
    Temp[, IsSig:=factor(IsSig,levels=Trends)]
    Temp[, Type:=Change_Levels(Type,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Bar - Moran Value.pdf")
    
    p1 <- DoGGPlot(Data=Temp,Type='bar',xCol='State2',yCol='Moran',fCol='Period',gCol='Period',hFacet='Type',vFacet='Seas',FacetScale='fixed',
                   FColors=PrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',
                   yLab='Moran-I',AxTxSize=AxTxSize,xAngle=30,yAngle=0,BarLabSize=1.5,BarLabDigs=1,BarLabGap=0.02,
                   RemGrids=T,RemPanMarg=F,BarDodge=0.8,BarWidth=0.8,Lwd=0.2,xyOffset=c(0,0,0.03,0.14),yRange=c(0,1),yBreaks=seq(0,1,0.5),
                   ExpPath=ExpPath,Size=PPSize(H=0.6,W=1.2),Device='pdf')
  }
  #=========================================================================================================================
  message('Plot (Bar): Regional trends in return periods')
  #=========================================================================================================================
  {
    for (i in 1:2) {
      if (i==1) Cols <- c('RP','RPt','RPr')
      if (i==2) Cols <- Vars
      
      Temp <- copy(RPTrendsState[TrendVar%in%Cols & Period%in%data.table::last(Periods,n=2)])
      Temp[, IsTrend:=Trends[IsTrend+2]]
      Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
      Temp[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
      
      if (i==1) {
        Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('RPt','RPr','RP'),NewLevs=c('Hot Years','Dry Years','Hot and Dry Years'),AsFactor=T)]
      }
      if (i==2) {
        Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=Vars,NewLevs=c('Temperature','Precipitation'),AsFactor=T)]
      }
      Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
      
      if (i==1) ExpPath <- paste0(Dir$Analyses,"/1 - Bar - State Trend of RP.pdf")
      if (i==2) ExpPath <- paste0(Dir$Analyses,"/1 - Bar - State Trend of Vars.pdf")
      if (i==1) yLab <- 'Trend of return periods (yr/decade)'
      if (i==2) yLab <- 'Trend of weather variables (/decade)'
      
      p1 <- DoGGPlot(Data=copy(Temp),Type='bar',xCol='TrendVar',yCol='LN_SL',fCol='IsTrend',
                     hFacet=c('Seas','Period'),vFacet='State2',FColors=TrCols,CColors='black',
                     LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',BarLabSize=1.6,BarLabDigs=2,BarLabGap=0.05,
                     yLab=yLab,AxTxSize=AxTxSize,xAngle=45,yAngle=0,Lwd=0.2,yConv=0.1,LegDrop=F,
                     RemGrids=T,RemPanMarg=F,HLine=0,HLineCol='black',HLineW=0.1,xyOffset=c(0,0,0.09,0.09),
                     ExpPath=ExpPath,Size=PPSize(H=0.85,W=1.5),Device='pdf')
    }
  }
  #=========================================================================================================================
  message('Plot (Bar): Return periods of CHD extremes vs univariate extremes')
  #=========================================================================================================================
  {
    Temp <- copy(AllDataState)
    for (i in 1:length(Periods)) {
      Years <- as.numeric(strsplit(Periods[i],'-')[[1]])
      Rows  <- which(between(Temp$Year,Years[1],Years[2],incbounds=T))
      iPeriod <- Periods[i]
      Temp[Rows,Period:=iPeriod]
    }
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Bar - xxxxxxxx.pdf")
    
    p1 <- DoGGPlot(Data=Temp,Type='cor',corCols=c('RP','RPt','RPr'),hFacet=c('State2','Period'),vFacet='Seas',
                   PSize=4.2,PShape=21,Stroke=0.3,BarLabSize=1.23,LegLength=unit(16,'cm'),LegTitleF='Correlation coefficient',
                   FColors=c('gray30','darkred'),RemGrids=T,xAngle=30,yAngle=0,FColorsCont=MyColors$RdBu,
                   LegPos='top',LegKeyText=LegKeyText,AxTxSize=AxTxSize,Add45L=T,LegNumBreaks=20,LegLabsGap=1,
                   ExpPath=ExpPath,Size=PPSize(H=0.9,W=1),Device='pdf')
  }
  #=========================================================================================================================
  
  
  #=========================================================================================================================
  # MAPS
  #=========================================================================================================================
  
  
  #=========================================================================================================================
  message('Plot (Map): Selected stations')
  #=========================================================================================================================
  {
    StNamesPos <- data.frame(x=c(143, 143.8, 145, 134, 123, 133, 146.5),
                             y=c(-22, -31.2, -37, -28, -24.1, -22.1, -42),
                             label=c('QLD', 'NSW', 'VIC', 'SA', 'WA', 'NT', 'TAS'))
    
    ArNamesPos <- data.frame(x=c(144.6, 139, 136.5, 120.8),
                             y=c(-28.1, -39.1, -31.8, -30),
                             label=c('North-East', 'South-East', 'South', 'West'))
    
    AusMapCol <- Plot_Australia_Base_Map(Plot=F,Type=1,AsList=T,JustLoad=T,BackFill='black',
                                         ArNamesPos=ArNamesPos,ArNamesCol='navy',AreaTx=3,
                                         StNamesPos=StNamesPos,StNamesCol='gray50',StatTx=5,
                                         StatLwd=0.2,Alpha=0.7,UseNorthEast=T,MainDir=MainDir) 
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Map - Selected Sites with States.pdf")
    
    p1 <- DoGGPlot(Data=Stations[IsGrid==T],Map=AusMap,Type='points',xCol='Long',yCol='Lat',fCol='State',
                   PSize=2,PShape=21,Stroke=0.1,RemPanMarg=T,FColors=MyColors$Set1,Alpha=0.8,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                   LegPos='top',LegNRow=2,LegKeyText=LegKeyText*0.8,yAngle=0,LegTitRot=0,AxTxSize=AxTxSize,LegScale=2,ForceLegScale=T,
                   xRange=AusMapXRange,yRange=AusMapYRange,Device='pdf',ExpPath=ExpPath,Size=PPSize(H=0.43,W=0.6))
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Map - Selected Sites.pdf")
    
    p2 <- DoGGPlot(Data=Stations[IsGrid==T],Map=AusMapCol,Type='points',xCol='Long',yCol='Lat',
                   PSize=0.8,PShape=21,Stroke=0.1,RemPanMarg=T,FColors='darkred',Alpha=0.7,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                   LegKeyText=LegKeyText,LegTitRot=0,yAngle=0,LegTitleF='States',AxTxSize=AxTxSize,
                   xRange=AusMapXRange,yRange=AusMapYRange,Device='pdf',ExpPath=ExpPath,Size=PPSize(H=0.377,W=0.6))
  }
  #=========================================================================================================================
  message('Plot (Map): Frequency of Hot and dry years')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      Temp1 <- copy(Freq$DryHot25[IsGrid==T & Seas==S & DryHot25==T])[, Type:='Hot and Dry Years']
      Temp2 <- copy(Freq$Hot25[IsGrid==T & Seas==S & Hot25==T])[, Type:='Hot Years']
      Temp3 <- copy(Freq$Dry25[IsGrid==T & Seas==S & Dry25==T])[, Type:='Dry Years']
      Temp  <- rbind(Temp1[,-'DryHot25'],Temp2[,-'Hot25'],Temp3[,-'Dry25'])
      Temp  <- merge(Temp,Stations[,-c('State2','IsGrid')],by=c('Name','ID'),sort=F)
      Temp[, Period:=factor(Period,levels=Periods)]
      Temp[, Type:=Change_Levels(Type,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
      Temp <- Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia')
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Bar - Freq of Hot and Dry Year - %s.pdf",Seasons[S+1]))
      
      p1 <- DoGGPlot(Data=Temp[Period!=last(Periods)],Type='bar',xCol='Period',yCol='Freq',fCol='Type',hFacet='State2',vFacet='Type',
                     FColors=YrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',BarLabSize=2,BarLabDigs=1,
                     yLab='Frequency of climate extremes (years)',AxTxSize=AxTxSize,xAngle=45,
                     RemGrids=T,RemPanMarg=F,yAngle=0,Lwd=0.2,xyOffset=c(0,0,0.04,0.08),
                     ExpPath=ExpPath,Size=PPSize(H=0.55,W=1),Device='pdf')
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Freq of Hot and Dry Year - %s.pdf",Seasons[S+1]))
      
      p1 <- DoGGPlot(Data=Temp[Period!=last(Periods)],Map=AusMap,Type='points',xCol='Long',yCol='Lat',cCol='Freq',vFacet='Period',hFacet='Type',
                     PSize=PSize*0.92,PShape=15,Stroke=0,RemPanMarg=T,ContinLeg=T,xyOffset=0.07,CColorsCont=MyColors$Reds,
                     yAngle=0,RemStripBack=T,xLab='Longitude',yLab='Latitude',LegTitleC='Frequency of climate extremes (years)',
                     LegPos='top',LegKeyText=LegKeyText,LegLength=unit(13,'cm'),LegNumBreaks=14,LegLabsGap=1,LegLabsStart=1,
                     xRange=AusMapXRange,yRange=AusMapYRange,AxisLabCoords=T,AxTxSize=AxTxSize,
                     Labels=FigLabs,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.02),
                     ExpPath=ExpPath,Size=PPSize(H=0.69,W=1),Device='pdf',AddLayersFront=list(AusMap$australia,AusMap$states))
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Average temperature and rainfall')
  #=========================================================================================================================
  {
    for (V in Vars) {
      Plots <- list()
      
      for (S in 0:5) {
        Temp1 <- AllData[IsGrid==T & Year>=1990, lapply(.SD,mean,na.rm=T), keyby=c('Long','Seas','Mon','Lat'), .SDcols=c(V)]
        LegLimits <- Temp1[, range(get(V),na.rm=T)]
        if (V==Vars[2]) LegLimits[1] <- 0
        if (V==Vars[1]) LegTitleC <- 'Average daily maximum temperature (\u00B0C)'
        if (V==Vars[2]) LegTitleC <- 'Average total precipitation (mm)'
        if (V==Vars[1]) CColorsCont <- MyColors$BuRd
        if (V==Vars[2]) CColorsCont <- MyColors$RdBu
        if (V==Vars[1]) ContinLegTrans <- MyTrans$SymThird
        if (V==Vars[2]) ContinLegTrans <- MyTrans$SymThird
        
        p <- DoGGPlot(Data=Temp1[Seas==S],Map=AusMap,Type='points',xCol='Long',yCol='Lat',cCol=V,
                      PSize=PSize,PShape=15,Stroke=0,RemPanMarg=T,ContinLeg=T,xyOffset=0.07,CColorsCont=CColorsCont,
                      LegPos='top',LegKeyText=LegKeyText*0.9,LegLength=unit(10,'cm'),LegWidth=0.4,yAngle=0,
                      LegTitleC=LegTitleC,LegNumBreaks=10,RemStripBack=T,ContinLegTrans=ContinLegTrans,LegLimits=LegLimits,
                      xRange=AusMapXRange,yRange=AusMapYRange,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                      AxTxSize=AxTxSize,AddLayersFront=list(AusMap$australia,AusMap$states))
        Plots[[S+1]] <- p
      }
      
      Ord <- c(0,5,1:4) + 1
      Labels <- paste0('(',FigLabs,') ',Seasons[Ord])
      
      if (V==Vars[1]) ExpPath <- paste0(Dir$Analyses,"/1 - Map - Average Temp (1990-2019).pdf")
      if (V==Vars[2]) ExpPath <- paste0(Dir$Analyses,"/1 - Map - Average Rain (1990-2019).pdf")
      
      p <- Plot_Grid(Plots[Ord],CommLegend=T,LegPos='top',RemTitles=F,RemLabs=F,RemAxText=F,RemMargs=F,NRow=3,NCol=2,
                     ExpPath=ExpPath,Size=PPSize(H=0.8,W=0.8),Device='pdf',
                     Labels=Labels,LabelSize=10,LabelXY=c(0.2,0.96))
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Correlation of temperature and rainfall')
  #=========================================================================================================================
  {
    Plots <- list()
    for (S in 0:5) {
      Temp1 <- AllData[IsGrid==T & Year>=1960, .(Cor=round(cor(.SD)[2],2)), keyby=c('Site','Long','Seas','Mon','Lat','State2'), .SDcols=c(Vars)]
      Temp2 <- AllData[IsGrid==T & Year>=1960, .(P=round(cor.test(.SD[[1]],.SD[[2]])$p.value,4)), keyby=c('Site','Long','Seas','Mon','Lat','State2'), .SDcols=c(Vars)]
      Temp1$P <- Temp2$P
      
      p <- DoGGPlot(Data=Temp1[Seas==S],Map=AusMap,Type='point-pval',xCol='Long',yCol='Lat',cCol=c('Cor','P'),
                    PSize=PSize,MinPSize=MinPSize,PShape=15,Stroke=0,RemPanMarg=T,ContinLeg=T,
                    xyOffset=0.07,CColorsCont=MyColors$RdBu,CColors='black',P2Type='marked',
                    LegPos='top',LegNRow=1,LegKeyText=LegKeyText*0.9,LegLength=unit(10,'cm'),LegWidth=0.4,yAngle=0,
                    LegTitleC='Correlation coefficient',LegNumBreaks=10,FMidPoint=0,LegLimits=c(-1,1),RemStripBack=T,PThr=0.05,
                    xRange=AusMapXRange,yRange=AusMapYRange,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                    AxTxSize=AxTxSize,AddLayersFront=list(AusMap$australia,AusMap$states))
      Plots[[S+1]] <- p
    }
    
    Ord <- c(0,5,1:4) + 1
    Labels <- paste0('(',FigLabs,') ',Seasons[Ord])
    
    ExpPath <- paste0(Dir$Analyses,"/1 - Map - Corr of Temp and Rain (1960-2019).pdf")
    p <- Plot_Grid(Plots[Ord],CommLegend=T,LegPos='top',RemTitles=F,RemLabs=F,RemAxText=F,RemMargs=F,NRow=3,NCol=2,
                   ExpPath=ExpPath,Size=PPSize(H=0.8,W=0.8),Device='pdf',
                   Labels=Labels,LabelSize=10,LabelXY=c(0.205,0.955))
  }
  #=========================================================================================================================
  message('Plot (Map): Trends in yield')
  #=========================================================================================================================
  {
    for (S in c(0,5)) {
      Temp <- copy(RPYTrends[Seas==S & TrendVar %in% c('RP','yield','yield2')])
      Temp[, IsTrend:=Trends[IsTrend+2]]
      Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
      Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('yield','yield2','RP'),NewLevs=c('Yield (Mean Top 50%)','Yield (Mean Mid-May)','Hot and Dry Years'))]
      Temp[,PValue:='P<0.05']
      
      Temp2 <- copy(RPYTrends[Seas==S & TrendVar %in% c('RP','yield','yield2')])
      Temp2[YP_P<=0.1, IsTrend:=sign(LN_SL)]
      Temp2[, IsTrend:=Trends[IsTrend+2]]
      Temp2[, IsTrend:=factor(IsTrend,levels=Trends)]
      Temp2[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('yield','yield2','RP'),NewLevs=c('Yield (Mean Top 50%)','Yield (Mean Mid-May)','Hot and Dry Years'))]
      Temp2[,PValue:='P<0.10']
      Temp <- rbind(Temp,Temp2)
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/2 - Map - Trend Sig of Yield RP - %s.pdf",Seasons[S+1]))
      
      p1 <- DoGGPlot(Data=Temp,Map=AusMapY,Type='points',xCol='Long',yCol='Lat',fCol='IsTrend',hFacet='TrendVar',vFacet='PValue',
                     PSize=PSize*0.7,PShape=21,Stroke=Stroke,RemPanMarg=T,FColors=TrCols,CColors='black',xyOffset=0.07,
                     LegPos='top',LegNRow=1,LegKeyText=LegKeyText,LegKeySize=0,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                     LegTitleF='Trend analysis of wheat yield',LegScale=1.3,ForceLegScale=T,yAngle=0,
                     xRange=AusMapXRange,yRange=c(-38.5,-22.2),AxTxSize=AxTxSize*0.85,
                     ExpPath=ExpPath,Size=PPSize(H=0.29,W=1),Device='pdf',RemStripBack=T,
                     AddLayersFront=list(AusMap$australia,AusMap$states))
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Change in distributions')
  #=========================================================================================================================
  {
    for (i in 1:2) {
      if (i==1) Cols <- c('RP','RPt','RPr')
      if (i==2) Cols <- Vars
      
      Plots1 <- Plots2 <- list()
      for (S in 0:5) {
        Temp <- copy(DistTest[IsGrid==T & Var %in% Cols])
        Temp <- merge(Temp,Stations[,-c('State2','IsGrid')],by=c('Name','ID'),sort=F)
        
        if (i==1) {
          Temp[, Var:=Change_Levels(Var,OldLevs=Cols,NewLevs=c('Hot and Dry Years','Hot Years','Dry Years'))]
          Temp[, Var:=Change_Levels(Var,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
          ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Change in Dist of RP - %s.pdf",Seasons[S+1]))
          if (S!=0) ExpPath <- NULL
          
        } else if (i==2) {
          Temp[, Var:=Change_Levels(Var,OldLevs=Cols,NewLevs=c('Temperature','Precipitation'))]
          Temp[, Var:=factor(Var,levels=c('Temperature','Precipitation'))]
          ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Change in Dist of Vars - %s.pdf",Seasons[S+1]))
          if (S!=0) ExpPath <- NULL
        }
        
        FUNC <- function(x) sum(x$`P-Value`<=Alpha)
        Temp <- Temp[, FUNC(.SD), keyby=c('Var','Name','Seas','Mon','Lat','Long','State2')]
        Temp[, V1:=c('Insignificant change','Significant change')[V1+1]]
        
        if (S==0) {
          p <- DoGGPlot(Data=Temp[Seas==S],Map=AusMap,Type='points',xCol='Long',yCol='Lat',cCol='V1',hFacet='Var',
                        PSize=PSize*0.974,PShape=15,Stroke=0,RemPanMarg=T,LegScale=3-(S==0),ForceLegScale=T,
                        CColors=TrCols[-1],xyOffset=0.07,LegTitleC='Change in distribution',
                        xLab='Longitude',yLab='Latitude',LegPos='top',LegKeyText=LegKeyText,LegKeySize=0,LegTitRot=0,AxisLabCoords=T,
                        xRange=AusMapXRange,yRange=AusMapYRange,AxTxSize=AxTxSize*0.8,yAngle=0,RemStripBack=T,
                        ExpPath=ExpPath,Size=PPSize(H=0.30,W=1-ifelse(i==2,0.31,0)),Device='pdf',
                        Labels=FigLabs,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.02),
                        AddLayersFront=list(AusMap$australia,AusMap$states))
        }
        
        PSize2 <- PSize-ifelse(i==1,0.77,0.535)
        
        Plots2[[S+1]] <- DoGGPlot(Data=Temp[Seas==S],Map=AusMap,Type='points',xCol='Long',yCol='Lat',cCol='V1',hFacet='Var',
                                  PSize=PSize2,PShape=15,Stroke=Stroke,RemPanMarg=T,LegScale=3-(S==0),ForceLegScale=T,
                                  CColors=TrCols[-1],xyOffset=0.07,LegTitleC='Change in distribution',
                                  xLab='Longitude',yLab='Latitude',LegPos='top',LegKeyText=LegKeyText,LegKeySize=0,LegTitRot=0,AxisLabCoords=T,
                                  xRange=AusMapXRange,yRange=AusMapYRange,AxTxSize=AxTxSize*0.8,yAngle=0,RemStripBack=T,
                                  Size=PPSize(H=0.30,W=1-ifelse(i==2,0.31,0)),Device='pdf',
                                  AddLayersFront=list(AusMap$australia,AusMap$states))
      }
      
      Ord <- c(0,5,1:4) + 1
      Labels <- paste0('(',FigLabs,') ',Seasons[Ord])
      
      if (i==1) ExpPath <- paste0(Dir$Analyses,"/1 - Map - Change in Dist of RP - Seasonal.pdf")
      if (i==2) ExpPath <- paste0(Dir$Analyses,"/1 - Map - Change in Dist of Vars - Seasonal.pdf")
      
      p <- Plot_Grid(Plots2[Ord],CommLegend=T,LegPos=T,NCol=2,RemTitles=F,RemLabs=F,RemAxText=F,RemMargs=F,
                     Labels=Labels,LabelSize=8,LabelXY=if (i==1) c(0.0941,0.835) else c(0.117,0.855),
                     Size=PPSize(H=0.7-ifelse(i==1,0.07,0),W=1.4-ifelse(i==1,0,0.26)),ExpPath=ExpPath,Device='pdf')
      
      Temp2 <- Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia')
      Temp2[, V1:=(V1=='Significant change')]
      Temp2 <- FreqTableXD(Temp2,Cols=c('State2','Seas','Var','V1'))
      
      if (i==1) Temp[, Var:=Change_Levels(Var,OldLevs=c('Hot25','Dry25','DryHot25'),NewLevs=YTypes,AsFactor=T)]
      if (i==2) Temp[, Var:=Change_Levels(Var,OldLevs=Vars,NewLevs=c('Temperature','Precipitation'),AsFactor=T)]
      Temp2[, Seas:=factor(Seasons[Seas+1],levels=Seasons)]
      
      if (i==1) ExpPath2 <- paste0(Dir$Analyses,"/1 - Bar - Change in Dist of RP Area.pdf")
      if (i==2) ExpPath2 <- paste0(Dir$Analyses,"/1 - Bar - Change in Dist of Vars Area.pdf")
      
      p <- DoGGPlot(Data=Temp2[V1==T],Type='bar',xCol='Var',yCol='Prob',fCol='Var',hFacet='State2',vFacet='Seas',Lwd=0.2,
                    CColors='black',FColors=YrCols,LegPos='top',LegKeyText=LegKeyText,UseMean=T,BarLabSize=1.8,BarLabDigs=0,
                    yLab='Share of area (%)',AxTxSize=AxTxSize,xAngle=30,yAngle=0,xyOffset=c(0,0,0.03,0.15),
                    RemGrids=T,RemPanMarg=F,BarDodge=0.8,BarWidth=0.8,yRange=c(0,100), yBreaks=seq(0,100,25),
                    ExpPath=ExpPath2,Size=PPSize(H=0.6,W=ifelse(i==1,1,0.8)),Device='pdf')
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Trends in return periods')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      Cols <- c('RP','RPt','RPr')      
      
      Temp <- copy(RPTrends[IsGrid==T & Seas==S & Period%in%last(Periods,n=2) & TrendVar%in%Cols])
      Temp <- merge(Temp,Stations[,-c('IsGrid','State2')],by=c('Name','ID'),sort=F)
      Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('RPt','RPr','RP'),NewLevs=c('Hot Years','Dry Years','Hot and Dry Years'),AsFactor=T)]
      
      LegTitleC      <- 'Linear trend in return periods (yr/decade)'
      CColorsCont    <- MyColors$BuRd
      ContinLegTrans <- MyTrans$SymSqrt
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Trend of RPs - %s.pdf",Seasons[S+1]))
      
      p <- DoGGPlot(Data=Temp[Period%in%last(Periods,n=2)],Map=AusMap,Type='point-pval',xCol='Long',yCol='Lat',cCol=c('LN_SL','YP_P'),
                    hFacet='TrendVar',vFacet='Period',CheckFacets=F,xyOffset=0.07,ContinLeg=T,fConv=0.1,P2Type='marked',
                    PSize=PSize*0.83,MinPSize=MinPSize*0.65,PShape=15,Stroke=Stroke,RemPanMarg=T,CColorsCont=CColorsCont,CColors='black',
                    LegPos='top',LegNRow=1,LegKeyText=LegKeyText*0.8,LegKeySize=0,LegTitRot=0,AxisLabCoords=T,yAngle=0,
                    xLab='Longitude',yLab='Latitude',LegTitleC=LegTitleC,LegLength=unit(10,'cm'),
                    LegWidth=0.3,FMidPoint=0,ContinLegTrans=ContinLegTrans,PThr=0.05,
                    xRange=AusMapXRange,yRange=AusMapYRange,AxTxSize=AxTxSize,RemStripBack=T,
                    Labels=FigLabs,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.02),ExpPath=ExpPath,Size=PPSize(H=0.49,W=1),
                    AddLayersFront=list(AusMap$australia,AusMap$states))
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Trends in weather variables')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      Plots <- list()
      
      for (i in seq_along(Vars)) {
        Temp <- copy(RPTrends[IsGrid==T & Seas==S & Period%in%last(Periods,n=2) & TrendVar==Vars[i]])
        Temp <- merge(Temp,Stations[,-c('IsGrid','State2')],by=c('Name','ID'),sort=F)
        Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=Vars,NewLevs=c('Temperature','Precipitation'))]
        
        if (i==1) LegTitleC <- 'Linear trend in average daily maximum temperature (\u00B0C/decade)'
        if (i==2) LegTitleC <- 'Linear trend in in average total precipitation (mm/decade)'
        if (i==1) CColorsCont <- MyColors$BuRd
        if (i==2) CColorsCont <- rev(MyColors$BuRd)
        if (i==1) ContinLegTrans <- MyTrans$SymSqrt
        if (i==2) ContinLegTrans <- MyTrans$SymThird
        
        Labels <- MySplit(FigLabs[1:4],2)[[i]]
        
        Plots[[i]] <- DoGGPlot(Data=Temp[Period%in%last(Periods,n=2)],Map=AusMap,Type='point-pval',xCol='Long',yCol='Lat',cCol=c('LN_SL','YP_P'),
                               hFacet='Period',vFacet='TrendVar',CheckFacets=F,xyOffset=0.07,ContinLeg=T,fConv=0.1,P2Type='marked',
                               PSize=PSize,MinPSize=MinPSize,PShape=15,Stroke=0,RemPanMarg=T,CColorsCont=CColorsCont,CColors='black',
                               LegPos='top',LegNRow=1,LegKeyText=LegKeyText*0.8,LegKeySize=0,LegTitRot=0,AxisLabCoords=T,yAngle=0,
                               xLab='Longitude',yLab='Latitude',LegTitleC=LegTitleC,LegLength=unit(10,'cm'),
                               LegWidth=0.3,FMidPoint=0,ContinLegTrans=ContinLegTrans,PThr=0.05,
                               xRange=AusMapXRange,yRange=AusMapYRange,AxTxSize=AxTxSize,RemStripBack=T,
                               Labels=Labels,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.02),
                               AddLayersFront=list(AusMap$australia,AusMap$states))
      }
      
      ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Trend of Vars - %s.pdf",Seasons[S+1]))
      p <- Plot_Grid(Plots,CommLegend=F,LegPos=T,RemTitles=F,RemLabs=F,RemAxText=F,RemMargs=F,
                     ExpPath=ExpPath,Size=PPSize(H=0.65,W=0.75),Device='pdf')
    }
  }
  #=========================================================================================================================
  message('Plot (Map): Average trends in return periods')
  #=========================================================================================================================
  {
    for (S in 0:5) {
      for (i in 1:2) {
        if (i==1) Cols <- c('RP','RPt','RPr')
        if (i==2) Cols <- Vars
        Temp <- copy(RPTrends[IsGrid==T & Seas==S & TrendVar%in%Cols & Period%in%data.table::last(Periods,n=2)])
        Temp <- merge(Temp,Stations[,-c('IsGrid','State2')],by=c('Name','ID'),sort=F)
        Temp[, IsTrend:=Trends[IsTrend+2]]
        Temp[, IsTrend:=factor(IsTrend,levels=Trends)]
        
        if (i==1) {
          Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=c('RPt','RPr','RP'),NewLevs=c('Hot Years','Dry Years','Hot and Dry Years'),AsFactor=T)]
        }
        if (i==2) {
          Temp[, TrendVar:=Change_Levels(TrendVar,OldLevs=Vars,NewLevs=c('Temperature','Precipitation'),AsFactor=T)]
        }
        
        if (i==1) yLab <- 'Trend of return periods (yr/decade)'
        if (i==2) yLab <- 'Trend of weather variables (/decade)'
        
        if (i==1) ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Bar - Av Trend of RP - %s.pdf",Seasons[S+1]))
        if (i==2) ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Bar - Av Trend of Vars - %s.pdf",Seasons[S+1]))
        
        p1 <- DoGGPlot(Data=Add_Aggregate_Unit(Temp,Col='State2',AggUnit='Australia'),
                       Type='bar',xCol='TrendVar',yCol='LN_SL',fCol='TrendVar',hFacet='State2',vFacet='Period',FacetScale='free_y',
                       FColors=YrCols,CColors='black',LegPos='top',LegKeyText=LegKeyText,UseMean=T,ErrBarType='se',BarLabSize=1.8,BarLabDigs=2,
                       yLab=yLab,AxTxSize=AxTxSize,xAngle=45,yAngle=0,Lwd=0.2,yConv=0.1,
                       RemGrids=T,RemPanMarg=F,HLine=0,HLineCol='black',HLineW=0.1,
                       ExpPath=ExpPath,Size=PPSize(H=0.5,W=1),Device='pdf')
        
        if (i==1) LegTitleC <- 'Trend analysis of return periods'
        if (i==2) LegTitleC <- 'Trend analysis of weather variables'
        
        if (i==1) ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Trend Sig of RP - %s.pdf",Seasons[S+1]))
        if (i==2) ExpPath <- paste0(Dir$Analyses,sprintf("/1 - Map - Trend Sig of Vars - %s.pdf",Seasons[S+1]))
        
        p1 <- DoGGPlot(Data=Temp[Period%in%data.table::last(Periods,n=2)],Map=AusMap,Type='points',xCol='Long',yCol='Lat',cCol='IsTrend',
                       hFacet='TrendVar',vFacet='Period',PSize=PSize*0.917,PShape=15,Stroke=0,
                       CColors=TrCols,xyOffset=0.07,LegScale=2,ForceLegScale=T,RemPanMarg=T,
                       LegPos='top',LegNRow=1,LegKeyText=LegKeyText,LegKeySize=0,LegTitRot=0,AxisLabCoords=T,xLab='Longitude',yLab='Latitude',
                       xRange=AusMapXRange,yRange=AusMapYRange,AxTxSize=AxTxSize,RemStripBack=T,
                       LegTitleC=LegTitleC,yAngle=0,ExpPath=ExpPath,Size=PPSize(H=0.483,W=1-ifelse(i==2,0.3,0)),Device='pdf',
                       Labels=FigLabs,LabelsSize=FigLabSize,LabelsOffs=c(0.02,0.02),
                       AddLayersFront=list(AusMap$australia,AusMap$states))
      }
    }
  }
  #=========================================================================================================================
}

#=========================================================================================================================
if (OnServer) for (i in 1:200) print('Job is done!')


