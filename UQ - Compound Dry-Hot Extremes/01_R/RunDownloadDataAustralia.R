
# Automatically download weather records from SILO , output names as location or station number (option)
{
  Project  <- '2020.09.28 Compound Dry-Hot Extremes'
  DownGrid <- T
  
  HTMLFile <- 'SILO 2020.09.30.html' # at least 70% during 1990-2020
  start    <- '18890101' # '18890101'
  username <- 'brian.collins4@uq.edu.au'
  password <- 'silo'
  S60      <- c('9037','21002','10007','77007','8025','18042','50052','51010','10536','18023','10035','41023','8039','81013','65012','35027','9631','8051','51018','78014','41038','55023','77018','10568','10579','10073','89016','10592','79028','24024','42022','10093','75142','18052','8093','53048','8095','10612','54120','10111','10619','51039','65026','25015','21043','10633','43030','23020','18075','12071','12074','74087','72150','24018','52088','76064','65028','8138','74123','10654' )
}
#========================================================================================================
# Sources and Folders
#========================================================================================================
{
  OnServer <- (length(grep(':',getwd()))==0)
  if (OnServer) MainDir <- RemMainDir else MainDir <- 'C:/Users/uqbababa/OneDrive - The University of Queensland/'
  
  source(paste(MainDir,'00_Projects/SharedScripts/MySource.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyFunctions.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyApsim.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/MyPlots.R',sep='/'))
  source(paste(MainDir,'00_Projects/SharedScripts/TrendAnalysis.R',sep='/'))
  
  Dir <- APSIM_Paths2(Dir=list(),Project=Project,Run=NULL,MainDir=MainDir)
  x <- sapply(Dir, function(x) {if(!dir.exists(x)) dir.create(x)})
  cat("\014")
}
#========================================================================================================
finish   <- format(today()-1,format='%Y%m%d')

if (DownGrid) {
  #========================================================================================================
  message("Downloading grid data")
  
  api_url  <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/DataDrillDataset.php'
  if(!dir.exists(paste0(Dir$Data,sprintf('/GridData - %sD',Mesh)))) dir.create(paste0(Dir$Data,sprintf('/GridData - %sD',Mesh)))
  
  Mesh <- 1
  if (Mesh==1) {
    ln <- seq(111.7,153.95,Mesh)
    lt <- seq(-43.15,-10.5,Mesh)
    
  } else {
    ln <- seq(111.9,153.65,Mesh)
    lt <- seq(-43.15,-10.5,Mesh)
  }
  Grid <- as.data.table(expand.grid(ln,lt))
  names(Grid) <- c('Long','Lat')
  Grid$Out <- F
  Grid$NoData <- F
  
  for (i in seq(along=Grid$Long)) {
    url <- sprintf('?lat=%s&lon=%s&format=apsim&start=%s&finish=%s&username=%s&password=gui&comment=standard',Grid$Lat[i],Grid$Long[i],start,finish,username)
    url <- curl(paste0(api_url,url),open='r')
    new_data  <- readLines(url)
    
    Grid[i, ID:=i]
    Grid[i, Name:=paste0('Grid',i)]
    
    if (length(new_data)<30) {
      Grid[i, Out:=T]
      next()
      
    } else {
      x <- grep('0.0   0.0   0.0   0.0',new_data[100])
      if (length(x)>0) {
        Grid[i, NoData:=T]
        next()
      }
      
      File <- paste0(Dir$Data,sprintf('/GridData - %sD/Grid',Mesh),i,'.met')
      writeLines(new_data,File)
    }
  }
  
  Stations2 <- unique(SimFilesFull[, .(Site,Lat,Long,Region2)])
  Stations2[, Region2:=as.character(Region2)]
  Stations2[, Name:=as.character(Site)]
  for (i in 1:nrow(Stations2)) {
    if (length(grep('^VIC',Stations2$Region2[i]))>0) Stations2$State[i] <- 'Victoria'
    else if (length(grep('^WA',Stations2$Region2[i]))>0) Stations2$State[i] <- 'Western Australia'
    else if (length(grep('^SA',Stations2$Region2[i]))>0) Stations2$State[i] <- 'South Australia'
    else if (length(grep('^NSW',Stations2$Region2[i]))>0) Stations2$State[i] <- 'New South Wales'
    else if (length(grep('^QLD',Stations2$Region2[i]))>0) Stations2$State[i] <- 'Queensland'
  }
  Stations2[, `:=`(IsGrid=F,Region2=NULL,Site=NULL,ID=0,Out=F,NoData=F)]
  
  Files1 <- list.files(paste0(Dir$Weather,'/Base'),pattern='*.met',full.names=T)
  Files <- File_Base_Name(Files1)
  Files <- sapply(Files, function(x) x <- gsub(' ','',x),simplify=T)
  for (i in 1:nrow(Stations2)) {
    x <- gsub(' ','',Stations2$Name[i])
    Stations2$Name[i] <- File_Base_Name(Files1[Files %like% x])
  }
  
  States <- readOGR(dsn="C:/Users/uqbababa/OneDrive - The University of Queensland/00_Projects/Archive/map graph_R code_chenu2013/0.input map shp files",layer="states",verbose=F)
  
  Stations <- Grid[Out==F & NoData==F,]
  Stations[, `:=`(IsGrid=T)]
  
  for (StId in as.numeric(States@data$COUNT)) {
    Rows <- Find_Inside_Points(MapDF=fortify(States@polygons[[StId]]),MXCol='long',MYCol='lat',
                               Points=Stations,PXCol='Long',PYCol='Lat',Buffer=0.05,ExpId=T,ToDT=F)
    if (length(Rows)>0) Stations[Rows, State:=StateMap[as.character(StateMap$New)==States@data$STATE[StId],'Old']]
  }
  Stations[is.na(State) & Long>140 & Lat> -36 & Lat< -28, State:='New South Wales']
  Stations[is.na(State) & Long>140 & Lat< -39, State:='Tasmania']
  Stations[is.na(State) & Long>129 & Long<138 & Lat> -26, State:='Northern Territory']
  Stations[is.na(State) & Long>129 & Long<141 & Lat< -26, State:='South Australia']
  Stations[is.na(State) & Long>141 & Lat< -36 & Lat> -40, State:='Victoria']
  Stations[is.na(State) & Long<129, State:='Western Australia']
  Stations[is.na(State) & Long>140 & Long<142 & Lat< -35 & State=='New South Wales', State:='Victoria']
  Stations[is.na(State), State:='Queensland']
  Stations <- rbind(Stations,Stations2)
  Stations[, State2:=Change_Levels(State,StateMap$Old,StateMap$New)]
  
  save(Grid,Stations,file=paste0(Dir$Data,sprintf('/GridData - %sD/Grid',Mesh),'.RData'))
  
} else {
  #========================================================================================================
  message("Downloading station data")
  
  api_url  <- 'https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php'
  load("C:/Users/uqbababa/OneDrive - The University of Queensland/02_Data/SimFilesFull.RData")
  
  HTML <- readLines(paste(Dir$R,HTMLFile,sep='/'))
  StationsID <- strsplit(HTML,'data-value=')[[1]][-1]
  StationsID <- lapply(StationsID, function(x) return(str_extract(x, "\\-*\\d+\\.*\\d*")))
  StationsID <- unlist(StationsID)
  StationsID <- union(StationsID,S60)
  
  Stats <- c('South Australia','Northern Territory','Queensland','Victoria','Western Australia','New South Wales','Tasmania')
  StatsPos <- str_locate(HTML,Stats)[,1]
  
  AllStates <- NULL
  for (St in StationsID) {
    if (St %in% S60) {
      Temp <- SimFilesFull[`Station Number`==St][1,]
      if (length(grep('^VIC',Temp$Region2))>0) Temp <- 'Victoria'
      else if (length(grep('^WA',Temp$Region2))>0) Temp <- 'Western Australia'
      else if (length(grep('^SA',Temp$Region2))>0) Temp <- 'South Australia'
      else if (length(grep('^NSW',Temp$Region2))>0) Temp <- 'New South Wales'
      else if (length(grep('^QLD',Temp$Region2))>0) Temp <- 'Queensland'
      AllStates <- c(AllStates,Temp)
      
    } else {
      Pos <- str_locate(HTML,paste0('"',St))[1]
      Pos <- last(which(Pos>StatsPos))
      AllStates <- c(AllStates,Stats[Pos])
    }
  }
  
  AllStations <- data.table(ID=StationsID,State=AllStates)
  setwd(Dir$Data)
  
  for (i in seq(along=StationsID)) {
    
    url <- sprintf('?start=%s&finish=%s&station=%s&format=apsim&comment=XN&username=%s',start,finish,StationsID[i],username)
    url <- curl(paste0(api_url,url),open='r')
    new_data  <- readLines(url)
    
    new_name  <- gsub("!station name =","",new_data[3])	
    new_name  <- gsub("^\\s","",new_name)
    new_name  <- gsub("\\s\\s","",new_name)
    
    lat  <- as.numeric(str_extract(new_data[4], "\\-*\\d+\\.*\\d*"))
    long <- as.numeric(str_extract(new_data[5], "\\-*\\d+\\.*\\d*"))
    
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    new_name <- tolower(str_trim(new_name))
    new_name <- simpleCap(new_name)
    
    file_name <- paste0(new_name,'.met')
    Ext       <- file_ext(file_name)
    
    NameWOutExt2 <- gsub("\\.met$","",file_name)
    NameWOutExt2 <- gsub(c("\\(")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\)")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\.+")," ",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\s\\."),".",NameWOutExt2)
    NameWOutExt2 <- gsub(c("\\s\\s")," ",NameWOutExt2)
    
    NameWOutExt2 <- Capitalize_First_Letters(NameWOutExt2,ignorePar=F)
    NameWithExt2 <- paste0(NameWOutExt2,'.',Ext)
    NameWOutExt3 <- paste0(NameWOutExt2,' - ',StationsID[i])
    NameWithExt3 <- paste0(NameWOutExt3,'.',Ext)
    
    writeLines(new_data,NameWithExt3)
    
    AllStations$Name[i]   <- NameWOutExt2
    AllStations$NameID[i] <- NameWOutExt3
    AllStations$Lat[i]    <- lat
    AllStations$Long[i]   <- long
  }
  
  Stations <- AllStations[, .(ID,Name,NameID,State,Lat,Long)]
  save(Stations,file=paste0(Dir$Data,'/AllStations.RData'))
}
  
  
