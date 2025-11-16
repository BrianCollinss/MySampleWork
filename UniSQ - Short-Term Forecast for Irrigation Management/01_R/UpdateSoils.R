
# Run this after running "DownloadSoils.R".

SoilFileX  <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/04_Data/Soils/Soils.apsimx'
SoilFileX2 <- 'C:/Users/U8019357/OneDrive - UniSQ/00_Projects/2024.04.26 Short-Term Forecast for Irrigation Management/04_Data/Soils/SoilsModified.apsimx'

Soils   <- GetNodeFromJSON(Json=SoilFileX, Attrs=c('$type'), Values='Models.Soils.Soil, Models')

NewPAWCs  <- c(100, 100, 170)
NewDepths <- c(800, 1400, 1400)
NewSWCONs <- c(0.4, 0.6, 0.4)
ScaleDepth <- F

OutData <- data.table()

NewSoils <- list()
for (s in 1:length(Soils)) {
  Name     <- unlist(Soils[[s]]$Name)
  Lat      <- unlist(Soils[[s]]$Latitude)
  Long     <- unlist(Soils[[s]]$Longitude)
  Country  <- strsplit(Name,split='-')[[1]][1]
  Location <- strsplit(Name,split='-')[[1]][2]
  
  Thicks <- unlist(Soils[[s]]$Children[[2]]$Thickness)
  DULs   <- unlist(Soils[[s]]$Children[[2]]$DUL)
  LL15s  <- unlist(Soils[[s]]$Children[[2]]$LL15)
  Ks     <- unlist(Soils[[s]]$Children[[2]]$KS)
  SWCONs <- unlist(Soils[[s]]$Children[[3]]$SWCON)
  
  Crops <- list()
  for (cr in 1:length(Soils[[s]]$Children[[2]]$Children)) {
    Crop <- Soils[[s]]$Children[[2]]$Children[[cr]]
    if (Crop$Name=='MaizeSoil') Crops[[length(Crops)+1]] <- Crop
  }
  Soils[[s]]$Children[[2]]$Children <- Crops
  
  LLs    <- unlist(Crops[[1]]$LL)
  TWHC   <- sum(Thicks*(DULs-LL15s))
  PAWC   <- sum(Thicks*(DULs-LLs))
  
  OutData <- rbind(OutData, data.table(Country=Country, Location=Location, Name=Name, Lat=Lat, Long=Long, Depth=sum(Thicks), 
                                       TWHC=TWHC, PAWC=PAWC, KS2=mean(Ks[1:2]), SWCON2=mean(SWCONs[1:2])))
  
  for (i in 1:length(NewPAWCs)) {
    SoilNew <- Soils[[s]]
    
    # Modify the soil to match the target depth
    Depth <- sum(unlist(SoilNew$Children[[2]]$Thickness))
    Change <- (NewDepths[i] / Depth - 1)
    CutPoint <- base::which.min(abs(cumsum(unlist(SoilNew$Children[[2]]$Thickness))-NewDepths[i]))
    
    for (chld in 1:length(SoilNew$Children)) {
      if (ScaleDepth==T) {
        if (!is.null(SoilNew$Children[[chld]]$Thickness)) {
          SoilNew$Children[[chld]]$Thickness <- as.list(unlist(SoilNew$Children[[chld]]$Thickness) * (1 + Change))
        }
        
      } else {
        for (chld2 in 1:length(SoilNew$Children[[chld]])) {
          if (is.list(SoilNew$Children[[chld]][[chld2]]) && length(SoilNew$Children[[chld]][[chld2]])>1) {
            SoilNew$Children[[chld]][[chld2]] <- as.list(unlist(SoilNew$Children[[chld]][[chld2]])[1:CutPoint])
          }
        }
        
        if (length(SoilNew$Children[[chld]]$Children)>0) {
          
          for (chld2 in 1:length(SoilNew$Children[[chld]]$Children)) {
            
            for (chld3 in 1:length(SoilNew$Children[[chld]]$Children[[chld2]])) {
              
              if (is.list(SoilNew$Children[[chld]]$Children[[chld2]][[chld3]]) && length(SoilNew$Children[[chld]]$Children[[chld2]][[chld3]])>1) {
                SoilNew$Children[[chld]]$Children[[chld2]][[chld3]] <- as.list(unlist(SoilNew$Children[[chld]]$Children[[chld2]][[chld3]])[1:CutPoint])
              }
            }
          }
        }
      }
    }
    
    # Modify the soil to match the target PAWC.
    Thicks <- unlist(SoilNew$Children[[2]]$Thickness)
    DULs   <- unlist(SoilNew$Children[[2]]$DUL)
    LL15s  <- unlist(SoilNew$Children[[2]]$LL15)
    LLs    <- unlist(SoilNew$Children[[2]]$Children[[1]]$LL)
    PAWC   <- sum(Thicks*(DULs-LLs))
    
    Change  <- (NewPAWCs[i] / PAWC - 1)
    
    # Modify a copy of the original soil.
    for (ly in 1:length(SoilNew$Children[[2]]$DUL)) { 
      x <- SoilNew$Children[[2]]$DUL[[ly]] - SoilNew$Children[[2]]$Children[[1]]$LL[[ly]]
      y <- SoilNew$Children[[2]]$DUL[[ly]] - SoilNew$Children[[2]]$LL15[[ly]]
      z <- SoilNew$Children[[2]]$SAT[[ly]] - SoilNew$Children[[2]]$DUL[[ly]]
      
      SoilNew$Children[[2]]$DUL[[ly]] <- SoilNew$Children[[2]]$DUL[[ly]] + x * Change/3 * 1
      SoilNew$Children[[2]]$Children[[1]]$LL[[ly]] <- SoilNew$Children[[2]]$Children[[1]]$LL[[ly]] - x * Change/3 * 2
      
      SoilNew$Children[[2]]$LL15[[ly]] <- (SoilNew$Children[[2]]$DUL[[ly]] - y * (1 + Change))
      SoilNew$Children[[2]]$SAT[[ly]]  <- SoilNew$Children[[2]]$DUL[[ly]] + z
      
      SoilNew$Children[[2]]$LL15[[ly]]   <- min(SoilNew$Children[[2]]$LL15[[ly]], SoilNew$Children[[2]]$Children[[1]]$LL[[ly]] - 0.03)
      SoilNew$Children[[2]]$AirDry[[ly]] <- min(SoilNew$Children[[2]]$AirDry[[ly]], SoilNew$Children[[2]]$LL15[[ly]] - 0.03)
  
      SoilNew$Children[[3]]$SWCON[[ly]] <- NewSWCONs[i]
    }
    
    # With Ks, most of irrigation water is lost via runoff.
    SoilNew$Children[[2]]$KS <- NULL
    
    SoilNew$Name <- paste0(Name,'_',NewPAWCs[i],'_',NewDepths[i])
    NewSoils[[length(NewSoils)+1]] <- SoilNew
  }
}

# Write the CSV table.
fwrite(OutData, file=gsub('Soils.apsimx','Soils.csv',SoilFileX))

# Write the new Soil file.
finalJson <- paste0(c('{"$type": "Models.Core.Simulations, Models",',
                      '"Version": 173,',
                      '"Name": "Simulations",',
                      '"ResourceName": null,',
                      '"Children": [],',
                      '"Enabled": true,',
                      '"ReadOnly": false}'), collapse='')

finalJson <- jsonlite::parse_json(finalJson)
finalJson$Children <- NewSoils
finalJson <- jsonlite::toJSON(finalJson, pretty=T, auto_unbox=T, force=T, null='null', simplifyVector=T)
writeLines(finalJson, SoilFileX2)





