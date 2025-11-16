

p_load(char=c('base','agricolae','boot','colorspace','cowplot','ggcorrplot','ggplot2','ggpubr','Rmisc',
              'magrittr','grDevices','grid','lubridate','plyr','RColorBrewer','rsample','scales','segmented',
              'SiZer','sp','utils','stringr','utils'), install=F, repos=getOption('repos'))

{
  MyColors          <- list()
  MyColors$ET4      <- c(rgb(0/255,140/255,255/255),rgb(178/255,0/255,178/255),rgb(255/255,128/255,0/255),rgb(128/255,0/255,0/255))
  MyColors$ET6      <- c(rgb(0/255,140/255,255/255),rgb(178/255,0/255,178/255),rgb(255/255,128/255,0/255),rgb(128/255,0/255,0/255),
                         rgb(22/255,22/255,111/255),rgb(128/255,55/255,0/255))
  
  MyColors$BkPrYl   <- function(n) {bpy.colors(n)} # From black to purple to yellow, from Bangyou's paper
  MyColors$Set1     <- brewer.pal(9,"Set1")[-c(6)]
  MyColors$Dark2    <- brewer.pal(8,'Dark2')
  MyColors$RdBu     <- brewer.pal(11,'RdBu')
  MyColors$BuRd     <- rev(brewer.pal(11,'RdBu'))
  MyColors$Acc      <- brewer.pal(8,'Accent')
  MyColors$Pair     <- brewer.pal(12,'Paired')
  MyColors$Reds     <- brewer.pal(9,'Reds')
  MyColors$RedsX    <- MyColors$Reds[-1]
  MyColors$RainB    <- rainbow(10)
  MyColors$Blue2Red <- colorRampPalette(c('dodgerblue','green','orange','mediumvioletred','red'))
  MyColors$RdYlVi   <- brewer.pal(11,'Spectral')[-c(6)]
  MyColors$ViYlRd   <- rev(MyColors$RdYlVl)
  MyColors$YlGn     <- brewer.pal(9,'YlGn')[-1]
  MyColors$GnYl     <- rev(MyColors$YlGn)
  MyColors$YlGnX    <- MyColors$YlGn[-1]
  MyColors$GnYlX    <- rev(MyColors$YlGnX)
  MyColors$GnYl2    <- sequential_hcl("ag_GrnYl",n=9)
  MyColors$YlGn2    <- rev(MyColors$GnYl2)
  MyColors$YlGnBu   <- brewer.pal(9,'YlGnBu')[-1]
  MyColors$YlGnBuX  <- MyColors$YlGnBu[-1]
  MyColors$YlOrRd   <- brewer.pal(9,'YlOrRd')
  MyColors$YlOrRdX  <- MyColors$YlOrRd[-1]
  MyColors$RdYlGn   <- brewer.pal(11,'RdYlGn')
  MyColors$RdYlGnX  <- MyColors$RdYlGn[-c(5,6,7)]
  MyColors$GnYlRd   <- rev(brewer.pal(11,'RdYlGn'))
  MyColors$GnYlRdX  <- rev(MyColors$RdYlGnX)
  MyColors$RdYlBu   <- brewer.pal(11,'RdYlBu')
  MyColors$RdYlBuX  <- MyColors$RdYlBu[-c(5,6,7)]
  MyColors$BuYlRd   <- rev(MyColors$RdYlBu)
  MyColors$BuYlRdX  <- MyColors$BuYlRd[-c(5,6,7)]
  MyColors$PRGn     <- brewer.pal(11,'PRGn')
  MyColors$PRGnX    <- MyColors$PRGn[-c(5,6,7)]
  MyColors$BrBG     <- brewer.pal(11,'BrBG')
  MyColors$BrBGX    <- MyColors$BrBG[-c(6)]
  MyColors$BuBr     <- diverge_hcl(palette='Vik',n=11)[-6]
  MyColors$BrBu     <- rev(MyColors$BuBr)
  MyColors$BlueRed  <- c('blue3','red3')
  # MyColors$JcPal12  <- jcolors("pal12")
  MyColors$ggplot   <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  PMarg <- 3
  CapSpace <- 3
  Paper <- list(); 
  Paper$A4   <- c(21,30); Paper$A4r <- rev(Paper$A4)
  Paper$A411 <- c(21-PMarg,1/1*(30-PMarg-CapSpace)); Paper$A4r11 <- rev(Paper$A411)
  Paper$A412 <- c(21-PMarg,1/2*(30-PMarg-CapSpace)); Paper$A4r12 <- rev(Paper$A412)
  Paper$A413 <- c(21-PMarg,1/3*(30-PMarg-CapSpace)); Paper$A4r13 <- rev(Paper$A413)
  Paper$A414 <- c(21-PMarg,1/4*(30-PMarg-CapSpace)); Paper$A4r14 <- rev(Paper$A414)
  Paper$A415 <- c(21-PMarg,1/5*(30-PMarg-CapSpace)); Paper$A4r15 <- rev(Paper$A414)
  Paper$A425 <- c(21-PMarg,2/5*(30-PMarg-CapSpace)); Paper$A4r25 <- rev(Paper$A425)
  Paper$A435 <- c(21-PMarg,3/5*(30-PMarg-CapSpace)); Paper$A4r35 <- rev(Paper$A435)
  Paper$A445 <- c(21-PMarg,4/5*(30-PMarg-CapSpace)); Paper$A4r45 <- rev(Paper$A445)
  Paper$A423 <- c(21-PMarg,2/3*(30-PMarg-CapSpace)); Paper$A4r23 <- rev(Paper$A423)
  Paper$A434 <- c(21-PMarg,3/4*(30-PMarg-CapSpace)); Paper$A4r34 <- rev(Paper$A434)
  
  Paper$A40045 <- Paper$A4;   Paper$A40045[1] <- 4/5*(Paper$A40045[1]+PMarg)-PMarg
  Paper$A40056 <- Paper$A4;   Paper$A40056[1] <- 5/6*(Paper$A40056[1]+PMarg)-PMarg
  Paper$A40067 <- Paper$A4;   Paper$A40067[1] <- 6/7*(Paper$A40067[1]+PMarg)-PMarg
  Paper$A40078 <- Paper$A4;   Paper$A40078[1] <- 7/8*(Paper$A40078[1]+PMarg)-PMarg
  Paper$A41123 <- Paper$A411; Paper$A41123[1] <- 2/3*(Paper$A41123[1]+PMarg)-PMarg
  Paper$A41223 <- Paper$A412; Paper$A41223[1] <- 2/3*(Paper$A41223[1]+PMarg)-PMarg
  Paper$A41256 <- Paper$A412; Paper$A41256[1] <- 5/6*(Paper$A41256[1]+PMarg)-PMarg
  Paper$A42323 <- Paper$A423; Paper$A42323[1] <- 2/3*(Paper$A42323[1]+PMarg)-PMarg
  Paper$A44512 <- Paper$A445; Paper$A44512[1] <- 1/2*(Paper$A44512[1]+PMarg)-PMarg
  Paper$A41312 <- Paper$A413; Paper$A41312[1] <- 1/2*(Paper$A41312[1]+PMarg)-PMarg
  Paper$A41323 <- Paper$A413; Paper$A41323[1] <- 2/3*(Paper$A41323[1]+PMarg)-PMarg
  Paper$A41412 <- Paper$A414; Paper$A41412[1] <- 1/2*(Paper$A41412[1]+PMarg)-PMarg
  Paper$A41423 <- Paper$A414; Paper$A41423[1] <- 2/3*(Paper$A41423[1]+PMarg)-PMarg
  Paper$A42512 <- Paper$A425; Paper$A42512[1] <- 1/2*(Paper$A42512[1]+PMarg)-PMarg
  
  MyTrans <- list()
  MyTrans$SymSqrt   <- c(function(x){sign(x)*abs(x)^(1/2)}, function(x){sign(x)*(abs(x)^2)})
  MyTrans$SymExp    <- c(function(x){sign(x)*exp(abs(x))}, function(x){sign(x)*log(abs(x))})
  MyTrans$SymPow2   <- c(function(x){sign(x)*abs(x)^2}, function(x){sign(x)*sqrt(abs(x))})
  MyTrans$SymThird  <- c(function(x){sign(x)*abs(x)^(1/3)}, function(x){sign(x)*(abs(x)^3)})
  MyTrans$SymLog    <- c(function(x){sign(x)*log(abs(x))}, function(x){sign(x)*exp(abs(x))})
  MyTrans$SymLog10  <- c(function(x){sign(x)*log10(abs(x))}, function(x){sign(x)*10^abs(x)})
  
  MySymb <- list()
  MySymb$Degree <- '\u00B0'
  MySymb$LessEq <- '\u2264'
  MySymb$GreaEq <- '\u2265'
}

#========================================================================================================
#========================================================================================================

DoGGPlot <- function(Data=NULL,Map=NULL,Type='box',xConv=1,yConv=1,fConv=1,cConv=1,
                     xCol=NULL,yCol=NULL,gCol=NULL,fCol=NULL,cCol=NULL,eCol=NULL,shCol=NULL,
                     sCol=NULL,ltCol=NULL,lbCol=NULL,errbCol=NULL,rgcCol=NULL,rggCol=gCol,corCols=NULL,
                     # Facets --------------------
                     hFacet='.',vFacet='.',FacetScale='fixed',FacetSpace='fixed',Margines=F,CheckFacets=T,NRow=NULL,NCol=NULL,
                     hFacetPref=NULL,hFacetSuff=NULL,vFacetPref=NULL,vFacetSuff=NULL,FacetSwitch=NULL,
                     FacetLabeller=NULL,FacetGridAxes='margins',FacetGridLabs='all',DropFacets=T,
                     StripSizes=NULL,StripColor='gray90',StripPlacement='outside',StripFace='bold',StripMarg=NULL,
                     RemPanMarg=F,RemStripBack=F,TextCol=NULL,
                     # Object and Legend Colors --------------------
                     FColors=NULL,FColorsCont=NULL,CColors=NULL,CColorsCont=NULL,Alpha=1,
                     LegAlpha=NULL,NaColor=NA,MidPointCol='dimgray',PThr=0.05,P2Type=NULL,
                     # Box Plop Properties --------------------
                     BoxCoef=c(0.1,0.9),BoxDodge=NULL,BoxWidth=0.9,RemOutliers=T,OutlierSize=0.3,Preserve='single',
                     # Bar Plop Properties --------------------
                     BarPos='dodge',BarWidth=0.9,BarDodge=NULL,BarMinY=0,PointLabGap=0.02,
                     BarLabSize=0,BarLabDigs=2,BarLabJust=NULL,BarLabType='mirror',BarLabPos=NULL,BarLabGap=0.02,BarLabAngle=0,BarLabCol='black',
                     # Errorbar Properties --------------------
                     ErrBarType=NULL,ErrBarCL=0.95,ErrBarWidth=0.3,ErrBarLType=NULL,ErrBarHoriz=F,ErrBarInFront=T,ErrBarLwd=NULL,
                     ErrBarGeom='errbar',DiffTest=NULL,DiffTestOn='x',DiffTestExcX=NULL,DiffTestExcG=NULL,DiffLabsMerge=NULL,
                     # Line Weight and Point Size --------------------
                     Lwd=0.1,LType=1,LTypes=NULL,PSize=1,PShape=16,MinPSize=0.3,
                     AddJitter=F,PointDodge=NULL,Stroke=0.1,Func='mean',
                     # Functions applied and date format --------------------
                     AggrFunc='none',AllCombsRep=NULL,AllCombsLab=T,DateFormat='%d/%m',Interpolate=F,
                     # Plot Title, SubTitle and Caption --------------------
                     Title=NULL,SubTitle=NULL,Caption=NULL,Tag=NULL,TitleColors=NULL,TitleSizes=NULL,
                     # Axis Labels and Margines, Date Format, Graph Title and Subtitle --------------------
                     xLab=NULL,yLab=NULL,xLabMarg=NULL,yLabMarg=NULL,xLabVJust=NULL,yLabVJust=NULL,
                     # Axis Numbers and Labels, Minor and Major Grid Properties --------------------
                     xAngle=0,xBreaks=NULL,xLabels=NULL,yAngle=0,yBreaks=NULL,yLabels=NULL,xAxTxJust=NULL,yAxTxJust=NULL,
                     xyOffset=0.05,xRange=NULL,yRange=NULL,AxisLabCoords=NULL,RemAxesLabs=F,RemAxesLines=T,RemAxesTicks=F,
                     AxTxSize=5,AxTiSize=NULL,xLabFace='bold',yLabFace='bold',AxTxCol=NULL,RemGrids=F,RemMinGrids=F,
                     CoordSymbol='\u00B0',RevAx=F,ExpandAxes=T,AxesInclude=NULL,
                     # Colorbar Properties --------------------
                     FMidPoint=NULL,FMidColor=NULL,CheckFMidPoint=F,NoGuides=F,GuideOrd=NULL,ContinLeg=F,PrettyLeg=T,RoundFirst=F,
                     ContinLegTrans=NULL,ContLegTransform='identity',LegLessDigs=NULL,NumBins=100,BinWidth=0.75,
                     # Legend General Properties --------------------
                     LegPos='none',LegsArrange='horizontal',LegDirection='horizontal',LegJust=NULL,LegBoxJust=NULL,LegMarg=NULL,LegBoxMarg=NULL,LegByRow=T,
                     LegLabPos=NULL,LegBreaks=NULL,LegLabs=NULL,LegBreaksAdd=NULL,LegLabsGap=NULL,LegLabsStart=1,LegDrop=T,LegTxAlign=NULL,LegFrameWidth=NULL,
                     LegLimits=NULL,LegSymmetr=F,LegRaster=F,LegSpaceX=NULL,LegSpaceY=NULL,LegKeySpaceX=NULL,LegKeySpaceY=NULL,ColorKeyBins=5000,RevSizes=F,
                     LegLength=NULL,LegWidth=NULL,LegKeySize=NULL,LegKeyText=5,LegKeyTextMarg=NULL,LegScale=1,ForceLegScale=F,LegBack=NULL,LegBackAlpha=1,
                     LegTitPos='top',LegTitSize=NULL,LegTitRot=NULL,LegTitHJust=0.5,LegNumDigs=NULL,LegNumBreaks=10,LegNRow=1,RevGroups=F,LegRev=F,
                     # Legend Title --------------------
                     LegTitleF=NULL,LegTitleC=NULL,LegTitleS=NULL,LegTitleSh=NULL,LegTitleLt=NULL,
                     # Regression Lines and Equations --------------------
                     RegType=NULL,ShowReg=T,RegDegree=1,RegXfrom1=F,AddRegEq=F,RegStatFromEq=F,RegEqAddNames=T,
                     RegConf=F,RegEqStat=NULL,RegFullRange=F,RegRange=NULL,RegAggUnit=NULL,RegPThr=NULL,ShowRegAggUnit=F,SafeSegReg=F,
                     Add45L=F,Rect=F,RegLCol=NULL,RegEqCol=NULL,RegEqBold=NULL,RegLwd=0.3,RegLTypes=1,RegInterc=NULL,RegEqTitles=NULL,
                     RegEqType='text',RegEqPos=NULL,RegEqSize=NULL,RegEqMarg=NULL,RegEqNRowsRange=NULL,RegEqNumCols=NULL,RegEqHVadj=c(0,0),RegEqHjust=NULL,
                     RegEqDigs=c(2,1),RegEqStDig=NULL,RegShowLeg=F,RegOnOrig=F,RegDrop=F,EqBoxAlpha=1.0,EqBoxFill='white',
                     RegData=NULL,RegTable=NULL,
                     # Horizontal and Vertical Lines --------------------
                     HLine=NULL,HLineCol='gray',HLineType=1,HLineW=NULL,VLine=NULL,VLineCol='gray',VLineType=1,VLineW=NULL,HVLineBack=T,
                     # Panel and Margines --------------------
                     PanCol='black',PanLw=0.3,PanSpace=unit(0.1,'cm'),GridLwd=NULL,GridLty=1,GridCol='grey70',
                     RemPanBorder=F,Flip=F,PlotMarg=margin(t=0.1,r=0.1,b=0.1,l=0.1,unit='cm'),PanelOnTop=F,
                     # Labels --------------------
                     Labels='none',LabelsOffs=c(0.05,0.05),LabelsFont=NULL,LabelsSize=6,LabelPos='topleft',LabelJust=NULL,
                     # Tile Label Properties --------------------
                     TileLabSize=NULL,TileLabDigs=2,SizeLabs=NULL,
                     # Theme and Fonts --------------------
                     Theme=theme_bw(base_family=FontFamily),TextSize=NULL,FontFamily='serif',
                     # Files and Sizes --------------------
                     ExpPath=NULL,Size=c(50,50),Device='pdf',Scale=1,Dpi=300,Paper='special',Note=NULL,
                     # Extra Layers, Empty Plot --------------------
                     InsetGraphs=NULL,InsetPos=c(0,0,0.2,0.2),AddEmptyPlot=F,AddLayersFront=NULL,AddLayersBack=NULL,SecXAxis=waiver(),SecYAxis=waiver()) {
  
  EB <- element_blank()
  graphics.off()
  
  if (Type=='empty' || Type=='blank') {
    x <- ggplot()
    if (!is.null(AddLayersBack)) x <- Add_gg_Layers(Base=x,Layers=AddLayersBack)
    x <- x + geom_blank() + Theme + theme(panel.border=EB,axis.text=EB,axis.ticks=EB,panel.grid=EB,axis.title=EB)
    if (!is.null(AddLayersFront)) x <- Add_gg_Layers(Base=x,Layers=AddLayersFront)
    return(x)
  }
  
  Cols <- c(xCol,yCol,gCol,fCol,cCol,eCol,shCol,sCol,ltCol,lbCol,errbCol,hFacet,vFacet,BarLabPos,rgcCol,rggCol,unlist(corCols))
  Cols <- unique(setdiff(Cols,c('.')))
  DataO <- as.data.table(Data)[, intersect(Cols,names(Data)), with=F]; rm(Data)
  if (!is.null(RegData)) RegData <- as.data.table(RegData)
  
  xRange1 <- xRange
  yRange1 <- yRange
  xRange  <- yRange <- NULL
  
  # Replacing old plot types.
  {
    if (Type=='bars') Type <- 'bar'
    if (Type%in%c('lines','mlines')) Type <- 'line'
    if (Type%in%c('linesp','mlinesp')) Type <- 'linep'
    if (Type%in%c('points')) Type <- 'point'
    if (Type%in%c('points-pval')) Type <- 'point-pval'
    if (Type%in%c('boxmeanpoint')) Type <- 'boxmeanp'
    if (Type%in%c('boxmeanline')) Type <- 'boxmeanl'
  }
  
  # Initialise theme parameters.
  {
    if (is.null(TextCol)) TextCol <- PanCol
    if (length(xyOffset)==1) xyOffset <- rep(xyOffset,4)
    if (length(xyOffset)==2) xyOffset <- c(rep(xyOffset[1],2),rep(xyOffset[2],2))
    if (length(RegEqHVadj)==1) RegEqHVadj <- c(0,RegEqHVadj)
    if (is.null(GridLwd)) GridLwd <- max(0.05,PanLw/3)
    if (is.null(HLineW)) HLineW <- 0.7*PanLw
    if (is.null(VLineW)) VLineW <- 0.7*PanLw
    if (is.null(LegScale)) LegScale <- 1
    if (is.null(PSize)) PSize < 1
    if (length(RevAx)==1) RevAx <- rep(RevAx,2)
    
    if (is.null(SecXAxis)) {
      SecXAxis <- waiver()
    } else if (length(SecXAxis)>0) {
      if (is_bool(SecXAxis) && SecXAxis==T) SecXAxis <- dup_axis()
    }
    if (is.null(SecYAxis)) {
      SecYAxis <- waiver()
    } else if (length(SecYAxis)>0) {
      if (is_bool(SecYAxis) && SecYAxis==T) SecYAxis <- dup_axis()
    }    
  }
  
  # Preparing data for correlation matrix.
  {
    if (Type %in% c('cor','corr')) {
      ContinLeg <- T
      Keys  <- setdiff(c(hFacet,vFacet),'.')
      corAllCols <- unique(unlist(corCols))
      
      FUNCc <- function(data) { 
        data <- copy(data)
        data[!is.finite(as.matrix(data)) | is.nan(as.matrix(data))] <- NA
        for (col in names(data)) if (all(is.na(data[,col,with=F]))) data[,(col):=0]
        corr <- if (nrow(data)<3) data.table(matrix(rep(0,ncol(data)^2),ncol=ncol(data))) else data.table(round(cor(data,use='pairwise.complete.obs'),2))
      }
      FUNCp <- function(data) {
        data <- copy(data)
        data[!is.finite(as.matrix(data)) | is.nan(as.matrix(data))] <- NA
        for (col in names(data)) if (all(is.na(data[,col,with=F]))) data[,(col):=0]
        p.mat <- if (nrow(data)<3) data.table(matrix(rep(1,ncol(data)^2),ncol=ncol(data))) else data.table(round(cor_pmat(data),4))
        # p.mat[is.na(p.mat)] <- 1
        # return(p.mat)
      }
      FUNCi <- function(data) { i.mat <- data.table(upper.tri(cor(data))); colnames(i.mat) <- corAllCols; return(i.mat) }
      
      DataI <- DataO[, FUNCi(.SD), keyby=Keys, .SDcols=corAllCols]
      DataI[, Xs:=rep(corAllCols,nrow(DataI)/length(corAllCols))]
      DataI <- melt(DataI,measure.vars=corAllCols,variable.name='Ys',value.name='Upper')
      
      DataP <- DataO[, FUNCp(.SD), keyby=Keys, .SDcols=corAllCols]
      DataP[, Xs:=rep(corAllCols,nrow(DataP)/length(corAllCols))]
      DataP <- melt(DataP,measure.vars=corAllCols,variable.name='Ys',value.name='PValue')
      DataP[, Xs:=factor(Xs,levels=corAllCols)]
      DataP[, Ys:=factor(Ys,levels=corAllCols)]
      
      DataO <- DataO[, FUNCc(.SD), keyby=Keys, .SDcols=corAllCols]
      DataO[, Xs:=rep(corAllCols,nrow(DataO)/length(corAllCols))]
      DataO <- melt(DataO,measure.vars=corAllCols,variable.name='Ys',value.name='CorValue')
      DataO[, Xs:=factor(Xs,corAllCols)]
      DataO[, Ys:=factor(Ys,corAllCols)]
      
      DataO[, Upper:=DataI$Upper]
      DataO[, CorLabel:=FixedDigits(CorValue,2)]
      DataO[Upper==T, CorLabel:='']
      DataO[Upper!=T & DataP$PValue<PThr, CorValue:=NA]
      DataO[Upper!=T & DataP$PValue>=PThr, CorValue:=0]
      
      DataO <- DataO[DataP$PValue>=PThr & Upper==T, `:=`(CorValue=0,CorLabel='')]
      DataO <- DataO[DataP$PValue>=PThr & Upper!=T, `:=`(CorValue=NA,CorLabel='NS')]
      DataO <- DataO[Xs!=Ys]
      
      if (is.list(corCols)) {
        DataO <- DataO[Xs %in% corCols[[1]] & Ys %in% corCols[[2]]]
      }
      
      if (is.null(FColorsCont)) FColorsCont <- MyColors$RdBu
      
      Type   <- 'point'
      xCol   <- 'Xs'
      yCol   <- 'Ys'
      fCol   <- 'CorValue'
      lbCol  <- 'CorLabel'
      FMidPoint <- 0
      FMidColor <- 'gray90'
      AggrFunc  <- 'none'
      FacetGridAxes <- 'all_x'
      FacetGridLabs <- 'all_x'
    }
  }
  
  # Initialising data label parameters.
  {
    BarLabParse <- F
    if (BarLabSize==0) BarLabGap <- 0
    if (is.null(BarLabGap)) BarLabGap <- 0
  } 
  
  # This generates letters for statistical compasisons.
  DiffTestFunc <- function() {
    FUNC <- function(data,ColX) {
      AllGroups <- unique(data[,get(ColX)])
      if (!is.null(DiffTestExcX)) AllGroups <- setdiff(AllGroups,DiffTestExcX)
      
      if (length(AllGroups)==1) {
        out <- data.table(x=AllGroups,DiffGroups='')
        names(out)[1] <- ColX
        
      } else {
        if (tolower(DiffTest)=='t') {
          Temp <- list()
          for (Gr in AllGroups) Temp[[Gr]] <- data[get(ColX)==Gr,get(yCol)]
          out  <- TTestP(Temp,PThr=PThr)$groups
          names(out)[1] <- ColX
          
        } else {
          model <- aov(as.formula(paste(yCol,ColX,sep='~')),data=data)
          if (is.null(tidy(model)$statistic)) model <- aov(as.formula(paste(yCol,ColX,sep='~')),data=rbind(data,data))
          if (is.null(summary(model)['F value'])) print(summary(model))
          if (tolower(DiffTest)=='lsd') out    <- LSD.test(model,ColX,alpha=1-ErrBarCL,p.adj='none')
          if (tolower(DiffTest)=='hsd') out    <- HSD.test(model,ColX,alpha=1-ErrBarCL)
          if (tolower(DiffTest)=='duncan') out <- duncan.test(model,ColX,alpha=1-ErrBarCL)
          
          out <- cbind.data.frame(rownames(out$groups),out$means$Q50,out$groups)
          rownames(out) <- NULL
          colnames(out) <- c(ColX,'DiffGroupsMed','DiffGroupsMean','DiffGroups')
        }
      }
      out[[1]] <- as.character(out[[1]])
      out[[2]] <- as.character(out[[2]])
      if (!is.null(DiffTestExcX)) {
        for (dx in DiffTestExcX) {
          out2 <- out[1,] 
          out2[,1] <- dx
          out2[,2] <- ''
          out  <- rbind(out,out2)
        }
      }
      return(out)
    }
    
    if (is.null(gCol)) DiffTestOn <- 'x' 
    if (DiffTestOn=='g') {
      ColX <- gCol[1]
      Keys <- c(Facets,xCol)
      
    } else if (DiffTestOn=='x') {
      ColX <- xCol
      Keys <- c(Facets,gCol)
      
    } else {
      ColX <- setdiff(gCol,xCol)
      Keys <- c(Facets,gCol,xCol)
    }
    Keys <- setdiff(Keys,ColX)
    
    Temp  <- DataO[, FUNC(.SD,ColX), keyby=Keys, .SDcols=setdiff(names(DataO),Facets)]
    DataS <- merge(DataS,Temp,by=c(Keys,ColX),sort=F)
    
    for (Key in c(Keys,ColX)) {
      if (is.factor(DataO[,get(Key)])) {
        DataS[, (Key):=factor(get(Key),levels=levels(DataO[,get(Key)]))]
      }
    }
    
    if (!is.null(DiffLabsMerge)) {
      if (DiffLabsMerge=='sub') {
        DataS[, DiffGroups:=paste0(FixedDigits(get(yCol),BarLabDigs),'[',DiffGroups,']')]
        BarLabParse <<- T
        
      } else if (DiffLabsMerge=='sup') {
        DataS[, DiffGroups:=paste0(FixedDigits(get(yCol),BarLabDigs),'^{',DiffGroups,'}')]
        BarLabParse <<- T
        
      } else if  (DiffLabsMerge=='par') {
        DataS[, DiffGroups:=paste0(FixedDigits(get(yCol),BarLabDigs),'(',DiffGroups,')')]
        BarLabParse <<- F
        
      } else {
        DataS[, DiffGroups:=paste0(FixedDigits(get(yCol),BarLabDigs),DiffGroups)]
        BarLabParse <<- F
      }
    }
    
    DataS  <<- DataS
    lbCol  <<- xLabel <- 'DiffGroups'
    return(xLabel)
  }
  
  # This adds horizontal/vertical line(s) to the plot
  HVLFunc <- function(x) {
    if (!is.null(HLine)) {
      if (is.function(HLine)) {
        if (is.null(HLineCol)) {
          Temp <- DataS[, lapply(.SD,HLine,na.rm=T), keyby=c(Facets,cCol), .SDcols=yCol]
          x <- x + geom_hline(data=Temp,aes_string(yintercept=yCol,color=cCol),linetype=HLineType,lwd=HLineW,show.legend=F)
          
        } else {
          Temp <- DataS[, lapply(.SD,HLine,na.rm=T), keyby=Facets, .SDcols=yCol]
          x <- x + geom_hline(data=Temp,aes_string(yintercept=yCol),colour=HLineCol,linetype=HLineType,lwd=HLineW,show.legend=F)
        }
        
      } else {
        x <- x + geom_hline(yintercept=HLine,colour=HLineCol,linetype=HLineType,lwd=HLineW)
      }
    }
    if (!is.null(VLine)) {
      if (is.function(VLine)) {
        Temp <- DataS[, lapply(.SD,VLine,na.rm=T), keyby=Facets, .SDcols=yCol]
        x <- x + geom_vline(data=Temp,aes_string(xintercept=yCol),colour=VLineCol,linetype=VLineType,lwd=VLineW,show.legend=F)
      } else {
        x <- x + geom_vline(xintercept=VLine,colour=VLineCol,linetype=VLineType,lwd=VLineW)
      }
    }
    return(x)
  }
  
  # Initialising some errorbar-related parameters.
  {
    if (!is.null(eCol) && length(eCol)==1) eCol <- rep(eCol,2)
    if (is.null(eCol) && !is.null(ErrBarType)) AggrFunc <- mean
    if (ErrBarGeom=='ribbon') ErrBarInFront <- F
  }
  
  # This adds error bars to the plot
  ErrBarFunc <- function(x) {
    if (is.null(eCol) && !is.null(ErrBarType)) {
      # When eCol is NOT given
      
      if (ErrBarHoriz) {
        Cols <- unique(c(yCol,gCol,cCol,fCol,sCol,shCol,Facets))
        ColX <- xCol
      } else {
        Cols <- unique(c(xCol,gCol,cCol,fCol,sCol,shCol,Facets))
        ColX <- yCol
      }
      
      ErrData <- data.table(Rmisc::summarySE(data=DataO,measurevar=ColX,groupvars=Cols,conf.interval=ErrBarCL,na.rm=T))
      
      ErrData[is.na(sd), sd:=0]
      ErrData[is.na(se), se:=0]
      ErrData[!is.finite(ci), ci:=0]
      
      if (grepl('se',ErrBarType,fixed=T) && ErrBarType!='se') {
        Temp <- as.numeric(gsub('se','',ErrBarType))
        ErrData[, xse:=Temp*se]
        ErrBarType <- 'xse'
      }
      
      xyMin <- paste0(ColX,'-',ErrBarType)
      
      if (is.null(yRange1)) {
        if (Type=='bar') {
          RangeX <- range(c(ErrData[,get(ColX)+get(ErrBarType)],ErrData[,get(ColX)-get(ErrBarType)]),na.rm=T)
          if (!is.null(BarMinY) && sum(sign(RangeX))!=0 && !ErrBarHoriz) RangeX[which.min(abs(yAxisRange))] <- BarMinY
          
        } else {
          RangeX <- range(c(ErrData[,get(ColX)+get(ErrBarType)],ErrData[,get(ColX)-get(ErrBarType)]),na.rm=T)
        }
        
        if (ErrBarHoriz) {
          xAxisRange <- RangeX <- RangeX + diff(RangeX)*c(-1,1)*xyOffset[1:2]
        } else {
          yAxisRange <- RangeX <- RangeX + diff(RangeX)*c(-1,1)*xyOffset[3:4]
        }
      }
      
      if ('BarLabPos'%in%names(DataS) && !ErrBarHoriz) {
        Temp  <- merge(DataS[,Cols,with=F],ErrData,by=Cols,sort=F)
        if ('LabGaps' %in% names(DataS)) DataS$LabGaps <- NULL
        
        if (FacetScale=='fixed' || FacetScale=='free_x' || (is.null(NRow) && !is.Facet(vFacet))) {
          LabGaps <- rep(BarLabGap*diff(yAxisRange),nrow(DataS))
          
        } else if (!is.null(NRow)) {
          if (Type%in%'bar' && !is.null(BarMinY))
            LabGaps <- DataS[, .(LabGaps=BarLabGap*max(get(yCol))-BarMinY), keyby=hFacet]
          else
            LabGaps <- DataS[, .(LabGaps=BarLabGap*diff(quantile(get(yCol),c(0.05,0.95),na.rm=T))), keyby=hFacet]
          
          DataS <- merge(DataS,LabGaps,by=hFacet,sort=F)
          LabGaps <- DataS$LabGaps
          
        } else {
          if (Type%in%'bar' && !is.null(BarMinY))
            LabGaps <- DataS[, .(LabGaps=BarLabGap*max(get(yCol))-BarMinY), keyby=vFacet]
          else
            LabGaps <- DataS[, .(LabGaps=BarLabGap*diff(quantile(get(yCol),c(0.05,0.95),na.rm=T))), keyby=intersect(Facets,vFacet)]
          
          DataS <- merge(DataS,LabGaps,by=intersect(Facets,vFacet),sort=F)
          LabGaps <- DataS$LabGaps
        }
        
        IndP <- which(!is.na(DataS[,get(yCol)]) & DataS[,get(yCol)]>=0)
        IndN <- which(!is.na(DataS[,get(yCol)]) & DataS[,get(yCol)]< 0)
        
        if (is.null(xAxisRange) && is.numeric(DataS[,get(xCol)])) xAxisRange <- DataS[,range(get(xCol),na.rm=T)] + diff(DataS[,range(get(xCol),na.rm=T)])*c(-1,1)*xyOffset[1:2]
        if (is.null(yAxisRange) && is.numeric(DataS[,get(yCol)])) yAxisRange <- DataS[,range(get(yCol),na.rm=T)] + diff(DataS[,range(get(yCol),na.rm=T)])*c(-1,1)*xyOffset[3:4]
        
        if (BarLabType=='bellow') {
          DataS$BarLabPos <- DataS$BarLabPos - Temp[,get(ErrBarType)] - LabGaps
          if (is.null(yRange1)) yAxisRange[1] <- min(yAxisRange[1],min(DataS$BarLabPos,na.rm=T) - any(DataS$BarLabPos<0)*1.5*xyOffset[3]*diff(yAxisRange))
        }
        if (BarLabType=='above') {
          DataS$BarLabPos <- DataS$BarLabPos + Temp[,get(ErrBarType)] + LabGaps
          if (is.null(yRange1)) yAxisRange[2] <- max(yAxisRange[2],max(DataS$BarLabPos,na.rm=T) + any(DataS$BarLabPos>0)*1.5*xyOffset[4]*diff(yAxisRange))
        }
        if (BarLabType=='mirror') {
          DataS$BarLabPos[IndP] <- DataS[IndP, get(yCol)] + Temp[IndP,get(ErrBarType)] + LabGaps[IndP]
          DataS$BarLabPos[IndN] <- DataS[IndN, get(yCol)] - Temp[IndN,get(ErrBarType)] - LabGaps[IndN]
          if (is.null(yRange1)) yAxisRange[1] <- min(yAxisRange[1],min(DataS$BarLabPos,na.rm=T) - any(DataS$BarLabPos<0)*1.5*xyOffset[3]*diff(yAxisRange),na.rm=T)
          if (is.null(yRange1)) yAxisRange[2] <- max(yAxisRange[2],max(DataS$BarLabPos,na.rm=T) + any(DataS$BarLabPos>0)*1.5*xyOffset[4]*diff(yAxisRange),na.rm=T)
        }
        if (BarLabType=='aroundzero') {
          DataS$BarLabPos[IndP] <- -LabGaps[IndP]
          DataS$BarLabPos[IndN] <- +LabGaps[IndN]
        }
      }
      
      #ErrData <- ErrData[!is.na(ci) & !is.nan(ci) & ci!=0]
      
      if (!ErrBarHoriz) {
        if (ErrBarGeom=='errbar') {
          x <- x + geom_errorbar(data=ErrData,mapping=aes_string(x=xCol,ymin=xyMin,ymax=paste0(yCol,'+',ErrBarType),group=Groups,color=errbCol),
                                 width=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
          
        } else if (ErrBarGeom=='ribbon') {
          x <- x + geom_ribbon(data=ErrData,mapping=aes_string(x=xCol,ymin=xyMin,ymax=paste0(yCol,'+',ErrBarType),group=Groups),
                               linetype=ErrBarLType,na.rm=T,position=ErrBarPos,fill='grey80',lwd=ErrBarLwd,inherit.aes=F,show.legend=F)
        }
        
      } else {
        if (ErrBarGeom=='errbar') {
          x <- x + geom_errorbarh(data=ErrData,mapping=aes_string(y=yCol,xmin=xyMin,xmax=paste0(xCol,'+',ErrBarType),group=Groups,color=errbCol),
                                  width=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
          
        } else if (ErrBarGeom=='ribbon') {
          x <- x + geom_ribbonh(data=ErrData,mapping=aes_string(y=yCol,xmin=xyMin,xmax=paste0(xCol,'+',ErrBarType),group=Groups),
                                linetype=ErrBarLType,na.rm=T,position=ErrBarPos,fill='grey80',lwd=ErrBarLwd,inherit.aes=F,show.legend=F)
        }
      }
      
    } else if (!is.null(eCol)) {
      # When eCol is given
      
      if (!ErrBarHoriz) {
        rangeCol <- yCol
        
      } else {
        rangeCol <- xCol
      }
      
      if (is.null(rangeCol)) {
        Temp1 <- range(DataO[,get(eCol[1])])
        Temp2 <- range(DataO[,get(eCol[2])])
        
      } else {
        Temp1 <- range(DataO[,get(rangeCol)-get(eCol[1])])
        Temp2 <- range(DataO[,get(rangeCol)+get(eCol[2])])
      }
      
      if (is.null(yRange1)) {
        yAxisRange <- range(c(Temp1,Temp2))
        yAxisRange <- yAxisRange + diff(yAxisRange)*c(-1,1)*xyOffset[3:4]
      }
      
      if (Type=='bar' && !is.null(BarMinY) && sum(sign(yAxisRange))!=0 && !ErrBarHoriz) {
        yAxisRange[which.min(abs(yAxisRange))] <- BarMinY - xyOffset[3]*diff(yAxisRange)
      }
      
      if (!ErrBarHoriz) {
        if (ErrBarGeom=='errbar') {
          if (!is.null(yCol)) {
            x <- x + geom_errorbar(data=DataO[!(is.na(get(yCol)) | is.na(get(eCol[1])) | is.na(get(eCol[2])))],
                                   mapping=aes_string(x=xCol,ymin=paste0(yCol,'-',eCol[1]),ymax=paste0(yCol,'+',eCol[2]),group=Groups,color=errbCol),
                                   width=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
          } else {
            x <- x + geom_errorbar(data=DataO[!(is.na(get(eCol[1])) | is.na(get(eCol[2])))],
                                   mapping=aes_string(x=xCol,ymin=eCol[1],ymax=eCol[2],group=Groups,color=errbCol),
                                   width=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
          }
          
        } else if (ErrBarGeom=='ribbon') {
          if (!is.null(yCol)) {
            x <- x + geom_ribbon(data=DataO,mapping=aes_string(x=xCol,ymin=paste0(yCol,'-',eCol[1]),ymax=paste0(yCol,'+',eCol[2]),group=Groups),
                                 linetype=ErrBarLType,na.rm=T,position=ErrBarPos,fill='grey80',lwd=ErrBarLwd,inherit.aes=F,show.legend=F)
          } else {
            x <- x + geom_ribbon(data=DataO,mapping=aes_string(x=xCol,ymin=eCol[1],ymax=eCol[2],group=Groups),
                                 linetype=ErrBarLType,na.rm=T,position=ErrBarPos,fill='grey80',lwd=ErrBarLwd,inherit.aes=F,show.legend=F)
          }
        }
        
      } else {
        if (ErrBarGeom=='errbar') {
          x <- x + geom_errorbarh(data=DataO,mapping=aes_string(y=yCol,xmin=paste0(xCol,'-',eCol[1]),ymax=paste0(yCol,'+',eCol[2]),group=Groups,color=errbCol),
                                  width=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
          
        } else if (ErrBarGeom=='ribbon') {
          x <- x + geom_ribbon(data=DataO,mapping=aes_string(x=yCol,ymin=paste0(xCol,'-',eCol[1]),ymax=paste0(xCol,'+',eCol[2]),group=Groups),
                               linetype=ErrBarLType,na.rm=T,position=ErrBarPos,fill='grey80',lwd=ErrBarLwd,inherit.aes=F,show.legend=F)
        }
      }
      
      if (length(eCol)>2) {
        if (!is.null(xAxisRange)) {
          Temp1 <- range(DataO[,get(xCol)-get(eCol[3])])
          Temp2 <- range(DataO[,get(xCol)+get(eCol[4])])
          xAxisRange <- range(c(Temp1,Temp2))
          if (is.null(is.null(yRange1))) xAxisRange <- xAxisRange + diff(xAxisRange)*c(-1,1)*xyOffset[1:2]
          ErrBarWidth <- diff(yAxisRange)/diff(xAxisRange) * ErrBarWidth
        }
        
        x <- x + geom_errorbarh(data=DataO,mapping=aes_string(y=yCol,xmin=paste0(xCol,'-',eCol[3]),xmax=paste0(xCol,'+',eCol[4]),group=Groups,color=errbCol),
                                height=ErrBarWidth,linetype=ErrBarLType,na.rm=T,position=ErrBarPos,lwd=ErrBarLwd,inherit.aes=F,show.legend=(Type=='errbar'))
      }
    }
    
    xAxisRange <<- xAxisRange
    yAxisRange <<- yAxisRange
    DataS <<- DataS
    return(x)
  }
  
  # Determining legeng title rotation, position and alignment.
  {
    if (is.null(LegTitRot)) {
      LegTitRot <- 0
      if (length(LegPos)==1 && LegPos %in% c('top','bottom')) LegTitRot <- 0
      if (length(LegPos)==1 && LegPos %in% c('right','left')) LegTitRot <- 90
    }
    if (is.null(LegTitPos)) {
      LegTitPos <- 'top'
      if (length(LegPos)==1 && LegPos %in% c('top','bottom')) LegTitPos <- 'top'
      if (length(LegPos)==1 && LegPos %in% c('right','left')) LegTitPos <- 'left'
    }
    if (is.null(LegTxAlign)) {
      LegTxAlign <- ifelse(ContinLeg,0.5,0)
      if (length(LegPos)==1 && LegPos %in% c('top','bottom')) LegTxAlign <- ifelse(ContinLeg,0.5,0)
      if (length(LegPos)==1 && LegPos %in% c('right','left')) LegTxAlign <- ifelse(ContinLeg,0,0)
    }
  }
  
  # Determining facets.
  {
    Facets <- NULL
    for (Fac in rev(hFacet)) if (is.Facet(Fac,Data=DataO,CheckFacets=CheckFacets)) Facets <- c(Facets,Fac)
    for (Fac in rev(vFacet)) if (is.Facet(Fac,Data=DataO,CheckFacets=CheckFacets)) Facets <- c(Facets,Fac)
    hFacet <- hFacet[hFacet%in%Facets]
    vFacet <- vFacet[vFacet%in%Facets]
    if (length(hFacet)==0) hFacet <- '.'
    if (length(vFacet)==0) vFacet <- '.'
    
    if (length(Facets)==1) Factors <- DataO[,Facets,with=F]
    else Factors <- as.list(DataO[,Facets,with=F])
  }
  
  # Prepare DataO and parameters for pair plots.
  if (Type=='pairs') {
    if (is.numeric(DataO[,get(xCol)]) | is.integer(DataO[,get(xCol)])) {
      Facets <- as.character(sort(as.numeric(unique(DataO[,get(xCol)]))))
      DataO <- DataO[order(get(xCol))]
      IsNum <- T
      
    } else {
      Facets <- as.character(sort(unique(DataO[,get(xCol)])))
      IsNum <- F
    }
    
    Grid <- expand.grid(Facets,Facets,stringsAsFactors=F)
    DataO[, (xCol):=as.character(get(xCol))]
    
    TempX <- list()
    for (i in 1:nrow(Grid)) {
      xGrid <- unlist(Grid[i,])
      if (xGrid[1]==xGrid[2]) next()
      Temp0 <- DataO[get(xCol)%in%xGrid]
      Temp0 <- data.table::dcast(Temp0,formula=paste0(paste(setdiff(names(Temp0),c(xCol,yCol)),collapse='+'),'~',xCol),
                                 value.var=yCol,fun.aggregate=mean,na.rm=T)
      if (is.na(Temp0[1,get(xGrid[1])])) Temp <- xGrid[1] else Temp <- xGrid[2]
      Temp0[, (Temp):=c(get(Temp)[-1],NA)]
      Temp0 <- Mapping_Cols(Temp0,data.frame(Old=xGrid,New=c('xCol__0','yCol__0')))
      Temp0$hFacet__0 <- xGrid[1]
      Temp0$vFacet__0 <- xGrid[2]
      TempX[[length(TempX)+1]] <- Temp0
    }
    
    DataO <- rbindlist(TempX,fill=T)
    DataO <- DataO[!is.na(xCol__0) & !is.na(yCol__0)]
    rm(Temp,Temp0,TempX)
    
    if (IsNum) {
      DataO[, hFacet__0:=factor(hFacet__0,levels=Facets)]
      DataO[, vFacet__0:=factor(vFacet__0,levels=Facets)]
    }
    
    xCol   <- 'xCol__0'
    yCol   <- 'yCol__0'
    hFacet <- 'hFacet__0'
    vFacet <- 'vFacet__0'
    yLab   <- xLab
    Rect   <- T
    Type   <- 'point'
    RegOnOrig <- T
    Facets <- c(hFacet,vFacet)
  }
  
  if (Type=='rangebar') {
    Type    <- 'box'
    BoxCoef <- c(0,1)
  }
  
  # Deciding wether any of the axes is date.
  {
    XisDate <- YisDate <- F
    if (any(c('Date','POSIXt') %in% class(DataO[,get(xCol)]))) XisDate <- T
    if (!is.null(yCol) && any(c('Date','POSIXt') %in% class(DataO[,get(yCol)]))) YisDate <- T
  }
  
  # For box plots, some space must be left for missing factors?
  {
    if (Type %in% c('box','violinbox','boxmeanp','boxmeanl') && !is.null(AllCombsRep) && 
        !((Flip==T & FacetScale=='free_y') || (Flip!=T & FacetScale=='free_x'))) AllCombsRep <- NA
    if (YisDate) AllCombsRep <- NULL
    
    Cols <- unique(c(Facets,xCol,gCol,cCol,fCol,shCol,sCol,ltCol))
    if (!is.null(AllCombsRep)) {
      if (grepl('box',Type) && !is.null(AllCombsRep)) AllCombsRep <- 987654321
      Temp <- list()
      DataO <- DataO[!is.na(get(yCol))]
      for (C in Cols) Temp[[C]] <- unique(DataO[,get(C)])
      Temp  <- as.data.table(expand.grid(Temp,KEEP.OUT.ATTRS=T,stringsAsFactors=F))
      DataO <- merge(Temp,DataO,by=Cols,all.x=T,sort=F)
      if (!is.function(AggrFunc)) DataO[is.na(get(yCol)), (yCol):=AllCombsRep]
    }
  }
  
  # Take average of yCol and lbCol, if is.function(AggrFunc)
  {
    if (is.function(AggrFunc)) {
      if (is.null(lbCol)) {
        Cols <- unique(c(Facets,xCol,gCol,cCol,fCol,shCol,sCol,ltCol))
        if (!is.null(fCol) && is.factor(DataO[,get(fCol)])) Temp <- levels(DataO[,get(fCol)])
        DataS <- DataO[, lapply(.SD,AggrFunc,na.rm=T), keyby=Cols, .SDcol=yCol]
        
      } else {
        if (is.numeric(DataO[,get(lbCol)])) {
          Cols <- unique(c(Facets,xCol,gCol,cCol,fCol,shCol,sCol,ltCol))
          DataS <- DataO[, lapply(.SD,AggrFunc,na.rm=T), keyby=Cols, .SDcol=unique(c(yCol,lbCol))]
          
        } else {
          Cols <- unique(c(Facets,xCol,gCol,cCol,fCol,shCol,sCol,ltCol,lbCol))
          DataS <- DataO[, lapply(.SD,AggrFunc,na.rm=T), keyby=Cols, .SDcol=yCol]
        }
      }
      if (!is.null(AllCombsRep)) DataS[is.nan(get(yCol)), (yCol):=AllCombsRep]
      
    } else {
      DataS <- copy(DataO)
    }
  }
  
  # Making sure facets are factors. Adding prefixes and suffixes.
  {
    if (!is.null(hFacetPref) && (length(hFacet)>1 || hFacet!='.')) {
      if (length(hFacetPref)==1 && hFacetPref==T) hFacetPref <- paste0(hFacet,': ')
      if (length(hFacetPref)==1 && length(hFacet)>1) hFacetPref <- rep(hFacetPref, length(hFacet))
      for (i in 1:length(hFacet)) {
        if (!is.na(hFacetPref[i])) {
          Levels <- unique(sort(DataS[,get(hFacet[i])]))
          DataS[,(hFacet[i]):=factor(as.character(paste0(hFacetPref[i],get(hFacet[i]))),levels=paste0(hFacetPref[i],Levels))]
          DataO[,(hFacet[i]):=factor(as.character(paste0(hFacetPref[i],get(hFacet[i]))),levels=paste0(hFacetPref[i],Levels))]
        }
      }
    }
    if (!is.null(hFacetSuff) && (length(hFacet)>1 || hFacet!='.')) {
      if (length(hFacetSuff)==1 && hFacetSuff==T) hFacetSuff <- paste0(' (',hFacet,')')
      if (length(hFacetSuff)==1 && length(hFacet)>1) hFacetSuff <- rep(hFacetSuff, length(hFacet))
      for (i in 1:length(hFacet)) {
        if (!is.na(hFacetSuff[i])) {
          Levels <- unique(sort(DataS[,get(hFacet[i])]))
          DataS[,(hFacet[i]):=factor(as.character(paste0(get(hFacet[i]),hFacetSuff[i])),levels=paste0(Levels,hFacetSuff[i]))]
          DataO[,(hFacet[i]):=factor(as.character(paste0(get(hFacet[i]),hFacetSuff[i])),levels=paste0(Levels,hFacetSuff[i]))]
        }
      }
    }
    
    if (!is.null(vFacetPref) && (length(vFacet)>1 || vFacet!='.')) {
      if (length(vFacetPref)==1 && vFacetPref==T) vFacetPref <- paste0(vFacet,': ')
      if (length(vFacetPref)==1 && length(vFacet)>1) vFacetPref <- rep(vFacetPref, length(vFacet))
      for (i in 1:length(vFacet)) {
        if (!is.na(vFacetPref[i])) {
          Levels <- unique(sort(DataS[,get(vFacet[i])]))
          DataS[,(vFacet[i]):=factor(as.character(paste0(vFacetPref[i],get(vFacet[i]))),levels=paste0(vFacetPref[i],Levels))]
          DataO[,(vFacet[i]):=factor(as.character(paste0(vFacetPref[i],get(vFacet[i]))),levels=paste0(vFacetPref[i],Levels))]
        }
      }
    }
    if (!is.null(vFacetSuff) && (length(vFacet)>1 || vFacet!='.')) {
      if (length(vFacetSuff)==1 && vFacetSuff==T) vFacetSuff <- paste0(' (',vFacet,')')
      if (length(vFacetSuff)==1 && length(vFacet)>1) vFacetSuff <- rep(vFacetSuff, length(vFacet))
      for (i in 1:length(vFacet)) {
        if (!is.na(vFacetSuff[i])) {
          Levels <- unique(sort(DataS[,get(vFacet[i])]))
          DataS[,(vFacet[i]):=factor(as.character(paste0(get(vFacet[i]),vFacetSuff[i])),levels=paste0(Levels,vFacetSuff[i]))]
          DataO[,(vFacet[i]):=factor(as.character(paste0(get(vFacet[i]),vFacetSuff[i])),levels=paste0(Levels,vFacetSuff[i]))]
        }
      }
    }
  }
  
  # Preparing DataS and parameters for various types of point plots.
  if (Type=='point-pval') {
    if (!is.null(cCol)) {
      pCol <- cCol[2]
      cCol <- cCol[1]
      DataS <- DataS[!is.na(DataS[,get(pCol)]) & !is.na(DataS[,get(cCol)])]
      DataS <- DataS[order(abs(DataS[,get(cCol)]),decreasing=F),]
      if (is.null(P2Type)) P2Type <- 'filled'
      
    } else if (!is.null(fCol)) {
      pCol <- fCol[2]
      fCol <- fCol[1]
      DataS <- DataS[order(abs(DataS[,get(fCol)]),decreasing=F),]
      if (is.null(P2Type)) P2Type <- 'hollow'
      
    } else if (!is.null(sCol)) {
      pCol <- sCol[2]
      sCol <- sCol[1]
    }
    
    if (P2Type=='hollow' || P2Type=='marked') {
      if (!is.null(fCol)) {
        pData  <- DataS[get(pCol)<PThr & !is.na(get(fCol))]
        if (!is.null(NaColor)) naData <- DataS[is.na(DataS[,get(pCol)]) | is.na(DataS[,get(fCol)])]
        DataS   <- DataS[!is.na(DataS[,get(pCol)]) & !is.na(DataS[,get(fCol)])]
      } else {
        pData  <- DataS[get(pCol)<PThr & !is.na(get(cCol))]
        if (!is.null(NaColor)) naData <- DataS[is.na(DataS[,get(pCol)]) | is.na(DataS[,get(cCol)])]
        DataS   <- DataS[!is.na(DataS[,get(pCol)]) & !is.na(DataS[,get(cCol)])]
      }
      P2Size <- PSize
      
    } else if (P2Type=='filled') {
      pData  <- DataS[DataS[,get(pCol)]>=PThr]
      P2Size <- 0.6*PSize
      DataS   <- DataS[which(DataS[,get(pCol)]<PThr)]
    }
  }
  
  # Preparing data for hex and bin2d plots.
  {
    ColX <- NULL
    if (((Type%in%c('hex','bin2d')) && (!is.null(fCol) || Func=='prob')) || (Type=='tiles' && is.null(fCol))) {
      Margines <- F
      if (Type=='tiles' && is.null(fCol)) Func <- 'prob'
      xFunc <- Func
      if (Func=='prob') {
        if (Type=='tiles') {
          Temp  <- Add_Key_DT(DataS[,c(Facets,yCol,xCol),with=F],Cols=c(yCol,xCol))
          DataS <- FreqTableXD(Temp,Cols=c(Facets,'Key'),Prob=T,ToDT=T,IncludeZero=F)
          DataS$Freq <- NULL
          DataS <- merge(DataS,Temp,by=c(Facets,'Key'),sort=F)
          DataS <- Mapping_Cols(DataS,data.frame(Old='Prob',New='TileColExtra'))
          rm(Temp)
          
        } else {
          FUNC <- function(x) return(cbind(TileColExtra=MakeUnitSum(x[[1]])*100,x[,c(xCol,yCol),with=F]))
          DataS$ColumnOfUnit <- 1
          DataS <- DataS[, FUNC(.SD), keyby=Facets, .SDcols=c('ColumnOfUnit',xCol,yCol)]
          DataS$ColumnOfUnit <- NULL
        }
        xFunc <- 'sum'
        
      } else {
        DataS$TileColExtra <- DataS[,get(fCol)]
      }
      
      fCol <- ColX <- 'TileColExtra'
      if (!is.null(xRange1)) DataS <- DataS[get(xCol)>=xRange1[1] & get(xCol)<=xRange1[2]]
      if (!is.null(yRange1)) DataS <- DataS[get(yCol)>=yRange1[1] & get(yCol)<=yRange1[2]]
    }
  }
  
  # Preparing data for tiles plots.
  {
    if (Type=='tiles' && !is.null(Func) && Func!='prob' && !is.null(fCol)) {
      DataS <- DataS[, .(TileColExtra=get(Func)(get(fCol))), keyby=c(Facets,xCol,yCol)]
      fCol <- 'TileColExtra'
    }
    if (Type=='tiles' && !is.null(TileLabSize) && is.null(lbCol)) {
      lbCol <- 'TileColExtraLab'
      DataS[, (lbCol):=get(fCol)]
    }
    if (Type=='tiles' && !is.null(TileLabSize)) DataS[,(lbCol):=format(round(get(lbCol),TileLabDigs), nsmall=TileLabDigs, trim=T, justify='centre')]
  }
  
  # Calculating LegLimits for ContinLeg==T
  {
    if (!is.null(fCol)) ColX <- fCol[1]
    else if (!is.null(cCol)) ColX <- cCol[1]
    
    RT <- RTTrans <- NULL
    if (ContinLeg && !is.null(ColX)) {
      if (is.null(LegNumDigs)) {
        if (is.null(LegLimits))
          RT <- RTTrans <- ifelse(is.numeric(RoundFirst),RoundFirst,Round_Bin_Digits(DataS[,get(ColX)],MidPoint=FMidPoint,IsInt=Is_Int(DataS[,get(ColX)]),CalcRange=T))
        else
          RT <- RTTrans <- ifelse(is.numeric(RoundFirst),RoundFirst,Round_Bin_Digits(LegLimits,MidPoint=FMidPoint,IsInt=Is_Int(DataS[,get(ColX)]),CalcRange=F))
        
      } else {
        RT <- RTTrans <- LegNumDigs
      }
      
      if ((is.numeric(RoundFirst) || RoundFirst==T) && (!is.Date(DataS[,get(ColX)]))) {
        DataS[, (ColX):=round(get(ColX),RT+2)]
        if (!is.null(LegLimits)) LegLimits <- c(MyRound(LegLimits[1],RT,F),MyRound(LegLimits[2],RT,T))
      }
    }
  }
  
  # Deciding whether FMidPoint is needed.
  if (CheckFMidPoint && !is.null(FMidPoint)) {
    if (!is.null(ColX) && !In_Interval(FMidPoint,range(DataS[,get(ColX)],na.rm=T))) FMidPoint <- NULL
  }
  
  # Determining text size for various elements.
  {
    if (is.null(TitleSizes)) TitleSizes <- list(Title=10,SubTitle=9,Caption=7,Tag=5)
    if (length(TitleSizes)==1) TitleSizes <- list(Title=TitleSizes[[1]],SubTitle=TitleSizes[[1]],Caption=TitleSizes[[1]],Tag=TitleSizes[[1]])
    if (is.null(TitleColors)) TitleColors <- list(Title='black',SubTitle='black',Caption='black',Tag='black')
    if (length(TitleColors)==1) TitleColors <- list(Title=TitleColors[[1]],SubTitle=TitleColors[[1]],Caption=TitleColors[[1]],Tag=TitleColors[[1]])
    if (is.null(LegAlpha)) LegAlpha <- Alpha
    
    if (is.null(xLabels)) xLabels <- xBreaks
    if (is.null(yLabels)) yLabels <- yBreaks
    
    if (is.null(LegKeyText) && !is.null(LegKeySize)) LegKeyText <- LegKeySize * 0.75
    if (is.null(LegKeySize) && !is.null(LegKeyText)) LegKeySize <- LegKeyText / 0.75
    if (is.null(LegTitSize) && !is.null(LegKeySize)) LegTitSize <- LegKeyText*ifelse(ContinLeg,1.5,1.2)
    if (!is.null(LegKeySize) && Type=='bar') LegKeySize <- LegKeySize*0.8
    
    if (is.null(LegKeyTextMarg)) {
      if (ContinLeg==T) {
        if (LegPos[1] %in% c('top','bottom')) 
          LegKeyTextMarg <- margin(t=LegKeyText*0.6, b=LegKeyText*0.6, r=0, l=0, unit='pt')
        else 
          LegKeyTextMarg <- margin(l=LegKeyText*0.6, r=LegKeyText*0.6, t=0, b=0, unit='pt')
        
      } else {
        if (LegDirection=='horizontal')
          LegKeyTextMarg <- margin(t=0, b=0, r=LegKeyText*0.6, l=LegKeyText*0.6, unit='pt')
        else 
          LegKeyTextMarg <- margin(t=LegKeyText*0.6, b=LegKeyText*0.6, r=0, l=0, unit='pt')
      }
    }
    
    if (is.null(LegWidth))  LegWidth  <- ifelse(ContinLeg==T, 0.5, 0.0)
    if (is.null(LegLength)) LegLength <- ifelse(ContinLeg==T, 10., 0.5)
    
    if (!is.null(AxTxSize) && length(AxTxSize)==1) AxTxSize <- rep(AxTxSize,2)
    if (is.null(AxTiSize) && !is.null(AxTxSize))   AxTiSize <- 1.3*mean(AxTxSize)
    if (!is.null(AxTiSize) && length(AxTiSize)==1) AxTiSize <- rep(AxTiSize,2)
    
    if (is.null(RegEqSize) && !is.null(AxTxSize)) RegEqSize <- 0.3*mean(AxTxSize)
    if (is.null(RegEqMarg)) RegEqMarg <- c(30,5)
    if (length(RegEqMarg)==1) RegEqMarg <- c(30,RegEqMarg)
    
    if (is.null(StripSizes) && !is.null(AxTiSize)) StripSizes <- AxTiSize
    if (!is.null(StripSizes) && length(StripSizes)==1) StripSizes <- rep(StripSizes,2)
    if (!is.null(StripMarg) && length(StripMarg)==1) StripMarg <- rep(StripMarg,2)
    
    if (is.null(xLab) && !Flip) {
      AxTiSize[1] <- 0.01
      xLabMarg=margin(t=0,r=0,b=0.01,l=0,unit='cm')
    }
    if (is.null(yLab) && !Flip) {
      AxTiSize[2] <- 0.01
      yLabMarg=margin(t=0,r=0,b=0,l=0.01,unit='cm')
    }
    if (Flip) AxTiSize <- rev(AxTiSize)
  }
  
  # Calculating axes and legend justifications and margins.
  {
    if (is.null(xAxTxJust)) {
      if (xAngle==0) xAxTxJust <- c(0.5, 1)
      else if (xAngle==90) xAxTxJust <- c(1, 0.5)
      else xAxTxJust <- c(0.97, 1)
    }
    if (is.null(yAxTxJust)) {
      if (yAngle==0) yAxTxJust <- c(1, 0.5)
      else if (yAngle==90) yAxTxJust <- c(0.5, 0)
      else yAxTxJust <- c(1, 0) # c(1, 0.5)
    }
    
    t <- AxTxSize[1]*2/100
    if (is.null(xLabMarg) && !is.null(xLab)) {
      if (xAngle==0) xLabMarg <- margin(t=t,b=0.01,unit='cm')
      else if (xAngle==90) xLabMarg <- margin(t=t,b=0.01,unit='cm')
      else xLabMarg <- margin(t=t,b=0.01,unit='cm')
    }
    r <- AxTxSize[2]*3/100
    if (is.null(yLabMarg) && !is.null(yLab)) {
      if (yAngle==0) yLabMarg <- margin(r=r,l=0.01,unit='cm')
      else if (yAngle==90) yLabMarg <- margin(r=r,l=0.01,unit='cm')
      else yLabMarg <- margin(r=0,l=0.01,unit='cm')
    }
    
    if (is.null(LegBoxMarg)) {
      LegBoxMarg <- unit(ifelse(ContinLeg,0.2,0.3),'cm')
    }
    
    if (is.null(LegMarg)) {
      LegMarg <- margin(l=ifelse(LegPos[1]=='right',0.1,0.1),r=ifelse(LegPos[1]=='right',0.1,0.1),
                        b=ifelse(LegNRow>1,0.1,0.1), t=ifelse(LegPos[1]=='bottom',0.1,0),unit='cm')
    }
    
    if (ContinLeg && is.null(LegFrameWidth)) LegFrameWidth <- if (is.null(PanLw)) 0.2 else PanLw*1.2
    
    if (is.null(LegJust)) {
      if (LegPos[1]=='top') LegJust <- c('center','bottom')
      if (LegPos[1]=='right') LegJust <- c('center','center')
      if (is.numeric(LegPos[1])) LegJust <- c('left','center')
    }
    if (is.null(LegBoxJust)) {
      if (LegPos[1]=='top') LegBoxJust <- 'center'
      if (LegPos[1]=='right') LegBoxJust <- 'left'
    }
  }
  
  # Initilising boxplot parameters.
  {
    if (is.null(BoxWidth) & !is.null(BoxDodge)) BoxWidth <- BoxDodge
    if (is.null(BoxDodge) & !is.null(BoxWidth)) BoxDodge <- BoxWidth
    if (is.null(BarDodge) & !is.null(BarWidth)) BarDodge <- BarWidth
  }
  
  # Determining the value of Preserve for box and bar plots.
  {
    if (Type %in% c('boxmeanp','boxmeanl','violsd','violinbox')) Preserve <- 'total'
    if (BarLabSize>0) Preserve <- 'total'
    if (BarPos=='dodge') BarPos <- position_dodge(width=BarDodge,preserve=Preserve)
    BoxPos <- position_dodge(BoxDodge,preserve=Preserve)
  }
  
  if (is.null(RegEqPos)) RegEqPos <- 'bottomright'
  
  # Only for graphs with two y-axes
  {
    if (length(yCol)==2) {
      yCol2 <- yCol[2]
      yCol  <- yCol[1]
    }
    if (length(yLab)==2) {
      yLab2 <- yLab[2]
      yLab  <- yLab[1]
    }
  }
  
  # Divide variables by xConv, yConv, cConv and fConv
  {
    Rows <- if (!is.null(AllCombsRep)) DataO[get(yCol)!=AllCombsRep,which=T] else 1:nrow(DataO)
    if (!is.null(xCol) && xCol%in%names(DataO) && is.numeric(DataO[,get(xCol)])) {DataO[,(xCol):=get(xCol)/xConv]}
    if (!is.null(yCol) && yCol%in%names(DataO) && is.numeric(DataO[,get(yCol)])) {DataO[Rows,(yCol):=get(yCol)/yConv]}
    if (!is.null(cCol) && cCol[1]%in%names(DataO) && is.numeric(DataO[,get(cCol[1])])) {DataO[,(cCol[1]):=get(cCol[1])/cConv]}
    if (!is.null(fCol) && fCol[1]%in%names(DataO) && is.numeric(DataO[,get(fCol[1])])) {DataO[,(fCol[1]):=get(fCol[1])/fConv]}
    if (!is.null(eCol) && eCol[1]%in%names(DataO) && is.numeric(DataO[,get(eCol[1])])) {DataO[,(eCol[1]):=get(eCol[1])/yConv]}
    if (!is.null(eCol) && length(eCol)>1 && eCol[2]%in%names(DataO) && is.numeric(DataO[,get(eCol[2])])) {DataO[,(eCol[2]):=get(eCol[2])/yConv]}
    
    Rows <- if (!is.null(AllCombsRep)) DataS[get(yCol)!=AllCombsRep,which=T] else 1:nrow(DataS)
    if (!is.null(xCol) && xCol%in%names(DataS) && is.numeric(DataS[,get(xCol)])) {DataS[,(xCol):=get(xCol)/xConv]}
    if (!is.null(yCol) && yCol%in%names(DataS) && is.numeric(DataS[,get(yCol)])) {DataS[Rows,(yCol):=get(yCol)/yConv]}
    if (!is.null(cCol) && cCol[1]%in%names(DataS) && is.numeric(DataS[,get(cCol[1])])) {DataS[,(cCol[1]):=get(cCol[1])/cConv]}
    if (!is.null(fCol) && fCol[1]%in%names(DataS) && is.numeric(DataS[,get(fCol[1])])) {DataS[,(fCol[1]):=get(fCol[1])/fConv]}
    if (!is.null(eCol) && eCol[1]%in%names(DataS) && is.numeric(DataS[,get(eCol[1])])) {DataS[,(eCol[1]):=get(eCol[1])/yConv]}
    if (!is.null(eCol) && length(eCol)>1 && eCol[2]%in%names(DataS) && is.numeric(DataS[,get(eCol[2])])) {DataS[,(eCol[2]):=get(eCol[2])/yConv]}
  } 
  
  # Transforming data.
  {
    if (!is.null(ContinLegTrans) && ContinLeg) {
      Trans   <- T
      RTTrans <- 10
      FuncTR1 <- ContinLegTrans[[1]]
      FuncTR2 <- ContinLegTrans[[2]]
      
      if (!is.null(LegLimits)) LegLimits <- FuncTR1(LegLimits)
      if (!is.null(LegBreaks)) LegBreaks <- FuncTR1(LegBreaks)
      
      if (!is.null(cCol)) DataS[,(cCol[1]):=FuncTR1(get(cCol[1]))]
      if (!is.null(fCol)) DataS[,(fCol[1]):=FuncTR1(get(fCol[1]))]
      
    } else {
      Trans   <- F
      FuncTR1 <- FuncTR2 <- function(x) return(x)
    }
    
    if (is.null(ContLegTransform)) ContLegTransform <- 'identity'
    if (!is.null(fCol) && fCol[1] %in% names(DataO) && is.Date(DataO[,get(fCol[1])])) ContLegTransform <- 'date'
    if (!is.null(cCol) && cCol[1] %in% names(DataO) && is.Date(DataO[,get(cCol[1])])) ContLegTransform <- 'date'
  }
  
  # Calculating axes' ranges.
  {
    xAxisRange <- yAxisRange <- NULL
    if (is.null(Map)) {
      
      if (is.null(xRange1) && !is.null(xCol) && (is.numeric(DataS[,get(xCol)]) || is.Date(DataS[,get(xCol)]))) {
        xRange <- range(DataS[,get(xCol)],na.rm=T)
      } else {xRange <- xRange1}
      
      if (is.numeric(DataS[,get(xCol)]) || is.Date(DataS[,get(xCol)])) {
        xRange <- as.numeric(xRange)
        xAxisRange <- xRange + diff(xRange)*c(-1,1)*xyOffset[1:2]
        if (is.Date(DataS[,get(xCol)])) xAxisRange <- as.Date(xAxisRange,origin='1970-01-01')
      }
      
      if (!is.null(yCol)) {
        if (is.null(yRange1) && !is.null(yCol) && (is.numeric(DataS[,get(yCol)]) || is.Date(DataS[,get(yCol)]))) {
          if (Type=='stbars') {
            if (length(c(Facets,xCol,fCol))>1) Temp <- aggregate(list(X=DataS[,get(yCol)]),DataS[,c(Facets,xCol,fCol),with=F],sum,na.rm=T)
            else Temp <- aggregate(list(X=DataS[,get(yCol)]),list(DataS[,c(Facets,xCol,fCol),with=F]),sum,na.rm=T)
            Temp <- data.table(Temp)[, sum(X,na.rm=T), keyby=c(Facets,xCol)]
            yRange <- c(0,max(Temp$V1,na.rm=T))
            
          } else {
            yRange <- range(DataS[,get(yCol)],na.rm=T)
          }
          
          if (!is.null(BarMinY) && Type=='bar' && all(yRange>=0)) yRange[which.min(abs(yRange))] <- BarMinY
          
        } else {
          yRange <- yRange1
        }
        
        if (is.numeric(DataS[,get(yCol)]) || is.Date(DataS[,get(yCol)])) yAxisRange <- yRange + diff(yRange)*c(-1,1)*xyOffset[3:4]
        if (is.Date(DataS[,get(yCol)])) yAxisRange <- as.Date(yAxisRange,origin='1970-01-01')
      } 
      
    } else {
      if (!is.null(xRange1)) xAxisRange <- xRange1 + diff(xRange1)*c(-1,1)*xyOffset[1:2]
      if (!is.null(yRange1)) yAxisRange <- yRange1 + diff(yRange1)*c(-1,1)*xyOffset[3:4]
    } 
  }
  
  # Preparing data labels.
  if (BarLabSize>0) {
    if (is.character(DataS[,get(yCol)]) || is.factor(DataS[,get(yCol)])) {
      DataS$BarLabJustX <- DataS$BarLabJustY <- 0.5
      DataS$BarLabPos   <- DataS[,get(yCol)]
      
    } else {
      #if (is.null(BarLabJust)) BarLabJust <- c(0.5,0.5)
      if (length(BarLabJust)==1) BarLabJust<- rep(BarLabJust,2)
      
      if (!is.null(BarLabJust)) {
        if (is.character(BarLabJust) && BarLabJust[1]%in%names(DataS)) {
          DataS <- Mapping_Cols(DataS,data.frame(X=BarLabJust,Y=c('BarLabJustX','BarLabJustY')))
          
        } else {
          if (is.vector(BarLabJust)) BarLabJust <- matrix(BarLabJust,nrow=1)
          BarLabJust <- as.matrix(BarLabJust)
          DataS[,`:=`(BarLabJustX=BarLabJust[,1],BarLabJustY=BarLabJust[,2])]
        }
        
      } else {
        if (BarLabType=='above') {
          BarLabJust <- c(ifelse(BarLabAngle==0,0.5,0),ifelse(BarLabAngle==0,0,0.5))
          DataS$BarLabJustX <- BarLabJust[1]
          DataS$BarLabJustY <- BarLabJust[2]
        }
        if (BarLabType=='bellow') {
          BarLabJust <- c(ifelse(BarLabAngle==0,0.5,1),ifelse(BarLabAngle==0,1,0.5))
          DataS$BarLabJustX <- BarLabJust[1]
          DataS$BarLabJustY <- BarLabJust[2]
        }
        if (BarLabType=='mirror') {
          BarLabJust <- c(ifelse(BarLabAngle==0,0.5,0),ifelse(BarLabAngle==0,0,0.5))
          DataS$BarLabJustX <- BarLabJust[1]
          DataS$BarLabJustY <- BarLabJust[2]
          DataS$BarLabJustY[DataS[,get(yCol)]<0] <- 1
        }
        if (BarLabType=='aroundzero') {
          BarLabJust <- c(ifelse(BarLabAngle==0,0.5,1),ifelse(BarLabAngle==0,1,0.5))
          DataS$BarLabJustX <- BarLabJust[1]
          DataS$BarLabJustY <- BarLabJust[2]
          DataS$BarLabJustY[DataS[,get(yCol)]<0] <- 0
        }
      }
      
      if (!is.null(BarLabPos)) {
        
        if (length(BarLabPos)==1 && is.character(BarLabPos)) {
          if (BarLabPos!='BarLabPos') DataS <- Mapping_Cols(DataS,data.frame(X=BarLabPos,Y='BarLabPos'))
          
        } else {
          DataS[, BarLabPos:=BarLabPos]
        }
        
      } else {
        
        if (FacetScale=='fixed' || FacetScale=='free_x' || (is.null(NRow) && !is.Facet(vFacet))) {
          if (!is.character(DataO[,get(yCol)]) && !is.factor(DataO[,get(yCol)])) {
            if (is.null(yRange1)) 
              LabGaps <- rep(BarLabGap*diff(quantile(DataS[,get(yCol)],c(0.05,0.95),na.rm=T)), nrow(DataS))
            else 
              LabGaps <- rep(BarLabGap*diff(yRange1), nrow(DataS))
            
          } else {
            LabGaps <- rep(0,nrow(DataS))
          }
          
        } else if (!is.null(NRow)) {
          if (Type%in%'bar' && !is.null(BarMinY))
            LabGaps <- DataS[, .(LabGaps=BarLabGap*max(get(yCol))-BarMinY), keyby=hFacet]
          else
            LabGaps <- DataS[, .(LabGaps=BarLabGap*diff(quantile(get(yCol),c(0.05,0.95),na.rm=T))), keyby=hFacet]
          
          DataS <- merge(DataS,LabGaps,by=hFacet,sort=F)
          LabGaps <- DataS$LabGaps
          
        } else {
          if (Type%in%'bar' && !is.null(BarMinY))
            LabGaps <- DataS[, .(LabGaps=BarLabGap*max(get(yCol))-BarMinY), keyby=vFacet]
          else
            LabGaps <- DataS[, .(LabGaps=BarLabGap*diff(quantile(get(yCol),c(0.05,0.95),na.rm=T))), keyby=intersect(Facets,vFacet)]
          
          DataS <- merge(DataS,LabGaps,by=intersect(Facets,vFacet),sort=F)
          LabGaps <- DataS$LabGaps
        }
        
        DataS$BarLabPos <- DataS[,get(yCol)]
        
        IndP <- DataS[!is.na(get(yCol)) & get(yCol)>=0, which=T]
        IndN <- DataS[!is.na(get(yCol)) & get(yCol)< 0, which=T]
        
        if (is.null(xAxisRange) && is.numeric(DataS[,get(xCol)])) xAxisRange <- DataS[,range(get(xCol),na.rm=T)] + diff(DataS[,range(get(xCol),na.rm=T)])*c(-1,1)*xyOffset[1:2]
        if (is.null(yAxisRange)) yAxisRange <- DataS[,range(get(yCol),na.rm=T)] + diff(DataS[,range(get(yCol),na.rm=T)])*c(-1,1)*xyOffset[3:4]
        
        if (BarLabType=='above') {
          DataS[, BarLabPos:=get(yCol)+LabGaps]
          if (is.null(yRange1)) yAxisRange[2] <- max(yAxisRange[2],max(DataS$BarLabPos,na.rm=T) + any(DataS$BarLabPos>0)*1.5*xyOffset[4]*diff(yAxisRange))
        }
        if (BarLabType=='bellow') {
          DataS[, BarLabPos:=get(yCol)-LabGaps]
          if (is.null(yRange1)) yAxisRange[1] <- min(yAxisRange[1],min(DataS$BarLabPos,na.rm=T) - any(DataS$BarLabPos<0)*1.5*xyOffset[3]*diff(yAxisRange))
        }
        if (BarLabType=='mirror') {
          DataS$BarLabPos[IndP] <- DataS$BarLabPos[IndP] + LabGaps[IndP]
          DataS$BarLabPos[IndN] <- DataS$BarLabPos[IndN] - LabGaps[IndN]
          if (is.null(yRange1)) yAxisRange[1] <- min(yAxisRange[1],min(DataS$BarLabPos,na.rm=T) - any(DataS$BarLabPos<0)*1.5*xyOffset[3]*diff(yAxisRange),na.rm=T)
          if (is.null(yRange1)) yAxisRange[2] <- max(yAxisRange[2],max(DataS$BarLabPos,na.rm=T) + any(DataS$BarLabPos>0)*1.5*xyOffset[4]*diff(yAxisRange),na.rm=T)
        }
        if (BarLabType=='aroundzero') {
          DataS$BarLabPos[IndP] <- -LabGaps[IndP]
          DataS$BarLabPos[IndN] <- +LabGaps[IndN]
        }
      } 
    }
  }
  
  # Calculating the parameter 'scale' for the second y-axis,
  if (Type=='barline2ax') {
    yScale <- max(DataS[,get(yCol2)])/max(DataS[,get(yCol)])
    DataS[, (yCol2):=ChangeMinMax(get(yCol2),min(get(yCol)),max(get(yCol)))]
  }
  
  # Reversing factor levels for groups if RevGroups==T
  if (RevGroups==T) {
    if (!is.null(gCol)) {
      for (i in 1:length(gCol)) 
        DataS[,(gCol[i]):=factor(get(gCol[i]),levels=rev(levels(as.factor(get(gCol[i])))))]
    }
    
    if (!is.null(CColors)) CColors <- rev(CColors)
    if (!is.null(FColors)) FColors <- rev(FColors)
  }
  
  # Determine groups when there is interactions.
  {
    Groups <- gCols <- gCol
    if (length(gCol)==1) {
    } else if (length(gCol)>1) {
      Groups <- paste0('interaction(',paste(gCol,collapse=','),')')
    } else {
      Groups <- NULL
    }
  }
  
  if (AddJitter) PointPos <- 'jitter'
  else if (!is.null(PointDodge)) PointPos <- position_dodge(width=PointDodge,preserve='total')
  else PointPos <-  'identity'
  
  if (!is.null(PointDodge)) RegLinePos <- position_dodge(width=PointDodge,preserve='total')
  else RegLinePos <-  'identity'
  
  # ------------------------------------------------
  # ------------------------------------------------
  # Start ------------------------------------------
  x <- ggplot(data=DataS,mapping=aes_string(x=xCol,y=yCol,fill=fCol,color=cCol,group=Groups,shape=shCol,size=sCol,linetype=ltCol),alpha=Alpha)
  # Start ------------------------------------------
  # ------------------------------------------------
  # ------------------------------------------------
  
  # Adding horizontal/vertical lines in the background.
  if (HVLineBack) x <- HVLFunc(x)
  
  # Initilising theme and add 'background' layers.
  {
    if (!is.null(Map)) {
      x <- Add_gg_Layers(Base=x,Layers=Map)
      RemGrids <- T
    }
    
    if (!is.null(Theme)) x <- x + Theme
    if (!is.null(AddLayersBack)) x <- Add_gg_Layers(Base=x,Layers=AddLayersBack)
  }
  
  # Adding error bars behind everything.
  {
    if (is.null(ErrBarLType)) ErrBarLType <- LType
    if (!is.null(Lwd) && is.null(ErrBarLwd)) ErrBarLwd <- max(0.1,0.6*Lwd)
    
    if (Type %in% c('errbar','point','line','linep')) {
      ErrBarPos <- position_dodge(PointDodge)
      if (is.null(PointDodge)) ErrBarPos <- 'identity'
      
    } else if (Type=='bar') {
      ErrBarPos <- position_dodge(BarDodge,preserve=Preserve)
      
    } else {
      ErrBarPos <- position_dodge(0)
    }
    
    if (!ErrBarInFront || Type=='errbar') x <- ErrBarFunc(x)
  }
  
  # Adding a diagonal line (by default, a 45-degree line) 
  if (Add45L!=FALSE) x <- x + geom_abline(slope=Add45L,intercept=0,color='black',size=RegLwd/2,linetype=3)
  
  if (Type=='empty') {
    x <- x + geom_blank()
  }
  
  if (Type=='ecdf') {
    if (!is.null(ltCol)) {
      if (is.null(sCol)) {
        x <- x + stat_ecdf(geom='line',pad=FALSE,na.rm=T,lwd=Lwd)
      } else {
        x <- x + stat_ecdf(aes_string(size=sCol),geom='line',pad=FALSE,na.rm=T)
      }
      if (!is.null(LTypes)) x <- x + scale_linetype_manual(values=LTypes,drop=LegDrop)
      
    } else {
      if (is.null(sCol)) {
        x <- x + stat_ecdf(geom='line',pad=FALSE,na.rm=T,lwd=Lwd,linetype=LType)
      } else {
        x <- x + stat_ecdf(aes_string(size=sCol),geom='line',pad=FALSE,na.rm=T,linetype=LType)
      }
    }
  }
  
  if (RegConf=='d' || Type%in%c('ribbon','ribbonl','ribbonp')) {
    FUNC <- function(x){
      y <- quantile(x,c(BoxCoef[1],0.5,BoxCoef[2]),na.rm=T)
      y <- as.data.frame(matrix(y,ncol=3))
      colnames(y)<-c('ymin','y','ymax')
      return(y)
    }
    x <- x <- x + stat_summary(mapping=aes(group=xCol),geom="ribbon",fun.data=FUNC,fill=FColors,na.rm=T)
    
    if (Type=='ribbonp') {
      x <- x + stat_summary(mapping=aes(group=xCol),geom='point',fun.y=mean,shape=PShape,color=CColors,fill=FColors,size=PSize,na.rm=T)
      
    } else if (Type=='ribbonl') {
      x <- x + stat_summary(mapping=aes(group=xCol),geom='line',fun.y=mean,linetype='dashed',na.rm=T)
    }
  }
  
  if (Type %in% c('box','boxp','boxmeanl','boxmeanp','boxmeanpl','violinbox')) {
    if (is.null(BoxCoef)) BoxCoef <- c(0.25,0.75)
    
    FUNC <- function(xx) {
      yy <- quantile(xx,probs=c(BoxCoef[1],0.25,0.5,0.75,BoxCoef[2]),na.m=T)
      names(yy) <- c("ymin", "lower", "middle", "upper", "ymax")
      return(yy)
    }
    FUNC1 <- function(y) {
      xDate <- F
      if (YisDate) {
        y <- Date2DOY(y,Year=2000)
        xDate <- T
      }
      
      y <- quantile(y,probs=BoxCoef,na.rm=T)
      
      if (YisDate) {
        y <- DOY2Date(y,2000)
      }
      y <- data.frame(Low=y[1],High=y[2])
      return(y)
    }
    
    if (RemOutliers || length(BoxCoef)==2) {
      RemOutliers <- T
      Rows <- if (!is.null(AllCombsRep)) DataS[get(yCol)!=AllCombsRep,which=T] else 1:nrow(DataS)
      if (is.null(gCols)) gCols <- xCol
      if (is.null(yRange1) & !YisDate) yRange <- CalcBoxRang_without_NA(DataS[Rows],xCol=gCols,yCol,Facets,Margines,pLow=0.25,pHigh=0.75,Coef=BoxCoef)
      yAxisRange <- CalcAxisRange(yRange,0.005)
    }
    
    if (is.null(BoxCoef) || length(BoxCoef)==2) {
      if (!is.character(Margines) && !Margines) {
        Temp <- copy(DataS)
        xCols <- unique(c(xCol,ifelse(Type=='violinbox',cCol,fCol),gCol,Facets))
        Temp <- Temp[, FUNC1(.SD), keyby=xCols, .SDcols=yCol]
        
        if (is.null(yRange1)) {
          if (!is.null(BoxCoef)) {
            if (!is.null(AllCombsRep)) 
              xTemp <- Temp[Low!=AllCombsRep & High!=AllCombsRep]
            else
              xTemp <- Temp
            
            xTemp <- as.numeric(c(min(xTemp$Low,na.rm=T),max(xTemp$High,na.rm=T)))
            yAxisRange <- xTemp + diff(xTemp)*c(-1,1)*xyOffset[3:4]
            if (is.Date(DataS[,get(yCol)])) yAxisRange <- as.Date(yAxisRange,origin='1970-01-01')
          }
        }
        
        if (is.null(AllCombsRep)) Temp <- Temp[!is.na(Low) & !is.na(High)]
        
        if (Type=='violinbox') {
          Groups <- unique(c(xCol,cCol,gCol))
          if (length(Groups)>1) Groups <- paste0('interaction(',paste(unique(c(xCol,cCol,gCol)),collapse=','),')')
          
        } else {
          Groups <- unique(c(xCol,fCol,gCol))
          if (length(Groups)>1) Groups <- paste0('interaction(',paste(unique(c(xCol,fCol,gCol)),collapse=','),')')
        }
        
        if (Type=='violinbox') {
          if (!is.null(BoxCoef)) {
            x <- x + geom_errorbar(data=Temp,aes_string(x=xCol,group=Groups,ymin='Low',ymax='High'),na.rm=T,color='black',width=0.1*BoxWidth,position=BoxPos,lwd=Lwd*0.35/2,linetype=LType,inherit.aes=F)
          }
          x <- x + geom_boxplot(mapping=aes_string(group=Groups),outlier.size=NA,outlier.color=NA,position=BoxPos,width=0.25*BoxWidth,coef=0,lwd=Lwd*0.35,alpha=Alpha,color='black',show.legend=F)
          x <- x + geom_violin(fill='transparent',trim=T,position=BoxPos,width=BoxWidth,lwd=Lwd,na.rm=T,show.legend=T)
          
        } else if (is.null(cCol)) {
          if (is.null(CColors)) CColors <- 'black'
          
          if (!is.null(BoxCoef)) {
            x <- x + geom_errorbar(data=Temp,aes_string(x=xCol,group=Groups,ymin='Low',ymax='High'),na.rm=T,color=CColors,width=0.5*BoxWidth,position=BoxPos,lwd=Lwd/2,linetype=LType,inherit.aes=F)
          }
          x <- x + geom_boxplot(outlier.size=NA,outlier.color=NA,position=BoxPos,width=BoxWidth,coef=0,lwd=Lwd,alpha=Alpha,color=CColors)
          
        } else {
          if (!is.null(BoxCoef)) {
            x <- x + geom_errorbar(data=Temp,aes_string(x=xCol,group=Groups,ymin='Low',ymax='High'),na.rm=T,width=0.5*BoxWidth,position=BoxPos,lwd=Lwd/2,linetype=LType,inherit.aes=F)
          }
          x <- x + geom_boxplot(outlier.size=NA,outlier.color=NA,position=BoxPos,width=BoxWidth,coef=0,lwd=Lwd,alpha=Alpha)
        }
        
      } else {
        if (RemOutliers) {
          x <- x + stat_summary(fun.data=FUNC,geom='boxplot',na.rm=T,outlier.size=NA,position=BoxPos,lwd=Lwd,alpha=Alpha)
        } else {
          x <- x + stat_summary(fun.data=FUNC,geom='boxplot',na.rm=T,outlier.size=OutlierSize,outlier.color='grey60',position=BoxPos,lwd=Lwd,alpha=Alpha)
        }
      }
      
    } else if (length(BoxCoef)==1) {
      if (RemOutliers) {
        x <- x + geom_boxplot(na.rm=T,outlier.size=NA,outlier.color=NA,position=BoxPos,width=BoxWidth,coef=BoxCoef,lwd=Lwd)
      } else {
        x <- x + geom_boxplot(na.rm=T,outlier.size=OutlierSize,outlier.color='grey60',position=BoxPos,width=BoxWidth,coef=BoxCoef,lwd=Lwd)
      }
    }
  }
  
  if (Type %in% c('boxmeanp','boxmeanpl')) {
    if (is.null(CColors)) CColors <- 'black'
    x <- x + stat_summary(mapping=aes_string(x=xCol,y=yCol,group=gCol[2]),position=position_dodge(width=BoxDodge,preserve='total'),
                          fun='mean',geom='point',na.rm=T,show.legend=F,shape=PShape,color=CColors,size=PSize,stroke=Stroke,fun.args=list(na.rm=T)) +
      scale_color_manual(values=FColors,drop=LegDrop,guide=FALSE)
  }
  
  if (Type %in% c('boxmeanl','boxmeanpl')) {
    if (is.null(CColors)) CColors <- 'black'
    x <- x + stat_summary(mapping=aes_string(x=xCol,y=yCol,group=gCol[2],color=gCol[2]),lty=ifelse(is.null(LType),5,LType),lwd=Lwd,
                          fun='mean',geom='line',position=position_dodge(BoxWidth,preserve='total'),na.rm=T,show.legend=F,fun.args=list(na.rm=T)) +
      scale_color_manual(values=FColors,drop=LegDrop,guide=FALSE)
  }
  
  if (Type=='boxp' || Type=='barp') {
    x <- x + geom_point(na.rm=TRUE,size=PSize,stroke=Stroke)
  }
  
  
  if (Type %in% c('viol','violsd')) {
    x <- x + geom_violin(trim=FALSE,position=BoxPos,width=BoxWidth,lwd=Lwd,na.rm=TRUE)
    
    if (Type=='violsd') {
      if (is.null(cCol)) {
        x <- x + stat_summary(fun.data="mean_sdl",geom="pointrange",position=BoxPos,size=PSize,na.rm=T,color=CColors,show.legend=F)
      } else {
        x <- x + stat_summary(fun.data="mean_sdl",geom="pointrange",position=BoxPos,lwd=Lwd,na.rm=T,show.legend=F)
      }
    }
  }
  
  if (Type=='stbars') {
    if (is.null(CColors)) CColors <- NA
    x <- x + geom_bar(stat="identity",position="stack",color=CColors,lwd=Lwd,width=BarWidth)
  }
  
  if (Type=='barline1ax') {
    # yCol : bar
    # yCol2: line
    if (is.null(CColors)) CColors <- NA
    x <- x + geom_bar(stat="identity",position=BarPos,color=CColors,lwd=Lwd[1],width=BarWidth)
    x <- x + geom_line(aes_string(y=yCol2),linetype=LType,lwd=Lwd[2])
  }
  
  if (Type=='barline2ax') {
    # yCol : point+line
    # yCol2: bar
    x <- x + geom_bar(stat="identity",position="dodge") +
      # First variable
      geom_line(aes_string(y=yCol),linetype=LType,lwd=Lwd) + 
      geom_point(aes_string(y=yCol),size=PSize) +
      geom_errorbar(aes_string(ymin=yCol-eCol[1],ymax=yCol+eCol[1]),width=0.1) +
      # Second variable
      scale_y_continuous(sec.axis=sec_axis(trans=~.*yScale,name=yLab2)) + 
      geom_bar(aes_string(y=yCol2),stat="identity") + 
      scale_fill_manual(name=NULL,values='gray',drop=LegDrop) + theme(legend.position="none")
  }
  
  if (Type=='tiles') {
    x <- x + geom_raster(na.rm=T,interpolate=Interpolate) + theme(panel.grid=EB)
    if (!is.null(TileLabSize)) x <- x + geom_text(aes_string(label=lbCol),na.rm=T,size=TileLabSize,family=FontFamily,hjust=0.5)
  }
  
  if (Type=='line') {
    Temp <- list()
    if (!is.null(Lwd)) {
      if (is.null(LType) || length(LType)>1) Temp <- list(na.rm=T,size=Lwd,position=PointPos)
      else Temp <- list(na.rm=T,size=Lwd,linetype=LType,position=PointPos,alpha=Alpha)
      
    } else {
      if (is.null(LType) || length(LType)>1) Temp <- list(na.rm=T,position=PointPos)
      else Temp <- list(na.rm=T,linetype=LType,position=PointPos,alpha=Alpha)
    }
    if (is.null(cCol)) Temp[['color']] <- CColors
    x <- x + do.call(geom_line, Temp)
    
    # if (!is.null(Lwd)) {
    #   if (is.null(LType) || length(LType)>1) x <- x + geom_line(na.rm=T,lwd=Lwd,position=PointPos)
    #   else x <- x + geom_line(na.rm=T,lwd=Lwd,linetype=LType,position=PointPos,alpha=Alpha)
    #   
    # } else {
    #   if (is.null(LType) || length(LType)>1) x <- x + geom_line(na.rm=T,position=PointPos)
    #   else x <- x + geom_line(na.rm=T,linetype=LType,position=PointPos,alpha=Alpha)
    # }
  }
  
  if (Type=='area') {
    if (!is.null(Lwd)) {
      if (is.null(LType) || length(LType)>1) x <- x + geom_area(na.rm=T,lwd=Lwd,position=PointPos,color=CColors)
      else x <- x + geom_area(na.rm=T,lwd=Lwd,linetype=LType,position=PointPos,alpha=Alpha,color=CColors)
      
    } else {
      if (is.null(LType) || length(LType)>1) x <- x + geom_area(na.rm=T,position=PointPos,color=CColors)
      else x <- x + geom_area(na.rm=T,linetype=LType,position=PointPos,alpha=Alpha,color=CColors)
    }
  }
  
  if (Type=='linep') {
    if (is.null(LType) || length(LType)>1) {
      x <- x + geom_line(na.rm=T,lwd=Lwd,show.legend=F,position=PointPos)
      #geom_point(na.rm=T,size=PSize,stroke=Stroke,position=PointPos)
      
    } else {
      x <- x + geom_line(na.rm=T,lwd=Lwd,linetype=LType,show.legend=F,position=PointPos)
      #geom_point(na.rm=T,size=PSize,stroke=Stroke,position=PointPos)
    }
  }
  
  if (Type%in%c('line','linep')) {
    if (is.null(LType) || length(LTypes)>1) x <- x + scale_linetype_manual(values=LTypes,guide=F)
  }
  
  if (Type=='hist') {
    if (is.null(CColors)) CColors <- 'black'
    if (is.null(fCol)) {
      x <- x + geom_histogram(mapping=aes_string(x=xCol,group=Groups),binwidth=BinWidth,center=0,na.rm=T,inherit.aes=F,stat='bin',
                              fill=FColors,color=CColors,position='dodge',lwd=Lwd,alpha=Alpha) 
    } else {
      x <- x + geom_histogram(mapping=aes_string(x=xCol,fill=fCol,group=Groups),binwidth=BinWidth,center=0,na.rm=T,inherit.aes=F,stat='bin',
                              color=CColors,position='dodge',lwd=Lwd,alpha=Alpha) 
    }
  }
  
  if (Type%in%c('freqpoly','pfreqpoly')) {
    if (is.null(CColors)) CColors <- 'black'
    if (Type=='freqpoly') xStat <- 'stat(count)'
    if (Type=='pfreqpoly') xStat <- '100 * ..count.. / sum(..count..)'
    
    if (is.null(cCol)) {
      x <- x + geom_freqpoly(mapping=aes_string(x=xCol,y=xStat,group=Groups),binwidth=BinWidth,na.rm=T,inherit.aes=F,
                             color=CColors,position='identity',lwd=Lwd,alpha=Alpha) 
    } else {
      x <- x + geom_freqpoly(mapping=aes_string(x=xCol,y=xStat,color=cCol,group=Groups),binwidth=BinWidth,na.rm=T,inherit.aes=T,
                             position='identity',lwd=Lwd,alpha=Alpha)
    }
  }
  
  if (Type%in%c('density')) {
    if (is.null(CColors)) CColors <- 'black'
    
    if (!is.null(ltCol)) {
      if (is.null(cCol)) {
        x <- x + geom_line(mapping=aes_string(x=xCol,group=Groups,linetype=ltCol),stat='density',bw=BinWidth,na.rm=T,
                           inherit.aes=F,lwd=Lwd,alpha=Alpha,color=CColors)
      } else {
        x <- x + geom_line(mapping=aes_string(x=xCol,color=cCol,group=Groups,linetype=ltCol),stat='density',bw=BinWidth,na.rm=T,
                           inherit.aes=F,lwd=Lwd,alpha=Alpha)
      }
      if (!is.null(LTypes)) x <- x + scale_linetype_manual(values=LTypes,drop=LegDrop)
      
    } else {
      if (is.null(cCol)) {
        x <- x + geom_line(mapping=aes_string(x=xCol,group=Groups),stat='density',bw=BinWidth,na.rm=T,
                           inherit.aes=F,lwd=Lwd,alpha=Alpha,color=CColors)
      } else {
        x <- x + geom_line(mapping=aes_string(x=xCol,color=cCol,group=Groups),stat='density',bw=BinWidth,na.rm=T,
                           inherit.aes=F,lwd=Lwd,alpha=Alpha)
      }
    }
  }
  
  if (Type%in%c('densityarea')) {
    if (is.null(FColors)) FColors <- 'black'
    
    if (is.null(fCol)) {
      x <- x + geom_density(mapping=aes_string(x=xCol,y='..density..',group=Groups),bw=BinWidth,na.rm=T,
                            inherit.aes=F,fill=FColors,position='identity',lwd=Lwd,alpha=Alpha)
    } else {
      x <- x + geom_density(mapping=aes_string(x=xCol,y='..density..',fill=fCol,group=Groups),bw=BinWidth,na.rm=T,
                            inherit.aes=F,lwd=Lwd,alpha=Alpha)
    }
  }
  
  if (Type%in%c('point','point-pval') && is.na(PShape[1]) && ShowReg) RegShowLeg <- T
  
  if (Type%in%c('point','point-pval','linep') && !is.na(PShape[1])) {
    if (is.null(CColors)) CColors <- 'black'
    if (is.null(FColors)) FColors <- 'black'
    
    PFunc <- function(xx,xData=NULL,cCol=NULL,fCol=NULL,sCol=NULL,xCColors=CColors,xFColors=FColors,xPShape=PShape,xPSize=PSize,xStroke=Stroke) {
      if (is.null(cCol)) {
        # Points are to be of shape xPShape
        if (is.null(fCol)) {
          # Points are to be filled using the xFColors
          if (is.null(sCol)) {
            if (is.null(shCol)) {
              xx <- xx + geom_point(data=xData,na.rm=T,color=xCColors,fill=xFColors,size=xPSize,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos)
            } else {
              xx <- xx + geom_point(data=xData,na.rm=T,color=xCColors,fill=xFColors,size=xPSize,alpha=Alpha,stroke=xStroke,position=PointPos)
            }
          } else {
            if (is.null(shCol)) {
              xx <- xx + geom_point(data=xData,na.rm=T,color=xCColors,fill=xFColors,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos)
            } else {
              xx <- xx + geom_point(data=xData,na.rm=T,color=xCColors,fill=xFColors,alpha=Alpha,stroke=xStroke,position=PointPos)
            }
          }
        } else {
          # Points are to be filled according to the fCol
          if (is.null(xCColors)) xCColors <- 'black'
          if (is.null(sCol)) {
            if (is.null(shCol)) {
              xx <- xx + geom_point(data=xData,aes_string(fill=fCol),na.rm=T,color=xCColors,size=xPSize,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos)
            } else {
              xx <- xx + geom_point(data=xData,aes_string(fill=fCol),na.rm=T,color=xCColors,size=xPSize,alpha=Alpha,stroke=xStroke,position=PointPos)
            }
          } else {
            if (is.null(shCol)) {
              xx <- xx + geom_point(data=xData,aes_string(fill=fCol),na.rm=T,color=xCColors,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos)
            } else {
              xx <- xx + geom_point(data=xData,aes_string(fill=fCol),na.rm=T,color=xCColors,alpha=Alpha,stroke=xStroke,position=PointPos)
            }
          }
        }
        
      } else {
        # Points are to be without outline and colored according to the cCol
        if (is.null(sCol)) {
          if (is.null(shCol)) {
            xx <- xx + geom_point(data=xData,aes_string(x=xCol,y=yCol,fill=fCol,color=cCol,group=Groups),na.rm=T,size=xPSize,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos,inherit.aes=F)
          } else {
            xx <- xx + geom_point(data=xData,aes_string(x=xCol,y=yCol,fill=fCol,color=cCol,group=Groups,shape=shCol),na.rm=T,size=xPSize,alpha=Alpha,stroke=xStroke,position=PointPos,inherit.aes=F)
          }
          
        } else {
          if (is.null(shCol)) {
            xx <- xx + geom_point(data=xData,aes_string(x=xCol,y=yCol,fill=fCol,color=cCol,group=Groups,size=sCol),na.rm=T,alpha=Alpha,shape=xPShape,stroke=xStroke,position=PointPos,inherit.aes=F)
          } else {
            xx <- xx + geom_point(data=xData,aes_string(x=xCol,y=yCol,fill=fCol,color=cCol,group=Groups,shape=shCol,size=sCol),na.rm=T,alpha=Alpha,stroke=xStroke,position=PointPos,inherit.aes=F)
          }
        }
      }
      return(xx)
    }
    
    if (Type!='point-pval') {
      x <- PFunc(xx=x,xData=NULL,cCol=cCol,fCol=fCol,sCol=sCol,xCColors=CColors,xFColors=FColors,xPShape=PShape,xPSize=PSize,xStroke=Stroke)
    }
    
    if (Type=='point-pval') {
      if (P2Type=='hollow') {
        # Main points are to be filled according to the fCol. Secondary points just have outlines.
        xPShape <- max(PShape,21)
        x <- PFunc(xx=x,xData=DataS[get(pCol)>=PThr & !is.na(get(fCol))],cCol=NULL,fCol=fCol,sCol=sCol,xCColors='transparent',xFColors=NA,xPShape=xPShape,xPSize=PSize+Stroke,xStroke=0)
        x <- PFunc(xx=x,xData=DataS[get(pCol)<PThr & !is.na(get(fCol))],cCol=NULL,fCol=fCol,sCol=sCol,xCColors=CColors,xFColors=NA,xPShape=xPShape,xPSize=PSize,xStroke=Stroke)
        if (!is.null(NaColor) && nrow(naData)>0) 
          x <- PFunc(xx=x,xData=naData,cCol=NULL,fCol=NULL,sCol=sCol,xCColors=NaColor,xFColors=NA,xPShape=16,xPSize=P2Size,xStroke=0)
        
      } else if (P2Type=='filled') {
        # Main points are to be colored according to the cCol. Secondary points are filled single-coloured points
        x <- PFunc(xx=x,xData=NULL,cCol=cCol,fCol=fCol,sCol=sCol,xCColors=CColors,xFColors=FColors,xPShape=PShape,xPSize=PSize,xStroke=0)
        x <- PFunc(xx=x,xData=pData,cCol=NULL,fCol=NULL,sCol=sCol,xCColors=CColors,xFColors=NA,xPShape=16,xPSize=P2Size,xStroke=0)
        
      } else if (P2Type=='marked') {
        # Main points are to be filled according to the fCol. Significant values are marked with dots.
        cfCol <- if (!is.null(fCol)) fCol else cCol
        if (!is.null(fCol)) xPShape <- max(PShape,21) else xPShape <- PShape
        x <- PFunc(xx=x,xData=DataS[!is.na(get(cfCol))],cCol=cCol,fCol=fCol,sCol=sCol,xCColors=CColors,xFColors=NA,xPShape=xPShape,xPSize=PSize,xStroke=Stroke)
        x <- PFunc(xx=x,xData=DataS[get(pCol)<PThr & !is.na(get(cfCol))],cCol=NULL,fCol=NULL,sCol=sCol,xCColors=CColors,xFColors=NA,xPShape=16,xPSize=MinPSize,xStroke=Stroke)
        if (!is.null(NaColor) && nrow(naData)>0) 
          x <- PFunc(xx=x,xData=naData,cCol=NULL,fCol=NULL,sCol=sCol,xCColors=NaColor,xFColors=NA,xPShape=16,xPSize=P2Size,xStroke=0)
      }
    }
    
    if (BarLabSize>0) {
      if (is.character(DataS[,get(yCol)]) || is.factor(DataS[,get(yCol)])) {
        if (is.null(lbCol)) lbCol <- unique(c(cCol,fCol))
        x <- x + geom_text(data=DataS,aes_string(x=xCol,y=yCol,label=lbCol),hjust='BarLabJustX',vjust='BarLabJustY',
                           size=BarLabSize,angle=BarLabAngle,na.rm=T,family=FontFamily,parse=BarLabParse)
        
      } else {
        if (is.null(lbCol)) lbCol <- yCol
        if (AllCombsLab==F) DataS[get(lbCol)==AllCombsRep, (lbCol):=NA]
        
        if (!is.null(DiffTest)) {
          xLabel <- DiffTestFunc()
          
        } else {
          if (is.numeric(DataS[,get(lbCol)])) 
            xLabel <- sprintf('sprintf("%%0.%sf", round(%s,digits=%s))',BarLabDigs,lbCol,BarLabDigs) 
          else xLabel <- lbCol
        }
        
        xPos <- if (Type %in% c('point','linep')) PointPos else 'identity'
        
        x <- x + geom_text(data=DataS,aes_string(y='BarLabPos',label=xLabel,hjust='BarLabJustX',vjust='BarLabJustY'),color=BarLabCol,
                           size=BarLabSize,angle=BarLabAngle,na.rm=T,family=FontFamily,parse=BarLabParse,position=xPos)
      }
    }
    
    if (ErrBarInFront) x <- ErrBarFunc(x)
  }
  
  if (Type=='hex') {
    if (is.null(fCol) && Func!='prob') {
      x <- ggplot(data=DataS,mapping=aes_string(x=xCol,y=yCol,group=Groups,color=cCol,shape=shCol,size=sCol),alpha=Alpha) + Theme
      x <- x + stat_bin_hex(colour=NA,na.rm=T,bins=NumBins)
      
    } else {
      x <- ggplot(data=DataS,mapping=aes_string(x=xCol,y=yCol,z=fCol,group=Groups,color=cCol,shape=shCol,size=sCol),alpha=Alpha) + Theme
      x <- x + stat_summary_hex(fun=xFunc,colour=NA,na.rm=T,bins=NumBins)
    } 
  } 
  
  if (Type=='bin2d') {
    if (is.null(fCol) && Func!='prob') {
      x <- ggplot(data=DataS,mapping=aes_string(x=xCol,y=yCol,group=Groups,color=cCol,shape=shCol,size=sCol),alpha=Alpha) + Theme
      x <- x + stat_bin_2d(colour=NA,na.rm=T,bins=NumBins)
      
    } else {
      x <- ggplot(data=DataS,mapping=aes_string(x=xCol,y=yCol,z=fCol,group=Groups,color=cCol,shape=shCol,size=sCol),alpha=Alpha) + Theme
      x <- x + stat_summary_2d(fun=xFunc,colour=NA,na.rm=T,bins=NumBins)
    } 
  } 
  
  if (Type=='bar' || Type=='barp') {
    if (is.null(CColors)) CColors <- NA
    if (is.null(FColors)) FColors <- 'gray'
    if (is.null(fCol)) {
      x <- x + geom_bar(stat='identity',position=BarPos,color=CColors,fill=FColors,lwd=Lwd,width=BarWidth,alpha=Alpha,na.rm=T)
    } else {
      x <- x + geom_bar(stat='identity',position=BarPos,color=CColors,lwd=Lwd,width=BarWidth,alpha=Alpha,na.rm=T)
    }
    
    if (ErrBarInFront) x <- ErrBarFunc(x)
  }
  
  if (Type %in% c('bar','barp','stbars')) {
    if (BarLabSize>0) {
      if (is.null(lbCol)) lbCol <- yCol
      if (AllCombsLab==F) DataS[get(lbCol)==AllCombsRep, (lbCol):=NA]
      
      if (!is.null(DiffTest)) {
        xLabel <- DiffTestFunc()
        
      } else {
        if (is.numeric(DataS[,get(lbCol)])) {
          xLabel <- sprintf('sprintf("%%0.%sf", round(%s,digits=%s))',BarLabDigs,lbCol,BarLabDigs) 
        } else {
          xLabel <- lbCol
        }
      }
      
      if (!is.null(BarLabPos) && is.null(gCol)) BarPos <- 'identity'
      
      x <- x + geom_text(data=DataS,mapping=aes_string(x=xCol,y='BarLabPos',label=xLabel,hjust='BarLabJustX',vjust='BarLabJustY'),
                         position=BarPos,size=BarLabSize,angle=BarLabAngle,na.rm=T,family=FontFamily,parse=BarLabParse)
    }
  }
  
  # Adding a shape scale, if needed. 
  if (!is.null(shCol) && length(PShape)>0) {
    x <- x + scale_shape_manual(values=PShape,drop=LegDrop)
  }
  
  # Adding error bars in front.
  #if (ErrBarInFront) x <- ErrBarFunc(x)
  
  # adjusting axes' ranges if the plot is to be Square.
  if (Rect && any(is.finite(c(xAxisRange,yAxisRange)))) {
    xAxisRange <- yAxisRange <- c(min(c(xAxisRange[1],yAxisRange[1]),na.rm=T),max(c(xAxisRange[2],yAxisRange[2]),na.rm=T))
  }
  
  # Regression
  {
    if (length(RegType)==1 && !is.null(RegType) && RegType==T) RegType <- 1
    if (length(RegType)==1 && !is.null(RegType) && RegType<1) RegDegree <- 1
    if (length(RegType)==1 && length(RegDegree)>1 && !RegType%in%c(88,99)) RegType <- 1
    if (length(RegType)!=length(RegDegree) && length(RegType)==1) RegType <- rep(RegType,length(RegDegree))
    
    if (!is.null(RegType)) { #!is.null(RegType) || AddRegEq!=FALSE
      if (!is.null(RegData)) {
        Data2 <- RegData
        IsRegData <- T
        
      } else {
        if (RegOnOrig) 
          Data2 <- DataO
        else 
          Data2 <- DataS
        
        IsRegData <- F
      }
      
      if (!is.null(RegRange)) {
        Data2 <- Data2[data.table::between(get(xCol),RegRange[1],RegRange[2],incbounds=T)]
      }
      
      if (!is.null(AllCombsRep) && !is.na(AllCombsRep)) Data2 <- Data2[get(yCol)!=AllCombsRep]
      
      if (!is.null(RegAggUnit)) {
        RegRange <- c(max(c(RegRange,xRange[1]),na.rm=T),min(c(RegRange[2],xRange[2]),na.rm=T))
        Data2 <- Add_Aggregate_Unit(Data2,Col=rggCol,AggUnit=RegAggUnit,PutLast=T,AsFactor=F)
        RegFullRange <- T
      }
      
      if (is.null(rggCol)) {
        Data2$RegGr1362 <- 'All'
        if (is.null(rgcCol)) 
          Data2$RegGr1362C <- 'All'
        else 
          Data2$RegGr1362C <- Data2[, get(rgcCol)]
        
      } else {
        
        if (length(rggCol)==1) {
          Data2[, RegGr1362:=get(rggCol)]
        } else {
          Data2$RegGr1362 <- apply(Data2[,rggCol,with=F],1,paste,collapse="-")
        }
        if (is.null(rgcCol)) 
          Data2$RegGr1362C <- 'All'
        else
          Data2$RegGr1362C <- Data2[, get(rgcCol)]
      }
      
      if (is.factor(Data2$RegGr1362)) {
        RegGroups  <- levels(Data2$RegGr1362)
        RegGroups  <- RegGroups[RegGroups%in%unique(as.character(Data2$RegGr1362))]
        RegGroupsC <- levels(as.factor(Data2$RegGr1362C))
        RegGroupsC <- RegGroupsC[RegGroupsC%in%unique(as.character(Data2$RegGr1362C))]
        
      } else {
        RegGroups <- sort(unique(as.character(Data2$RegGr1362)))
        RegGroupsC <- sort(unique(as.character(Data2$RegGr1362C)))
      }
    }
    
    FuncSegReg <- function(data,OutType=1,Degree=1) {
      data2 <- cbind.data.frame(x=data[,get(xCol)],y=data[,get(yCol)])
      LmModel <- lm(y~x,data=data2)
      
      if (Degree==1) PSIs <- median(data2$x,na.rm=T)
      if (Degree>1) {
        PSIs <- quantile(data2$x,seq(0,1,length.out=Degree+2),na.rm=T)
        PSIs <- PSIs[-c(1,length(PSIs))]
      }
      
      if (Degree==1 & SafeSegReg) {
        Model <- piecewise.linear(data2$x,data2$y,CI=F)
        RegAIC <- AIC(Model)
        RegBIC <- BIC(Model)
        
        if (OutType==1) {
          # Outputs a data.frame
          data2 <- cbind.data.frame(x=sort(c(range(data2$x,na.rm=T),Model$change.point)))
          Out <- data.frame(x=data2$x,y=predict(Model,data2))
          names(Out) <- c(xCol,yCol)
          
        } else {
          # Outputs a list
          data2 <- data.frame(x=data2$x,y=predict(Model,data2))
          names(data2) <- c(xCol,yCol)
          
          Out <- list()
          Out$Fit <- data2
          Out$Model <- Model
          Out$AIC   <- RegAIC
          Out$BIC   <- RegBIC
          Out$rp    <- sqrt(summary(Model)$r.squared)
          Out$r2    <- summary(Model)$r.squared
          Out$r2adj <- summary(Model)$adj.r.squared
          Out$Break <- Model$change.point
          Out$Coefs <- coef(Model$model)
          names(Out$Coefs)[2:length(Out$Coefs)] <- paste0('slope',1:(length(Out$Coefs)-1))
          if (length(Out$Coefs)>2) Out$Coefs[3:length(Out$Coefs)] <- Out$Coefs[2] + Out$Coefs[3:length(Out$Coefs)]
        }
        
      } else {
        Model <- segmented(LmModel,seg.Z=~x,psi=list(x=PSIs),
                           control=seg.control(display=F,it.max=50,maxit.glm=50,quant=F,n.boot=20,seed=5459432))
        RegAIC <- AIC(Model)
        RegBIC <- BIC(Model)
        
        if (OutType==1) {
          data2 <- cbind.data.frame(x=sort(c(range(data2$x,na.rm=T),Model$psi[,'Est.'])))
          Out <- data.frame(x=data2$x,y=predict.segmented(Model,data2))
          names(Out) <- c(xCol,yCol)
          
        } else {
          data2 <- data.frame(x=data2$x,y=predict.segmented(Model,data2))
          names(data2) <- c(xCol,yCol)
          
          Out <- list()
          Out$Fit   <- data2
          Out$Model <- Model
          Out$AIC   <- RegAIC
          Out$BIC   <- RegBIC
          Out$rp    <- sqrt(summary(Model)$r.squared)
          Out$r2    <- summary(Model)$r.squared
          Out$r2adj <- summary(Model)$adj.r.squared
          Out$Break <- Model$psi[,'Est.']
          Out$Coefs <- c(coef(Model)[1],slope(Model)[[1]][,'Est.'])
        }
      }
      return(Out)
    }
    
    # Add regresson equation and stats
    if (!is.null(RegType)) {
      
      Lm_Eqn <- function(df) {
        dfOrg <- df
        if (!('x' %in% colnames(df)) && ncol(df)==2) colnames(df) <- c('x','y')
        df1 <- df
        
        if (is.factor(df$x) || is.character(df$x)) df$x <- seq_along(df$x)
        if (RegXfrom1) df$x <- df$x - min(df$x,na.rm=T) + 1
        
        RangeRegX     <- range(df$x,na.rm=T)
        RangeRegY     <- range(df$y,na.rm=T)
        RegStatVal <- 0
        BestAIC    <- BestBIC <- Inf
        BestDegree <- RegDegree[1]
        BestType   <- RegType[1]
        
        # Start of loop
        for (Ri in 1:length(RegDegree)) {
          iRType   <- RegType[Ri]
          iRType   <- ifelse(iRType!='loess',as.numeric(iRType),iRType)
          iRDegree <- RegDegree[Ri]
          
          if (is.numeric(iRType)) {
            if (iRDegree==1 && iRType>1 && !iRType%in%c(88,99)) {
              Tr <- Trend_Analysis(Table=df,LevCols=NULL,TimeCol='x',MainCols='y',Alpha=PThr,Methods=unique(c(1,iRType)),TimeFrom1=F,Intercept=RegInterc)
              Tr <- as.data.frame(Tr)
              
              a <- which(c('LN2_SL','LN1_SL','LN0_SL','LN_SL')%in%names(Tr))[1]
              a <- as.numeric(unlist(Tr[c('LN2_SL','LN1_SL','LN0_SL','LN_SL')[a]]))
              intr <- which(c('LN2_intercept','LN1_intercept','LN0_intercept','LN_intercept','MK_intercept','YP_intercept','ZH_intercept')%in%names(Tr))[1]
              intr <- as.numeric(unlist(Tr[c('LN2_intercept','LN1_intercept','LN0_intercept','LN_intercept','MK_intercept','YP_intercept','ZH_intercept')[intr]]))[1]
              p    <- which(c('MK_P','YP_P','ZH_P','LN2_P','LN1_P','LN0_P','LN_P')%in%names(Tr))[1]
              pval_p <- as.numeric(unlist(Tr[c('MK_P','YP_P','ZH_P','LN2_P','LN1_P','LN0_P','LN_P')[p]]))
              
              Coefs <- c(intr,a)
              if (!is.numeric(iRType) || !RegStatFromEq) {
                xx <- df$x
                yy <- df$y
                pval_p <- round(cor.test(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='pearson')$p.value,digits=3)
                pval_s <- round(cor.test(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='spearman')$p.value,digits=3)
                
              } else {
                xx <- df$y
                yy <- intr + a*df1[,'x']
              }
              Reg <- lm(formula='y~x',data=df)
              
              if (length(xx)>2) {
                rp <- round(cor(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='pearson'),digits=2)
                rs <- round(cor(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='spearman'),digits=2)
                
              } else {
                rp <- rs <- NA_real_
              }
              
            } else {
              
              if (iRType==88) {
                # Logitsic regression
                Asy  <- quantile(df$y,0.9,na.rm=T)
                Reg  <- coef(lm(logit(y/Asy)~x,data=df))
                Form <- 'y~a/(1+exp(-(b+c*x)))'
                Reg  <- nls(Form,data=df,trace=F,model=T,list(a=Asy,b=Reg[1],c=Reg[2]),control=list(maxiter=500,minFactor=0.0001))
                
                RegAIC <- AIC(Reg)
                RegBIC <- BIC(Reg)
                
                if (RegAIC < BestAIC || RegBIC < BestBIC) {
                  BestAIC <- RegAIC
                  BestBIC <- RegBIC
                  BestType   <- iRType
                  BestDegree <- iRDegree
                  Coefs <- coef(Reg)
                  
                } else{
                  next()
                }
                
                if (!is.numeric(iRType) || !RegStatFromEq) {
                  xx <- df$x
                  yy <- df$y
                  
                } else {
                  xx <- df$y
                  yy <- predict(Reg,df)
                }
                
              } else if (iRType==99) {
                # Piece-wise linear regression
                Reg    <- FuncSegReg(setDT(dfOrg),OutType=2,Degree=iRDegree)
                RegAIC <- Reg$AIC
                RegBIC <- Reg$BIC
                
                if (RegAIC < BestAIC || RegBIC < BestBIC) {
                  BestAIC <- RegAIC
                  BestBIC <- RegBIC
                  BestType   <- iRType
                  BestDegree <- iRDegree
                  Coefs <- Reg$Coefs
                  Break <- Reg$Break
                  
                } else{
                  next()
                }
                
                if (!is.numeric(iRType) || !RegStatFromEq) {
                  xx <- df$x
                  yy <- df$y
                  
                } else {
                  xx <- df$y
                  yy <- Reg$Fit[,yCol]
                }
                
              } else if (is.numeric(iRType)) {
                # Regular regression
                
                if (!is.null(RegInterc) || iRType==0) {
                  if (is.null(RegInterc)) RegInterc <- 0
                  # Regular regression with known intercept
                  Form  <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)+0+offset(%s*I(x^0))',iRDegree,RegInterc))
                  Reg   <- lm(formula=Form,data=df)
                  
                } else {
                  # Regular regression with unknown intercept
                  Form <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)',iRDegree))
                  Reg   <- lm(formula=Form,data=df)
                }
                
                RegAIC <- AIC(Reg)
                RegBIC <- BIC(Reg)
                
                if (RegAIC < BestAIC || RegBIC < BestBIC) {
                  BestAIC <- RegAIC
                  BestBIC <- RegBIC
                  BestType   <- iRType
                  BestDegree <- iRDegree
                  
                  if (BestType==88) {
                    Coefs <- coef(Reg)
                    
                  } else {
                    Coefs <- c(RegInterc,as.numeric(coef(Reg)))
                  }
                  
                } else{
                  next()
                }
                RegPredict <- predict(Reg,df1)
                
                if (!is.numeric(iRType) || !RegStatFromEq) {
                  xx <- df$x
                  yy <- df$y
                  
                } else {
                  xx <- df$y
                  yy <- RegPredict
                }
              }
              
              if (length(xx)>2) {
                rp   <- round(cor(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='pearson'),digits=2)
                rs   <- round(cor(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='spearman'),digits=2)
                pval_p <- round(cor.test(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='pearson')$p.value,digits=3)
                pval_s <- round(cor.test(as.numeric(xx),as.numeric(yy),use='pairwise.complete.obs',method='spearman')$p.value,digits=3)
                
              } else {
                rp <- rs <- pval_p <- pval_s <- NA_real_
              }
            }
            
            K <- length(Coefs)-(iRDegree!=0)
            N <- sum(!is.na(df$x) & !is.na(df$y))
            r2 <- round(rp^2,digits=2)
            r2adj <- max(0, 1 - (1-r2)*(N-1)/(N-K-1))
            
            if (!is.null(RegEqStat)) {
              RegStatVal <- MyCompare(Stats=RegEqStat,Obs=xx,Sims=yy,Digits=2,Probs=c(0.1,0.9),CompleteCases=F)
              if (is.null(RegEqStDig)) RegEqStDig <- rep(RegEqDigs[2],length(RegEqStat))
              if (length(RegEqStDig)==1) RegEqStDig <- rep(RegEqStDig,length(RegEqStat))
              
              for (i in 1:length(RegEqStat)) {
                if (!is.na(as.numeric(RegStatVal[i]))) RegStatVal[i] <- sprintf(paste0('%.',RegEqStDig[i],'f'),as.numeric(RegStatVal[i]))
              }
              if ('N' %in% RegEqStat) RegStatVal[RegEqStat=='N'] <- as.character(as.numeric(RegStatVal[RegEqStat=='N']))
            }
            
            if (is.na(r2))     r2     <- 0
            if (is.na(pval_p)) pval_p <- 1
            if (is.na(pval_s)) pval_s <- 1
            
          } else {
            BestType <- 'loess'
          }
        }
        # End of loop
        
        if (is.numeric(BestType)) {
          rp    <- sprintf('%.2f',rp)
          rs    <- sprintf('%.2f',rs)
          r2    <- sprintf('%.2f',r2)
          PVal  <- pval_p
          pval_p<- sprintf("%.3f",pval_p)
          pval_s<- sprintf("%.3f",pval_s)
          meanx <- Print_Decimals(mean(yy,na.rm=T),RegEqDigs[2],AddSign=F)
          
          vals <- NULL
          vals <- list(rp=rp,rs=rs,r2=r2,r2adj=r2adj,pval_p=pval_p,pval_s=pval_s,meanx=meanx)
          if (!is.null(RegEqStat)) {
            for (St in RegEqStat) {
              Ind <- which(RegEqStat==St)
              vals[[paste0('RegStatVal',Ind)]] <- RegStatVal[Ind]
              vals[[paste0('Stat',Ind)]] <- St
            }
          }
          
          if (BestType==88) {
            for (i in 1:2) vals[[letters[i]]] <- Print_Decimals(as.numeric(Coefs[i]),RegEqDigs[2],AddSign=F)
            for (i in   3) vals[[letters[i]]] <- Print_Decimals(as.numeric(Coefs[i]),RegEqDigs[1],AddSign=F)
            
          } else if (BestType==99) {
            for (i in 1:BestDegree) vals[[paste0('br',i)]] <- Print_Decimals(as.numeric(Break[i]),RegEqDigs[2],AddSign=F)
            vals[['intr']] <- Print_Decimals(-as.numeric(Coefs[1]),RegEqDigs[2],AddSign=F)
            vals[['a']] <- Print_Decimals(as.numeric(Coefs['slope1']),RegEqDigs[1],AddSign=F)
            for (i in 1:BestDegree) vals[[letters[i+1]]] <- Print_Decimals(as.numeric(Coefs[paste0('slope',i+1)]),RegEqDigs[1],AddSign=F)
            
          } else {
            vals[['intr']] <- Print_Decimals(Coefs[1],RegEqDigs[2],AddSign=T)
            for (i in 1:BestDegree) vals[[letters[i]]] <- Print_Decimals(Coefs[i+1],RegEqDigs[1],AddSign=(i!=BestDegree))
          }
          
          vals[['space1']] <- vals[['space2']] <- ''
          if (RegEqType=='label' & length(RegGroups)==1) {
            if (is.null(RegEqHjust)) vals[['space2']] <- ' ' else {
              if (RegEqHjust=='left')  vals[['space1']] <- ' '
              if (RegEqHjust=='right') vals[['space2']] <- ' '
            }
          }
        }
        
        eq <- ""
        if (is.numeric(BestType) && AddRegEq!=FALSE) {
          
          if (BestType==88) {
            EqExp  <- "list(space1*"
            EqFlag <- F
            
            if (AddRegEq==T) {
              RegEqComps <- c('Eq')
            } else {
              RegEqComps <- strsplit(AddRegEq,split='-')[[1]]
            }
            
            for (EqComp in RegEqComps) {
              if (EqComp=='Eq') {
                EqExp <- paste0(EqExp,'italic(A)==a*`,`~italic(B)==b*`,`~italic(C)==c*')
                EqFlag <- T
              }
            }
            
          } else if (BestType==99) {
            EqExp <- "list(space1*"
            EqFlag  <- F
            
            if (AddRegEq==T) {
              RegEqComps <- c('S','B')
            } else {
              RegEqComps <- strsplit(AddRegEq,split='-')[[1]]
            }
            
            for (EqComp in RegEqComps) {
              if (EqComp=='S') {
                EqExp <- paste0(EqExp,'italic(S[1])==a*`,`~italic(S[2])==b*')
                if (BestDegree>1) for (Deg in 2:BestDegree) EqExp <- paste0(EqExp,'`,`~italic(S[',Deg+1,'])==',letters[Deg+1],'*')
                EqFlag <- T
              }
              if (EqComp=='B') {
                if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
                EqExp <- paste0(EqExp,'italic(B[1])==br1*')
                if (BestDegree>1) for (Deg in 2:BestDegree) EqExp <- paste0(EqExp,'`,`~italic(B[',Deg,'])==br',Deg,'*')
                EqFlag <- T
              }
            }
            
          } else {
            
            EqExp <- "list(space1*"
            EqFlag  <- F
            
            if (AddRegEq==T) {
              RegEqComps <- c('Eq','R2','P')
            } else {
              RegEqComps <- strsplit(AddRegEq,split='-')[[1]]
            }
            
            for (EqComp in RegEqComps) {
              if (EqComp=='Eq') {
                EqExp <- paste0(EqExp,'italic(y)==')
                if (BestDegree>1) for (Deg in BestDegree:2) EqExp <- paste0(EqExp,letters[Deg],"*italic(x^",Deg,")*")
                EqExp <- paste0(EqExp,"a*italic(x)",ifelse(BestType!=0 || (!is.null(RegInterc) && RegInterc!=0),'*intr*','*'))
                EqFlag <- T
              }
              if (EqComp=='Sl') {
                if (BestDegree==1)
                  EqExp <- paste0(EqExp,'italic(SL)==',letters[1],"*")
                else 
                  for (Deg in BestDegree:1) EqExp <- paste0(EqExp,'italic("',letters[Deg],'")==',letters[Deg],"*")
                  EqFlag <- T
              }
              if (EqComp=='Int') {
                EqExp <- paste0(EqExp,'italic(I)==','intr',"*")
                vals$intr <- gsub('\\+','',vals$intr)
              }
            }
          }
          
          for (EqComp in RegEqComps) {
            if (EqComp=='R') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(r)==rp*')
              EqFlag <- T
            }
            if (EqComp=='Rs') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(r[sp])==rs*')
              EqFlag <- T
            }
            if (EqComp=='R2') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(r)^2==r2*')
              EqFlag <- T
            }
            if (EqComp=='R2Adj') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(r[adj])^2==r2adj*')
              EqFlag <- T
            }
            if (EqComp=='P') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(p)==pval_p*')
              EqFlag <- T
            }
            if (EqComp=='Ps') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(p[sp])==pval_s*')
              EqFlag <- T
              PVal <- pval_s
            }
            if (EqComp=='Ast' || EqComp=='Star') {
              if (EqFlag) EqExp <- paste0(EqExp,'~')
              if (pval_p<0.001) Temp <- "`***`" else if (pval_p<0.01) Temp <- "`**`" else if (pval_p<0.05) Temp <- "`*`" else {
                if (EqFlag) EqExp <- str_sub(EqExp,1,nchar(EqExp)-1)
                Temp <- '` `^{ns}'
              }
              if (!is.null(Temp)) EqExp <- paste0(EqExp,Temp,'*')
              EqFlag <- T
            }
            if (EqComp=='Mean') {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              EqExp <- paste0(EqExp,'italic(Mean)==meanx*')
              EqFlag <- T
            }
          }
          
          if (AddRegEq!=FALSE && !is.null(RegEqStat)) {
            for (St in RegEqStat) {
              if (EqFlag) EqExp <- paste0(EqExp,'`,`~')
              Ind <- which(RegEqStat==St)
              EqExp <- paste0(EqExp,'Stat',Ind,'==RegStatVal',Ind,'*')
              EqFlag <- T
            }
          }
          
          EqExp <- paste0(EqExp,'space2)')
          eq    <- do.call("substitute", list(parse(text=paste(EqExp))[[1]], vals))
          Out   <- data.table(eq=as.character(as.expression(eq)), RegType=BestType, RegDegree=BestDegree, Model=list(Reg), PVal=PVal, RangeRegX=list(RangeRegX), RangeRegY=list(RangeRegY))
          
        } else {
          Out <- data.table(eq="", RegType=BestType, RegDegree=BestDegree, RangeRegX=list(RangeRegX), RangeRegY=list(RangeRegY))
        }
        
        return(Out)
      }
      # End of Lm_Eqn
      
      if (is.null(RegTable)) {
        RegTable <- Data2[, Lm_Eqn(.SD), keyby=c(Facets,'RegGr1362','RegGr1362C'), .SDcols=c(xCol,yCol)]
        
        if (!RegDrop) {
          Cols <- c(Facets,'RegGr1362','RegGr1362C')
          xx <- levels(Data2$RegGr1362)
          if (is.null(xx)) xx <- unique(Data2$RegGr1362)
          yy <- levels(Data2$RegGr1362C)
          if (is.null(yy)) yy <- unique(Data2$RegGr1362C)
          zz <- unique(Data2[,Facets,with=F])
          Temp <- list(RegGr1362=xx, RegGr1362C=yy)
          for (ii in names(zz)) Temp[[ii]] <- unique(as.character(zz[[ii]]))
          Temp <- unique(expand.grid(Temp))
          if (names(Temp)[1]=='Var1') names(Temp) <- 'RegGr1362'
          RegTable <- setDT(merge(Temp,RegTable,by=c(Facets,'RegGr1362','RegGr1362C'),all=T,sort=T))
          RegTable[is.na(eq), eq:=ifelse(length(RegGroups)==1 && length(RegGroupsC)==1,'','No~Data')]
        }
      }
      
      if (!is.null(Facets)) {
        RegTable$FGroups <- apply(RegTable[,c(Facets),with=F],1,paste,collapse='-')
        FacetGroups <- unique(RegTable[,FGroups])
        
      } else {
        RegTable$FGroups <- 'All'
        FacetGroups <- 'All'
      }
      
      # Only show significant regressions
      if (!is.null(RegPThr) && RegPThr<=1) RegTable <- RegTable[PVal<RegPThr]
      
      # Positioning of regression equations
      if (AddRegEq!=FALSE && any(!is.na(as.numeric(RegTable$RegType)))) {
        if (is.null(RegEqNRowsRange)) RegEqNRowsRange <- c(1,200)
        if (is.null(RegEqNumCols)) RegEqNumCols <- 1
        
        for (FGroup in FacetGroups) {
          RegGroups2 <- RegGroups[RegGroups%in%RegTable[FGroups==FGroup,RegGr1362]]
          ColNum <- RowNum <- 0
          
          if (FacetScale %in% c('free','free_x')) {
            rTemp <- range(unlist(RegTable[FGroups==FGroup,RangeRegX]))
            xAxisRange2 <- rTemp + diff(rTemp)*c(0.5,-0.5)*xyOffset[1:2]
            
          } else {
            xAxisRange2 <- xAxisRange + diff(xAxisRange)*c(0.5,-0.5)*xyOffset[1:2]
          }
          
          if (FacetScale %in% c('free','free_y')) {
            rTemp <- range(unlist(RegTable[FGroups==FGroup,RangeRegY]))
            yAxisRange2 <- rTemp + diff(rTemp)*c(0.5,-0.5)*xyOffset[3:4]
            
          } else {
            yAxisRange2 <- yAxisRange + diff(yAxisRange)*c(0.5,-0.5)*xyOffset[3:4]
          }
          
          xEqMarg <- RegEqMarg[1] * base::diff(xAxisRange2) / 100
          yEqMarg <- RegEqMarg[2] * base::diff(yAxisRange2) / 100
          xEqAdj  <- RegEqHVadj[1] * base::diff(xAxisRange2) / 100
          yEqAdj  <- RegEqHVadj[2] * base::diff(yAxisRange) / 100
          
          Rows <- RegTable[FGroups==FGroup, which=T]
          nRow <- length(Rows)
          RegEqNumRows <- ceiling(nRow/RegEqNumCols) + !is.null(RegEqTitles)
          
          if (!is.null(RegEqTitles)) {
            if (length(grep('top',RegEqPos))>0) {
              RegGroups2 <- c('RegEqHeader',RegGroups2)
            } else {
              RegGroups2 <- c(RegGroups2,'RegEqHeader')
            }  
            
            Temp <- RegTable[FGroups==FGroup][1]
            Temp[, `:=`(RegGr1362='RegEqHeader',RegGr1362C='RegEqHeaderC',eq=paste0('bold(',ifelse(ColNum==0,RegEqTitles,''),')'))]
            RegTable   <- rbind(RegTable,Temp)
            RegGroupsC <- union(RegGroupsC,'RegEqHeaderC')
          }
          
          for (RegGroup in RegGroups2) {
            RegGroupsC2 <- RegGroupsC[RegGroupsC%in%RegTable[FGroups==FGroup & RegGr1362==RegGroup,RegGr1362C]]
            
            for (RegGroupC in RegGroupsC2) {
              Rows <- RegTable[RegGr1362==RegGroup & RegGr1362C==RegGroupC & FGroups==FGroup, which=T]
              
              if ((RowNum>=RegEqNRowsRange[2] || RowNum>=RegEqNumRows) && RowNum>RegEqNRowsRange[1]) {
                ColNum <- ColNum + 1
                RowNum <- !is.null(RegEqTitles)
              }
              
              if (RegEqPos=='bottomright') {
                EqLabelsX <- xAxisRange2[2]+xEqAdj-ColNum*xEqMarg
                EqLabelsY <- yAxisRange2[1]+yEqAdj+(RowNum+(RegGroup!='RegEqHeader')*0.1)*yEqMarg
                EqVjust <- 'bottom'
                if (is.null(RegEqHjust)) RegEqHjust <- 'right'
                
              } else if (RegEqPos=='bottomleft'){
                EqLabelsX <- xAxisRange2[1]+xEqAdj+ColNum*xEqMarg
                EqLabelsY <- yAxisRange2[1]+yEqAdj+(RowNum+(RegGroup!='RegEqHeader')*0.1)*yEqMarg
                EqVjust <- 'bottom'
                if (is.null(RegEqHjust)) RegEqHjust <- 'left'
                
              } else if (RegEqPos=='bottom'){
                EqLabelsX <- mean(xAxisRange2,na.rm=T)+xEqAdj+ColNum*xEqMarg
                EqLabelsY <- yAxisRange[1]+yEqAdj+(RowNum+(RegGroup!='RegEqHeader')*0.1)*yEqMarg
                EqVjust <- 'bottom'
                if (is.null(RegEqHjust)) RegEqHjust <- 'center'
                
              } else if (RegEqPos=='topright') {
                EqLabelsX <- xAxisRange2[2]+xEqAdj-ColNum*xEqMarg
                EqLabelsY <- yAxisRange2[2]+yEqAdj-(RowNum+(RegGroup!='RegEqHeader')*0.1)*yEqMarg
                EqVjust <- 'top'
                if (is.null(RegEqHjust)) RegEqHjust <- 'right'
                
              } else if (RegEqPos=='topleft') {
                EqLabelsX <- xAxisRange2[1]+xEqAdj+ColNum*xEqMarg
                EqLabelsY <- yAxisRange2[2]+yEqAdj-(RowNum+(RegGroup!='RegEqHeader')*0.1)*yEqMarg
                EqVjust <- 'top'
                if (is.null(RegEqHjust)) RegEqHjust <- 'left'
                
              } else if (RegEqPos=='top') {
                EqLabelsX <- mean(xAxisRange2,na.rm=T)+xEqAdj
                EqLabelsY <- yAxisRange2[2]+yEqAdj-(which(RegGroups2==RegGroup)-1)*yEqMarg
                EqVjust <- 'top'
                if (is.null(RegEqHjust)) RegEqHjust <- 'center'
                
              } else if (is.numeric(RegEqPos)) {
                EqLabelsX <- RegEqPos[1]
                EqLabelsY <- RegEqPos[2]+(which(RegGroups2==RegGroup)-1)*yEqMarg
                EqVjust <- 'middle'
                if (is.null(RegEqHjust)) RegEqHjust <- 'center'
              }
              
              EqLabels  <- unclass(RegTable$eq[Rows])
              if (length(RegGroups)>1 && RegGroup!='RegEqHeader' && RegEqAddNames) {
                space1 <- ifelse(RegEqType=='label' & RegEqHjust=='left','~~','')
                if (length(EqLabels)>1 || is.null(levels(EqLabels))) EqLabels <- paste0(space1,gsub(' ',' ',paste0('`',RegGroup,'`')),'~','":"','~~',EqLabels)
                else EqLabels <- paste0(space1,gsub0(' ',' ',paste0('`',RegGroup,'`')),'~','":"','~~',levels(EqLabels))
              }
              RegTable[Rows, `:=`(eq=EqLabels,x=EqLabelsX,y=EqLabelsY)]
              RowNum  <- RowNum + 1
            }
          }
        }
        
        if (is.null(RegEqBold)) {
          i2 <- 1 
          RegEqBold <- 0
          
        } else if (length(RegEqBold)==1 && RegEqBold==TRUE) {
          RegEqBold <- 0.001
          i2 <- 2 
          
        } else if (length(RegEqBold)==2) {
          i2 <- RegEqBold[2] 
          RegEqBold <- RegEqBold[1]
          
        } else if (RegEqBold==F) {
          RegEqBold <- 0
          i2 <- 1 
        }
        
        if (!is.null(RegAggUnit)) Temp <- RegTable[RegGr1362!=RegAggUnit] else Temp <- RegTable
        
        if (!is.null(RegEqCol) && RegEqCol==T) RegEqCol <- NULL
        if (length(RegGroupsC)==1 && is.null(RegEqCol)) RegEqCol <- 'black'
        
        if (RegEqType=='text') {
          for (i in 1:i2) {
            for (j in 1:(1+!is.null(RegEqTitles))) {
              
              if (j==1) {
                Temp2 <- copy(Temp)[RegGr1362!='RegEqHeader']
                
                if (length(RegGroupsC)>1 && is.null(RegEqCol)) {
                  if (!is.null(rgcCol)) {
                    x <- x + geom_text(data=Temp2,aes(x=x,y=y,label=eq,color=RegGr1362C),family=FontFamily,inherit.aes=F,show.legend=F,
                                       hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize*(1+(i-1)*RegEqBold[1]),alpha=EqBoxAlpha)
                    
                  } else if (!is.null(fCol) && length(FColors)>1) {
                    TempCol <- FColors[seq_along(unique(Temp2$RegGr1362))]
                    TempCol <- rep(TempCol,nrow(Temp2)/length(TempCol))
                    x <- x + geom_text(data=Temp2,aes(x=x,y=y,label=eq),family=FontFamily,inherit.aes=F,show.legend=F,color=TempCol,
                                       hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize*(1+(i-1)*RegEqBold[1]),alpha=EqBoxAlpha)
                  }
                  
                } else {
                  x <- x + geom_text(data=Temp2,aes(x=x,y=y,label=eq),family=FontFamily,inherit.aes=F,color=RegEqCol,
                                     hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize*(1+(i-1)*RegEqBold[1]),alpha=EqBoxAlpha,show.legend=F)
                }
                if (!is.null(RegAggUnit)) {
                  x <- x + geom_text(data=RegTable[RegGr1362==RegAggUnit],aes(x=x,y=y,label=eq,color=NULL,group=NULL,shape=NULL),family=FontFamily,
                                     inherit.aes=F,color='black',show.legend=F,
                                     hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize*(1+(i-1)*RegEqBold[1]),alpha=EqBoxAlpha)
                }
                
              } else {
                # Adding headers
                Temp2 <- copy(Temp)[RegGr1362=='RegEqHeader']
                x <- x + geom_text(data=Temp2,aes(x=x,y=y,label=eq),family=FontFamily,inherit.aes=F,show.legend=F,color='black',
                                   
                                   hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize*(1+(i-1)*RegEqBold[1]),alpha=EqBoxAlpha)
                
              }
            }
          }
          
        } else if (RegEqType=='label') {
          if (length(RegEqCol)==1 && RegEqCol==T && length(RegGroups)>1) {
            x <- x + geom_label(data=Temp,aes(x=x,y=y,label=eq,color=RegGr1362),family=FontFamily,inherit.aes=F,show.legend=F,
                                hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize,fill=EqBoxFill,alpha=EqBoxAlpha)
          } else {
            if (is.null(RegEqCol)) RegEqCol <- 'black'
            x <- x + geom_label(data=Temp,aes(x=x,y=y,label=eq),family=FontFamily,inherit.aes=F,color=RegEqCol,show.legend=F,
                                hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize,fill=EqBoxFill,alpha=EqBoxAlpha)
          }
          if (!is.null(RegAggUnit)) {
            x <- x + geom_label(data=RegTable[RegGr1362==RegAggUnit],aes(x=x,y=y,label=eq),family=FontFamily,inherit.aes=F,color='black',show.legend=F,
                                hjust=RegEqHjust,vjust=EqVjust,parse=T,size=RegEqSize,fill=EqBoxFill,alpha=EqBoxAlpha)
          }
        }
      }
    }
    
    # Add regression line
    if (!is.null(RegType) & ShowReg) {
      if (is.null(CColors)) CColors <- 'black'
      
      if (!is.null(RegLCol) && RegLCol==T) RegLCol <- NULL
      if (length(RegGroupsC)==1 && is.null(RegLCol)) RegLCol <- 'black'
      
      NN <- ifelse(!88%in%Data2$RegType && (!exists('RegTable') || (length(RegType)==1 && length(RegDegree)==1) || 
                                              (length(unique(Data2$RegType))==1 && length(unique(Data2$RegDegree))==1)),1,nrow(RegTable))
      
      for (i in 1:NN) {
        if (is.null(RegRange)) 
          Temp <- Data2
        else 
          Temp <- Data2[data.table::between(get(xCol),RegRange[1],RegRange[2],incbounds=T),]
        
        if (exists('RegTable') && RegTable$RegType[i]!='loess') {
          Temp <- Temp[RegGr1362 %in% RegTable$RegGr1362]
          
          if (NN>1) {
            Temp       <- merge(Temp,RegTable[i,c(Facets,'RegGr1362'),with=F],by=c(Facets,'RegGr1362'),sort=F)
            BestType   <- RegTable$RegType[i]
            BestType   <- ifelse(!is.na(as.numeric(BestType)),as.numeric(BestType),BestType)
            BestDegree <- RegTable$RegDegree[i]
            
          } else {
            BestType   <- RegTable$RegType[1]
            BestType   <- ifelse(!is.na(as.numeric(BestType)),as.numeric(BestType),BestType)
            BestDegree <- RegTable$RegDegree[1]
          }
          
        } else {
          BestType   <- RegType
          BestDegree <- 1
        }
        
        if (!is.null(BestType) && BestType==T) BestType <- 1
        if (!is.null(BestType) && BestType<1) BestDegree <- 1
        if (BestDegree>1 && BestType!=99) BestType <- 1
        
        if (BestType==88) {
          # Logistic regression
          Temp1  <- Temp[,range(get(xCol),na.rm=T)]
          Temp   <- data.table(x=seq(Temp1[1],Temp1[2],length.out=100),RegGr1362=Temp$RegGr1362[1])
          Temp$y <- predict(RegTable$Model[[i]],Temp)
          
          if (length(RegGroups)>1 && is.null(RegLCol)) {
            if (!is.null(LTypes) && length(LTypes)==1) {
              x <- x + geom_line(data=Temp,aes_string(x='x',y='y',group='interaction(RegGr1362,RegGr1362C)',color='RegGr1362'),inherit.aes=F,
                                 show.legend=F,na.rm=T,lty=LTypes,lwd=RegLwd)
            } else {
              x <- x + geom_line(data=Temp,aes_string(x='x',y='y',group='interaction(RegGr1362,RegGr1362C)',color='RegGr1362',linetype=ltCol),inherit.aes=F,
                                 show.legend=F,na.rm=T,lwd=RegLwd)
            }
            
          } else {
            if (is.null(RegLCol)) RegLCol <- 'black'
            
            if (!is.null(RegLTypes) && length(RegLTypes)==1) {
              x <- x + geom_line(data=Temp,aes_string(x='x',y='y',group='interaction(RegGr1362,RegGr1362C)'),inherit.aes=F,
                                 show.legend=F,na.rm=T,lty=RegLTypes,lwd=RegLwd)
            } else {
              x <- x + geom_line(data=Temp,aes_string(x='x',y='y',group='interaction(RegGr1362,RegGr1362C)',linetype=ltCol),inherit.aes=F,
                                 show.legend=F,na.rm=T,lwd=RegLwd)
            }
          }
          
        } else if (BestType==99) {
          # Piece-wise regression
          Temp <- Temp[, FuncSegReg(.SD,OutType=1,Degree=BestDegree), keyby=c(Facets,'RegGr1362','RegGr1362C'), .SDcols=setdiff(names(Temp),c(Facets,'RegGr1362','RegGr1362C'))]
          
          if (length(RegGroupsC)>1 && is.null(RegLCol)) {
            if (!is.null(RegLTypes) && length(RegLTypes)==1) {
              x <- x + geom_line(data=Temp,aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color='RegGr1362C'),inherit.aes=F,
                                 show.legend=F,na.rm=T,lty=RegLTypes,lwd=RegLwd)
            } else {
              x <- x + geom_line(data=Temp,aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color='RegGr1362C',linetype=ltCol),inherit.aes=F,
                                 show.legend=F,na.rm=T,lwd=RegLwd)
            }
            
          } else {
            if (is.null(RegLCol)) RegLCol <- 'black'
            
            if (!is.null(RegLTypes) && length(RegLTypes)==1) {
              x <- x + geom_line(data=Temp,aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)'),inherit.aes=F,
                                 show.legend=F,na.rm=T,lty=RegLTypes,lwd=RegLwd)
            } else {
              x <- x + geom_line(data=Temp,aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',linetype=ltCol),inherit.aes=F,
                                 show.legend=F,na.rm=T,lwd=RegLwd)
            }
          }
          
        } else {
          # Regular regression
          if (is.null(rggCol)) ShowRegAggUnit <- F
          
          if (!is.null(RegAggUnit) && !ShowRegAggUnit && !is.null(gCol)) {
            Temp <- Temp[as.character(RegGr1362)!=RegAggUnit]
            
          } else {
            if (ShowRegAggUnit) CColors <- c('black',CColors)
          } 
          
          if (length(RegGroupsC)>1 && is.null(RegLCol)) {
            if (is.character(BestType)) {
              Form <- 'y~x'
              
            } else {
              if (!is.null(RegInterc)) {
                Form  <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)+0+offset(%s*I(x^0))',BestDegree,RegInterc))
                
              } else {
                Form <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)',BestDegree))
              }
            }
            
            if (BestType>0 || is.character(BestType)) {
              if (!is.null(RegLTypes) && length(RegLTypes)==1) {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color=ifelse(!is.null(rgcCol),rgcCol,fCol)),inherit.aes=F,method=ifelse(is.character(BestType),BestType,'lm'),show.legend=RegShowLeg,
                                     position=RegLinePos,formula=Form,se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,lty=RegLTypes,fullrange=RegFullRange,span=BinWidth)
              } else {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color=ifelse(!is.null(rgcCol),rgcCol,fCol),linetype=ltCol),inherit.aes=F,method=ifelse(is.character(BestType),BestType,'lm'),show.legend=RegShowLeg,
                                     position=RegLinePos,formula=Form,se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
              }
              
            } else if (BestType==0) {
              x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color=ifelse(!is.null(rgcCol),rgcCol,fCol),linetype=ltCol),inherit.aes=F,method='lm',show.legend=RegShowLeg,
                                   position=RegLinePos,formula=as.formula('y~x-1'),se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,lty=RegLTypes,fullrange=RegFullRange,span=BinWidth)
              
            } else if (BestType==-1) {
              if (!is.null(RegLTypes) && length(RegLTypes)==1) {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color=ifelse(!is.null(rgcCol),rgcCol,fCol)),inherit.aes=F,method='lm',show.legend=RegShowLeg,
                                     position=RegLinePos,formula=as.formula(sprintf('y~0+x+offset(%s*I(x^0))',RegInterc)),se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,lty=RegLTypes,fullrange=RegFullRange,span=BinWidth)
              } else {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,group='interaction(RegGr1362,RegGr1362C)',color=ifelse(!is.null(rgcCol),rgcCol,fCol),linetype=ltCol),inherit.aes=F,method='lm',show.legend=RegShowLeg,
                                     position=RegLinePos,formula=as.formula(sprintf('y~0+x+offset(%s*I(x^0))',RegInterc)),se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
              }
            }
            
          } else {
            if (is.null(RegLCol)) {
              if (is.null(CColors)) RegLCol <- 'black' else RegLCol <- CColors[1]
            }
            
            if (!is.null(RegInterc) && !BestType%in%c(88)) {
              Form  <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)+0+offset(%s*I(x^0))',BestDegree,RegInterc))
              
            } else {
              Form <- as.formula(sprintf('y~poly(x,%s,raw=TRUE)',BestDegree))
            }
            
            if (BestType>0 || is.character(BestType)) {
              if (!is.null(RegLTypes) && length(RegLTypes)==1) {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol),color=RegLCol,inherit.aes=F,method=ifelse(is.character(BestType),BestType,'lm'),show.legend=RegShowLeg,
                                     position=RegLinePos,formula=Form,se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lty=RegLTypes,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
              } else {
                x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol,linetype=ltCol),color=RegLCol,inherit.aes=F,method=ifelse(is.character(BestType),BestType,'lm'),show.legend=RegShowLeg,
                                     position=RegLinePos,formula=Form,se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
              }
              
            } else if (BestType==0) {
              x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol),color=RegLCol,inherit.aes=F,method='lm',show.legend=RegShowLeg,
                                   position=RegLinePos,formula=as.formula('y~x-1'),se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lty=RegLTypes,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
              
            } else if (BestType==-1) {
              x <- x + geom_smooth(data=Temp,mapping=aes_string(x=xCol,y=yCol),color=RegLCol,inherit.aes=F,method='lm',show.legend=RegShowLeg,
                                   position=RegLinePos,formula=as.formula(sprintf('y~0+x+offset(%s*I(x^0))',RegInterc)),se=ifelse(RegConf=='r',T,F),level=ErrBarCL,na.rm=T,lty=RegLTypes,lwd=RegLwd,fullrange=RegFullRange,span=BinWidth)
            }
          }
          
          if (!is.null(RegLTypes) && length(RegLTypes)>1) x <- x + scale_linetype_manual(values=RegLTypes)
        }
      }
    }
  }
  
  # Adding facets.
  if (!is.null(Facets)) {
    if (is.null(FacetLabeller)) FacetLabeller <- as_labeller(Facet_Labels)
    if (is.null(NRow) && is.null(NCol)) {
      x <- x + facet_grid(facets=as.formula(paste(paste(vFacet,collapse='+'),"~",paste(hFacet,collapse='+'))),axes=FacetGridAxes,axis.labels=FacetGridLabs,
                          drop=DropFacets,margins=Margines,labeller=FacetLabeller,scales=FacetScale,space=FacetSpace,switch=FacetSwitch)
    } else {
      x <- x + facet_wrap(facets=as.formula(paste("~",paste(hFacet,collapse='+'))),drop=DropFacets,nrow=NRow,ncol=NCol,
                          labeller=FacetLabeller,scales=FacetScale,switch=FacetSwitch,axes=FacetGridAxes,axis.labels=FacetGridLabs)
    }
  }
  
  # Adding title, subtitle and caption.
  if (!is.null(Title) || !is.null(SubTitle) || !is.null(Caption) || !is.null(Tag)) {
    if (length(SubTitle)>1) SubTitle <- paste(SubTitle,collapse='\n')
    if (length(Title)>1) Title <- paste(Title,collapse='\n')
    
    if (!is.null(Caption)) x <- x + labs(caption=Caption,hjust=1)
    if (!is.null(Tag)) x <- x + labs(tag=Tag,hjust=1)
    x <- x + ggtitle(label=Title,subtitle=SubTitle)
  }
  
  # Adding colorbars for continuous plots.
  if (Type=='tiles' || ContinLeg) {
    FColors <- NULL
    if (PrettyLeg) LegSymmetr <- T
    
    BFunc <- function(y) {
      End <- F
      if ('TileColExtra'%in%names(DataS) && Type=='tiles' && is.null(LegLimits)) y <- range(DataS$TileColExtra,na.rm=T)
      else if ('TileColExtra'%in%names(DataS) && Type%in%c('hex','bin2d') && is.null(LegLimits)) y <- y
      else if (is.null(LegLimits)) y <- range(DataS[,get(ColX)],na.rm=T)
      else y <- LegLimits
      
      if (!is.null(FMidPoint) && LegSymmetr) {
        y <- max(abs(y-FMidPoint))
        y <- c(-1,1)*(FMidPoint+y)
      }
      
      if (PrettyLeg) {
        out <- PrettyBreaks(Range=y,NumBins=LegNumBreaks,MidPoint=FMidPoint,Digits=RTTrans,LessDigits=LegLessDigs)$Breaks
        RT  <- PrettyBreaks(Range=FuncTR2(y),NumBins=LegNumBreaks,MidPoint=FMidPoint,Digits=RT,LessDigits=LegLessDigs)$Digits
        
      } else {
        while(!End) {
          if (!is.null(FMidPoint) && In_Interval(FMidPoint,y,F)) {
            MinDiff <- diff(y)/(LegNumBreaks+1)
            Share1 <- round((FMidPoint-y[1])/(y[2]-y[1]),2)
            Share2 <- round((y[2]-FMidPoint)/(y[2]-y[1]),2)
            Share1 <- max(2-(Share1<0.1),round(2+Share1*(LegNumBreaks),0))
            Share2 <- max(2-(Share2<0.1),round(2+Share2*(LegNumBreaks),0))
            
            while (T) {
              out1 <- unique(round(seq(MyRound(y[1],RT,F),FMidPoint,length.out=Share1),RT))
              out2 <- unique(round(seq(FMidPoint,MyRound(y[2],RT,T),length.out=Share2),RT))
              out  <- sort(unique(c(out1,out2)))
              
              if (length(out1)==2) out1 <- out1[c(diff(out1)>=0.7*MinDiff,T)]
              else if (length(out1)>2) out1 <- out1[c(T,diff(out1)>=0.7*MinDiff)]
              if (length(out2)==2) out2 <- out2[c(T,diff(out2)>=0.7*MinDiff)]
              else if (length(out2)>2) out2 <- out2[c(diff(out2)>=0.7*MinDiff,T)]
              
              if((length(out1)==1) || all(diff(out1)<=1.35*min(diff(out1)))) {}
              else {RT <- RT + 1; next}
              
              if((length(out2)==1) || all(diff(out2)<=1.35*min(diff(out2)))) {break()}
              else {RT <- RT + 1; next}
              RT <<- RT
              break()
            }
            out  <- sort(unique(c(out1,out2)))
            
          } else {
            
            MinDiff <- diff(y)/(LegNumBreaks+1)
            while (T) {
              out <- unique(round(seq(MyRound(y[1],RT,F),MyRound(y[2],RT,T),length.out=LegNumBreaks+2),RT))
              if(all(diff(out)<=1.35*min(diff(out)))) break
              RT <- RT + 1
            }
            out <- out[c(diff(out)>=0.7*MinDiff,T)]
          }
          
          if (CheckFMidPoint && !is.null(FMidPoint) && !In_Interval(FMidPoint,range(out),WithEqual=F)) {
            FMidPoint <<- NULL
            y <- range(out)
            End <- F
          } else {
            End <- T
          }
        }
      }
      RT <<- RT
      return(out)
    }
    
    LFunc <- function(y) {
      if (is.numeric(y)) {
        if (Trans) y <- FuncTR2(y)
        out <- sprintf(paste0('%.',max(0,RT),'f'),round(y,RT))
        while (sum(duplicated(out))>0) {
          RT  <- RT+1
          out <- sprintf(paste0('%.',max(0,RT),'f'),round(y,RT))
        }
        out[as.numeric(out)==0] <- '0'
        
      } else {
        if (is.Date(y)) y <- format(y,format=DateFormat)
        out <- y
      }
      
      # Add gaps to labels of continuous legends
      if (!is.null(LegLabsGap)) {
        LegLabsGap <- LegLabsGap + 1
        
        if (!is.null(FMidPoint) && LegSymmetr) {
          Temp <- MyRound(length(out)/2,0,Up=T)
          out[-c(seq(Temp,1,-LegLabsGap),seq(Temp+LegLabsGap,length(out),LegLabsGap))] <- ''
          
        } else {
          out[seq(ifelse(LegLabsStart==1,LegLabsStart+1,LegLabsStart),length(out),LegLabsGap)] <- ''
        }
      }
      RT <<- RT
      return(out)
    }
    
    B <- L <- NULL
    if ((Type%in%c('bin2d','hex') && !is.null(xFunc))) {
      Temp <- aggregate(DataS$TileColExtra,DataS[,c(Facets,xCol,yCol),with=F],xFunc,na.rm=T)
      if (is.null(RT)) RT <- Round_Bin_Digits(Temp$x,MidPoint=FMidPoint,IsInt=Is_Int(Temp$x),CalcRange=T)
      B <- unique(round(seq(min(Temp$x,na.rm=T),max(Temp$x,na.rm=T),length.out=LegNumBreaks+2),RT))
      L <- range(Temp$x,na.rm=T)
      BFunc <- BFunc(L)
      L <- CalcAxisRange(range(BFunc),0.005*!PrettyLeg)
      if (!is.null(FMidPoint) && !In_Interval(FMidPoint,range(BFunc),WithEqual=F)) FMidPoint <- range(L)[which.min(abs(FMidPoint-range(L)))]
      LFunc <- LFunc(BFunc)
      BFunc <- FuncTR1(as.numeric(LFunc))
      L <- range(as.numeric(BFunc))
      
    } else {
      if (is.null(LegBreaks) && is.null(LegLimits)) {
        LegLimits <- range(DataS[,get(ColX)],na.rm=T)
        BFunc <- BFunc(LegLimits)
        if (!is.null(LegBreaksAdd)) BFunc <- sort(c(BFunc,LegBreaksAdd))
        if (is.null(LegLabs)) {
          LFunc <- LFunc(BFunc) 
        } else {
          LFunc <- LegLabs
        }
        LegLimits <- range(BFunc,na.rm=T)
        
      } else { 
        if (!is.null(LegBreaks) && !is.null(LegLimits)) {
          if (is.null(LegLabs)) {
            LFunc <- LFunc(BFunc) 
          } else {
            LFunc <- LegLabs[data.table::between(LegBreaks,LegLimits[1],LegLimits[2],incbounds=T)]
          }
          BFunc <- LegBreaks[data.table::between(LegBreaks,LegLimits[1],LegLimits[2],incbounds=T)]
          
        } else if (!is.null(LegBreaks) && is.null(LegLimits)) { 
          LegLimits <- range(LegBreaks,na.rm=T)
          BFunc <- LegBreaks
          if (is.null(LegLabs)) {
            LFunc <- LFunc(BFunc) 
          } else {
            LFunc <- LegLabs
          }
          
        }  else if (is.null(LegBreaks) && !is.null(LegLimits)) { 
          BFunc <- BFunc(LegLimits)
          LegLimits <- range(BFunc)
          if (!is.null(LegBreaksAdd)) BFunc <- sort(c(BFunc,FuncTR1(LegBreaksAdd)))
          if (is.null(LegLabs)) {
            LFunc <- LFunc(BFunc) 
          } else {
            LFunc <- LegLabs
          }
        } 
      } 
      L <- LegLimits
      
      if (!is.null(FMidPoint) && !In_Interval(FMidPoint,range(BFunc),WithEqual=F)) FMidPoint <- range(L)[which.min(abs(FMidPoint-range(L)))]
    }
    
    if (is.null(LegNumBreaks)) BFunc <- LFunc <- waiver()
    
    ColFunc <- function(XColorsCont) {
      if (is.null(FMidPoint) || !In_Interval(FMidPoint,L,WithEqual=F)) {
        Num <- length(BFunc)
        if (is.null(XColorsCont)) XColorsCont <- c('blue3','dodgerblue','yellow','brown1','red4')
        
        if (PrettyLeg) {
          Times <- MyRound(ColorKeyBins*diff(BFunc)/as.numeric(diff(range(BFunc))),Digits=0,Up=T)
          XColorsCont <- rep(colorRampPalette(XColorsCont)(Num-1),times=Times)
          
        } else {
          XColorsCont <- colorRampPalette(XColorsCont)(ColorKeyBins)
        }
        
      } else {
        Num <- MyRound(length(BFunc)/2,Digits=0,Up=F)
        if (PrettyLeg) {
          if (is.null(XColorsCont)) XColorsCont <- MyColors$BuRd
          if (mod(length(XColorsCont),2)!=0) XColorsCont <- XColorsCont[-MyRound(length(XColorsCont)/2,0,Up=T)]
          Len <- length(XColorsCont)
          Mid <- MyRound(Len/2,0,Up=T)
          
          Times <- BFunc[BFunc<=FMidPoint]
          Times <- MyRound(0.5*ColorKeyBins*diff(Times)/diff(range(Times)),Digits=0,Up=T)
          
          XColorsContL <- rep(colorRampPalette(XColorsCont[seq(1,Mid)])(Num),times=Times)
          XColorsContR <- rep(colorRampPalette(XColorsCont[seq(Mid+1,Len)])(Num),times=rev(Times))
          XColorsCont <-  c(XColorsContL,XColorsContR)
          XColorsCont <-  c(XColorsContL,FMidColor,XColorsContR)
          
        } else if (is.null(XColorsCont)) {
          XColorsCont <- c(colorRampPalette(MyColors$BuRd[1:5])(ColorKeyBins),'ivory',colorRampPalette(MyColors$BuRd[8:11])(ColorKeyBins))
        }
      }
      if (RevGroups) XColorsCont <- rev(XColorsCont)
      return(XColorsCont)
    }
    
    if (!is.null(fCol)) {
      FColorsCont <- ColFunc(FColorsCont)
      if (is.null(FMidPoint)) {
        x <- x + scale_fill_gradientn(name=NULL,limits=L,breaks=BFunc,labels=LFunc,colours=FColorsCont,na.value=NaColor,trans=ContLegTransform)
        
      } else {
        Low <- FColorsCont[1:floor(length(FColorsCont)/2)]
        Mid <- FColorsCont[1+floor(length(FColorsCont)/2)]
        Hig <- FColorsCont[(floor(length(FColorsCont)/2)+1):length(FColorsCont)]
        if (!is.null(MidPointCol)) Mid <- MidPointCol
        x <- x + scale_fill_gradient2(name=NULL,low=Low,mid=Mid,high=Hig,midpoint=FMidPoint,limits=L,breaks=BFunc,labels=LFunc,na.value=NaColor,trans=ContLegTransform)
      }
      
    } else if (!is.null(cCol)) {
      CColorsCont <- ColFunc(CColorsCont)
      if (is.null(FMidPoint)) {
        x <- x + scale_color_gradientn(limits=L,breaks=BFunc,labels=LFunc,colours=CColorsCont,na.value=NaColor,trans=ContLegTransform)
        
      } else {
        Low <- CColorsCont[1:floor(length(CColorsCont)/2)]
        Mid <- CColorsCont[1+floor(length(CColorsCont)/2)]
        Hig <- CColorsCont[(floor(length(CColorsCont)/2)+1):length(CColorsCont)]
        if (!is.null(MidPointCol)) Mid <- MidPointCol
        x <- x + scale_color_gradient2(low=Low,mid=Mid,high=Hig,midpoint=FMidPoint,limits=L,breaks=BFunc,labels=LFunc,na.value=NaColor,trans=ContLegTransform)
      }
    }
  }
  
  # Adding color and fill scales, if needed.
  {
    if (((!is.null(cCol) && cCol %in% names(DataS)) || (Type=='errbar' && !is.null(eCol))) && !is.null(CColors) && !ContinLeg) {
      x <- x + scale_colour_manual(values=CColors,drop=LegDrop)
    }
    if (!is.null(fCol) && !is.null(FColors) && (fCol %in% names(DataS)) && !ContinLeg) {
      x <- x + scale_fill_manual(name=NULL,values=FColors,drop=LegDrop)
    }
  }
  
  # Adding a size scale, if needed.
  if (!is.null(sCol)) {
    if (is.numeric(DataS[,get(sCol)])) {
      if (is.null(RT)) RT <- Round_Bin_Digits(DataS[,get(sCol)],MidPoint=FMidPoint,IsInt=Is_Int(DataS[,get(sCol)]),CalcRange=T)
      B  <- unique(round(seq(min(DataS[,get(sCol)],na.rm=T),max(DataS[,get(sCol)],na.rm=T),length.out=LegNumBreaks+2),RT))
      
      if (!is.null(SizeLabs)) {
        if (length(SizeLabs)==1) B <- mean(DataS[,get(sCol)],na.rm=T)
      } else {
        SizeLabs <- B
      }
      x  <- x + scale_size_continuous(range=range(c(MinPSize,PSize)),breaks=B,labels=SizeLabs)
      
    } else {
      SizeLabs <- sort(unique(as.character(DataS[,get(sCol)])))
      sRange <- if (RevSizes) rev(range(c(MinPSize,PSize))) else range(c(MinPSize,PSize))
      x  <- x + scale_size_discrete(range=sRange,breaks=SizeLabs,labels=SizeLabs)
    }
  }
  
  # Adding legend guides, if needed.
  B <- NULL
  if (!is.null(LegPos)) {
    Guides <- waiver()
    
    if (!NoGuides) {
      CGuide <- FGuide <- ShGuide <- SGuide <- ltGuide <- 'none'
      if (is.null(GuideOrd)) GuideOrd <- data.frame(Color=1,Fill=2,Size=3,Shape=4,LType=5)
      if (!is.null(LegNRow) && length(LegNRow)==1) LegNRow <- data.frame(Color=1,Fill=1,Size=1,Shape=1,LType=1)*LegNRow
      if (!is.null(LegScale) && length(LegScale)==1) LegScale <- data.frame(Color=1,Fill=1,Size=1,Shape=1,LType=1)*LegScale
      
      LegOverrideF <- LegOverrideSh <- list(alpha=LegAlpha)
      LegOverRideC <- list(alpha=LegAlpha,size=(Type!='line')*PSize*LegScale$Color,lwd=Lwd*LegScale$Color)
      
      if (ForceLegScale) {
        LegOverrideF  <- c(LegOverrideF,size=PSize*LegScale$Fill,shape=PShape[1])
        LegOverrideSh <- c(LegOverrideSh,size=PSize*LegScale$Shape)
        
      } else {
        LegOverrideF  <- c(LegOverrideF,shape=PShape[1])
        LegOverrideSh <- c(LegOverrideSh)
      }
      
      if (Type=='tiles' || ContinLeg) {
        if (LegPos[1]=='right') {
          if (!is.null(cCol) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) 
            CGuide <- guide_colourbar(order=GuideOrd$Color,nbin=ColorKeyBins,raster=LegRaster,title=LegTitleC,barheight=LegLength,barwidth=LegWidth,ncol=LegNRow$Color,title.position=LegTitPos,title.hjust=LegTitHJust,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),label.position=LegLabPos,reverse=LegRev,draw.ulim=F,draw.llim=F,ticks.colour='black',ticks.linewidth=LegFrameWidth,frame.colour='black',frame.linetype=1,frame.linewidth=LegFrameWidth)
          if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_colourbar(order=GuideOrd$Fill,nbin=ColorKeyBins,raster=LegRaster,title=LegTitleF,barheight=LegLength,barwidth=LegWidth,ncol=LegNRow$Fill,title.position=LegTitPos,title.hjust=LegTitHJust,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),label.position=LegLabPos,reverse=LegRev,draw.ulim=F,draw.llim=F,ticks.colour='black',ticks.linewidth=LegFrameWidth,frame.colour='black',frame.linetype=1,frame.linewidth=LegFrameWidth)
          if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,title=LegTitleSh,barheight=LegLength,barwidth=LegWidth,ncol=LegNRow$Shape,title.position=LegTitPos,title.hjust=LegTitHJust,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),label.position=LegLabPos)
          if (!is.null(sCol) && !is.null(GuideOrd$Size))   SGuide <- guide_legend(order=GuideOrd$Size,title=LegTitleS,barheight=LegLength,barwidth=LegWidth,ncol=LegNRow$Size,title.position=LegTitPos,title.hjust=LegTitHJust,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),label.position=LegLabPos)
          Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide,size=SGuide)
          
        } else {
          if (!is.null(cCol) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) 
            CGuide <- guide_colourbar(order=GuideOrd$Color,nbin=ColorKeyBins,raster=LegRaster,title=LegTitleC,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barwidth=LegLength,barheight=LegWidth,nrow=LegNRow$Color,reverse=LegRev,label.position=LegLabPos,draw.ulim=F,draw.llim=F,ticks.colour='black',ticks.linewidth=LegFrameWidth,frame.colour='black',frame.linetype=1,frame.linewidth=LegFrameWidth)
          if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_colourbar(order=GuideOrd$Fill,nbin=ColorKeyBins,raster=LegRaster,title=LegTitleF,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barwidth=LegLength,barheight=LegWidth,nrow=LegNRow$Fill,reverse=LegRev,label.position=LegLabPos,draw.ulim=F,draw.llim=F,ticks.colour='black',ticks.linewidth=LegFrameWidth,frame.colour='black',frame.linetype=1,frame.linewidth=LegFrameWidth)
          if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,title=LegTitleSh,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barwidth=LegLength,barheight=LegWidth,nrow=LegNRow$Shape,reverse=LegRev,label.position=LegLabPos)
          if (!is.null(sCol) && !is.null(GuideOrd$Size))   SGuide <- guide_legend(order=GuideOrd$Size,title=LegTitleS,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barwidth=LegLength,barheight=LegWidth,nrow=LegNRow$Size,reverse=LegRev,label.position=LegLabPos)
          Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide,size=SGuide)
        }
        
      } else {
        if (LegPos[1]=='right') {
          if (is.null(sCol)) {
            if ((!is.null(cCol) || (Type=='errbar' && !is.null(eCol))) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) CGuide <- guide_legend(order=GuideOrd$Color,title=LegTitleC,scale=LegScale$Color,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverRideC)
            if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_legend(order=GuideOrd$Fill,title=LegTitleF,scale=LegScale$Fill,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverrideF)
            if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,title=LegTitleSh,scale=LegScale$Shape,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverrideSh)
            Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide)
            
          } else {
            if ((!is.null(cCol) || (Type=='errbar' && !is.null(eCol))) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) CGuide <- guide_legend(order=GuideOrd$Color,scale=LegScale$Color,title=LegTitleC,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverRideC)
            if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_legend(order=GuideOrd$Fill,scale=LegScale$Fill,title=LegTitleF,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverrideF)
            if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,scale=LegScale$Shape,title=LegTitleSh,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=LegOverrideSh)
            if (!is.null(sCol) && !is.null(GuideOrd$Size))   SGuide <- guide_legend(order=GuideOrd$Size,scale=LegScale$Size,title=LegTitleS,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,barheight=LegLength,barwidth=LegWidth,ncol=1,reverse=LegRev,label.position=LegLabPos,override.aes=list(alpha=LegAlpha))
            if (!is.null(ltCol) && !is.null(GuideOrd$LType)) ltGuide <- guide_legend(order=GuideOrd$LType,scale=LegScale$LType,title=LegTitleLt,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,ncol=1,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,override.aes=list(size=Lwd*LegScale$LType,alpha=LegAlpha,color=ifelse(is.null(cCol),CColors[1],'black')),label.position=LegLabPos)
            Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide,size=SGuide)
          }
          
        } else {
          # LegPos!='right'
          if (is.null(sCol)) {
            if ((!is.null(cCol) || (Type=='errbar' && !is.null(eCol))) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) CGuide <- guide_legend(override.aes=LegOverRideC,order=GuideOrd$Color,label.hjust=0,scale=LegScale$Color,title=LegTitleC,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Color,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos)
            if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_legend(order=GuideOrd$Fill,label.hjust=0,scale=LegScale$Fill,title=LegTitleF,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Fill,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=LegOverrideF)
            if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,label.hjust=0,scale=LegScale$Shape,title=LegTitleSh,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Shape,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,override.aes=LegOverrideSh,label.position=LegLabPos)
            if (!is.null(ltCol) && !is.null(GuideOrd$LType)) ltGuide <- guide_legend(order=GuideOrd$LType,label.hjust=0,scale=LegScale$LType,title=LegTitleLt,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$LType,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,override.aes=list(size=Lwd*LegScale$LType,alpha=LegAlpha,color=ifelse(is.null(cCol),CColors[1],'black')),label.position=LegLabPos)
            Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide,linetype=ltGuide)
            
          } else {
            if ((!is.null(cCol) || (Type=='errbar' && !is.null(eCol))) && !is.null(GuideOrd$Color) && !Type%in%c('boxmeanl','boxmeanp')) CGuide <- guide_legend(order=GuideOrd$Color,label.hjust=0,scale=LegScale$Color,title=LegTitleC,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Color,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=LegOverRideC)
            if (!is.null(fCol) && !is.null(GuideOrd$Fill))   FGuide <- guide_legend(order=GuideOrd$Fill,label.hjust=0,scale=LegScale$Fill,title=LegTitleF,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Fill,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=LegOverrideF)
            if (!is.null(shCol) && !is.null(GuideOrd$Shape)) ShGuide <- guide_legend(order=GuideOrd$Shape,label.hjust=0,title=LegTitleSh,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Shape,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=LegOverrideSh)
            if (!is.null(sCol) && !is.null(GuideOrd$Size))   SGuide <- guide_legend(order=GuideOrd$Size,label.hjust=0,title=LegTitleSh,scale=LegScale$Size,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$Size,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=list(alpha=LegAlpha))
            if (!is.null(ltCol) && !is.null(GuideOrd$LType)) ltGuide <- guide_legend(order=GuideOrd$LType,label.hjust=0,title=NULL,scale=LegScale$LType,title.theme=element_text(size=LegTitSize,angle=LegTitRot,color=TextCol,family=FontFamily),title.position=LegTitPos,title.hjust=LegTitHJust,nrow=LegNRow$LType,reverse=LegRev,barwidth=LegLength,barheight=LegWidth,label.position=LegLabPos,override.aes=list(size=Lwd*LegScale$LType,alpha=LegAlpha,color=ifelse(is.null(cCol),CColors[1],'black')))
            Guides <- guides(fill=FGuide,color=CGuide,shape=ShGuide,size=SGuide,linetype=ltGuide)
          }
        }
      }
      x <- x + Guides  
    }
  } 
  
  # Formatting axes' labels if they are coordinates.
  if (!is.null(AxisLabCoords)) {
    xFUNC <- function(xx) unlist(lapply(xx,function(yy){
      yy<-as.numeric(yy)
      ifelse(yy<0,paste0(yy,CoordSymbol,"W"),ifelse(yy>0,paste0(yy,CoordSymbol,"E"),yy))
    }))
    yFUNC <- function(xx) unlist(lapply(xx,function(yy){
      yy<-as.numeric(yy)
      ifelse(yy<0,paste0(yy,CoordSymbol,"S"),ifelse(yy>0,paste0(yy,CoordSymbol,"N"),yy))
    }))
    if (AxisLabCoords=='x') xLabels <- xFUNC
    if (AxisLabCoords=='y') yLabels <- yFUNC
    if (AxisLabCoords=='xy' || AxisLabCoords==T) {xLabels <- xFUNC; yLabels <- yFUNC}
  }
  
  # Adding labels to panels
  if (!is.null(Labels) && Labels!='none') {
    if (length(LabelsOffs)==1) LabelsOffs <- rep(LabelsOffs,2)
    
    if (is.character(LabelPos)) {
      
      xAxisRange2 <- xAxisRange
      yAxisRange2 <- yAxisRange
      
      if (LabelPos=='topleft') {
        LabelsX <- xAxisRange2[1]+diff(xAxisRange2)*LabelsOffs[1]
        LabelsY <- yAxisRange2[2]-diff(yAxisRange2)*LabelsOffs[2]
        if (is.null(LabelJust)) LabelJust <- c(0,1)
        
      } else if (LabelPos=='topright') {
        LabelsX <- xAxisRange2[2]-diff(xAxisRange2)*LabelsOffs[1]
        LabelsY <- yAxisRange2[2]-diff(yAxisRange2)*LabelsOffs[2]
        if (is.null(LabelJust)) LabelJust <- c(1,1)
        
      } else if (LabelPos=='bottomleft') {
        LabelsX <- xAxisRange2[1]+diff(xAxisRange2)*LabelsOffs[1]
        LabelsY <- yAxisRange2[1]+diff(yAxisRange2)*LabelsOffs[2]
        if (is.null(LabelJust)) LabelJust <- c(0,0)
        
      } else if (LabelPos=='bottomright') {
        LabelsX <- xAxisRange2[2]-diff(xAxisRange2)*LabelsOffs[1]
        LabelsY <- yAxisRange2[1]+diff(yAxisRange2)*LabelsOffs[2]
        if (is.null(LabelJust)) LabelJust <- c(1,0)
        
      } else if (LabelPos=='top') {
        LabelsX <- mean(xAxisRange2,na.rm=T)
        LabelsY <- yAxisRange2[2]-diff(yAxisRange2)*LabelsOffs[2]
        if (is.null(LabelJust)) LabelJust <- c(0.5,1)
      } 
      if (is.null(LabelsX) || length(LabelsX)==0) LabelsX <- 0.5
      if (is.null(LabelsY) || length(LabelsY)==0) LabelsY <- 0.5
      
    } else {
      if (is.null(dim(LabelPos))) {
        LabelsX <- LabelPos[1]
        LabelsY <- LabelPos[2]
      } else {
        LabelsX <- unlist(LabelPos[,1])
        LabelsY <- unlist(LabelPos[,2])
      }
      if (is.null(LabelJust)) LabelJust <- c(0,1)
    }
    
    if (length(Labels)==1 && Labels==T) Labels <- 'small'
    if (length(Labels)==1) {
      if (Labels=='cap') Labels <- LETTERS
      else if (Labels=='small') Labels <- letters
    }
    
    if (is.null(Facets)) i <- 1
    else i <- prod(apply(DataS[,Facets,with=F],2,function(x)length(unique(x[!is.na(x)]))))
    
    if (is.character(Margines)) {
      i <- i + prod(apply(DataS[,setdiff(Facets,Margines),with=F],2,function(x)length(unique(x[!is.na(x)]))))
    }
    if (Margines==TRUE) {
      i <- prod(apply(DataS[,Facets,with=F],2,function(x){1+length(unique(x[!is.na(x)]))}))
    }
    
    if (is.null(Facets)) {
      Temp <- DataS 
    } else {
      Temp <- unique(DataS[,Facets,with=F])
      setkeyv(Temp,cols=rev(Facets))
    }
    Labels <- cbind.data.frame(Temp,lab=Labels[1:nrow(Temp)],x=LabelsX,y=LabelsY)
    if (is.null(LabelsFont)) LabelsFont <- FontFamily
    
    x <- x + geom_text(data=Labels,mapping=aes(x=x,y=y,label=lab),hjust=LabelJust[1],vjust=LabelJust[2],
                       size=LabelsSize,family=LabelsFont,inherit.aes=F,fontface='bold')
  }
  
  # Theme settings
  
  if (!is.null(LegKeySize)) x <- x + theme(legend.key.size=unit(LegKeySize,'points'), 
                                           legend.text=element_text(size=LegKeyText, margin=LegKeyTextMarg, vjust=ifelse(ContinLeg && LegPos[1]=='top',1,0.5), 
                                                                    hjust=ifelse(LegPos[1]=='right',0,0.5)))
  if (!is.null(LegByRow)) x <- x + theme(legend.byrow=LegByRow)
  
  if (!is.null(LegPos))  {
    if (is.null(LegSpaceX)) {
      LegSpaceX <- unit(ifelse(LegPos[1]=='right',0.3,1.0),'cm')
    }
    if (is.null(LegSpaceY)) {
      LegSpaceY <- unit(ifelse(LegPos[1]=='right',0.15,0.1)*(LegsArrange!='vertical'),'cm')
    }
    if (is.null(LegKeySpaceX)) {
      LegKeySpaceX <- unit(LegKeyText,'pt')
    }
    if (is.null(LegKeySpaceY)) {
      LegKeySpaceY <- unit(LegKeyText*0.7,'pt')
    }
    
    x <- x + theme(legend.position=LegPos,legend.box=LegsArrange,legend.direction=LegDirection,
                   legend.justification=LegJust,legend.box.just=LegBoxJust,
                   legend.text.align=LegTxAlign,legend.box.spacing=LegBoxMarg,
                   legend.margin=LegMarg,legend.spacing.x=LegSpaceX,legend.spacing.y=LegSpaceY,
                   legend.key.spacing.x=LegKeySpaceX,legend.key.spacing.y=LegKeySpaceY,
                   legend.title=element_text(size=LegTitSize,vjust=0.5))
  }
  
  if (!is.null(AxTiSize)) x <- x + theme(axis.title.x=element_text(size=AxTiSize[1]),axis.title.y=element_text(size=AxTiSize[2]))
  
  if (length(RemAxesLabs)==1) RemAxesLabs <- rep(RemAxesLabs, 3)
  
  if (!is.null(AxTxSize)) { 
    if (RemAxesLabs[1]==F) 
      x <- x + theme(axis.text.x=element_text(angle=xAngle,size=AxTxSize[1],hjust=xAxTxJust[1],vjust=xAxTxJust[2],
                                              margin=margin(t=0.07,r=0,b=0,l=0,unit='cm')))
    if (RemAxesLabs[2]==F) 
      x <- x + theme(axis.text.y=element_text(angle=yAngle,size=AxTxSize[2],hjust=yAxTxJust[1],vjust=yAxTxJust[2],
                                              margin=margin(t=0,r=ifelse(yAngle==0,0.05,0.06),b=0,l=0,unit='cm')))
    if (RemAxesLabs[3]==F) 
      x <- x + theme(axis.text.y.right=element_text(angle=yAngle,size=AxTxSize[2],hjust=yAxTxJust[1],vjust=1-yAxTxJust[2],
                                                    margin=margin(t=0,l=ifelse(yAngle==0,0.05,0.06),b=0,r=0,unit='cm')))
  }
  
  if (length(PanSpace)==1) PanSpace <- rep(PanSpace, 2)
  if (RemPanMarg) PanSpace <- PanSpace * 0
  
  x <- x + theme(axis.text=element_text(color=ifelse(!is.null(AxTxCol),AxTxCol,TextCol)),axis.ticks.length=unit(0.07,'cm'),
                 axis.ticks=element_line(linewidth=max(PanLw*0.5,0.1),color=ifelse(!is.null(AxTxCol),AxTxCol,PanCol)),strip.placement=StripPlacement,
                 panel.spacing.x=PanSpace[1],panel.spacing.y=PanSpace[2],panel.border=element_rect(fill=NA,colour=PanCol,linewidth=PanLw))
  
  x <- x + theme(plot.title=element_text(color=TitleColors$Title,size=TitleSizes$Title),
                 plot.subtitle=element_text(color=TitleColors$SubTitle,size=TitleSizes$SubTitle),
                 plot.caption=element_text(color=TitleColors$Caption,size=TitleSizes$Caption),
                 plot.tag=element_text(color=TitleColors$Tag,size=TitleSizes$Tag))
  
  if (!is.null(FontFamily)) x <- x + theme(text=element_text(family=FontFamily), title=element_text(family=FontFamily), legend.title=element_text(family=FontFamily))
  if (!HVLineBack) x <- HVLFunc(x)
  
  if (is.null(LegBack)) 
    LegBack <- element_rect(fill=alpha('white',alpha=LegBackAlpha))
  else 
    if (is.character(LegBack)) LegBack <- element_rect(fill=alpha(LegBack,alpha=LegBackAlpha))
  
  x <- x + theme(panel.grid=element_line(linewidth=GridLwd,color=GridCol,linetype=GridLty))
  x <- x + theme(plot.margin=PlotMarg,legend.background=LegBack)
  
  if (is.null(xLabVJust)) xLabVJust <- ifelse(xAngle%in%c(0,90),0,0.4)
  if (is.null(yLabVJust)) yLabVJust <- 1
  if (!is.null(TextSize)) x <- x + theme(text=element_text(size=TextSize))
  
  x <- x + theme(axis.title.x=element_text(margin=xLabMarg,color=TextCol,vjust=xLabVJust,face=xLabFace))
  x <- x + theme(axis.title.y=element_text(margin=yLabMarg,color=TextCol,vjust=yLabVJust,face=yLabFace))
  x <- x + theme(axis.title.y.right=element_text(margin=yLabMarg[c(1,4,3,2)],color=TextCol,vjust=yLabVJust,face=yLabFace))
  
  x <- x + xlab(label=xLab) + ylab(label=yLab)
  
  if (!Flip) {
    if (is.null(xRange1) && (!is.numeric(DataS[,get(xCol)]) && !is.Date(DataS[,get(xCol)])) || (FacetScale%in%c('free_x','free') && is.null(Map))) xAxisRange <- NULL
    if (is.null(yRange1) && (!is.null(yCol) && !is.numeric(DataS[,get(yCol)]) && !is.Date(DataS[,get(yCol)])) || (FacetScale%in%c('free_y','free') && is.null(Map))) yAxisRange <- NULL
    
  } else {
    if (is.null(xRange1) && (!is.numeric(DataS[,get(xCol)]) && !is.Date(DataS[,get(xCol)])) || (FacetScale%in%c('free_y','free') && is.null(Map))) xAxisRange <- NULL
    if (is.null(yRange1) && (!is.null(yCol) && !is.numeric(DataS[,get(yCol)]) && !is.Date(DataS[,get(yCol)])) || (FacetScale%in%c('free_x','free') && is.null(Map))) yAxisRange <- NULL
  }
  
  # Adding axes' limits
  {
    if (is.null(Map)) {
      if (Flip) {
        x <- x + coord_flip(xlim=xAxisRange,ylim=yAxisRange,expand=ExpandAxes)
        
      } else {
        x <- x + coord_cartesian(xlim=xAxisRange,ylim=yAxisRange,expand=ExpandAxes)
      }
      
    } else {
      if (!is.null(xAxisRange) && !is.null(yAxisRange) && all(!is.na(c(xAxisRange,yAxisRange))))
        x <- x + coord_fixed(xlim=xAxisRange,ylim=yAxisRange)
      else
        x <- x + coord_fixed(expand=ExpandAxes)
    }
  } 
  
  # Adding axes' labels and breaks, if provided.
  {
    if (is.null(xBreaks)) xBreaks <- waiver()
    if (is.null(xLabels)) xLabels <- waiver()
    if (is.null(yBreaks)) yBreaks <- waiver()
    if (is.null(yLabels)) yLabels <- waiver()
    
    if (!XisDate && !is.factor(DataS[,get(xCol)]) && !is.character(DataS[,get(xCol)]) && !FacetScale%in%c('free','free_x')) x <- x + scale_x_continuous(breaks=xBreaks,labels=xLabels,sec.axis=SecXAxis,expand=c(0,0))
    if (!is.null(yCol) && !YisDate && !is.factor(DataS[,get(yCol)]) && !is.character(DataS[,get(yCol)]) && !FacetScale%in%c('free','free_y')) x <- x + scale_y_continuous(breaks=yBreaks,labels=yLabels,sec.axis=SecYAxis,expand=c(0,0))
    if (is.null(yCol) && Type=='ecdf' && !FacetScale%in%c('free','free_y') && class(yBreaks)!='waiver') x <- x + scale_y_continuous(breaks=yBreaks,labels=yLabels)
    
    if (!XisDate && (is.factor(DataS[,get(xCol)]) || is.character(DataS[,get(xCol)]))) x <- x + scale_x_discrete(breaks=xBreaks,labels=xLabels)
    if (!is.null(yCol) && !YisDate && (is.factor(DataS[,get(yCol)]) || is.character(DataS[,get(yCol)]))) x <- x + scale_y_discrete(breaks=yBreaks,labels=yLabels)
    
    if (XisDate) x <- x + scale_x_datetime(date_labels=DateFormat,breaks=xBreaks,labels=xLabels,expand=c(0,0))
    if (YisDate) x <- x + scale_y_datetime(date_labels=DateFormat,breaks=yBreaks,labels=yLabels,expand=c(0,0))
  }
  
  if (RemAxesTicks==TRUE || RemAxesTicks=='xy') x <- x + theme(axis.ticks=EB)
  if (RemAxesTicks=='x') x <- x + theme(axis.ticks.x=EB)
  if (RemAxesTicks=='y') x <- x + theme(axis.ticks.y=EB)
  if (RemGrids==TRUE || RemGrids=='xy') x <- x + theme(panel.grid.major=EB,panel.grid.minor=EB)
  if (RemGrids=='x') x <- x + theme(panel.grid.major.x=EB,panel.grid.minor.x=EB)
  if (RemGrids=='y') x <- x + theme(panel.grid.major.y=EB,panel.grid.minor.y=EB)
  if (RemMinGrids==TRUE || RemMinGrids=='xy') x <- x + theme(panel.grid.minor=EB)
  if (RemMinGrids=='x') x <- x + theme(panel.grid.minor.x=EB)
  if (RemMinGrids=='y') x <- x + theme(panel.grid.minor.y=EB)
  if (RemPanBorder) x <- x + theme(panel.border=EB,axis.line=element_line(colour=PanCol,linewidth=PanLw/2))
  if (RemAxesLines==TRUE || RemAxesLines=='xy') x <- x + theme(axis.line=EB,axis.title=EB,axis.text=EB)
  if (RemAxesLines=='x') x <- x + theme(axis.line.x=EB)
  if (RemAxesLines=='y') x <- x + theme(axis.line.y=EB)
  
  # Strips.
  x <- x + theme(strip.text=element_text(color=TextCol,face=StripFace))
  
  t <- if (is.null(StripMarg)) StripSizes[1]*2.5/100 else StripMarg[1]
  l <- if (is.null(StripMarg)) StripSizes[2]*2.5/100 else StripMarg[2]
  if (!is.null(StripSizes)) x <- x + theme(strip.text.x=element_text(size=StripSizes[1],vjust=0.5,hjust=0.5,color=TextCol,margin=margin(0,0,t,0,"cm")),
                                           strip.text.y=element_text(size=StripSizes[2],vjust=0.5,hjust=0.5,color=TextCol,margin=margin(0,0,0,l,"cm")))
  
  # if (RemPanMarg) StripMarg <- 0
  # if (!is.null(StripMarg)) x <- x + theme(strip.switch.pad.grid=if (class(StripMarg)=='unit') StripMarg else unit(as.numeric(StripMarg),'cm'))
  
  if (RemStripBack) {
    x <- x + theme(strip.background=EB,strip.text=element_text(vjust=0,hjust=0.5))
  } else {
    if (length(StripColor)==1) StripColor <- rep(StripColor,2)
    x <- x + theme(strip.background.x=element_rect(fill=StripColor[1],color=PanCol,linewidth=PanLw),
                   strip.background.y=element_rect(fill=StripColor[2],color=PanCol,linewidth=PanLw))
  }
  
  # Making the plot square.
  if (Rect) x <- x + theme(aspect.ratio=1)
  
  # Move panel and grid to the top
  if (PanelOnTop) x <- x + theme(panel.ontop=PanelOnTop, panel.background=element_rect(fill=NA))
  
  # Force axes to include AxesInclude.
  if (!is.null(AxesInclude)) x <- x + expand_limits(x=AxesInclude[1], y=AxesInclude[2])
  
  # Add layers in front.
  if (!is.null(AddLayersFront)) x <- Add_gg_Layers(Base=x,Layers=AddLayersFront)
  
  # Adding inset graphs
  if (!is.null(InsetGraphs)) {
    if (is.ggplot(InsetGraphs)) InsetGraphs <- list(InsetGraphs)
    if (!is.list(InsetPos)) InsetPos <- list(pos=InsetPos)
    x <- ggdraw() + draw_plot(x)
    for (i in 1:length(InsetGraphs)) {
      InsG <- InsetGraphs[[i]]
      x <- x + draw_plot(InsG,x=InsetPos[[i]][1],y=InsetPos[[i]][2],width=InsetPos[[i]][3],height=InsetPos[[i]][4])
    }
  }
  
  # Saving the plot
  if (!is.null(ExpPath)) {
    if (!is.null(class(Device)) || Device!='pdf') { 
      ggplot2::ggsave(filename=ExpPath,plot=x,device=Device,path=NULL,scale=Scale,
                      width=Size[1],height=Size[2],units=c("cm"),dpi=Dpi,limitsize=F)
    } else {
      ggplot2::ggsave(filename=ExpPath,plot=x,device=Device,path=NULL,scale=Scale,
                      width=Size[1],height=Size[2],units=c("cm"),dpi=Dpi,limitsize=F,paper=Paper)
    }
    
    if (!is.null(Note)) {
      writeLines(Note, con=paste0(ExpPath,'.txt'))
    }
  } 
  
  graphics.off()
  return(x)
}

#========================================================================================================
#========================================================================================================

Facet_Labels <- function(labels) {
  labels <- as.character(labels)
  labels[labels=='(all)'] <- 'All'
  return (labels)
}

is.Facet <- function(Facet,Data=NULL,CheckFacets=T) {
  if (!is.null(Data)) Data <- as.data.table(Data)
  if (is.null(Data) && !is.null(Facet) && Facet!='.') return(T)
  else if (!is.null(Data) && !is.null(Facet) && Facet!='.' && (ifelse(CheckFacets,length(unique(as.character(Data[,Facet,with=F][[1]])))>1,T))) return(T)
  else return(F)
}

CalcAxisRange <- function(Range,Offset) {
  return(Range+c(-Offset,Offset)*base::diff(Range))
}

RemOutliers_for_Facets <- function(Data,Facets,xCol,yCol) {
  Data <- as.data.table(Data)
  if (is.null(Facets)) {
    Factors <- Data[,get(xCol)]
  } else {
    Factors <- apply(Data[,c(xCol,Facets),with=F],1,paste,collapse="-")
  }
  
  Levels <- unique(Factors)
  for (Lev in Levels) {
    Rows <- which(Factors==Lev)
    if (length(Rows)>0) Data[Rows,yCol,with=F] <- NaOutliers(Data[Rows,yCol,with=F])
  }
  
  return(Data)
}

#========================================================================================================
#========================================================================================================

Plot_Australia_Base_Map <- function(Plot=F,Type=1,AsList=F,JustLoad=T,NArrowPos=c(x=128.5,y=-34),BackFill='grey60',CroppingFill='grey70',
                                    StNamesPos=NULL,StNamesCol=NULL,StatFill=NULL,StatLwd=0.1,StatTx=5,
                                    ArNamesPos=NULL,ArNamesCol=NULL,Alpha=1,AreaTx=3,UseNorthEast=T,FontFamily='serif',OzLwd=0.15,
                                    MainDir='C:/Users/uqbababa/UQ') {
  
  # Type=1 -> regions with filling color based on Area, but with no border
  # Type=2 -> no filling color, without regions
  # Type=3 -> no filling color, cropping area in grey, without regions
  # Type=4 -> no filling color, without regions and cropping area
  
  File <- paste0(MainDir,'/02_Data/Australia Base Maps.RData')
  
  if (JustLoad) {
    base::load(file=File)
    
  } else {
    setwd("C:/Users/uqbababa/OneDrive - The University of Queensland/00_Projects/Archive/map graph_R code_chenu2013")
    
    # australia map
    # ozLines <- fortify(readShapeSpatial(fn="./0.input map shp files/Australia.shp"))
    ozLines <- fortify(readOGR(dsn="./0.input map shp files",layer="Australia",verbose=F))
    
    ##states
    # states <- fortify(readShapeSpatial(fn="./0.input map shp files/states.shp"))
    states <- fortify(readOGR(dsn="./0.input map shp files",layer="states",verbose=F))
    
    ##cropping map
    # cropping <- fortify(readShapeSpatial(fn="./0.input map shp files/Australia_wheat_cropping adjusted for Victoria and Walgett.shp"))
    cropping <- fortify(readOGR(dsn="./0.input map shp files",verbose=F,
                                layer="Australia_wheat_cropping adjusted for Victoria and Walgett"))
    
    ## wheat region area
    # region.poly  <- readShapeSpatial(fn="./0.input map shp files/wheat_MyRegion_1975to2006_SLA2000modified.shp")
    region.poly    <- readOGR(dsn="./0.input map shp files",layer="wheat_MyRegion_1975to2006_SLA2000modified",verbose=F)
    region.col.tab <- read.table("region-map-characteristics-1b.csv",sep=",",header=T)
    region.col.tab <- region.col.tab[region.col.tab$region!='blank',]
    region.poly@data$id <- rownames(region.poly@data)
    region.col.tab <- merge(region.col.tab,region.poly@data[,c('Australia_','id')],by.x='region2',by.y='Australia_')
    region.col.tab$id <- as.numeric(region.col.tab$id)
    region.poly <- fortify(region.poly)
    region.poly$id <- as.numeric(region.poly$id)
    region.poly <- merge(region.poly,region.col.tab,by='id')
    
    save(ozLines,states,cropping,region.poly,region.col.tab,region.col.tab, file=File)
  }
  
  Layers <- list()
  if (Type==1) {
    if (is.null(StatFill)) StatFill <- 'transparent'
    Layers[['cropping']] <- geom_polygon(data=cropping,aes(x=long,y=lat,group=group),fill='white',color=BackFill,lwd=0.05)
    Layers[['region']] <- geom_polygon(data=region.poly,aes(x=long,y=lat,group=group,fill=area),color=NA,alpha=Alpha)
    Layers[['states']] <- geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=StatFill,color=BackFill,lwd=StatLwd)
    
  } else if (Type==2) {
    if (is.null(StatFill)) StatFill <- 'transparent'
    Layers[['cropping']] <- geom_polygon(data=cropping,aes(x=long,y=lat,group=group),fill='white',color=BackFill,lwd=0.05)
    Layers[['states']] <- geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=StatFill,color=BackFill,lwd=StatLwd)
    
  } else if (Type==3) {
    if (is.null(StatFill)) StatFill <- 'transparent'
    Layers[['cropping']] <- geom_polygon(data=cropping,aes(x=long,y=lat,group=group),fill=CroppingFill,color=NA)
    Layers[['states']] <- geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=StatFill,color=BackFill,lwd=StatLwd)
    
  } else if (Type==4) {
    if (is.null(StatFill)) StatFill <- 'white'
    Layers[['states']] <- geom_polygon(data=states,aes(x=long,y=lat,group=group),fill=StatFill,color=BackFill,lwd=StatLwd)
  }
  Layers[['australia']] <- geom_polygon(data=ozLines,aes(x=long,y=lat,group=group),fill='transparent',color='black',lwd=OzLwd)
  if (!is.null(NArrowPos)) Layers[['north']] <- terra::north(ozLines,symbol=2,scale=0.07,location='topleft',anchor=NArrowPos)
  Layers[['theme1']] <- theme(legend.position='none',legend.title=element_blank())
  Layers[['theme2']] <- theme(title=element_blank(),axis.title=element_blank(),axis.text=element_blank(),
                              axis.line=element_blank(),axis.ticks=element_blank())
  
  if (!is.null(StNamesCol) && StatTx>0) {
    if (is.null(StNamesPos)) {
      StNamesPos <- data.frame(x=c(143.2, 143.2, 146, 134, 123, 134, 146.5),
                               y=c(-22, -31.2, -37.5, -28, -26, -22, -42),
                               label=c('QLD', 'NSW', 'VIC', 'SA', 'WA', 'NT', 'TAS'))
    }
    Layers[['StatTx']] <- geom_text(data=StNamesPos,mapping=aes(x=x,y=y,label=label),color=StNamesCol,size=StatTx,inherit.aes=F,show.legend=F,family=FontFamily)
  }
  
  if (!is.null(ArNamesCol) && AreaTx>0) {
    if (is.null(ArNamesPos)) {
      ArNamesPos <- data.frame(x=c(146-UseNorthEast*1.4, 139, 136.5, 121.5),
                               y=c(-27.5, -39.2, -31.3, -30.5),
                               label=c(ifelse(UseNorthEast,'North-East','East'), 'South-East', 'South', 'West'))
    }
    Layers[[length(Layers)+1]] <- geom_text(data=ArNamesPos,mapping=aes(x=x,y=y,label=label),color=ArNamesCol,size=AreaTx,inherit.aes=F,show.legend=F,family=FontFamily)
  }
  Layers[['AreaTx']] <- theme(text=element_text(family=FontFamily), title=element_text(family=FontFamily), legend.title=element_text(family=FontFamily))
  
  if (AsList) {
    AusPlot <- Layers
  } else {
    AusPlot <- Add_gg_Layers(Base=NULL,Layers=Layers)
  }
  if (Plot && !AsList) print(AusPlot,newpage=F)
  return(AusPlot)
}

#========================================================================================================
#========================================================================================================

Plot_Grid <- function(PlotList,LegPos='none',RemTitles=F,RemLabs=T,RemMargs=F,RemAxText=F,Align='hv',
                      Widths=1,Heights=1,NRow=NULL,NCol=NULL,LabelXY=c(0.02,1),LabelSize=12,Labels=NULL,PlotsMarg=NULL,PlotsPad=NULL,
                      Title=NULL,SubTitle=NULL,CommLegend=F,xLab=NULL,yLab=NULL,AxTiSize=10,xVJust=0.5,yVJust=0.1,
                      TopMarg=0,Size=c(50,50),Scale=1,Paper='special',Dpi=300,Device='pdf',FontFamily='serif',ExpPath=NULL,Note=NULL) {
  Plots <- PlotList
  
  if (is.null(NRow)) NRow <- ceiling(length(Plots)/ifelse(is.null(NCol),1,NCol))
  if (is.null(NCol)) NCol <- 1
  
  if (RemLabs==T)   RemLabs   <- 'xy'
  if (RemAxText==T) RemAxText <- 'xy'
  if (RemLabs   %in% c('x','xy')) Plots <- lapply(Plots,function(x){x<-x+rremove('xlab')})
  if (RemLabs   %in% c('y','xy')) Plots <- lapply(Plots,function(x){x<-x+rremove('ylab')})
  if (RemAxText %in% c('x','xy')) Plots <- lapply(Plots,function(x){x<-x+rremove('x.text')})
  if (RemAxText %in% c('y','xy')) Plots <- lapply(Plots,function(x){x<-x+rremove('y.text')})
  
  if (RemTitles) Plots <- lapply(Plots,function(x){x<-x+ggtitle(label=NULL,subtitle=NULL)})
  if (!is.null(Title) || !is.null(SubTitle)) Plots[[1]] <- Plots[[1]] + ggtitle(label=Title,subtitle=SubTitle)
  
  if (RemMargs) Plots <- lapply(Plots,function(x){x<-x+theme(plot.margin=unit(c(0.01,0.1,0.01,0.01),'cm'))})
  if (!is.null(PlotsMarg)) Plots <- lapply(Plots,function(x){x<-x+theme(plot.margin=PlotsMarg)})
  
  if (!is.null(PlotsPad) & NCol==1) {
    for (i in 1:(length(Plots)-1)) {
      Temp <- Plots[[i]]$theme$plot.margin
      Temp[3] <- PlotsPad
      Temp <- margin(Temp[1],Temp[2],Temp[3],Temp[4],unit=attributes(Plots[[i]]$theme$plot.margin)$unit)
      Plots[[i]] <- Plots[[i]] + theme(plot.margin=Temp)
    }
  }
  
  #if (NRow==1) Align <- 'h'
  
  Plot <- ggarrange(plotlist=Plots,nrow=NRow,ncol=NCol,common.legend=CommLegend,hjust=0.0,vjust=1.0,
                    legend=LegPos,labels=Labels,label.x=LabelXY[1],label.y=LabelXY[2],align=Align,
                    widths=Widths,heights=Heights,font.label=list(family=FontFamily,size=LabelSize,face='bold')) 
  
  if (!is.null(xLab))
    Plot <- annotate_figure(Plot,bottom=text_grob(xLab,hjust=0.5,vjust=xVJust,rot=0,family=FontFamily,size=AxTiSize))
  if (!is.null(yLab))
    Plot <- annotate_figure(Plot,left=text_grob(yLab,hjust=0.5,vjust=yVJust,rot=90,family=FontFamily,size=AxTiSize))
  
  if (TopMarg>0) Plot <- annotate_figure(Plot,top=text_grob('',hjust=0.5,vjust=0.2,rot=0,family=FontFamily,size=unit(TopMarg,'cm')))
  
  if (!is.null(ExpPath)) {
    x <- ggplot2::ggsave(filename=ExpPath,plot=Plot,device=Device,path=NULL,scale=Scale,width=Size[1],height=Size[2],units=c('cm'),dpi=Dpi,limitsize=F)
    
    if (!is.null(Note)) {
      writeLines(Note, con=paste0(ExpPath,'.txt'))
    }
  }
  return(Plot)
}
Plot_Together_Vert <- Plots_Together <- Plot_Grid

#========================================================================================================
#========================================================================================================

Set_Size <- function(BinWidth=0.4,RowWidth=8,BinNum=10,RowNum=2,ColNum=5,HeightBuff=0.3,toInch=F) {
  Width  <- BinWidth*BinNum*ColNum
  Height <- RowWidth*(RowNum+HeightBuff)
  Size   <- c(Width,Height)
  if (toInch) Size <- Size/2.54
  return(Size)
}

#========================================================================================================
#========================================================================================================

Add_gg_Layers <- function(Base=NULL,Layers) {
  if (is.null(Base)) {
    Base <- ggplot()
  }
  if (!is.list(Layers)) {
    Layers <- list(Layers)
  }
  for (Layer in Layers) {
    Base <- Base + Layer
  }
  return(Base)
}

#========================================================================================================
#========================================================================================================

# Plot_Australia_Karine <- function(ExportShapes=T) {
#   setwd("C:/Users/uqbababa/UQ/01_RScripts/(2017.12.07) Karine's Code for Maps and Graphs/0.input map shp files")
#   
#   # australia map
#   ozLines <- readShapeLines("Australia.shp")
#   ozLines.sp = list("sp.lines", ozLines[1],col="black")
#   ozBox <- bbox(ozLines)
#   
#   ##states
#   states<-readShapePoly("states.shp")
#   states.sp = list("sp.polygons", states[1],col="white",fill="azure2")       #, label=as.character(states[1]$STATE)
#   states.label.sp=list("sp.text", coordinates(states[1]),col="grey43", as.character(states[1]$STATE),cex=0.8)
#   
#   ##cropping map
#   cropping<-readShapePoly("Australia_wheat_cropping adjusted for Victoria and Walgett.shp")
#   cropping.sp<-list("sp.polygons",cropping[1],fill=1:10,col="grey60")     #"azure3"
#   
#   ## wheat region area
#   region.poly<-readShapePoly("wheat_MyRegion_1975to2006_SLA2000modified.shp")
#   
#   ## parameters for the north arpow and the scale
#   arpow<-list("SpatialPolygonsRescale", layout.north.arpow(), offset = c(147.5,-17), scale = 3)
#   scale<-list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(113,-16),scale = 4.7, fill=c("transparent","black"))  #offset = c(115,-15),scale = 4.7,   between 115,-35 and 119.5,-39: 500km
#   text1<-list("sp.text", c(113.3,-17), "0",cex=0.6)         #ATTENTION ADJUST SCALE DEPENDING ON WHERE PLACED!!!!   #between 120,-39 and 125.8,-39: 500km
#   text2<-list("sp.text", c(118.5,-17), "500 km",cex=0.6)    #to calculate distance between 2 points (http://www.movable-type.co.uk/scripts/latlong.html)
#   
#   ##      Oz map + wheat regions
#   xPlot <- spplot(cropping[1], as.table = TRUE,
#                   xlim = c(ozBox[1,1] - 18, ozBox[1,2] + 13),
#                   ylim = c(ozBox[2,1] -15.5, ozBox[2,2] +14.5),
#                   sp.layout = list(states.sp,region.poly,cropping.sp,ozLines.sp,arpow,scale,text1,text2), # the order is important, draw element on top of each other
#                   col.regions = "bisque",col = "bisque",colorkey = FALSE,
#                   par.settings = list(axis.line = list(col = 'transparent')))
#   
#   Shapes <- list(Country=ozLines,States=states,Regions=region.poly,Arpow=arpow,Scale=scale,Text1=text1,Text2=text2)
#   
#   if (ExportShapes) xPlot <- list(xPlot=xPlot,Shapes=Shapes)
#   return(xPlot)
# }

#========================================================================================================
#========================================================================================================

# Plot_Australia_Base_Map_Karine <- function(Plot=F) {
#   
#   setwd("C:/Users/uqbababa/UQ/01_RScripts/(2017.12.07) Karine's Code for Maps and Graphs")
#   # australia map
#   ozLines <- readShapeLines("./0.input map shp files/Australia.shp")
#   ozLines.sp=list("sp.lines", ozLines[1],col="black")
#   ozBox <- bbox(ozLines)
#   
#   ##states
#   states<-readShapePoly("./0.input map shp files/states.shp")
#   states.sp=list("sp.polygons", states[1],col="azure4",fill="azure2")       #, label=as.character(states[1]$STATE)
#   states.label.sp=list("sp.text", coordinates(states[1]),col="grey43", as.character(states[1]$STATE),cex=0.8)
#   ##cropping map
#   cropping<-readShapePoly("./0.input map shp files/Australia_wheat_cropping adjusted for Victoria and Walgett.shp")
#   cropping.sp<-list("sp.polygons",cropping[1],fill=NA,col="grey60")     #"azure3"
#   croppingBox <- bbox(cropping)
#   
#   ## wheat region area
#   ##NB: I had to rename wheat_MyRegion_1975to2006_SLA2000modified_dissolve_sum area_2a_ dissolve with wheat_croppingAdjustVictoriaWalgett_IntersectWithSLA2000modifiedForMildura_sum area.shp"
#   ## as the file name was too long
#   region.poly<-readShapePoly("./0.input map shp files/wheat_MyRegion_1975to2006_SLA2000modified.shp")
#   region.name<-levels(region.poly[[1]][[2]][[1]])      #name from the shp file
#   region.col.tab<-read.table("region-map-characteristics-1b.csv",sep=",",header=T)
#   region.col.tab<-region.col.tab[!is.na(region.col.tab$region2)&region.col.tab$region2!="blank",]
#   region.col.ordered.tab<-region.col.tab[order(region.col.tab$region2),]
#   region.col.ordered.tab$rgb<-rgb(region.col.ordered.tab$color.R/255,region.col.ordered.tab$color.G/255,region.col.ordered.tab$color.B/255)
#   #region.col.ordered-gsub("\"","",region.col.ordered.tab$color)
#   #region.col.ordered<- gsub("\"","",region.col.ordered.tab$color)
#   region.col.ordered<-region.col.ordered.tab$rgb
#   region.sp=list("sp.polygons", region.poly[1],fill=region.col.ordered,col=NA)
#   
#   ## parameters for the north arpow and the scale
#   arpow<- list("SpatialPolygonsRescale", layout.north.arpow(), offset=c(147.5,-17), scale=3) #offset=c(150,-18)
#   scale<-list("SpatialPolygonsRescale", layout.scale.bar(), offset=c(113,-16),scale=4.7, fill=c("transparent","black"))  #offset=c(115,-15),scale=4.7,   between 115,-35 and 119.5,-39: 500km
#   # offset=c(125,-36),scale=5.55
#   text1<-list("sp.text", c(113.3,-17), "0",cex=0.6)         #ATTENTION ADJUST SCALE DEPENDING ON WHERE PLACED!!!!   #between 120,-39 and 125.8,-39: 500km
#   text2<-list("sp.text", c(118.5,-17), "500 km",cex=0.6)    #to calculate distance between 2 points (http://www.movable-type.co.uk/scripts/latlong.html)
#   
#   ###      Oz map + cropping area
#   if(F)
#     print(spplot(cropping[1], as.table=TRUE,
#                  xlim=c(ozBox[1,1] - 18, ozBox[1,2] + 13),                 #-0.5   +3.5    #adjusted to fit with pushviewport of "RGraphics_margin adjusted-2a.r"
#                  ylim=c(ozBox[2,1] -15.5, ozBox[2,2] +14.5),                       #+1     +0
#                  sp.layout=list(states.sp,cropping.sp,states.label.sp,ozLines.sp,arpow,scale,text1,text2),      #   the order is important, draw element on top of each other
#                  col.regions="bisque",col="bisque",colorkey=FALSE,
#                  par.settings=   list(axis.line=list(col= 'transparent'))))
#   
#   
#   ###      Oz map + wheat regions
#   AusPlot <- spplot(cropping[1],as.table=TRUE,
#                     xlim=c(ozBox[1,1] - 18, ozBox[1,2] + 13),                 #-0.5   +3.5    #adjusted to fit with pushviewport of "RGraphics_margin adjusted-2a.r"
#                     ylim=c(ozBox[2,1] -15.5, ozBox[2,2] +14.5),                       #+1     +0
#                     sp.layout=list(states.sp,region.sp,cropping.sp,ozLines.sp,arpow,scale,text1,text2),      #   the order is important, draw element on top of each other
#                     col.regions=NA,col=NA,colorkey=FALSE,
#                     par.settings=list(axis.line=list(col='transparent')))
#   
#   if (Plot) print(AusPlot,newpage=F)
#   return(AusPlot)
# }

#========================================================================================================
#========================================================================================================

PPSize <- function(H=1,W=1,R=F,Marg=3,Cap=3) {
  if (!R) Size <- c(W*(21-Marg),H*(30-Marg-Cap))
  if ( R) Size <- c(W*(30-Marg),H*(21-Marg-Cap))
  return(Size)
}

#========================================================================================================
#========================================================================================================

Print_Plots_to_File <- function(PList,File,Paper='special',Size=NULL,Width=20,Height=30,Device='pdf',Note=NULL) {
  graphics.off()
  if (!is.null(Size)) {Width <- Size[1]; Height <- Size[2]}
  if (Device=='pdf') pdf(file=File,width=Width/2.54,height=Height/2.54,onefile=T)
  if (Device!='pdf') cairo_pdf(file=File,width=Width/2.54,height=Height/2.54,onefile=T)
  for (P in PList) plot(P)
  x <- dev.off()
  graphics.off()
  
  if (!is.null(Note) && !is.null(File)) {
    writeLines(Note, con=paste0(File,'.txt'))
  }
  return(NULL)
}

#========================================================================================================
#========================================================================================================

# CreateRadialPlot <- function(plot.data,
#                              axis.labels=colnames(plot.data)[-1],                             
#                              grid.min=-0.5,  #10,
#                              grid.mid=0,  #50,
#                              grid.max=0.5,  #100,
#                              centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
#                              plot.extent.x.sf=1.2,
#                              plot.extent.y.sf=1.2,
#                              x.centre.range=0.02*(grid.max-centre.y),
#                              label.centre.y=FALSE,
#                              grid.line.width=0.5,
#                              gridline.min.linetype="longdash",
#                              gridline.mid.linetype="longdash",
#                              gridline.max.linetype="longdash",
#                              gridline.min.colour="grey",
#                              gridline.mid.colour="blue",
#                              gridline.max.colour="grey",
#                              grid.label.size=4,
#                              gridline.label.offset=-0.02*(grid.max-centre.y),
#                              label.gridline.min=TRUE,
#                              axis.label.offset=1.15,
#                              axis.label.size=3,
#                              axis.line.colour="grey",
#                              group.line.width=1,
#                              group.point.size=4,
#                              background.circle.colour="yellow",
#                              background.circle.transparency=0.2,
#                              plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
#                              legend.title="Cluster",
#                              legend.text.size=grid.label.size ) {
#   
#   var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
#   #axis.labels [if supplied] is designed to hold 'long version' of variable names
#   #with line-breaks indicated using \n
#   
#   #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
#   plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
#   plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
#   
#   #Check supplied data makes sense
#   if (length(axis.labels) != ncol(plot.data)-1) 
#     return("Error: 'axis.labels' contains the wrong number of axis labels") 
#   if(min(plot.data[,-1])<centre.y)
#     return("Error: plot.data' contains value(s) < centre.y")
#   if(max(plot.data[,-1])>grid.max)
#     return("Error: 'plot.data' contains value(s) > grid.max")
#   
#   #Declare required internal functions
#   
#   CalculateGroupPath <- function(df) {
#     #Converts variable values into a set of radial x-y coordinates
#     #Code adapted from a solution posted by Tony M to
#     #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
#     
#     #Args:
#     #  df: Col 1 -  group ('unique' cluster / group ID of entity)
#     #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
#     
#     path <- as.factor(as.character(df[,1]))
#     
#     ##find increment
#     angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
#     
#     ##create graph data frame
#     graphData= data.frame(seg="", x=0,y=0)
#     graphData=graphData[-1,]
#     
#     for(i in levels(path)){
#       
#       pathData = subset(df, df[,1]==i)
#       
#       for(j in c(2:ncol(df))){
#         
#         #pathData[,j]= pathData[,j]
#         
#         graphData=rbind(graphData, data.frame(group=i, 
#                                               x=pathData[,j]*sin(angles[j-1]),
#                                               y=pathData[,j]*cos(angles[j-1])))
#       }
#       ##complete the path by repeating first pair of coords in the path
#       graphData=rbind(graphData, data.frame(group=i, 
#                                             x=pathData[,2]*sin(angles[1]),
#                                             y=pathData[,2]*cos(angles[1])))
#       
#     }
#     
#     #Make sure that name of first column matches that of input data (in case !="group")
#     colnames(graphData)[1] <- colnames(df)[1]
#     
#     graphData #data frame returned by function
#     
#   }
#   
#   CaclulateAxisPath = function(var.names,min,max) {
#     #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
#     
#     #Args:
#     #var.names - list of variables to be plotted on radar plot
#     #min - MININUM value required for the plotted axes (same value will be applied to all axes)
#     #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
#     
#     #var.names <- c("v1","v2","v3","v4","v5")
#     n.vars <- length(var.names) # number of vars (axes) required
#     
#     #Cacluate required number of angles (in radians)
#     angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
#     
#     #calculate vectors of min and max x+y coords
#     min.x <- min*sin(angles)
#     min.y <- min*cos(angles)
#     max.x <- max*sin(angles)
#     max.y <- max*cos(angles)
#     
#     #Combine into a set of uniquely numbered paths (one per variable)
#     axisData <- NULL
#     for (i in 1:n.vars) {
#       a <- c(i,min.x[i],min.y[i])
#       b <- c(i,max.x[i],max.y[i])
#       axisData <- rbind(axisData,a,b)
#     }
#     
#     #Add column names + set row names = row no. to allow conversion into a data frame
#     colnames(axisData) <- c("axis.no","x","y")
#     rownames(axisData) <- seq(1:nrow(axisData))
#     
#     #Return calculated axis paths
#     as.data.frame(axisData)
#   }
#   
#   
#   funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
#     #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
#     tt <- seq(0,2*pi,length.out = npoints)
#     xx <- center[1] + r * cos(tt)
#     yy <- center[2] + r * sin(tt)
#     return(data.frame(x = xx, y = yy))
#   }
#   
#   ### Convert supplied data into plottable format
#   
#   # (a) add abs(centre.y) to supplied plot data 
#   #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
#   # in user-supplied data]
#   plot.data.offset <- plot.data
#   plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
#   #print(plot.data.offset)
#   
#   # (b) convert into radial coords
#   group <-NULL
#   group$path <- CalculateGroupPath(plot.data.offset)
#   #print(group$path)
#   
#   # (c) Calculate coordinates required to plot radial variable axes
#   axis <- NULL
#   axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
#   #print(axis$path)
#   
#   # (d) Create file containing axis labels + associated plotting coordinates
#   
#   #Labels
#   axis$label <- data.frame(
#     text=axis.labels,
#     x=NA,
#     y=NA )
#   #print(axis$label)
#   
#   #axis label coordinates
#   n.vars <- length(var.names)
#   angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
#   axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
#   axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
#   #print(axis$label)
#   
#   # (e) Create Circular grid-lines + labels
#   
#   #caclulate the cooridinates required to plot circular grid-lines for three user-specified
#   #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
#   gridline <- NULL
#   gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
#   gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
#   gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
#   #print(head(gridline$max$path))
#   
#   #gridline labels
#   gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
#                                    text=as.character(grid.min))
#   gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
#                                    text=as.character(grid.max))
#   gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
#                                    text=as.character(grid.mid))
#   #print(gridline$min$label)
#   #print(gridline$max$label)
#   #print(gridline$mid$label)
#   
#   
#   ### Start building up the radar plot
#   
#   # Delcare 'theme_clear', with or without a plot legend as required by user
#   #[default = no legend if only 1 group [path] being plotted]
#   theme_clear <- theme_bw() + 
#     theme(axis.text.y=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks=element_blank(),
#           panel.grid.major=element_blank(),
#           panel.grid.minor=element_blank(),
#           panel.border=element_blank(),
#           legend.key=element_rect(linetype="blank"))
#   
#   if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
#   
#   #Base-layer = axis labels + plot extent
#   # [need to declare plot extent as well, since the axis labels don't always
#   # fit within the plot area automatically calculated by ggplot, even if all
#   # included in first plot; and in any case the strategy followed here is to first
#   # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
#   # then centred labels for axis labels almost immediately above/below x= 0 
#   # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
#   # This building up the plot in layers doesn't allow ggplot to correctly 
#   # identify plot extent when plotting first (base) layer]
#   
#   #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
#   base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
#     geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
#               aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
#     scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
#     scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
#   
#   # + axis labels for any vertical axes [abs(x)<=x.centre.range]
#   base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
#                            aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
#   
#   # + axis labels for any vertical axes [x>x.centre.range]
#   base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
#                            aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
#   
#   # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
#   base <- base + theme_clear
#   
#   #  + background circle against which to plot radar data
#   base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
#                               fill=background.circle.colour,
#                               alpha=background.circle.transparency)
#   
#   # + radial axes
#   base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
#                            colour=axis.line.colour)
#   
#   # ... + group (cluster) 'paths'
#   base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
#                            size=group.line.width)
#   
#   # ... + group points (cluster data)
#   base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)
#   
#   #... + amend Legend title
#   if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
#   
#   # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
#   base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
#                             lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
#   base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
#                             lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
#   base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
#                             lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
#   
#   # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
#   if (label.gridline.min==TRUE) {
#     base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,face="bold",size=grid.label.size, hjust=1) }
#   base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,face="bold",size=grid.label.size, hjust=1)
#   base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,face="bold",size=grid.label.size, hjust=1)
#   
#   # ... + centre.y label if required [i.e. value of y at centre of plot circle]
#   if (label.centre.y==TRUE) {
#     centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
#     base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,face="bold",size=grid.label.size, hjust=0.5) }
#   
#   return(base)
#   
# }
