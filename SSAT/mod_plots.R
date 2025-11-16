

p_load(data.table, grid, gridExtra, ggplot2, ggsci, ggthemes, scales, shinycssloaders, showtext, sysfonts, RColorBrewer)

if (!"Roboto Condensed" %in% font_families()) font_add_google("Roboto Condensed")
showtext_auto()

# -----------------------------------------------------------------------------

base::load('WWW/AusMap.RData')

# -----------------------------------------------------------------------------

set_theme <- function(isMobile) {
  GridSize  <- ifelse(isMobile, 5, 10)
  AnnotSize <- ifelse(isMobile, 15, 7)
  LnHeight  <- ifelse(isMobile, 0.4, 1.4)
  AusMapXRange <- c(114.2,152.5)
  AusMapYRange <- c(-42.5,-12.0)
  LineCol  <- "black"
  LineW    <- 0.40
  StateColors <- rev(brewer.pal(8,'Dark2'))
  PlotMargin  <- margin(0.05,0.03,0.03,0.03, unit='npc')
  EmptyTextP  <- 'This type of visualisation is\nnot suitable for point analysis!'
  EmptyTextS  <- 'This type of visualisation is\nnot suitable for spatial analysis!'
  NoDataText  <- 'No data found!'

  my_theme <- theme_foundation(base_size=ifelse(isMobile,14,14)) + theme(
    plot.margin=PlotMargin, text=element_text(color="gray20", family="Roboto Condensed"),
    panel.background=element_rect(colour=NA), plot.background=element_rect(colour=NA), panel.border=element_rect(colour=NA),
    axis.title.y=element_text(angle=90,vjust=2,face="plain", size=rel(ifelse(isMobile,2,1))), axis.title.x=element_text(vjust=-0.5,face="plain", size=rel(1)),
    axis.text=element_text(size=rel(ifelse(isMobile,1.6,0.9))), axis.line=element_line(colour="black"),
    axis.ticks=element_line(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    legend.key=element_rect(colour=NA), legend.key.size=unit(rel(0.9), "cm"), legend.spacing=unit(0.1, "lines"),
    legend.text=element_text(size=rel(ifelse(isMobile,1.7,0.9))), legend.title=element_text(size=rel(1.0), face="bold"), legend.position="top",
    legend.box.margin=margin(0,0,0,0), legend.direction="horizontal", legend.justification=c(0.5,0),
    legend.margin=margin(0,0,0,0,unit='npc'), plot.caption=element_text(size=rel(ifelse(isMobile,1.5,0.75)),hjust=0,vjust=0.5,lineheight=LnHeight),
    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
    strip.text=element_text(face="bold",size=rel(1.4))
  )
  return(list(theme=my_theme, GridSize=GridSize, AnnotSize=AnnotSize, LnHeight=LnHeight, AusMapXRange=AusMapXRange, AusMapYRange=AusMapYRange,
              EmptyTextP=EmptyTextP, EmptyTextS=EmptyTextS, NoDataText=NoDataText, LineCol=LineCol, LineW=LineW, StateColors=StateColors))
}

# -----------------------------------------------------------------------------

GenSubtitleStats <- function(data, text) {
  return(text)
}

# -----------------------------------------------------------------------------

GenPlot <- function(data, Type, X, Y, Title, Unit, Yconv=1, NDigs=2, Limits=NULL, SubTitle=NULL, isMobile=F, isPoint=F) {
  my_theme <- set_theme(isMobile)

  if (Type == "MapPlot1") {
    p <- GenMapPlot1(data=data, Y=Y, Unit=Unit, NDigs=NDigs, Limits=Limits, SubTitle=SubTitle, Theme=my_theme, isMobile=isMobile, isPoint=isPoint)

  } else if (Type == "BoxPlot1") {
    p <- GenBoxPlot(data=data, Y=Y, Unit=Unit, SubTitle=SubTitle, Theme=my_theme, isMobile=isMobile)

  } else if (Type == "BarPlot1") {
    p <- GenBarPlot1(data=data, Y=Y, Unit=Unit, Limits=Limits, SubTitle=SubTitle, Theme=my_theme, isMobile=isMobile)

  } else if (Type == "ProbExcPlot1") {
    p <- GenProbExcPlot1(data=data, Y=Y, Unit=Unit, SubTitle=SubTitle, Theme=my_theme, isMobile=isMobile, isPoint=isPoint)

  } else if (Type == "DensityPlot1") {
    p <- GenDensityPlot1(data=data, Y=Y, Unit=Unit, SubTitle=SubTitle, Theme=my_theme, isMobile=isMobile, isPoint=isPoint)
  }

  x <- gc()
  return(p)
}

# -----------------------------------------------------------------------------

GenMapPlot1 <- function(data, Y, Unit, NDigs, Limits, SubTitle=NULL, Theme=NULL, isMobile=F, isPoint=F) {
  if (isPoint) {
    p <- ggplot() + geom_blank() + theme_void() +
      annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$EmptyTextP, lineheight=Theme$LnHeight)

  } else {

    if (nrow(data)==0) {
      p <- ggplot() + geom_blank() + theme_void() +
        annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$NoDataText, lineheight=Theme$LnHeight)

    } else {

      data <- data[StateShort=='AUS', lapply(.SD, function(x) mean(x)), keyby=c('Long','Lat'), .SDcols=Y]
      nBreaks <- ifelse(identical(Limits, c(0,1)), 4, 4)

      p <- ggplot(data=data, aes_string(x='Long', y='Lat', fill=Y)) + AusMap +
        coord_cartesian(xlim=Theme$AusMapXRange, ylim=Theme$AusMapYRange) + geom_point(color=Theme$LineCol,size=Theme$GridSize,shape=22,stroke=0.2) +

        scale_fill_stepsn(name=NULL,colors=brewer.pal(11,'RdYlGn'), nice.breaks=F, n.breaks=nBreaks, show.limits=T, limits=Limits,
                          label=function(x) sprintf(sprintf("%%.%sf", NDigs), x)) +

        Theme$theme + theme(plot.caption=element_text(margin=margin(0,0,0,0,unit='npc')),
                            axis.line=element_blank(),axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
                            panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.frame=element_rect(linewidth=0.1),
                            legend.ticks=element_line(linetype=1,linewidth=0.1),legend.ticks.length=unit(1,'npc'),legend.direction='horizontal',
                            legend.position='top',legend.key.height=unit(0.03,"npc"),legend.key.width=unit(0.1,"npc")) +
        xlab(label=NULL) + ylab(label=NULL)
    }

    if (length(unique(data$phase))>1) p <- p + facet_wrap('~phase')
  }

  if (isPoint) {
    SubTitle <- NULL

  } else if (is.null(SubTitle) && any(is.na(data[,get(Y)]))) {
    SubTitle <- 'Gray cells: crop cannot reach maturity in 250 days.'
  }

  return(list(
    plot = p,
    subtitle = SubTitle,
    unit = Unit
  ))
}

# -----------------------------------------------------------------------------

GenBoxPlot <- function(data, Y, Unit, SubTitle=NULL, Theme=NULL, isMobile=F, isPoint=F) {
  if (nrow(data)==0) {
    p <- ggplot() + geom_blank() + theme_void() +
      annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$NoDataText, lineheight=Theme$LnHeight)

  } else {

    FUNC <- function(xx) {
      yy <- quantile(xx,probs=c(0.05,0.25,0.5,0.75,0.95),na.m=T)
      names(yy) <- c("ymin", "lower", "middle", "upper", "ymax")
      return(yy)
    }

    FUNC1 <- function(y,coefs=c(0.05,0.95)) {
      y <- quantile(y,probs=coefs,na.rm=T)
      y <- data.frame(Low=y[1],High=y[2])
      return(y)
    }
    Temp1 <- data[, FUNC1(.SD, coefs=c(0.10,0.90)), keyby='StateShort', .SDcols=Y]
    Temp2 <- data[, FUNC1(.SD, coefs=c(0.01,0.99)), keyby='StateShort', .SDcols=Y]

    p <- ggplot(data=data, aes_string(x='StateShort', y=Y, fill='StateShort')) + coord_flip() +

      Theme$theme + theme(plot.caption=element_text(margin=margin(0.04,0,0.01,0,unit='npc'))) +
      xlab(label=NULL) + ylab(label=NULL) + scale_fill_manual(name=NULL,values=Theme$StateColors,drop=T) +
      labs(caption='Circle: average. Vertical lines (from left): 1|10|50|90|99th quantiles.\nBox: 25|75th quantiles.') +
      geom_errorbar(data=Temp2,aes_string(x='StateShort', ymin='Low',ymax='High'),na.rm=T,position=position_dodge2(width=0.8),width=0.4,color=Theme$LineCol,lwd=0.9*Theme$LineW,linetype='dashed',inherit.aes=F) +
      geom_errorbar(data=Temp1,aes_string(x='StateShort', ymin='Low',ymax='High'),na.rm=T,position=position_dodge2(width=0.8),width=0.4,color=Theme$LineCol,lwd=1.1*Theme$LineW,inherit.aes=F) +
      geom_boxplot(data=data,outlier.size=NA,outlier.color=NA,position=position_dodge2(width=0.8),width=0.6,coef=0,color=Theme$LineCol,lwd=Theme$LineW,show.legend=F,na.rm=T) +
      stat_summary(geom='point',fun='mean',size=4,show.legend=F,na.rm=T)

    if (length(unique(data$phase))>1) p <- p + facet_wrap('~phase')
  }

  return(list(
    plot = p,
    subtitle = SubTitle,
    unit = Unit
  ))
}

# -----------------------------------------------------------------------------

GenBarPlot1 <- function(data, Y, Unit, Limits, SubTitle=NULL, Theme=NULL, isMobile=F, isPoint=F) {
  if (nrow(data)==0) {
    p <- ggplot() + geom_blank() + theme_void() +
      annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$NoDataText, lineheight=Theme$LnHeight)

  } else {
    p <- ggplot(data=data, aes_string(x='StateShort', y=Y, fill='StateShort')) + scale_y_continuous(limits=Limits) +
      Theme$theme + xlab(label=NULL) + ylab(label=NULL) + scale_fill_manual(name=NULL,values=Theme$StateColors,drop=T) +
      stat_summary(geom='bar',fun='mean',position=position_dodge2(width=0.8),width=0.7,color=Theme$LineCol,lwd=Theme$LineW,show.legend=F,na.rm=T)

    if (length(unique(data$phase))>1) p <- p + facet_wrap('~phase')
  }

  return(list(
    plot = p,
    subtitle = SubTitle,
    unit = Unit
  ))
}

# -----------------------------------------------------------------------------

GenProbExcPlot1 <- function(data, Y, Unit, SubTitle=NULL, Theme=NULL, isMobile=F, isPoint=F) {
  if (!isPoint) {
    p <- ggplot() + geom_blank() + theme_void() +
      annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$EmptyTextS, lineheight=Theme$LnHeight)

  } else {

    if (nrow(data)==0) {
      p <- ggplot() + geom_blank() + theme_void() +
        annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$NoDataText, lineheight=Theme$LnHeight)

    } else {
      p <- ggplot(data=data, aes_string(x=Y, color='StateShort')) + scale_y_continuous(limits=c(0,1)) +
        stat_ecdf(geom='line',position='identity',lwd=Theme$LineW*2,show.legend=T,na.rm=T) +
        scale_color_manual(name=NULL,values=Theme$StateColors,drop=F) +
        Theme$theme + theme(panel.grid.major=element_line(color='grey',linewidth=Theme$LineW*0.5,linetype=2)) +
        guides(colour=guide_legend(nrow=2,override.aes=list(lwd=Theme$LineW*8),keyheight=0.5)) +
        xlab(label=NULL) + ylab(label='Exceedance probability')

      if (length(unique(data$phase))>1) p <- p + facet_wrap('~phase')
    }
  }

  return(list(
    plot = p,
    subtitle = SubTitle,
    unit = Unit
  ))
}

# -----------------------------------------------------------------------------

GenDensityPlot1 <- function(data, Y, Unit, SubTitle=NULL, Theme=NULL, isMobile=F, isPoint=F) {
  if (!isPoint) {
    p <- ggplot() + geom_blank() + theme_void() +
      annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$EmptyTextS, lineheight=Theme$LnHeight)

  } else {

    if (nrow(data)==0) {
      p <- ggplot() + geom_blank() + theme_void() +
        annotate('text', x=mean(Theme$AusMapXRange), y=mean(Theme$AusMapYRange), colour='darkred', size=Theme$AnnotSize, label=Theme$NoDataText, lineheight=Theme$LnHeight)

    } else {
      p <- ggplot(data=data, aes_string(x=Y, y='..density..', fill='StateShort')) +
        geom_density(position='identity',alpha=0.6,lwd=Theme$LineW*2,show.legend=T,na.rm=T) +
        scale_fill_manual(name=NULL,values=Theme$StateColors,drop=F) +
        Theme$theme + theme(panel.grid.major.x=element_line(color='grey',linewidth=Theme$LineW*0.5,linetype=2)) +
        xlab(label=NULL) + ylab(label='Density') + guides(fill=guide_legend(nrow=2,override.aes=list(linewidth=Theme$LineW,size=0.1),keyheight=0.5))

      if (length(unique(data$phase))>1) p <- p + facet_wrap('~phase')
    }
  }

  return(list(
    plot = p,
    subtitle = SubTitle,
    unit = Unit
  ))
}

# -----------------------------------------------------------------------------

id_plot_densyielddry <- "DensYieldDry"
id_plot_yielddry <- "PlotYieldDry"
id_plot_yieldwet <- "PlotYieldWet"
id_plot_biomass  <- "PlotBiomass"
id_plot_flowering  <- "PlotFlowering"
id_plot_maturity  <- "PlotMaturity"
id_plot_fw  <- "PlotFW"
id_plot_wetroot  <- "PlotWetRoot"
id_plot_psfw  <- "PlotPsFW"
id_plot_psftmean  <- "PlotPsFTMean"
id_plot_psftmin  <- "PlotPsFTMin"
id_plot_psft  <- "PlotPsFT"
id_plot_psfn  <- "PlotPsFN"
id_plot_frgr  <- "PlotFRGR"

# -----------------------------------------------------------------------------

plots <- list()

plots[[id_plot_yielddry]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "GrainDryYield",
    Title = "Dry grain yield",
    Unit = "kg/ha", NDigs = 0, Yconv = 1
  )
)
plots[[id_plot_yieldwet]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "GrainWetYield",
    Title = "Wet grain yield",
    Unit = "kg/ha", NDigs = 0, Yconv = 1
  )
)
plots[[id_plot_biomass]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AbovegroundDW",
    Title = "Aboveground biomass",
    Unit = "kg/ha", NDigs = 0, Yconv = 1
  )
)
plots[[id_plot_flowering]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "Soybean.Phenology.StartFloweringDAS",
    Title = "Days to flowering",
    Unit = "days", NDigs = 0, Yconv = 1
  )
)
plots[[id_plot_maturity]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "Soybean.Phenology.MaturityDAS",
    Title = "Days to maturity",
    Unit = "days", NDigs = 0, Yconv = 1
  )
)
plots[[id_plot_fw]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AveFW",
    Title = "Water deficieicny stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_wetroot]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AveWetRootFracFact",
    Title = "Waterlogging stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_psfw]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AvePsFW",
    Title = "Overall water stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_psftmean]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AvePsFTMeanT",
    Title = "Mean-temperature stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_psftmin]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AvePsFTMinT",
    Title = "Frost stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_psft]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AvePsFT",
    Title = "Overal temperature stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_psfn]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AvePsFN",
    Title = "N deficiency stress",
    Unit = "0-1", NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)
plots[[id_plot_frgr]] <- list(
  generator = GenPlot,
  permission = permissions$view_linecanefw,
  parameters = list(
    X = id_paddock_settings_harv_year,
    Y = "AveFRGR",
    Title = "Growth fraction",
    Unit = "0-1",  NDigs = 1, Limits = c(0,1), Yconv = 1,
    SubTitle = '0: Maximum stress | 1: No stress'
  )
)

all_plot_ids <- sapply(plots, function(x) paste0(x$parameters$Title, ' (', x$parameters$Unit, ')'))
all_plot_ids <- setNames(names(all_plot_ids), all_plot_ids)

# -----------------------------------------------------------------------------

generate_plots_id <- function(paddock) {
  paste("plot", paddock, sep = "_")
}

generate_plots_ns <- function(paddock) {
  NS(generate_plots_id(paddock))
}

# -----------------------------------------------------------------------------

plots_ui <- function(paddock, user_role) {
  ns <- generate_plots_ns(paddock)

  tags$div(
    tags$div(class = 'myapp-plots__buttons',
             selectInput(
               inputId = ns('targetVarName'),
               label = NULL, choices = all_plot_ids,
               selectize = F, selected = all_plot_ids[1],
             )
    ),
    tags$div(class = 'myapp-plots__buttons',
             tags$p(
               actionButton(ns("addPlotBtn"), "Add Plot"),
               actionButton(ns("remPlotBtn"), "Remove Plot"),
             )
    ),

    tags$div(class = 'myapp-plots__buttons',
             id=paste("paddock_plot_selector", paddock, sep="_"),
             uiOutput(ns('SelectorUI'))),

    tags$div(
      class = "row",
      singleton(tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "plots/plots.css"
        )
      )),

      uiOutput(ns("Plots"))
    )
  )
}

# -----------------------------------------------------------------------------

plots_server <- function(paddock, settings, plot_types, plot_type, data_x) {
  ns <- generate_plots_ns(paddock)

  maxNumPlots <- 6
  maxDuration <- 5

  moduleServer(generate_plots_id(paddock), function(input, output, session) {
    plot_ids <- reactiveVal(c(all_plot_ids[1]))

    output$SelectorUI <- renderUI({
      plot_selector_ui(paddock, user_role, plot_types())
    })

    output$Plots <- renderUI({
      lapply(plot_ids(), function(pid) {
        plot <- plots[[pid]]

        tags$div(class="col-sm-6 col-md-4",
                 tags$div(class="myapp-plots__item",
                          tags$div(class="text-center myapp-plots__item-heading",
                                   tags$h2(
                                     tags$strong(plot$parameters$Title),
                                     tags$strong(textOutput(ns(paste(pid, "unit", sep="_")), inline=T))
                                   )
                          ),
                          withSpinner(
                            plotOutput(ns(pid), height="auto", width="100%"),
                            type = spinner_type
                          ),
                          tags$div(class="myapp-plots__item-subtitlte",
                                   tags$p(textOutput(ns(paste(pid, "subtitle", sep = "_"))))
                          )
                 )
        )
      })
    })

    # Add new plot
    observeEvent(input$addPlotBtn, {
      if (length(plot_ids())>=maxNumPlots) {
        showNotification(sprintf("You cannot add more than %s plots!", maxNumPlots), type='error', duration=maxDuration)

      } else if (input$targetVarName %in% plot_ids()) {
        showNotification("This plot already exists!", type='message', duration=maxDuration)

      } else if (!input$targetVarName %in% plot_ids()) {
        plot_ids(c(plot_ids(), input$targetVarName))
      }
    })

    # Remove last plot
    observeEvent(input$remPlotBtn, {
      if (length(plot_ids())>0 && input$targetVarName %in% plot_ids()) {
        plot_ids(setdiff(plot_ids(), input$targetVarName))
      }
    })

    toListen1 <- reactive({
      list(input$targetVarName, input$addPlotBtn, input$remPlotBtn, plot_ids())
    })
    toListen2 <- reactive({
      list(input$addPlotBtn, input$remPlotBtn)
    })

    # Disable/enable add/remove button when the selected variable changes
    observeEvent(ignoreInit=T, toListen1(), {
      if (!input$targetVarName %in% plot_ids()) {
        shinyjs::enable("addPlotBtn")
        shinyjs::disable("remPlotBtn")

      } else {
        shinyjs::disable("addPlotBtn")
        shinyjs::enable("remPlotBtn")
      }
    })

    # Disable remove button when there is no plot
    observeEvent(ignoreInit=T, toListen2(), {
      if (length(plot_ids())==0) {
        shinyjs::disable("remPlotBtn")

      } else {
        if (input$targetVarName %in% plot_ids()) {
          shinyjs::enable("remPlotBtn")
        } else {
          shinyjs::enable("remPlotBtn")
        }
      }
    })
  }
  )

  plots_server_sec(paddock, settings, plot_type, data_x)
}

# -----------------------------------------------------------------------------

plots_server_sec <- function(paddock, settings, plot_type, data_x) {
  ns <- generate_plots_ns(paddock)

  isMobile <- shinybrowser::is_device_mobile()
  set_theme(isMobile)

  moduleServer(generate_plots_id(paddock), function(input, output, session) {

    isolate(
      lapply(names(plots), function(id) {
        plot <- plots[[id]]

        output[[id]] <- renderPlot(
          {
            isPoint <- settings()[[id_paddock_settings_ispoint]]
            data <- data_x()
            if (nrow(data)>0) data[, YModified := get(plot$parameters$Y) / plot$parameters$Yconv]
            result <- do.call(plot$generator, c(list(data=data, Type=plot_type(), isMobile=isMobile, isPoint=isPoint), plot$parameters))

            if ("unit" %in% names(result)) {
              output[[paste(id, "unit", sep = "_")]] <- renderText(paste0(" (", result$unit, ")"))
            } else {
              output[[paste(id, "unit", sep = "_")]] <- renderText(NULL)
            }
            output[[paste(id, "subtitle", sep = "_")]] <- renderText(result$subtitle)
            return(result$plot)
          },

          height = function() {
            isPoint <- settings()[[id_paddock_settings_ispoint]]
            data <- data_x()
            if (nrow(data)==0 || ((isPoint && plot_type() %in% c('MapPlot1')) || (!isPoint && plot_type() %in% c('ProbExcPlot1','DensityPlot1')))) {
              aspect_ratio <- 0.4

            } else {
              aspect_ratio <- ifelse(plot_type() %in% c('MapPlot1'), 0.9, 0.75)
            }
            width <- session$clientData[[paste("output", ns(id), "width", sep="_")]]
            return(aspect_ratio * width)
          }
        )
      })
    )
  }
  )
}


