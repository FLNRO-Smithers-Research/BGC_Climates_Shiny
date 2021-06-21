###Station data###
################Station data######################################
###Create html list to catch station plots
output$stnPlots <- renderUI({
  stnVars <- input$var.pick
  stnPlotOutput <- lapply(1:length(stnVars), function(j){
    plotname <- paste("StnPlot", j, sep= "")
    plotOutput(plotname, height = "400px", width = "100%")
  })
  do.call(tagList, stnPlotOutput)
})

stPrepGraph <- reactive({
  modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station","bgc", input$var.pick),collapse = ","),
                                     " FROM stpoints_mod81 WHERE station IN ('",
                                     paste(input$stn.pick,collapse = "','"),"')"))
  modelSub$Type <- "Model"
  m.oldSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station","bgc", input$var.pick),collapse = ","),
                                     " FROM stpoints_mod61 WHERE station IN ('",
                                     paste(input$stn.pick,collapse = "','"),"')"))
  m.oldSub$Type <- "Model61-90"
  stationSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station","bgc", input$var.pick),collapse = ","),
                                       " FROM st_summary WHERE station IN ('",
                                       paste(input$stn.pick,collapse = "','"),"')"))
  stationSub$Type <- "Station"
  dat <- as.data.table(rbind(modelSub,stationSub,m.oldSub))
  setkey(dat,station)
  dat[,Type := as.factor(Type)]
  return(dat)
})


for (j in 1:50){
  local({
    my_j <- j
    
    plotname <- paste("StnPlot", my_j, sep= "")
    
    output[[plotname]] <- renderPlot({
      input$removeNA
      if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
        dat <- stPrepGraph()
        dat <- dat[,.(station,Type,bgc,get(input$var.pick[my_j]))]
        setnames(dat,c("Station","Type","BGC","Mean"))
        dat2 <- dcast(dat, Station + BGC ~ Type, value.var = "Mean", fun.aggregate = mean)
        colnames(dat2)[1] <- "ID"
        dat2$Diff <- apply(dat2[,.(Model,Station)],1,FUN = function(x){(1-(min(x)/max(x)))*100})
        dat2$Diff[is.na(dat2$Diff)] <- 0
        dat <- dat[Station %in% dat2$ID[dat2$Diff >= input$maxDiff],]
        
        if(input$removeNA %% 2 == 0){
          stNAs <- as.character(dat$Station[is.na(dat$Mean)])
          dat <- dat[!Station %in% stNAs,]
        }
        if(input$showNorm %% 2 == 0){
          dat <- dat[Type != "Model61-90",]
        }
        
        dat <- unique(dat)
        ggplot(dat, aes(x = Station, y = Mean, fill = Type)) +
          geom_bar(position = position_dodge(), stat = "identity", color = "black") +
          scale_fill_manual(values = c("Model" = "black","Station" = "grey","Model61-90" = "white"))+
          facet_grid(.~BGC, scales = "free_x",space = "free_x")+
          theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
          {if(length(dat$Station) > 12)theme(axis.text.x = element_text(angle = 90, hjust = 1))}+
          ggtitle(input$var.pick[my_j])
      }
      
    })
  })
}

###function to clean station data
stnDat <- reactive({
  stationSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station","name","bgc","latitude",
                                                         "longitude","elevation",input$var.pick),collapse = ","),
                                       " FROM st_summary WHERE station IN ('",
                                       paste(input$stn.pick,collapse = "','"),"')")) %>% as.data.table()
  modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station", input$var.pick),collapse = ","),
                                     " FROM stpoints_mod81 WHERE station IN ('",
                                     paste(input$stn.pick,collapse = "','"),"')"))
  modelSub <- as.data.table(unique(modelSub))
  stnBoth <- merge(stationSub, modelSub, by = "station", suffixes = c("_Station","_Model"), all.x = TRUE)
  colnames(stnBoth)[1] <- "St_ID"
  setkey(stnBoth, name)
  return(stnBoth)
})

###create plot with density distributions showing difference between station and model data
output$diffDens <- renderPlot({
  if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
    stnBoth <- stnDat()[,-c(3:6)]
    stnBoth <- melt(stnBoth, id.vars = c("St_ID","name"))
    stnBoth$Type <- fifelse(grepl("Station",stnBoth$variable), "Station","Model")
    stnBoth$variable <- gsub("_Station|_Model","",stnBoth$variable)
    stnBoth <- stnBoth[!is.na(value),]
    diffFun <- function(x){
      return (x[1]-x[2])
    }
    temp <- dcast(stnBoth, St_ID + name ~ variable, value.var = "value", fun.aggregate = diffFun)
    temp <- melt(temp, id.vars = c("St_ID","name"))
    ggplot(temp)+
      stat_density(aes(x = value, y = ..scaled.., fill = variable), alpha = 0.25)+
      labs(x = "Difference", y = "Density")
  }
  
})

output$downloadStn <- downloadHandler(
  filename = "ClimStation.csv",
  content = function(file){
    write.csv(stnDat(), file, row.names = FALSE)
  }
)

##only show stations as choices that are in that BGC
output$selectStn <- renderUI({
  BGC <- input$StnBGC.pick
  BGC <- paste("stn.",BGC, sep = "")
  sts <- foreach(i = 1:length(BGC), .combine = c) %do% {
    stn.list[[BGC[i]]]
  }
  pickerInput("stn.pick","Select Stations", 
              choices = sts, 
              multiple = TRUE, 
              selected = sts, 
              options = list(`actions-box` = TRUE))
})

stPrepMap <- reactive({
  if(length(input$var.pick) > 0){
    foreach(currVar = input$var.pick, .combine = rbind) %do% {
      stationSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station","latitude",
                                                             "longitude","bgc",currVar),collapse = ","),
                                           " FROM st_summary WHERE station IN ('",
                                           paste(input$stn.pick,collapse = "','"),"')"))
      colnames(stationSub)[c(1,5)] <- c("Station","Mean")
      stationSub$Type <- "Station"
      stationSub <- as.data.table(stationSub)
      stNames <- dbGetQuery(con, "SELECT DISTINCT station,name FROM st_summary")
      if(input$mapType == "Mean"){
        modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc", currVar),collapse = ","),
                                           " FROM climsum_curr_v11 WHERE period = '1961 - 1990' AND bgc IN ('",
                                           paste(input$StnBGC.pick,collapse = "','"),"')"))
        colnames(modelSub)[2] <- "BGCMean"
        modelSub <- as.data.table(modelSub)
        dat2 <- merge(stationSub, modelSub, by = "bgc", all.x = T)
        dat2[,Diff := BGCMean - Mean]
      }else{
        modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station", "latitude","longitude","bgc", 
                                                             currVar),collapse = ","),
                                           " FROM stpoints_mod81 WHERE station IN ('",
                                           paste(input$stn.pick,collapse = "','"),"')"))
        colnames(modelSub)[c(1,5)] <- c("Station","Mean")
        modelSub$Type <- "Model"
        modelSub <- as.data.table(modelSub)
        dat <- rbind(modelSub,stationSub)
        setkey(dat,Station)
        dat <- merge(dat,stNames, by.x = "Station", by.y = "station", all.x = TRUE)
        dat$Type <- as.factor(dat$Type)
        dat2 <- dcast(dat, Station + latitude + longitude + bgc + name ~ Type, value.var = "Mean", 
                      fun.aggregate = mean) ## shouldn't actually need to aggregate
        colnames(dat2)[1] <- "ID"
        dat2$Diff <- apply(dat2[,c("Model","Station")],1,FUN = function(x){((max(x)-min(x))/max(x))*100})
      }
      dat2$Diff[is.na(dat2$Diff)] <- 999
      dat2 <- dat2[dat2$Diff > input$maxDiff,]
      dat2 <- unique(dat2)
      dat2$Var = currVar
      dat2
    }
    
  }else{NULL}
  
})

output$stnDiffMap <- renderLeaflet({
  dat <- stPrepMap()
  dat <- dat[Diff != 999,]
  dat$Var <- as.factor(dat$Var)
  ##browser()
  if(is.data.table(dat)){
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addAwesomeMarkers(data = dat, ~longitude, ~latitude, icon = icons,
                        popup = paste0("<b>", dat$ID, "</b>", "<br>",
                                       "Variable: ", dat$Var, "<br>",
                                       "BGC: ", dat$bgc, "<br>",
                                       "Difference: ", round(dat$Diff, digits = 2), "<br>"))
  }
  
})