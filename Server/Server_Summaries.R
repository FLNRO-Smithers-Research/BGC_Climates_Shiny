##Climate Summaries tab
###Create UI for selecting climate summary and periods
observeEvent(input$byZone,{
  toggleElement(id = "sz.choose",
                condition = input$byZone != "Zone")
})

##UI for selecting zones
observeEvent(input$includeWNA,{
  tempChoose <- zone.chooseBC
  if(input$includeWNA == "Yes"){tempChoose <- zone.choose}
  updatePickerInput(session,"BGCZone.choose", choices = tempChoose, selected = "IDF")
})

##UI for selecting subzones
observe({
  if(input$byZone != "Zone"){
    t1 <- paste(input$BGCZone.choose, collapse = "|")
    tempChoose <- BGC.chooseBC
    if(input$includeWNA == "Yes"){tempChoose <- BGC.choose}
    szChoose <- tempChoose[grep(t1,tempChoose)]
    updatePickerInput(session,"sz.choose", choices = szChoose, selected = szChoose[1])
  }
})

#############Table Summaries and Data Download#################### 
getData <- reactive({
  selectVars <- c(input$annual, input$seasonal, input$monthly)
  selectPer <- c(input$periodTS, input$periodOther)
  selectPerFut <- selectPer[selectPer %in% c("2025","2055","2085")]
  ModSet <- input$ModSet
  if(input$byZone == "Zone"){
    selectBGC <- input$BGCZone.choose
    tabCurr <- "zonesum_curr_v12"
    tabFut <- "zonesum_fut_v12"
    selectBC <- "BC"
    if(input$includeWNA == "Yes"){selectBC <- "US_AB"}
    q1 <- paste0("SELECT ",paste(c("bgc", "period","var", selectVars),collapse = ","),
                 " FROM ",tabCurr," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
                 "') AND period IN ('",paste(selectPer,collapse = "','"),"') AND location = '",selectBC,"'")
    q2 <- paste0("SELECT ",paste(c("bgc", "period","var", selectVars),collapse = ","),
                 " FROM ",tabFut," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
                 "') AND period IN (",paste(selectPerFut,collapse = ","),") AND scenario = '",
                 input$Scenario,"' AND modset = '",ModSet,"' AND location = '",selectBC,"'")
  }else{
    selectBGC <- input$sz.choose
    tabCurr <- "climsum_curr_v12"
    tabFut <- "climsum_fut_v12"
    q1 <- paste0("SELECT ",paste(c("bgc", "period","var", selectVars),collapse = ","),
                 " FROM ",tabCurr," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
                 "') AND period IN ('",paste(selectPer,collapse = "','"),"')")
    q2 <- paste0("SELECT ",paste(c("bgc", "period","var", selectVars),collapse = ","),
                 " FROM ",tabFut," WHERE bgc IN ('",paste(selectBGC,collapse = "','"),
                 "') AND period IN ('",paste(selectPerFut,collapse = "','"),"') AND scenario = '",
                 input$Scenario,"' AND modset = '",ModSet,"'")
  }
  
  if(length(selectVars) > 0){
    climSubset <- tryCatch({
      dbGetQuery(con, q1)
    },
    error = function(e){
      dbClearResult(dbListResults(con)[[1]])
      invisible(lapply(dbListConnections(PostgreSQL()), dbDisconnect))
      con <<- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                        port = 5432, dbname = "bgc_climate_date")
      dat <- dbGetQuery(con, q1)
      return(dat)
    })
    futureSub <- dbGetQuery(con, q2)
    ##browser()
    climSubset <- as.data.table(rbind(climSubset, futureSub))
  }
  return(climSubset)
})

###function to create table of data
createTable <- reactive({
  selectVars <- c(input$annual, input$seasonal, input$monthly)
  selectPer <- c(input$periodTS, input$periodOther)
  if(length(selectPer) > 0 & length(selectVars) > 0){
    climSubset <- getData()
    #browser()
    molten <- melt(climSubset,id.vars = c("bgc","period","var"))
    reShape <- dcast(molten, period+var+variable~bgc)
    setorder(reShape,variable)
    setnames(reShape, old = c("period","var","variable"), new = c("TimePeriod","Statistic","ClimateVar"))
    setcolorder(reShape,c(c(1,3,2,4:length(reShape))))
    return(reShape)
  }
})

observeEvent({c(input$includeWNA,input$BGCZone.choose,
                input$sz.choose, input$periodTS,input$periodOther,
                input$annual, input$seasonal)},{
  output$table <- renderTable({
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    selectPer <- c(input$periodTS, input$periodOther)
    # if(length(selectPer) > 0 & length(selectVars) > 0){
    climSubset <- getData()
    molten <- melt(climSubset, id.vars = c("bgc","period","var"))
    if(input$dataForm == "BGCs as Columns"){
      reShape <- dcast(molten, period+var+variable~bgc)
      setorder(reShape,variable)
      setnames(reShape, old = c("period","var","variable"), new = c("TimePeriod","Statistic","ClimateVar"))
      setcolorder(reShape,c(c(1,3,2,4:length(reShape))))
    }else{
      reShape <- dcast(molten, bgc+var+variable~period)
      setorder(reShape,variable)
      setnames(reShape, old = c("period","var","variable"), new = c("TimePeriod","Statistic","ClimateVar"))
      reShape <- reShape[,c(1,3,2,4:length(reShape))]
    }
    reShape
    # }
  })
})

###Download data
output$downloadTable <- downloadHandler(
  filename = "ClimateSummary.csv",
  content = function(file){
    write.csv(createTable(), file)
  }
)

###create main summary plots
summaryPlots <- reactive({
  plots <- list()
  selectVars <- c(input$annual, input$seasonal, input$monthly)
  selectPer <- c(input$periodTS, input$periodOther)
  for(i in 1:length(selectVars)){
    
    name <- selectVars[i]
    if (length(selectVars) > 0 & length(selectPer) > 0){
      data <- createTable()
      if(input$grType == "Bar" | input$grType == "Line"){
        graph <- data[ClimateVar == selectVars[i] & 
                        (Statistic == "mean" | Statistic == input$Error | Statistic == input$futError),]
        graph[,Statistic := fifelse(Statistic != "mean", "StDev", "mean")]
        graph[,ClimateVar := NULL]
        graph <- melt(graph,id.vars = c("TimePeriod","Statistic"))
        graph <- dcast(graph, variable+TimePeriod~Statistic)
        setnames(graph,old = c("variable","TimePeriod","StDev","mean"), new = c("BGC","Period","Error","Mean"))
        setorder(graph,Period)
        if(input$grType == "Bar"){
          graph[,Period := as.factor(Period)]
          plots[[i]] <- ggplot(graph, aes(x = BGC, y = Mean, fill = Period)) +
            geom_bar(position = position_dodge(), stat = "identity") +
            geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, 
                          position = position_dodge(0.9))+ theme_bw() + 
            theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
            ggtitle(selectVars[i]) + labs(x = "BGC")
        }else{
          graph$Period <- as.character(graph$Period)
          graph$Period <- as.numeric(substring(graph$Period, first = 1, last = 4))
          plots[[i]] <- ggplot(graph, aes(x = Period, y = Mean, colour = BGC))+
            geom_line()+
            geom_ribbon(aes(ymin = Mean - Error, ymax = Mean + Error), linetype = 2, alpha = 0.1)+
            theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
            ggtitle(selectVars[i]) + labs(x = "First Year of Normal Period", y = "")
        }
      }else{
        graph <- data[ClimateVar == selectVars[i] & Statistic %in% c("mean","max","min","10%","90%",input$Error,input$futError),]
        graph[,Statistic := fifelse(Statistic %chin% c(input$Error, input$futError), "StDev", Statistic)]
        graph[,ClimateVar := NULL]
        graph <- melt(graph, id.vars = c("TimePeriod","Statistic"))
        graph <- dcast(graph, variable+TimePeriod~Statistic)
        setnames(graph, old = c("variable","TimePeriod"), new = c("BGC","Period"))
        setorder(graph, Period)
        
        graph[,Period := as.numeric(substring(as.character(Period), first = 1, last = 4))]
        graph[,BGC := as.factor(BGC)]
        plots[[i]] <- ggplot(graph, aes(x = Period, lower = `10%`, upper = `90%`, middle = mean, 
                                        ymin = min, ymax = max,group = 1:(length(selectPer)*length(unique(graph$BGC))), color = BGC))+
          geom_boxplot(stat = "identity", position = "dodge")+
          theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
          ggtitle(selectVars[i]) + labs(x = "First Year of Normal Period")
      }
    } 
  }
  return(plots)
})

pHeight <- reactive({
  selectVars <- c(input$annual, input$seasonal, input$monthly)
  print(length(selectVars))
  return(400*(ceiling((length(selectVars))/2)))
})

observe({
  output$sumPlots <- renderPlot({
    ptlist <- summaryPlots()
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    
    grid.arrange(grobs=ptlist,ncol=2)
  }, height = pHeight())
})

##download summary plots
output$downloadSumPlots <- downloadHandler(
  filename = function(){
    if(input$includeWNA == "Yes"){
      "WNA_ClimateSummaryPlots.png"
    }else{
      "BC_ClimateSummaryPlots.png"
    }
  },
  content = function(file){
    ptlist <- summaryPlots()
    to_delete <- !sapply(ptlist,is.null)
    ptlist <- ptlist[to_delete] 
    if (length(ptlist)==0) return(NULL)
    plts <- arrangeGrob(grobs=ptlist,ncol=2)
    ggsave(file,plot = plts,device = "png",units = "in", width = 12, height = pHeight()/64)
  }
)

###prepare data for walter chart 
walterPrep <- reactive({
  loc <- "BC"
  if(input$includeWNA == "Yes"){loc <- "US_AB"}
  if(input$byZone == "Zone"){
    BGC <- input$BGCZone.choose[1]
    tab <- "zonesum_fut_v12"
  }else{
    BGC <- input$sz.choose[1]
    tab <- "climsum_fut_v12"
  }
  x1 <- dbGetQuery(con, paste0("SELECT * FROM ",tab," WHERE bgc = '",
                               BGC,"' AND period = 2025 
                                   AND scenario = '",input$Scenario,"' 
                                   AND modset = '",input$ModSet,"'
                                   AND var = 'mean'
                                   AND location = '",loc,"'"))
  ppt=x1[,c(paste("ppt0",1:9,sep=""),paste("ppt",10:12,sep=""))]	
  tmx=x1[,c(paste("tmax0",1:9,sep=""),paste("tmax",10:12,sep=""))]	
  tmn=x1[,c(paste("tmin0",1:9,sep=""),paste("tmin",10:12,sep=""))]	
  tmn2=x1[,c(paste("dd_0_0",1:9,sep=""),paste("dd_0_",10:12,sep=""))]	
  tmn2[tmn2>0] <- -16
  tmn2[tmn2==0] <- 15
  waltMat <- rbind(as.matrix(ppt),as.matrix(tmx),
                   as.matrix(tmn),as.matrix(tmn2))
  return(waltMat)
  
})

output$walterPlot <- renderPlot({
  if(!is.null(input$BGCZone.choose)){
    diagwl(walterPrep(), mlab="en", p3line=F, est=input$BGC.choose[1], per=2025)
  }
})