###This shiny app imports a dataset of future and historic climate summaries, and allows choices
###for various different user selected statistics and graphics. 
###Kiri Daust, June 2019

require(shiny)
require(reshape2)
require(shinyWidgets)
require(ggplot2)
require(vegan)
require(shinythemes)
require(devtools)
require(leaflet)
require(data.table)
require(sf)
require(climatol)
require(sp)
require(shinyBS)
require(shinyjs)
require(scales)

###Read in climate summary data
climSummary <- fread("ClimateSummaryCurrent_v11_5.6.csv", data.table = F)
futureSummary <- fread("ClimateSummary_Future_v11_5.6.csv", data.table = F)
futureSummary$period <- as.character(futureSummary$period)
####Set up choices
BGC.choose <- as.character(unique(climSummary$BGC))
period.choose <- as.character(unique(climSummary$period))
period.ts <- c(period.choose[1:4], "2025","2055","2085")
period.other <- c(period.choose[4:length(period.choose)])
stat.choose <- as.character(unique(climSummary$Var))
var.choose <- as.character(colnames(climSummary)[-c(1:3)])
var.choose <- var.choose[order(var.choose)]
monthly <- var.choose[grep("01|02|03|04|05|06|07|08|09|10|11|12", var.choose)]
seasonal <- var.choose[grep("sp|sm|at|wt", var.choose)]
seasonalShort <- seasonal[grep("PPT|RAD|Tave|Tmin|Tmax", seasonal)]
annual <- var.choose[!var.choose %in% c(monthly,seasonal)]
zone.choose <- c("BAFA", "BG", "BWBS", "CDF",  "CMA",  "CWH",  "ESSF", "ICH",  "IDF",  "IMA",  "MH",   "MS",   "PP",   "SBPS" ,"SBS", 
                 "SWB")
annualDirect <- c("MAT","MWMT","MCMT","TD","MAP","MSP","AHM","SHM")
futScn <- c("rcp45","rcp85")
for(i in 1:length(zone.choose)){
  name <- paste(zone.choose[i],".choose", sep = "")
  temp <- BGC.choose[grep(zone.choose[i],BGC.choose)]
  assign(name,temp)
}

BGC.list <- list()

###Create list with choices for selection
for (i in 1:length(zone.choose)){
  name <- zone.choose[i]
  name2 <- paste(zone.choose[i],".choose", sep = "")
  temp <- get(name2)
  BGC.list[[name]] <- temp
}
###read in model data
#modelDat <- read.csv("WeatherStationLocations_updated_Normal_1961_1990MSY.csv",stringsAsFactors = FALSE)
modelDat <- fread("StPoints_Model81-10.csv", data.table = FALSE)
modelReg <- fread("StPoints_Model61-90.csv", data.table = FALSE)
stationDat <- fread("StationSummary.csv", data.table = FALSE)
stationDat <- merge(modelDat[,1:5], stationDat, by = "STATION", all = FALSE)
stationDat <- unique(stationDat)
stationDat$BGC <- as.character(stationDat$BGC)
stationDat$STATION <- as.character(stationDat$STATION)
stn.BGC <- unique(stationDat$BGC)
stn.BGC <- sort(stn.BGC)
stn.var <- colnames(stationDat)[-c(1:6)]
stn.var <- sort(stn.var)
stnModDiff <- fread("StnModelDiff2.csv",data.table = FALSE)
stnDiff.var <- colnames(stnModDiff)[-c(1:6)]
stnMeanDiff <- fread("StnBGCMeanDiff.csv", data.table = FALSE)
mapData <- fread("GridForMapsAll.csv", data.table = FALSE)
mapVar.choose <- colnames(mapData)[-c(1:5)]
mapMod.choose <- unique(mapData$GCM)
mapFut.choose <- c(2025,2055,2085)
mapScn.choose <- c("rcp45","rcp85")

stn.list <- list()
for(i in 1:length(stn.BGC)){
  temp <- stationDat$STATION[stationDat$BGC == stn.BGC[i]]
  name <- paste("stn.",stn.BGC[i],sep = "")
  stn.list[[name]] <- temp
}

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){##function from StackOverflow for tooltips
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

icons <- awesomeIcons(icon = "circle",  markerColor = "blue", iconColor = "#ffffff", library = "fa")

####USER INTERFACE########################
ui <- navbarPage(title = "BC Climate Summaries", theme = "css/bcgov.css",
    tabPanel("Climate BC Summaries", ####First tab: Summary
            useShinyjs(),
            shinyjs::hidden(
              div(id = "main",
                  fluidRow(
                    column(2,
                           titlePanel("Select Input"), ###Select which summary data to use
                           pickerInput("data",
                                       "Select which summary to use",
                                       choices = c("ClimateSummaryCurrent_v11_5.6.csv", "ClimateSummaryCurrent_v10_5.5.csv"),
                                       selected = "ClimateSummaryCurrent_v11_5.6.csv",
                                       multiple = FALSE),
                           
                           pickerInput(inputId = "BGC.choose",###Select BGCs
                                       label = "Select BGCs for Summary",
                                       choices = BGC.list, 
                                       selected = NULL, multiple = TRUE),
                           htmlOutput("periodSelect"), ###Select periods (changes based on user input)
                           dropdown( ###Select variables
                             pickerInput("annual",
                                         "Select Annual Variables:",
                                         choices = annual,
                                         inline = FALSE,
                                         multiple = TRUE,
                                         selected = NULL, options = list(`actions-box` = TRUE)),
                             pickerInput("seasonal",
                                         "Select Seasonal Variables:",
                                         choices = seasonal,
                                         inline = FALSE,
                                         multiple = TRUE,
                                         selected = NULL, options = list(`actions-box` = TRUE)),
                             pickerInput("monthly",
                                         "Select Monthly Variables:",
                                         choices = monthly,
                                         inline = FALSE,
                                         multiple = TRUE,
                                         selected = NULL, options = list(`actions-box` = TRUE)),
                             circle = FALSE, label = "ClimateVars",status = "primary"),
                           
                           ####Select choices for graphs
                           awesomeRadio("Error",
                                        "Select Error Type:",
                                        choices = c("st.dev.Geo","st.dev.Ann"),
                                        selected = "st.dev.Geo",
                                        inline = FALSE),
                           awesomeRadio("Scenario",
                                        "Select Future Scenario",
                                        choices = futScn,
                                        selected = "rcp85",
                                        inline = FALSE),
                           awesomeRadio("futError",
                                        "Select Future Error Type",
                                        choices = c("SD.mod","SD.Geo"),
                                        selected = "SD.mod",
                                        inline = FALSE),
                           awesomeRadio("grType",
                                        "Choose Graph Type",
                                        choices = c("Bar","Boxplot","Line"),
                                        selected = "Bar",
                                        inline = FALSE)
                    ),
                    radioTooltip(id = "Error", choice = "st.dev.Geo", title = "Geographical standard deviation (current)", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Error", choice = "st.dev.Ann", title = "Standard Deviation over time period (current)", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Scenario", choice = "rcp45", title = "Conservative Emission Projection", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Scenario", choice = "rcp85", title = "High Emission Projection", placement = "right", trigger = "hover"),
                    radioTooltip(id = "futError", choice = "SD.mod", title = "Standard Deviation between future models", placement = "right", trigger = "hover"),
                    radioTooltip(id = "futError", choice = "SD.Geo", title = "Future geographical standard deviation (ensemble model)", placement = "right", trigger = "hover"),
                    column(6, ###choose how to format data
                           titlePanel("Data"), 
                           pickerInput("dataForm",
                                       "Choose Data Format",
                                       choices = c("BGCs as Columns","Timeperiods as Columns"),
                                       selected = "BGCs as Columns",
                                       multiple = FALSE),
                           tableOutput("table"), 
                           downloadButton('downloadTable', 'Download')),
                    column(4,
                           titlePanel("Summary Figures"),
                           h4("ClimateBC Summary by BGC"),
                           uiOutput("plots"),
                           br(),
                           br(),
                           h4("Walter plot of BGC"),
                           plotOutput("walterPlot"))
                  )
              )
            ),
            div(
              id = "start",
              h1("Welcome to the BC Climate Summary webtool!"),
              p("The purpose of this tool is to make climate and climate change information easily accessible. The first tab provides an interface
          to download and summarise climate data from Tongli Wang's Climate BC; data is summarised by Biogeoclimatic unit and time period (inlcuding 
          modelled future periods). Select one or more time periods, BGCs, and climate variables to show the data. You can then download the data and view some 
          summary graphs."),
              p("The second tab allows for viewing of climate BC projections on a map: select a model, future climate scenario, and climate variable, and 
          it will display time series and difference maps of the provincial data."),
              p("The third tab contains tools for investigating climate station data and comparing to modelled data; either select stations within a BGC to compare station
          data to climate BC data for that location, or view the location of stations on a map with a large difference from the model."),
              br(),
              actionButton("startBut", "Let's Get Started!"),
              br(),
              p("Please note that this tool is still being developed and will likely change frequently. While this information is accurate to the best
          of our knowledge, it has not been officially approved by the BC Government."),
              p("Site author: Kiri Daust - please send bug reports or suggestions to kiri.daust@gov.bc.ca")
            )
            
            
          )
            ,
    
    tabPanel("Climate Maps",
             titlePanel("BC Climate Variable Maps"),
             h4("Select a GCM, scenario, future period, and climate variable to display a map 
                representation."),
             column(3,
                    pickerInput("mapMod","Select GCM", choices = mapMod.choose, multiple = F),
                    pickerInput("mapScn", "Select Future Climate Scenario", choices = mapScn.choose, multiple = F),
                    pickerInput("mapVar", "Select Climate Variable", choices = mapVar.choose, multiple = F),
                    br(),
                    pickerInput("futForDiff","Select Future Period for Difference", choices = c(2025,2055,2085), multiple = F)),
             column(9,
                    h3("Time Series Maps"),
                    plotOutput("climMap"),
                    h3("Change Map"),
                    plotOutput("climDiff"))),
    
    ###Tab 4: Station data
    tabPanel("StationData",
             fluidRow(
               column(7,
                      titlePanel("Station and climateBC comparison"),
                      p("Select one or more BGCs, climate stations within the BGCs, and variables of interest.
                        Note that climateBC data is from the 1951-1980 period to match with the station data. You can 
                        also view model data where station data is unavailable, declutter the data by only viewing stations
                        with a big difference from climateBC, and view density plots of the difference between station and model
                        data."),
                      pickerInput("StnBGC.pick",
                                  "Which BGC?",
                                  choices = stn.BGC,
                                  multiple = TRUE),
                      htmlOutput("selectStn"),
                      pickerInput("var.pick",
                                  "Select Variables",
                                  choices = stn.var,
                                  multiple = TRUE),
                      p("Show stations with difference > specified amount"),
                      numericInput("maxDiff","",
                                   value = 0, max = 100, min = 0),
                      actionButton("removeNA", "Include NAs"),
                      actionButton("showNorm","Show Standard Noramal Data"),
                      downloadButton('downloadStn', 'Download Data')),
               column(5,
                      titlePanel("Map of divergent stations"),
                      h4("Choose whether to investigate divergence by station/model or by station/mean for BGC, then
                         select variable of interest, and number of most divergent stations to display."),
                      pickerInput("diffType",
                                  "Select comparison type",
                                  choices = c("Mean","Model"),
                                  selected = "Model",
                                  multiple = FALSE),
                      pickerInput("stnDiffVar",
                                  "Select Variable to Investigate",
                                  choices = stnDiff.var,
                                  selected = "MAT",
                                  multiple = FALSE),
                      numericInput("diffAmountMap","Select number of most divergent stations to display",
                                   value = 5, max = 50, min = 0)
                      )
             ),
             fluidRow(
               column(7,
                      uiOutput("stnPlots"),
                      titlePanel("Density of Difference"),
                      plotOutput("diffDens")),
               column(5,
                      leafletOutput("stnDiffMap", height = "600px"))
             ))
  
)
             

####SERVER LOGIC#################
server <- function(input, output) {
  
  observeEvent(input$startBut, {
    shinyjs::hide("start")
    shinyjs::show("main")
  })
  
  chooseData <- reactive({
    dat <- read.csv(input$data, stringsAsFactors = FALSE)
    return(dat)
  })
  
  ###Choose time period (calls above function)
  timePer <- reactive({
    period.choose <- as.character(unique(chooseData()$period))
    return(period.choose)
  })
  
  ###Create UI for selecting climate summary and periods
  output$periodSelect <- renderUI({
    if(input$data == "ClimateSummaryCurrent_v11_5.6.csv"){
      dropdown(
        pickerInput("periodTS",
                    label = "Sequential Normal Periods",
                    choices = period.ts,
                    multiple = TRUE,
                    selected = NULL, options = list(`actions-box` = TRUE)),
        pickerInput("periodOther",
                    label = "Other Normal Periods",
                    choices = period.other,
                    multiple = TRUE,
                    selected = NULL, options = list(`actions-box` = TRUE)),
        circle = FALSE, label = "Period", status = "primary") 
    }else{
      pickerInput("periodOld",
                  label = "Normal Period",
                  choices = timePer(),
                  multiple = TRUE,
                  selected = NULL)
    }
  })
  
  ###function to create table of data
  createTable <- reactive({
    climSummary <- chooseData()
    selectBGC <- c(input$BGC.choose)
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    if(input$data == "ClimateSummaryCurrent_v11_5.6.csv"){
      selectPer <- c(input$periodTS, input$periodOther)
    }else{
      selectPer <- input$periodOld
    }
    
    
    if(length(selectVars) > 0){
      climSubset <- climSummary[climSummary$BGC %in% selectBGC & climSummary$period %in% selectPer,
                                c("BGC", "period","Var", selectVars)]
      futureSub <- futureSummary[futureSummary$BGC %in% selectBGC & futureSummary$period %in% selectPer &
                                   futureSummary$Scenario %in% input$Scenario, c("BGC", "period","Var", selectVars)]
      climSubset <- rbind(climSubset, futureSub)
    }
    
    if(length(selectBGC) > 0 & length(selectPer) > 0 & length(selectVars) > 0){
      molten <- melt(climSubset)
        reShape <- dcast(molten, period+Var+variable~BGC)
        reShape <- reShape[order(reShape$variable),]
        reShape <- reShape[,c(1,3,2,4:length(reShape))]
        colnames(reShape)[1:3] <- c("TimePeriod","ClimateVar","Statistic")
      reShape <- as.data.frame(reShape)
      return(reShape)
    }
  })
  
  
  output$table <- renderTable({
    climSummary <- chooseData()
    selectBGC <- c(input$BGC.choose)
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    if(input$data == "ClimateSummaryCurrent_v11_5.6.csv"){
      selectPer <- c(input$periodTS, input$periodOther)
    }else{
      selectPer <- input$periodOld
    }
    
    
    if(length(selectVars) > 0){
      climSubset <- climSummary[climSummary$BGC %in% selectBGC & climSummary$period %in% selectPer,
                                c("BGC", "period","Var", selectVars)]
      futureSub <- futureSummary[futureSummary$BGC %in% selectBGC & futureSummary$period %in% selectPer &
                                   futureSummary$Scenario %in% input$Scenario, c("BGC", "period","Var", selectVars)]
      climSubset <- rbind(climSubset, futureSub)
    }
    
    if(length(selectBGC) > 0 & length(selectPer) > 0 & length(selectVars) > 0){
      molten <- melt(climSubset)
      if(input$dataForm == "BGCs as Columns"){
        reShape <- dcast(molten, period+Var+variable~BGC)
        reShape <- reShape[order(reShape$variable),]
        reShape <- reShape[,c(1,3,2,4:length(reShape))]
        colnames(reShape)[1:3] <- c("TimePeriod","ClimateVar","Statistic")
      }else{
        reShape <- dcast(molten, BGC+Var+variable~period)
        reShape <- reShape[order(reShape$variable),]
        reShape <- reShape[,c(1,3,2,4:length(reShape))]
        colnames(reShape)[1:3] <- c("BGC","ClimateVar","Statistic")
      }
      
      reShape <- as.data.frame(reShape)
      return(reShape)
    }
   })
  ###Download data
  output$downloadTable <- downloadHandler(
    filename = "ClimateSummary.csv",
    content = function(file){
    write.csv(createTable(), file)
    }
  )
  
  ##Set up list to html list to catch plots
  output$plots <- renderUI({
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    plot_output_list <- lapply(1:length(selectVars), function(i){
      plotname <- paste("Plot", i, sep= "")
      plotOutput(plotname, height = "400px", width = "100%")
    })
    do.call(tagList, plot_output_list)
  })
  
  ###Loop through selected variables and create a graph for each
  for (i in 1:100){
    local({
      my_i <- i
      plotname <- paste("Plot", my_i, sep= "")
      
      output[[plotname]] <- renderPlot({
        selectBGC <- c(input$BGC.choose)
        selectVars <- c(input$annual, input$seasonal, input$monthly)
        selectPer <- c(input$periodTS, input$periodOther)
        name <- selectVars[my_i]
        if (length(selectVars) > 0 & length(selectPer) > 0 & length(selectBGC) > 0){
        data <- createTable()
        if(input$grType == "Bar" | input$grType == "Line"){
          graph <- data[data$ClimateVar == selectVars[my_i] & (data$Statistic == "mean" | data$Statistic == input$Error | data$Statistic == input$futError),]
          graph$Statistic <- ifelse(graph$Statistic != "mean", "StDev", "mean")
          graph <- graph[,-2]
          graph <- as.data.frame(graph)
          graph <- melt(graph)
          graph <- dcast(graph, variable+TimePeriod~Statistic)
          graph <- as.data.frame(graph)
          colnames(graph) <- c("BGC","Period","Mean","Error")
          graph <- graph[order(graph$Period),]
          if(input$grType == "Bar"){
            graph$Period <- as.factor(graph$Period)
            ggplot(graph, aes(x = BGC, y = Mean, fill = Period)) +
              geom_bar(position = position_dodge(), stat = "identity") +
              geom_errorbar(aes(ymin = Mean - Error, ymax = Mean + Error), width = 0.2, 
                            position = position_dodge(0.9))+ theme_bw() + 
              theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
              ggtitle(selectVars[my_i]) + labs(x = "BGC")
          }else{
            graph$Period <- as.character(graph$Period)
            graph$Period <- as.numeric(substring(graph$Period, first = 1, last = 4))
            ggplot(graph, aes(x = Period, y = Mean, colour = BGC))+
              geom_line()+
              geom_ribbon(aes(ymin = Mean - Error, ymax = Mean + Error), linetype = 2, alpha = 0.1)+
              theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
              ggtitle(selectVars[my_i]) + labs(x = "First Year of Normal Period", y = "")
          }
        }else{
          graph <- data[data$ClimateVar == selectVars[my_i] & data$Statistic %in% c("mean","max","min","10%","90%",input$Error,input$futError),]
          graph$Statistic <- ifelse(graph$Statistic %in% c(input$Error, input$futError), "StDev", graph$Statistic)
          graph <- graph[,-2]
          graph <- as.data.frame(graph)
          graph <- melt(graph)
          graph <- dcast(graph, variable+TimePeriod~Statistic)
          graph <- as.data.frame(graph)
          colnames(graph)[1:2] <- c("BGC","Period")
          graph <- graph[order(graph$Period),]
          
          graph$Period <- as.character(graph$Period)
          graph$Period <- as.numeric(substring(graph$Period, first = 1, last = 4))
          ggplot(graph, aes(x = Period, lower = `10%`, upper = `90%`, middle = mean, 
                            ymin = min, ymax = max, colour = BGC))+
            geom_boxplot(stat = "identity", position = "identity")+
            theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
            ggtitle(selectVars[my_i]) + labs(x = "First Year of Normal Period")
            
          
        }
                        
        }
        
       
      })
    })
  }
  
  
  ###prepare data for walter chart 
  walterPrep <- reactive({
      BGC <- input$BGC.choose[1]
      Period <- 2025
      if(Period %in% c(2025,2055,2085)){
        x1 <- futureSummary[futureSummary$BGC %in% BGC & futureSummary$period == Period & 
                              futureSummary$Scenario == "rcp85",]
      }else{
        x1 <- climSummary[climSummary$BGC %in% BGC & climSummary$period == Period,]
      }
      ppt=subset(x1,x1$Var=="mean")[c(paste("PPT0",1:9,sep=""),paste("PPT",10:12,sep=""))]	
      tmx=subset(x1,x1$Var=="mean")[c(paste("Tmax0",1:9,sep=""),paste("Tmax",10:12,sep=""))]	
      tmn=subset(x1,x1$Var=="mean")[c(paste("Tmin0",1:9,sep=""),paste("Tmin",10:12,sep=""))]	
      tmn2=subset(x1,x1$Var=="mean")[c(paste("DD_0_0",1:9,sep=""),paste("DD_0_",10:12,sep=""))]	
      tmn2[which(tmn2>0)]=-16
      tmn2[which(tmn2==0)]=15
      waltMat <- rbind(as.matrix(ppt),as.matrix(tmx),
                       as.matrix(tmn),as.matrix(tmn2))
      return(waltMat)

  })
  
  output$walterPlot <- renderPlot({
    if(!is.null(input$BGC.choose)){
      diagwl(walterPrep(), mlab="en", p3line=F, est=input$BGC.choose[1], per=2025)
    }
  })
  
  ###Create html list to catch station plots
  output$stnPlots <- renderUI({
    stnVars <- input$var.pick
    stnPlotOutput <- lapply(1:length(stnVars), function(j){
      plotname <- paste("StnPlot", j, sep= "")
      plotOutput(plotname, height = "400px", width = "100%")
    })
    do.call(tagList, stnPlotOutput)
  })
  
for (j in 1:50){
    
    local({
      my_j <- j
      stPrep <- reactive({
        modelSub <- modelDat[modelDat$STATION %in% input$stn.pick,c("STATION", input$var.pick[my_j])]
        colnames(modelSub) <- c("Station","Mean")
        modelSub$Type <- "Model"
        m.oldSub <- modelReg[modelReg$STATION %in% input$stn.pick,c("STATION", input$var.pick[my_j])]
        colnames(m.oldSub) <- c("Station","Mean")
        m.oldSub$Type <- "Model61-90"
        stationSub <- stationDat[stationDat$STATION %in% input$stn.pick, c("STATION",input$var.pick[my_j])]
        colnames(stationSub) <- c("Station","Mean")
        stationSub$Type <- "Station"
        dat <- rbind(modelSub,stationSub,m.oldSub)
        dat <- dat[order(dat$Station),]
        dat$Type <- as.factor(dat$Type)
        ##makeReactiveBinding(dat)
        return(dat)
      })
      
      
      
      plotname <- paste("StnPlot", my_j, sep= "")

      output[[plotname]] <- renderPlot({
        input$removeNA
        if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
          dat <- stPrep()
          dat2 <- dcast(dat, Station ~ Type, value.var = "Mean", fun.aggregate = mean)
          colnames(dat2)[1] <- "ID"
          dat2$Diff <- apply(dat2[,c("Model","Station")],1,FUN = function(x){(1-(min(x)/max(x)))*100})
          dat2$Diff[is.na(dat2$Diff)] <- 0
          dat <- dat[dat$Station %in% dat2$ID[dat2$Diff >= input$maxDiff],]
          
          if(input$removeNA %% 2 == 0){
            stNAs <- as.character(dat$Station[is.na(dat$Mean)])
            dat <- dat[!dat$Station %in% stNAs,]
          }
          if(input$showNorm %% 2 == 0){
            dat <- dat[dat$Type != "Model61-90",]
          }
          
        ggplot(dat, aes(x = Station, y = Mean, fill = Type)) +
          geom_bar(position = position_dodge(), stat = "identity", color = "black") +
          scale_fill_manual(values = c("Model" = "black","Station" = "grey","Model61-90" = "white"))+
          theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
          {if(length(dat$Station) > 12)theme(axis.text.x = element_text(angle = 90, hjust = 1))}+
          ggtitle(input$var.pick[my_j])
        }
        
      })
    })
  }
  
  
  
  ###function to clean station data
  stnDat <- reactive({
    stationSub <- stationDat[stationDat$STATION %in% input$stn.pick, c("STATION","Name","BGC","Latitude","Longitude","Elevation",stn.var)]
    stationSub <- stationSub[rowSums(stationSub[,-c(1:6)], na.rm = TRUE) != 0,]
    modelSub <- modelDat[modelDat$STATION %in% input$stn.pick,c("STATION", stn.var)]
    modelSub <- unique(modelSub)
    stnBoth <- merge(stationSub, modelSub, by = "STATION", suffixes = c("_Station","_Model"), all.x = TRUE)
    colnames(stnBoth)[1] <- "St_ID"
    stnBoth <- stnBoth[stnBoth$St_ID %in% unique(stnBoth$St_ID),]
    stnBoth <- stnBoth[order(stnBoth$Name),]
    return(stnBoth)
  })
  
  ###create plot with density distributions showing difference between station and model data
  output$diffDens <- renderPlot({
    if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
      stationSub <- stationDat[stationDat$STATION %in% input$stn.pick, c("STATION","Name",input$var.pick)]
      ##stationSub <- stationSub[rowSums(stationSub[,-c(1:2)], na.rm = TRUE) != 0,]
      modelSub <- modelDat[modelDat$STATION %in% input$stn.pick,c("STATION", input$var.pick)]
      modelSub <- unique(modelSub)
      stnBoth <- merge(stationSub, modelSub, by = "STATION", suffixes = c("_Station","_Model"), all.x = TRUE)
      colnames(stnBoth)[1] <- "St_ID"
      stnBoth <- melt(stnBoth, id.vars = c("St_ID","Name"))
      stnBoth$Type <- ifelse(grepl("Station",stnBoth$variable), "Station","Model")
      stnBoth$variable <- gsub("_Station|_Model","",stnBoth$variable)
      stnBoth <- stnBoth[!is.na(stnBoth$value),]
      diffFun <- function(x){
        return (x[1]-x[2])
      }
      temp <- dcast(stnBoth, St_ID + Name ~ variable, value.var = "value", fun.aggregate = diffFun)
      temp <- melt(temp, id.vars = c("St_ID","Name"))
      ggplot(temp)+
        stat_density(aes(x = value, y = ..scaled.., fill = variable), alpha = 0.25)+
        ##xlim(c(-1.5,1.5))+
        labs(x = "Difference", y = "Density")
    }
    
  })
  
  output$downloadStn <- downloadHandler(
    filename = "ClimStation.csv",
    content = function(file){
      write.csv(stnDat(), file, row.names = FALSE)
    }
  )
  
###create station summary plots
output$stnSumPlots <- renderUI({
  stnSumVars <- input$var.pick
  stnSumPlots <- lapply(1:length(stnSumVars), function(k){
    plotname <- paste("StnSumPlot", k, sep= "")
    plotOutput(plotname, height = "400px", width = "100%")
  })
  do.call(tagList, stnSumPlots)
})

for (k in 1:50){
  local({
    my_k <- k
    plotname <- paste("StnSumPlot", my_k, sep= "")
    
    output[[plotname]] <- renderPlot({
      BGC.select <- c(input$StnBGC.summ)
      stationSub <- stationDat[stationDat$BGC %in% BGC.select, c("BGC",input$var.pick[my_k])]
      if(length(BGC.select) > 0 & length(input$var.pick) > 0){
      colnames(stationSub) <- c("BGC","Value")
      stationSub$Type <- "Station"
      modelSub <- modelDat[modelDat$BGC %in% BGC.select,c("BGC", input$var.pick[my_k])]
      colnames(modelSub) <- c("BGC","Value")
      modelSub$Type <- "Model"
      
      dat <- rbind(modelSub,stationSub)
      dat <- dat[dat$Value > -998,]
      dat <- dat[order(dat$Type, dat$BGC),]
      dat$Mean <- ave(dat$Value, dat$BGC, dat$Type, FUN = mean)
      dat$SD <- ave(dat$Value, dat$BGC, dat$Type, FUN = sd)
      dat <- dat[,-c(2)]
      dat <- unique(dat)
      dat$Type <- as.factor(dat$Type)
      
      ggplot(dat, aes(x = BGC, y = Mean, fill = Type)) +
        geom_bar(position = position_dodge(), stat = "identity") +
        scale_fill_manual(values = c("Model" = "purple","Station" = "darkgreen"))+
        geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, 
                      position = position_dodge(0.9))+ theme_bw()+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        ggtitle(input$var.pick[my_k])
      }
    })
  })
}

##only show stations as choices that are in that BGC
output$selectStn <- renderUI({
  BGC <- input$StnBGC.pick
  BGC <- paste("stn.",BGC, sep = "")
  pickerInput("stn.pick","Select Stations", choices = c(stn.list[[BGC[1]]],stn.list[[BGC[2]]],stn.list[[BGC[3]]]), 
              multiple = TRUE, selected = NULL, options = list(`actions-box` = TRUE))
})

stDiffDat <- reactive({
  if(input$diffType == "Mean"){
    temp <- stnMeanDiff[,c("BGC","Longitude","Latitude","Elevation","Name",input$stnDiffVar)]
  }else{
    temp <- stnModDiff[,c("St_ID","Longitude","Latitude","Elevation","Name",input$stnDiffVar)]
  }
  colnames(temp)[6] <- "Var"
  temp <- temp[!is.na(temp$Var),]
  temp <- temp[order(abs(temp$Var), decreasing = TRUE),]
  temp$Order <- seq_along(temp$Var)
  temp <- temp[temp$Order < input$diffAmountMap,]
  return(temp)
})

output$stnDiffMap <- renderLeaflet({
  dat <- stDiffDat()
  if(input$diffType == "Mean"){
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addAwesomeMarkers(data = dat, ~Longitude, ~Latitude, ~BGC, icon = icons,
                          popup = paste0("<b>", dat$Name, "</b>", "<br>",
                                         "BGC: ", dat$BGC, "<br>",
                                         "Difference: ", dat$Var, "<br>",
                                         "Order #: ", dat$Order, "<br>",
                                         "Elevation: ", dat$Elevation, "m", "<br>"))
  }else{
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addAwesomeMarkers(data = dat, ~Longitude, ~Latitude, ~St_ID, icon = icons,
                        popup = paste0("<b>", dat$Name, "</b>", "<br>",
                                       "Difference: ", dat$Var, "<br>",
                                       "Order #: ", dat$Order, "<br>",
                                       "Elevation: ", dat$Elevation, "m", "<br>"))
  }
})

output$climMap <- renderPlot({
  dat <- mapData[,c("Longitude", "Latitude", "GCM", "Scenario", "Future", input$mapVar)]
  colnames(dat)[6] <- "Var"
  dat <- dat[dat[,6] > -2000,]
  dat <- dat[(dat$GCM %in% c(input$mapMod,"Current")) & (dat$Scenario %in% c(input$mapScn,"")),]
  dat <- unique(dat)
  ##browser()
  dat <- dcast(dat, Longitude + Latitude ~ Future, value.var = "Var")
  dat <- st_as_sf(dat, coords = c("Longitude","Latitude"), crs = 4326, agr = "constant")
  CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
  dat <- st_transform(dat, crs = CRS.albers)
  plot(dat, pch = 16, cex = 0.3, key.pos = 1)
})

output$climDiff <- renderPlot({
  dat <- mapData[,c("Longitude", "Latitude", "GCM", "Scenario", "Future", input$mapVar)]
  colnames(dat)[6] <- "Var"
  dat <- dat[dat[,6] > -2000,]
  dat <- dat[dat$GCM %in% c(input$mapMod, "Current") & (dat$Scenario %in% c(input$mapScn,"")) & dat$Future %in% c(2010,input$futForDiff),]
  dat <- unique(dat)
  dat <- dcast(dat, Longitude + Latitude ~ Future, value.var = "Var")
  colnames(dat)[3:4] <- c("Current","Future")
  dat$Diff <- dat$Future - dat$Current
  dat <- dat[,c("Longitude","Latitude","Diff")]
  dat <- st_as_sf(dat, coords = c("Longitude","Latitude"), crs = 4326, agr = "constant")
  CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
  dat <- st_transform(dat, crs = CRS.albers)
  plot(dat, pch = 16, cex = 0.3, key.pos = 1)
})


}

# Run the application 
shinyApp(ui = ui, server = server)