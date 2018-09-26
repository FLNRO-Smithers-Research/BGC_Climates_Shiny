###This shiny app imports a dataset of future and historic climate summaries, and allows choices
###for various different user selected statistics and graphics. 
###Kiri Daust, April 2018

.libPaths("E:/R packages351")
require(shiny)
require(reshape)
require(shinyWidgets)
require(ggplot2)
require(climatol)
require(vegan)
require(shinythemes)
require(openxlsx)
require(gganimate)
require(ggConvexHull)
require(devtools)
require(animation)
require(magick)

###Read in climate summary data
climSummary <- read.csv("ClimateSummaryCurrent_v11_5.6.csv", stringsAsFactors = FALSE)
futureSummary <- read.csv("ClimateSummary_Future_v11_5.6.csv", stringsAsFactors = FALSE)
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
modelDat <- read.xlsx("ModelAtStationNormals.xlsx")
stationDat <- read.xlsx("ClimateStationNormals.xlsx")
modelDat <- merge(stationDat[,1:2], modelDat, by.x = "STATION",by.y = "ID1")
stationDat$BGC <- as.character(stationDat$BGC)
stationDat$STATION <- as.character(stationDat$STATION)
stn.BGC <- unique(stationDat$BGC)
stn.BGC <- sort(stn.BGC)
stn.var <- colnames(stationDat)[-c(1:2)]
stn.var <- sort(stn.var)

stn.list <- list()
for(i in 1:length(stn.BGC)){
  temp <- stationDat$STATION[stationDat$BGC == stn.BGC[i]]
  name <- paste("stn.",stn.BGC[i],sep = "")
  stn.list[[name]] <- temp
}

####USER INTERFACE########################
ui <- fluidPage(theme = shinytheme("slate"),
  tabsetPanel(
    tabPanel("Summary", ####First tab: Summary
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
                      uiOutput("plots"))
             )
    ),
    
    ###Tab 2: walter chart
    tabPanel("WalterChart", 
             fluidRow(
               column(2,
                        pickerInput("walterBGC",
                                    "Select 1 BGC",
                                    choices = BGC.list),
                      
                      dropdownButton(
                        radioButtons("walterPeriod",
                                     "Select 1 Period:",
                                     choices = period.ts), circle = FALSE, label = "Period")
                      
                      ),
               
               column(10,
                      titlePanel("Walter Figure"),
                      plotOutput("walterPlot", height = "500px", width = "600px"),
                      downloadButton('walterDown', 'Download Figure'),
                      dropdownButton(
                        radioButtons("wType",
                                     "Select Download Format:",
                                     choices = c("pdf","png")), circle = FALSE, label = "Download Type"))
             )
    ),
    
    ###Tab 3: Ordinations
    tabPanel("nMDS_Chart",
             fluidRow(
               column(6,
                      titlePanel("nMDS Plot"),
                      dropdownButton(
                        checkboxGroupInput("mdsZone",
                                           "Select > 1 Zone:",
                                           choices = zone.choose), circle = FALSE, label = "Zones"),
                      dropdownButton(
                        radioButtons("mdsPeriod",
                                     "Select 1 Period:",
                                     choices = period.choose), circle = FALSE, label = "Period"),
                      awesomeRadio("vectors",
                                   "Display Eigen Vectors?",
                                   choices = c("No", "Yes"),
                                   selected = "No"),
                      
                      plotOutput("mdsPlot", height = "500px", width = "600px"),
                      downloadButton('mdsDown', 'Download Figure'),
                      dropdownButton(
                        radioButtons("mType",
                                     "Select Download Format:",
                                     choices = c("pdf","png")), circle = FALSE, label = "Download Type")
                      
             ),
             column(6,
                    titlePanel("Animated PCA"),
                    pickerInput("zonePCA",
                                "Select zones for PCA",
                                choices = zone.choose,
                                multiple = TRUE,
                                selected = NULL),
                    pickerInput("periodPCA",
                                label = "Select periods for PCA animation",
                                choices = period.ts,
                                multiple = TRUE,
                                selected = NULL, options = list(`actions-box` = TRUE)),
                    imageOutput("climPCA", height = "500px", width = "600px"))
             )
    ),
    
    ###Tab 4: Station data
    tabPanel("StationData",
             fluidRow(
               column(7,
                      titlePanel("Individual station and modelled data (1951-1980)"),
                      pickerInput("StnBGC.pick",
                                  "Which BGC?",
                                  choices = stn.BGC,
                                  multiple = TRUE),
                      htmlOutput("selectStn"),
                      pickerInput("var.pick",
                                  "Select Variables",
                                  choices = stn.var,
                                  multiple = TRUE),
                      downloadButton('downloadStn', 'Download Data')),
               column(5,
                      titlePanel("Summarised by BGC"),
                      pickerInput("StnBGC.summ",
                                  "Select additional BGCs for Summary",
                                  choices = stn.BGC,
                                  multiple = TRUE)
                      )
             ),
             fluidRow(
               column(7,
                      uiOutput("stnPlots")),
               column(5,
                      uiOutput("stnSumPlots"))
             ))
    
  )
  
)
             

####SERVER LOGIC#################
server <- function(input, output) {
  
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
        reShape <- cast(molten, period+Var+variable~BGC)
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
        reShape <- cast(molten, period+Var+variable~BGC)
        reShape <- reShape[order(reShape$variable),]
        reShape <- reShape[,c(1,3,2,4:length(reShape))]
        colnames(reShape)[1:3] <- c("TimePeriod","ClimateVar","Statistic")
      }else{
        reShape <- cast(molten, BGC+Var+variable~period)
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
          graph <- cast(graph, variable+TimePeriod~Statistic)
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
          graph <- cast(graph, variable+TimePeriod~Statistic)
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
    BGC <- input$walterBGC
    Period <- input$walterPeriod
    if(Period %in% c(2025,2055,2085)){
      x1 <- futureSummary[futureSummary$BGC == BGC & futureSummary$period == Period & 
                          futureSummary$Scenario == "rcp85",]
    }else{
      x1 <- climSummary[climSummary$BGC == BGC & climSummary$period == Period,]
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
    diagwl(walterPrep(), mlab="en", p3line=F, est=input$walterBGC, per=input$walterPeriod)
  })
  
  ##Prepare for ordination
  mdsPrep <- reactive({
    climOrd <- climSummary
    climOrd$Zone <- gsub("[[:lower:]]", "", climOrd$BGC)
    climOrd$Zone <- gsub("[[:digit:]]", "", climOrd$Zone)
    climOrd$Zone <- gsub("[[:blank:]]", "", climOrd$Zone)
    climOrd <- climOrd[,c(length(climOrd),1:(length(climOrd)-1))]
    climOrd <- climOrd[climOrd$period == input$mdsPeriod & climOrd$Var == "mean",]
    climOrd <- climOrd[climOrd$Zone %in% input$mdsZone,]
    return(climOrd)
  })
  
  output$mdsPlot <- renderPlot({
    if(length(input$mdsZone) >= 2){
      climOrd <- mdsPrep()
      MDS <- metaMDS(climOrd[,annual], distance = "euclidian", k = 2, trymax = 2000)
      vectors <- envfit(MDS, climOrd[,annual], permutations = 999)
      MDS.df <- as.data.frame(scores(MDS, display = "sites"))
      MDS.df <- cbind(climOrd$Zone, MDS.df)
      colnames(MDS.df)[1] <- "Zone"
      vect.df <- as.data.frame(scores(vectors, display = "vectors"))
      vect.df <- cbind(vect.df, Pval = vectors$vectors$pvals)
      vect.df$ClimVar <- rownames(vect.df)
      vect.df[,1:2] <- vect.df[,1:2]*(max(MDS.df[,2:3])*0.5)
      
      ggplot(MDS.df)+
        geom_point(mapping = aes(x = NMDS1, y = NMDS2, colour = Zone), size = 3, shape = 17)+
        coord_fixed()+
        {if(input$vectors == "Yes") geom_segment(data = vect.df[vect.df$Pval < 0.01,], aes(x = 0, xend = NMDS1, y = 0, yend  = NMDS2),
                                         arrow = arrow(length = unit(0.25, "cm")), colour = "purple",linetype = "dashed")}+
        {if(input$vectors == "Yes") geom_text(data = vect.df[vect.df$Pval < 0.01,], aes(x = NMDS1, y = jitter(NMDS2, amount = 0.1), label = ClimVar))}+
        theme(panel.background = element_rect(fill = "white", colour = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    }
  })
  
  ##Download walter chart (choose pdf or png)
  output$walterDown <- downloadHandler(
    filename = function(){
      paste("WalterFigure", input$wType, sep = ".")
    },
    content = function(file){
      if(input$wType == "pdf"){
        pdf(file)
      }
      if (input$wType == "png"){
        png(file)
      }
        
      diagwl(walterPrep(), mlab="en", p3line=F, est=input$walterBGC, per=input$walterPeriod)
      dev.off()
      }
  )
  
  output$mdsDown <- downloadHandler(
    filename = function(){
      paste("nMDS_Function", input$mType, sep = ".")
    },
    content = function(file){
      if(input$wType == "pdf"){
        pdf(file)
      }
      if (input$wType == "png"){
        png(file)
      }
      climOrd <- mdsPrep()
      MDS <- metaMDS(climOrd[,annual], distance = "euclidian", k = 2, trymax = 2000)
      plot(MDS, display = "sites")
      ordihull(MDS, climOrd$Zone, label = T)
      if (input$vectors == "Yes"){
        vectors <- envfit(MDS, climOrd[,annual], permutations = 999)
        plot(vectors, p.max = 0.01)
      }
      dev.off()
    }
  )
  
  ###Create html list to catch station plots
  output$stnPlots <- renderUI({
    stnVars <- input$var.pick
    stnPlotOutput <- lapply(1:length(stnVars), function(j){
      plotname <- paste("StnPlot", j, sep= "")
      plotOutput(plotname, height = "400px", width = "100%")
    })
    do.call(tagList, stnPlotOutput)
  })
  
  ###loop through selected variables
  for (j in 1:50){
    local({
      my_j <- j
      plotname <- paste("StnPlot", my_j, sep= "")
      
      output[[plotname]] <- renderPlot({
        modelSub <- modelDat[modelDat$STATION %in% input$stn.pick,c("STATION", input$var.pick[my_j])]
        if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
        colnames(modelSub) <- c("Station","Mean")
        modelSub$Type <- "Model"
        stationSub <- stationDat[stationDat$STATION %in% input$stn.pick, c("STATION",input$var.pick[my_j])]
        colnames(stationSub) <- c("Station","Mean")
        stationSub$Type <- "Station"
        dat <- rbind(modelSub,stationSub)
        dat <- dat[order(dat$Station),]
        dat$Type <- as.factor(dat$Type)
        ggplot(dat, aes(x = Station, y = Mean, fill = Type)) +
          geom_bar(position = position_dodge(), stat = "identity") +
          scale_fill_manual(values = c("Model" = "purple","Station" = "darkgreen"))+
          theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
          {if(length(dat$Station) > 12)theme(axis.text.x = element_text(angle = 90, hjust = 1))}+
          ggtitle(input$var.pick[my_j])
        }
        
      })
    })
  }
  
  ###function to clean station data
  stnDat <- reactive({
    modelSub <- modelDat[modelDat$STATION %in% input$stn.pick,c("STATION", "BGC", input$var.pick)]
    colnames(modelSub)[1:2] <- c("Station","BGC")
    modelSub$Type <- "Model"
    stationSub <- stationDat[stationDat$STATION %in% input$stn.pick, c("STATION","BGC", input$var.pick)]
    colnames(stationSub)[1:2] <- c("Station","BGC")
    stationSub$Type <- "Station"
    dat <- rbind(modelSub,stationSub)
    datLong <- melt(dat, variable_name = "ClimVar")
    dat <- cast(datLong, Station+BGC+ClimVar~Type)
    dat <- as.data.frame(dat)
    dat <- dat[order(dat$ClimVar, dat$BGC),]
    return(dat)
  })

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

output$downloadStn <- downloadHandler(
  filename = "ClimStation.csv",
  content = function(file){
    write.csv(stnDat(), file)
  }
)

##animated PCA
output$climPCA <- renderImage({
  outfile <- tempfile(fileext='.gif')
  
  if(length(input$periodPCA) > 0 & length(input$zonePCA) > 0){
  climOrd <- climSummary
  climOrd <- rbind(climOrd, futureSummary[futureSummary$Scenario == "rcp85",-3])
  climOrd$Zone <- gsub("[[:lower:]]", "", climOrd$BGC)
  climOrd$Zone <- gsub("[[:digit:]]", "", climOrd$Zone)
  climOrd$Zone <- gsub("[[:blank:]]", "", climOrd$Zone)
  climOrd <- climOrd[,c(length(climOrd),1:(length(climOrd)-1))]
  climOrd <- climOrd[climOrd$period %in% input$periodPCA & climOrd$Var == "mean",]
  climOrd <- climOrd[climOrd$Zone %in% input$zonePCA,]
  climOrd <- climOrd[,c("Zone","period",annual, seasonalShort)]
  climPCA <- prcomp(climOrd[,-c(1:2)], scale. = TRUE)
  dfPCA <- data.frame(climPCA$x, Zone = climOrd$Zone, Year = climOrd$period)
  
  p <- ggplot(dfPCA, aes(PC1,PC2, col = Zone, frame = Year))+
    geom_point(size = 2)+
    geom_convexhull(aes(colour = Zone), alpha = 0.3)+
    coord_fixed()
  
  gganimate(p, "outfile.gif")
  
  list(src = "outfile.gif",
       contentType = 'image/gif')
  }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)