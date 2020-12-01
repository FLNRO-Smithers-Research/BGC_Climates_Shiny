###This shiny app imports a dataset of future and historic climate summaries, and allows choices
###for various different user selected statistics and graphics. 
###Kiri Daust, May 13 2020

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
require(foreach)
require(leaflet)
require(RPostgreSQL)
require(magrittr)
require(gridExtra)

###Read in climate summary data
drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
#con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "localhost", port = 5432, dbname = "bgc_climate_data")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "FLNRServer", port = 5432, dbname = "bgc_climate_data")
##read in zone map
# map <- st_read(dsn = "ZoneMap", layer = "bec11vsmall")
# map <- st_transform(map,crs = "+proj=longlat +datum=WGS84")
####Set up choices
BGC.choose <- dbGetQuery(con, "SELECT DISTINCT bgc from climsum_curr_v12 where bgc <> '' ORDER BY bgc")$bgc
BGC.chooseBC <- dbGetQuery(con, "SELECT DISTINCT bgc from climsum_curr_v12 where location = 'BC' AND bgc <> '' ORDER BY bgc")$bgc
period.choose <- dbGetQuery(con, "SELECT DISTINCT period FROM climsum_curr_v12")$period
period.ts <- c("1901 - 1930","1931 - 1960","1961 - 1990","1991 - 2019","2025","2055","2085")
period.other <- period.choose[!period.choose %in% period.ts]
stat.choose <- dbGetQuery(con, "SELECT DISTINCT var FROM climsum_curr_v12")$var
var.choose <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                         WHERE table_name = 'climsum_curr_v12'")[,1]
var.choose <- var.choose[!var.choose %in% c("period","var","bgc")]
monthly <- var.choose[grep("01|02|03|04|05|06|07|08|09|10|11|12", var.choose)]
seasonal <- var.choose[grep("_sp|_sm|_at|_wt", var.choose)]
seasonalShort <- seasonal[grep("PPT|RAD|Tave|Tmin|Tmax", seasonal)]
annual <- var.choose[!var.choose %in% c(monthly,seasonal)]
zone.choose <- dbGetQuery(con, "SELECT DISTINCT bgc FROM zonesum_curr_v12 ORDER BY bgc")$bgc
zone.chooseBC <- dbGetQuery(con, "SELECT DISTINCT bgc FROM zonesum_curr_v12 WHERE location = 'BC' ORDER BY bgc")$bgc
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
stn.BGC <- dbGetQuery(con, "SELECT DISTINCT bgc FROM st_summary ORDER BY bgc")[,1]
stn.var <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                         WHERE table_name = 'st_summary'")[-c(1:6),1]

stnDiff.var <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                         WHERE table_name = 'stmod_diff'")[-c(1:6),1]
mapVar.choose <- dbGetQuery(con, "SELECT column_name FROM information_schema.columns 
                         WHERE table_name = 'map_grid'")[-c(1:5),1]
mapMod.choose <- dbGetQuery(con, "SELECT DISTINCT gcm FROM map_grid ORDER BY gcm")[,1]
mapFut.choose <- c(2025,2055,2085)
mapScn.choose <- c("rcp45","rcp85")

stn.list <- list()
for(i in 1:length(stn.BGC)){
  temp <- dbGetQuery(con, paste("SELECT station from st_summary where bgc = '",stn.BGC[i],"'",sep = ""))[,1]
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

mapCol <- colorFactor(rainbow(length(stn.var)), domain = stn.var)

icons <- awesomeIcons(icon = "circle",  markerColor = "blue", iconColor = ~mapCol(Var), library = "fa")

####USER INTERFACE########################
ui <- navbarPage(title = "Biogeoclimatic Climate Summaries", theme = "css/bcgov.css",
    tabPanel("Select BGCs and Variables", ####First tab: Summary
            useShinyjs(),
            shinyjs::hidden(
              div(id = "main",
                  fluidRow(
                    column(2,
                           titlePanel("Select Input"), 
                           awesomeRadio("includeWNA",
                                        "Include WNA units?",
                                        choices = c("No", "Yes"),
                                        selected = "No",
                                        inline = TRUE),
                          
                           awesomeRadio("byZone",
                                        "Summarize by:",
                                        choices = c("Zone","Subzone/variant"),
                                        selected = "Subzone/variant",
                                        inline = TRUE),
                            htmlOutput("zoneSelect"),
                           htmlOutput("szSelect"),
                           htmlOutput("periodSelect"), ###Select periods (changes based on user input)

                             dropdown( ###Select variables
                             pickerInput("annual",
                                         "Select Annual Variables:",
                                         choices = annual,
                                         inline = FALSE,
                                         multiple = TRUE,
                                         selected = c("mat", "msp"), options = list(`actions-box` = TRUE)),
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
                                        choices = c("std.dev.Geo","std.dev.Ann"),
                                        selected = "std.dev.Geo",
                                        inline = TRUE),
                           awesomeRadio("ModSet",
                                        "Select GCM Set",
                                        choices = c("Reduced", "Full"),
                                        selected = "Reduced",
                                        inline = TRUE),
                           awesomeRadio("Scenario",
                                        "Select Future Scenario",
                                        choices = futScn,
                                        selected = "rcp45",
                                        inline = TRUE),
                           awesomeRadio("futError",
                                        "Select Future Error Type",
                                        choices = c("SD.Geo","SD.Mod"),
                                        selected = "SD.Geo",
                                        inline = TRUE),
                           awesomeRadio("grType",
                                        "Choose Graph Type",
                                        choices = c("Bar","Boxplot","Line"),
                                        selected = "Bar",
                                        inline = TRUE)
                    ),
                    radioTooltip(id = "Error", choice = "std.dev.Geo", title = "Geographical standard deviation (current)", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Error", choice = "std.dev.Ann", title = "Standard Deviation over time period (current)", placement = "right", trigger = "hover"),
                    radioTooltip(id = "ModSet", choice = "Full", title = "Summary based on all 15 GCMs in ClimateBC", placement = "right", trigger = "hover"),
                    radioTooltip(id = "ModSet", choice = "Reduced", title = "Summary based on 8 GCMs recommended by provincial climatologist", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Scenario", choice = "rcp45", title = "Conservative Emission Projection", placement = "right", trigger = "hover"),
                    radioTooltip(id = "Scenario", choice = "rcp85", title = "High Emission Projection", placement = "right", trigger = "hover"),
                    radioTooltip(id = "futError", choice = "SD.Mod", title = "Standard Deviation between future models", placement = "right", trigger = "hover"),
                    radioTooltip(id = "futError", choice = "SD.Geo", title = "Future geographical standard deviation averaged accross models", placement = "right", trigger = "hover"),
                    column(10,
                           titlePanel("Summary Figures"),
                           h4("ClimateBC Summary by BGC"),
                           plotOutput("sumPlots")
                  )
                )
              )
            ),
            div(
              id = "start",
              h3("Welcome to the Climate Attributes of Biogeoclimatic Units Webtool"),
              p("The primary purpose of this tool is to provide access to summarized climate data for Biogeoclimatic units and allow climatic comparisons between BGCs and different time periods"), 
              tags$b("Click here to select BGCs, Climate Variables, and TimePeriod."),
              actionButton("startBut", "Let's Get Started!"),
              br(),
              ("Goto the"), tags$b("View/Download Summary Tables"), ("tab to view and downloaded data in table form."),
              
              h4("Other Data Summaries"),
              ("The"), tags$b("Two Variable Graphic Comparison"),  ("tab shows selected BGC units position on two selected climate variables"),
              br(),
              ("The"), tags$b ("Climate Station Data"), ("tab contains tools for comparing PRISM climate station data to modelled climate surface data by BGC"),
              
            tags$hr(),
              
              p("This tool is still being developed and will likely change periodically in format and content. 
                Data sets will be periodically updated to reflect new Biogeoclimatic mapping or updates to the ClimateBC surface. 
                While this information is accurate to the best
          of our knowledge, it has not yet been officially reviewed."),

              p("Site Development: Kiri Daust - please send bug reports or formatting suggestions to kiri.daust@gov.bc.ca"),
              p("Content author: William MacKenzie - inquiries about data or BEC contact will.mackenzie@gov.bc.ca"),
              p("Citation: MacKenzie, W.H. and K. Daust. Climate Characteristics of Biogeoclimatic Units. ")
            )
            
            
          )
            ,
    tabPanel("View/Download Summary Tables",
             h3("Climate Data in Table Format"),
             p("Table data reflects the BGC, Variable, and Time Period selections made in Select Comparisons Tab"),
               
             p(tags$b("BGCv12 is current mapping version")),
             p(tags$b("ClimateBCv6.30 is the current climate surface data")),

            
             pickerInput("dataForm",
                         "Choose Data Format",
                         choices = c("BGCs as Columns","Timeperiods as Columns"),
                         selected = "BGCs as Columns",
                         multiple = FALSE),
             downloadButton('downloadTable', 'Download'),
             br(),
                          tableOutput("table"), 

             h4("Walter plot of BGC"),
             plotOutput("walterPlot")),
    
    tabPanel("Two Variable Graphic Comparison",
             h4("Initial input based on BGC Summary choices"),
             column(3,
                    awesomeRadio("includeWNAV2",
                                 "Include WNA units?",
                                 choices = c("Yes","No"),
                                 selected = "No",
                                 inline = TRUE),
                    h2("Select Zone: "),
                    htmlOutput("zoneSelectV2"),
                    htmlOutput("szSelectV2"),
                    pickerInput("yvar",
                                "Select Y Variable:",
                                choices = c(annual, seasonal),
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "mat"),
                    pickerInput("xvar",
                                "Select X Variable:",
                                choices = c(annual, seasonal),
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "map"),
                    pickerInput("compNormPer",
                                "Select Time Period:",
                                choices = period.ts,
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "1961 - 1990")
                    ),
             column(9,
                    h2("Two-Variable Plot"),
                    h4("Filled dots represent mean of each zone"),
                    plotOutput("twovar"))),
    ###Tab 4: Station data
    tabPanel("Climate Station Data",
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
                                  selected = "SBSdk",
                                  multiple = TRUE),
                      htmlOutput("selectStn"),
                      pickerInput("var.pick",
                                  "Select Variables",
                                  choices = stn.var,
                                  selected = "mat",
                                  multiple = TRUE),
                      p("Show stations with difference > specified amount"),
                      numericInput("maxDiff","",
                                   value = 0, max = 100, min = 0),
                      actionButton("removeNA", "Include NAs"),
                      actionButton("showNorm","Show Standard Normal Data"),
                      downloadButton('downloadStn', 'Download Data')),
               column(5,
                      titlePanel("Map of divergent stations"),
                      h4("Choose whether to investigate divergence by station/model or by station/mean for BGC, then
                         select variable of interest, and number of most divergent stations to display."),
                      awesomeRadio("mapType","Compare stations to individual locations or BGC mean", choices = c("Individual","Mean"),
                                   selected = "Individual")
               )
             ),
             fluidRow(
               column(7,
                      uiOutput("stnPlots"),
                      titlePanel("Density of Difference"),
                      plotOutput("diffDens")),
               column(5,
                      leafletOutput("stnDiffMap", height = "600px"))
             )),
    
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
                    plotOutput("climDiff")))

)
             

####SERVER LOGIC#################
server <- function(input, output) {
  ###########Setup######################
  observeEvent(input$startBut, {
    shinyjs::hide("start")
    shinyjs::show("main")
  })
  
  ###Choose time period (calls above function)
  timePer <- reactive({
    period.choose <- as.character(unique(chooseData()$period))
    return(period.choose)
  })
  
  output$zoneSelectV2 <- renderUI({
    tempChoose <- zone.chooseBC
    if(input$includeWNAV2 == "Yes"){tempChoose <- zone.choose}
    pickerInput(inputId = "BGCZone.chooseV2",###Select BGCs
                label = "Select Zones for Summary",
                choices = tempChoose, 
                selected = input$BGCZone.choose, multiple = TRUE)
  })
  
  ##select subzones for two-variable comparison
  output$szSelectV2 <- renderUI({
    t1 <- paste(input$BGCZone.chooseV2, collapse = "|")
    tempChoose <- BGC.chooseBC
    if(input$includeWNAV2 == "Yes"){tempChoose <- BGC.choose}
    szChoose <- tempChoose[grep(t1,tempChoose)]
    pickerInput("sz.chooseV2",
                label = "Select Subzones",
                choices = szChoose,
                multiple = TRUE,
                selected = szChoose, options = list(`actions-box` = TRUE))
  })
  
  getCompDat <- reactive({
    sz.pick <- input$sz.chooseV2
    if(input$compNormPer %in% c("2025","2055","2085")){datLoc <- "climsum_fut_v12"}
    else datLoc <- "climsum_curr_v12"
    selectBC <- "BC"
    if(input$includeWNAV2 == "Yes"){selectBC <- "US_AB"}
    dat <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc","period","var", input$xvar,input$yvar),collapse = ","),
                                  " FROM climsum_curr_v12 WHERE var = 'mean' AND period = '1961 - 1990' AND bgc IN ('",paste(sz.pick,collapse = "','"),"')"))
    dat <- as.data.table(dat)
    if(input$compNormPer != "1961 - 1990"){
      if(input$compNormPer %in% c("2025","2055","2085")){
        q1 <- paste0("SELECT ",paste(c("bgc", input$xvar,input$yvar),collapse = ","),
                     " FROM ",datLoc," WHERE var = 'mean' AND period = '",
                     input$compNormPer, "' AND bgc IN ('",paste(sz.pick,collapse = "','"),
                     "') AND scenario = 'rcp85' AND modset = 'Reduced'")
      }else{
        q1 <- paste0("SELECT ",paste(c("bgc", input$xvar,input$yvar),collapse = ","),
                     " FROM ",datLoc," WHERE var = 'mean' AND period = '",
                     input$compNormPer, "' AND bgc IN ('",paste(sz.pick,collapse = "','"),
                     "')")
      }
      dat2 <- dbGetQuery(con, q1)
      dat2 <- as.data.table(dat2)
      setnames(dat2, c("bgc","v1_fut","v2_fut"))
      
      dat <- dat[dat2, on = "bgc"]
    }
    dat
  })
  
  output$twovar <- renderPlot({
    if(!is.null(input$sz.chooseV2)){
      dat <- as.data.table(getCompDat())
      dat[,Zone := gsub("[[:lower:]]|[[:digit:]]","",bgc)]
      dat[,Zone := as.factor(gsub("_.*","",Zone))]
      tZone <- dat[,lapply(.SD,mean),by = .(Zone), .SDcols = -c("bgc","period","var")]
      ##browser()
      
      if(input$compNormPer == "1961 - 1990"){
        ggplot(dat, aes(x = get(input$xvar), y = get(input$yvar))) +
          geom_label(aes(label = bgc,colour = Zone)) +
          geom_point(data = tZone, aes(x = get(input$xvar), y = get(input$yvar), colour = Zone),size = 6)+
          theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none")+
          labs(x = input$xvar, y = input$yvar)
      }else{
        ggplot(dat, aes(x = get(input$xvar), y = get(input$yvar))) +
          geom_label(aes(label = bgc,colour = Zone)) +
          geom_point(data = tZone, aes(x = get(input$xvar), y = get(input$yvar), colour = Zone),size = 6)+
          geom_segment(aes(x = get(input$xvar), y = get(input$yvar),xend = v1_fut, yend = v2_fut),
                       arrow = arrow(length = unit(0.3,"cm")))+
          geom_segment(data = tZone, aes(x = get(input$xvar), y = get(input$yvar),xend = v1_fut, yend = v2_fut),
                       arrow = arrow(length = unit(0.3,"cm")), size = 2)+
          theme(panel.background = element_rect(fill = "white", colour = "black"), legend.position = "none")+
          labs(x = input$xvar, y = input$yvar)
      }
      
    }
    
  }, height = 600)
  
  ###Create UI for selecting climate summary and periods
  output$periodSelect <- renderUI({
      dropdown(
        pickerInput("periodTS",
                    label = "Sequential Normal Periods",
                    choices = period.ts,
                    multiple = TRUE,
                    selected = c('1961 - 1990', '1991 - 2019', '2055'), options = list(`actions-box` = TRUE)),
        pickerInput("periodOther",
                    label = "Other Normal Periods",
                    choices = period.other,
                    multiple = TRUE,
                    selected = NULL, options = list(`actions-box` = TRUE)),
        circle = FALSE, label = "Period", status = "primary") 
  })
  
  
  ##UI for selecting zones
  output$zoneSelect <- renderUI({
    tempChoose <- zone.chooseBC
    if(input$includeWNA == "Yes"){tempChoose <- zone.choose}
    pickerInput(inputId = "BGCZone.choose",###Select BGCs
                label = "Select Zones for Summary",
                choices = tempChoose, 
                selected = "IDF", multiple = TRUE)
  })
  
  ##UI for selecting subzones
  output$szSelect <- renderUI({
    if(input$byZone != "Zone"){
      t1 <- paste(input$BGCZone.choose, collapse = "|")
      tempChoose <- BGC.chooseBC
      if(input$includeWNA == "Yes"){tempChoose <- BGC.choose}
      szChoose <- tempChoose[grep(t1,tempChoose)]
      pickerInput("sz.choose",
                  label = "Select Subzones",
                  choices = szChoose,
                  multiple = TRUE,
                  selected = c('IDFdk3', 'IDFxh2'), options = list(`actions-box` = TRUE))
    }
  })
  
#############Table Summaries and Data Download#################### 
  getData <- function(){
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
      climSubset <- dbGetQuery(con, q1)
      futureSub <- dbGetQuery(con, q2)
      ##browser()
      climSubset <- rbind(climSubset, futureSub)
    }
    return(climSubset)
  }
  
  ###function to create table of data
  createTable <- reactive({
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    selectPer <- c(input$periodTS, input$periodOther)
    if(length(selectPer) > 0 & length(selectVars) > 0){
      climSubset <- getData()
      molten <- melt(climSubset)
      reShape <- dcast(molten, period+var+variable~bgc)
      reShape <- reShape[order(reShape$variable),]
      reShape <- reShape[,c(1,3,2,4:length(reShape))]
      colnames(reShape)[1:3] <- c("TimePeriod","ClimateVar","Statistic")
      reShape <- as.data.frame(reShape)
      return(reShape)
    }
  })
  
  
  output$table <- renderTable({
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    selectPer <- c(input$periodTS, input$periodOther)
    if(length(selectPer) > 0 & length(selectVars) > 0){
      climSubset <- getData()
      molten <- melt(climSubset)
      if(input$dataForm == "BGCs as Columns"){
        reShape <- dcast(molten, period+var+variable~bgc)
        reShape <- reShape[order(reShape$variable),]
        reShape <- reShape[,c(1,3,2,4:length(reShape))]
        colnames(reShape)[1:3] <- c("TimePeriod","ClimateVar","Statistic")
      }else{
        reShape <- dcast(molten, bgc+var+variable~period)
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
  
  summaryPlots <- reactive({
    plots <- list()
    selectVars <- c(input$annual, input$seasonal, input$monthly)
    selectPer <- c(input$periodTS, input$periodOther)
    for(i in 1:length(selectVars)){
      
      name <- selectVars[i]
      if (length(selectVars) > 0 & length(selectPer) > 0){
        data <- createTable()
        if(input$grType == "Bar" | input$grType == "Line"){
          graph <- data[data$ClimateVar == selectVars[i] & 
                          (data$Statistic == "mean" | data$Statistic == input$Error | data$Statistic == input$futError),]
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
          graph <- data[data$ClimateVar == selectVars[i] & data$Statistic %in% c("mean","max","min","10%","90%",input$Error,input$futError),]
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
          graph$BGC <- as.factor(graph$BGC)
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
    dat <- rbind(modelSub,stationSub,m.oldSub)
    dat <- dat[order(dat$station),]
    dat$Type <- as.factor(dat$Type)
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
          dat <- dat[,c("station","Type","bgc",input$var.pick[my_j])] %>% 
            set_colnames(c("Station","Type","BGC","Mean"))
          dat2 <- dcast(dat, Station + BGC ~ Type, value.var = "Mean", fun.aggregate = mean)
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
                                         paste(input$stn.pick,collapse = "','"),"')"))
    ##stationSub <- stationSub[rowSums(stationSub[,-c(1:6)], na.rm = TRUE) != 0,]
    modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station", input$var.pick),collapse = ","),
                                       " FROM stpoints_mod81 WHERE station IN ('",
                                       paste(input$stn.pick,collapse = "','"),"')"))
    modelSub <- unique(modelSub)
    stnBoth <- merge(stationSub, modelSub, by = "station", suffixes = c("_Station","_Model"), all.x = TRUE)
    colnames(stnBoth)[1] <- "St_ID"
    stnBoth <- stnBoth[stnBoth$St_ID %in% unique(stnBoth$St_ID),]
    stnBoth <- stnBoth[order(stnBoth$name),]
    return(stnBoth)
  })
  
  ###create plot with density distributions showing difference between station and model data
  output$diffDens <- renderPlot({
    if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
      stnBoth <- stnDat()[,-c(3:6)]
      stnBoth <- melt(stnBoth, id.vars = c("St_ID","name"))
      stnBoth$Type <- ifelse(grepl("Station",stnBoth$variable), "Station","Model")
      stnBoth$variable <- gsub("_Station|_Model","",stnBoth$variable)
      stnBoth <- stnBoth[!is.na(stnBoth$value),]
      diffFun <- function(x){
        return (x[1]-x[2])
      }
      temp <- dcast(stnBoth, St_ID + name ~ variable, value.var = "value", fun.aggregate = diffFun)
      temp <- melt(temp, id.vars = c("St_ID","name"))
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
        stNames <- dbGetQuery(con, "SELECT DISTINCT station,name FROM st_summary")
        if(input$mapType == "Mean"){
          modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc", currVar),collapse = ","),
                                             " FROM climsum_curr_v11 WHERE period = '1961 - 1990' AND bgc IN ('",
                                             paste(input$StnBGC.pick,collapse = "','"),"')"))
          colnames(modelSub)[2] <- "BGCMean"
          dat2 <- merge(stationSub, modelSub, by = "bgc", all.x = T)
          dat2$Diff <- dat2$BGCMean - dat2$Mean
        }else{
          modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station", "latitude","longitude","bgc", 
                                                               currVar),collapse = ","),
                                             " FROM stpoints_mod81 WHERE station IN ('",
                                             paste(input$stn.pick,collapse = "','"),"')"))
          colnames(modelSub)[c(1,5)] <- c("Station","Mean")
          modelSub$Type <- "Model"
          dat <- rbind(modelSub,stationSub)
          dat <- dat[order(dat$Station),]
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
    dat <- dat[dat$Diff != 999,]
    dat$Var <- as.factor(dat$Var)
    ##browser()
    if(is.data.frame(dat)){
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
  
  getMapData <- reactive({
    dat <- dbGetQuery(con, paste0("SELECT longitude,latitude,gcm,scenario,future,",input$mapVar," FROM map_grid"))
    colnames(dat)[6] <- "Var"
    dat <- dat[dat[,6] > -2000,]
    dat
  })
  
  output$climMap <- renderPlot({
    dat <- getMapData()
    dat <- dat[(dat$gcm %in% c(input$mapMod,"Current")) & (dat$scenario %in% c(input$mapScn,"")),]
    dat <- unique(dat)
    ##browser()
    dat <- dcast(dat, longitude + latitude ~ future, value.var = "Var")
    dat <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326, agr = "constant")
    plot(dat, pch = 16, cex = 0.3, key.pos = 1)
  })
  
  output$climDiff <- renderPlot({
    dat <- getMapData()
    dat <- dat[dat$gcm %in% c(input$mapMod, "Current") & (dat$scenario %in% c(input$mapScn,"")) & dat$future %in% c(2010,input$futForDiff),]
    dat <- unique(dat)
    dat <- dcast(dat, longitude + latitude ~ future, value.var = "Var")
    colnames(dat)[3:4] <- c("Current","Future")
    dat$Diff <- dat$Future - dat$Current
    dat <- dat[,c("longitude","latitude","Diff")]
    dat <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326, agr = "constant")
    plot(dat, pch = 16, cex = 0.3, key.pos = 1)
  })
  
  onStop(function() {
    dbDisconnect(conn = con)
  })


}

# Run the application 
shinyApp(ui = ui, server = server)


##not currently used
###create station summary plots
# output$stnSumPlots <- renderUI({
#   stnSumVars <- input$var.pick
#   stnSumPlots <- lapply(1:length(stnSumVars), function(k){
#     plotname <- paste("StnSumPlot", k, sep= "")
#     plotOutput(plotname, height = "400px", width = "100%")
#   })
#   do.call(tagList, stnSumPlots)
# })
# 
# for (k in 1:50){
#   local({
#     my_k <- k
#     plotname <- paste("StnSumPlot", my_k, sep= "")
#     
#     output[[plotname]] <- renderPlot({
#       BGC.select <- c(input$StnBGC.summ)
#       stationSub <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc",input$var.pick[my_k]),collapse = ","),
#                                            " FROM st_summary WHERE bgc IN ('",
#                                            paste(BGC.select,collapse = "','"),"')"))
#       if(length(BGC.select) > 0 & length(input$var.pick) > 0){
#         colnames(stationSub) <- c("BGC","Value")
#         stationSub$Type <- "Station"
#         modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("bgc",input$var.pick[my_k]),collapse = ","),
#                                            " FROM stpoints_mod81 WHERE bgc IN ('",
#                                            paste(BGC.select,collapse = "','"),"')"))
#         colnames(modelSub) <- c("BGC","Value")
#         modelSub$Type <- "Model"
#         
#         dat <- rbind(modelSub,stationSub)
#         dat <- dat[dat$Value > -998,]
#         dat <- dat[order(dat$Type, dat$BGC),]
#         dat$Mean <- ave(dat$Value, dat$BGC, dat$Type, FUN = mean)
#         dat$SD <- ave(dat$Value, dat$BGC, dat$Type, FUN = sd)
#         dat <- dat[,-c(2)]
#         dat <- unique(dat)
#         dat$Type <- as.factor(dat$Type)
#         
#         ggplot(dat, aes(x = BGC, y = Mean, fill = Type)) +
#           geom_bar(position = position_dodge(), stat = "identity") +
#           scale_fill_manual(values = c("Model" = "purple","Station" = "darkgreen"))+
#           geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, 
#                         position = position_dodge(0.9))+ theme_bw()+
#           theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
#           ggtitle(input$var.pick[my_k])
#       }
#     })
#   })
# }

# output$znMap <- renderLeaflet({
#   leaflet(data = map) %>%
#     addPolygons(label = ~ Zone,
#                 layerId = map$Zone,
#                 fillColor = topo.colors(16, alpha = NULL),
#                 stroke = F,
#                 highlightOptions = highlightOptions(color = "white",
#                                                     weight = 2,
#                                                     bringToFront = TRUE))
# })

