###This shiny app imports a dataset of future and historic climate summaries, and allows choices
###for various different user selected statistics and graphics. 
###Kiri Daust, May 13 2020

require(shiny)
require(shinyWidgets)
require(ggplot2)
#require(vegan)
require(shinythemes)
#require(devtools)
require(leaflet)
require(data.table)
require(sf)
require(climatol)
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
con <- dbConnect(drv, user = "postgres", password = "postgres", host = "138.197.168.220", 
                 port = 5432, dbname = "bgc_climate_data") ##  eXTERNAL USE

####Set up choices
BGC.choose <- dbGetQuery(con, "SELECT DISTINCT bgc from szsum_curr where bgc <> '' ORDER BY bgc")$bgc
BGC.chooseBC <- dbGetQuery(con, "SELECT DISTINCT bgc from szsum_curr where region = 'BC' AND bgc <> '' ORDER BY bgc")$bgc
period.choose <- dbGetQuery(con, "SELECT DISTINCT period FROM szsum_curr")$period
fp.choose <- dbGetQuery(con, "SELECT DISTINCT period FROM zonesum_fut")$period
period.ts <- c("1901 - 1930","1931 - 1960","1961 - 1990","1991 - 2020","2021-2040",
               "2041-2060","2061-2080","2081-2100")
period.other <- period.choose[!period.choose %in% period.ts]
stat.choose <- dbGetQuery(con, "SELECT DISTINCT stat FROM szsum_curr")$var
var.choose <- dbGetQuery(con, "SELECT distinct climvar from zonesum_curr")[,1]
monthly <- var.choose[grep("01|02|03|04|05|06|07|08|09|10|11|12", var.choose)]
seasonal <- var.choose[grep("_sp|_sm|_at|_wt", var.choose)]
seasonalShort <- seasonal[grep("PPT|RAD|Tave|Tmin|Tmax", seasonal)]
annual <- var.choose[!var.choose %in% c(monthly,seasonal)]
zone.choose <- dbGetQuery(con, "SELECT DISTINCT bgc FROM zonesum_curr ORDER BY bgc")$bgc
zone.chooseBC <- dbGetQuery(con, "SELECT DISTINCT bgc FROM zonesum_curr WHERE region = 'BC' ORDER BY bgc")$bgc
annualDirect <- c("MAT","MWMT","MCMT","TD","MAP","MSP","AHM","SHM")
futScn <- dbGetQuery(con,"SELECT DISTINCT scenario from zonesum_fut")[,1]
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

load("./inputs/StationList.Rdata")
# stn.list <- list()
# for(i in 1:length(stn.BGC)){
#   temp <- dbGetQuery(con, paste("SELECT station from st_summary where bgc = '",stn.BGC[i],"'",sep = ""))[,1]
#   name <- paste("stn.",stn.BGC[i],sep = "")
#   stn.list[[name]] <- temp
# }

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
                           pickerInput(inputId = "BGCZone.choose",###Select BGCs
                                       label = "Select Zones for Summary",
                                       choices = zone.chooseBC, 
                                       selected = "IDF", multiple = TRUE),
                           hidden(pickerInput("sz.choose",
                                              label = "Select Subzones",
                                              choices = "",
                                              multiple = TRUE,
                                              options = list(`actions-box` = TRUE))),
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
                             circle = FALSE, label = "Period", status = "primary"), 

                             dropdown( ###Select variables
                             pickerInput("annual",
                                         "Select Annual Variables:",
                                         choices = annual,
                                         inline = FALSE,
                                         multiple = TRUE,
                                         selected = c("MAT", "MSP"), options = list(`actions-box` = TRUE)),
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
                                        inline = TRUE),
                           awesomeRadio("ModSet",
                                        "Select GCM Set",
                                        choices = c("Reduced", "Full"),
                                        selected = "Reduced",
                                        inline = TRUE),
                           awesomeRadio("Scenario",
                                        "Select Future Scenario",
                                        choices = futScn,
                                        selected = "ssp585",
                                        inline = TRUE),
                           awesomeRadio("futError",
                                        "Select Future Error Type",
                                        choices = c("SD.Mod","SD.Geo"),
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
                           downloadButton("downloadSumPlots",label = "Download Plots"),
                           plotOutput("sumPlots")
                  )
                )
              )
            ),
            div(
              id = "start",
              h3("Welcome to the Climate Attributes of Biogeoclimatic Units Webtool"),
              p("The primary purpose of this tool is to provide access to summarized climate data for Biogeoclimatic units and allow climatic comparisons between BGCs and different time periods"),
              h4("Climate Summaries"),
              p("1. User selects BGCs, Climate Variables, and TimePeriods of Interest."),
              p("Graphical comparisons of selections can be viewed and downloaded"),
              p(""),
              actionButton("startBut", tags$b("Click here to create Climate Summaries")),
              br(),
              p(""),
              ("2. Data tables of the selected summary can then be downloaded from the "), tags$b("View/Download Summary Tables"), ("tab."),
              p(""),
              h4("Other Data Summaries"),
              ("The"), tags$b("Two Variable Graphic Comparison"),  ("tab shows selected BGC units position on two selected climate variables"),
              br(),
              p(""),
              ("The"), tags$b ("Climate Station Data"), ("tab contains tools for comparing PRISM climate station data to modelled climate surface data by BGC"),
              
            tags$hr(),
              
              p("This tool is still being developed and will likely change periodically in format and content. 
              Data sets will be periodically updated to reflect new Biogeoclimatic mapping or updates to the ClimateBC surface. 
              While this information is accurate to the best of our knowledge, it has not yet been officially reviewed."),
              p(tags$b("The current version is based on ClimateBC/NA v6.30 data and BECv11 mapping.")),
            
              p("Site Development: Kiri Daust - please send bug reports or formatting suggestions to kiri.daust@gov.bc.ca"),
              p("Content author: William H. MacKenzie - inquiries about data or BEC contact will.mackenzie@gov.bc.ca"),
              p("Citation: MacKenzie, W.H., K. Daust, H. Griesbauer, & E. Matsuzaki. 2021. Climatic Characteristics of Biogeoclimatic Units. Shiny App https://becweb.shinyapps.io/BGC_Climates_Shiny/  ")
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
             h5("Initial input based on BGC Summary choices"),
             column(3,
                    awesomeRadio("includeWNAV2",
                                 "Include WNA units?",
                                 choices = c("Yes","No"),
                                 selected = "No",
                                 inline = TRUE),
                    h2("Select Zone: "),
                    pickerInput(inputId = "BGCZone.chooseV2",###Select BGCs
                                label = "Select Zones for Summary",
                                choices = zone.chooseBC, 
                                selected = "", multiple = TRUE),
                    pickerInput("sz.chooseV2",
                                label = "Select Subzones",
                                choices = "",
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE)),
                    pickerInput("yvar",
                                "Select Y-axis variable:",
                                choices = c(annual, seasonal),
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "mat"),
                    pickerInput("xvar",
                                "Select X-axis variable:",
                                choices = c(annual, seasonal),
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "map"),
                    pickerInput("compNormPer",
                                "Select Past or Future Time Period (compares to 1961-90) :",
                                choices = period.ts,
                                inline = FALSE,
                                multiple = FALSE,
                                selected = "1961 - 1990")
                    ),
             column(9,
                    h3("Two-Variable Plot"),
                    h5("Filled dots represent mean of selected subzone/variants"),
                    downloadButton("download2Var", label = "Download Plot"),
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
             ))

)
####SERVER LOGIC#################
server <- function(input, output, session) {
  ###########Setup######################
  observeEvent(input$startBut, {
    shinyjs::hide("start")
    shinyjs::show("main")
  })
  
  source("./Server/Server_Summaries.R",local = T)
  source("./Server/Server_TwoVar.R",local = T)
  source("./Server/Server_Station.R",local = T)

  onStop(function() {
    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(conn = con)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

# getMapData <- reactive({
#   dat <- dbGetQuery(con, paste0("SELECT longitude,latitude,gcm,scenario,future,",input$mapVar," FROM map_grid"))
#   colnames(dat)[6] <- "Var"
#   dat <- dat[dat[,6] > -2000,]
#   dat
# })
# 
# output$climMap <- renderPlot({
#   dat <- getMapData()
#   dat <- dat[(dat$gcm %in% c(input$mapMod,"Current")) & (dat$scenario %in% c(input$mapScn,"")),]
#   dat <- unique(dat)
#   ##browser()
#   dat <- dcast(dat, longitude + latitude ~ future, value.var = "Var")
#   dat <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326, agr = "constant")
#   plot(dat, pch = 16, cex = 0.3, key.pos = 1)
# })
# 
# output$climDiff <- renderPlot({
#   dat <- getMapData()
#   dat <- dat[dat$gcm %in% c(input$mapMod, "Current") & (dat$scenario %in% c(input$mapScn,"")) & dat$future %in% c(2010,input$futForDiff),]
#   dat <- unique(dat)
#   dat <- dcast(dat, longitude + latitude ~ future, value.var = "Var")
#   colnames(dat)[3:4] <- c("Current","Future")
#   dat$Diff <- dat$Future - dat$Current
#   dat <- dat[,c("longitude","latitude","Diff")]
#   dat <- st_as_sf(dat, coords = c("longitude","latitude"), crs = 4326, agr = "constant")
#   plot(dat, pch = 16, cex = 0.3, key.pos = 1)
# })