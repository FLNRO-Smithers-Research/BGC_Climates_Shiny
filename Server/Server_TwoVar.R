###Two variable plots###

observe({
  tempChoose <- zone.chooseBC
  if(input$includeWNAV2 == "Yes"){tempChoose <- zone.choose}
  updatePickerInput(session,"BGCZone.chooseV2",choices = tempChoose, selected = input$BGCZone.choose)
})

##select subzones for two-variable comparison
observe({
  t1 <- paste(input$BGCZone.chooseV2, collapse = "|")
  tempChoose <- BGC.chooseBC
  if(input$includeWNAV2 == "Yes"){tempChoose <- BGC.choose}
  szChoose <- tempChoose[grep(t1,tempChoose)]
  updatePickerInput(session,"sz.chooseV2",choices = szChoose,selected = szChoose)
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

createTwoVar <- reactive({
  if(!is.null(input$sz.chooseV2)){
    dat <- as.data.table(getCompDat())
    dat[,Zone := gsub("[[:lower:]]|[[:digit:]]","",bgc)]
    dat[,Zone := as.factor(gsub("_.*","",Zone))]
    tZone <- dat[,lapply(.SD,mean),by = .(Zone), .SDcols = -c("bgc","period","var")]
    
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
})

output$twovar <- renderPlot({
  print(createTwoVar())
}, height = 600)

output$download2Var <- downloadHandler(
  filename = function(){paste0("TwoVar_",input$yvar,"-",input$xvar,"_",input$compNormPer,".png")},
  content = function(file){
    ggsave(file,plot = createTwoVar(), device = "png", width = 7, height = 7, units = "in")
  }
)