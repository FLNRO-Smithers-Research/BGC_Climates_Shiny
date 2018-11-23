stn <- stn.list[["stn.SBSmc2"]]
vars <- c("Tmin_sm")

stationSub <- stationDat[stationDat$STATION %in% stn, c("STATION","Name",vars)]
stationSub <- stationSub[rowSums(stationSub[,-c(1:2)], na.rm = TRUE) != 0,]
modelSub <- modelDat[modelDat$STATION %in% stn,c("STATION", vars)]
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
plot(0,0, xlim = c(max(-2, min(temp[,-(1:2)], na.rm = TRUE)), min(3,max(temp[,-(1:2)], na.rm = TRUE))), ylim = c(0,2))
cols <- rainbow(length(vars))
j <- 1
for(i in vars){
  dat <- temp[,i]
  dat <- dat[!is.na(dat)]
  ##dat <- dat/sum(dat)
  lines(density(dat), col = cols[j])
  j <- j+1
}
legend("topleft", legend = vars, fill = cols)
lines(density(temp$TD[!is.na(temp$TD)]))
lines(density(temp$Tave_wt[!is.na(temp$Tave_wt)]))
lines(density(temp))
stnBoth <- stnBoth[stnBoth$St_ID %in% unique(stnBoth$St_ID),]
stnBoth <- stnBoth[order(stnBoth$Name),]

###########################################
modelSub <- modelDat[modelDat$STATION %in% stn,c("STATION", vars)]
colnames(modelSub) <- c("Station","Mean")
modelSub$Type <- "Model"
modelSub <- unique(modelSub)
stationSub <- stationDat[stationDat$STATION %in% stn, c("STATION",vars)]
colnames(stationSub) <- c("Station","Mean")
stationSub$Type <- "Station"
dat <- rbind(modelSub,stationSub)
dat <- dat[order(dat$Station),]


if(length(input$stn.pick) > 0 & length(input$var.pick) > 0){
  dat <- stPrep()
  dat2 <- dcast(dat, Station ~ Type, value.var = "Mean", fun.aggregate = mean)
  colnames(dat2)[1] <- "ID"
  dat2$Diff <- apply(dat2[,c("Model","Station")],1,FUN = function(x){(1-(min(x)/max(x)))*100})
  dat2$Diff[is.na(dat2$Diff)] <- 100
  dat <- dat[dat$Station %in% dat2$ID[dat2$Diff <= maxDiff],]
  if(input$removeNA %% 2 == 0){
    stNAs <- as.character(dat$Station[is.na(dat$Mean)])
    dat <- dat[!dat$Station %in% stNAs,]
  }
  ggplot(dat, aes(x = Station, y = Mean, fill = Type)) +
    geom_bar(position = position_dodge(), stat = "identity") +
    scale_fill_manual(values = c("Model" = "purple","Station" = "darkgreen"))+
    theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    {if(length(dat$Station) > 12)theme(axis.text.x = element_text(angle = 90, hjust = 1))}+
    ggtitle(input$var.pick[my_j])
}
dat$Type <- as.factor(dat$Type)