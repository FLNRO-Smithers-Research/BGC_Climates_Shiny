library(tcltk)
library(reshape2)
setwd(tk_choose.dir())

modelDat <- read.csv("StPoints_Model81-10.csv")
modelReg <- read.csv("StPoints_Model61-90.csv")
stationDat <- read.csv("StationSummary.csv")
stationDat <- merge(modelDat[,1:5], stationDat, by = "STATION", all = FALSE)
stationDat <- unique(stationDat)
stationDat$BGC <- as.character(stationDat$BGC)
stationDat$STATION <- as.character(stationDat$STATION)

stationSub <- stationDat[,-c(2:5)]
stationSub <- unique(stationSub)
#stationSub$Type <- "Station"
modelSub <- modelDat
modelSub$STATION <- as.character(modelSub$STATION)
modelSub <- modelSub[!duplicated(modelSub$STATION),]
modelSub <- modelSub[,names(modelSub) %in% names(stationSub)]
stnBoth <- merge(stationSub, modelSub, by = "STATION", suffixes = c("_Station","_Model"), all.x = TRUE)
colnames(stnBoth)[1] <- "St_ID"
stnBoth <- melt(stnBoth, id.vars = c("St_ID","Name"))
stnBoth$Type <- ifelse(grepl("Station",stnBoth$variable), "Station","Model")
stnBoth$variable <- gsub("_Station|_Model","",stnBoth$variable)
stnBoth <- stnBoth[!is.na(stnBoth$value),]
diffFun <- function(x){
  return (x[1]-x[2])
}
stnBoth$value <- as.numeric(stnBoth$value)
temp <- dcast(stnBoth, St_ID + Name ~ variable, value.var = "value", fun.aggregate = diffFun)
write.csv(temp, "StnModelDiff.csv", row.names = FALSE)

diffDat <- temp
library(vegan)
diffDat[,-c(1:2)] <- abs(diffDat[,-c(1:2)])
diffDat[,-c(1:2)] <- decostand(diffDat[,-c(1:2)], MARGIN = 2, method = "standardize", na.rm = TRUE)
diffDat$Mean <- rowMeans(diffDat[,-(1:2)], na.rm = TRUE)
diffDat <- diffDat[diffDat$Mean != 0,]
diffMean <- diffDat[,c("St_ID","MAT")]

geoDat <- stationDat[,1:5]
colnames(geoDat)[1] <- "St_ID"
geoDat <- merge(geoDat, diffMean, by = "St_ID", all.y = TRUE)
points <- geoDat[,c(3,4,6)]
points <- points[!is.na(points$Latitude),]
colnames(points)[3] <- "Mean"
points <- points[points$Mean < 0.05,]
points <- points[points$Mean < 0.05 & points$Mean > 0.0005,]

library(sp)
library(raster)
library(rgdal)
library(rgeos)

CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
BC <- readOGR(dsn = "BC_AB_US_Shp", layer = "ProvincialOutline")
BC <- spTransform(BC,CRS.albers)

coordinates(points) <- c("Longitude","Latitude")
proj4string(points) <- CRS("+init=epsg:4326")
points <- spTransform(points, CRS.albers)  # standard albers projection for BC gov't data

install.packages("colorRamps")
library(RColorBrewer)
library(colorRamps)
###Colour gradient
rbPal <- colorRampPalette(c("blue","red"))
points$col <- rbPal(30)[as.numeric(base::cut(points$Mean, breaks = 30))]

pdf("StandardisedModelDiff.pdf",paper = "letter")
plot(BC, main = "Diff")
plot(points, col = points$col, pch = 15, cex = 0.6)
dev.off()
