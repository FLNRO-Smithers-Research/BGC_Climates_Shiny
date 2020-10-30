###Script to clean and summarise climate sation data into ClimateBC type variables
###Kiri Daust, 2018

library(reshape2)
library(dplyr)
library(magrittr)
library(ggplot2)
library(foreach)
library(tcltk)
library(rgdal)
library(sp)
library(sf)
library(raster)
library(rgeos)
library(maptools)

###Set months for each variables
wt <- c("Dec","Jan","Feb")
sp <- c("Mar","April","May")
sm <- c("June","July","Aug")
at <- c("Sept","Oct","Nov")
meanSm <- c("May","June","July","Aug","Sept")
pptWt <- c("Oct","Nov","Dec","Jan","Feb","Mar")

wd <- tk_choose.dir(); setwd(wd)

###Precipetation
dat <- read.csv(file.choose(), stringsAsFactors = F)###import ppt file
dat <- dat[,!colnames(dat) %in% c("St_Flag","El_Flag","Annual")]
###Clean
dat <- melt(dat, id.vars = c("St_ID","Name","Elevation","Long","Lat"))
colnames(dat)[6:7] <- c("Month","Value")
dat <- dat[order(dat$Name, dat$Month),]
dat[dat == -9999] <- NA
stNames <- unique(dat$St_ID)

###loop through each station id, and calculate each variable if no NAs
pptOut <- foreach(st = stNames, .combine = rbind) %do% {
  sub <- dat[dat$St_ID == st,]
  MAP <- NA; pptWt <- NA; pptSp <- NA; pptSm <- NA; pptAt <- NA; MSP <- NA; MWP <- NA###Set initial value to NA
  if(!any(is.na(sub$Value))){
    MAP <- sum(sub$Value)
  }
  if(!any(is.na(sub$Value[sub$Month %in% wt]))){
    pptWt <- sum(sub$Value[sub$Month %in% wt])
  }
  if(!any(is.na(sub$Value[sub$Month %in% sp]))){
    pptSp <- sum(sub$Value[sub$Month %in% sp])
  }
  if(!any(is.na(sub$Value[sub$Month %in% sm]))){
    pptSm <- sum(sub$Value[sub$Month %in% sm])
  }
  if(!any(is.na(sub$Value[sub$Month %in% at]))){
    pptAt <- sum(sub$Value[sub$Month %in% at])
  }
  if(!any(is.na(sub$Value[sub$Month %in% meanSm]))){
    MSP <- sum(sub$Value[sub$Month %in% meanSm])
  }
  if(!any(is.na(sub$Value[sub$Month %in% pptWt]))){
    MWP <- sum(sub$Value[sub$Month %in% pptWt])
  }
  
  out <- data.frame(St_ID = st,Name = sub$Name[1], Long = sub$Long[1], Lat = sub$Lat[1], MAP = MAP, PPT_wt = pptWt, PPT_sp = pptSp, PPT_sm = pptSm,
                    PPT_at = pptAt, MSP = MSP)
  out
}

####Min Temperature
dat <- read.csv(file.choose(), stringsAsFactors = F) ###import min temperature data
dat <- dat[,!colnames(dat) %in% c("St_Flag","El_Flag","Annual")]

dat <- melt(dat, id.vars = c("St_ID","Name","Elevation","Long","Lat"))
colnames(dat)[6:7] <- c("Month","Value")
dat <- dat[order(dat$Name, dat$Month),]
dat[dat == -9999] <- NA
dat$Value <- dat$Value/10
stNames <- unique(as.character(dat$St_ID))

###Loop through and calculate variables
tMin <- foreach(st = stNames, .combine = rbind) %do% {
  sub <- dat[dat$St_ID == st,]
  MeanMin <- NA; Tmin_wt <- NA; Tmin_sp <- NA; 
  Tmin_sm <- NA; Tmin_at <- NA; MWMT <- NA; MCMT <- NA; ###Set inital to NA
  if(!any(is.na(sub$Value))){
    MeanMin <- mean(sub$Value)
  }
  if(!any(is.na(sub$Value[sub$Month %in% wt]))){
    Tmin_wt <- mean(sub$Value[sub$Month %in% wt])
    MCMT <- min(sub$Value[sub$Month %in% wt])###Min value in winter months
  }
  if(!any(is.na(sub$Value[sub$Month %in% sp]))){
    Tmin_sp <- mean(sub$Value[sub$Month %in% sp])
  }
  if(!any(is.na(sub$Value[sub$Month %in% sm]))){
    Tmin_sm <- mean(sub$Value[sub$Month %in% sm])
    MWMT <- max(sub$Value[sub$Month %in% sm])###Max value in summer months
  }
  if(!any(is.na(sub$Value[sub$Month %in% at]))){
    Tmin_at <- mean(sub$Value[sub$Month %in% at])
  }
  out <- data.frame(St_ID = st, Name = sub$Name[1], Long = sub$Long[1], Lat = sub$Lat[1], MeanMin = MeanMin, Tmin_wt = Tmin_wt, Tmin_sp = Tmin_sp, Tmin_sm = Tmin_sm, 
                    Tmin_at = Tmin_at, MCMTmin = MCMT, MWMTmin = MWMT)
  out
}

##test <- aggregate(Value ~ Month, dat, FUN = mean, na.rm = T)

####Max Temp
dat <- read.csv(file.choose(), stringsAsFactors = F) ###import max temperature data
dat <- dat[,!colnames(dat) %in% c("St_Flag","El_Flag","Annual")]

dat <- melt(dat, id.vars = c("St_ID","Name","Elevation","Long","Lat"))
colnames(dat)[6:7] <- c("Month","Value")
dat <- dat[order(dat$Name, dat$Month),]
dat[dat == -9999] <- NA
dat$Value <- dat$Value/10
stNames <- unique(as.character(dat$St_ID))

tMax <- foreach(st = stNames, .combine = rbind) %do% {
  sub <- dat[dat$St_ID == st,]
  MeanMin <- NA; Tmin_wt <- NA; Tmin_sp <- NA; Tmin_sm <- NA; Tmin_at <- NA;MWMT <- NA; MCMT <- NA
  if(!any(is.na(sub$Value))){
    MeanMin <- mean(sub$Value)
  }
  if(!any(is.na(sub$Value[sub$Month %in% wt]))){
    Tmin_wt <- mean(sub$Value[sub$Month %in% wt])
    MCMT <- min(sub$Value[sub$Month %in% wt])
  }
  if(!any(is.na(sub$Value[sub$Month %in% sp]))){
    Tmin_sp <- mean(sub$Value[sub$Month %in% sp])
  }
  if(!any(is.na(sub$Value[sub$Month %in% sm]))){
    Tmin_sm <- mean(sub$Value[sub$Month %in% sm])
    MWMT <- max(sub$Value[sub$Month %in% sm])
  }
  if(!any(is.na(sub$Value[sub$Month %in% at]))){
    Tmin_at <- mean(sub$Value[sub$Month %in% at])
  }
  out <- data.frame(St_ID = st,Name = sub$Name[1], Long = sub$Long[1], Lat = sub$Lat[1], MeanMax = MeanMin, Tmax_wt = Tmin_wt, Tmax_sp = Tmin_sp, Tmax_sm = Tmin_sm, 
                    Tmax_at = Tmin_at, MCMTmax = MCMT, MWMTmax = MWMT)
  out
}

###combine into one file and calculate additional variables
aveTemp <- tMax
aveTemp <- merge(aveTemp, tMin, by = "St_ID")
aveTemp <- merge(aveTemp, pptOut, by = "St_ID", all.x = TRUE)
aveTemp$MAT <- (aveTemp$MeanMax + aveTemp$MeanMin)/2 ###average between min and max
aveTemp$MCMTAve <- (aveTemp$MCMTmax+aveTemp$MCMTmin)/2
aveTemp$MWMTAve <- (aveTemp$MWMTmax+aveTemp$MWMTmin)/2
aveTemp$TD <- aveTemp$MWMTAve - aveTemp$MCMTAve
aveTemp$AHM <- (aveTemp$MAT+10)/(aveTemp$MAP/1000)
aveTemp$SHM <- aveTemp$MWMTAve/(aveTemp$MSP/1000)
aveTemp <- within(aveTemp, {Tave_sp <- (Tmin_sp+Tmax_sp)/2
       Tave_sm <- (Tmin_sm+Tmax_sm)/2 
       Tave_at <- (Tmin_at+Tmax_at)/2 
       Tave_wt <- (Tmin_wt+Tmax_wt)/2})

outVars <- c("St_ID","Name.x","PPT_sp", "PPT_sm", "PPT_at", "PPT_wt", "Tmax_sp", "Tmax_sm", "Tmax_at", "Tmax_wt","Tmin_sp", 
             "Tmin_sm", "Tmin_at", "Tmin_wt","Tave_sp","Tave_sm", "Tave_at", "Tave_wt", 
             "MSP", "MAP", "MAT", "MWMTAve", "MCMTAve", "TD", "AHM", "SHM") ###variables to export
StationOut <- aveTemp[,outVars]
colnames(StationOut) <- c("STATION","Name", "PPT_sp", "PPT_sm", "PPT_at", "PPT_wt", "Tmax_sp", "Tmax_sm", "Tmax_at", "Tmax_wt",
                          "Tmin_sp", "Tmin_sm", "Tmin_at", "Tmin_wt","Tave_sp","Tave_sm", "Tave_at", "Tave_wt", 
             "MSP", "MAP", "MAT", "MWMT", "MCMT", "TD", "AHM", "SHM")
write.csv(StationOut, "StationSummary.csv", row.names = FALSE) ###Final data set

####Now create list of all stations for climateBC
ppt <- read.csv(file.choose())
tmax <- read.csv(file.choose())
tmin <- read.csv(file.choose())
st.list <- rbind(ppt[,c(1,3,6,7,4)],tmax[,c(1,3,6,7,4)],tmin[,c(1,3,6,7,4)])
st.list <- st.list[unique(st.list$St_ID),]

####Assign BGCs to stations######################
setwd(tk_choose.dir())
bec11 <- st_read(dsn="bgc.v11.gdb",layer="bgcv11_bc") ##read bec file
CRS.albers <- CRS ("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")
allUnits <- unique(as.character(bec11$MAP_LABEL))###What units are in BEC?
dem <- raster("bc25fill") ###Read DEM

require(doParallel)
set.seed(123321)
coreNum <- as.numeric(detectCores()-2)
coreNo <- makeCluster(coreNum)
registerDoParallel(coreNo, cores = coreNum)

###Only keeps stations in a BGC
stBGCOut <- foreach(BGC = allUnits, .combine = rbind, .packages = c("sp","sf","raster")) %dopar%{
  dat <- st.list
  pointsOrig <- dat
  coordinates(dat) <- c("Long","Lat")
  proj4string(dat) <- CRS("+init=epsg:4326")
  dat <- spTransform(dat, CRS.albers)  # standard albers projection for BC gov't data
  
  tempPoly <- bec11[bec11$MAP_LABEL == BGC,]
  tempPoly <- as(tempPoly, "Spatial") ##conver to sp
  tempPoly <- spTransform(tempPoly, CRS.albers) 
  dat <- over(dat, tempPoly) ###which ones are inside the BGC
  pointsOrig <- pointsOrig[!is.na(dat$BGC_LABEL),] ###Remove points not inside BGC
  if(nrow(pointsOrig) > 0){ ###check that some points fall inside BGC=
    pointsOrig$BGC <- BGC
    pointsOrig
  }
}

###add elevation data - given elevation data is often innacurate or missing
temp <- stBGCOut
coordinates(temp) <- c("Long","Lat")
proj4string(temp) <- CRS("+init=epsg:4326")
temp <- spTransform(temp, CRS(proj4string(dem)))
stBGCOut$ElevationGood <- raster::extract(dem,temp)

stPointsOut <- stBGCOut[,c("St_ID","BGC","Lat","Long","ElevationGood")]
colnames(stPointsOut) <- c("ID1","ID2","Lat","Long","Elevation")
write.csv(stPointsOut, "StPoints.csv", row.names = FALSE)###write file to input to climateBC





############################################################################################
###Old Code###
colnames(stBGCOut)[1] <- "St_ID"
stBGCOut <- merge(stBGCOut, pptOut[,-(2:3)], by = "St_ID", all.x = TRUE)
stBGCOut <- merge(stBGCOut, tMin[,c(1,4:7)], by = "St_ID", all.x = TRUE)
stBGCOut <- merge(stBGCOut, tMax[,c(1,4:7)], by = "St_ID", all.x = TRUE)
stBGCOut <- merge(stBGCOut, aveTemp[,c(1,6:8)], by = "St_ID", all.x = TRUE)
##save <- stBGCOut

stBGCOut <- stBGCOut[!duplicated(stBGCOut$St_ID),]
write.csv(stBGCOut, "StationDataOct21.csv", row.names = F)

####Cool recursive function##########3
solveTowers <- function(n, source, destination, spare){
  if(n == 1){
    cat("From",source,"To",destination,"\n", sep = " ")
  }else{
    solveTowers(n - 1, source, spare, destination)
    cat("From",source,"To",destination,"\n", sep = " ")
    solveTowers(n-1, spare, destination, source)
  }
}

#######
install.packages("sn")
library(sn)
f1 <- makeSECdistr(dp=c(3,2,5), family="SN", name="First-SN")
show(f1)
summary(f1)
plot(f11)
plot(f1, probs=c(0.1, 0.9))
#
f2 <- makeSECdistr(dp=c(3, 5, -4, 8), family="ST", name="First-ST")
f9 <- makeSECdistr(dp=c(5, 1, Inf, 0.5), family="ESN", name="ESN,alpha=Inf")
#
dp0 <- list(xi=1:2, Omega=diag(3:4), alpha=c(3, -5))
f10 <- makeSECdistr(dp=dp0, family="SN", name="SN-2d", compNames=c("u1", "u2"))
#
dp1 <- list(xi=1:2, Omega=diag(1:2)+outer(c(3,3),c(2,2)), alpha=c(-3, 5), nu=6)
f11 <- makeSECdistr(dp=dp1, family="ST", name="ST-2d", compNames=c("t1", "t2"))

data(ais)
m1 <- selm(log(Fe) ~ BMI + LBM, family="SN", data=ais)
print(m1)
summary(m1)
s<- summary(m1, "DP", cov=TRUE, cor=TRUE)
plot(m1)

plot(m1, param.type="DP")
logLik(m1)
coef(m1)
coef(m1, "DP")
var <- vcov(m1)
