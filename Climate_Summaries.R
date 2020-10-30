# Script to create climate summaries given output from climate BC
##Kiri Daust, April 2020
######Future Data#################################

library(foreach)
library(data.table)
library(matrixStats)
library(tidyr)

betterMods <- c("CanESM2", "CCSM4", "CESM1-CAM5", "CSIRO-Mk3.6.0", "GISS-E2R", "HadGEM2-ES", "MIROC5", "MPI-ESM-LR")

futSumSZ <- function(dat){
  setDT(dat)
  cat("Mean...\n")
  avg <- dat[,lapply(.SD,mean), 
             by = .(Scenario,FuturePeriod,Unit),.SDcols = -c("GCM","ID1")]
  avg[,Var := "mean"]
  cat("Max...\n")
  mX <- dat[,lapply(.SD,max), 
            by = .(Scenario,FuturePeriod,Unit),.SDcols = -c("GCM","ID1")]
  mX[,Var := "max"]
  cat("Min...\n")
  mN <- dat[,lapply(.SD,min), 
            by = .(Scenario,FuturePeriod,Unit),.SDcols = -c("GCM","ID1")]
  mN[,Var := "min"]
  cat("Quantiles...\n")
  q10 <- dat[,lapply(.SD,quantile,probs = 0.1), 
             by = .(Scenario,FuturePeriod,Unit),.SDcols = -c("GCM","ID1")]
  q10[,Var := "10%"]
  q90 <- dat[,lapply(.SD,quantile,probs = 0.9), 
             by = .(Scenario,FuturePeriod,Unit),.SDcols = -c("GCM","ID1")]
  q90[,Var := "90%"]
  cat("SDmod...\n")
  sd.mod <- dat[,lapply(.SD,sd),
                by = .(Scenario,FuturePeriod,Unit,ID1),
                .SDcols = -"GCM"][,lapply(.SD,mean),by = .(Scenario,FuturePeriod,Unit),
                                  .SDcols = -c("ID1")]
  sd.mod[,Var := "SD.Mod"]
  cat("SDgeo...\n")
  sd.geo <- dat[,lapply(.SD,mean),
                by = .(Scenario,FuturePeriod,Unit,ID1),
                .SDcols = -"GCM"][,lapply(.SD,sd),by = .(Scenario,FuturePeriod,Unit),
                                  .SDcols = -c("ID1")]
  sd.geo[,Var := "SD.Geo"]
  sumOut <- rbind(avg,mN,mX,q10,q90,sd.geo,sd.mod)
  return(sumOut)
}
###FUTURE CLIMATE DATA
##read in climate BC output - 500 - 1500 pts per BGC currently
rawDat <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), 
                stringsAsFactors = FALSE, data.table = T)
rawDat2 <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), 
                 stringsAsFactors = FALSE, data.table = T)
rawDat <- rbind(rawDat,rawDat2)

###Clean data, separate variables
rawDat[,c("GCM","Scenario","FuturePeriod") := tstrsplit(Year, split = "_")]
rawDat[,Year := NULL]
rawDat[,FuturePeriod := gsub(".gcm","",FuturePeriod)]
for(j in seq_along(rawDat)){
  set(rawDat, i = which(rawDat[[j]]==-9999), j = j, value = NA)
}
rawDat <- na.omit(rawDat)
rawDat <- rawDat[!grepl("Ensemble",GCM),]
setnames(rawDat, old = "ID2", new = "Unit")
datAll <- futSumSZ(rawDat)
rawDat8 <- rawDat[grep(paste(betterMods, collapse = "|"),GCM),]
dat8 <- futSumSZ(rawDat8)
rawDat[,Unit := gsub("[[:lower:]]|[[:digit:]]","",Unit)]
rawDat[,Unit := gsub("_.*","",Unit)]
zoneAll <- futSumSZ(rawDat)
rawDat8[,Unit := gsub("[[:lower:]]|[[:digit:]]","",Unit)]
rawDat8[,Unit := gsub("_.*","",Unit)]
zone8 <- futSumSZ(rawDat8)

datAll[,ModSet := "Full"]
dat8[,ModSet := "Reduced"]
datAll <- rbind(datAll,dat8)
fwrite(datAll, "climsum_usab_fut_v12.csv")

zoneAll[,ModSet := "Full"]
zone8[,ModSet := "Reduced"]
zoneAll <- rbind(zoneAll,zone8)
fwrite(zoneAll, "zonesum_all_fut_v12.csv")

###############################################
############Historic Data#######################################
require(Rcpp)
require(matrixStats)
require(data.table)

###read in historical data
rawDat <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), 
                stringsAsFactors = FALSE, data.table = T)
rawDat2 <- fread(file.choose(), drop = c("Latitude","Longitude","Elevation"), 
                 stringsAsFactors = FALSE, data.table = T)
rawDat <- rbind(rawDat,rawDat2)
rm(rawDat2)
gc()

for(j in seq_along(rawDat)){
  set(rawDat, i = which(rawDat[[j]]==-9999), j = j, value = NA)
}
setnames(rawDat, old = "ID2", new = "Unit")
rawDat[,Unit := gsub("[[:lower:]]|[[:digit:]]","",Unit)]
rawDat[,Unit := gsub("_.*","",Unit)]
dSmall <- rawDat[,.SD[sample(.N,min(1000,.N))], by = Unit]
rm(rawDat)
gc()
rawDat <- dSmall
##choose what periods to create summaries for
Period <- list(c(1901,1930),c(1931,1960),c(1961,1990),c(1991,2019),c(1901,1990),c(1901,2019),
               c(1945,1976),c(1971,2000),c(1977,1998))

library(doSNOW)
cl <- makeCluster(10)
registerDoSNOW(cl)
iterations <- length(Period)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

results <- foreach(PER = Period, .combine = rbind, 
                   .packages = c("matrixStats","foreach","data.table")) %do%{
    cat(".")
    avg <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,mean,na.rm = T), 
               by = .(Unit),.SDcols = -c("Year","ID1")]
    avg[,Var := "mean"]
    mX <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,max,na.rm = T), 
              by = .(Unit),.SDcols = -c("Year","ID1")]
    mX[,Var := "max"]
    mN <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,min,na.rm = T), 
              by = .(Unit),.SDcols = -c("Year","ID1")]
    mN[,Var := "min"]
    q10 <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,quantile,probs = 0.1,na.rm = T), 
               by = .(Unit),.SDcols = -c("Year","ID1")]
    q10[,Var := "10%"]
    q90 <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,quantile,probs = 0.9,na.rm = T), 
               by = .(Unit),.SDcols = -c("Year","ID1")]
    q90[,Var := "90%"]
    sd.geo <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,sd,na.rm = T),by = .(Unit,Year),.SDcols = -c("ID1")
                  ][,lapply(.SD,mean,na.rm = T),by = .(Unit),
                                    .SDcols = -c("Year")]
    sd.geo[,Var := "st.dev.Geo"]
    sd.ann <- rawDat[Year >= PER[1] & Year <= PER[2],lapply(.SD,sd,na.rm = T),by = .(Unit,ID1),.SDcols = -"Year"
                  ][,lapply(.SD,mean,na.rm = T),by = .(Unit),
                                    .SDcols = -c("ID1")]
    sd.ann[,Var := "st.dev.Ann"]
    sumOut <- rbind(avg,mN,mX,q10,q90,sd.geo,sd.ann)
    sumOut[,TimePer := paste(PER[1],"-",PER[2])]
    gc()
    sumOut
  }

setnames(results,old = "Var", new = "Statistic")
nms <- colnames(results)
nms <- nms[c(1,249,250,2:248)]
setcolorder(results, nms)

fwrite(results,"climsum_all_curr_v12.csv")


