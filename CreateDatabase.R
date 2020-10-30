library(RPostgreSQL)
library(data.table)

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "localhost", 
                 port = 5432, dbname = "bgc_climate_data")

con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "smithersresearch.ca", 
                 port = 5432, dbname = "greenhouse")

setwd("./SummaryData/")

dbExecute(con, "UPDATE zonesum_fut_v11 SET var = 'SD.mod' WHERE var = 'SD.Mod'")

temp <- fread("StPoints_Model61-90.csv")
colnames(temp) <- dbSafeNames(colnames(temp))
dbWriteTable(con, "stpoints_mod61", temp, row.names = FALSE, overwrite = TRUE)

modelDat <- fread("StPoints_Model81-10.csv", data.table = T)
modelReg <- fread("StPoints_Model61-90.csv", data.table = T)
stationDat <- fread("StationSummary.csv", data.table = T)
stationDat <- merge(modelDat[,1:5], stationDat, by = "STATION", all = FALSE)
colnames(stationDat) <- dbSafeNames(colnames(stationDat))
dbWriteTable(con, "st_summary", stationDat, row.names = FALSE, overwrite = TRUE)

temp <- fread("StnModelDiff2.csv", data.table = T)
colnames(temp) <- dbSafeNames(colnames(temp))
dbWriteTable(con, "stmod_diff", temp, row.names = FALSE, overwrite = TRUE)

temp <- fread("StnBGCMeanDiff.csv", data.table = T)
colnames(temp) <- dbSafeNames(colnames(temp))
dbWriteTable(con, "st_diff_bgc", temp, row.names = FALSE, overwrite = TRUE)

temp <- fread("GridForMapsAll.csv", data.table = T)
colnames(temp) <- dbSafeNames(colnames(temp))
dbWriteTable(con, "map_grid", temp, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con)

modelSub <- dbGetQuery(con, paste0("SELECT ",paste(c("station", "mat"),collapse = ","),
                                   " FROM stpoints_mod81 WHERE station IN ('",
                                   paste(stnp,collapse = "','"),"')"))
stnp <- c("glac10","glac19")
