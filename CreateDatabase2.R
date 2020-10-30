library(RPostgreSQL)
library(data.table)

dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "smithersresearch.ca", 
                 port = 5432, dbname = "bgc_climate_data")

setwd("./SummaryData/")
temp <- fread("zonesum_curr.csv")
dbWriteTable(con,"zonesum_curr_v12",temp, row.names = F, overwrite = T)


temp <- fread("climsum_bc_curr_v12.csv")
setcolorder(temp,c(1,3,2,4:250))
setnames(temp, old = c("Unit","Statistic","TimePer"), new = c("bgc","var","period"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "BC"]
dbWriteTable(con, "climsum_curr_v12", temp, row.names = FALSE, overwrite = TRUE)

temp <- fread("climsum_usab_curr_v12.csv")
setcolorder(temp,c(1,3,2,4:250))
setnames(temp, old = c("Unit","Statistic","TimePer"), new = c("bgc","var","period"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "US_AB"]
dbWriteTable(con, "climsum_curr_v12", temp, row.names = FALSE, append = TRUE)
##########################

temp <- fread("climsum_bc_fut_v12.csv")
setcolorder(temp,c(3,2,1,251,252,4:250))
setnames(temp, old = c("Unit","FuturePeriod","Scenario","Var"), 
         new = c("bgc","period","scenario","var"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "BC"]

temp2 <- fread("climsum_usab_fut_v12.csv")
setcolorder(temp2,c(3,2,1,251,252,4:250))
setnames(temp2, old = c("Unit","FuturePeriod","Scenario","Var"), 
         new = c("bgc","period","scenario","var"))
colnames(temp2) <- dbSafeNames(colnames(temp2))
temp2[,location := "US_AB"]
temp <- rbind(temp,temp2)
dbWriteTable(con, "climsum_fut_v12", temp, row.names = FALSE, overwrite = TRUE)
################################

temp <- fread("zonesum_bc_curr_v12.csv")
setcolorder(temp,c(1,3,2,4:250))
setnames(temp, old = c("Unit","Statistic","TimePer"), new = c("bgc","var","period"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "BC"]
dbWriteTable(con, "zonesum_curr_v12", temp, row.names = FALSE, overwrite = TRUE)

temp <- fread("zonesum_all_curr_v12.csv")
setcolorder(temp,c(1,3,2,4:250))
setnames(temp, old = c("Unit","Statistic","TimePer"), new = c("bgc","var","period"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "US_AB"]
dbWriteTable(con, "zonesum_curr_v12", temp, row.names = FALSE, append = TRUE)
###################

temp <- fread("zonesum_bc_fut_v12.csv")
setcolorder(temp,c(3,2,1,251,252,4:250))
setnames(temp, old = c("Unit","FuturePeriod","Scenario","Var"), 
         new = c("bgc","period","scenario","var"))
colnames(temp) <- dbSafeNames(colnames(temp))
temp[,location := "BC"]

temp2 <- fread("zonesum_all_fut_v12.csv")
setcolorder(temp2,c(3,2,1,251,252,4:250))
setnames(temp2, old = c("Unit","FuturePeriod","Scenario","Var"), 
         new = c("bgc","period","scenario","var"))
colnames(temp2) <- dbSafeNames(colnames(temp2))
temp2[,location := "US_AB"]
temp <- rbind(temp,temp2)
dbWriteTable(con, "zonesum_fut_v12", temp, row.names = FALSE, overwrite = TRUE)
dbDisconnect(con)
