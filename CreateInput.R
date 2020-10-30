library(data.table)
library(foreach)
library(tidyverse)
library(sf)
library(raster)
library(scales)

setwd("~/CommonTables/")
bgc <- st_read(dsn = "WNA_BGC_v12_12Oct2020.gpkg")
bgc <- bgc["BGC"]
bgc <- st_cast(bgc,"MULTIPOLYGON")
bgc$Area <- st_area(bgc)
bgc <- as.data.table(bgc)
bgcArea <- bgc[,.(TotArea = sum(Area)), by = .(BGC)]
bgcArea[,NPoints := rescale(units::drop_units(TotArea),to = c(500,1500))]
bgcArea[,NPoints := as.integer(NPoints)]

dem <- raster("./WNA_DEM_SRT_30m.tif")
bgc <- st_as_sf(bgc)

library(doParallel)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

bgcs <- bgcArea$BGC %>% as.character()
points <- foreach(unit = bgcs, .combine = rbind, .packages = c("raster","sf","data.table")) %dopar% {
  cat(".")
  sub <- bgc[bgc$BGC == unit,]
  p1 <- st_sample(sub, size = bgcArea[BGC == unit,(NPoints)], type = "random")
  p1 <- st_transform(p1, st_crs(dem))
  coords <- st_coordinates(p1)
  coords <- as.data.table(coords)
  coords[,el := raster::extract(dem,st_coordinates(p1))]
  coords[,ID2 := unit]
  coords
}

points[,ID1 := seq_along(ID2)]
setnames(points, old = c("X","Y"), new = c("long","lat"))
setcolorder(points, c("ID1","ID2","lat","long","el"))
fwrite(points, "WNA_v12_rand_points.csv")

bcUnits <- fread("./All_BGCs_v11_21.csv")
bcUnits <- bcUnits[,.(Map_Label,DataSet)]
points[bcUnits, Country := i.DataSet, on = c(ID2 = "Map_Label")]
points[ID2 %chin% c("ESSFdh1", "ESSFdh2", "ICHxm1", "IDFdh", "PPxw"), Country := "BC"]
points[is.na(Country), Country := "USA"]
bcPoints <- points[Country == "BC",]
bcPoints[,Country := NULL]
otherPoints <- points[Country != "BC",]
otherPoints[,Country := NULL]

setwd("~/BGC_Climates_Working/")
fwrite(bcPoints, "WNA_v12_BC.csv", eol = "\r\n")
fwrite(otherPoints, "WNA_v12_AB_USA.csv", eol = "\r\n")
