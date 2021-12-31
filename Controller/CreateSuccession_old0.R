# Get shp created ==============
shp = c('OriginArea', 'polyCoreDisturbedArea', 'polyDisturbedBoundaryArea')
ShpPath = './Result/Intermediate/Map'

source("./Model/CreateGridPolygon.R")
if (file.exists(paste(ShpPath, 'Habitat', paste(shp[1], '.shp',sep=""), sep="/"))) {
  assign(shp[1], CreateGridPolygon(rgdal::readOGR(paste(ShpPath, 'Habitat', paste(shp[1], '.shp',sep=""), sep="/")), OTCdefine))
}
if (file.exists(paste(ShpPath, 'Stressor', paste(shp[2], '.shp',sep=""), sep="/"))) {
  assign(shp[2], CreateGridPolygon(rgdal::readOGR(paste(ShpPath, 'Stressor', paste(shp[2], '.shp',sep=""), sep="/")), OTCdefine))
}
if (file.exists(paste(ShpPath, 'Stressor', paste(shp[3], '.shp',sep=""), sep="/"))) {
  assign(shp[3], CreateGridPolygon(rgdal::readOGR(paste(ShpPath, 'Stressor', paste(shp[3], '.shp',sep=""), sep="/")), OTCdefine))
}

# call function from ModelForOnesp Or ModelForTwoSp in the Model folder ============
source("./Model/GetDataSp.R")
data = GetDataSp(DataSptable)
source("./Core/Controller.R")

names = names(data)


ID <- c()
sp <- c()
beginT <- c()
endT <- c()
PointX <- c()
PointY <- c()
for(i in c(1:length(names))) {
  # assign(names[i],
  #        Model(
  #              DataSptable$Number_sp[i],
  #              DataSptable$Area[i],
  #              OTCdefine,
  #              data
  #             )
  #        )
  
  if (i == 1) {
    for (j in eval(parse(text = shp[i]))) {
        if (length(j) == 1) {
          # next
          minX <- min(j[[1]][,1])
          minY <- min(j[[1]][,2])
          # 
          # Result <-   Model(
          #   DataSptable$Number_sp[i],
          #   DataSptable$Area[i],
          #   OTCdefine,
          #   data
          # )
          # 
          # Long <- Result$data[which(is.na(Result$data$endtime)),3] + minX
          # Lat <- Result$data[which(is.na(Result$data$endtime)),4] + minY
  
          # group <- c(group, Result$data[,c(1,2,5,6)])
          # PointX <- c(PointX, Long)
          # PointY <- c(PointY, Lat)
        } else if (length(j) > 1) {
          minX <- min(j[[k]][[1]][,1])
          minY <- min(j[[k]][[1]][,2])
            # for (k in c(1:length(j))) {
              # print(j[[k]][[1]][,1])
              # minX <- min(j[[k]][[1]][,1])
              # minY <- min(j[[k]][[1]][,2])
              # 
              # Result <-   Model(
              #   DataSptable$Number_sp[i],
              #   DataSptable$Area[i],
              #   OTCdefine,
              #   data)
              #   
              # Long <- Result$data[which(is.na(Result$data$endtime)),3] + minX
              # Lat <- Result$data[which(is.na(Result$data$endtime)),4] + minY
              # 
              # group <- c(group, Result$data[,c(1,2,5,6)])
              # PointX <- c(PointX, Long)
              # PointY <- c(PointY, Lat)
            # }
        }
      Result <-   Model(
        DataSptable$Number_sp[i],
        DataSptable$Area[i],
        OTCdefine,
        data
      )

      Long <- Result$data[which(is.na(Result$data$endtime) & Result$data$sp == 3),3] + minX
      Lat <- Result$data[which(is.na(Result$data$endtime) & Result$data$sp == 3),4] + minY
      
      sp <- c(sp, Result$data[which(is.na(Result$data$endtime) & Result$data$sp == 3),2])
      beginT <- c(beginT, Result$data[which(is.na(Result$data$endtime) & Result$data$sp == 3),5])
      endT <- c(endT, Result$data[which(is.na(Result$data$endtime) & Result$data$sp == 3),6])
      
      PointX <- c(PointX, Long)
      PointY <- c(PointY, Lat)
      }
  }
  print(i)
}

coorPoint <- data.frame(x=PointX, y=PointY)
dataG <- data.frame(sp, beginT, endT)
coorPoint <- SpatialPointsDataFrame(coorPoint, dataG)
rgdal::writeOGR(coorPoint, 
                dsn = "./Result/Intermediate/Output", 
                layer= "output", 
                driver = "ESRI Shapefile", 
                overwrite_layer=T)
# rgdal::writeOGR(coorPoint, dsn = "coorPoint.kml", layer = "coorPoint.gb", driver = "KML")
rm(ls())

names
