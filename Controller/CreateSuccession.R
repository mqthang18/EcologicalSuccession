# call function from ModelForOnesp Or ModelForTwoSp in the Model folder ============
source("./Model/GetDataSp.R")

source("./Core/Controller.R")

# names = names(data)

RunModel = function(
                    #names, # names => ten cua 3 bien luu du lieu polygon 
                    OTCdefine,
                    dataRaw, # Du lieu sinh ly cho tung vung
                    shp, # ten cua 3 bien luu du lieu polygon
                    DataSptable, # xac dinh du lieu su dung 
                    PeriodTime = NULL, # Xac dinh khoang thoi gian mong muon
                    species, # giai doan loai
                    area # chon khu vuc theo chi so cua bien shp
                    ){

    id <- c()
    sp <- c()
    beginT <- c()
    endT <- c()
    
    PointX <- c()
    PointY <- c()
    
    dataG <- c()
    # Result <-   Model(
    #   DataSptable$Number_sp[area],
    #   DataSptable$Area[area],
    #   OTCdefine,
    #   dataRaw
    # )

      for (j in eval(parse(text = shp[area]))) {
          Result <- c()
          index <- c()
          i1 <- c()
          i2 <- c()
          dataR <- c()
          if (length(j) == 1) {
            # next
            minX <- min(j[[1]][,1])
            minY <- min(j[[1]][,2])

          } else if (length(j) > 1) {
            arrX = c()
            arrY = c()
            for (m in j) {
              # print(item)
              for (n in m) {
                # print(min(n[,1]))
                arrX = c(arrX, min(n[,1]))
                # print(min(n[,2]))
                arrY = c(arrY, min(n[,2]))
              }
            }
            minX = min(arrX)
            minY = min(arrY)
          }
     
          Result <-   Model(
            DataSptable$Number_sp[area],
            DataSptable$Area[area],
            OTCdefine,
            dataRaw
          )

          # dataR <- facilitation::restart(data=Result$data,
          #                                time=as.number(OTCdefine$Number[1]),
          #                                start=0)
            # dataR <- facilitation::restart(Result,10,start=0)
            # dataR <- dataR$data
          dataR <- Result$data
          if (is.null(PeriodTime)) {
            # index <- which(is.na(dataR$endtime) & (dataR$sp == species))
            index <- which(dataR$sp == species)
            # print('=====')
            # print(index)
          } else {
            # i1 <- which(dataR$begintime >= PeriodTime[1] & dataR$endtime <= PeriodTime[2])
            # i2 <- which(dataR$sp == species)
            # index = intersect(i1,i2)
            index <- which(dataR$begintime >= PeriodTime[1] & dataR$endtime <= PeriodTime[2] & dataR$sp == species)
            # index <- c(1:length(dataR$sp))
            # print(index)
          }

          if (length(index) == 0) {
            next
          } else {
            id <- c(id, dataR[index,2])
            sp <- c(sp, dataR[index,1])
            beginT <- c(beginT, dataR[index,5])
            endT <- c(endT, dataR[index,6])

            Long <- dataR[index,3] + minX
            Lat <- dataR[index,4] + minY

            PointX <- c(PointX, Long)
            PointY <- c(PointY, Lat)
          }
        }
      print('Successful')
      # print(i)
      # print(Result)
      
      coorPoint <- data.frame(x=PointX, y=PointY)
      dataG <- data.frame(ID=id, sp=sp, beginTime=beginT, endTime=endT)
      coorPoint <- sp::SpatialPointsDataFrame(coorPoint, dataG)

      rgdal::writeOGR(coorPoint,
                      dsn = "./Result/Intermediate/Output",
                      layer= paste(shp[area],'sp',species,sep="_"),
                      driver = "ESRI Shapefile",
                      overwrite_layer=T)
      return(dataG)
}


