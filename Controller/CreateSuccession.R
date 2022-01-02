# names = names(data)

RunModel = function(
                    #names, # names => ten cua 3 bien luu du lieu polygon 
                    OTCdefine,
                    dataRaw, # Du lieu sinh ly cho tung vung
                    shp, # ten cua 3 bien luu du lieu polygon
                    DataSptable, # xac dinh du lieu su dung 
                    PeriodTime = NULL, # Xac dinh khoang thoi gian (beginTime; EndTime) mong muon hoac bo trong
                    species, # giai doan loai
                    area, # chon khu vuc theo chi so cua bien shp
                    dispKernel # chon kieu khuech tan hat 
                    ){

    pop <- c()
    total <- c()
    stages <- c()
  
    id <- c()
    sp <- c()
    beginT <- c()
    endT <- c()
    longevity <- c()
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
          longe <- c()
          dataR <- c()
          indexID <- c()
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
            dataRaw,
            dispKernel = dispKernel
          )

          # dataR <- facilitation::restart(data=Result$data,
          #                                time=as.number(OTCdefine$Number[1]),
          #                                start=0)
            # dataR <- facilitation::restart(Result,10,start=0)
            # dataR <- dataR$data
          # dataR <- Result$data
          dataR <- facilitation::longevity(Result)
          # print(dataR)
          if (!is.null(species)) {
            if (is.null(PeriodTime)) {
              index <- which(dataR$last.stage == species)
            } else {
                if (length(PeriodTime) != 1) {
                  index <- which((dataR$birth >= PeriodTime[1] & dataR$birth <= PeriodTime[2]) & dataR$death >= PeriodTime[2] & dataR$last.stage == species)
                
              } else {
                  index <- which((dataR$birth <= PeriodTime & dataR$death >= PeriodTime) & dataR$last.stage == species)
              }
            }
          } else {
            if (is.null(PeriodTime)) {
              index <- which(!is.null(dataR$last.stage))
            } else {
                if (length(PeriodTime) != 1) {
                  index <- which((dataR$birth >= PeriodTime[1] & dataR$birth <= PeriodTime[2]) & dataR$death >= PeriodTime[2] & !is.null(dataR$last.stage))
                  
                } else {
                  index <- which((dataR$birth <= PeriodTime & dataR$death >= PeriodTime) & !is.null(dataR$last.stage))
                }
              }
            }
          
          if (length(index) == 0) {
            next
          } else {
            # longe <- facilitation::longevity(Result)
            indexID = dataR[index,2]
            # print(longevity)
            id <- c(id, indexID)
            sp <- c(sp, dataR[index,1])
            beginT <- c(beginT, dataR[index,3])
            endT <- c(endT, dataR[index,4])
            longevity <- c(longevity, dataR[index,5])
            
            
            dataR = Result$data
            index = which((indexID %in% dataR$id)==T)
            Long <- dataR[index,3] + minX
            Lat <- dataR[index,4] + minY
            
            
            PointX <- c(PointX, Long)
            PointY <- c(PointY, Lat)
          
            
          }
          pop <- Result$num.pop
          total <- Result$num.total
          stages <- Result$num.stages
        }
      
      # print(i)
      # print(Result)
      
      
      dataG <- data.frame(ID=as.integer(id), 
                          sp=as.integer(sp), 
                          begintime=beginT, 
                          endtime=endT, 
                          longevity = longevity)
      
      print(paste(paste('num.pop',pop,sep=": "),
                  paste('num.total',total,sep=": "),
                  paste('num.stages',stages,sep=": "), sep="; "))
      if (length(dataG) == 0) {
        print("Don't have result for this condition")
      } else {
        print("Successful!")
        coorPoint <- data.frame(x=PointX, y=PointY)
        coorPoint <- sp::SpatialPointsDataFrame(coorPoint, dataG)
        rgdal::writeOGR(coorPoint,
                        dsn = "./Result/Output",
                        layer= paste(shp[area],'sp',species,sep="_"),
                        driver = "ESRI Shapefile",
                        overwrite_layer=T)
        # return(dataG)
      }
}


