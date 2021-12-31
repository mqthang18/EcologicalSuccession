
library(sf)
library(raster)
shp <- st_as_sf(shp)
Point <- st_as_sf(SpatialPoints(Need[,3:4]))
grid <- shp %>% 
  st_make_grid(cellsize = 10, what = "polygons") %>% # grid of points
  st_intersection(shp)                               # only within the polygon  


plot(test)
plot(Point, add=T)


x <- Need[,3] + min(test[[1]][,1])
y <- Need[,4] + min(test[[1]][,2])
Needle <- data.frame(x,y)
plot(test)
Point <- st_as_sf(SpatialPoints(Needle))
plot(Point, add=T)

minX <- c()
minY <- c()
for (i in grid) {
  minX <- c(minX, min(i[[1]][,1]))
  minY <- c(minY, min(i[[1]][,2]))
}
coor <- data.frame(minX, minY)

ID <- c()
group <- c()
PointX <- c()
PointY <- c()
for (i in c(1:length(coor$minX))) {
  Long <- Need[,3] + coor[i,"minX"]
  Lat <- Need[,4] + coor[i,"minY"]
  ID <- c(ID, i)
  group <- c(group, Need[,2])
  PointX <- c(PointX, Long)
  PointY <- c(PointY, Lat)
} 

coorPoint <- data.frame(x=PointX, y=PointY)
data <- data.frame(ID=ID,group=group)
Point <- st_as_sf(SpatialPoints(coorPoint))
plot(Point, pch=0)

coorPoint@proj4string@projargs <- OriginArea@proj4string@projargs


plot(coorPoint, pch = 3, cex = 0.5, col="green")

library('rgdal')
# coorPoint <- SpatialPoints(coorPoint)
coorPoint <- SpatialPointsDataFrame(coorPoint, data)
# coorPoint <- as(coorPoint,"SpatialPointsDataFrame")
writeOGR(coorPoint,
         dsn = './',
         layer = 'Point',
         driver = "ESRI Shapefile",
         overwrite_layer=T)

writeOGR(coorPoint, dsn = "coorPoint.kml", layer = "coorPoint.gb", driver = "KML")
