# Create grid polygon for each area 

CreateGridPolygon = function(shp, OTC) {
  require(sf)
  require(raster)
  if (as.double(OTC$Number[2]) == as.double(OTC$Number[3])) {
    size = as.double(OTC$Number[2])
  } else {
    size = 10
  }
  shp <- st_as_sf(shp)
  grid <- shp %>% 
    st_make_grid(cellsize = size, what = "polygons") %>% # grid of points
    st_intersection(shp)                               # only within the polygon
  return(grid)
}
