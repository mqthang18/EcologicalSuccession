CreateBoundary = function(data) {
  # Import essential library ===============
  require('rgeos') # => need to call gbuffer()
  require('rgdal') # => need to call readOGR()
  for (i in c(1:length(data$Name))) {
    # print(data$Type[i])
    Pathdir = normalizePath(paste(".",data$Pathdir[i],sep=""))
    if (is.na(data$`Stressor buffer (meters)`[i])) {
      input = c(tolower(data$Type[i]), Pathdir)
      # print(input)
      ChooseFunction(type=input[1], Pathdir=input[2])
    } else {
      stressbuffer = data$`Stressor buffer (meters)`[i]
      if (typeof(stressbuffer) != "integer") {
        stressbuffer = as.integer(stressbuffer)
      }
      input = c(tolower(data$Type[i]), 
                Pathdir,
                stressbuffer)
      print(input[1])
      ChooseFunction(type=input[1], Pathdir=input[2], Str_buffer=input[3])
    }
  }
}


# Choose between or Habitat or Stressor
ChooseFunction = function(type, Pathdir, Str_buffer) {
  if (missing(Str_buffer)) 
    {
    input = Pathdir
    result = switch(
      type, 
      habitat = CreateBoundaryHabitat(Pathdir=input), 
      stressor = CreateBoundaryStressor(Pathdir=input),
      stop("The model is required type parameter in Habitat_stressor_info table!")
    )
  } 
  else 
    {
    input = c(Pathdir, Str_buffer)
    result = switch(
      type, 
      habitat = CreateBoundaryHabitat(Pathdir=input), 
      stressor = CreateBoundaryStressor(Pathdir = input[1], Str_buffer = input[2]),
      stop("The model is required type parameter in Habitat_stressor_info table!")
    )
  }
}
# Funciton create boundary shapefile for habitat 
CreateBoundaryHabitat = function(Pathdir) {
  # Pathdir = paste(".",Pathdir,sep="")
  # print(Pathdir)
  shpA = readOGR(Pathdir)
  # plot(shpA)
  writeOGR(shpA,
           dsn = './Result/Intermediate/Map/Habitat',
           layer = 'OriginArea',
           driver = "ESRI Shapefile",
           overwrite_layer=T)
}
# Function create boundary shapefile for stressor 
CreateBoundaryStressor = function (Pathdir, Str_buffer) {
  # Import '.shp' file into program ====================
  shpA = readOGR(Pathdir)
  if (missing(Str_buffer)) {
    writeOGR(shpA,
             dsn = './Result/Intermediate/Map/Stressor',
             layer = 'polyDisturbedArea',
             driver = "ESRI Shapefile",
             overwrite_layer=T)
  } else 
    {
      # Create shpA buffered ======================
      shpA_buffered = gBuffer(shpA, width=-20)
      # plot(shpA)
      # plot(shpA_buffered, add=T)
      # Get symmetric different part between two polygon =====================
      BoundaryArea <- gSymdifference(shpA, shpA_buffered)
      # plot(BoundaryArea, add=T, col="red")
      # Convert SpatialPolygons obj into SpatialPolygonsDataframe ===================
      BoundaryArea <- ConvertObj(BoundaryArea)
      CoreDistured_df <- ConvertObj(shpA_buffered)
      # plot(Spol_SPDF, add=T, col='blue')
      # Export shapefile have just created =====================
      writeOGR(BoundaryArea, 
               dsn = './Result/Intermediate/Map/Stressor', 
               layer = 'polyDisturbedBoundaryArea', 
               driver = "ESRI Shapefile",
               overwrite_layer=T)
      writeOGR(CoreDistured_df,
               dsn='./Result/Intermediate/Map/Stressor',
               layer = 'polyCoreDisturbedArea',
               driver = "ESRI Shapefile",
               overwrite_layer=T)
  }
}
# Function convert spatialClass to Spatial Dataframe to export data ================
ConvertObj <- function(Area) {
  Area_df <- as.data.frame(sapply(slot(Area, "polygons"), function(x) slot(x, "ID")))
  row.names(Area_df) <- sapply(slot(Area, "polygons"), function(x) slot(x, "ID"))
  Vector <- SpatialPolygonsDataFrame(Area, data = Area_df)
  return(Vector)
}