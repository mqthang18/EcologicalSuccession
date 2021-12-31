rm(list=ls())
workDir = getwd()
setwd(workDir)
rm('workDir')
# Core folder contain controller function and app function
# App function is main program for flow processes
# Controller function is a function to use for create route to database and call view template
source("./Core/Controller.R")
source("./Core/App.R")

# Read Datafile ==================
# Import essential library 
require(readxl)

# Get PATH to folder have include data 
PATH_DATA = normalizePath('./Datasource')

# Read data table from csv or xlsx file 
files <- list.files(path=PATH_DATA, pattern="*.xlsx")

for (i in c(1:length(files))) {
  name = strsplit(files[i], split = '[.]')
  assign(name[[1]][1], read_excel(normalizePath(paste(PATH_DATA, files[i], sep="\\"))))
}

rm('name')


# Create boundary map =================
CreateBoundary(Habitat_stressor_info)

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
# Get Species data
data = GetDataSp(DataSptable)
# Export result
R1 = RunModel(
  OTCdefine = OTCdefine,
  dataRaw = data, 
  shp = shp, 
  DataSptable = DataSptable, 
  PeriodTime = c(0,6), 
  species = 3, 
  area = 1)

R2 = RunModel(
              OTCdefine = OTCdefine, 
              dataRaw = data, 
              shp=shp, 
              DataSptable = DataSptable, 
              PeriodTime = NULL, 
              species = 2, 
              area = 1
              )
R3 = RunModel(OTCdefine = OTCdefine, 
              dataRaw = data, 
              shp=shp, 
              DataSptable = DataSptable, 
              PeriodTime = NULL, 
              species = 1, 
              area = 1
              )

rm(ls())