# rm(list=ls())
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

