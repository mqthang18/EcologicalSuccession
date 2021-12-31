GetDataSp = function(data) {
  for (i in c(1:length(data$Area))) {
    Area = tolower(data$Area[i])
    pathdir = data$Pathdir[i]
    print(Area)
    print(pathdir)
    pathdir = paste(".",pathdir,sep="")
    if (Area == "origin") 
    {
        originspdata = read.csv(pathdir, header = F, encoding = 'UTF-8')
        colnames(originspdata) = chartr('123456789','ABCDEFGHJ',c(1: length(originspdata)))
        assign('origin', originspdata)
    }
    else if (Area == "core")
    {
        corespdata = read.csv(pathdir, header = F, encoding = 'UTF-8')
        colnames(corespdata) = chartr('123456789','ABCDEFGHJ',c(1: length(corespdata)))
        assign('core', corespdata)
    }
    else 
    {
        boundaryspdata = read.csv(pathdir, header = F, encoding = 'UTF-8')
        colnames(boundaryspdata) = chartr('123456789','ABCDEFGHJ',c(1: length(boundaryspdata)))
        assign('boundary', boundaryspdata)
    } 
  }
  data = list(origin = origin, core = core, boundary = boundary)
  return(data)
}



