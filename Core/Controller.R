Model = function(Number_sp, Area, OTC, dataRaw) {
  if (typeof(Number_sp)!='integer') {Number_sp = as.integer(Number_sp)}
  Area = tolower(Area)
  if (Number_sp == 1) {
      source('./Model/ModelForOneSp.R')
      if (Area == 'origin') {
        return(singleModel(dataRaw$origin, OTC))
      } else if (Area == 'core') {
        return(singleModel(dataRaw$core, OTC))
      } else if (Area == 'boundary') {
        return(singleModel(dataRaw$boundary, OTC))
      }
    
    } 
  else if (Number_sp > 1) {
          source('./Model/ModelForTwoSp.R')
          if (Area == 'core') {
            CoreResult = MultipleModel(dataRaw$core, OTC)
            return(CoreResult)
          } 
          else {
            BoundaryResult = MultipleModel(dataRaw$boundary, OTC)
            return(BoundaryResult)
          }
        }
  }




