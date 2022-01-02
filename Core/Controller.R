Model = function(
                 Number_sp, # So luong loai cua khu vuc 
                 Area, # Chi ra khu vuc la rung con nguyen hay vung bi tac dong hoac vung bien giua hai vung
                 OTC, # O tieu chuan du lieu 
                 dataRaw, # Du lieu mo ta ti le song, chet, sinh san, tac tuong cua loai
                 dispKernel # Chi ra cach thuc phan tan hat "random" (bo qua ban kinh phan tan) hoac "exponential"
                 ) {
  if (typeof(Number_sp)!='integer') {Number_sp = as.integer(Number_sp)}
  Area = tolower(Area)
  if (Number_sp == 1) {
      source('./Model/ModelForOneSp.R')
      if (Area == 'origin') {
        return(singleModel(dataRaw$origin, OTC, dispKernel = dispKernel))
      } else if (Area == 'core') {
        return(singleModel(dataRaw$core, OTC, dispKernel = dispKernel))
      } else if (Area == 'boundary') {
        return(singleModel(dataRaw$boundary, OTC, dispKernel = dispKernel))
      }
    } 
  else if (Number_sp > 1) {
          source('./Model/ModelForTwoSp.R')
          if (Area == 'core') {
            return(MultipleModel(dataRaw$core, OTC, dispKernel = dispKernel))
          } else if (Area == 'origin') {
            return(MultipleModel(dataRaw$origin, OTC, dispKernel = dispKernel))
          }
          else if (Area == 'boundary') {
            return(MultipleModel(dataRaw$boundary, OTC, dispKernel = dispKernel))
          }
        }
  }




