MultipleModel = function(data, OTC, dispKernel) {
  # require("facilitation")
  ### Two species competition+facilitation
  # maxt <- as.numeric(OTC$Number[1])
  # height <- as.numeric(OTC$Number[2])
  # width <- as.numeric(OTC$Number[3])
  
  maxt <- 10
  height <- 10
  width <- 10
  
  
  nstages <- c(as.numeric(data[1, 2]), as.numeric(data[1,5]))
  init <- list(as.integer(c(data[10,2],data[10,3],data[10,4],
               data[10,5],data[10,6],data[10,7])))
  ### parameter matrix has one stage per row
  ###                D G R d r
  param <- matrix(as.double(c(
                    data[c(3:5, 7:8), 2],
                    data[c(3:5, 7:8), 3],
                    data[c(3:5, 7:8), 4],
                    data[c(3:5, 7:8), 5],
                    data[c(3:5, 7:8), 6],
                    data[c(3:5, 7:8), 7])), byrow=T, nrow=6)
  ### interaction matrix: positive values represent facilitation, negative ones, competition
  interactD <- matrix(as.double(unlist(data[14:19,2:7], use.names=F)),ncol=6)
  
  ### let's also add effects over growth rates: adults limit growth
  interactG <- matrix(as.double(unlist(data[23:27,2:7], use.names=F)),ncol=6) ## effects over species 2
  
  results <-
    facilitation::community(maxt,nstages,param,init, 
              interactionsD=interactD,
              interactionsG=interactG,
              height = height, width = width,
              dispKernel = dispKernel
    )
  return(results)
}
