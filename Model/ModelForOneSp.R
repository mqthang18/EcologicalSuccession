singleModel <- function(data, OTC) {
  # require("facilitation")
  
  numstages <- as.integer(data$B[1])
  
  deathrates <- as.double(c(data$B[3], data$C[3], data$D[3]))     # death rates for seed, sapling and adult
  growthrates <- as.double(c(data$B[4], data$C[4]))         # transition rates seed-->sapling and sapling-->adult
  reproductionrate <- as.double(data$D[5])  # reproduction rate (only adult)
  radius <- as.double(c(data$B[8],data$C[8],data$D[8]))             # interaction radii per stage
  dispersalradius <- as.double(data$B[7])   # average distance a seed falls from the parent (distance is exponentially distributed)
  
  param <- facilitation::create.parameters(D=deathrates,G=growthrates,R=reproductionrate,dispersal=dispersalradius,radius=radius) # create the parameter object
  
  init <- as.integer(c(data[10,2:4]))    # initial pop. sizes for the 3 stages plus the facilitator species
  effects <- as.double(c(
               data[12,2:4], # effects over seeds (none)
               data[13,2:4], # effects over saplings (competition and facilitation)
               data[14,2:4])) # effects over adults (competition with adults)
  
  maxt <- as.double(OTC$Number[1]) # time up to which the simulation shall run
  h <- as.double(OTC$Number[2]) # arena height
  w <- as.double(OTC$Number[3]) # arena width
  
  OriginArea <- facilitation::community(maxt,
                                        numstages,
                                        param,
                                        init,
                                        interactionsD=effects,
                                        height=h,width=w
                                        )
  
  return(OriginArea)
}

