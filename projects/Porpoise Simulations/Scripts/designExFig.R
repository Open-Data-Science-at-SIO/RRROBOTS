require(nlme)
load("./Data/predGrid.RData") #gridOut created by spDensity
load("./Data/Dmodel.RData") #

# the impact coefficient for the magnitude of the impact is scaled from 0 to 1
# according to habitat quality.  So high density areas have a lower I val.
gridOut$I <- (gridOut$PpSqKm*(1/(max(gridOut$PpSqKm)))) 

# Restrict possible instrument locations to be within specified depth range
gO <- gridOut[which(gridOut$D>d.max & gridOut$D <= d.min),]
# Add a column for scaled density
gO$scaled.D <- gO$PpSqKm/max(gO$PpSqKm)

# Choose random X, Y locations for n moorings and get D vals
# if scale = 1, then instruments are placed proportionally to local density

if(scale == 0) {gLocs <- gO[sample(1:nrow(gO), n, replace=FALSE),]} else {
  if(scale == 1){
    
    locs <- vector()
    i <- 0
    while(i<n){
      test.id <- sample(1:nrow(gO), 1)
      if(test.id %in% locs) {next} else # don't want to put 2 moorings at 1 loc
        if(rbinom(1, 1, gO$scaled.D[test.id]) == 1){locs <- c(locs, test.id); i <- i+1} else {next}
    }
    
    gLocs <- gO[locs,]
    
  }
}