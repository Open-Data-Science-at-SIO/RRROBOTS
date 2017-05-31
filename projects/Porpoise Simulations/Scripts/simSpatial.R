# simSpatial generates spatially explicit PPS values 
# for n moorings over y years given some rate of change
# if d.min and d.max are not supplied, default 

simSpatial <- function(n, y, r, d.min=0, d.max=-1000, b=0, scale=0){
  
  # n is the number of moorings
  # y is the number of years
  # r is the growth rate over the entire period (e.g., for a 50% decline r = -0.5)
  
  # d.min and d.max are the depth boundaries for mooring deployment
  # default d.min and d.max include the entire range of the spatial density obj.
  # note depth is negative, so default d.min=0 and d.max=-1000
  # 20170530 currently, effort allocation is 100% within depth boundaries
  # TODO: include par for % effort allocation to inshore v. offshore?
  
  # b is whether decline is applied uniformly over the study area (b=0; default)
  # or whether the decline results in contraction to core habitat (b=1)
  
  # scale is whether sampling design is proportional to underlying density (scale = 1)
  # default scale = 0, not proportional
  
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
  
  # Generate single random intercept term 
  int <- D.model$coefficients[1]
  i.se <- summary(D.model)$coefficients[4]
  i <- rnorm(1, int, i.se/sqrt(30))
  
  # Generate underlying densities for n spatial locs
  # using the mean and SE of the density surface
  d.n <- rnorm(n, gLocs$PpSqKm, gLocs$SE.PpSqKm)
  
  # Generate overall effect of density on PPS
  d.eff <- summary(D.model)$coefficients[3]
  d.se <- summary(D.model)$coefficients[6]
  b1 <- rnorm(1, d.eff, d.se/sqrt(30))
  
  # Add noise to overall rate of change in population over time
  if (r == 0) {cum.r <- rep(1, y)} else {
    rvals <- rnorm(y-2, mean=((1+r)^(1/(y-1))), sd=0.05) # sd is arbitrary
    rvals[y-1] <- (1+r)/prod(rvals[1:(y-2)]) # solve for final decline
    rvals <- rvals[sample(1:(y-1), (y-1))] 
    cum.r <- c(1, cumprod(rvals))}
  
  sdev <- sigma(D.model)
  
  df <- expand.grid("MOORING" = 1:n, 
                    "YEAR" = 1:y, 
                    "DENSITY" = NA, 
                    "PPS"= NA)
  df$DENSITY <- exp(rep(d.n, y)) # underlying mean density in real space
  df$X <- rep(gLocs$X, y)
  df$Y <- rep(gLocs$Y, y)
  df$D <- rep(gLocs$D, y)
  
  
  # PPS = exp(intercept + density effect + noise) * rate change
  df$PPSorig <- exp(i + (b1*d.n)[df$MOORING] + rnorm(n*y, 0, sd=sdev))
  
  X <- vector()
  # X corrects for habitat quality, so animals move towards core habitat
      for (i in 1:y){
        dsub <- subset(df, YEAR==i)
      X[i] <- sum(dsub$PPSorig)/sum(dsub$PPSorig*gLocs$I)
      }
  if(b==0){df$PPS <- df$PPSorig * cum.r[df$YEAR]} else
    if(b==1){df$PPS <- df$PPSorig * gLocs$I[df$MOORING] * cum.r[df$YEAR] * X[df$YEAR]}
    
  return(df)
  
}


# Code for testing the function
#
# new.data <- simSpatial(n = 50, y = 10, r = -.25)
# 
# new.data <- as.data.frame(new.data, names=c("MOORING", "YEAR", "DENSITY", "PPS"))
# 
# plot(new.data$YEAR, new.data$PPS)
# 
# new.model <- lme(fixed = log(PPS) ~ YEAR, 
#                  random = ~ 1 | MOORING, data = new.data)
# 
# summary(new.model)
# 
# plot(new.model, resid(.) ~ fitted(.))

