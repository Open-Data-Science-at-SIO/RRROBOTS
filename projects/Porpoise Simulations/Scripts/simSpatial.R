# simSpatial generates spatially explicit PPS values 
# for n moorings over y years given some rate of change
# if d.min and d.max are not supplied, default 

simSpatial <- function(n, y, r, d.min=0, d.max=-1000, b=0){
  

  # n is the number of moorings
  # y is the number of years
  # r is the growth rate over the entire period (e.g., for a 50% decline r = -0.5)
  
  # d.min and d.max are the depth boundaries for mooring deployment
  # default d.min and d.max include the entire range of the spatial density obj.
  # note depth is negative, so default d.min=0 and d.max=-1000
  
  # b is whether decline is applied uniformly over the study area (b=0; default)
  # or whether the decline results in contraction to core habitat (b=1)
  
  require(nlme)
  load("./Data/predGrid.RData") #gridOut created by spDensity
  load("./Data/Dmodel.RData")
  
  # the impact coefficient for the magnitude of the impact is scaled from 0 to 1
  # according to habitat quality.  So high density areas have a lower I val.
  gridOut$I <- (gridOut$PpSqKm*(1/(2*max(gridOut$PpSqKm)))) + 0.5

  # Choose random X, Y locations for n moorings and get D vals
  gO <- gridOut[which(gridOut$D>d.max & gridOut$D <= d.min),]
  gLocs <- gO[sample(1:nrow(gO), n, replace=FALSE),]
  
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

# new.data <- simSpatial(n = nrow(gridOut), y = 10, r = -.5, b = 1)
# load("./Data/coastXY.RData")
# require(animation)
# saveGIF(
# for (i in 1:y){
# p <- ggplot()+
#   ggtitle(paste("Year", i, sep=" "))+
#   geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
#   geom_point(data=subset(new.data, YEAR==i), 
#              aes(x=X/1000, y=Y/1000, color=PPS, size =PPS, alpha=PPS, stroke=1))+
#   scale_alpha_continuous(range=c(0.25, 1), guide="none")+
#   scale_size_continuous(range=c(0.1, 3), guide="none")+
#   scale_color_gradient2(low="dodgerblue2", mid="yellow", high="red",
#                         midpoint=(max(new.data$PPS)/2), 
#                         limits=c(0, max(new.data$PPS)))+
#   theme_bw()+
#   coord_equal(xlim=c(-50, 50), ylim=c(-50, 50))+
#   xlab("X (km)")+
#   ylab("Y (km)")+
#   theme(panel.grid.major=element_blank(), 
#         panel.grid.minor=element_blank(),
#         legend.key=element_blank(),
#         legend.title=element_text(face="bold", size=10),
#         
#         strip.background=element_blank(),
#         panel.margin=unit(1.25, "lines"),
#         legend.key.height=unit(c(1.25), "lines"),
#         plot.margin=unit(c(2, 0, 0, 0), "lines"),
#         legend.position=c(0.1, 0.2))+
#   theme(plot.title = element_text(face="bold"))
# print(p)
# }, movie.name="./Figures/PPScore.gif")
# 
# 
# 
# # Code for testing the function
# #
# # new.data <- simSpatial(n = 50, y = 10, r = -.25)
# # 
# # new.data <- as.data.frame(new.data, names=c("MOORING", "YEAR", "DENSITY", "PPS"))
# # 
# # plot(new.data$YEAR, new.data$PPS)
# # 
# # new.model <- lme(fixed = log(PPS) ~ YEAR, 
# #                  random = ~ 1 | MOORING, data = new.data)
# # 
# # summary(new.model)
# # 
# # plot(new.model, resid(.) ~ fitted(.))
# 
# X <- sum(gridOut$PpSqKm)*0.5/sum(gridOut$PpSqKm * 1/(gridOut$Q) * 0.9)
