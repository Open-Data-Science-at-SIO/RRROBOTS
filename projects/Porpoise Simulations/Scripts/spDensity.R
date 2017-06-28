# Function to generate spatial density surface using
# aerial survey data from the Monterey Bay region

# TODO: clean up this file up so it works as a function called from the master.rmd

spDensity <- function(){
  
  # outputs: saves df as .rdata
  # with columns for lat, lon, depth, density, and se(density)
  # where density is estimated with a spline on lat and lon
  
  require(DSsim)
  require(Distance)
  require(mgcv)
  require(ggplot2)

  source("./Scripts/aerChopR.R")
#   aerChopR(daspath="./Data/DAS Files/", outloc="./Data/", 
#            eff.out="effort", si.out="sightings", flat.out="flatfile", 
#            species="pp", length=1, bf.cutoff=4)
  
  load("./Data/flatfile.RData")
  
  
  flatfile$Region.Label <- "MBAY"
  #flatfile$Area <- 370*1000*1000
  flatfile$object[which(!is.na(flatfile$object))] <- 1:length(na.omit(flatfile$object))
  
  ds.model <- ds(flatfile, truncation=500, adjustment="cos", order=2, formula=~as.factor(BF))
  
  ESW <- rev(round(as.numeric(names(table(predict(ds.model$ddf, esw=TRUE))))))
  
  # visualize all of the data collected
  ggplot(flatfile, aes(x=X, y=Y, size=size))+geom_point()+facet_wrap(~Date)
  
  ggplot(flatfile, aes(x=X, y=Y))+
    stat_bin_2d(binwidth=2500)+
    scale_fill_gradient(name="no. eff. segs.", low="yellow", high="red")
  
  
  od <- data.frame("object"=1:length(na.omit(flatfile$object)),
                   "Sample.Label"=flatfile$Sample.Label[which(is.na(flatfile$object) == FALSE)], 
                   "size"=flatfile$size[which(is.na(flatfile$object) == FALSE)], 
                   "distance"=flatfile$distance[which(is.na(flatfile$object)==FALSE)])
  
  # remove sightings beyond (arbitrary) truncation distance
  od <- od[which(od$distance<500),]
  
  sd <- data.frame("Effort"=flatfile$Effort[!duplicated(flatfile$Sample.Label)],
                   "Sample.Label"=unique(flatfile$Sample.Label),
                   "BF"=flatfile$BF[!duplicated(flatfile$Sample.Label)],
                   "X"=flatfile$X[!duplicated(flatfile$Sample.Label)],
                   "Y"=flatfile$Y[!duplicated(flatfile$Sample.Label)])
  
  pd <- expand.grid("X" = seq(range(flatfile$X)[1], range(flatfile$X)[2], by = 2500),
                    "Y"= seq(range(flatfile$Y)[1], range(flatfile$Y)[2], by = 2500))
  
  #mod1 <- dsm(N~s(X,Y), ds.model$ddf, sd, od)
  
  #mod1.pred <- predict(mod1, pd, 1)
  
  #pred.df <- cbind(pd, mod1.pred)
  
  #ggplot(pred.df, aes(x=X, y=Y, color=mod1.pred))+
  #  geom_point(size=5)+
  #  scale_color_gradient(low="yellow", high="red")
  
  ###
  
  # This loop generates density values for each subsegment of aerial survey data
  # according to total sightings on that seg/BF corrected effort
  
  data <- data.frame()
  
  for (i in unique(flatfile$Sample.Label)){
    
    d <- flatfile[flatfile$Sample.Label==i, ]
    
    if(is.na(d$object[1])){data <- rbind.data.frame(data, data.frame("X"=d$X[1], "Y"=d$Y[1], "D"=d$M.Depth[1], "PpSqKm"=0)); next} else {
      
      D <- sum(d$size)/(d$Effort[1]*2*ESW[d$BF[1] + 1]/(1000*1000)) 
      data <- rbind.data.frame(data, data.frame("X"=d$X[1], "Y"=d$Y[1], 
                                                "D" = d$M.Depth[1], "PpSqKm"=D))
      
    }
    
  }
  
  data <- data[which(data$D >= -1000),]
  
  save(data, file="./Data/DensityDF.RData")
  
  load("./Data/DensityDF.RData")
  
  # plot number of nonzero density values observed
  ggplot(data[which(data$PpSqKm != 0),], aes(x=X, y=Y))+
    stat_bin2d()+
    scale_fill_gradient(name="No.Obs",low="yellow", high="red")+
    theme_bw()
  
  
  model <- gam(PpSqKm ~ s(X, Y, bs="tp"), data=data, family = tw)
  
  grid <- expand.grid(seq(min(flatfile$M.Lon), max(flatfile$M.Lon), by = 0.005),
                      seq(min(flatfile$M.Lat), max(flatfile$M.Lat), by = 0.005))
  
  grid$D <- extract(mbathy, data.frame("x"=grid$Var1, "y"=grid$Var2))
  
  grid <- grid[which(grid$D>=-1000 & grid$D<=0),]
  
  C.Lat <- mean(range(flatfile$M.Lat))
  C.Lon <- mean(range(flatfile$M.Lon))
  
  xy <- latlong2km(grid$Var1, grid$Var2, C.Lon, C.Lat)
  
  grid$X <- xy$km.e*1000
  grid$Y <- xy$km.n*1000
  
  grid <- grid[,c(4, 5, 3)]
  
  save(gridOut, file="./Data/predGrid.RData")
  
  output <- predict.gam(model, gridOut[,1:2], type="response", se.fit=TRUE)
  
  gridOut$PpSqKm <- output$fit
  gridOut$SE.PpSqKm <- output$se.fit
  
  save(gridOut, file="./Data/predGrid.RData")
  
  ggplot(gridOut, aes(x=X, y=Y, color=SE.PpSqKm))+
    geom_point()
  
  
  coast <- read.csv("./Data/mbaycoast.csv")
  
  xy <- latlong2km(coast$lon, coast$lat, C.Lon, C.Lat)
  
  coast$X <- xy$km.e*1000
  coast$Y <- xy$km.n*1000
  
  save(coast, file="coastXY.RData")
  
  p <- ggplot()+
    geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
    geom_point(data=gridOut, aes(x=X/1000, y=Y/1000, color=PpSqKm), size=1)+
    scale_color_gradient2(name=bquote('Density ('*km^-2*')'),
                          low="dodgerblue2", mid="yellow", high="red", 
                          midpoint=max(gridOut$PpSqKm)/2, limits=c(0, max(gridOut$PpSqKm)))+
    theme_bw()+
    coord_equal(xlim=c(-50, 50), ylim=c(-50,50))+
    xlab("X (km)")+
    ylab("Y (km)")+
    theme(panel.grid.major=element_blank(), 
          panel.grid.minor=element_blank(),
          legend.key=element_blank(),
          legend.title=element_text(face="bold", size=10),
          
          strip.background=element_blank(),
          panel.margin=unit(1.25, "lines"),
          legend.key.height=unit(c(1.25), "lines"),
          plot.margin=unit(c(0, 0, 0, 0), "lines"),
          legend.position=c(0.1, 0.2))
  
  ggsave(p, file="GAMOut.pdf", width=6, height=6)
  
  
  ######## Need X and Y vals for C-Pod locs
  
  C.Lat <- mean(range(flatfile$M.Lat))
  C.Lon <- mean(range(flatfile$M.Lon))
  
  cpods <- read.csv("./Data/cpod.metadata.csv", header=TRUE)
  CLL <- cpods[which(cpods$year==2013), c(3,5:6)]
  cxy <- latlong2km(CLL$lon, CLL$lat, C.Lon, C.Lat)
  CXY <- data.frame("Mooring"=CLL$location, "X"=cxy$km.e*1000, "Y"=cxy$km.n*1000)
  
  
  cpod.pred <- predict.gam(model, CXY, type="response", se.fit=TRUE)
  CXY <- cbind(CXY, "Density"=cpod.pred$fit, "SE.D" = cpod.pred$se.fit)
  
  save(CXY, file="./Data/CXY.RData")  
  
  
  
  
}

