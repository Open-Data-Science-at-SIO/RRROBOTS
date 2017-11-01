require(nlme)
load("./Data/predGrid.RData") #gridOut created by spDensity
load("./Data/Dmodel.RData") #

d.min <- 0
d.max <- -150
n <- 50

# the impact coefficient for the magnitude of the impact is scaled from 0 to 1
# according to habitat quality.  So high density areas have a lower I val.
gridOut$I <- (gridOut$PpSqKm*(1/(max(gridOut$PpSqKm)))) 

# Restrict possible instrument locations to be within specified depth range
gO <- gridOut[which(gridOut$D>d.max & gridOut$D <= d.min),]
# Add a column for scaled density
gO$scaled.D <- gO$PpSqKm/max(gO$PpSqKm)

# Choose random X, Y locations for n moorings and get D vals
# if scale = 1, then instruments are placed proportionally to local density

design <- 2

if(design == 0) {gLocs <- gO[sample(1:nrow(gO), n, replace=FALSE),]} else {
  if(design == 1){ 
    s1 <- round(.84*n)
    s2 <- n-s1
    
    gs1 <- gO[which(gO$D>-40),]
    gs2 <- gO[which(gO$D<=-40),]
    
    gLocs <- rbind.data.frame(gs1[sample(1:nrow(gs1), s1, replace=FALSE),],
                              gs2[sample(1:nrow(gs2), s2, replace=FALSE),])
    
  } else {
    if(design == 2){
      
      locs <- vector()
      i <- 0
      while(i<n){
        test.id <- sample(1:nrow(gO), 1)
        if(test.id %in% locs) {next} else # don't want to put 2 moorings at 1 loc
          if(rbinom(1, 1, gO$scaled.D[test.id]) == 1){locs <- c(locs, test.id); i <- i+1} else {next}
      }
      
      gLocs <- gO[locs,]
      
    }
  }}

gLocs$Design <- "Scaled"

gAll <- rbind.data.frame(gAll, gLocs)

gAll$Design <- factor(gAll$Design, 
                      levels=c("Random", "Stratified", "Scaled"),
                      ordered=TRUE)

load("./Data/coastXY.RData")

ggsave("./Figures/designExamples.pdf",
ggplot()+
  geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
  geom_point(data=gAll, aes(x=X/1000, y=Y/1000), size=0.5)+
  facet_wrap(~Design)+
  theme_bw()+
  coord_equal(xlim=c(-50, 50), ylim=c(-50, 50))+
  xlab("X (km)")+
  ylab("Y (km)")+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill="aliceblue"),
        legend.key=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1.25, "lines"),
        plot.margin=unit(c(2, 0, 0, 0), "lines")),
height=3, width=6.5, units="in")
