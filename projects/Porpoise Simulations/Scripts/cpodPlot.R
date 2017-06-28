load("./Data/coastXY.RData")
load("./Data/CXY.RData")

require(dplyr)
require(ggplot2)

cxy <- select(CXY, Mooring, X, Y)
cxy$Mooring <- gsub(pattern="CIEE", replacement="CPOD", x=cxy$Mooring)

ggplot()+
  geom_point(data=cxy, aes(x=X/1000, y=Y/1000))+
  geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
  geom_text(data=cxy,
              aes(x=X/1000, y=Y/1000, label=as.character(Mooring)),size=3,vjust=2.5) +
  theme_bw()+
  coord_equal(xlim=c(25, 52), ylim=c(0, 22))+
  xlab("X (km)")+
  ylab("Y (km)")+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill="aliceblue"),
        legend.key=element_blank(),
        strip.background=element_blank(),
        panel.margin=unit(1.25, "lines"),
        plot.margin=unit(c(2, 0, 0, 0), "lines"))