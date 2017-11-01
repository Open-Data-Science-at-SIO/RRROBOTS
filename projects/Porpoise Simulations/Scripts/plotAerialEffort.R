
require(ggplot2)
require(dsm)
require(lubridate)
require(dplyr)

load("./Data/coastXY.RData")
load("./Data/flatfile.RData")
load("./Data/effort.RData")

yearlyeff <- eff.data %>% group_by(Year = year(Date)) %>% summarize(Eff = sum(Dist))
ggsave("./Figures/aerialEffort.pdf",
ggplot(yearlyeff, aes(x=Year, y=Eff))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=2000:2013,labels=as.character(2000:2013))+
  xlab("")+
  ylab("Aerial Survey Effort (km)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=-45, hjust=-0.1)),
height=4, width=6, units="in")


eff.data <- eff.data[which(eff.data$Depth>-200),]

C.Lat <- mean(range(flatfile$M.Lat))
C.Lon <- mean(range(flatfile$M.Lon))

start.xy <- latlong2km(eff.data$S.Lon, eff.data$S.Lat, C.Lon, C.Lat)
end.xy <- latlong2km(eff.data$E.Lon, eff.data$E.Lat, C.Lon, C.Lat)

SX <- start.xy$km.e
SY <- start.xy$km.n
EX <- end.xy$km.e
EY <- end.xy$km.n

segments <- data.frame(SX, SY, EX, EY)

ggplot()+
    #stat_bin2d(data=flatfile[!duplicated(flatfile$Sample.Label),], aes(x=X/1000, y=Y/1000))+
    geom_segment(data=segments, aes(x=SX, y=SY, xend=EX, yend=EY))+
    geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
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
          plot.margin=unit(c(2, 0, 0, 0), "lines"))

