
require(ggplot2)
require(dsm)
require(lubridate)
require(dplyr)

load("./Data/coastXY.RData")
load("./Data/flatfile.RData")
load("./Data/effort.RData")
load("./Data/sightings.RData")
load("./Data/predGrid.RData")

usa<-map_data("usa")
mapstates <- map_data("state")

yearlyeff <- eff.data %>% group_by(Year = year(Date)) %>% summarize(Eff = round(sum(Dist)))
yearlysi <- flatfile  %>% group_by(Year = year(Date)) %>% summarize(Si = length(na.omit(size)))

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

C.Lat <- mean(range(flatfile$M.Lat))
C.Lon <- mean(range(flatfile$M.Lon))

start.xy <- latlong2km(eff.data$S.Lon, eff.data$S.Lat, C.Lon, C.Lat)
end.xy <- latlong2km(eff.data$E.Lon, eff.data$E.Lat, C.Lon, C.Lat)

SX <- start.xy$km.e
SY <- start.xy$km.n
EX <- end.xy$km.e
EY <- end.xy$km.n

segments <- data.frame(SX, SY, EX, EY)

aer.map <- ggplot()+
    #stat_bin2d(data=flatfile[!duplicated(flatfile$Sample.Label),], aes(x=X/1000, y=Y/1000))+
    geom_segment(data=segments, aes(x=SX, y=SY, xend=EX, yend=EY), alpha=0.5)+
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


inset <- ggplot() +
  geom_polygon(data=mapstates[mapstates$region=="california",],
               aes(long,lat), fill="grey60") +
  coord_map()+
 # coord_map(xlim=c(-124.4, -120), ylim=c(34.25, 38.5))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border = element_rect(fill=NA,colour="black"),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_rect(fill="aliceblue",colour=NA),
        plot.background=element_rect(fill=NA,colour=NA),
        strip.background=element_rect(fill=NA,colour=NA)) +
  geom_rect(aes(xmin=-122.9922, xmax=-121.65,ymin=36.31, ymax=37.25),
            fill="NA",colour="black", size=0.6) +
  geom_text(size=3, fontface="italic", aes(label="Monterey Bay", -121.15, 35.75))


load("./Data/coastXY.RData")
load("./Data/CXY.RData")

require(dplyr)

cxy <- select(CXY, Mooring, X, Y)
cxy$Mooring <- gsub(pattern="CIEE", replacement="CPOD", x=cxy$Mooring)

ac.map <- ggplot()+
  geom_point(data=cxy, aes(x=X/1000, y=Y/1000), size=2)+
  geom_polygon(data=coast, aes(x=X/1000, y=Y/1000), fill="gray")+
  geom_text(data=cxy,
            aes(x=X/1000, y=Y/1000, label=as.character(Mooring)),size=2,vjust=2.5) +
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



### Printing CPOD map with inset of California 



v1<-viewport(width = 0.5, height = 1, x = 0.25, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.5, height = 0.4, x = 0.17, y = 0.3) #plot area for the inset map
v3<-viewport(width = 0.4, height = 1, x=0.75, y = 0.5)

pdf("./Figures/AerAcMapwInset.pdf", width=8, height=4)
print(aer.map, vp=v1) 
print(inset, vp=v2)
print(ac.map, vp=v3)
dev.off()

