# NOTE 05302017: prev. there was a bug; b and depth were not being passed to simSpatial in line 19 
runSim <- function(n.moorings, p.change, nrep, b, d.min, d.max, scale, file.out){

  source("./Scripts/simSpatial.R")
  #n.moorings <- c(10, 25, 50, 75, 100)
  #p.change <- seq(-0.5, 0.5, by = 0.05)
  #nrep <- 1000

  tab.res <- data.frame()

  for (no in n.moorings){
  
    for (pc in p.change){
    
      r.est <- vector()
    
      for (i in 1:nrep){
      
      nd <- as.matrix(simSpatial(no, 10, pc, d.min=d.min, d.max=d.max, b=b, scale=scale))
      
      nd <- as.data.frame(nd, names=c("MOORING", "YEAR", "DENSITY", "PPS"))
      
      nm <- lme(fixed = log(PPS) ~ YEAR, 
                random = ~ 1 | MOORING, data = nd)
      
      ye <- ifelse(summary(nm)$tTable[10]<0.05 & sign(summary(nm)$tTable[2])==sign(pc), 1, 0)
      
      r.est <- c(r.est, ye)
      
    } # end i in n.rep
    
    tab.res <- rbind.data.frame(tab.res, 
                                cbind.data.frame("No.M" = no, "P.Change" = pc, 
                                                 "P.Dect" = sum(r.est)/length(r.est)))
    
    } # end pc in p.change
  
  
  } # end nm in no. moorings

  save(tab.res, file=file.out)
  
}

n.moorings <- 75
p.change <- -0.25
p.change.fig <- seq(-0.5, 0.5, by = 0.05)
p.change.table <- c(-0.5, -0.25, -0.1, 0.25)
nrep <- 10000
#runSim(n.moorings, p.change, nrep, b, d.min, d.max, file.out)
#runSim(n.moorings, p.change.fig, 1000, 0, file.out="./Data/simSpatialb0Results.RData")

runSim(n.moorings, p.change, nrep, 0, d.min=0, d.max=-1000, scale=1, file.out="./Data/simSpatialScaledb0Resultsx10000_SingleCell.RData")
runSim(n.moorings, p.change, nrep, 1, d.min=0, d.max=-1000, scale=1, file.out="./Data/simSpatialScaledb1Resultsx10000_SingleCell.RData")


runSim(n.moorings, p.change, nrep, 0, d.min=0, d.max=-100, file.out="./Data/simSpatialStratb0Resultsx10000_SingleCell.RData")
runSim(n.moorings, p.change, nrep, 1, d.min=0, d.max=-100, file.out="./Data/simSpatialStratb1Resultsx10000_SingleCell.RData")



cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#0072B2",  "#CC79A7")

ggplot()+
  geom_line(data=tab.res, aes(x=P.Change, y=P.Dect, 
                              group=as.factor(No.M), color=as.factor(No.M)), size=1.5)+
  scale_color_manual(name="No. Moorings", values=cbPalette)+
  geom_line(data=data.frame("P.Change"=c(-0.5, 0.5), "P.Dect"=c(0.8, 0.8)), 
            aes(x=P.Change, y=P.Dect), linetype=4, size=2)+
  geom_line(data=data.frame("P.Change"=c(-0.5, -0.5), "P.Dect"=c(0.14, 0.33)), 
            aes(x=P.Change, y=P.Dect) ,size =3, color = "red")+
  xlab("Percent Change in Population")+
  ylab("Power")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.key=element_blank())

table <- dcast(tab.res, No.M ~ P.Change)

print(table)