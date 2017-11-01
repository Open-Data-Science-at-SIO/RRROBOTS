# NOTE 05302017: prev. there was a bug; b and depth were not being passed to simSpatial in line 19 
runSim <- function(n.moorings, p.change, nrep, b, d.min, d.max, design, file.out){

  source("./Scripts/simSpatial.R")
  #n.moorings <- c(10, 25, 50, 75, 100)
  #p.change <- seq(-0.5, 0.5, by = 0.05)
  #nrep <- 1000

  tab.res <- data.frame()

  for (no in n.moorings){
  
    for (pc in p.change){
    
      r.est <- vector()
    
      for (i in 1:nrep){
      
      nd <- as.matrix(simSpatial(no, 10, pc, d.min=d.min, d.max=d.max, design=design, b=b))
      
      nd <- as.data.frame(nd, names=c("MOORING", "YEAR", "DENSITY", "PPS"))
      
      nd$PPS <- nd$PPS+1 # so that zeros can be log-transformed
      
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

n.moorings <- c(10, 25, 50, 75, 100)
p.change <- -0.25
p.change.fig <- seq(-0.5, 0.5, by = 0.05)
p.change.table <- c(-0.5, -0.25, -0.1, 0.25)
nrep <- 1000

# RANDOM CASE FOR FIGURE
runSim(n.moorings, p.change.fig, nrep, b=0, d.min=0, d.max=-150, design=0,
       file.out="./Data/RandomUnifTo150_AllSensorsAllChange_1000rep.RData")
load("./Data/RandomUnifTo150_AllSensorsAllChange_1000rep.RData")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#0072B2",  "#CC79A7")
ggsave("./Figures/ChangeVPower.pdf",
       ggplot()+
         geom_line(data=data.frame("P.Change"=c(-0.5, 0.5), "P.Dect"=c(0.8, 0.8)), 
                   aes(x=P.Change, y=P.Dect), linetype=1, size=1)+
         geom_line(data=tab.res, aes(x=P.Change, y=P.Dect, 
                                     group=as.factor(No.M), color=as.factor(No.M)), size=1)+
         scale_color_manual(name="No. Sensors", values=cbPalette)+
         
         # geom_line(data=data.frame("P.Change"=c(-0.5, -0.5), "P.Dect"=c(0.14, 0.33)), 
         #          aes(x=P.Change, y=P.Dect) ,size =3, color = "red")+
         xlab("Percent Change in Population")+
         ylab("Power")+
         scale_x_continuous(labels=scales::percent)+
         theme_bw()+
         theme(legend.key=element_blank()),
       height=4, width=5, units="in")

# SINGLE CELL RESULTS
runSim(75, -0.25, 10000, b=0, d.min=0, d.max=-150, design=0, 
       file.out="./Data/RandomUnifTo150_75sensors-25.RData")
runSim(75, -0.25, 10000, b=0, d.min=0, d.max=-150, design=1, 
       file.out="./Data/StratUnifTo150_75sensors-25.RData")
runSim(75, -0.25, 10000, b=0, d.min=0, d.max=-150, design=2, 
       file.out="./Data/ScaledUnifTo150_75sensors-25.RData")
runSim(75, -0.25, 10000, b=1, d.min=0, d.max=-150, design=0, 
       file.out="./Data/RandomContractTo150_75sensors-25.RData")
runSim(75, -0.25, 10000, b=1, d.min=0, d.max=-150, design=1, 
       file.out="./Data/StratContractTo150_75sensors-25.RData")
runSim(75, -0.25, 10000, b=1, d.min=0, d.max=-150, design=2, 
       file.out="./Data/ScaledContractTo150_75sensors-25.RData")

# COMPARING DESIGNS WHEN RANGE CONTRACTION OCCURS
# FIXED NUMBER OF SENSORS, CHANGE -.5 to .5

n.moorings <- 75
p.change <- -0.25
p.change.fig <- seq(-0.5, 0.5, by = 0.05)
nrep <- 1000

runSim(75, p.change.fig, nrep, b=1, d.min=0, d.max=-150, design=0, 
       file.out="./Data/RandomContractTo150_75sensorsAllChanges.RData")
load("./Data/RandomContractTo150_75sensorsAllChanges.RData")
tr0 <- tab.res
tr0$Design <- "Random"
runSim(75, p.change.fig, nrep, b=1, d.min=0, d.max=-150, design=1, 
       file.out="./Data/StratContractTo150_75sensorsAllChanges.RData")
load("./Data/StratContractTo150_75sensorsAllChanges.RData")
tr1 <- tab.res
tr1$Design <- "Stratified"
runSim(75, p.change.fig, nrep, b=1, d.min=0, d.max=-150, design=2, 
       file.out="./Data/ScaledContractTo150_75sensorsAllChanges.RData")
load("./Data/ScaledContractTo150_75sensorsAllChanges.RData")
tr2$Design <- "Scaled"

compare1 <- rbind.data.frame(tr0, tr1, tr2)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#0072B2",  "#CC79A7")

ggplot()+
  geom_line(data=data.frame("P.Change"=c(-0.5, 0.5), "P.Dect"=c(0.8, 0.8)), 
            aes(x=P.Change, y=P.Dect), linetype=1, size=1)+
  geom_line(data=compare1, aes(x=P.Change, y=P.Dect, color=Design), size=1.5)+
  scale_color_manual(name="Design", values=cbPalette)+
  xlab("Percent Change in Population")+
  ylab("Power")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.key=element_blank())


# COMPARING DESIGNS ACROSS NO. SENSORS

n.sensors <- seq(5, 100, by = 5)
p.change <- -0.25
p.change.fig <- seq(-0.5, 0.5, by = 0.05)
nrep <- 1000

runSim(n.sensors, p.change, nrep, b=1, d.min=0, d.max=-150, design=0, 
       file.out="./Data/RandomContractTo150_AllSensors-25.RData")
load("./Data/RandomContractTo150_AllSensors-25.RData")
tr3 <- tab.res
tr3$Design <- "Random"
runSim(n.sensors, p.change, nrep, b=1, d.min=0, d.max=-150, design=1, 
       file.out="./Data/StratContractTo150_AllSensors-25.RData")
load("./Data/StratContractTo150_AllSensors-25.RData")
tr4 <- tab.res
tr4$Design <- "Stratified"
runSim(n.sensors, p.change, nrep, b=1, d.min=0, d.max=-150, design=2, 
       file.out="./Data/ScaledContractTo150_AllSensors-25.RData")
load("./Data/ScaledContractTo150_AllSensors-25.RData")
tr5 <- tab.res
tr5$Design <- "Scaled"

compare2 <- rbind.data.frame(tr3, tr4, tr5)

ggplot()+
  geom_line(data=data.frame("P.Change"=c(-0, 100), "P.Dect"=c(0.8, 0.8)), 
            aes(x=P.Change, y=P.Dect), linetype=1, size=1)+
  geom_line(data=compare2, aes(x=No.M, y=P.Dect, color=Design), size=1.5)+
  scale_color_manual(name="Design", values=cbPalette)+
  xlab("Number of Sensors")+
  ylab("Power")+
  theme_bw()+
  theme(legend.key=element_blank())

#


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