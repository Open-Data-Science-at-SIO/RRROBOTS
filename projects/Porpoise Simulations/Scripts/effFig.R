
require(tidyr)
require(dplyr)
require(reshape2)

load("./Data/effort.RData")
load("./Data/CpodPPS.RData")

call <- data.frame("Month" = month(PPS.Data$DATE), "Year"=year(PPS.Data$DATE))
aall <- data.frame("Month" = month(eff.data$Date), "Year"=year(eff.data$Date))

ctable <- table(call)
atable <- table(aall)

ctally <- melt(ctable)
ctally$value <- ifelse(ctally$value==0, NA, 1)

atally <- melt(atable)
atally$value <- ifelse(atally$value==0, NA, 1)

t <- full_join(atally, ctally, by=c("Month", "Year"))
names(t) <- c("Month", "Year", "Aerial", "Acoustic")

t <- na.omit(melt(t, id.vars=c("Month", "Year")))

for (i in 1:nrow(t)){
if(t$variable[i] == "Aerial"){t$Month[i] <- t$Month[i]-0.1} else
  if(t$variable[i] == "Acoustic"){t$Month[i] <- t$Month[i]+0.1}
}

ggsave("./Figures/EffTable.pdf",
ggplot(t, aes(x=Month, y=Year, shape=variable))+
  geom_point()+
  scale_y_reverse(breaks=2015:2000)+
  ylab("")+
  scale_x_continuous(breaks = 1:12)+
  scale_shape_manual(name="Survey Type", values=c(19, 17))+
  theme_bw()+
  theme(legend.position=c(0.15, 0.15),
        legend.key=element_blank(),
        panel.grid.minor=element_blank()),
width=5, height=4, units="in")