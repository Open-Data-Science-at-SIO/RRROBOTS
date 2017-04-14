
### Power analysis based on TRENDS (Gerrodette et al. 1987)

require(fishmethods)
require(ggplot2)

powertrend(trend=1, A1=1, PSE=0.34, maxyrs=10, step=1, alpha=0.05, tail=2, pR=100)

cvs <- c(0.34, 0.24, 0.15, 0.11, 0.07)
aer.power <- expand.grid("pR" = -100:100, "Reps" = c(1, 2, 5, 10, 25), "CV"=NA, "Power"=NA)

df <- data.frame()

for (c in 1:length(cvs)){
  
  res <- powertrend(trend=1, A1=1, PSE=cvs[c], maxyrs=10, step=1, alpha=0.05, tail=2, pR=100)
  df <- rbind.data.frame(df, res[,c(3,7)])
  
}

aer.power$CV <- df$pse
aer.power$Power <- df$power

cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#0072B2",  "#CC79A7")

ggplot()+
  geom_line(data=aer.power, aes(x=pR, y=Power, group=as.factor(Reps), color=as.factor(Reps)), size=1)+
  scale_color_manual(name="No. Replicates", values=cbPalette)+
  geom_line(data=data.frame("pR"=c(-50, 50), "Power"=c(0.8, 0.8)), 
            aes(x=pR, y=Power), linetype=1, size=1)+
  xlab("Percent Change in Population")+
  ylab("Power")+
  theme_bw()+
  xlim(-50, 50)+
  theme(legend.key=element_blank())
  

