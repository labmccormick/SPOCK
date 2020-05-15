ggplot(data=meltladin, aes(x=variable, y=value, group=Names)) +
  geom_line()+
  geom_point()
library(Rmsic)
tgc <- summarySE(meltzeo, measurevar="value", groupvars=c("Names","variable"))
pd <- position_dodge(0.1) # move them .05 to the left and right
ggplot(tgc, aes(x=variable, y=value, colour=Names, group = Names)) + 
  geom_errorbar(aes(ymin=(value-se), ymax=(value+se)), width=0.1, size=1) +
  geom_line(size=1) +
  ggtitle("O.D Measured by Non-Linear Instrumentation")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Actual O.D 600nm", y="O.D 600nm measured by Bioscreen", colour="Strains")
  

ladintow[1,2:11]<-ladintow[1,2:11]-ladintow[1,2]
