
#----IMPORT PACKAGES----

library(ggplot2)
library(dplyr)
library(ggpubr)  # for 'ggarrange' (to put multiple ggplots together)

########################################################################

##                   Whale paper Figures                              ##

########################################################################

DF = read.csv("DATA/detectionProbabilities.csv")

distLabels = c("< 100","100-500","500-1000","1000-1500",
                "1500-2000","2000-3000","3000-4000","4000-5000","> 5000")

lineProb = DF %>%
  #mutate(Mandatory = ordered(Mandatory, levels = c("Yes","No"))) %>%
  ggplot( aes(minDist,prob, colour = percOfEnc))+geom_point() + geom_line()+
  facet_wrap(~ location, nrow = 1)+
  geom_line(size = 1.5)+
  geom_point(size = 2)+
  scale_colour_discrete(type=c("#b2df8a","#1f78b4","#a6cee3"))+
  xlab("Distance (m)")+ylab("Detection Probability")+
  theme_classic() +
  theme(axis.text.y = element_text(colour = "black", size = 7),
        axis.text.x = element_text(colour = "black", angle = 90, hjust = 1, vjust = 0.5, size = 6),
        legend.position = "none")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01))+
  scale_x_continuous(expand = c(0, 50),
                     breaks = c(0,100,500,1000,1500,2000,3000,4000,5000), labels=distLabels)

lineProb

ggsave("CK Results/LinePlot_DetectionProb.png",plot = lineProb,
       units = "in", width = 6.5, height = 5)

