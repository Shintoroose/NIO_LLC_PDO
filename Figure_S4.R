#Shinto Roose, Cochin University of Science and Technology

library(ncdf4)
library(ggplot2) # package for plotting
library(RColorBrewer)
library(scales)
library("ggpubr")
library(grid)

# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory
getwd()
# ================================================================================ 





#-------Figure1C
rect1 <- data.frame (xmin=-Inf, xmax=3.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=3.5, xmax=Inf, ymin=-Inf, ymax=Inf)

 

#------- OLD Figure1D NIO
Cyclones<-c(21,33,43,24,15,9,8,4) #1951-2010
epoch1<-c(14,19,29,10,3,3,6,1)  #1951-1980
epoch2<-c(7,14,14,12,12,6,2,3)  #1981-2010
EpochalDiff=epoch2-epoch1


LatBands=c("5-7°N","7-9°N","9-11°N","11-13°N","13-15°N","15-17°N","17-19°N","19-21°N")

df<-data.frame(LatBand=c(1:8),epoch1,epoch2, EpochalDiff)


df$Phase <- ifelse(df$EpochalDiff < 0, "Negative","Positive")

theme_set(theme_grey()) 
pdiffLat<-ggplot(df, aes(x=LatBand, y=EpochalDiff)) +
  #geom_line(color = "blue",size = 2)+
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1.5*0.5,alpha = 0.7,aes(fill = Phase), show.legend = FALSE) +
  scale_fill_manual(values=c(Positive="steelblue",Negative="firebrick1")) +
  scale_y_continuous(name="Number of Cyclones", breaks = seq(-15,15,3), limits = c(-15,15))+
  coord_flip()+
  #scale_x_reverse()+
  scale_x_discrete(name = "Latitudinal band",limits=LatBands) + 
  geom_hline(yintercept = 0, lty = 2, color = "royalblue4") +
  ggtitle("Epochal change in NIO cyclones (60°E-100°E)") +
  theme(#legend.position="bottom",
    #legend.position=c(.85,.60),
    axis.text.x = element_text(family="Helvetica"
                               ,size=9
                               #,face="bold"
                               ,color="black"),
    axis.text.y = element_text(family="Helvetica"
                               ,size=9
                               #,face="bold"
                               ,color="black"),
    axis.title.y = element_text(family="Helvetica"
                                ,size=9
                                #,face="bold"
                                ,color="black"),
    axis.title.x = element_text(family="Helvetica"
                                ,size=9
                                #,face="bold"
                                ,color="black"),
    plot.title = element_text(hjust = 0.5, color="black", 
                              #       face="bold"
                              #       family="Comic Sans MS",
                              #       family="CM Roman",
                              family="Helvetica",
                              #       family="Sans",
                              #       family="Serif",
                              size=9, angle=0))
pdiffLat
#ggsave("60_100E.png", width = 12, height = 10, units = "cm", dpi = 300)

ggsave(pdiffLat, file="FigureS4.eps", device=cairo_ps, width = 88, height = 88, units = "mm", dpi = 1600,family="Helvetica")
 
