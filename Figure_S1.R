
# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory
getwd()
# ================================================================================ 
library(ncdf4)
library(ggplot2) # package for plotting
library(png)
library(raster)
library("ggpubr")
library(plyr)
library(extrafont)
library(grid)
library(pBrackets) 

df2 <- data.frame(LLC=rep(c("October", "November", "December"), each=2),
                  dose=rep(c("Epoch-1", "Epoch-2"),3),
                  len=c(11, 3, 21, 15, 14, 8))


# Sort by dose and supp
df_sorted <- arrange(df2, dose, LLC) 

df_cumsum <- ddply(df_sorted, "dose",
                   transform, label_ypos=cumsum(len))





# Create the barplot
p<-ggplot(data=df_cumsum, aes(x=dose, y=len, fill=LLC)) +
  geom_bar(stat="identity", width = 0.3)+
  #scale_fill_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Paired")+
  #scale_fill_manual(values = c("yellow", "orange","red")) +
  annotate("text", x = c(1,1,1,2,2,2), y = c(40,20,5,22,10,1.5), label = c("14","21","11","8","15","3"), size = 4,family="Helvetica") +
  scale_x_discrete(name = "")+
  scale_y_continuous(name = "Number of LLC", breaks = seq(0,50,10), limits = c(0,50))+
  ggtitle("Epochal change in LLC") +
  theme(aspect.ratio = 1.5/1,
        legend.title = element_text(color = "black", size = 12,family="Helvetica"),
        legend.text = element_text(color = "black", size = 12,family="Helvetica"),
          axis.text.x = element_text(family="Helvetica"
                                     ,size=12
                                     #,face="bold"
                                     ,color="black"),
          axis.text.y = element_text(family="Helvetica"
                                     ,size=12
                                     #,face="bold"
                                     ,color="black"),
          axis.title.y = element_text(family="Helvetica"
                                      ,size=12
                                      #,face="bold"
                                      ,color="black"),
          axis.title.x = element_text(family="Helvetica"
                                      ,size=12
                                      #,face="bold"
                                      ,color="black"),
          plot.title = element_text(hjust = 0.5, color="black", 
                                    #       face="bold"
                                    #       family="Comic Sans MS",
                                    #       family="CM Roman",
                                    family="Helvetica",
                                    #       family="Sans",
                                    #       family="Serif",
                                    size=12, angle=0),
        legend.position = c(0.7, 0.8))
p
#ggsave("FigureS1.png", width = 10, height = 13, units = "cm", dpi = 300)
#ggsave("FigureS2_thesis.png", width = 20, height = 15, units = "cm", dpi = 300)
#system("convert -trim FigureS2_thesis.png FigureS2_thesis.png")

ggsave(p, file="FigureS1.eps", device=cairo_ps, width = 85, height = 120, units = "mm", dpi = 1600,family="Helvetica")



