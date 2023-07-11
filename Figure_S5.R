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
LLC_clim<-c(3,1,1,6,10,1,0,0,1,14,36,22) #1951-2010  83E-95E,5N-11N
round((sum(LLC_clim[10:12])/sum(LLC_clim))*100) # OND %
    
(sum(LLC_clim[10:12])/sum(LLC_clim))*100


LLC<-data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"), LLC_clim)

#------------------------------------------------------------ 
rect1 <- data.frame (xmin=9.5, xmax=12.5, ymin=0, ymax=Inf)
p1<-ggplot(data=LLC, aes(x=Month, y=LLC_clim)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE) +
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1.5*0.5, color = "blue",fill = "blue",alpha = 0.7)+
  geom_text(aes(label=LLC_clim), vjust=1.6, color="white",
            position = position_dodge(0.5), size=3)+
  geom_vline(xintercept = 9.5, linetype="dashed", color = "red", size=.3)+
  geom_vline(xintercept = 12.5, linetype="dashed", color = "red", size=.3)+
  geom_text(x=11, y=40.05, label="76%", color="red",family="Helvetica", size=3, angle=0)+
  
  geom_segment(aes(x = 9.5, y = 39, xend = 12.5, yend = 39, colour = "segment"), lineend = "round", linejoin = "round", color="red", size=.1, arrow = arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = 12.5, y = 39, xend = 9.5, yend = 39, colour = "segment"), lineend = "round", linejoin = "round", color="red", size=.1, arrow = arrow(length = unit(0.03, "npc"))) +
  #geom_hline(intercept=20)+
  #scale_fill_brewer(palette="Paired")+
  #scale_fill_manual(values = c("yellow", "orange","red")) +
  scale_x_discrete(name = "Month",limits=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", "Oct","Nov","Dec"))+
  # limit the vertical space to 1 to 50, but keep the data
  coord_cartesian(ylim = c(0, 40)) +
  scale_y_continuous(name = "Number of LLC", breaks = seq(0,40,5), limits = c(0,40))+
  ggtitle("Monthly frequency of LLC in the Bay of Bengal") +
  theme_gray() +
  #theme_economist() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        #panel.grid.minor = element_blank(),
        #panel.grid.major = element_blank(),
        plot.title = element_text(color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=10, angle=0),
        axis.text.x = element_text(color="black",family="Helvetica", 
                                   size=10, angle=0),
        axis.text.y = element_text(#face="bold",
                                    color="black",family="Helvetica", 
                                   size=10, angle=0)
  )

#theme_minimal()

p1
#ggsave("LLC_CLIM_BOB_1951_2010.png", width = 20, height = 15, units = "cm", dpi = 300)
 


ggsave(p1, file="FigureS5.eps", device=cairo_ps, width = 130, height = 88, units = "mm", dpi = 1600,family="Helvetica")

