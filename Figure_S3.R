# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory
getwd()
# ================================================================================INPUT
library(ncdf4)
library(ggplot2) # package for plotting
library(png)
 
library(formattable)
library(kableExtra)
library(dplyr)
library("ggpubr")
require("ggrepel")




                    

#---------------------------------------------------------------------------------BoB TC 83-95E
BOB_TC <- readLines("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/IMD_BOB_TC/BoB_TropicalCyclone_OND_1951_2020.csv")
colname<-c("Year","5-6N",">6-7N",">7-8N",">8-9N",">9-10N",
           ">10-11N",">11-12N",">12-13N",">13-14N",">14-15N",
           ">15-16N",">16-17N",">17-18N",">18-19N",">19-20N",
           ">20-21N",">21N","Total","LLC","HLC")


TC_1951_2020 = read.table(text = BOB_TC,  
                          col.names = colname,
                          header=F, 
                          sep = ",",
                          skip = 1, nrows = length(BOB_TC) - 2)

 
#----------------------------------------------------------
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function  (https://statisticsglobe.com/standard-error-in-r-example)
#----------------------------------------------------------

LLC_1951_2020_11 <- mav(TC_1951_2020$LLC,11)
plot(c(1951:2020),LLC_1951_2020_11,type="l")


#TC_1951_2020$LLC<-rowSums(TC_1951_2020[2:7])  
#TC_1951_2020$HLC<-rowSums(TC_1951_2020[8:18])  
#TC_1951_2020$BOB<-rowSums(TC_1951_2020[2:18])  

Epoch_1=TC_1951_2020$LLC[1:30]
Epoch_2=TC_1951_2020$LLC[31:60]

# Make individual data frames
#----------------------------------
Epoch_1 <- data.frame(Phase = "Epoch-1", value = TC_1951_2020$LLC[1:30])  
Epoch_2 <- data.frame(Phase = "Epoch-2", value = TC_1951_2020$LLC[31:60]) 
#----------------------------------

LLC<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                          DATA=c(mean(TC_1951_2020$LLC[1:30]),
                                 mean(TC_1951_2020$LLC[31:60])), 
                          SE=c(standard_error(TC_1951_2020$LLC[1:30]),
                               standard_error(TC_1951_2020$LLC[31:60])),
                          SD=c(sd(TC_1951_2020$LLC[1:30]),
                               sd(TC_1951_2020$LLC[31:60])))
rect1 <- data.frame (xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf)

test=t.test(TC_1951_2020$LLC[1:30],TC_1951_2020$LLC[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_llc=round(test$p.value,2)
p1<-ggplot(data=LLC, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  #scale_color_manual(values = c('ERSST' = 'royalblue4', 'HadISST' = 'orange')) +
  #labs(color = '')+
  #geom_point(color = "royalblue4", size = 2)+ 
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  #coord_cartesian(ylim = c(27, 29)) +
  geom_label(aes(x=2.11, y=1.5, label = paste0("p-value =  ",pvalue_llc)),size=3,color = "royalblue4", family="Helvetica") +
  geom_label(aes(x=1.5, y=2), label ="46 LLCs in Epoch-1 & 26 LLCs in Epoch-2",size=3,color = "royalblue4", family="Helvetica", fill="white") +
  
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("Number of LLC")))+
  ggtitle("BoB LLC (OND, 1951-2010)") +
  #xlab("Longitude") + ylab(expression(paste("SST (°C)")))+
  theme(legend.position = c(0.3, 0.9),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=10
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=10
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=10, angle=0))

p1
 
#ggsave("LLC_E1_E2.png", width = 15, height = 15, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 
 

########################HLC############
# HLC
########################HLC############


Epoch_1=TC_1951_2020$HLC[1:30]
Epoch_2=TC_1951_2020$HLC[31:60]

# Make individual data frames
#----------------------------------
Epoch_1 <- data.frame(Phase = "Epoch-1", value = TC_1951_2020$HLC[1:30])  
Epoch_2 <- data.frame(Phase = "Epoch-2", value = TC_1951_2020$HLC[31:60]) 
#----------------------------------


HLC<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                DATA=c(mean(TC_1951_2020$HLC[1:30]),
                       mean(TC_1951_2020$HLC[31:60])), 
                SE=c(standard_error(TC_1951_2020$HLC[1:30]),
                     standard_error(TC_1951_2020$HLC[31:60])),
                SD=c(sd(TC_1951_2020$HLC[1:30]),
                     sd(TC_1951_2020$HLC[31:60])))
rect1 <- data.frame (xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf)

test=t.test(TC_1951_2020$HLC[1:30],TC_1951_2020$HLC[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_hlc=round(test$p.value,2)
p2<-ggplot(data=HLC, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  #scale_color_manual(values = c('ERSST' = 'royalblue4', 'HadISST' = 'orange')) +
  #labs(color = '')+
  #geom_point(color = "royalblue4", size = 2)+ 
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  #coord_cartesian(ylim = c(27, 29)) +
  geom_label(aes(x=2.11, y=1.5, label = paste0("p-value =  ",pvalue_hlc)),size=3,color = "royalblue4", family="Helvetica") +
  geom_label(aes(x=1.5, y=2), label ="20 HLCs in Epoch-1 & 21 HLCs in Epoch-2",size=3,color = "royalblue4", family="Helvetica", fill="white") +  
  
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("Number of LLC")))+
  ggtitle("BoB HLC (OND, 1951-2010)") +
  #xlab("Longitude") + ylab(expression(paste("SST (°C)")))+
  theme(legend.position = c(0.3, 0.9),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=10
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=10
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=10
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=10, angle=0))

p2

#ggsave("HLC_E1_E2.png", width = 15, height = 15, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 
 
#----------------------------------------------------------------------------------------------------- 
#system("montage -mode concatenate -tile 2x LLC_E1_E2.png HLC_E1_E2.png BOB_TC_E1_E2.png")

#----------------------------------------------------------------------------------------------------


figure <- ggarrange(p1, p2, 
                    labels = c("a", "b"),
                    font.label = list(size = 12, family="Helvetica"),
                    ncol = 2, nrow = 1)



ggsave(figure, file="figure_S3.eps", device=cairo_ps, width = 185, height = 90, units = "mm", dpi = 1600, family="Helvetica")























