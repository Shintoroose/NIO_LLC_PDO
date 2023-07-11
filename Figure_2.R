# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory
getwd()
# ================================================================================ 
library(ncdf4)
library(ggplot2) # package for plotting
library(png)
library("ggpubr")
library(grid)

mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)} #moving average fuction

standard_error <- function(x) sd(x) / sqrt(length(x)) # Create own function  (https://statisticsglobe.com/standard-error-in-r-example)

#----------------------------------------------------------
rect1 <- data.frame (xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf)
#
#system("cdo -selname,sst /Volumes/CAOS_BKUP_1/DATASETS/ERSSTV5/sst.mnmean.nc out.nc")
#system("cdo selyear,1951/2010 out.nc out1.nc")
#system("cdo selmon,10/12 out1.nc out2.nc")
#system("cdo yearmean out2.nc out3.nc")
#system("cdo sellonlatbox,83,95,5,11 out3.nc out4.nc")
#system("cdo fldmean out4.nc ERSSTV5.nc")
#system("rm out.nc out*.nc")
nc <- nc_open("ERSSTV5.nc")
ersstv5<-ncvar_get(nc, "sst")
ERSST_1951_2010<-data.frame(Year=c(1951:2010),ersstv5)
ERSST_1951_2010
plot(ERSST_1951_2010)

ERSST_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                          DATA=c(mean(ERSST_1951_2010$ersstv5[1:30]),
                                 mean(ERSST_1951_2010$ersstv5[31:60])), 
                          SE=c(standard_error(ERSST_1951_2010$ersstv5[1:30]),
                               standard_error(ERSST_1951_2010$ersstv5[31:60])),
                          SD=c(sd(ERSST_1951_2010$ersstv5[1:30]),
                               sd(ERSST_1951_2010$ersstv5[31:60])))
rect1 <- data.frame (xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf)

test=t.test(ERSST_1951_2010$ersstv5[1:30],ERSST_1951_2010$ersstv5[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_sst=round(test$p.value,2)
p1<-ggplot(data=ERSST_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  #scale_color_manual(values = c('ERSST' = 'royalblue4', 'HadISST' = 'orange')) +
  #labs(color = '')+
  #geom_point(color = "royalblue4", size = 2)+ 
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  coord_cartesian(ylim = c(27, 29)) +
  geom_label(aes(x=2.15, y=29, label = paste0("p-value =  ",pvalue_sst)),size=1.9,color = "royalblue4", family="Helvetica") +
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("SST (°C)")))+
  scale_x_discrete(name = "")+
  ggtitle("Sea Surface Temperature") +
  #xlab("Longitude") + ylab(expression(paste("SST (°C)")))+
  theme(legend.position = c(0.3, 0.9),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))

p1
#ggsave("Figure2a.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 


#----------------------------------------------------------TCHP
#----------------------------------------------------------EN4
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,TCHP /Volumes/CAOS_BKUP_1/DATASETS/EN4/EN4point2point1/TCHP/New_TCHP/1951_2020_TCHP.nc TCHP_EN4.nc")
nc <- nc_open("TCHP_EN4.nc")
tchp_en4<-ncvar_get(nc, "TCHP")
EN4_TCHP_1951_2010<-data.frame(Year=c(1951:2010),tchp_en4=tchp_en4*10^-7,Parameter=rep("TCHP",60))
EN4_TCHP_1951_2010
EN4_TCHP_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                             DATA=c(mean(EN4_TCHP_1951_2010$tchp_en4[1:30]),
                                    mean(EN4_TCHP_1951_2010$tchp_en4[31:60])),
                             SE=c(standard_error(EN4_TCHP_1951_2010$tchp_en4[1:30]),
                                  standard_error(EN4_TCHP_1951_2010$tchp_en4[31:60])),
                             SD=c(sd(EN4_TCHP_1951_2010$tchp_en4[1:30]),
                                  sd(EN4_TCHP_1951_2010$tchp_en4[31:60])))

test=t.test(EN4_TCHP_1951_2010$tchp_en4[1:30],EN4_TCHP_1951_2010$tchp_en4[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_tchp=round(test$p.value,2)

p2<-ggplot(data=EN4_TCHP_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  coord_cartesian(ylim = c(30, 72)) +
  geom_label(aes(x=2.16, y=72, label = paste0("p-value =  ",pvalue_tchp)),size=1.9,color = "royalblue4", family="Helvetica") +
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("TCHP ( kJ cm"^"-2",")")))+
  scale_x_discrete(name = "")+
  ggtitle("Tropical Cyclone Heat Potential") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))
#----------------------------------------------------------
p2
#ggsave("Figure2b.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 

#----------------------------------------------------------Relative Humidity
#----------------------------------------------------------ERA5
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/RH/RelHum_1951_2020.nc era5RH.nc")
nc <- nc_open("era5RH.nc")
era5RH<-ncvar_get(nc, "H")
ERA5RH_1951_2010<-data.frame(Year=c(1951:2010),era5RH)
ERA5RH_1951_2010
ERA5RH_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                           DATA=c(mean(ERA5RH_1951_2010$era5RH[1:30]),
                                  mean(ERA5RH_1951_2010$era5RH[31:60])),
                           SE=c(standard_error(ERA5RH_1951_2010$era5RH[1:30]),
                                standard_error(ERA5RH_1951_2010$era5RH[31:60])),
                           SD=c(sd(ERA5RH_1951_2010$era5RH[1:30]),
                                sd(ERA5RH_1951_2010$era5RH[31:60])))

test=t.test(ERA5RH_1951_2010$era5RH[1:30],ERA5RH_1951_2010$era5RH[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_rh=round(test$p.value,2)
p3<-ggplot(data=ERA5RH_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  coord_cartesian(ylim = c(60,65)) +
  geom_label(aes(x=2.08, y=65, label = paste0("p-value =  ",pvalue_rh)),size=1.9,color = "royalblue4", family="Helvetica") +
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("RH(%)")))+
  scale_x_discrete(name = "")+
  ggtitle("Relative Humidity (700 hPa)") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))
#----------------------------------------------------------
p3
#ggsave("Figure2c.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#-----------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------
#----------------------------------------------------------WindShear
#----------------------------------------------------------ERA5
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,VSHEAR /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/Vshear/VWS_1951_2020.nc era5_VWS.nc") #today
nc <- nc_open("era5_VWS.nc")
VWS_era5<-ncvar_get(nc, "VSHEAR")
VWS_ERA5_1951_2010<-data.frame(Year=c(1951:2010),VWS_era5)
VWS_ERA5_1951_2010
VWS_ERA5_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                             DATA=c(mean(VWS_ERA5_1951_2010$VWS_era5[1:30]),
                                    mean(VWS_ERA5_1951_2010$VWS_era5[31:60])),
                             SE=c(standard_error(VWS_ERA5_1951_2010$VWS_era5[1:30]),
                                  standard_error(VWS_ERA5_1951_2010$VWS_era5[31:60])),
                             SD=c(sd(VWS_ERA5_1951_2010$VWS_era5[1:30]),
                                  sd(VWS_ERA5_1951_2010$VWS_era5[31:60])))

test=t.test(VWS_ERA5_1951_2010$VWS_era5[1:30],VWS_ERA5_1951_2010$VWS_era5[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_vws=round(test$p.value,2)

p4<-ggplot(data=VWS_ERA5_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  coord_cartesian(ylim = c(9,12)) +
  geom_label(aes(x=2.11, y=12, label = paste0("p-value =  ",pvalue_vws)),size=1.9,color = "royalblue4", family="Helvetica") +
  #scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  scale_y_continuous(name = expression(paste("Wind Shear [200hPa-850hPa] (ms"^"-1"*")")))+
  scale_x_discrete(name = "")+
  ggtitle("Vertical wind shear") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))
#----------------------------------------------------------
p4
#ggsave("Figure2d.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#----------------------------------------------------------ERA5
#
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,ETA /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/Abs_Vort/AbsVort_1951_2020.nc era5_ETA.nc")
nc <- nc_open("era5_ETA.nc")
ETA_era5<-ncvar_get(nc, "ETA")
ETA_ERA5_1951_2010<-data.frame(Year=c(1951:2010),ETA=ETA_era5*10^5)

ETA_ERA5_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                             DATA=c(mean(ETA_ERA5_1951_2010$ETA[1:30]),
                                    mean(ETA_ERA5_1951_2010$ETA[31:60])),
                             SE=c(standard_error(ETA_ERA5_1951_2010$ETA[1:30]),
                                  standard_error(ETA_ERA5_1951_2010$ETA[31:60])),
                             SD=c(sd(ETA_ERA5_1951_2010$ETA[1:30]),
                                  sd(ETA_ERA5_1951_2010$ETA[31:60])))



test=t.test(ETA_ERA5_1951_2010$ETA[1:30],ETA_ERA5_1951_2010$ETA[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_eta=round(test$p.value,2)
p5<-ggplot(data=ETA_ERA5_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  coord_cartesian(ylim = c( 2.6,2.91)) +
  geom_label(aes(x=2.08, y=2.91, label = paste0("p-value =  ",pvalue_eta)),size=1.9,color = "royalblue4", family="Helvetica") +
  scale_y_continuous(name = expression(paste("Abs. Vorticity ( x 10"^"-5","sec"^"-1",")")))+
  scale_x_discrete(name = "")+
  ggtitle("Absolute vorticity") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))
#----------------------------------------------------------
p5
#ggsave("Figure2e.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 



#----------------------------------------------------------ERA5

#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selyear,1951/2010 -selmon,10/12 -selname,GPI /Volumes/CAOS_BKUP_1/tcpyPI-master/GPI/GPI_1951_2020.nc era5_GPI.nc")
nc <- nc_open("era5_GPI.nc")
GPI_era5<-ncvar_get(nc, "GPI")
GPI_ERA5_1951_2010<-data.frame(Year=c(1951:2010),GPI=GPI_era5)

GPI_ERA5_epochal<-data.frame(EPOCH=c("Epoch-1","Epoch-2"),
                             DATA=c(mean(GPI_ERA5_1951_2010$GPI[1:30],na.rm = T),
                                    mean(GPI_ERA5_1951_2010$GPI[31:60],na.rm = T)),
                             SE=c(standard_error(GPI_ERA5_1951_2010$GPI[1:30]),
                                  standard_error(GPI_ERA5_1951_2010$GPI[31:60])),
                             SD=c(sd(GPI_ERA5_1951_2010$GPI[1:30]),
                                  sd(GPI_ERA5_1951_2010$GPI[31:60])))


test=t.test(GPI_ERA5_1951_2010$GPI[1:30],GPI_ERA5_1951_2010$GPI[31:60],
            var.equal = TRUE)
test
test$p.value
pvalue_gpi=round(test$p.value,2)
p6<-ggplot(data=GPI_ERA5_epochal, aes(x=EPOCH, y=DATA)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.3, colour = "black")+
  geom_errorbar(aes(ymin=DATA-SE, ymax=DATA+SE), width=.15, position=position_dodge(.9), color="red", size=0.2) +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4", size=0.3) +
  geom_label(aes(x=2.08, y=6.5, label = paste0("p-value =  ",pvalue_gpi)),size=1.9,color = "royalblue4", family="Helvetica") +
  coord_cartesian(ylim = c( 5,6.5)) +
  scale_y_continuous(name = "GPI")+
  scale_x_discrete(name = "")+
  ggtitle("Genesis potential index") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.3, 0.85),
        legend.direction = "horizontal",
        
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=8
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=8
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=8, angle=0))
#----------------------------------------------------------
p6

#----------------------------------------------------------

#ggsave("Figure2f.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------


#----------------------------------------------------------

#ggsave("GPI.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 

#system("montage -mode concatenate -tile 3x Figure2a.png Figure2b.png Figure2c.png Figure2d.png Figure2e.png Figure2f.png Figure2.png")

#----------------------------------------------------------------------------------------------------- 
 




figure <- ggarrange(p1, p2, p3, p4, p5, p6,
                    labels = c("a", "b", "c", "d", "e", "f"),
                    font.label = list(size = 10, family="Helvetica"),
                    ncol = 3, nrow = 2)



ggsave(figure, file="figure_2.eps", device=cairo_ps, width = 185, height = 122, units = "mm", dpi = 1600, family="Helvetica")



