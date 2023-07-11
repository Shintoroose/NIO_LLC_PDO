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

#----------------------------------------------------------HADISST
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,sst /Volumes/CAOS_BKUP_1/DATASETS/HadISST/HadISST_sst.nc hadISST.nc")
nc <- nc_open("hadISST.nc")
hadisst<-ncvar_get(nc, "sst")
hadISST_1951_2010<-data.frame(Year=c(1951:2010),hadisst)
#----------------------------------------------------------ERSSTV5
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
 
               Epoch1=c(mean(ERSST_1951_2010$ersstv5[1:30]),
                        mean(hadISST_1951_2010$hadisst[1:30]))
                      
               Epoch2=c(mean(ERSST_1951_2010$ersstv5[31:60]),
                       mean(hadISST_1951_2010$hadisst[31:60]))
               
df <- data.frame(Parameters=rep(c("Epoch-1", "Epoch-2"), each=2),
                 Data=rep(c("ERSST", "hadISST"),2),
                 SST=c(Epoch1,Epoch2))

theme_set(theme_gray()) 
p1<-ggplot(data=df, aes(x=Parameters, y=SST, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.5)+
  scale_fill_brewer(palette="Paired")+
  scale_colour_brewer(palette="Paired")+
  #scale_color_manual(values=c("#999999", "#E69F00","#56B4E9","slateblue1","green", "red", "black")) +
  #geom_errorbar(aes(ymin=ST-ST_SE, ymax=ST+ST_SE), width=.01, position=position_dodge(.9), color="black") + #, color="black"
  #geom_errorbar(aes(xmin=Change_IH_AOD_G-SE, xmax=Change_IH_AOD_G+SE), width=.01, position=position_dodge(.9), color="black") + #, color="black"
  geom_hline(yintercept = 0, lty = 1, color = "royalblue4") +
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  coord_cartesian(ylim = c(27, 29)) +
  scale_x_discrete(name = "Epoch")+
  scale_y_continuous(name = expression(paste("SST (°C)")))+
  labs(color = '')+
  ggtitle("Sea Surface Temperature") +
  theme(legend.position = c(0.6, 0.92),
        legend.direction = "horizontal",
        legend.key.size = unit(0.22, "cm"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=14, angle=0))

p1
#ggsave("Figure2a.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 

#----------------------------------------------------------TCHP
#----------------------------------------------------------SODA
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,TCHP /Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/TCHP/TCHP_SODA_80E95E_5N11N.nc TCHP_SODA.nc")
nc <- nc_open("TCHP_SODA.nc")
tchp_soda<-ncvar_get(nc, "TCHP")
tchp_soda_1951_2010<-data.frame(Year=c(1951:2010),tchp=(tchp_soda*10^-7)) 
#----------------------------------------------------------
#----------------------------------------------------------EN4
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,TCHP /Volumes/CAOS_BKUP_1/DATASETS/EN4/EN4point2point1/TCHP/New_TCHP/1951_2020_TCHP.nc TCHP_EN4.nc")
nc <- nc_open("TCHP_EN4.nc")
tchp_en4<-ncvar_get(nc, "TCHP")
tchp_en4_1951_2010<-data.frame(Year=c(1951:2010),tchp=(tchp_en4*10^-7)) 
#----------------------------------------------------------

Epoch1=c(mean(tchp_en4_1951_2010$tchp[1:30]),
         mean(tchp_soda_1951_2010$tchp[1:30]))

Epoch2=c(mean(tchp_en4_1951_2010$tchp[31:60]),
         mean(tchp_soda_1951_2010$tchp[31:60]))

df <- data.frame(Parameters=rep(c("Epoch-1", "Epoch-2"), each=2),
                 Data=rep(c("EN4", "SODA"),2),
                 TCHP=c(Epoch1,Epoch2))

theme_set(theme_gray()) 
p2<-ggplot(data=df, aes(x=Parameters, y=TCHP, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.5)+
  scale_fill_brewer(palette="Paired")+
  scale_colour_brewer(palette="Paired")+
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  coord_cartesian(ylim = c(30, 72)) +
  scale_x_discrete(name = "Epoch")+
  scale_y_continuous(name = expression(paste("TCHP ( kJ cm"^"-2",")")))+
  ggtitle("Tropical Cyclone Heat Potential") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.6, 0.92),
        legend.direction = "horizontal",
        legend.key.size = unit(0.22, "cm"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=14, angle=0))
#----------------------------------------------------------
p2
#ggsave("Figure2b.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 




#----------------------------------------------------------Relative Humidity

#----------------------------------------------------------ERA5
#system("cdo -fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -sellevel,700 -selname,H /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/RH/RelHum_1951_2020.nc RH_ERA5.nc")
nc <- nc_open("RH_ERA5.nc")
era5_RH<-ncvar_get(nc, "H")
era5_RH_1951_2010<-data.frame(Year=c(1951:2010),era5_RH) 
#----------------------------------------------------------

#----------------------------------------------------------NCEP
#system("cdo selname,rhum /Volumes/CAOS_BKUP_1/DATASETS/NCEP_NCAR/rhum.mon.mean.nc out1.nc")
#system("cdo sellevel,700 out1.nc out2.nc")
#system("cdo selmon,10/12 out2.nc out3.nc")
#system("cdo yearmean out3.nc out4.nc")
#system("cdo sellonlatbox,83,95,5,11 out4.nc out5.nc")
#system("cdo fldmean out5.nc out6.nc")
#system("cdo selyear,1951/2010 out6.nc RH_NCEP.nc")

nc <- nc_open("RH_NCEP.nc")
NCEP_RH<-ncvar_get(nc, "rhum")
NCEP_RH_1951_2010<-data.frame(Year=c(1951:2010),NCEP_RH) 
#----------------------------------------------------------

Epoch1=c(mean(era5_RH_1951_2010$era5_RH[1:30]),
         mean(NCEP_RH_1951_2010$NCEP_RH[1:30]))

Epoch2=c(mean(era5_RH_1951_2010$era5_RH[31:60]),
         mean(NCEP_RH_1951_2010$NCEP_RH[31:60]))



df <- data.frame(Parameters=rep(c("Epoch-1", "Epoch-2"), each=2),
                 Data=rep(c("ERA5", "NCEP-NCAR"),2),
                 RH=c(Epoch1,Epoch2))

theme_set(theme_gray()) 
p3<-ggplot(data=df, aes(x=Parameters, y=RH, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.5)+
  scale_fill_brewer(palette="Paired")+
  scale_colour_brewer(palette="Paired")+
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  coord_cartesian(ylim = c(50,70)) +
  scale_x_discrete(name = "Epoch")+
  scale_y_continuous(name = expression(paste("RH(%)")))+
  ggtitle("Relative Humidity (700 hPa)") +
  theme(legend.position = c(0.6, 0.92),
        legend.direction = "horizontal",
        legend.key.size = unit(0.22, "cm"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=14, angle=0))
#----------------------------------------------------------
p3
#ggsave("Figure2c.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#-----------------------------------------------------------------------------------------------------



#----------------------------------------------------------Wind Shear

#----------------------------------------------------------ERA5
#system("cdo fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,VSHEAR /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/Vshear/VWS_1951_2020.nc VWS_ERA5.nc")
nc <- nc_open("VWS_ERA5.nc")
era5_VWS<-ncvar_get(nc, "VSHEAR")
era5_VWS_1951_2010<-data.frame(Year=c(1951:2010),era5_VWS) 
#----------------------------------------------------------ERA5
#system("cdo fldmean -sellonlatbox,83,95,5,11 -yearmean -selmon,10/12 -selyear,1951/2010 -selname,VSHEAR /Volumes/CAOS_BKUP_1/tcpyPI-master/All_Parameters/NCEP/Vshear/Vshear.nc VWS_NCEP.nc")
nc <- nc_open("VWS_NCEP.nc")
NCEP_VWS<-ncvar_get(nc, "VSHEAR")
NCEP_VWS_1951_2010<-data.frame(Year=c(1951:2010),NCEP_VWS) 
#----------------------------------------------------------

Epoch1=c(mean(era5_VWS_1951_2010$era5_VWS[1:30]),
         mean(NCEP_VWS_1951_2010$NCEP_VWS[1:30]))

Epoch2=c(mean(era5_VWS_1951_2010$era5_VWS[31:60]),
         mean(NCEP_VWS_1951_2010$NCEP_VWS[31:60]))

df <- data.frame(Parameters=rep(c("Epoch-1", "Epoch-2"), each=2),
                 Data=rep(c("ERA5", "NCEP-NCAR"),2),
                 VWS=c(Epoch1,Epoch2))

theme_set(theme_gray()) 
p4<-ggplot(data=df, aes(x=Parameters, y=VWS, fill=Data)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.5)+
  scale_fill_brewer(palette="Paired")+
  scale_colour_brewer(palette="Paired")+
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  coord_cartesian(ylim = c(9,12)) +
  scale_x_discrete(name = "Epoch")+
  scale_y_continuous(name = expression(paste("Wind Shear [200hPa-850hPa] (ms"^"-1"*")")))+
  ggtitle("Vertical wind shear") +
  #xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.6, 0.92),
        legend.direction = "horizontal",
        legend.key.size = unit(0.22, "cm"),
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 10),
        
        axis.text.x = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=14
                                   #,face="bold"
                                   ,color="royalblue4"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        plot.title = element_text(hjust = 0.5, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=14, angle=0))
#----------------------------------------------------------
p4
#ggsave("Figure2d.png", width = 14.5, height = 14, units = "cm", dpi = 300)
#-----------------------------------------------------------------------------------------------------

#system("montage -mode concatenate -tile 2x Figure2a.png Figure2b.png Figure2c.png Figure2d.png FigS6.png")

#----------------------------------------------------------------------------------------------------- 




figure         <- ggarrange(p1, p2, p3, p4,
                           labels = c("a", "b", "c", "d"),
                           font.label = list(size = 14, family="Helvetica"),
                           ncol = 2, nrow = 2) #, widths = c(1,1, 1.4)  , widths = c(1,1, 1.7) , widths = c(1.1,1.07, 1.35)


ggsave(figure, file="Figure_S6.eps", device=cairo_ps, width = 180, height = 200, units = "mm", dpi = 1600,family="Helvetica")
 
