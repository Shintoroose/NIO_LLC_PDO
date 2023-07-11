setwd("/LLC/Manuscript_Figures/Figure_4")
library(ncdf4)
library(ggplot2) # package for plotting
library(png)
 
circle <- readPNG("/Documents/worldcoastalShape/circle-512-4.png")
#df2 <- data.frame(Category=rep(c("LLC", "LLC intensified into SCS","LLC intensified into SCS with in 10°N"), each=2),
#                  epochs=rep(c("Epoch-1", "Epoch-2"),3),
 #                 LLC=c(46,26,34,19,12,8))

df2 <- data.frame(Category=rep(c("LLC", "LLC intensified into SCS with in 11°N"), each=2),
                  epochs=rep(c("Epoch-1", "Epoch-2"),2),
                 LLC=c(46,26,12,8))
df2
head(df2)
#---------------------------------------------------------------------------------------------
rect1 <- data.frame (xmin=-Inf, xmax=1.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1.5, xmax=Inf, ymin=-Inf, ymax=Inf)
theme_set(theme_light() )
p2<-ggplot(data=df2, aes(x=epochs, y=LLC, fill=Category)) +
  geom_bar(stat="identity", position=position_dodge(), width = 1.2*0.5)+
  geom_vline(xintercept = 1.5, lty = 2, color = "royalblue4") +
  geom_text(aes(label=LLC), vjust=1.6, color="white",
            position = position_dodge(0.5),size=9,family="Times New Roman")+
  #scale_fill_brewer(palette="Paired")+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  #scale_fill_manual(values = c("yellow", "orange","red")) +
  scale_fill_manual(values = c("#0072B2","red")) +
  scale_x_discrete(name = "Epoch")+
  # limit the vertical space to 1 to 50, but keep the data
  coord_cartesian(ylim = c(2, 50)) +
  scale_y_continuous(name = "Number of LLC", breaks = seq(0,50,10), limits = c(0,50))+
  annotate("text", x = c(1.17,2.17), y = c(14,11), label = c("26%","31%"),family="Times New Roman",size=7) +
  #ggtitle("(b) Epochal Change in LLC (OND)") +
  ggtitle("(b)") +
  theme(legend.position = c(0.62, 0.826),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.title=element_text(size=18,family="Times New Roman"),
        legend.text=element_text(size=18,family="Times New Roman"),
        axis.text.x = element_text(family="Times New Roman"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Times New Roman"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                    ,family="Times New Roman"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                    ,family="Times New Roman"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Times New Roman",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=22, angle=0))

p2
#------------------------------------------------------------
ggsave("Figure4b.png", width = 20, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------- 

rect1 <- data.frame (xmin=-Inf, xmax=1980, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1980, xmax=Inf, ymin=-Inf, ymax=Inf)

#-------------------------------------------------------------------------------


system("cdo -selname,ETA_0 LatitudeChange_1951_2020.nc out.nc")
nc <- nc_open("out.nc")
eta_zero_loc<-ncvar_get(nc, "ETA_0")


a              <- eta_zero_loc[1:60]
#xx            <- seq(as.Date("1979/1/1"), by = "month", length.out = 468, format = "%B")
#xx             <- seq(as.Date('1951-11-1'),to=as.Date('2010-11-1'),by='1 year')

YEARS <- c(1951:2010) 

mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
eta_zero_loc_11YEAR<-mav(a,11)
#----------------------------------------------------------------------------------------------------- 
yat <- pretty(t(eta_zero_loc_11YEAR)*-1)
ylab <- paste(yat, "°S")
#----------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------
xx<-c(1951:2010)
#----------------------------------------------------------

av <- data.frame(AV=eta_zero_loc_11YEAR, Year=xx) 

change=av$AV[]

p<-ggplot(av, aes(Year, AV)) +
  geom_line(color = "royalblue4", size = 0.8) +
  #geom_text(x=1965, y=-1, label="Epoch-1") +
  geom_vline(xintercept = 1980, lty = 2, color = "royalblue4") +
  geom_hline(yintercept = min(eta_zero_loc_11YEAR,na.rm=TRUE), lty = 2, color = "black") +
  geom_hline(yintercept = max(eta_zero_loc_11YEAR,na.rm=TRUE), lty = 2, color = "black") +
  geom_segment(aes(x = 1960, y = min(eta_zero_loc_11YEAR,na.rm=TRUE), xend = 1960, yend =  max(eta_zero_loc_11YEAR,na.rm=TRUE)-0.06),
                 arrow = arrow(length = unit(0.2, "cm")), color = "red")+
  geom_segment(aes(x = 1991, y = max(eta_zero_loc_11YEAR,na.rm=TRUE), xend = 1991
                   , yend = min(eta_zero_loc_11YEAR,na.rm=TRUE)+0.06),
               arrow = arrow(length = unit(0.2, "cm")), color = "red")+
  #geom_smooth(method = "lm", color = "red", size = 1 ) +
  #scale_x_discrete(name = "")+
  #coord_cartesian(ylim = c(-0.9,-2.25,)) +
  #scale_y_continuous(name = "Latitude", breaks = seq(-2.25,-0.9,0.25), limits = c(-2.25,-0.9))+
  #scale_y_continuous(name = "Latitude", breaks = seq(-0.5,0.5,0.2), limits = c(-0.5,0.5))+
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  scale_y_continuous(name = "Latitude", breaks = seq(-0.5,0.5,0.2), limits = c(-0.5,0.5), labels=c("0.5°S","0.3°S","0.1°S","0.1°N","0.3°N","0.5°N"))+ 
  #ggtitle(expression(paste("(a) Latitudinal position of ", eta,"= 0 (OND), 80°E", sep=""))) +
  ggtitle("(a)") +
  theme(axis.text.x = element_text(family="Times New Roman"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Times New Roman"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                    ,family="Times New Roman"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                    ,family="Times New Roman"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Times New Roman",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=22, angle=0))
#----------------------------------------------------------------------------------------------------- 
p
#------------------------------------------------------------
ggsave("Figure4a.png", width = 20, height = 14, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 
 
#---------------------
#IMD data
storm_initial_loc<- read.csv("/Documents/IMD/NIO_storms_OND_1951_2010.csv", header = T,  sep = ",")

C_E1<-subset(storm_initial_loc, Year<=1980 & Lat>=5 & Lat<=20 & Lon>=50 & Lon<=100 & Max.Intensity!="D")
C_E2<-subset(storm_initial_loc, Year>=1981 & Lat>=5 & Lat<=20 & Lon>=50 & Lon<=100 & Max.Intensity!="D")

latE1 = C_E1$Lat
latE2 = C_E2$Lat
lonE1 = C_E1$Lon
lonE2 = C_E2$Lon

Epoch1 = rep("E1", each=length(lonE1))
Epoch2 = rep("E2", each=length(lonE2))

hist(c(C_E1$Lat,C_E2$Lat))


LLC_E1<-subset(C_E1, Lat>=5 & Lat<=11 & Lon>=83 & Lon<=95)
LLC_E2<-subset(C_E2, Lat>=5 & Lat<=11 & Lon>=83 & Lon<=95)
SCS_E1<-subset(LLC_E1, Lat<=11 & Max.Intensity=="SCS")
SCS_E2<-subset(LLC_E2, Lat<=11 & Max.Intensity=="SCS")

LLC_E2$Lon

LLC<-data.frame(length(LLC_E1$Year),length(LLC_E2$Year),length(SCS_E1$Year),length(SCS_E2$Year))

LLC_timeseries<-subset(storm_initial_loc, Year>=1951 & Year<=2010 & Lat>=5 & Lat<=11 & Lon>=83 & Lon<=95 & Max.Intensity!="D")
ts<-data.frame(table(LLC_timeseries$Year))
TS_LLC<-data.frame(Year=as.numeric(levels(ts$Var1)), Freq=ts$Freq)
#LLC_Frequency <- TS_LLC %>%
#  complete(Year = full_seq(TS_LLC$Year,1), fill = list(Value = 0))

LLC_Frequency <- TS_LLC %>%
  complete(Year = full_seq(c(1951,2010),1), fill = list(Value = 0))

library(tidyverse)
library(lubridate)
library(tidyr)
LLC_Frequency <- LLC_Frequency %>% mutate_all(funs(replace_na(.,0)))



#----------------------------------------------------------
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)} #moving average fuction
#----------------------------------------------------------

plot(LLC_Frequency$Year,mav(LLC_Frequency$Freq,11), type="l")

#IMD data
#---------------------
#---------------------
#----------------------------------------------------------------------------------------PDO
INDEX = read.csv("/Documents/indices/PDO/PDO.latest.txt", header = F)
INDEX = INDEX[26:143,]    
#write.table(INDEX,"/Users/shintoroose/Documents/indices/PDO/PDO.latest_new.txt")

YEAR=substr(INDEX$V1, 1,4)
JAN=substr(INDEX$V1, 8,13)
FEB=substr(INDEX$V1, 14,20)
MAR=substr(INDEX$V1, 21,27)
APR=substr(INDEX$V1, 28,34)
MAY=substr(INDEX$V1, 35,41)
JUN=substr(INDEX$V1, 42,48)
JUL=substr(INDEX$V1, 49,55)
AUG=substr(INDEX$V1, 56,62)
SEP=substr(INDEX$V1, 63,69)
OCT=substr(INDEX$V1, 70,76)
NOV=substr(INDEX$V1, 77,83)
DEC=substr(INDEX$V1, 84,90)

Year<-as.numeric(YEAR)
Jan<-as.numeric(JAN)
Feb<-as.numeric(FEB)
Mar<-as.numeric(MAR)
Apr<-as.numeric(APR)
May<-as.numeric(MAY)
Jun<-as.numeric(JUN)
Jul<-as.numeric(JUL)
Aug<-as.numeric(AUG)
Sep<-as.numeric(SEP)
Oct<-as.numeric(OCT)
Nov<-as.numeric(NOV)
Dec<-as.numeric(DEC)

PDO<-data.frame(cbind(Year,Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec))
PDO_1951_2010<-subset(PDO, Year>=1951 & Year<=2010)
#----------------------------------------------------------
Annual_PDO=rowMeans(PDO_1951_2010[2:13])
Annual_PDO_11 <- mav(Annual_PDO,11)

#----------------------------------------------------------
df<-data.frame(Year=PDO_1951_2010$Year,PDO=Annual_PDO_11, LLC=mav(LLC_Frequency$Freq,11))

cor.test(df$PDO,df$LLC)


rect1 <- data.frame (xmin=-Inf, xmax=1980, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1980, xmax=Inf, ymin=-Inf, ymax=Inf)

theme_set(theme_gray())

df$PDO_Phase <- ifelse(df$PDO < 0, "Negative","Positive")

#----------------------------------------------------------
ylim.PDO <- c(-1, 1)           # deltak ABS_VORT_CLIM
ylim.LLC <- c(0.5, 2.5)       # lkmean Eqwin

b <- diff(ylim.PDO)/diff(ylim.LLC)
a <- b*(ylim.PDO[1] - ylim.LLC[1])
#----------------------------------------------------------

rect1 <- data.frame (xmin=-Inf, xmax=1980, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1980, xmax=Inf, ymin=-Inf, ymax=Inf)

theme_set(theme_bw()) 

p1<-ggplot(df, aes(Year, PDO)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  geom_vline(xintercept = 1980, lty = 2, color = "royalblue4") +
  geom_label(aes(x = 2005, y = 0.8, label = "r=-0.5"),size=8,color="blue") +
  geom_line(size = 1.5, color = "red") +
  
  #stat_smooth(method=lm, formula = y ~ poly(x,7), level=0.95, se = F, color = "red",size = 1.2) +
  geom_line(aes(y = a + LLC*b), size = 1.5, color = "royalblue4") +
  #stat_smooth(aes(y = a + LLC*b),method=lm, formula = y ~ poly(x,7), level=0.95, se = F, color = "royalblue4",size = 1.2) +
  scale_y_continuous(name=expression(paste("Pacific Decadal Oscillation ")), sec.axis = sec_axis(~ (. - a)/b, name = expression(paste("Number of LLC")))) +
  scale_x_continuous(name = "Year", breaks = seq(1950,2010,10), limits = c(1951, 2010))+
  ggtitle("(c)") +
  theme(axis.line.y.right = element_line(color = "royalblue4"), 
        axis.ticks.y.right = element_line(color = "royalblue4"),
        axis.text.y.right = element_text(color = "royalblue4"), 
        axis.title.y.right = element_text(color = "royalblue4"),
        
        axis.line.y.left = element_line(color = "red"), 
        axis.ticks.y.left = element_line(color = "red"),
        axis.text.y.left = element_text(color = "red"), 
        axis.title.y.left = element_text(color = "red"),
              axis.text.x = element_text(family="Times New Roman"
                                         ,size=22
                                         #,face="bold"
                                         ,color="black"),
              axis.text.y = element_text(family="Times New Roman"
                                         ,size=22
                                         #,face="bold"
                                         ,color="black"),
              axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                          ,family="Times New Roman"
                                          ,size=22
                                          #,face="bold"
                                          ,color="black"),
              axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                          ,family="Times New Roman"
                                          ,size=22
                                          #,face="bold"
                                          ,color="black"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.title = element_text(color="black", 
                                        #       face="bold"
                                        #       family="Comic Sans MS",
                                        #       family="CM Roman",
                                        family="Times New Roman",
                                        #       family="Sans",
                                        #       family="Serif",
                                        size=22, angle=0))

p1



ggsave("Figure4c.png", width = 22, height = 14, units = "cm", dpi = 300)


z

system("montage -mode concatenate -tile 2x Figure4a.png Figure4b.png Figure4c.png ./PDO_SST_U_V_Regression/cropped2.png Figure4.png")
 
 