# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory

getwd()
# ================================================================================ 

#Shinto Roose, Cochin University of Science and Technology
library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library("ggpubr")
library(rgeos)
library(leaflet)
library(broom) # if you plot with ggplot and need to turn sp data into dataframes
options(stringsAsFactors = FALSE)
# download the data 
#download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
#      destfile = 'coastlines.zip')
# unzip the file
#unzip(zipfile = "coastlines.zip", 
#      exdir = 'ne-coastlines-10m')
#coastlines <- readOGR("/Documents/worldcoastalShape/ne_10m_coastline/ne_10m_coastline.shp")


library(raster)
library(png)



India <- readOGR("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/IndiaShape/india_bndry.shp")
bluemarble <- readPNG("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/Blue_Marble/world.200409.3x5400x2700.png") #out1.png
s <- readOGR("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/worldcoastalShape/ne_10m_coastline/ne_10m_coastline.shp")
# ================================================================================INPUT

sub <- crop(s, extent(45, 105, 0, 30))
system("rm croppedMap.shp")
shapefile(sub, 'croppedMap.shp')
coastlines <- readOGR('croppedMap.shp')
system("ls")
# view spatial attributes
class(coastlines)
extent(coastlines)
crs(coastlines)
#plot(coastlines, 
#     main = "Global Coastlines")
# simplify with a lower tolerance value (keeping more detail)
coastlines_sim2 <- gSimplify(coastlines, 
                             tol = 0.01, 
                             topologyPreserve = TRUE)
plot(coastlines_sim2, 
     main = "")
# turn the data into a spatial data frame 
coastlines_sim2_df <- SpatialLinesDataFrame(coastlines_sim2,
                                            coastlines@data) 




#tidy(coastlines_sim2_df)
d=data.frame(x1=45,x2=105,y1=0,y2=30)
LL=data.frame(x1=50,x2=100,y1=5,y2=10)

LatBands=c("0°N","10°N","20°N","30°N")
LonBands=c("50°E","60°E","70°E","80°E","90°E","100°E")

# plot the data 
basemap<-ggplot() +
  annotation_raster(bluemarble, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_path(data = coastlines_sim2_df, aes(x = long, y = lat, group = group), size=0.09) + #alpha = 0.02
  coord_fixed() +  
  geom_hline(yintercept = 11, lty = 2, color = "yellow") +
  geom_hline(yintercept = 5, lty = 2, color = "yellow") +
  scale_x_continuous(breaks = seq(50,100,10), limits = c(50, 100), labels=LonBands)+
  scale_y_continuous(breaks = seq(0,30,10), limits = c(1, 29), labels=LatBands)+
  #geom_point(aes(x = 2, y = 23), color = "white", size =4) +
  ggtitle("") +
  xlab("Longitude") + ylab("Latitude")+
  theme(axis.text.x = element_text(family="Helvetica"
                                   ,size=7.5
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=7.5
                                   #,face="bold"
                                   ,color="black"),
        axis.title.y = element_text(family="Helvetica"
                                    ,size=7.5
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(family="Helvetica"
                                    ,size=7.5
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
#basemap
#ggsave("basemap.png", width = 15, height = 10, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------
system("rm croppedMap*")
p1<-basemap
p2<-basemap
#----------------------------------------------------------------------------------------
path = "/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/IMD/50E100E_5N11N/83E_95E_5N_11N/epoch_1"
file.names <- dir(path, pattern =".csv")
ff<-paste0(path,"/",file.names)
x <- c(1:length(file.names))
length(file.names)
for (i in x) {
  #print(i)
  #print(ff[(i)])
  print(file.names[(i)])
  #----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------
  dnam<- read.csv(ff[(i)], header = T,  sep = ",")
  lat = (as.numeric(dnam$LATITUDE))
  lon = (as.numeric(dnam$LONGITUDE))
  df <- data.frame(latitude=lat,longitude=lon)
  # remove variable information
  #df = df[-1,]
  #----------------------------------------------------------------------------------------
  p1<-p1 + geom_path(
    data = df, aes(x = longitude, y = latitude), size = 0.25,  lty = 1, color = "mistyrose") + #alpha = 0.8,
    ggtitle("Epoch-1") +
    geom_point(data = df, aes(x = longitude[1], y = latitude[1]), size = 0.25,color="red") #, alpha = 0.8
}

#----------------------------------------------------------------------------------------
path = "/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/IMD/50E100E_5N11N/83E_95E_5N_11N/epoch_1/10_11N"
file.names <- dir(path, pattern =".csv")
ff<-paste0(path,"/",file.names)
x <- c(1:length(file.names))
length(file.names)
for (i in x) {
  #print(i)
  #print(ff[(i)])
  print(file.names[(i)])
  #----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------
  dnam<- read.csv(ff[(i)], header = T,  sep = ",")
  lat = (as.numeric(dnam$LATITUDE))
  lon = (as.numeric(dnam$LONGITUDE))
  df <- data.frame(latitude=lat,longitude=lon)
  # remove variable information
  #df = df[-1,]
  #----------------------------------------------------------------------------------------
  p1<-p1 + geom_path(
    data = df, aes(x = longitude, y = latitude), size = 0.25, lty = 1, color = "mistyrose") + #, alpha = 0.8
    ggtitle("Epoch-1") +
    geom_point(data = df, aes(x = longitude[1], y = latitude[1]), size = 0.25, color="red")  #alpha = 0.8,

}
#p
EPOCH_1<-p1
EPOCH_1<-EPOCH_1+geom_point(data = df, aes(x = 77.5, y = 17.5), size = 7., color="white") +
  annotate("text", x = 77.5, y = 17.5, label = "46", family="Helvetica"
           ,size=3
           ,color="red")+
  #geom_rect(data = df, aes(xmin = 50.2, xmax = 69.8, ymin = 26, ymax = 28.5), fill = "white", alpha = 0.06)+ #
  #annotate("text", x = 60, y = 27.5, label = "Cyclone eAtlas - IMD", family="Helvetica",size=2.5,color="black")+
  theme(#legend.position="bottom",
    #legend.position=c(.85,.60),
    axis.text.x = element_text(family="Helvetica"
                               ,size=7
                               #,face="bold"
                               ,color="black"),
    axis.text.y = element_text(family="Helvetica"
                               ,size=7
                               #,face="bold"
                               ,color="black"),
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
#ggsave("EPOCH1.png", width = 25, height = 10, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------- 
# remove variable information
#df = df[-1,]


p2<-basemap
#----------------------------------------------------------------------------------------
path = "/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/IMD/50E100E_5N11N/83E_95E_5N_11N/epoch_2"
file.names <- dir(path, pattern =".csv")
ff<-paste0(path,"/",file.names)
x <- c(1:length(file.names))
length(file.names)
for (i in x) {
  #print(i)
  #print(ff[(i)])
  print(file.names[(i)])
  #----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------
  dnam<- read.csv(ff[(i)], header = T,  sep = ",")
  lat = (as.numeric(dnam$LATITUDE))
  lon = (as.numeric(dnam$LONGITUDE))
  df <- data.frame(latitude=lat,longitude=lon)
  # remove variable information
  #df = df[-1,]
  #----------------------------------------------------------------------------------------
  p2<-p2 + geom_path(
    data = df, aes(x = longitude, y = latitude), size = 0.25,  lty = 1, color = "mistyrose") + #alpha = 0.8,
    ggtitle("Epoch-2") +
    geom_point(data = df, aes(x = longitude[1], y = latitude[1]), size = 0.25,color="red") #, alpha = 0.8
}


#----------------------------------------------------------------------------------------
path = "/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/IMD/50E100E_5N11N/83E_95E_5N_11N/epoch_2/10_11N"
file.names <- dir(path, pattern =".csv")
ff<-paste0(path,"/",file.names)
x <- c(1:length(file.names))
length(file.names)
for (i in x) {
  #print(i)
  #print(ff[(i)])
  print(file.names[(i)])
  #----------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------
  dnam<- read.csv(ff[(i)], header = T,  sep = ",")
  lat = (as.numeric(dnam$LATITUDE))
  lon = (as.numeric(dnam$LONGITUDE))
  df <- data.frame(latitude=lat,longitude=lon)
  # remove variable information
  #df = df[-1,]
  #----------------------------------------------------------------------------------------
  p2<-p2 + geom_path(
    data = df, aes(x = longitude, y = latitude), size = 0.25,  lty = 1, color = "mistyrose") + #alpha = 0.8,
    ggtitle("Epoch-2") +
    geom_point(data = df, aes(x = longitude[1], y = latitude[1]), size = 0.25,color="red") #, alpha = 0.8
    #geom_point(data = df, aes(x = 77.5, y = 17.5), size = 7, color="white") +
    #annotate("text", x = 77.5, y = 17.5, label = "26", family="Helvetica"
    #         ,size=3
    #         ,color="red")+
    #geom_rect(data = df, aes(xmin = 50.2, xmax = 69.8, ymin = 26, ymax = 28.5), fill = "white", alpha = 0.04)+ #
    #annotate("text", x = 60, y = 27.5, label = "Cyclone eAtlas - IMD", family="Helvetica"
    #         ,size=2.5
    #         ,color="black")+

  
}
#p
EPOCH_20<-p2
EPOCH_2<-EPOCH_20+geom_point(data = df, aes(x = 77.5, y = 17.5), size = 7, color="white") +
  annotate("text", x = 77.5, y = 17.5, label = "26", family="Helvetica"
           ,size=3
           ,color="red")+
  #geom_rect(data = df, aes(xmin = 50.2, xmax = 69.8, ymin = 26, ymax = 28.5), fill = "white", alpha = 0.06)+ #
  #annotate("text", x = 60, y = 27.5, label = "Cyclone eAtlas - IMD", family="Helvetica",size=2.5,color="black")+    
  theme(#legend.position="bottom",
             #legend.position=c(.85,.60),
             axis.text.x = element_text(family="Helvetica"
                                        ,size=7
                                        #,face="bold"
                                        ,color="black"),
             axis.text.y = element_text(family="Helvetica"
                                        ,size=7
                                        #,face="bold"
                                        ,color="black"),
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
#ggsave("EPOCH2.png", width = 25, height = 10, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 

figure_top <- ggarrange(EPOCH_1, EPOCH_2,
                        labels = c("a", "b"),
                        font.label = list(size = 10, family="Helvetica"),
                        ncol = 2, nrow = 1)



#-------Figure1C
#---------------------------------------------------------------------------------BoB TC 83-95E
BOB_TC <- readLines("/Volumes/CAOS_BKUP_1/LLC/New_ANALYSIS_2022_JUNE/IMD_BOB_TC/BoB_TropicalCyclone_OND_1951_2020.csv") #(RSMC-IMD eAtlas)
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
df_c <- data.frame(Category=c("1950's", "1960's","1970's","1980's", "1990's","2000's"),
                   LLC=c(sum(TC_1951_2020$LLC[1:10]),
                         sum(TC_1951_2020$LLC[11:20]),
                         sum(TC_1951_2020$LLC[21:30]),
                         sum(TC_1951_2020$LLC[31:40]),
                         sum(TC_1951_2020$LLC[41:50]),
                         sum(TC_1951_2020$LLC[51:60])))

rect1 <- data.frame (xmin=-Inf, xmax=3.5, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=3.5, xmax=Inf, ymin=-Inf, ymax=Inf)



df_c <- data.frame(Category=c("1950's", "1960's","1970's","1980's", "1990's","2000's"),
                   LLC=c(8,19,19,10,8,8))
head(df_c)
df_c
theme_set(theme_grey()) 
p3<-ggplot(data=df_c, aes(x=Category, y=LLC)) +
  #geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE) +
  #geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1.2*0.5)+
  geom_text(aes(label=LLC), vjust=1.6, color="white", position = position_dodge(0.5),size=2.5,family="Helvetica")+
  geom_vline(xintercept = 3.5, lty = 2, color = "royalblue4") +
  scale_x_discrete(name = "Decade")+
  coord_cartesian(ylim = c(2, 23.2),xlim = c(1, 6)) +
  scale_y_continuous(name = "Number of LLC", breaks = seq(0,30,5), limits = c(0,25))+
  geom_point(aes(x = 2, y = 23), color = "white", size =5.5) +
  annotate("text", x = 2, y = 23, label = "46", family="Helvetica",size=3,color="red")+
  geom_segment(aes(x = 0.5, y = 21.5, xend = 3.5, yend =21.5),arrow = arrow(length = unit(0.11, "cm")), color = "red",size=0.2)+
  geom_segment(aes(x = 3.5, y = 21.5, xend = 0.5, yend =21.5),arrow = arrow(length = unit(0.11, "cm")), color = "red",size=0.2)+
  annotate("text", x = 2, y = 20.5, label = "Epoch-1", family="Helvetica",size=2.5,color="black")+
  geom_point(aes(x = 5, y = 13.5), color = "white", size =5.5) +
  annotate("text", x = 5, y = 13.5, label = "26", family="Helvetica",size=3,color="red")+
  geom_segment(aes(x = 3.5, y = 12, xend = 6.5, yend =12),arrow = arrow(length = unit(0.11, "cm")), color = "red",size=0.2)+
  geom_segment(aes(x = 6.5, y = 12, xend = 3.5, yend =12),arrow = arrow(length = unit(0.11, "cm")), color = "red",size=0.2)+
  annotate("text", x = 5, y = 11, label = "Epoch-2", family="Helvetica",size=2.5,color="black")+
  ggtitle("Decadal Changes in LLC") +
  theme(#legend.position="bottom",
    #legend.position=c(.85,.60),
    axis.text.x = element_text(family="Helvetica"
                               ,size=6.5
                               #,face="bold"
                               ,color="black", angle=0),
    axis.text.y = element_text(family="Helvetica"
                               ,size=8
                               #,face="bold"
                               ,color="black"),
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
p3
#ggsave("Figure1c.png", width = 10, height = 10, units = "cm", dpi = 300)
#----------------------------------------------------------------------------------------------------- 



#-------Figure1D
#-------

#-------NIO
#Cyclones<-c(21,33,43,24,15,9,8,4) #1951-2010 (RSMC-IMD eAtlas)
#  epoch1<-c(14,19,29,10,3,3,6,1)  #1951-1980 (RSMC-IMD eAtlas)
#  epoch2<-c(7,14,14,14,12,6,2,3)  #1981-2010
#  EpochalDiff=epoch2-epoch1

#--------80E-100E   
epoch1<-c(11,18,21,10,3,2,5,0)  #1951-1980 (RSMC-IMD eAtlas)
epoch2<-c(4,13,12,10,9,4,1,3)   #1981-2010 (RSMC-IMD eAtlas)
Cyclones=epoch1+epoch2  #1951-2010
EpochalDiff=epoch2-epoch1
#--------
LatBands=c("5-7°N","7-9°N","9-11°N","11-13°N","13-15°N","15-17°N","17-19°N","19-21°N")

df_d<-data.frame(LatBand=c(1:8), Cyclones=Cyclones,epoch1,epoch2, EpochalDiff)


df_d$Phase <- ifelse(df_d$EpochalDiff < 0, "Negative","Positive")

theme_set(theme_grey()) 
pdiffLat<-ggplot(df_d, aes(x=LatBand, y=EpochalDiff)) +
  #geom_line(color = "blue",size = 2)+
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1.5*0.5,aes(fill = Phase), show.legend = FALSE) + # ,alpha = 0.8
  scale_fill_manual(values=c(Positive="steelblue",Negative="firebrick1")) +
  scale_y_continuous(name="Number of Cyclones", breaks = seq(-10,10,4), limits = c(-10,10))+
  coord_flip()+
  #scale_x_reverse()+
  scale_x_discrete(name = "Latitudinal band",limits=LatBands) + 
  geom_hline(yintercept = 0, lty = 2, color = "royalblue4") +
  ggtitle("Epochal change in BoB cyclones") +
  theme(#legend.position="bottom",
    #legend.position=c(.85,.60),
    axis.text.x = element_text(family="Helvetica"
                               ,size=7
                               #,face="bold"
                               ,color="black"),
    axis.text.y = element_text(family="Helvetica"
                               ,size=5.2
                               #,face="bold"
                               ,color="black"),
    axis.title.y = element_text(family="Helvetica"
                                ,size=8
                                #,face="bold"
                                ,color="black"),
    axis.title.x = element_text(family="Helvetica"
                                ,size=8
                                #,face="bold"
                                ,color="black"),
    plot.title = element_text(hjust = 0.5,  color="black", 
                              #       face="bold"
                              #       family="Comic Sans MS",
                              #       family="CM Roman",
                              family="Helvetica",
                              #       family="Sans",
                              #       family="Serif",
                              size=7.2, angle=0))
pdiffLat
#ggsave("Figure1d.png", width = 12, height = 10, units = "cm", dpi = 300)

#---Figure1E

epoch1<-c(1,2,7,1,6,19,23,3) #1951-1980
epoch2<-c(2,2,1,1,3,14,11,2) #1981-2010
Cyclones=epoch1+epoch2       #1951-2010

Cyclones

EpochalDiff=epoch2-epoch1

EpochalDiff
LonBands=c("60-65","65-70","70-75","75-80","80-85","85-90","90-95","95-100")

df_e<-data.frame(LonBand=c(1:8), Cyclones=Cyclones,epoch1,epoch2, EpochalDiff)
df_e$Phase <- ifelse(df_e$EpochalDiff < 0, "Negative","Positive")

theme_set(theme_grey()) 
pdiffLon<-ggplot(df_e, aes(x=LonBand, y=EpochalDiff)) +
  #geom_line(color = "blue",size = 2)+
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1.5*0.5,aes(fill = Phase), show.legend = FALSE) + #,alpha = 0.8
  scale_fill_manual(values=c(Positive="steelblue",Negative="firebrick1")) +
  scale_y_continuous(name="Number of LLC", breaks = seq(-12,12,2), limits = c(-12,2))+
  #coord_flip()+
  #scale_x_reverse()+
  scale_x_discrete(name = "Longitudinal band (°E)",limits=LonBands) + 
  geom_hline(yintercept = 0, lty = 2, color = "royalblue4") +
  ggtitle("Epochal change in LLC against longitudes") +
  theme(#legend.position="bottom",
    #legend.position=c(.85,.60),
    axis.text.x = element_text(family="Helvetica"
                               ,size=6.5
                               #,face="bold"
                               ,color="black"), #, angle=25
    axis.text.y = element_text(family="Helvetica"
                               ,size=7
                               #,face="bold"
                               ,color="black"),
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
pdiffLon
#ggsave("Figure1e.png", width = 17, height = 10, units = "cm", dpi = 300)


figure_bottom <- ggarrange(p3, pdiffLat, pdiffLon,
                           labels = c("c", "d", "e"),
                           font.label = list(size = 10, family="Helvetica"),
                           ncol = 3, nrow = 1, widths = c(1.15,0.98, 1.45)) #, widths = c(1,1, 1.4)  , widths = c(1,1, 1.7) , widths = c(1.1,1.07, 1.35)



figure <- ggarrange(figure_top, figure_bottom,
                    ncol = 1, nrow = 2)



ggsave(figure, file="figure_1.eps", device=cairo_ps, width = 180, height = 130, units = "mm", dpi = 300,family="Helvetica")















