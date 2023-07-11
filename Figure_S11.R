library(ncdf4)
library(ggplot2) # package for plotting
library(dplyr)
# ================================================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set working Directory
getwd()
# ================================================================================ 
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)} #moving average fuction
# Load function replace.outliers
replace.outliers <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}


#----------------------------------------------------------remove 1997
replace.extreme_elnino <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
#----------------------------------------------------------



system("cdo -selname,ETA_0 /Volumes/CAOS_BKUP_1/LLC/Manuscript_Figures/Era20C/LatitudeChange_1900_2010_850.nc out.nc")
nc <- nc_open("out.nc")
eta_zero_loc<-ncvar_get(nc, "ETA_0")



#quantile(a, probs=c(.05, .95), na.rm = T)
#quantile(a, probs=c(.05), na.rm = T)

YEARS <- c(1900:2010) 
eta_zero_loc_No_Outliers = replace.outliers(eta_zero_loc)
eta_zero_loc_No_Outliers_11YEAR<-mav(eta_zero_loc_No_Outliers,11) 
df1=data.frame(Year=YEARS, eta_zero_loc, eta_zero_loc_No_Outliers, eta_zero_loc_No_Outliers_11YEAR)
df1$Year <- as.numeric(as.character(df1$Year))



p<-ggplot() +
  geom_line(data=df1, aes(Year, eta_zero_loc, group = 1)) +
  geom_line(data=df1, aes(Year, eta_zero_loc_No_Outliers_11YEAR, group = 1)) +
  geom_hline(yintercept = quantile(eta_zero_loc, probs=c(.999), na.rm = T), lty = 2, color = "black") +
  geom_hline(yintercept = quantile(eta_zero_loc, probs=c(.0), na.rm = T), lty = 2, color = "black") +
  #ggtitle(expression(paste("(a) Latitudinal position of ", eta,"= 0 (OND), 80°E", sep=""))) +
  
  
  ggtitle("(a)") +
  theme(axis.text.x = element_text(family="Helvetica"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.text.y = element_text(family="Helvetica"
                                   ,size=22
                                   #,face="bold"
                                   ,color="black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=22
                                    #,face="bold"
                                    ,color="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=22, angle=0))
#----------------------------------------------------------------------------------------------------- 
p



#----------------------------------------------------------------------------------------PDO
INDEX = read.csv("/Volumes/CAOS_BKUP_1/ShintoRoose_Laptop/Documents/indices/PDO/PDO.latest.txt", header = F)
INDEX = INDEX[26:136,]    
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
#----------------------------------------------------------
Annual_PDO=rowMeans(PDO[2:13])
quantile(Annual_PDO, 0.95, na.rm=T)
quantile(Annual_PDO, 0.05, na.rm=T)
Annual_PDO_no_outliers <- replace.outliers(Annual_PDO)
Annual_PDO_11 <- mav(Annual_PDO,11)

#----------------------------------------------------------

cor.test(Annual_PDO_11,df1$eta_zero_loc_No_Outliers_11YEAR)
cor.test(Annual_PDO_11[52:111],df1$eta_zero_loc_No_Outliers_11YEAR[52:111])

round(-0.5387798,2)
round(-0.5913437,2)




df_PDO_eta_loc<-data.frame(Year=PDO$Year,PDO=Annual_PDO,PDO_11=Annual_PDO_11, eta_zero_loc_No_Outliers_11YEAR)


p_pdo<-ggplot() +
  geom_line(data=df_PDO_eta_loc, aes(Year, Annual_PDO, group = 1)) +
  geom_line(data=df_PDO_eta_loc, aes(Year, Annual_PDO_11, group = 1)) +
  geom_hline(yintercept = quantile(Annual_PDO, probs=c(.95), na.rm = T), lty = 2, color = "black") +
  geom_hline(yintercept = quantile(Annual_PDO, probs=c(.05), na.rm = T), lty = 2, color = "black")


#----------------------------------------------------------
#----------------------------------------------------------
range(df_PDO_eta_loc$eta_zero_loc_No_Outliers_11YEAR,na.rm=TRUE)
range(df_PDO_eta_loc$PDO_11,na.rm=TRUE)
#ylim.PDO <- c(-0.3929649, 0.2834469)           # deltak ABS_VORT_CLIM
#ylim.LOC <- c(min(df_PDO_eta_loc$eta_zero_loc_No_Outliers_11YEAR,na.rm=TRUE), max(df_PDO_eta_loc$eta_zero_loc_No_Outliers_11YEAR,na.rm=TRUE))       # lkmean Eqwin

ylim.PDO <- c(-0.3929649, 0.2834469)           # deltak ABS_VORT_CLIM
ylim.LOC <- c(-0.3929649, 0.2834469)       # lkmean Eqwin
 


b <- diff(ylim.PDO)/diff(ylim.LOC)
a <- b*(ylim.PDO[1] - ylim.LOC[1])
#----------------------------------------------------------


rect1 <- data.frame (xmin=-Inf, xmax=1980, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=1980, xmax=Inf, ymin=-Inf, ymax=Inf)

theme_set(theme_bw()) 

df_PDO_eta_loc$PDO_Phase <- ifelse(df_PDO_eta_loc$PDO_11 < 0, "Negative","Positive")

p1<-ggplot(df_PDO_eta_loc, aes(Year, PDO_11)) +
  #geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.1, inherit.aes = FALSE)+
  #geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE)+
  #geom_vline(xintercept = 1980, lty = 2, color = "royalblue4") +
  
  geom_bar(stat="identity", position=position_dodge(width=0.0), width =1,aes(fill = PDO_Phase))+
  scale_fill_manual(values=c(Positive="firebrick1",Negative="blue")) +
  #geom_label(aes(x = 2005, y = 0.8, label = "r=-0.1533196 "),size=8,color="blue") +
  #geom_line(size = 1.5, color = "red") +
  #stat_smooth(method=lm, formula = y ~ poly(x,7), level=0.95, se = F, color = "red",size = 1.2) +
  #geom_line(aes(y = a + UMAXLAT*b), size = 0.3, color = "royalblue4") +
  
  geom_line(aes(y = a + df_PDO_eta_loc$eta_zero_loc_No_Outliers_11YEAR*b), size = 0.5, color = "black") + #,linetype = "dotted" ,linetype="longdash"
  coord_cartesian(xlim = c(1909, 2001)) +
  
  #stat_smooth(aes(y = a + LLC*b),method=lm, formula = y ~ poly(x,7), level=0.95, se = F, color = "royalblue4",size = 1.2) +
  scale_y_continuous(name=expression(paste("Pacific Decadal Oscillation ")), sec.axis = sec_axis(~ (. - a)/b, name = expression(paste("Latitudinal position of ", eta,"= 0", sep="")))) +
  scale_x_continuous(name = "Year", breaks = seq(1900,2010,20), limits = c(1900, 2010))+
  ggtitle("cor 1900-2010=-0.54, cor 1951-2010=-0.59") +
  theme(legend.position='none',
        #legend.position = c(0.7, 0.15),
        legend.direction = "horizontal",
        axis.line.y.right = element_line(color = "black"), 
        axis.ticks.y.right = element_line(color = "black"),
        axis.text.y.right = element_text(color = "black"), 
        axis.title.y.right = element_text(color = "black"),
        
        axis.line.y.left = element_line(color = "black"), 
        axis.ticks.y.left = element_line(color = "black"),
        axis.text.y.left = element_text(color = "black"), 
        axis.title.y.left = element_text(color = "black"),
        
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)
                                    ,family="Helvetica"
                                    ,size=14
                                    #,face="bold"
                                    ,color="black"),
        #panel.grid.minor = element_blank(),
        #panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 1.0, color="black", 
                                  #       face="bold"
                                  #       family="Comic Sans MS",
                                  #       family="CM Roman",
                                  family="Helvetica",
                                  #       family="Sans",
                                  #       family="Serif",
                                  size=14, angle=0),
        axis.text.x = element_text(color="black",family="Helvetica", 
                                   size=14, angle=0),
        axis.text.y = element_text(#face="bold", 
          color="black",family="Helvetica", 
          size=14, angle=0)
  )

p1

#ggsave("PDO_Lat_of_etazero_80E.png", width = 20, height = 10, units = "cm", dpi = 300)

ggsave(p1, file="figure_S11.eps", device=cairo_ps, width = 180, height =100 , units = "mm", dpi = 1600, family="Helvetica")