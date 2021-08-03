######## Plots for the AAAI paper - comparing performance metrics of all models 
install.packages(c("dplyr","ggplot2","tidyverse"))
library(dplyr)
library(ggplot2)
library(tidyverse)


GP_plot <- finalGP_365 %>% rename(ILI_Case_Counts=ILIIncidenceCount)
GP_xts <- xts(finalGP_365$ILIIncidenceCount,order.by=finalGP_365$ReportedDate)
GP_xts<-na_ma(GP_xts,k=1,weighting='simple')
GP_xts[.indexmon(GP_xts) %in% c(0:3,9:12)] <- 0 

GP_plot.m5 <- rollmean(GP_xts,k=5,align = "left",fill=NA)
GP_plot.m10 <- rollmean(GP_xts,k=10,align = "left",fill=NA)
GP_plot.m20 <- rollmean(GP_xts,k=20,align = "left",fill=NA)

GP_plot_m5<-data.frame(date1=date(GP_plot.m5), Five_day_RollingAverage=coredata(GP_plot.m5))
GP_plot_m10<-data.frame(date2=date(GP_plot.m10), Ten_day_RollingAverage=coredata(GP_plot.m10))
GP_plot_m20<-data.frame(date3=date(GP_plot.m20), Twenty_day_RollingAverage=coredata(GP_plot.m20))

combine_meancols <- cbind(GP_plot_m5,GP_plot_m10,GP_plot_m20)

colors<- c("5day RollingAverage"="green","10day RollingAverage"="blue","20day RollingAverage"="red")
plotGPtry<-ggplot(combine_meancols,aes(x=date1))+
  geom_line(aes(y=Five_day_RollingAverage,color="5day RollingAverage"))+
  geom_line(linetype="twodash", aes(y=Ten_day_RollingAverage,color="10day RollingAverage"))+
  geom_line(aes(y=Twenty_day_RollingAverage,color="20day RollingAverage"))+
  scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month")+
  scale_y_continuous(n.breaks = 10)+
  theme_classic(base_size = 20)+ 
  labs(x="Month/Year",y="ILI Case Counts",color="Legend")+scale_color_manual(values=colors)+
  theme(legend.position = "right")

plotGP<-ggplot(GP_plot_m,aes(date,ILI_case_counts))+
  geom_line(color="blue")+#geom_smooth(method = lm)+
  scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month")+
  #theme(text = element_text(size=22))+ 
  scale_y_continuous(n.breaks = 10)+
  theme_classic(base_size = 12)+
  xlab("Month/Year")+ylab("ILI Case Counts")
  
#########################################
#########GP RMSE Plot
AutoRegression <- c(4.19,4.56,4.64,4.72,4.75,4.76,4.79)
ARIMA <- c(4.21,4.51,4.65,4.77,4.81,4.81,4.87)
ETS <- c(4.83,4.94,4.94,4.87,4.88,5.04,5.31)
TBATS <- c(4.97,5.21,5.3,5.22,5.23,5.28,5.47)
RandomForest <- c(4.32,4.59,4.48,4.5,4.54,4.56,4.62)
REACH <- c(4.18,4.39,4.46,4.45,4.49,4.52,4.59)
Prophet <- c(5.91,5.93,5.98,6.05,6.1,6.12,6.1)

AR_sd <- c(0,0,0,0,0,0,0)
ARIMA_sd <- c(0,0,0,0,0,0,0)
ETS_sd <- c(0,0,0,0,0,0,0)
TBATS_sd <- c(0,0,0,0,0,0,0)
RF_sd <- c(0,0.06,0.05,0.01,0.02,0.03,0.02)
REACH_sd <- c(0.01,0.01,0.02,0.02,0.01,0.01,0.01)
Prophet_sd <- c(0,0,0,0,0,0,0)

GP_errors <- data.frame(AutoRegression,ETS,ARIMA,TBATS,RandomForest,REACH,Prophet)
GP_SD <- data.frame(AR_sd,ETS_sd,ARIMA_sd,TBATS_sd,RF_sd,REACH_sd,Prophet_sd)
rmse_long <- GP_errors %>% mutate(id = row_number()) %>% gather(key, value, -id)
sd_all <- GP_SD %>% mutate(id = row_number()) %>% gather(key, value, -id)
sd_all
rmse_long <- rmse_long %>% mutate(ymin = value-sd_all$value,ymax=value+sd_all$value) 
install.packages("scales")
library(scales)
plotrmse_long <- ggplot(rmse_long, aes(x = id, y = value,colour=key)) + 
  geom_line()+
  geom_ribbon(aes(ymin=rmse_long$ymin,ymax=rmse_long$ymax),fill="grey50",alpha=0.7,show.legend=FALSE,linetype=0)+
  theme(text = element_text(size=14),legend.position = c(0.5, 0.74),legend.direction = "horizontal",
        legend.title = element_blank(),legend.text = element_text(size=12))+guides(fill=guide_legend(ncol=2))+
  scale_y_continuous(n.breaks=12)+scale_x_continuous(n.breaks=7)+
  xlab("Forecast Horizon")+ylab("RMSE")

#####################################GP Weather RMSE
AR <- c(4.27,4.65,4.73,4.80,4.84,4.84,4.86)
ARIMA <- c(4.19,4.5,4.65,4.79,4.78,4.78,4.8)
RandomForest <- c(4.35,4.47,4.52,4.65,4.68,4.64,4.6)
REACH <- c(4.12,4.33,4.43,4.5,4.54,4.50,4.47)
Prophet <- c(6.01,6.01,6.09,6.17,6.23,6.27,6.38)

AR_sd <- c(0,0,0,0,0,0,0)
ARIMA_sd <- c(0,0,0,0,0,0,0)
RF_sd <- c(0.04,0.03,0.02,0.02,0.02,0.03,0.02)
REACH_sd <- c(0.02,0.02,0.03,0.03,0.03,0.03,0.02)
Prophet_sd <- c(0,0,0,0,0,0,0)

WGP_errors <- data.frame(AR,ARIMA,RandomForest,REACH,Prophet)
WGP_SD <- data.frame(AR_sd,ARIMA_sd,RF_sd,REACH_sd,Prophet_sd)
Wsd_all <- WGP_SD %>% mutate(id = row_number()) %>% gather(key, value, -id)
Wsd_all

Wrmse_long <- WGP_errors %>% mutate(id = row_number()) %>% gather(key, value, -id)
Wrmse_long <- Wrmse_long %>% mutate(ymin = value-Wsd_all$value,ymax=value+Wsd_all$value) 
Wrmse_long
plotWrmse_long <- ggplot(Wrmse_long, aes(x = id, y = value,colour=key)) + 
  geom_line()+
  geom_ribbon(aes(ymin=Wrmse_long$ymin,ymax=Wrmse_long$ymax),fill="grey50",alpha=0.7,show.legend=FALSE,linetype=0)+
  theme(text = element_text(size=14),legend.position = c(0.5, 0.70),legend.direction = "horizontal",
        legend.title = element_blank(),legend.text = element_text(size=12))+guides(fill=guide_legend(ncol=2))+
  scale_y_continuous(n.breaks=12)+scale_x_continuous(n.breaks=7)+
  xlab("Forecast Horizon")+ylab("RMSE")

###########################################################
library(gridExtra)
grid.arrange(plotrmse_long, plotWrmse_long, ncol=2)

########################Diebold-Mariano##############################
#WGP - ARIMA & REACH
dm.test(c(4.19,4.5,4.65,4.79,4.78,4.78,4.8),c(4.12,4.33,4.43,4.5,4.54,4.50,4.47))
#WGP - RF & REACH
dm.test(c(4.35,4.47,4.52,4.65,4.68,4.64,4.6),c(4.12,4.33,4.43,4.5,4.54,4.50,4.47))
#WGP - Prophet & REACH
dm.test(c(6.01,6.01,6.09,6.17,6.23,6.27,6.38),c(4.12,4.33,4.43,4.5,4.54,4.50,4.47))
#WGP - AR and REACH
dm.test(c(4.27,4.65,4.73,4.80,4.84,4.84,4.86),c(4.12,4.33,4.43,4.5,4.54,4.50,4.47))
##########
#GP - ARIMA & REACH
dm.test(c(4.21,4.51,4.65,4.77,4.81,4.81,4.87),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))
#GP - RF & REACH
dm.test(c(4.32,4.59,4.48,4.5,4.54,4.56,4.62),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))
#GP - Prophet & REACH
dm.test(c(5.91,5.93,5.98,6.05,6.1,6.12,6.1),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))
#GP - AR & REACH
dm.test(c(4.19,4.56,4.64,4.72,4.75,4.76,4.79),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))
#GP - ETS & REACH
dm.test(c(4.83,4.94,4.94,4.87,4.88,5.04,5.31),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))
#GP - TBATS & REACH
dm.test(c(4.97,5.21,5.3,5.22,5.23,5.28,5.47),c(4.18,4.39,4.46,4.45,4.49,4.52,4.59))

############################################################################
####################################GP Only MAE
ARIMA <- c(4.19,4.5,4.65,4.79,4.78,4.78,4.8)
RandomForest <- c(4.35,4.47,4.58,4.65,4.68,4.64,4.6)
REACH <- c(4.16,4.38,4.47,4.57,4.61,4.58,4.53)
Prophet <- c(4.78,4.81,4.84,4.89,4.94,4.96,4.93)