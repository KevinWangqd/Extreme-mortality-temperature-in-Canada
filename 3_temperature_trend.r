### Temperature Trend
library(pacman)
p_load(dplyr, ggplot2, ggpubr, gridExtra, tseries)

temp10 <- read.csv("T10.csv")
temp90 <- read.csv("T90.csv")
months <- seq.Date(from = as.Date("2000/01/01",format = "%Y/%m/%d"), 
                   by = "month", length.out = 252)
time <- as.character(months)
temp10$X <- months
temp10$Year <- temp10$X
temp90$X <- months
temp90$Year <- temp90$X

#p8
T90_CAR <- ggplot(temp90) + geom_line(data = temp90, aes(x = Year, y = CAR, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= CAR, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle("(a) CAR") +
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T90_CAR

#pp9
T90_NEA <- ggplot(temp90) + geom_line(data = temp90, aes(x = Year, y = NEA, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NEA, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(b) NEA')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T90_NEA

#p10
T90_NEF <- ggplot(temp90) + geom_line(data = temp90, aes(x = Year, y = NEF, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NEF, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(c) NEF')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T90_NEF

#p11
T90_NPL <- ggplot(temp90) + geom_line(data = temp90, aes(x = Year, y = NPL, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NPL, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(d) NPL')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T90_NPL

#p12
T90_NWP <- ggplot(temp90) + geom_line(data = temp90, aes(x = Year, y = NWP, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NWP, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(e) NWP')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T90_NWP

#p2
T10_CAR <- ggplot(temp10) + geom_line(data = temp10, aes(x = Year, y = CAR, group = 1),linewidth = 0.4, col = '#00BFC4') +
  geom_smooth(aes(x = Year, y= CAR, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(a) CAR')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T10_CAR

#p3
T10_NEA <- ggplot(temp10) + geom_line(data = temp10, aes(x = Year, y = NEA, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NEA, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(b) NEA')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T10_NEA

#p4
T10_NEF <- ggplot(temp10) + geom_line(data = temp10, aes(x = Year, y = NEF, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NEF, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(c) NEF')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10))
T10_NEF

#p5
T10_NPL <- ggplot(temp10) + geom_line(data = temp10, aes(x = Year, y = NPL, group = 1),linewidth = 0.4, col = '#00BFC4') +
  geom_smooth(aes(x = Year, y= NPL, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(d) NPL')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T10_NPL

#p6
T10_NWP <- ggplot(temp10) + geom_line(data = temp10, aes(x = Year, y = NWP, group = 1),linewidth = 0.4, col = '#00BFC4') + 
  geom_smooth(aes(x = Year, y= NWP, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(e) NWP')+
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10)) 
T10_NWP

grid.arrange(T90_CAR, T10_CAR, T90_NEA, T10_NEA, T90_NEF, T10_NEF, 
             T90_NPL, T10_NPL, T90_NWP, T10_NWP, nrow = 5, ncol = 2)


######################################################################
## T90的趋势，五个地区画在一张图上

trend_T90 <- ggplot(temp90, aes(x = Year, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE) + # Disable confidence interval
  xlab("Year") + ylab("T90") + ggtitle("") +
  scale_y_continuous(breaks=seq(0, 50, 5))+
  coord_cartesian(ylim = c(0, 45))+
  theme_minimal() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) 
trend_T90

## T10的趋势，五个地区画在一张图上
trend_T10 <- ggplot(temp10, aes(x = Year, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE) + # Disable confidence interval
  xlab("Year") + ylab("T10") + ggtitle("") +
  scale_y_continuous(breaks=seq(0, 50, 5))+
  coord_cartesian(ylim = c(0, 45))+
  theme_bw() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) 
trend_T10



#jpeg("temperature_plot_T10.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
#trend_T10
# Close the jpeg device
#dev.off()



pinjie <- ggarrange(trend_T90, trend_T10, ncol = 2, nrow = 1, 
                    common.legend = TRUE, legend = "right")
pinjie

# 指定标题位置
# theme(plot.title = element_text(hjust = 0.5))+
#############################################################################

### Seasonal trend
# 结合1与2
library(tseries)
library(stats)
s90CAR <- data90.1$seasonal
s90NEA <- data90.2$seasonal
s90NEF <- data90.3$seasonal
s90NPL <- data90.4$seasonal
s90NWP <- data90.5$seasonal
s10CAR <- data10.1$seasonal
s10NEA <- data10.2$seasonal
s10NEF <- data10.3$seasonal
s10NPL <- data10.4$seasonal
s10NWP <- data10.5$seasonal

temp_seasonal <- cbind(s90CAR, s90NEA, s90NEF, s90NPL, s90NWP, 
                       s10CAR, s10NEA, s10NEF, s10NPL, s10NWP)
setwd("E:/Project/Temperature_Mortality_Canada/data_0020")
write.table(temp_seasonal, "temp_seasonal.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

temps <- read.csv("E:/Project/Temperature_Mortality_Canada/data_0020/temp_seasonal.csv")
temps$time <- time
temps$time <- as.Date.character(temps$time)

## T90
sT90_CAR <- ggplot(temps, aes(time, group = 1), linewidth = 0.4) +
  geom_line(aes(y = s90CAR), col = '#00BFC4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('(a) CAR') +
  theme(axis.title.y = element_blank())
sT90_CAR

sT90_NEA <- ggplot(temps, aes(time, group = 1), linewidth = 0.4) +
  geom_line(aes(y = s90NEA), col = '#00BFC4') +
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('(b) NEA') +
  theme(axis.title.y = element_blank())
sT90_NEA

sT90_NEF <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s90NEF), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('(c) NEF') +
  theme(axis.title.y = element_blank())
sT90_NEF

sT90_NPL <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s90NPL), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('(d) NPL') +
  theme(axis.title.y = element_blank())
sT90_NPL

sT90_NWP <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s90NWP), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('(e) NWP') +
  theme(axis.title.y = element_blank())
sT90_NWP

## T10
sT10_CAR <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s10CAR), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('CAR') +
  theme(axis.title.y = element_blank())
sT10_CAR

sT10_NEA <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s10NEA), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('NEA') +
  theme(axis.title.y = element_blank())
sT10_NEA

sT10_NEF <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s10NEF), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('NEF') +
  theme(axis.title.y = element_blank())
sT10_NEF

sT10_NPL <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s10NPL), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('NPL') +
  theme(axis.title.y = element_blank())
sT10_NPL

sT10_NWP <- ggplot(temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = s10NWP), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('NWP') +
  theme(axis.title.y = element_blank())
sT10_NWP


tempsplot <- ggarrange(sT90_CAR, sT10_CAR, sT90_NEA, sT10_NEA, sT90_NEF, sT10_NEF, 
                       sT90_NPL, sT10_NPL, sT90_NWP, sT10_NWP,
                       ncol = 2, nrow = 5)
tempsplot

T90plot <- ggarrange(T90_CAR, sT90_CAR, T90_NEA, sT90_NEA, T90_NEF, sT90_NEF, 
                     T90_NPL, sT90_NPL, T90_NWP, sT90_NWP, 
                     ncol = 2, nrow = 5)
T90plot

T10plot <- ggarrange(T10_CAR, sT10_CAR, T10_NEA, sT10_NEA, T10_NEF, sT10_NEF, 
                     T10_NPL, sT10_NPL, T10_NWP, sT10_NWP, 
                     ncol = 2, nrow = 5)
T10plot




#########################################################################
### Canada
## Temperature Trend 
canT10 <- read.csv("Canada_T10.csv")
canT90 <- read.csv("Canada_T90.csv")
canT10$X <- months
canT10$Year <- canT10$X
canT90$X <- months
canT90$Year <- canT90$X

#T90
temp_can_T90 <- ggplot(canT90) + geom_line(data = canT90, aes(x = Year, y = Canada, group = 1),linewidth = 0.4, col = '#B11901FF') +
  geom_smooth(aes(x = Year, y= Canada, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(a) Canada')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10))+
  scale_y_continuous(breaks=seq(0, 40, 5))+
  coord_cartesian(ylim = c(0, 30))
temp_can_T90

#T10
temp_can_T10 <- ggplot(canT10) + geom_line(data = canT10, aes(x = Year, y = Canada, group = 1),linewidth = 0.4, col = 'darkblue') +
  geom_smooth(aes(x = Year, y= Canada, group = 1), col = 'darkblue') +
  xlab("Time") + ylab('T10') + ggtitle('(b) Canada')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10))+
  scale_y_continuous(breaks=seq(0, 40, 5))+
  coord_cartesian(ylim = c(0, 30))
temp_can_T10
# T10和T90 trend 的拼接
canplot <- ggarrange(temp_can_T90, temp_can_T10, 
                     ncol = 2, nrow = 1)
canplot

## Temperature Seasonal Trend 
canT10 <- read.csv("E:/Project/Temperature_Mortality_Canada/data_0020/Canada_T10.csv")
canT90 <- read.csv("E:/Project/Temperature_Mortality_Canada/data_0020/Canada_T90.csv")
canT10 <- ts(canT10[, 2],start = c(2000,1),frequency = 12) 
canT90 <- ts(canT90[, 2],start = c(2000,1),frequency = 12) 
canT10d <- decompose(canT10)
canT90d <- decompose(canT90)

canT10s <- canT10d$seasonal
canT90s <- canT90d$seasonal

can_temp_seasonal <- cbind(canT90s, canT10s)
setwd("E:/Project/Temperature_Mortality_Canada/data_0020")
write.table(can_temp_seasonal, "can_temp_seasonal.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

can_temps <- read.csv("E:/Project/Temperature_Mortality_Canada/data_0020/can_temp_seasonal.csv")
can_temps$time <- time
can_temps$time <- as.Date.character(can_temps$time)

## T90
stemp_can_T90 <- ggplot(can_temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = canT90s), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('Canada') +
  theme(axis.title.y = element_blank())
stemp_can_T90

## T10
stemp_can_T10 <- ggplot(can_temps, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = canT10s), col = '#00BFC4')+
  theme(axis.text = element_text(size = 10)) +
  xlab("Time") + ggtitle('Canada') +
  theme(axis.title.y = element_blank())
stemp_can_T10

canT90plot <- ggarrange(temp_can_T90, stemp_can_T90,  
                        ncol = 2, nrow = 1)
# 8.50 * 4.00
canT90plot

canT10plot <- ggarrange(temp_can_T10, stemp_can_T10,  
                        ncol = 2, nrow = 1)

###################################################
temp_can_T90 <- ggplot(canT90) + geom_line(data = canT90, aes(x = Year, y = Canada, group = 1),linewidth = 0.4, col = '#B11901FF') +
  geom_smooth(aes(x = Year, y= Canada, group = 1), col = '#B11901FF') +
  xlab("Time") + ylab('T90') + ggtitle('(a) Canada')+ 
  theme(axis.title.y=element_text(size = 10)) + theme(axis.text= element_text(size = 10))+
  scale_y_continuous(breaks=seq(0, 40, 5))+
  coord_cartesian(ylim = c(0, 30))

## T90的趋势，五个地区画在一张图上
trend_T90 <- ggplot(temp90, aes(x = Year, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) + # Disable confidence interval
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE) + # Disable confidence interval
  xlab("Year") + ylab("T90") + ggtitle("") +
  scale_y_continuous(breaks=seq(0, 50, 5))+
  coord_cartesian(ylim = c(0, 45))+
  theme_bw() +
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" )) 

trend_T90

trend_T90 + ggplot(canT90) + geom_line(data = canT90, aes(x = Year, y = Canada, group = 1),linewidth = 0.4, col = '#B11901FF') +
  geom_smooth(aes(x = Year, y= Canada, group = 1), col = '#B11901FF')


jpeg("temperature_plot_T90.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
trend_T90
# Close the jpeg device
dev.off()
