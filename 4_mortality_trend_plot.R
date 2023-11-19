## Mortality Trend Plot
library(tseries)
library(ggplot2)
library(ggpubr)

age044 <- read.csv("mortality_age0_44.csv")
age4564 <- read.csv("mortality_age45_64.csv")
age6584 <- read.csv("mortality_age65_84.csv")
age85 <- read.csv("mortality_age85+.csv")
temp <- seq.Date(from = as.Date("2000/01/01", format = "%Y/%m/%d"), 
                 by = "month", length.out = 252)
time <- as.character(temp)
age044$time <- time
age044$time <- as.Date.character(age044$time)
age4564$time <- time
age4564$time <- as.Date.character(age4564$time)
age6584$time <- time
age6584$time <- as.Date.character(age6584$time)
age85$time <- time
age85$time <- as.Date.character(age85$time)

plot_age044 <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 30, 5))+
  theme(plot.title = element_text(size = 20,hjust = 0.5))+
  theme_bw()+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))     
plot_age044
###########################试验
plot_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) +
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE)+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 30, 5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))        
plot_age044t


jpeg("mortality_044.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_age044t
# Close the jpeg device
dev.off()




#####################################
plot_age4564 <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 85, 5))+  
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))         
plot_age4564
###########################试验
plot_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) +
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE)+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 85, 5))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))        
plot_age4564t


jpeg("mortality_4564.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_age4564t
# Close the jpeg device
dev.off()



#####################################
plot_age6584 <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 700, 50))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))   
plot_age6584
###########################试验
plot_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) +
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE)+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 700, 50))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))              
plot_age6584t

jpeg("mortality_6584.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_age6584t
# Close the jpeg device
dev.off()

#####################################
plot_age85 <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 3500, 250))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))   
plot_age85
###########################试验
plot_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_line(aes(y = Total, colour = "National"))+
  geom_smooth(aes(y = CAR, colour = "CAR"), se = FALSE) +
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  geom_smooth(aes(y = Total, colour = "National"), se = FALSE)+
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 3500, 250))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(name = "Region", values = c("CAR" = "blue", "NEA" = "red", "NEF" = "purple", 
                                                 "NPL" = "orange", "NWP" = "brown", "National"="#2ca02c")) + 
  scale_linetype_manual(name = "Region", values = c("CAR" = "solid", "NEA" = "dashed", "NEF" = "dotted", 
                                                    "NPL" = "twodash", "NWP" = "longdash","National"="dotdash" ))            
plot_age85t
#####################################
jpeg("mortality_85.jpeg", quality = 100, units = "in", width = 7, height = 6, res = 300)

# Print the plot or visualization
plot_age85t
# Close the jpeg device
dev.off()


#################################################################
### 分CAR和其他4个地区绘制
############################################### Age 0-44
plot1_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_smooth(aes(y = CAR, colour = "CAR")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 30, 5))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot1_age044t

plot2_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot2_age044t

############################################### Age 45-64
plot1_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_smooth(aes(y = CAR, colour = "CAR")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 90, 10))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot1_age4564t

plot2_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 60, 5))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot2_age4564t

############################################### Age 65-84
plot1_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_smooth(aes(y = CAR, colour = "CAR")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 700, 100))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot1_age6584t

plot2_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot2_age6584t

############################################### Age 65-84
plot1_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = CAR, colour = "CAR"))+
  geom_smooth(aes(y = CAR, colour = "CAR")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 3500, 500))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot1_age85t

plot2_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NEA, colour = "NEA"), se = FALSE) +
  geom_smooth(aes(y = NEF, colour = "NEF"), se = FALSE) +
  geom_smooth(aes(y = NPL, colour = "NPL"), se = FALSE) +
  geom_smooth(aes(y = NWP, colour = "NWP"), se = FALSE) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 1700, 100))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot2_age85t

plotsample <- ggarrange(plot_age044t, plot1_age044t, plot2_age044t, 
                        plot_age4564t, plot1_age4564t, plot2_age4564t, 
                        plot_age6584t, plot1_age6584t, plot2_age6584t, 
                        plot_age85t, plot1_age85t, plot2_age85t, 
                        ncol = 3, nrow = 4)
plotsample
###############################################################################
## monthly mortality 分地区分年龄的trend和seasonal的单独绘制
## trend部分
# Age 0 - 44
plot3_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_smooth(aes(y = NEA, colour = "NEA")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot3_age044t

plot4_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_smooth(aes(y = NEF, colour = "NEF")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot4_age044t

plot5_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_smooth(aes(y = NPL, colour = "NPL")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot5_age044t

plot6_age044t <- ggplot(age044, aes(time, group = 1))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NWP, colour = "NWP")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 0 - 44")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot6_age044t
# Age 45-64
plot3_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_smooth(aes(y = NEA, colour = "NEA")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot3_age4564t

plot4_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_smooth(aes(y = NEF, colour = "NEF")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot4_age4564t

plot5_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_smooth(aes(y = NPL, colour = "NPL")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot5_age4564t

plot6_age4564t <- ggplot(age4564, aes(time, group = 1))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NWP, colour = "NWP")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 45 - 64")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot6_age4564t
# Age 65 - 84
plot3_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_smooth(aes(y = NEA, colour = "NEA")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot3_age6584t

plot4_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_smooth(aes(y = NEF, colour = "NEF")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot4_age6584t

plot5_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_smooth(aes(y = NPL, colour = "NPL")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot5_age6584t

plot6_age6584t <- ggplot(age6584, aes(time, group = 1))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NWP, colour = "NWP")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 65 - 84")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot6_age6584t
# Age 85 +
plot3_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = NEA, colour = "NEA"))+
  geom_smooth(aes(y = NEA, colour = "NEA")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot3_age85t

plot4_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = NEF, colour = "NEF"))+
  geom_smooth(aes(y = NEF, colour = "NEF")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot4_age85t

plot5_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = NPL, colour = "NPL"))+
  geom_smooth(aes(y = NPL, colour = "NPL")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot5_age85t

plot6_age85t <- ggplot(age85, aes(time, group = 1))+
  geom_line(aes(y = NWP, colour = "NWP"))+
  geom_smooth(aes(y = NWP, colour = "NWP")) +
  ylab("Mortality: per 100,000 people per year")+
  ggtitle("Age 85 +")+
  xlab("Year")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual("Legend", values = c("CAR" = "#F16C23", "NEA" = "#D15C6B", "NEF" = "#8386A8", 
                                          "NPL" = "#8FB943", "NWP" = "#78B9D2"))       
plot6_age85t
# 合并
mortality_trend <- ggarrange(plot1_age044t, plot1_age4564t, plot1_age6584t, plot1_age85t,
                             plot3_age044t, plot3_age4564t, plot3_age6584t, plot3_age85t,
                             plot4_age044t, plot4_age4564t, plot4_age6584t, plot4_age85t,
                             plot5_age044t, plot5_age4564t, plot5_age6584t, plot5_age85t,
                             plot6_age044t, plot6_age4564t, plot6_age6584t, plot6_age85t, 
                             ncol = 4, nrow = 5, legend = "none")
mortality_trend
## seasonal部分



###############################################################################
### Canada mortality trend plot
canmt <- read.csv("Canada_mortality.csv")
temp <- seq.Date(from = as.Date("2000/01/01", format = "%Y/%m/%d"), 
                 by = "month", length.out = 252)
time <- as.character(temp)
canmt$time <- time
canmt$time <- as.Date.character(canmt$time)

plot_Canada_t <- ggplot(canmt, aes(time, group = 1))+
  geom_line(aes(y = Canada), col = "#00BFC4")+
  geom_smooth(aes(y = Canada), col = "#F8766D", se = FALSE) +
  ylab("Mortality: per 100,000 people per year")+
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 80, 5))
plot_Canada_t

### Canada mortality seasonal plot
canmt <- read.csv("Canada_mortality.csv")
canmt <- ts(canmt[, 1],start = c(2000,1),frequency = 12) 
canmtd <- decompose(canmt)

canmts <- canmtd$seasonal

setwd("E:/Project/Temperature_Mortality_Canada/data_0020")
write.table(canmts, "can_mor_seasonal.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

can_mors <- read.csv("can_mor_seasonal.csv")
can_mors$time <- time
can_mors$time <- as.Date.character(can_mors$time)

## T90
can_mors_plot <- ggplot(can_mors, aes(time, group = 1), linewidth = 0.4)+
  geom_line(aes(y = x), col = '#00BFC4')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Time")+theme_bw()+ggtitle("Mortality sesonal trend: Canada")


can_mors_plot

can_mor_plot <- ggarrange(plot_Canada_t, can_mors_plot)
can_mor_plot


