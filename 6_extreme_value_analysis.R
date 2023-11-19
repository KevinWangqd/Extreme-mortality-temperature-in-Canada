## Threshold Selection and Pickands Dependence Function Plot
# packages
library(pacman)
library(extRemes)
library(fExtremes)
library(evd)
library(POT)
library(ggplot2)

## This chunk is for mrlplot and tcplot
#mrplot_1 <- mrlplot(National$T90)
#data <- data.frame(mrplot_1)
#quantile(National$T90,0.7)
#par(mfrow = c(1,1))
#tc <- POT::tcplot(National$T90)
#tc <- POT::tcplot(National$T90,nt=50)
# Assuming you already have the tcplot stored in the variable 'tc'
#tc <- POT::tcplot(National$T90, nt = 150)
#plot(tc[[1]])
# Plot the second panel
#plot(tc[[2]])
# Add vertical lines at x = 3.67
#abline(v = 3.67, col = "red", lty = 2)  # Add a red dashed vertical line
# Create the mrlplot
#mrplot_1 <- mrlplot(National$T90)
# Add the custom line with slope -0.3 and intercept 5.5
#abline(a = 5.5, b = -0.3, col = "blue", lty = 2)
#abline(v = 3.67, col = "red", lty = 2)  # Add a red dashed vertical line
#jpeg("mrl_shape.jpeg", quality = 100, units = "in", width = 6, height = 6, res = 300)
# Print the plot or visualization
#POT::tcplot(National$T90, nt = 150,which = 1)
# Add the custom line with slope -0.3 and intercept 5.5
#abline(v = 3.67, col = "red", lty = 2)  # Add a red dashed vertical line
# Close the jpeg device
#dev.off()





#p_load(dplyr, extRemes, fExtremes, MASS, scales, nortest, evd, POT, gridExtra)
##National level
National <-  read.csv("Total_residual.csv", header = T)
National_T10_t <- quantile(National$T10,0.7)
National_T90_t <- quantile(National$T90,0.7)
National_044_t <- quantile(National$Total_044,0.7)
National_4564_t <- quantile(National$Total_4564,0.7)
National_6584_t <- quantile(National$Total_6584,0.7)
National_85_t <- quantile(National$Total_85,0.7)


National_10044 <- National[,c(1,3)]
National_104564 <- National[,c(1,4)]
National_106584 <- National[,c(1,5)]
National_1085 <- National[,c(1,6)]
taildep(National_10044[,1],National_10044[,2],0.7)
taildep(National_104564[,1],National_104564[,2],0.7)
taildep(National_106584[,1],National_106584[,2],0.7)
taildep(National_1085[,1],National_1085[,2],0.7)


## T10
model_National_10044 <- fitbvgpd(National_10044, threshold = c(National_T10_t, National_044_t),model = "log")
model_National_10044$chi
print(model_National_10044) 
pickdep(model_National_10044, main = "") 

model_National_104564 <- fitbvgpd(National_104564, threshold = c(National_T10_t, National_4564_t),model = "log")
model_National_104564$chi
print(model_National_104564) 


model_National_106584 <- fitbvgpd(National_106584, threshold = c(National_T10_t, National_6584_t),model = "log")
model_National_106584$chi
print(model_National_106584) 

model_National_1085 <- fitbvgpd(National_1085, threshold = c(National_T10_t, National_85_t),model = "log")
model_National_1085$chi
print(model_National_1085) 


## T90
National_90044 <- National[,c(2,3)]
National_904564 <- National[,c(2,4)]
National_906584 <- National[,c(2,5)]
National_9085 <- National[,c(2,6)]

model_National_90044 <- fitbvgpd(National_90044, threshold = c(National_T90_t, National_044_t),model = "log")
model_National_90044$chi
print(model_National_90044) 

model_National_904564 <- fitbvgpd(National_904564, threshold = c(National_T90_t, National_4564_t),model = "log")
model_National_904564$chi
print(model_National_904564) 

model_National_906584 <- fitbvgpd(National_906584, threshold = c(National_T90_t, National_6584_t),model = "log")
model_National_906584$chi
print(model_National_906584) 

model_National_9085 <- fitbvgpd(National_9085, threshold = c(National_T90_t, National_85_t),model = "log")
model_National_9085$chi
print(model_National_9085) 










## For subsequent regions
# Import the residuals data
residual <- read.csv("residual_series.csv", header = T)



## Age 0-44 T10
CART10044 <- residual[, c(1, 11)]
CAR_x11 <- quantile(CART10044$CAR_T10, 0.7)
CAR_y11 <- quantile(CART10044$CAR_044, 0.7)
model_CART10044 <- fitbvgpd(CART10044, threshold = c(CAR_x11, CAR_y11),model = "log")
AIC(model_CART10044)
#Must be one of log (the default), alog, nlog, anlog, mix and amix for the logistic, asymmetric logistic, negative logistic, asymmetric negative logistic, mixed and asymmetric mixed models.
model_CART10044$chi  #Get the dependence coefficient Chi
pickdep(model_CART10044, main = "CAR: T10 and Death Aged 0-44") 
print(model_CART10044) #Get the Joint Proportion Above
taildep(CART10044$CAR_T,CART10044$CAR_044,0.9)
chiplot(CART10044)


scatter.smooth(CART10044$CAR_T,CART10044$CAR_044)



NEAT10044 <- residual[, c(2, 12)]
NEA_x12 <- quantile(NEAT10044$NEA_T10,0.7)
NEA_y12 <- quantile(NEAT10044$NEA_044,0.7)
model_NEAT10044 <- fitbvgpd(NEAT10044, threshold = c(NEA_x12, NEA_y12))
model_NEAT10044$chi
AIC(model_NEAT10044)


pickdep(model_NEAT10044, main = "NEA: T10 and Death Aged 0-44")
print(model_NEAT10044)
taildep(NEAT10044$NEA_T,NEAT10044$NEA_044,0.7)
taildep(NEAT10044$NEA_T,NEAT10044$NEA_044,0.9)

chiplot(NEAT10044)


NEFT10044 <- residual[, c(3, 13)]
NEF_x13 <- quantile(NEFT10044$NEF_T10,0.7)
NEF_y13 <- quantile(NEFT10044$NEF_044,0.7)
model_NEFT10044 <- fitbvgpd(NEFT10044, threshold = c(NEF_x13, NEF_y13))
model_NEFT10044$chi
AIC(model_NEFT10044)

pickdep(model_NEFT10044, main = "NEF: T10 and Death Aged 0-44")
print(model_NEFT10044)


taildep(NEFT10044$NEF_T,NEFT10044$NEF_044,0.7)
taildep(NEFT10044$NEF_T,NEFT10044$NEF_044,0.9)

chiplot(NEFT10044)




NPLT10044 <- residual[, c(4, 14)]
NPL_x14 <- quantile(NPLT10044$NPL_T10,0.7)
NPL_y14 <- quantile(NPLT10044$NPL_044,0.7)
model_NPLT10044 <- fitbvgpd(NPLT10044, threshold = c(NPL_x14, NPL_y14))
model_NPLT10044$chi
pickdep(model_NPLT10044, main = "NPL: T10 and Death Aged 0-44")
print(model_NPLT10044)
taildep(NPLT10044$NPL_T10,NPLT10044$NPL_044,0.7)
taildep(NPLT10044$NPL_T10,NPLT10044$NPL_044,0.9)
chiplot(NPLT10044)





NWPT10044 <- residual[, c(5, 15)]
NWP_x15 <- quantile(NWPT10044$NWP_T10,0.7)
NWP_y15 <- quantile(NWPT10044$NWP_044,0.7)
model_NWPT10044 <- fitbvgpd(NWPT10044, threshold = c(NWP_x15, NWP_y15))
model_NWPT10044$chi
pickdep(model_NWPT10044, main = "NWP: T10 and Death Aged 0-44")
print(model_NWPT10044)

taildep(NWPT10044$NWP_T10,NWPT10044$NWP_044,0.7)
taildep(NWPT10044$NWP_T10,NWPT10044$NWP_044,0.9)




## Age 0-44 T90
CART90044 <- residual[, c(6, 11)]
CAR_x21 <- quantile(CART90044$CAR_T90, 0.7)
CAR_y21 <- quantile(CART90044$CAR_044, 0.7)
model_CART90044 <- fitbvgpd(CART90044, threshold = c(CAR_x21, CAR_y21),model = "log")
model_CART90044$chi


pickdep(model_CART90044, main = "CAR: T90 and Death Aged 0-44")
print(model_CART90044)
taildep(CART90044[,1],CART90044[,2],0.7)
taildep(CART90044[,1],CART90044[,2],0.9)



NEAT90044 <- residual[, c(7, 12)]
NEA_x22 <- quantile(NEAT90044$NEA_T90,0.7)
NEA_y22 <- quantile(NEAT90044$NEA_044,0.7)
model_NEAT90044 <- fitbvgpd(NEAT90044, threshold = c(NEA_x22, NEA_y22),model = "log")
model_NEAT90044$chi


pickdep(model_NEAT90044, main = "NEA: T90 and Death Aged 0-44")
print(model_NEAT90044)
taildep(NEAT90044[,1],NEAT90044[,2],0.7)
taildep(NEAT90044[,1],NEAT90044[,2],0.9)

scatter.smooth(NEAT90044[,1],NEAT90044[,2])


NEFT90044 <- residual[, c(8, 13)]
NEF_x23 <- quantile(NEFT90044$NEF_T90,0.7)
NEF_y23 <- quantile(NEFT90044$NEF_044,0.7)
model_NEFT90044 <- fitbvgpd(NEFT90044, threshold = c(NEF_x23, NEF_y23))
model_NEFT90044$chi
pickdep(model_NEFT90044, main = "NEF: T90 and Death Aged 0-44")
print(model_NEFT90044)
taildep(NEFT90044[,1],NEFT90044[,2],0.7)
taildep(NEFT90044[,1],NEFT90044[,2],0.9)


NPLT90044 <- residual[, c(9, 14)]
NPL_x24 <- quantile(NPLT90044$NPL_T90,0.7)
NPL_y24 <- quantile(NPLT90044$NPL_044,0.7)
model_NPLT90044 <- fitbvgpd(NPLT90044, threshold = c(NPL_x24, NPL_y24))
model_NPLT90044$chi
pickdep(model_NPLT90044, main = "NPL: T90 and Death Aged 0-44")
print(model_NPLT90044)
taildep(NPLT90044[,1],NPLT90044[,2],0.7)
taildep(NPLT90044[,1],NPLT90044[,2],0.9)


NWPT90044 <- residual[, c(10, 15)]
NWP_x25 <- quantile(NWPT90044$NWP_T90,0.7)
NWP_y25 <- quantile(NWPT90044$NWP_044,0.7)
model_NWPT90044 <- fitbvgpd(NWPT90044, threshold = c(NWP_x25, NWP_y25))
model_NWPT90044$chi
pickdep(model_NWPT90044, main = "NWP: T90 and Death Aged 0-44")
print(model_NWPT90044)
taildep(NWPT90044[,1],NWPT90044[,2],0.7)
taildep(NWPT90044[,1],NWPT90044[,2],0.9)


## Age 45-64 T10
CART1045 <- residual[, c(1, 16)]
CAR_x31 <- quantile(CART1045$CAR_T10, 0.7)
CAR_y31 <- quantile(CART1045$CAR_4564, 0.7)
model_CART1045 <- fitbvgpd(CART1045, threshold = c(CAR_x31, CAR_y31))
model_CART1045$chi
pickdep(model_CART1045, main = "CAR: T10 and Death Aged 45-64")
print(model_CART1045)
taildep(CART1045$CAR_T10,CART1045$CAR_4564,0.7)
taildep(CART1045$CAR_T10,CART1045$CAR_4564,0.9)

chiplot(CART1045)



NEAT1045 <- residual[, c(2, 17)]
NEA_x32 <- quantile(NEAT1045$NEA_T10,0.7)
NEA_y32 <- quantile(NEAT1045$NEA_4564,0.7)
model_NEAT1045 <- fitbvgpd(NEAT1045, threshold = c(NEA_x32, NEA_y32))
model_NEAT1045$chi
pickdep(model_NEAT1045, main = "NEA: T10 and Death Aged 45-64")
taildep(NEAT1045$NEA_T10,NEAT1045$NEA_4564,0.7)
taildep(NEAT1045$NEA_T10,NEAT1045$NEA_4564,0.9)
print(model_NEAT1045)


NEFT1045 <- residual[, c(3, 18)]
NEF_x33 <- quantile(NEFT1045$NEF_T10, 0.7)
NEF_y33 <- quantile(NEFT1045$NEF_4564, 0.7)
model_NEFT1045 <- fitbvgpd(NEFT1045, threshold = c(NEF_x33, NEF_y33))
model_NEFT1045$chi
pickdep(model_NEFT1045, main = "NEF: T10 and Death Aged 45-64")
print(model_NEFT1045)
taildep(NEFT1045$NEF_T10, NEFT1045$NEF_4564, 0.7)
taildep(NEFT1045$NEF_T10, NEFT1045$NEF_4564, 0.9)



NPLT1045 <- residual[, c(4, 19)]
NPL_x34 <- quantile(NPLT1045$NPL_T10, 0.7)
NPL_y34 <- quantile(NPLT1045$NPL_4564, 0.7)
model_NPLT1045 <- fitbvgpd(NPLT1045, threshold = c(NPL_x34, NPL_y34))
model_NPLT1045$chi
pickdep(model_NPLT1045, main = "NPL: T10 and Death Aged 45-64")
print(model_NPLT1045)
taildep(NPLT1045$NPL_T10, NPLT1045$NPL_4564, 0.7)
taildep(NPLT1045$NPL_T10, NPLT1045$NPL_4564, 0.9)


NWPT1045 <- residual[-1, c(5, 20)]
NWP_x35 <- quantile(NWPT1045$NWP_T10,0.7)
NWP_y35 <- quantile(NWPT1045$NWP_4564,0.7)
model_NWPT1045 <- fitbvgpd(NWPT1045, threshold = c(NWP_x35, NWP_y35))
model_NWPT1045$chi
pickdep(model_NWPT1045, main = "NWP: T10 and Death Aged 45-64")
print(model_NWPT1045)
taildep(NWPT1045$NWP_T10, NWPT1045$NWP_4564, 0.7)
taildep(NWPT1045$NWP_T10, NWPT1045$NWP_4564, 0.9)



## Age 45-64 T90
CART9045 <- residual[, c(6, 16)]
CAR_x41 <- quantile(CART9045$CAR_T90, 0.7)
CAR_y41 <- quantile(CART9045$CAR_4564, 0.7)
model_CART9045 <- fitbvgpd(CART9045, threshold = c(CAR_x41, CAR_y41), model = "log")
model_CART9045$chi

pickdep(model_CART9045, main = "CAR: T10 and Death Aged 45-64")
print(model_CART9045)
taildep(CART9045$CAR_T90, CART9045$CAR_4564, 0.7)
taildep(CART9045$CAR_T90, CART9045$CAR_4564, 0.9)



NEAT9045 <- residual[, c(7, 17)]
NEA_x42 <- quantile(NEAT9045$NEA_T90,0.7)
NEA_y42 <- quantile(NEAT9045$NEA_4564,0.7)
model_NEAT9045 <- fitbvgpd(NEAT9045, threshold = c(NEA_x42, NEA_y42), model = "log")
model_NEAT9045$chi
pickdep(model_NEAT9045, main = "NEA: T90 and Death Aged 45-64")
print(model_NEAT9045)
taildep(NEAT9045[,1],NEAT9045[,2],0.7)
taildep(NEAT9045[,1],NEAT9045[,2],0.9)



NEFT9045 <- residual[, c(8, 18)]
NEF_x43 <- quantile(NEFT9045$NEF_T90,0.7)
NEF_y43 <- quantile(NEFT9045$NEF_4564,0.7)
model_NEFT9045 <- fitbvgpd(NEFT9045, threshold = c(NEF_x43, NEF_y43))
model_NEFT9045$chi
pickdep(model_NEFT9045, main = "NEF: T90 and Death Aged 45-64")
print(model_NEFT9045)
taildep(NEFT9045[,1],NEFT9045[,2],0.7)
taildep(NEFT9045[,1],NEFT9045[,2],0.9)

NPLT9045 <- residual[, c(9, 19)]
NPL_x44 <- quantile(NPLT9045$NPL_T90,0.7)
NPL_y44 <- quantile(NPLT9045$NPL_4564,0.7)
model_NPLT9045 <- fitbvgpd(NPLT9045, threshold = c(NPL_x44, NPL_y44))
model_NPLT9045$chi
pickdep(model_NPLT9045, main = "NPL: T90 and Death Aged 45-64")
print(model_NPLT9045)
taildep(NPLT9045[,1],NPLT9045[,2],0.7)
taildep(NPLT9045[,1],NPLT9045[,2],0.9)


NWPT9045 <- residual[-1, c(10, 20)]
NWP_x45 <- quantile(NWPT9045$NWP_T90,0.7)
NWP_y45 <- quantile(NWPT9045$NWP_4564,0.7)
model_NWPT9045 <- fitbvgpd(NWPT9045, threshold = c(NWP_x45, NWP_y45))
model_NWPT9045$chi
pickdep(model_NWPT9045, main = "NWP: T90 and Death Aged 45-64")
print(model_NWPT9045)
taildep(NWPT9045[,1],NWPT9045[,2],0.7)
taildep(NWPT9045[,1],NWPT9045[,2],0.9)

## Age 65-84 T10
CART1065 <- residual[, c(1, 21)]
CAR_x51 <- quantile(CART1065$CAR_T10, 0.7)
CAR_y51 <- quantile(CART1065$CAR_6584, 0.7)
model_CART1065 <- fitbvgpd(CART1065, threshold = c(CAR_x51, CAR_y51))
model_CART1065$chi
pickdep(model_CART1065, main = "CAR: T10 and Death Aged 65-84")
print(model_CART1065)
taildep(CART1065$CAR_T10, CART1065$CAR_6584, 0.7)
taildep(CART1065$CAR_T10, CART1065$CAR_6584, 0.9)

NEAT1065 <- residual[-1, c(2, 22)]
NEA_x52 <- quantile(NEAT1065$NEA_T10, 0.7)
NEA_y52 <- quantile(NEAT1065$NEA_6584, 0.7)
model_NEAT1065 <- fitbvgpd(NEAT1065, threshold = c(NEA_x52, NEA_y52))
model_NEAT1065$chi
pickdep(model_NEAT1065, main = "NEA: T10 and Death Aged 65-84")
print(model_NEAT1065)
taildep(NEAT1065$NEA_T10, NEAT1065$NEA_6584, 0.7)
taildep(NEAT1065$NEA_T10, NEAT1065$NEA_6584, 0.9)

NEFT1065 <- residual[-1, c(3, 23)]
NEF_x53 <- quantile(NEFT1065$NEF_T10, 0.7)
NEF_y53 <- quantile(NEFT1065$NEF_6584, 0.7)
model_NEFT1065 <- fitbvgpd(NEFT1065, threshold = c(NEF_x53, NEF_y53))
model_NEFT1065$chi
pickdep(model_NEFT1065, main = "NEF: T10 and Death Aged 65-84")
print(model_NEFT1065)
taildep(NEFT1065$NEF_T10, NEFT1065$NEF_6584, 0.7)
taildep(NEFT1065$NEF_T10, NEFT1065$NEF_6584, 0.9)

NPLT1065 <- residual[, c(4, 24)]
NPL_x54 <- quantile(NPLT1065$NPL_T10, 0.7)
NPL_y54 <- quantile(NPLT1065$NPL_6584, 0.7)
model_NPLT1065 <- fitbvgpd(NPLT1065, threshold = c(NPL_x54, NPL_y54))
model_NPLT1065$chi
pickdep(model_NPLT1065, main = "NPL: T10 and Death Aged 65-84")
print(model_NPLT1065)
taildep(NPLT1065$NPL_T10, NPLT1065$NPL_6584, 0.7)
taildep(NPLT1065$NPL_T10, NPLT1065$NPL_6584, 0.9)

NWPT1065 <- residual[-1, c(5, 25)]
NWP_x55 <- quantile(NWPT1065$NWP_T10, 0.7)
NWP_y55 <- quantile(NWPT1065$NWP_6584, 0.7)
model_NWPT1065 <- fitbvgpd(NWPT1065, threshold = c(NWP_x55, NWP_y55))
model_NWPT1065$chi
pickdep(model_NWPT1065, main = "NWP: T10 and Death Aged 65-84")
print(model_NWPT1065)
taildep(NWPT1065$NWP_T10, NWPT1065$NWP_6584, 0.7)
taildep(NWPT1065$NWP_T10, NWPT1065$NWP_6584, 0.9)

## Age 65-84 T90
CART9065 <- residual[, c(6, 21)]
CAR_x61 <- quantile(CART9065$CAR_T90, 0.7)
CAR_y61 <- quantile(CART9065$CAR_6584, 0.7)
model_CART9065 <- fitbvgpd(CART9065, threshold = c(CAR_x61, CAR_y61))
model_CART9065$chi
pickdep(model_CART9065, main = "CAR: T90 and Death Aged 65-84")
print(model_CART9065)
taildep(CART9065[,1],CART9065[,2],0.7)
taildep(CART9065[,1],CART9065[,2],0.9)

NEAT9065 <- residual[-1, c(7, 22)]
NEA_x62 <- quantile(NEAT9065$NEA_T90,0.7)
NEA_y62 <- quantile(NEAT9065$NEA_6584,0.7)
model_NEAT9065 <- fitbvgpd(NEAT9065, threshold = c(NEA_x62, NEA_y62))
model_NEAT9065$chi
pickdep(model_NEAT9065, main = "NEA: T90 and Death Aged 65-84")
print(model_NEAT9065)
taildep(NEAT9065[,1],NEAT9065[,2],0.7)
taildep(NEAT9065[,1],NEAT9065[,2],0.9)

NEFT9065 <- residual[-1, c(8, 23)]
NEF_x63 <- quantile(NEFT9065$NEF_T90,0.7)
NEF_y63 <- quantile(NEFT9065$NEF_6584,0.7)
model_NEFT9065 <- fitbvgpd(NEFT9065, threshold = c(NEF_x63, NEF_y63))
model_NEFT9065$chi
pickdep(model_NEFT9065, main = "NEF: T90 and Death Aged 65-84")
print(model_NEFT9065)
taildep(NEFT9065[,1],NEFT9065[,2],0.7)
taildep(NEFT9065[,1],NEFT9065[,2],0.9)


NPLT9065 <- residual[, c(9, 24)]
NPL_x64 <- quantile(NPLT9065$NPL_T90,0.7)
NPL_y64 <- quantile(NPLT9065$NPL_6584,0.7)
model_NPLT9065 <- fitbvgpd(NPLT9065, threshold = c(NPL_x64, NPL_y64))
model_NPLT9065$chi
pickdep(model_NPLT9065, main = "NPL: T90 and Death Aged 65-84")
print(model_NPLT9065)
taildep(NPLT9065[,1],NPLT9065[,2],0.7)
taildep(NPLT9065[,1],NPLT9065[,2],0.9)


NWPT9065 <- residual[-1, c(10, 25)]
NWP_x65 <- quantile(NWPT9065$NWP_T90,0.7)
NWP_y65 <- quantile(NWPT9065$NWP_6584,0.7)
model_NWPT9065 <- fitbvgpd(NWPT9065, threshold = c(NWP_x65, NWP_y65))
model_NWPT9065$chi
pickdep(model_NWPT9065, main = "NWP: T90 and Death Aged 65-84")
print(model_NWPT9065)
taildep(NWPT9065[,1],NWPT9065[,2],0.7)
taildep(NWPT9065[,1],NWPT9065[,2],0.9)


## Age 85+ T10
CART1085 <- residual[, c(1, 26)]
CAR_x71 <- quantile(CART1085$CAR_T10, 0.7)
CAR_y71 <- quantile(CART1085$CAR_85, 0.7)
model_CART1085 <- fitbvgpd(CART1085, threshold = c(CAR_x71, CAR_y71))
model_CART1085$chi
pickdep(model_CART1085, main = "CAR: T10 and Death Aged 85+")
print(model_CART1085)
taildep(CART1085$CAR_T10,CART1085$CAR_85,0.7)
taildep(CART1085$CAR_T10,CART1085$CAR_85,0.9)



NEAT1085 <- residual[, c(2, 27)]
NEA_x72 <- quantile(NEAT1085$NEA_T10,0.7)
NEA_y72 <- quantile(NEAT1085$NEA_85,0.7)
model_NEAT1085 <- fitbvgpd(NEAT1085, threshold = c(NEA_x72, NEA_y72))
model_NEAT1085$chi
pickdep(model_NEAT1085, main = "NEA: T10 and Death Aged 85+")
print(model_NEAT1085)
taildep(NEAT1085$NEA_T10,NEAT1085$NEA_85,0.7)
taildep(NEAT1085$NEA_T10,NEAT1085$NEA_85,0.9)


NEFT1085 <- residual[, c(3, 28)]
NEF_x73 <- quantile(NEFT1085$NEF_T10,0.7)
NEF_y73 <- quantile(NEFT1085$NEF_85,0.7)
model_NEFT1085 <- fitbvgpd(NEFT1085, threshold = c(NEF_x73, NEF_y73))
model_NEFT1085$chi
pickdep(model_NEFT1085, main = "NEF: T10 and Death Aged 85+")
print(model_NEFT1085)
taildep(NEFT1085$NEF_T10,NEFT1085$NEF_85,0.7)
taildep(NEFT1085$NEF_T10,NEFT1085$NEF_85,0.9)


NPLT1085 <- residual[, c(4, 29)]
NPL_x74 <- quantile(NPLT1085$NPL_T10,0.7)
NPL_y74 <- quantile(NPLT1085$NPL_85,0.7)
model_NPLT1085 <- fitbvgpd(NPLT1085, threshold = c(NPL_x74, NPL_y74))
model_NPLT1085$chi
pickdep(model_NPLT1085, main = "NPL: T10 and Death Aged 85+")
print(model_NPLT1085)
taildep(NPLT1085$NPL_T10,NPLT1085$NPL_85,0.7)
taildep(NPLT1085$NPL_T10,NPLT1085$NPL_85,0.9)


NWPT1085 <- residual[, c(5, 30)]
NWP_x75 <- quantile(NWPT1085$NWP_T10,0.7)
NWP_y75 <- quantile(NWPT1085$NWP_85,0.7)
model_NWPT1085 <- fitbvgpd(NWPT1085, threshold = c(NWP_x75, NWP_y75))
model_NWPT1085$chi
pickdep(model_NWPT1085, main = "NWP: T10 and Death Aged 85+")
print(model_NWPT1085)
taildep(NWPT1085$NWP_T10,NWPT1085$NWP_85,0.7)
taildep(NWPT1085$NWP_T10,NWPT1085$NWP_85,0.9)


## Age 85+ T90
CART9085 <- residual[, c(6, 26)]
CAR_x81 <- quantile(CART9085$CAR_T90, 0.7)
CAR_y81 <- quantile(CART9085$CAR_85, 0.7)
model_CART9085 <- fitbvgpd(CART9085, threshold = c(CAR_x81, CAR_y81))
model_CART9085$chi
pickdep(model_CART9085, main = "CAR: T90 and Death Aged 85+")
print(model_CART9085)
taildep(CART9085[,1],CART9085[,2],0.7)
taildep(CART9085[,1],CART9085[,2],0.9)


NEAT9085 <- residual[, c(7, 27)]
NEA_x82 <- quantile(NEAT9085$NEA_T90,0.7)
NEA_y82 <- quantile(NEAT9085$NEA_85,0.7)
model_NEAT9085 <- fitbvgpd(NEAT9085, threshold = c(NEA_x82, NEA_y82))
model_NEAT9085$chi
pickdep(model_NEAT9085, main = "NEA: T90 and Death Aged 85+")
print(model_NEAT9085)
taildep(NEAT9085[,1],NEAT9085[,2],0.7)
taildep(NEAT9085[,1],NEAT9085[,2],0.9)


NEFT9085 <- residual[, c(8, 28)]
NEF_x83 <- quantile(NEFT9085$NEF_T90,0.7)
NEF_y83 <- quantile(NEFT9085$NEF_85,0.7)
model_NEFT9085 <- fitbvgpd(NEFT9085, threshold = c(NEF_x83, NEF_y83))
model_NEFT9085$chi
pickdep(model_NEFT9085, main = "NEF: T90 and Death Aged 85+")
print(model_NEFT9085)
taildep(NEFT9085[,1],NEFT9085[,2],0.7)
taildep(NEFT9085[,1],NEFT9085[,2],0.9)


NPLT9085 <- residual[, c(9, 29)]
NPL_x84 <- quantile(NPLT9085$NPL_T90,0.7)
NPL_y84 <- quantile(NPLT9085$NPL_85,0.7)
model_NPLT9085 <- fitbvgpd(NPLT9085, threshold = c(NPL_x84, NPL_y84))
model_NPLT9085$chi
pickdep(model_NPLT9085, main = "NPL: T90 and Death Aged 85+")
print(model_NPLT9085)
taildep(NPLT9085[,1],NPLT9085[,2],0.7)
taildep(NPLT9085[,1],NPLT9085[,2],0.9)


NWPT9085 <- residual[, c(10, 30)]
NWP_x85 <- quantile(NWPT9085$NWP_T90,0.7)
NWP_y85 <- quantile(NWPT9085$NWP_85,0.7)
model_NWPT9085 <- fitbvgpd(NWPT9085, threshold = c(NWP_x85, NWP_y85))
model_NWPT9085$chi
pickdep(model_NWPT9085, main = "NWP: T90 and Death Aged 85+")
print(model_NWPT9085)
taildep(NWPT9085[,1],NWPT9085[,2],0.7)
taildep(NWPT9085[,1],NWPT9085[,2],0.9)

