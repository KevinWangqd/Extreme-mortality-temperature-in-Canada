## mortality-ARIMA
library(tseries)
library(forecast)
library(FinTS)

#National level ARIMA
inital_044 <- read.csv("mortality_age0_44.csv")
data044 <- ts(inital_044[,2:7],start = c(2000,1),frequency=12) 

inital_4564 <- read.csv("mortality_age45_64.csv")
data4564 <- ts(inital_4564[,2:7],start = c(2000,1),frequency=12) 
plot(data4564,xlab="Time")

inital_6584 <- read.csv("mortality_age65_84.csv")
data6584 <- ts(inital_6584[,2:7],start = c(2000,1),frequency=12) 
plot(data6584,xlab="Time")

inital_85 <- read.csv("mortality_age85+.csv")
data85 <- ts(inital_85[,2:7],start = c(2000,1),frequency=12) 
plot(data85,xlab="Time")

data.fit044_6 <- auto.arima(data044[, 6], D = 1)
summary(data.fit044_6)
model044_6 <- arima(data044[, 6], order = c(2,1,1), 
                    seasonal = list(order = c(0,1,2), period = 12))
Box.test(model044_6$residuals,type="Ljung-Box") # All are white noisy now.
Total_044 <- model044_6$residuals

data.fit4564_6 <- auto.arima(data4564[, 6], D = 1)
summary(data.fit4564_6)
model4564_6 <- arima(data4564[, 6], order = c(3,1,1), 
                     seasonal = list(order = c(0,1,2), period = 12))
Box.test(model4564_6$residuals,type="Ljung-Box") # All are white noisy now.
Total_4564 <- model4564_6$residuals

data.fit6584_6 <- auto.arima(data6584[, 6], D = 1)
summary(data.fit6584_6)

model6584_6 <- arima(data6584[, 6], order = c(2,0,3), 
                     seasonal = list(order = c(1,1,2), period = 12))

Box.test(model6584_6$residuals,type="Ljung-Box") # All are white noisy now.
Total_6584 <- model6584_6$residuals

data.fit85_6 <- auto.arima(data85[, 6], D = 1)
summary(data.fit85_6)

model85_6 <- arima(data85[, 6], order = c(3,1,2), 
                   seasonal = list(order = c(0,1,2), period = 12))

Box.test(model85_6$residuals,type="Ljung-Box") # All are white noisy now.
Total_85 <- model85_6$residuals


Residual_total <- cbind(Total_044, Total_4564, Total_6584, Total_85)

#write.table(Residual_total, "Residual_total.csv", row.names = FALSE, 
#            col.names = TRUE, sep = ",")

























## The following is regional specific mortality ARIMA


## Mortality of Age 0-44
inital_044 <- read.csv("mortality_age0_44.csv")
data044 <- ts(inital_044[,2:7],start = c(2000,1),frequency=12) 
plot(data044,xlab="Time")

# Decompose the time series and view the trend
data044_1 <- decompose(data044[, 1])
plot(data044_1)
data044_2 <- decompose(data044[, 2])
plot(data044_2)
data044_3 <- decompose(data044[, 3])
plot(data044_3)
data044_4 <- decompose(data044[, 4])
plot(data044_4)
data044_5 <- decompose(data044[, 5])
plot(data044_5)

# Stationary Test
adf.test(data044[, 1])
adf.test(data044[, 2]) 
adf.test(data044[, 3])
adf.test(data044[, 4]) # Not stationary
adf.test(data044[, 5]) # Not stationary



ndiffs(data044[, 4])
ndiffs(data044[, 5])

data044.4 <- diff(data044[, 4])
adf.test(data044.4) # Now is stationary 
data044.5 <- diff(data044[, 5])
adf.test(data044.5) # Now is stationary
data044.5 <- diff(data044[, 6])
adf.test(data044.5) # Now is stationary

# Auto.arima (Order Identification)
data.fit044_1 <- auto.arima(data044[, 1], D = 1)
summary(data.fit044_1)
data.fit044_2 <- auto.arima(data044[, 2], D = 1)
summary(data.fit044_2)
data.fit044_3 <- auto.arima(data044[, 3], D = 1)
summary(data.fit044_3)
data.fit044_4 <- auto.arima(data044[, 4], D = 1)
summary(data.fit044_4)
data.fit044_5 <- auto.arima(data044[, 5], D = 1)
summary(data.fit044_5)


model044_1 <- arima(data044[, 1], order = c(2,0,2), 
                  seasonal = list(order = c(1,1,0), period = 12))
model044_2 <- arima(data044[, 2], order = c(2,0,0), 
                  seasonal = list(order = c(1,1,0), period = 12))
model044_3 <- arima(data044[, 3], order = c(2,1,1), 
                  seasonal = list(order = c(1,1,2), period = 12))
model044_4 <- arima(data044[, 4], order = c(1,1,1), 
                  seasonal = list(order = c(1,1,1), period = 12))
model044_5 <- arima(data044[, 5], order = c(1,1,1), 
                  seasonal = list(order = c(0,1,1), period = 12))
# Residual White Noise Test
Box.test(model044_1$residuals,type="Ljung-Box")
Box.test(model044_2$residuals,type="Ljung-Box")
Box.test(model044_3$residuals,type="Ljung-Box")
Box.test(model044_4$residuals,type="Ljung-Box")
Box.test(model044_5$residuals,type="Ljung-Box") # All are white noisy now.

# Heteroscedasticity Test
library(FinTS)
# Portmanteau Q Test
for (i in 1:5) print(Box.test(model044_1$residuals,lag=i))
for (i in 1:5) print(Box.test(model044_2$residuals,lag=i))
for (i in 1:5) print(Box.test(model044_3$residuals,lag=i))
for (i in 1:5) print(Box.test(model044_4$residuals,lag=i))
for (i in 1:5) print(Box.test(model044_5$residuals,lag=i))

# Save Residual Sequence 
CAR_044 <- model044_1$residuals
NEA_044 <- model044_2$residuals
NEF_044 <- model044_3$residuals
NPL_044 <- model044_4$residuals
NWP_044 <- model044_5$residuals

Residual_044 <- cbind(CAR_044, NEA_044, NEF_044, NPL_044, NWP_044)
write.table(Residual_044, "Residual_044_result.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

##########################################################################

## Mortality of Age 45-64
inital_4564 <- read.csv("mortality_age45_64.csv")
data4564 <- ts(inital_4564[,2:6],start = c(2000,1),frequency=12) 
plot(data4564,xlab="Time")

# Decompose the time series and view the trend
data4564_1 <- decompose(data4564[, 1])
plot(data4564_1)
data4564_2 <- decompose(data4564[, 2])
plot(data4564_2)
data4564_3 <- decompose(data4564[, 3])
plot(data4564_3)
data4564_4 <- decompose(data4564[, 4])
plot(data4564_4)
data4564_5 <- decompose(data4564[, 5])
plot(data4564_5)

# Stationary Test
adf.test(data4564[, 1])
adf.test(data4564[, 2])
adf.test(data4564[, 3])
adf.test(data4564[, 4])
adf.test(data4564[, 5]) # All are stationary

# Auto.arima (Order Identification)
data.fit4564_1 <- auto.arima(data4564[, 1], D = 1)
summary(data.fit4564_1)
data.fit4564_2 <- auto.arima(data4564[, 2], D = 1)
summary(data.fit4564_2)
data.fit4564_3 <- auto.arima(data4564[, 3], D = 1)
summary(data.fit4564_3)
data.fit4564_4 <- auto.arima(data4564[, 4], D = 1)
summary(data.fit4564_4)
data.fit4564_5 <- auto.arima(data4564[, 5], D = 1)
summary(data.fit4564_5)

model4564_1 <- arima(data4564[, 1], order = c(2,0,1), 
                    seasonal = list(order = c(2,1,2), period = 12))
model4564_2 <- arima(data4564[, 2], order = c(1,0,0), 
                    seasonal = list(order = c(2,1,0), period = 12))
model4564_3 <- arima(data4564[, 3], order = c(2,1,2), 
                    seasonal = list(order = c(1,1,2), period = 12))
model4564_4 <- arima(data4564[, 4], order = c(1,1,1), 
                    seasonal = list(order = c(1,1,2), period = 12))
model4564_5 <- arima(data4564[, 5], order = c(1,0,0), 
                    seasonal = list(order = c(0,1,1), period = 12))
# Residual White Noise Test
Box.test(model4564_1$residuals,type="Ljung-Box")
Box.test(model4564_2$residuals,type="Ljung-Box")
Box.test(model4564_3$residuals,type="Ljung-Box")
Box.test(model4564_4$residuals,type="Ljung-Box")
Box.test(model4564_5$residuals,type="Ljung-Box") # Not white noise.

plot(model4564_5$residuals)

# Heteroscedasticity Test
# Portmanteau Q Test
for (i in 1:5) print(Box.test(model4564_1$residuals,lag=i))
for (i in 1:5) print(Box.test(model4564_2$residuals,lag=i))
for (i in 1:5) print(Box.test(model4564_3$residuals,lag=i))
for (i in 1:5) print(Box.test(model4564_4$residuals,lag=i))
for (i in 1:5) print(Box.test(model4564_5$residuals,lag=i))

# GARCH model
garch4564_5 <- garch(x=model4564_5$residuals, order = c(0, 1))
summary(garch4564_5)

# Save Residual Sequence 
CAR_4564 <- model4564_1$residuals
NEA_4564 <- model4564_2$residuals
NEF_4564 <- model4564_3$residuals
NPL_4564 <- model4564_4$residuals
NWP_4564 <- garch4564_5$residuals


Residual_4564 <- cbind(CAR_4564, NEA_4564, NEF_4564, NPL_4564, NWP_4564)
write.table(Residual_4564, "Residual_4564_result.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

##########################################################################

## Mortality of Age 65-84
inital_6584 <- read.csv("mortality_age65_84.csv")
data6584 <- ts(inital_6584[,2:6],start = c(2000,1),frequency=12) 
plot(data6584,xlab="Time")

# Decompose the time series and view the trend
data6584_1 <- decompose(data6584[, 1])
plot(data6584_1)
data6584_2 <- decompose(data6584[, 2])
plot(data6584_2)
data6584_3 <- decompose(data6584[, 3])
plot(data6584_3)
data6584_4 <- decompose(data6584[, 4])
plot(data6584_4)
data6584_5 <- decompose(data6584[, 5])
plot(data6584_5)

# Stationary Test
adf.test(data6584[, 1])
adf.test(data6584[, 2])
adf.test(data6584[, 3])
adf.test(data6584[, 4])
adf.test(data6584[, 5]) # All are stationary

# Auto.arima (Order Identification)
data.fit6584_1 <- auto.arima(data6584[, 1], D = 1)
summary(data.fit6584_1)
data.fit6584_2 <- auto.arima(data6584[, 2], D = 1)
summary(data.fit6584_2)
data.fit6584_3 <- auto.arima(data6584[, 3], D = 1)
summary(data.fit6584_3)
data.fit6584_4 <- auto.arima(data6584[, 4], D = 1)
summary(data.fit6584_4)
data.fit6584_5 <- auto.arima(data6584[, 5], D = 1)
summary(data.fit6584_5)


model6584_1 <- arima(data6584[, 1], order = c(4,0,3), 
                    seasonal = list(order = c(1,1,0), period = 12))
model6584_2 <- arima(data6584[, 2], order = c(1,0,1), 
                    seasonal = list(order = c(2,1,1), period = 12))
model6584_3 <- arima(data6584[, 3], order = c(2,0,0), 
                    seasonal = list(order = c(1,1,2), period = 12))
model6584_4 <- arima(data6584[, 4], order = c(1,0,0), 
                    seasonal = list(order = c(1,1,0), period = 12))
model6584_5 <- arima(data6584[, 5], order = c(1,0,0), 
                    seasonal = list(order = c(1,1,0), period = 12))

# Residual White Noise Test
Box.test(model6584_1$residuals,type="Ljung-Box")
Box.test(model6584_2$residuals,type="Ljung-Box") # Not white noisy.
Box.test(model6584_3$residuals,type="Ljung-Box")
Box.test(model6584_4$residuals,type="Ljung-Box")
Box.test(model6584_5$residuals,type="Ljung-Box") # Not white noisy.
plot(model6584_2$residuals)
plot(model6584_5$residuals)

# Portmanteau Q Test
for (i in 1:5) print(Box.test(model6584_1$residuals,lag=i))
for (i in 1:5) print(Box.test(model6584_2$residuals,lag=i))
for (i in 1:5) print(Box.test(model6584_3$residuals,lag=i))  # Not completely white noisy.
for (i in 1:5) print(Box.test(model6584_4$residuals,lag=i))
for (i in 1:5) print(Box.test(model6584_5$residuals,lag=i))

# GARCH model
garch6584_2 <- garch(x=model6584_2$residuals, order = c(0, 1))
summary(garch6584_2)
garch6584_3 <- garch(x=model6584_3$residuals, order = c(0, 1))
summary(garch6584_3)
garch6584_5 <- garch(x=model6584_5$residuals, order = c(0, 1))
summary(garch6584_5)

# Save Residual Sequence 
CAR_6584 <- model6584_1$residuals
NEA_6584 <- garch6584_2$residuals
NEF_6584 <- garch6584_3$residuals
NPL_6584 <- model6584_4$residuals
NWP_6584 <- garch6584_5$residuals
Residual_6584 <- cbind(CAR_6584, NEA_6584, NEF_6584, NPL_6584, NWP_6584)
write.table(Residual_6584, "Residual_6584_result.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")

##########################################################################

## Mortality of Age 85+
inital_85 <- read.csv("mortality_age85+.csv")
data85 <- ts(inital_85[,2:6],start = c(2000,1),frequency=12) 
plot(data85,xlab="Time")

# Decompose the time series and view the trend
data85_1 <- decompose(data85[, 1])
plot(data85_1)
data85_2 <- decompose(data85[, 2])
plot(data85_2)
data85_3 <- decompose(data85[, 3])
plot(data85_3)
data85_4 <- decompose(data85[, 4])
plot(data85_4)
data85_5 <- decompose(data85[, 5])
plot(data85_5)

# Stationary Test
adf.test(data85[, 1]) # Non-stationary
adf.test(data85[, 2])
adf.test(data85[, 3])
adf.test(data85[, 4])
adf.test(data85[, 5])

ndiffs(data85[, 1])
data85.1 <- diff(data85[, 1])
adf.test(data85.1) # Now are stationary

# Auto.arima (Order Identification)
data.fit85_1 <- auto.arima(data85[, 1], D = 1)
summary(data.fit85_1)
data.fit85_2 <- auto.arima(data85[, 2], D = 1)
summary(data.fit85_2)
data.fit85_3 <- auto.arima(data85[, 3], D = 1)
summary(data.fit85_3)
data.fit85_4 <- auto.arima(data85[, 4], D = 1)
summary(data.fit85_4)
data.fit85_5 <- auto.arima(data85[, 5], D = 1)
summary(data.fit85_5)

model85_1 <- arima(data85[, 1], order = c(1,0,1), 
                   seasonal = list(order = c(2,1,2), period = 12))
acf(data85[, 1])
pacf(data85[, 1]) # Original recommended order 3 has convergence issues, changed to 2
model85_2 <- arima(data85[, 2], order = c(2,0,1), 
                   seasonal = list(order = c(1,1,0), period = 12))
model85_3 <- arima(data85[, 3], order = c(2,1,1), 
                   seasonal = list(order = c(1,1,2), period = 12))
model85_4 <- arima(data85[, 4], order = c(1,0,0), 
                   seasonal = list(order = c(1,1,0), period = 12))
model85_5 <- arima(data85[, 5], order = c(0,0,1), 
                   seasonal = list(order = c(0,1,1), period = 12))

# Residual White Noise Test
Box.test(model85_1$residuals,type="Ljung-Box")
Box.test(model85_2$residuals,type="Ljung-Box")
Box.test(model85_3$residuals,type="Ljung-Box")
Box.test(model85_4$residuals,type="Ljung-Box")
Box.test(model85_5$residuals,type="Ljung-Box") # All are white noisy now.

# Heteroscedasticity Test
# Portmanteau Q Test
for (i in 1:5) print(Box.test(model85_1$residuals,lag=i))
for (i in 1:5) print(Box.test(model85_2$residuals,lag=i))
for (i in 1:5) print(Box.test(model85_3$residuals,lag=i))
for (i in 1:5) print(Box.test(model85_4$residuals,lag=i))
for (i in 1:5) print(Box.test(model85_5$residuals,lag=i))

# Save Residual Sequence 
CAR_85 <- model85_1$residuals
NEA_85 <- model85_2$residuals
NEF_85 <- model85_3$residuals
NPL_85 <- model85_4$residuals
NWP_85 <- model85_5$residuals
Residual_85 <- cbind(CAR_85, NEA_85, NEF_85, NPL_85, NWP_85)
write.table(Residual_85, "Residual_85_result.csv", row.names = FALSE, 
            col.names = TRUE, sep = ",")







