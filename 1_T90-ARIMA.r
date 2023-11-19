library(tseries)
library(forecast)

initalT90 <- read.csv("T90.csv")


data90 <- ts(initalT90[,2:7],start = c(2000,1),frequency = 12) 
plot(data90,xlab="Time")
#plot_90CAR <- plot(data90[, 1], xlab = "Time", ylab = "CAR")
  
# Decompose the time series and view the trend
data90.1 <- decompose(data90[, 1])
plot(data90.1)
plot(data90.1$seasonal, ylab = "seasonal")
data90.2 <- decompose(data90[, 2])
plot(data90.2)
plot(data90.2$seasonal, ylab = "seasonal")
data90.3 <- decompose(data90[, 3])
plot(data90.3)
plot(data90.3$seasonal, ylab = "seasonal")
data90.4 <- decompose(data90[, 4])
plot(data90.4)
plot(data90.4$seasonal, ylab = "seasonal")
data90.5 <- decompose(data90[, 5])
plot(data90.5)
plot(data90.5$seasonal, ylab = "seasonal")
data90.6 <- decompose(data90[, 6])
plot(data90.6)
plot(data90.6$seasonal, ylab = "seasonal")


# Stationary Test (p<0.05)
adf.test(data90[, 1])
adf.test(data90[, 2])
adf.test(data90[, 3]) 
adf.test(data90[, 4]) 
adf.test(data90[, 5])
adf.test(data90[, 6])

# Auto.arima (Order Identification)
data90.fit1 <- auto.arima(data90[, 1], D = 1)
summary(data90.fit1)
data90.fit2 <- auto.arima(data90[, 2], D = 1)
summary(data90.fit2)
data90.fit3 <- auto.arima(data90[, 3], D = 1)
summary(data90.fit3)
data90.fit4 <- auto.arima(data90[, 4], D = 1)
summary(data90.fit4)
data90.fit5 <- auto.arima(data90[, 5], D = 1)
summary(data90.fit5)
data90.fit6 <- auto.arima(data90[, 6], D = 1)
summary(data90.fit6)


model901 <- arima(data90[, 1], order = c(1,0,0), 
                seasonal = list(order = c(1,1,0), period = 12))
model902 <- arima(data90[, 2], order = c(1,0,0), 
                seasonal = list(order = c(1,1,0), period = 12))
model903 <- arima(data90[, 3], order = c(1,0,0), 
                seasonal = list(order = c(1,1,0), period = 12))
model904 <- arima(data90[, 4], order = c(0,0,1), 
                seasonal = list(order = c(2,1,0), period = 12))
model905 <- arima(data90[, 5], order = c(1,0,1), 
                seasonal = list(order = c(1,1,0), period = 12))
model906 <- arima(data90[, 5], order = c(1,0,1), 
                  seasonal = list(order = c(2,1,2), period = 12))
# Residual White Noise Test (p>0.05)
Box.test(model901$residuals,type="Ljung-Box")
Box.test(model902$residuals,type="Ljung-Box")
Box.test(model903$residuals,type="Ljung-Box")
Box.test(model904$residuals,type="Ljung-Box")
Box.test(model905$residuals,type="Ljung-Box") # All are white noisy now.
Box.test(model906$residuals,type="Ljung-Box") # All are white noisy now.

# Heteroscedasticity test
library(FinTS)
# Portmanteau Q Test
for (i in 1:5) print(Box.test(model901$residuals,lag=i))
for (i in 1:5) print(Box.test(model902$residuals,lag=i))
for (i in 1:5) print(Box.test(model903$residuals,lag=i))
for (i in 1:5) print(Box.test(model904$residuals,lag=i))
for (i in 1:5) print(Box.test(model905$residuals,lag=i))
for (i in 1:5) print(Box.test(model906$residuals,lag=i))

# Get the CSV file of T90 Residual
CAR_T90 <- model901$residuals
NEA_T90 <- model902$residuals
NEF_T90 <- model903$residuals
NPL_T90 <- model904$residuals
NWP_T90 <- model905$residuals
Total_T90 <- as.data.frame(model906$residuals) #note here we create a dataset for national obs only

#write.csv(Total_T90,"Total_T90.csv")


#ResidualT90 <- cbind(CAR_T90, NEA_T90, NEF_T90, NPL_T90, NWP_T90)
#write.table(ResidualT90, "ResidualT90_result.csv", row.names = FALSE, 
#           col.names = TRUE, sep = ",")

