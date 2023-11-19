library(tseries)
library(forecast)

initalT10 <- read.csv("T10.csv")


data10 <- ts(initalT10[,2:7],start = c(2000,1),frequency = 12) 
plot(data10,xlab="Time")
#plot(data10[, 1], xlab = "Time", ylab = "CAR")

# Decompose the time series and view the trend
data10.1 <- decompose(data10[, 1])
plot(data10.1)
plot(data10.1$seasonal, ylab = "seasonal")
data10.2 <- decompose(data10[, 2])
plot(data10.2)
plot(data10.2$seasonal, ylab = "seasonal")
data10.3 <- decompose(data10[, 3])
plot(data10.3)
plot(data10.3$seasonal, ylab = "seasonal")
data10.4 <- decompose(data10[, 4])
plot(data10.4)
plot(data10.4$seasonal, ylab = "seasonal")
data10.5 <- decompose(data10[, 5])
plot(data10.5)
plot(data10.5$seasonal, ylab = "seasonal")
plot(data10.6)
plot(data10.6$seasonal, ylab = "seasonal")


# Stationary test (p<0.05)
adf.test(data10[, 1]) 
adf.test(data10[, 2]) 
adf.test(data10[, 3])
adf.test(data10[, 4])
adf.test(data10[, 5]) 
adf.test(data10[, 6]) 

# Auto.arima (Order Identification)
data10.fit1 <- auto.arima(data10[, 1], D = 1)
summary(data10.fit1)
data10.fit2 <- auto.arima(data10[, 2], D = 1)
summary(data10.fit2)
data10.fit3 <- auto.arima(data10[, 3], D = 1)
summary(data10.fit3)
data10.fit4 <- auto.arima(data10[, 4], D = 1)
summary(data10.fit4)
data10.fit5 <- auto.arima(data10[, 5], D = 1)
summary(data10.fit5)
data10.fit6 <- auto.arima(data10[, 6], D = 1)
summary(data10.fit6)


model101 <- arima(data10[, 1], order = c(0,0,1), 
                seasonal = list(order = c(2,1,2), period = 12))
model102 <- arima(data10[, 2], order = c(1,0,1), 
                seasonal = list(order = c(2,1,0), period = 12))
model103 <- arima(data10[, 3], order = c(1,0,1), 
                seasonal = list(order = c(2,1,2), period = 12))
model104 <- arima(data10[, 4], order = c(0,0,1), 
                seasonal = list(order = c(1,1,0), period = 12))
model105 <- arima(data10[, 5], order = c(0,0,1), 
                seasonal = list(order = c(1,1,0), period = 12))
model106 <- arima(data10[, 6], order = c(1,0,1), 
                  seasonal = list(order = c(2,1,2), period = 12))

# Residual White Noise Test (p>0.05)
Box.test(model101$residuals,type="Ljung-Box")
Box.test(model102$residuals,type="Ljung-Box")
Box.test(model103$residuals,type="Ljung-Box")
Box.test(model104$residuals,type="Ljung-Box")
Box.test(model105$residuals,type="Ljung-Box") # All are white noisy now.
Box.test(model106$residuals,type="Ljung-Box") # All are white noisy now.

# Heteroscedasticity test
library(FinTS)
# Portmanteau Q Test
for (i in 1:5) print(Box.test(model101$residuals,lag=i))
for (i in 1:5) print(Box.test(model102$residuals,lag=i))
for (i in 1:5) print(Box.test(model103$residuals,lag=i))
for (i in 1:5) print(Box.test(model104$residuals,lag=i))
for (i in 1:5) print(Box.test(model105$residuals,lag=i))
for (i in 1:5) print(Box.test(model106$residuals,lag=i))

# Get the CSV file of T10 Residual
CAR_T10 <- model101$residuals
NEA_T10 <- model102$residuals
NEF_T10 <- model103$residuals
NPL_T10 <- model104$residuals
NWP_T10 <- model105$residuals
Total_T10 <- as.data.frame(model106$residuals) ## a dataframe of T10 only

#write.csv(Total_T10,"Total_T10.csv")

#ResidualT10 <- cbind(CAR_T10, NEA_T10, NEF_T10, NPL_T10, NWP_T10)
#write.table(ResidualT10, "ResidualT10_result.csv", row.names = FALSE, 
#           col.names = TRUE, sep = ",")

