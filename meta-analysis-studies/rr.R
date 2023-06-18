getwd()
setwd("/Users/zhouyue/Downloads")

library(xts)
library(lmtest)
library(tidyverse)
library(urca)


data <- read.csv("NFLX.csv")
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
data %>% glimpse()
data <- xts(data[, -1],order.by = data$Date,frequency = 12)
Adj.Close <- data$Adj.Close
plot(Adj.Close)

adf_test <- ur.df(Adj.Close$Adj.Close, type = "none", lags = 0)
summary(adf_test)

par(mfrow = c(2, 1)) 
acf(Adj.Close$Adj.Close,lag.max = 36, ylim = c(-0.1, 0.1),lwd = 5,col = "dark green",na.action = na.pass)  
pacf(Adj.Close$Adj.Close,lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1)) 

dadj <- na.exclude(diff.xts(Adj.Close$Adj.Close))
adf_test_dadj <- ur.df(dadj, type = "none", lags = 0)
summary(adf_test_dadj)

par(mfrow = c(2, 1)) 
acf(dadj,lag.max = 36, ylim = c(-0.1, 0.1),lwd = 5,col = "dark green",na.action = na.pass)  
pacf(dadj,lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1)) 



library(forecast)
auto.arima(Adj.Close)


cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
arima212 <- Arima(cdata.shortx1$Adj.Close,order = c(2, 1, 2))
arima212
coeftest(arima212)
summary(arima212)
plot(resid(arima212))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima212 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
acf(resid(arima212),lag.max = 36,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima212),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima212), type = "Ljung-Box", lag = 4)


arima1133 <- Arima(cdata.shortx1$Adj.Close,order = c(1, 1,33))
arima1133
coeftest(arima1133)
summary(arima1133)
plot(resid(arima1133))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima1133 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
acf(resid(arima1133),lag.max = 36,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima1133),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima1133), type = "Ljung-Box", lag = 4)



arima1233 <- Arima(cdata.shortx1$Adj.Close,order = c(1, 2,33))
arima1233
coeftest(arima1233)
summary(arima1233)
plot(resid(arima1233))
tibble(date = index(cdata.shortx1$Adj.Close),resid = arima1233 %>% resid() %>% as.numeric()
) %>%
  ggplot(aes(date, resid)) +
  geom_line(col = "royalblue3") +theme_bw()


par(mfrow = c(2, 1)) 
acf(resid(arima1233),lag.max = 36,ylim = c(-0.1, 0.1),lwd = 5, col = "dark green",na.action = na.pass)
pacf(resid(arima1233),lag.max = 36,lwd = 5, col = "dark green",na.action = na.pass)
par(mfrow = c(1, 1))
Box.test(resid(arima1233), type = "Ljung-Box", lag = 4)


forecasts <- forecast(arima212, h = 627) 
forecasts
forecasts$mean
class(forecasts$mean)
as.numeric(forecasts$mean)
forecasts$lower
forecasts$upper
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))

forecasts_data 



cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1

tail(cdata.shortx1)
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))

cdata3 <- merge(Adj.Close,forecasts_xts)

head(cdata3)
tail(cdata3, n =627)
plot(cdata3)

forecasts <- forecast(arima1133, h = 627) 
forecasts
forecasts$mean
class(forecasts$mean)
as.numeric(forecasts$mean)
forecasts$lower
forecasts$upper
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))

forecasts_data 



cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1

tail(cdata.shortx1)
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))

cdata3 <- merge(Adj.Close,forecasts_xts)

head(cdata3)
tail(cdata3, n =627)
plot(cdata3)


forecasts <- forecast(arima1233, h = 627) 
forecasts
forecasts$mean
class(forecasts$mean)
as.numeric(forecasts$mean)
forecasts$lower
forecasts$upper
forecasts_data <- data.frame(f_mean  = as.numeric(forecasts$mean),
                             f_lower = as.numeric(forecasts$lower[, 2]),
                             f_upper = as.numeric(forecasts$upper[, 2]))

forecasts_data 



cdata.shortx1 <- Adj.Close$Adj.Close["/20171007", ]
cdata.shortx1

tail(cdata.shortx1)
cdata.shortx2 <-Adj.Close$Adj.Close["20171007/", ]
forecasts_xts <- xts(forecasts_data,order.by =index(cdata.shortx2))

cdata3 <- merge(Adj.Close,forecasts_xts)

head(cdata3)
tail(cdata3, n =627)
plot(cdata3)



