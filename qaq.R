install.packages("forecast")
library(zoo)
library(forecast)
library(readr)
OpenAQSanFran <- read_csv("OpenAQSanFran.csv")
View(OpenAQSanFran)

o3col <- which((OpenAQSanFran[,6]=="o3"))
o3 <- OpenAQSanFran[o3col,]
o3sfloc <- which((o3[,1]=="San Francisco"))
o3sf <- o3[o3sfloc,]
o3sf_final <- o3sf[-c(1:10),]


o3sf_ts <- ts(o3sf_final[,7], frequency = 24)
plot(o3sf_ts)
acf(o3sf_ts)

ndiffs(o3sf_ts)
n_o3sf_ts <- diff(o3sf_ts,1)
ndiffs(n_o3sf_ts)
acf(n_o3sf_ts)
o3md210 <- arima(o3sf_ts, order=c(2,1,0), method="ML")

Box.test(o3md210$residuals, type = "Ljung-Box")
qqnorm(o3md210$residuals)
qqline(o3md210$residuals)
o3forecast <- forecast(o3md210, h=24)
plot(o3forecast)
