#####################
### MOEXCH Index  ###
#####################

library(rio)
library(fpp3)
library(lmtest)
library(stringr)
library(stargazer)
library(corrplot)
library(forecast)

# Clean the data and transform it into time series format
setwd("/Users/saidmagomedov/")
MOEXCH <- import("Desktop/chemicals/MOEX Chemicals Historical Data.csv")
glimpse(MOEXCH)
MOEXCH$Date <- mdy(MOEXCH$Date)
MOEXCH$Date <- as.Date(MOEXCH$Date, format = "%d-%m-%Y")
MOEXCH <- MOEXCH[, 1:2]
MOEXCH$Price <- as.numeric(gsub(",", "", MOEXCH$Price))
MOEXCH <- MOEXCH[order(MOEXCH$Date), ]
its <- ts(MOEXCH$Price, start = c(year(min(MOEXCH$Date)), month(min(MOEXCH$Date))), frequency = 12)


############################
### Regression analysis  ###
############################

data <- import("Desktop/chemicals/chemical_features_data.xlsx")
data1 <- data[-c(1:15, 23), -c(3:5, 9:10)]
colnames(data1) <- c("Year", "Production", "Investments", "Import", "Export")
glimpse(data1)
data1[, 4:5] <- sapply(data1[, 4:5], str_trim)
data1[, 4:5] <- sapply(data1[, 4:5], function(x) as.numeric(gsub(",", ".", x)))

MOEXCH1 <- MOEXCH[MOEXCH$Date >= "2015-01-01" & MOEXCH$Date <= "2022-01-01", ]
moexch_monthly <- diff(log(MOEXCH1$Price))
moexch <- numeric(7)

# Define yearly returns as average of monthly ones
for (i in 1:7) {
  start <- (i-1)*12 + 1
  end <- i*12
  moexch[i] <- mean(moexch_monthly[start:end])
}
df <- data.frame(data1, moexch)
df <- df[, c(1, 6, 2:5)]

# Estimate OLS model and conduct Durbin-Watson and Ramsey RESET tests
ols <- lm(moexch ~ Production + Investments + Import + Export, df)
summary(ols)
dwtest(ols)
resettest(ols, power = 2, type = "fitted")

# Display a beautiful table with the results
stargazer(ols)

# Correlation matrix of our variables
mcor <- cor(df[, -1])
round(mcor, digits = 2)
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 50, addCoef.col = "black")


#############################
### ETS and SARIMA models ###
#############################

# Estimate the best ETS model and plot the graph of original and predicted values
ets_auto <- ets(its)
summary(ets_auto)
forecast_ets <- forecast(ets_auto, h = 19)
print(forecast_ets)
autoplot(forecast_ets, predict_interval = TRUE) +
  autolayer(its, series = "Original", lwd = 1.3) +
  autolayer(forecast_ets$mean, series = "Forecast",lwd = 1.3) +
  xlab("Date") + ylab("Value") +
  ggtitle("ETS(M,A,N) Forecast") +
  theme_bw()

# Do the same for SARIMA model
arima_auto <- auto.arima(its)
summary(arima_auto)
forecast_arima <- forecast(arima_auto, h = 19, level = c(80, 95))
print(forecast_arima)
autoplot(forecast_arima, predict_interval = TRUE) +
  autolayer(its, series = "Original", lwd = 1.3) +
  autolayer(forecast_arima$mean, series = "Forecast", lwd = 1.3) +
  xlab("Date") + ylab("Value") +
  ggtitle("SARIMA(0,1,1)(0,0,2) Forecast") +
  theme_bw() 

fc_df <- data.frame(forecast_arima)
export(fc_df, "Desktop/chemicals/fcdf.xlsx")
