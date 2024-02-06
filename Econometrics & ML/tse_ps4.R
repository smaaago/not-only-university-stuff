library(rio)
library(stats)
library(urca)
library(rugarch)
library(strucchange)
library(vars)
library(cointReg)

setwd("/Users/saidmagomedov/")
d <- import("Desktop/METRICS/Semester 2/ps4_quotes.xls")
S <- d[, 1]
P <- d[, 2]

#######################
###   Paragraph a   ###
#######################
# Subparagraph i
logS <- log(S)
rS <- diff(logS)
acf_logS <- acf(logS, lag.max = 50, main = "Logarithmic Exchange Rate", col = "darkmagenta")
acf_logS$acf
acf_rS <- acf(rS, lag.max = 50, main = "Exchange Rate Returns", col = "darkmagenta")
acf_rS$acf

# ACF for for the daily logarithmic exchange rates decreases very-very slowly
# This is most likely because the series is non-stationary (presence of the unit root)
# After taking the first differences the ACF changes as expected
# It cuts off lag 1 and corresponds to a stationary time series
# Result: daily logarithmic exchange rate series - non-stationary, their returns - stationary

# Let's plot the series of logarithmic exchange rate and their returns 
# in order to analyze the presence trend and determine the specification of ADF test 
plot(logS, type = "l", xlab = "", ylab = "", main = "Logarithmic Exchange Rate", col = "darkmagenta")
plot(rS, type = "l", xlab = "", ylab = "", main = "Exchange Rate Returns", col = "darkmagenta")

# There is almost no trend in the first series. It seems like it has a decreasing slope
# But let us call it not significant enough to include trend into tests. Let it just constant be
# Exchange rate returns fluctuate around zero, so there is no evidence to include either constant or trend into testing procedure
summary(ur.df(logS, type = "drift", lags = 50, selectlags = "BIC"))
summary(ur.pp(logS, lags = "long", model = "constant"))
PP.test(logS)
summary(ur.kpss(logS, type = "mu"))
# ADF: test-statistics are -1.1556 and 0.7594 - less in absolute values than critical values for all reasonable significance levels
# That is why H0 is not rejected and series logS is not stationary
# PP: I do not know why it (ur.pp) returns test-statistic bigger than critical, but PP.test does it normally (p-value = 0.4). H0 of unit root is not rejected
# KPSS: here null hypothesis is that TS is stationary unlike the ADF test
# Test-statistic is 27.1805 and much greater than any critical value => null rejected and series is not stationary

summary(ur.df(rS, type = "drift", lags = 50, selectlags = "BIC"))
summary(ur.pp(rS, model = "constant"))
summary(ur.kpss(rS, type = "mu"))
# ADF: super significantly stationary since test-statistics are -40.8048 and 832.5163 - very large
# PP: analogically to ADF, test-statistic is -3239.949 - extremely high
# KPSS: test-statistic = 0.1557 less than any critical value => null not rejected, i.e. series is stationary

# Summary: our previously noted conclusions about stationarity of both series were confirmed by the results of all tests


# Subparagraph ii
Box.test(rS, 20, "Ljung-Box")
Box.test(rS, 40, "Ljung-Box")
Box.test(rS, 60, "Ljung-Box")
# P-value is rising with number of lags: 0.0005 => 0.0075 => 0.0349
# H0 of independence in TS, i.e. no autocorrelation is rejected at 5% 


# Subparagraph iii
my_arima <- auto.arima(rS)
summary(my_arima)
my_arima$aic 
my_arima$bic
# The most appropriate model following the BJ-procedure is ARIMA(0,0,1) with zero mean
# AIC = -25913.91, BIC = -25901.58


# Subparagraph iv
# Store squared residuals from the estimated model, plot and 
res2 <- my_arima$residuals ^ 2
plot(res2, type = "l", xlab = "", ylab = "", main = "Squared Residuals", col = "darkmagenta")
acf(res2)
# There is a strong correlation on the first lag and less significant but still out of 5% CI on all other lags
# Looking at the plot of res2 we can suppose that there is evidence of conditional heteroskedasticity 
# Since the residuals' variance is not obviously constant in time

# GARCH(1,1)
garchspec11 <- ugarchspec(
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1))
)
my_garch11 <- ugarchfit(garchspec11, rS)
my_garch11
ic_garch11 <- infocriteria(my_garch11)

# GARCH(1,2)
garchspec12 <- ugarchspec(
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 2))
)
my_garch12 <- ugarchfit(garchspec12, rS, solver ='hybrid')
my_garch12
ic_garch12 <- infocriteria(my_garch12)

# GARCH(2,1)
garchspec21 <- ugarchspec(
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(2, 1))
)
my_garch21 <- ugarchfit(garchspec21, rS)
my_garch21
ic_garch21 <- infocriteria(my_garch21)

# GARCH(2,2)
garchspec22 <- ugarchspec(
  mean.model = list(armaOrder = c(0, 1),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(2, 2))
)
my_garch22 <- ugarchfit(garchspec22, rS)
my_garch22
ic_garch22 <- infocriteria(my_garch22)

# GARCH(p,q) model selection
ic_garch <- data.frame(ic_garch11, ic_garch12, ic_garch21, ic_garch22)
colnames(ic_garch) <- c("GARCH(1,1)", "GARCH(1,2)", "GARCH(2,1)", "GARCH(2,2)")
# GARCH(1,1) produces the smallest BIC equal to -7.477 => it is the best model for this data


#######################
###   Paragraph b   ###
#######################
# Subparagraph i
logP <- log(P)
rP <- diff(logP)
acf_logP <- acf(logP, lag.max = 50, main = "Logarithmic S&P500 Prices", col = "darkmagenta")
acf_logP$acf
acf_rP <- acf(rP, lag.max = 50, main = "S&P500 Returns", col = "darkmagenta")
acf_rP$acf

# ACF for for the logarithmic S&P500 prices decreases very-very slowly
# This is most likely because the series is non-stationary (presence of the unit root)
# After taking the first differences the ACF changes as expected
# It cuts off lag 1 and corresponds to a stationary time series
# Result: logarithmic S&P500 prices series - non-stationary, their returns - stationary

# Let's plot the series of logarithmic S&P500 prices and their returns 
# in order to analyze the presence trend and determine the specification of ADF test 
plot(logP, type = "l", xlab = "", ylab = "", main = "Logarithmic S&P500 Prices", col = "darkmagenta")
plot(rP, type = "l", xlab = "", ylab = "", main = "S&P500 Returns", col = "darkmagenta")

# There is almost no trend in the first series. It jumps and falls remaining on average on a same level
# But let us call it not significant enough to include trend into tests. Let it just constant be
# S&P500 Returns fluctuate around zero, so there is no evidence to include either constant or trend into testing procedure
summary(ur.df(logP, type = "drift", lags = 50, selectlags = "BIC"))
summary(ur.pp(logP, lags = "long", model = "constant"))
PP.test(logP)
summary(ur.kpss(logP, type = "mu"))
# ADF: test-statistics are -2.273 and 2.601 - less in absolute values than critical values for all printed significance levels
# That is why H0 is not rejected and series logP is not stationary
# PP: I do not know why it (ur.pp) returns test-statistic bigger than critical, but PP.test does it normally (p-value = 0.4). H0 of unit root is not rejected
# KPSS: here null hypothesis is that TS is stationary unlike the ADF test
# Test-statistic is 1.469 and it is greater than any critical value => null rejected and series is not stationary

summary(ur.df(rP, type = "drift", lags = 50, selectlags = "BIC"))
summary(ur.pp(rP, model = "constant"))
summary(ur.kpss(rP, type = "mu"))
# ADF: super significantly stationary since test-statistics are -45.66 and 1043 - super large
# PP: analogically to ADF, test-statistic is -3546 - extremely high
# KPSS: test-statistic = 0.0822 less than any critical value => null not rejected, i.e. series is stationary

# Summary: our previously noted conclusions about stationarity of both series were confirmed by the results of all tests


# Subparagraph ii
Box.test(rP, 20, "Ljung-Box")
Box.test(rP, 40, "Ljung-Box")
Box.test(rP, 60, "Ljung-Box")
# P-value decreases (almost) with number of lags: 2e-10 => 8e-14 => 9e-14
# H0 of independence in TS, i.e. no autocorrelation is rejected at any significance level


# Subparagraph iii
my_arimaP <- auto.arima(rP)
summary(my_arimaP)
my_arimaP$aic 
my_arimaP$bic
# The most appropriate model following the BJ-procedure is ARIMA(0,0,2) with zero mean
# AIC = -20261, BIC = -20243


# Subparagraph iv
# Store squared residuals from the estimated model, plot and 
res2P <- my_arimaP$residuals ^ 2
plot(res2P, type = "l", xlab = "", ylab = "", main = "Squared Residuals", col = "darkmagenta")
acf(res2P)
# There is a strong correlation on the first lag and less significant but still far out of 5% CI on all other lags
# Looking at the plot of res2 we can suppose that there is evidence of conditional heteroskedasticity 
# Since the residuals' variance is not obviously constant in time

# GARCH(1,1)
garchspec11P <- ugarchspec(
  mean.model = list(armaOrder = c(0, 2),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1))
)
my_garch11P <- ugarchfit(garchspec11P, rP)
my_garch11P
ic_garch11P <- infocriteria(my_garch11P)

# GARCH(1,2)
garchspec12P <- ugarchspec(
  mean.model = list(armaOrder = c(0, 2),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 2))
)
my_garch12P <- ugarchfit(garchspec12P, rP)
my_garch12P
ic_garch12P <- infocriteria(my_garch12P)

# GARCH(2,1)
garchspec21P <- ugarchspec(
  mean.model = list(armaOrder = c(0, 2),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(2, 1))
)
my_garch21P <- ugarchfit(garchspec21P, rP)
my_garch21P
ic_garch21P <- infocriteria(my_garch21P)

# GARCH(2,2)
garchspec22P <- ugarchspec(
  mean.model = list(armaOrder = c(0, 2),
                    include.mean = FALSE),
  variance.model = list(model = "sGARCH",
                        garchOrder = c(2, 2))
)
my_garch22P <- ugarchfit(garchspec22P, rP)
my_garch22P
ic_garch22P <- infocriteria(my_garch22P)

# GARCH(p,q) model selection
# Let's construct a table with information criteria corresponding to each GARCH model estimated
ic_garchP <- data.frame(ic_garch11P, ic_garch12P, ic_garch21P, ic_garch22P)
colnames(ic_garchP) <- c("GARCH(1,1)", "GARCH(1,2)", "GARCH(2, 1)", "GARCH(2,2)")
# GARCH(2,1) produces the smallest BIC equal to -6.1605 => it is the best model for this data


#######################
###   Paragraph с   ###
#######################
# Structural break in mean
Fs <- Fstats(logS ~ 1, from = 0.15, to = 0.85)
sctest(Fs)
breakpoints(Fs)
# Estimated breakdate in logarithmic exchange rates is at observation number 1131
FsP <- Fstats(rP ~ 1, from = 0.15, to = 0.85)
sctest(FsP)
breakpoints(FsP)
# Estimated breakdate in log returns on S&P500 is at observation number 2806

# Structural break in variances
Fsvar <- Fstats(res2 ~ 1, from = 0.15, to = 0.85)
sctest(Fsvar)
breakpoints(Fsvar)
# Estimated breakdate in logarithmic exchange rates' variances is at observation number 1423
FsvarP <- Fstats(res2P ~ 1, from = 0.15, to = 0.85)
sctest(FsvarP)
breakpoints(FsvarP)
# Estimated breakdate in variances of log returns on S&P500 is at observation number 2678


#######################
###   Paragraph d   ###
#######################
# Create a matrix of our variables
Y <- cbind(logS, logP)
VARselect(Y)
# AIC recommends 3, HQ and BIC - 2, let's choose 2 according to BIC
my_var <- VAR(Y, 2)
summary(my_var)
# Looking at the correlation matrix of residuals we can notice that they are almost uncorrelated (estimate = -0.0293)
# It is a good result

# Check for Granger-causality
causality(my_var, "logS")$Granger
causality(my_var, "logP")$Granger
# Both H0 are not rejected at a very high significance level of 80%
# Hence, these variables do not Granger-cause each other


#######################
###   Paragraph e   ###
#######################
# Subparagraph i
# Personally I do not think these series are going to be cointegrated
# They are not directly (enough strongly) connected to each other to represent cointegration
# There are plenty of other factors that affect S&P500 even more significantly than EUR/USD exchange rate

# Subparagraph ii
# Perform Johansen test for cointegration using maximum eigenvalue specification 
summary(ca.jo(Y, type = "eigen", K = 2))
# Both hypotheses are not rejected at any printed significance level => no evidence of cointegration

# Subparagraph iii
cointRegD(logS, logP, n.lag = 30, n.lead = 30, kmax = "k4")
# Cointegration coefficient estimated by the dynamic OLS is equal to -21.0 with p-value = 0.087
# So it is significant at the 10% level
