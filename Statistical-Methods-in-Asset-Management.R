## Statistical-Methods-in-Asset-Management

library(quantmod)
library(Quandl)
library(PerformanceAnalytics)
library(dplyr)

#-----------Benchmark-----------#
ticker = '^GSPC'
#Get Data
stock <- getSymbols.yahoo(ticker, from="2013-01-01", to="2019-11-02" ,periodicity = "monthly", auto.assign=FALSE)[,4]
#Remove Dates With No Prices
stock <- stock[apply(stock,1,function(x) all(!is.na(x))),]
#Calculate Returns for Assets: Monthly
bench_ret <- na.omit(ROC(stock, type="discrete"))
#Rename Columns
colnames(bench_ret) <- "SP500"

#-----------15 assets-----------#

#Get Data of 15 assets 
symbol = c('AAPL', 'AAL', 'AEP', 'F', 'FB', 'JPM', 'KO', 'LMT', 'M', 'MSFT', 'MU', 'NKE', 'SPG', 'T', 'TIF')
for(smb in symbol){
  getSymbols(smb, src='yahoo', from = "2013-01-01", to = '2019-11-02', periodicity = "monthly")
}

#Combine them to one dataframe
p <- cbind.data.frame(AAPL[,4], AAL[,4], AEP[,4], F[,4], FB[,4], JPM[,4], KO[,4], LMT[,4],
                      M[,4], MSFT[,4], MU[,4], NKE[,4], SPG[,4], T[,4], TIF[,4])
#Rename Columns
colnames(p) <- symbol

#Calculate Returns for Assets: Monthly RoC
portfolioReturns <- na.omit(ROC(p, type="discrete"))
porReturn <- portfolioReturns
library(data.table)
setDT(porReturn, keep.rownames = TRUE)[]
porReturn$rn <- factor(porReturn$rn) 
porReturn <- as.data.frame(porReturn)


#-----------Risk Free Data-----------#
rf_data = read.csv('Risk free.csv',row.names = 1)
rf <- (rf_data$TB3MS/100)/12


#-----------sample statistics-----------#

#sample statistics of asset prices (means, standard deviation, skewness coefficients, Kurtosis Coefficients and beta of each asset)
library(psych)
describ_stats_stock <- psych::describe(p)
describ_stats_stock1 <- as.data.frame(describ_stats_stock)
price_stats_stock <- describ_stats_stock1[,c(1,2,3,4,11,12)]

#sample statistics of asset returns (means, standard deviation, skewness coefficients, Kurtosis Coefficients and beta of each asset)
library(psych)
describ_stats_stock <- psych::describe(portfolioReturns)
describ_stats_stock1 <- as.data.frame(describ_stats_stock)
return_stats_stock <- describ_stats_stock1[,c(1,2,3,4,11,12)]

#beta of each asset 
beta_stats <- apply(porReturn[-1],2,function(x) {summary(lm(x~bench_ret))} )
beta_stats


#-----------generate plots-----------#

#-----------monthly return plots-----------#

#generate multiple monthly returns plots of different stocks simultaneously
 ##one-time solved

n_col <- length(names(portfolioReturns))
#n_col=15
plot.ts(portfolioReturns[,1:8],axes=F)
plot.ts(portfolioReturns[,9:15],axes=F)

# for (i in 2:16) {
#   plot(porReturn$rn,porReturn[,i],main="Return plot of AAPL")
# }


#generate monthly returns plot one at a time

SP500 <- as.matrix(bench_ret)

par(mfrow=c(3,5))
plot(porReturn$rn,porReturn$AAPL,type='l',main="Return plot of AAPL")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$AAL,main="Return plot of AAL")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$AEP,main="Return plot of AEP")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$F,main="Return plot of F")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$FB,main="Return plot of FB")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$JPM,main="Return plot of JPM")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$KO,main="Return plot of KO")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$LMT,main="Return plot of LMT")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$M,main="Return plot of M")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$MSFT,main="Return plot of MSFT")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$MU,main="Return plot of MU")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$NKE,main="Return plot of NKE")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$SPG,main="Return plot of SPG")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$T,main="Return plot of T")
lines(x=porReturn$rn,y=SP500,col="red")
plot(porReturn$rn,porReturn$TIF,main="Return plot of TIF")
lines(x=porReturn$rn,y=SP500,col="red")

#-----------monthly stock price plots-----------#

#generate multiple monthly stock price plots of different stocks 
p2 <- p
setDT(p2, keep.rownames = TRUE)[]
p2$rn <- factor(p2$rn) 
p2 <- as.data.frame(p2)
SP500 <- as.matrix(bench_ret)

par(mfrow=c(3,5))
plot(p2$rn,p2$AAPL,main="Stock price plot of AAPL")
plot(p2$rn,p2$AAL,main="Stock price plot of AAL")
plot(p2$rn,p2$AEP,main="Stock price plot of AEP")
plot(p2$rn,p2$F,main="Stock price plot of F")
plot(p2$rn,p2$FB,main="Stock price plot of FB")
plot(p2$rn,p2$JPM,main="Stock price plot of JPM")
plot(p2$rn,p2$KO,main="Stock price plot of KO")
plot(p2$rn,p2$LMT,main="Stock price plot of LMT")
plot(p2$rn,p2$M,main="Stock price plot of M")
plot(p2$rn,p2$MSFT,main="Stock price plot of MSFT")
plot(p2$rn,p2$MU,main="Stock price plot of MU")
plot(p2$rn,p2$NKE,main="Stock price plot of NKE")
plot(p2$rn,p2$SPG,main="Stock price plot of SPG")
plot(p2$rn,p2$T,main="Stock price plot of T")
plot(p2$rn,p2$TIF,main="Stock price plot of TIF")


#-----------equity curve-----------#

#equity curve for SP500
par(mfrow=c(1,1))
rn2 <- as.data.frame(porReturn$rn)
SP500 <- as.matrix(bench_ret)
#plot(x=rn2,y=1*cumprod(1+SP500),main="SP500 Equity Curve")
test = 1*cumprod(1+SP500)
plot(x=rn2,y=test,main="SP500 Equity Curve")

#equity curve for assets
par(mfrow=c(1,1))
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$AAPL),main="AAPL Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$AAL),main="AAL Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$AEP),main="AEP Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$F),main="F Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$FB),main="FB Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$JPM),main="JPM Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$KO),main="KO Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$LMT),main="LMT Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$M),main="M Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$MSFT),main="MSFT Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$MU),main="MU Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$NKE),main="NKE Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$SPG),main="SPG Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$T),main="T Equity Curve")
lines(x=porReturn$rn,y=test,col="red")
plot(x=porReturn$rn,y=1*cumprod(1+porReturn$TIF),main="TIF Equity Curve")
lines(x=porReturn$rn,y=test,col="red")



#-----------histagram-----------#

#plot histagram for every asset at one time

  # library(ggplot2)
  # library(reshape2)
  # ggplot(melt(porReturn[,2:16]),aes(x=value)) + geom_histogram() + facet_wrap(~variable)

par(mfrow=c(3,5))

symbol2 <- as.data.frame(symbol)

loop.vector.hist <- 2:16

for (i in loop.vector.hist) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- porReturn[,i]
  
  # Plot histagram of x
  hist(x,main = paste("Stock", symbol2[i-1,],sep = ' '),xlab = "Return Rate")
}

#plot histagram for SP500
hist(bench_ret,main = "S&P500",xlab = "Return Rate")


#-----------boxplot-----------#

#plot boxplot for every asset at one time
par(mfrow=c(3,5))

symbol2 <- as.data.frame(symbol)

loop.vector.boxplot <- 2:16

for (i in loop.vector.boxplot) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- porReturn[,i]
  
  # Plot boxplot of x

  boxplot(x,
          main=paste("Stock", symbol2[i-1,],sep = ' '),
          xlab='return rate',
          horizontal=TRUE)
}

#plot boxplot for SP500
boxplot(SP500,xlab='return rate',horizontal=TRUE)


#-----------qqplot-----------#

#plot qqplot for every asset at one time

par(mfrow=c(3,5))

loop.vector.qqplot <- 2:16

for (i in loop.vector.qqplot) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- porReturn[,i]
  
  # Plot qqplot of x
  
  
  qqnorm(x,
         main=paste("Stock", symbol2[i-1,],sep = ' '),
         xlab="Theoretical Quantiles",
         ylab="Sample Quantiles")
  
  qqline(x)
  
}

#plot qqplot for SP500
qqnorm(bench_ret,
       main="S&P500",
       xlab="Theoretical Quantiles",
       ylab="Sample Quantiles")

qqline(SP500)


#-----------test for stationarity-----------#

#acf plot

par(mfrow=c(3,5))

loop.vector.stationarity <- 2:16

for (i in loop.vector.stationarity) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- porReturn[,i]
  
  # Plot qqplot of x
  
  acf(x,lag.max = length(x),xlab = "lag #", ylab = 'ACF', main=paste("Stock", symbol2[i-1,],sep = ' '))
}


#-----------fit different distributions-----------#

#fit different distributions to your data, which one fits better?
library(fitdistrplus)
library(fGarch)
library(MASS)
t <- porReturn[,2] 
n = length(t)
#porReturn is the dataframe that contain all return rate info. 
#about the 15 stocks we chose, each column represents
#the return rate info of one stock


#-----------normal distribution fits stock AAPL better-----------#
AIC_norm <- summary(fitdist(porReturn[,2],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,2],"cauchy"))

## t
t <- porReturn[,2] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------normal distribution fits stock AAL better-----------#
AIC_norm <- summary(fitdist(porReturn[,3],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,3],"cauchy"))

## t
t <- porReturn[,3] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t



#-----------skewed t distribution fits stock AEP better-----------#
AIC_norm <- summary(fitdist(porReturn[,4],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,4],"cauchy"))

## t
t <- porReturn[,4] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#----------t distribution fits stock F better-----------#
AIC_norm <- summary(fitdist(porReturn[,5],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,5],"cauchy"))

## t
t <- porReturn[,5] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------t distribution fits stock FB better-----------#
AIC_norm <- summary(fitdist(porReturn[,6],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,6],"cauchy"))

## t
t <- porReturn[,6] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------normal distribution fits stock JPM better-----------#
AIC_norm <- summary(fitdist(porReturn[,7],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,7],"cauchy"))

## t
t <- porReturn[,7] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------skewed t distribution fits stock KO better-----------#
AIC_norm <- summary(fitdist(porReturn[,8],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,8],"cauchy"))

## t
t <- porReturn[,8] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------t distribution fits stock LMT better-----------#
AIC_norm <- summary(fitdist(porReturn[,9],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,9],"cauchy"))

## t
t <- porReturn[,9] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------skewed t distribution fits stock M better-----------#
AIC_norm <- summary(fitdist(porReturn[,10],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,10],"cauchy"))

## t
t <- porReturn[,10] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------t distribution fits stock MSFT better-----------#
AIC_norm <- summary(fitdist(porReturn[,11],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,11],"cauchy"))

## t
t <- porReturn[,11] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------normal distribution fits stock MU better-----------#
AIC_norm <- summary(fitdist(porReturn[,12],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,12],"cauchy"))

## t
t <- porReturn[,12] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t


#-----------normal distribution fits stock NKE better-----------#
AIC_norm <- summary(fitdist(porReturn[,13],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,1],"cauchy"))

## t
t <- porReturn[,13] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------normal distribution fits stock SPG better-----------#
AIC_norm <- summary(fitdist(porReturn[,14],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,14],"cauchy"))

## t
t <- porReturn[,14] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------normal distribution fits stock T better-----------#
AIC_norm <- summary(fitdist(porReturn[,15],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,15],"cauchy"))

## t
t <- porReturn[,15] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t

#-----------t distribution fits stock TIF better-----------#
AIC_norm <- summary(fitdist(porReturn[,16],"norm"))
AIC_cauchy <- summary(fitdist(porReturn[,16],"cauchy"))

## t
t <- porReturn[,16] 
mymodel = stdFit(t)
mymodel$par
logl = -sum(log(dstd(t, mean=mymodel$par[1], sd=mymodel$par[2], nu=mymodel$par[3])))
AIC_t = 2*logl + 2 * 3

## skewed t
mymodel = sstdFit(t)
mymodel$estimate
logl = -sum(log(dsstd(t, mean=mymodel$estimate[1], sd=mymodel$estimate[2], 
                      nu=mymodel$estimate[3],xi=mymodel$estimate[4])))
AIC_sk_t = 2*logl + 2 * 4

AIC_norm$aic
AIC_cauchy$aic
AIC_t
AIC_sk_t


#-----------sharpe's slope-----------#

#compute sharpe's slope for each asset
rf1 <- as.data.frame(rf)
sharpe1 <- matrix(0, nrow = 15, ncol = 1)

for (i in 2:16) {
  x <- porReturn[,i]
  sharpe1[i-1,] <- (mean(x)-mean(rf1$rf))/sd(x)
}

sharpe1
# MSFT has the largest sharpe's slope which is 0.25232726


#-----------convert monthly data sample statistics to year sample statistics-----------#

#Convert the monthly sample means into annual estimates

annual.mean <- matrix(0, nrow = 15, ncol = 1)
annual.sd <- matrix(0, nrow = 15, ncol = 1)

for (i in 2:16) {
  x <- porReturn[,i]
  annual.mean[i-1,] <- (mean(x)*12)
  annual.sd[i-1,] <- (sd(x)*sqrt(12))
}


#table - annual mean and annual sd for each stock
annual.mean.sd = data.frame(symbol,annual.mean,annual.sd)

#-----------scatter plots, cor plots, cov matrix-----------#

library(GGally)
return.each.asset <- porReturn[,-1]
ggcorr(return.each.asset)

par(mfrow=c(1,1))
library(corrplot)
library(RColorBrewer)
cor_mat = cor(return.each.asset, method = "spearman")
corrplot(cor_mat, type="upper", 
         col=brewer.pal(n=8, name="RdBu"))

summary(cor_mat)

#Construct pairwise scatter plots between your assets returns
pairs(return.each.asset, pch = 19,  cex = 0.5,lower.panel=NULL)

#compute the sample correlation and covariance matrix of the returns on your assets
cov.matrix <- cov(return.each.asset)
cor.matrix <- cor(return.each.asset, method = "spearman")



#------------------------------------------------cutting line 1 ----------------------------------------------------------------------------#\



# Functions for portfolio analysis

# Functions:
#	1. efficient.portfolio			compute minimum variance portfolio
#							                subject to target return
#	2. globalMin.portfolio			compute global minimum variance portfolio
#	3. tangency.portfolio			  compute tangency portfolio
#	4. efficient.frontier			  Compute efficient frontier of risky assets with Markowitz approach
#	5. plot.Portfolio					  Plot method of class portfolio
#

#-----------------------------Helper Function--------------------------------------#\

# @param er \samp{N x 1} vector of expected returns
# @param cov.mat \samp{N x N} return covariance matrix
# @param nport scalar, number of efficient portfolios to compute
# @param alpha.min minimum value of \samp{alpha}, default is \samp{-.5}
# @param alpha.max maximum value of \samp{alpha}, default is \samp{1.5}
# @param shorts logical, if \samp{TRUE} then short sales (negative portfolio weights)
# are allowed. If \samp{FALSE} then no asset is allowed to be sold short
# 
# @return 
#  \item{call}{captures function call}
#  \item{er}{\samp{nport x 1} vector of expected returns of efficient porfolios}
#  \item{sd}{\samp{nport x 1} vector of standard deviations of efficient portfolios}
#  \item{weights}{\samp{nport x N} matrix of weights of efficient portfolios}
# 
#  
#
# Please run these helper function for porfolio analysis, Then jump to part 3


#-----------------------------1. efficient.portfolio--------------------------------------#
efficient.portfolio <-
  function(er, cov.mat, target.return, shorts=TRUE)
  {
    call <- match.call()
    
    #
    # check for valid inputs
    #
    asset.names <- names(er)
    er <- as.vector(er) # assign names if none exist
    N <- length(er)
    cov.mat <- as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semidefinite
    
    #
    # compute efficient portfolio
    #
    if(shorts==TRUE){
      ones <- rep(1, N)
      top <- cbind(2*cov.mat, er, ones)
      bot <- cbind(rbind(er, ones), matrix(0,2,2))
      A <- rbind(top, bot)
      b.target <- as.matrix(c(rep(0, N), target.return, 1))
      x <- solve(A, b.target)
      w <- x[1:N]
    } else if(shorts==FALSE){
      Dmat <- 2*cov.mat
      dvec <- rep.int(0, N)
      Amat <- cbind(rep(1,N), er, diag(1,N))
      bvec <- c(1, target.return, rep(0,N))
      result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=2)
      w <- round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    #
    # compute portfolio expected returns and variance
    #
    names(w) <- asset.names
    er.port <- crossprod(er,w)
    sd.port <- sqrt(w %*% cov.mat %*% w)
    ans <- list("call" = call,
                "er" = as.vector(er.port),
                "sd" = as.vector(sd.port),
                "weights" = w) 
    class(ans) <- "portfolio"
    return(ans)
  }
#-----------------------------2. globalMin.portfolio-------------------------------#

globalMin.portfolio <-
  function(er, cov.mat, shorts=TRUE)
  {
    call <- match.call()
    
    #
    # check for valid inputs
    #
    asset.names <- names(er)
    er <- as.vector(er) # assign names if none exist
    cov.mat <- as.matrix(cov.mat)
    N <- length(er)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semi-definite
    
    #
    # compute global minimum portfolio
    #
    if(shorts==TRUE){
      cov.mat.inv <- solve(cov.mat)
      one.vec <- rep(1,N)
      w.gmin <- rowSums(cov.mat.inv) / sum(cov.mat.inv)
      w.gmin <- as.vector(w.gmin)
    } else if(shorts==FALSE){
      Dmat <- 2*cov.mat
      dvec <- rep.int(0, N)
      Amat <- cbind(rep(1,N), diag(1,N))
      bvec <- c(1, rep(0,N))
      result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.gmin <- round(result$solution, 6)
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(w.gmin) <- asset.names
    er.gmin <- crossprod(w.gmin,er)
    sd.gmin <- sqrt(t(w.gmin) %*% cov.mat %*% w.gmin)
    gmin.port <- list("call" = call,
                      "er" = as.vector(er.gmin),
                      "sd" = as.vector(sd.gmin),
                      "weights" = w.gmin)
    class(gmin.port) <- "portfolio"
    gmin.port
  }

#-----------------------------3. tangency.portfolio--------------------------------------#
tangency.portfolio <-
  function(er,cov.mat,risk.free, shorts=TRUE)
  {
    call <- match.call()
    
    #
    # check for valid inputs
    #
    asset.names <- names(er)
    if(risk.free < 0)
      stop("Risk-free rate must be positive")
    er <- as.vector(er)
    cov.mat <- as.matrix(cov.mat)
    N <- length(er)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    # remark: could use generalized inverse if cov.mat is positive semi-definite
    
    #
    # compute global minimum variance portfolio
    #
    gmin.port <- globalMin.portfolio(er, cov.mat, shorts=shorts)
    if(gmin.port$er < risk.free)
      stop("Risk-free rate greater than avg return on global minimum variance portfolio")
    
    # 
    # compute tangency portfolio
    #
    if(shorts==TRUE){
      cov.mat.inv <- solve(cov.mat)
      w.t <- cov.mat.inv %*% (er - risk.free) # tangency portfolio
      w.t <- as.vector(w.t/sum(w.t))          # normalize weights
    } else if(shorts==FALSE){
      Dmat <- 2*cov.mat
      dvec <- rep.int(0, N)
      er.excess <- er - risk.free
      Amat <- cbind(er.excess, diag(1,N))
      bvec <- c(1, rep(0,N))
      result <- quadprog::solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=1)
      w.t <- round(result$solution/sum(result$solution), 6)
    } else {
      stop("Shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(w.t) <- asset.names
    er.t <- crossprod(w.t,er)
    sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
    tan.port <- list("call" = call,
                     "er" = as.vector(er.t),
                     "sd" = as.vector(sd.t),
                     "weights" = w.t)
    class(tan.port) <- "portfolio"
    return(tan.port)
  }

#-----------------------------4. efficient.frontier--------------------------------------#

efficient.frontier <-
  function(er, cov.mat, nport=20, alpha.min=-0.5, alpha.max=1.5, shorts=TRUE)
  {
    call <- match.call()
    
    #
    # check for valid inputs
    #
    asset.names <- names(er)
    er <- as.vector(er)
    N <- length(er)
    cov.mat <- as.matrix(cov.mat)
    if(N != nrow(cov.mat))
      stop("invalid inputs")
    if(any(diag(chol(cov.mat)) <= 0))
      stop("Covariance matrix not positive definite")
    
    #
    # create portfolio names
    #
    port.names <- rep("port",nport)
    ns <- seq(1,nport)
    port.names <- paste(port.names,ns)
    
    #
    # compute global minimum variance portfolio
    #
    cov.mat.inv <- solve(cov.mat)
    one.vec <- rep(1, N)
    port.gmin <- globalMin.portfolio(er, cov.mat, shorts)
    w.gmin <- port.gmin$weights
    
    if(shorts==TRUE){
      # compute efficient frontier as convex combinations of two efficient portfolios
      # 1st efficient port: global min var portfolio
      # 2nd efficient port: min var port with ER = max of ER for all assets
      er.max <- max(er)
      port.max <- efficient.portfolio(er,cov.mat,er.max)
      w.max <- port.max$weights    
      a <- seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
      we.mat <- a %o% w.gmin + (1-a) %o% w.max	         # rows are efficient portfolios
      er.e <- we.mat %*% er							                 # expected returns of efficient portfolios
      er.e <- as.vector(er.e)
    } else if(shorts==FALSE){
      we.mat <- matrix(0, nrow=nport, ncol=N)
      we.mat[1,] <- w.gmin
      we.mat[nport, which.max(er)] <- 1
      er.e <- as.vector(seq(from=port.gmin$er, to=max(er), length=nport))
      for(i in 2:(nport-1)) 
        we.mat[i,] <- efficient.portfolio(er, cov.mat, er.e[i], shorts)$weights
    } else {
      stop("shorts needs to be logical. For no-shorts, shorts=FALSE.")
    }
    
    names(er.e) <- port.names
    cov.e <- we.mat %*% cov.mat %*% t(we.mat) # cov mat of efficient portfolios
    sd.e <- sqrt(diag(cov.e))					        # std devs of efficient portfolios
    sd.e <- as.vector(sd.e)
    names(sd.e) <- port.names
    dimnames(we.mat) <- list(port.names,asset.names)
    
    # 
    # summarize results
    #
    ans <- list("call" = call,
                "er" = er.e,
                "sd" = sd.e,
                "weights" = we.mat)
    class(ans) <- "Markowitz"
    ans
  }

# @examples
#' # construct the data
#' asset.names = c("A", "B", "C")
#' er = c(0.0427, 0.0015, 0.0285)
#' names(er) = asset.names
#' covmat = matrix(c(0.0100, 0.0018, 0.0011,
#'                   0.0018, 0.0109, 0.0026,
#'                   0.0011, 0.0026, 0.0199),
#'                 nrow=3, ncol=3)
#' r.free = 0.008
#' dimnames(covmat) = list(asset.names, asset.names)
#' 
#' # tangency portfolio
#' tan.port <- tangency.portfolio(er, covmat, r.free)
#' # compute global minimum variance portfolio
#' gmin.port = globalMin.portfolio(er, covmat)
#' 
#' # compute portfolio frontier
#' ef <- efficient.frontier(er, covmat, alpha.min=-2, 
#'                          alpha.max=1.5, nport=20)
#' attributes(ef)
#' 
#' plot(ef)
#' plot(ef, plot.assets=TRUE, col="blue", pch=16)
#' points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
#' points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
#' text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
#' text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)    
#' sr.tan = (tan.port$er - r.free)/tan.port$sd
#' abline(a=r.free, b=sr.tan, col="green", lwd=2)
#' 
# @export efficient.frontier

#-----------------------------5. plot.portfolio--------------------------------------#
# Plot portfolio weight
plot.portfolio <-
  function(x, ...)
  {
    asset.names <- names(x$weights)
    barplot(x$weights, names=asset.names,
            xlab="Assets", ylab="Weight", main="Portfolio Weights", ...)
    invisible()
  }

# Plot efficient.frontier with short
plot.ef_s <-
  function(x, ...)
  {
    asset.names <- names(x$weights)
    
    plot(x$sd,x$er, names=asset.names,type = "l", xlim = c(0,max(x$sd)*1.2), 
         ylim = c(min(x$er)*2.5, max(x$er)*1.2),
         xlab="sd_p", ylab="er_p", main="efficient.frontier with short",lwd=2.5, ...)
    points(0, rf, cex = 3.5, pch = "*")
    points(tp_s$sd, tp_s$er, col= 'blue', cex = 3.5, pch = "*")
    invisible()
  }

# Plot efficient.frontier without short
plot.ef_nos <-
  function(x, ...)
  {
    asset.names <- names(x$weights)
    
    plot(x$sd,x$er, names=asset.names,type = "l", xlim = c(0,max(x$sd)*1.2), 
         ylim = c(0, max(x$er)*1.2),
         xlab="sd_p", ylab="er_p", main="efficient.frontier without short",lwd=2.5, ...)
    points(0, rf, cex = 3.5, pch = "*")
    points(tp_nos$sd, tp_nos$er, col= 'blue', cex = 3.5, pch = "*")
    invisible()
  }


#Part 3
##Load the data and pre-calculate



# You must load these packages to get the data
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)
library(timeSeries)
library(fPortfolio)
library(quadprog)


#Get Data of 15 assets 
symbol = c('AAPL', 'AAL', 'AEP', 'F', 'FB', 'JPM', 'KO', 'LMT', 'M', 'MSFT', 'MU', 'NKE', 'SPG', 'T', 'TIF')
for(smb in symbol){
  getSymbols(smb, src='yahoo', from = "2013-01-01", to = '2019-11-02', periodicity = "monthly")
}

#Combine them to one dataframe
p <- cbind.data.frame(AAPL[,4], AAL[,4], AEP[,4], F[,4], FB[,4], JPM[,4], KO[,4], LMT[,4],
                      M[,4], MSFT[,4], MU[,4], NKE[,4], SPG[,4], T[,4], TIF[,4])
#Rename Columns
colnames(p) <- symbol

#Calculate Returns for Assets: Monthly Return
portfolioReturns <- na.omit(ROC(p, type="discrete"))

# pairs(portfolioReturns)
mean_vect = colMeans(portfolioReturns)  # Mean of portfolio
cov_mat = cov(portfolioReturns)       # covariance matrix of portfolio
sd_vect = sqrt(diag(cov_mat))        # std of portfolio

# all data is monthly, you have to transform to annually if the question request
# annualize the monthly mean and risk by multiplying the mean by 12 and the risk bythe square root of 12


rf_rate = read.csv('Risk free.csv',row.names = 1) # Risk free rate
rf = colMeans(rf_rate)/1200





# MVP short allowed
mvp_s <- globalMin.portfolio(mean_vect, cov_mat, shorts = TRUE)
# mvp_s

# MVP_ short not allowed
mvp_nos <- globalMin.portfolio(mean_vect, cov_mat, shorts = FALSE)                             
# mvp_nos

# efficient.frontier short allowed
ef_s <- efficient.frontier(mean_vect, cov_mat, nport=30, alpha.min=-3.2, alpha.max=2, shorts=TRUE)
# ef_s

# efficient.frontier not short allowed
ef_nos <- efficient.frontier(mean_vect, cov_mat, nport=30,  alpha.min=-3.2, alpha.max=2, shorts=FALSE)
# ef_s

# Tangency Portfolio short allowed
tp_s <- tangency.portfolio(mean_vect, cov_mat, rf,shorts = TRUE)
# tp_s

# Tangency Portfolio short not allowed
tp_nos <- tangency.portfolio(mean_vect, cov_mat, rf,shorts = FALSE)
# tp_nos





# Weight of MVP with short
plot(mvp_s)

# to calculate the monthly returns on the portfolio
mvp_monthly_return<- Return.portfolio(portfolioReturns, weights = mvp_s$weights)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(mvp_monthly_return, main = "MVP_short Monthly Return") %>%
  dyAxis("y", label = "%")

# Add the wealth.index = TRUE argument and, instead of returning monthly returns,
# the function will return the growth of $100,000 invested in the portfolio.
dollar_growth_mvp_short <- Return.portfolio(portfolioReturns, weights = mvp_s$weights, wealth.index = TRUE)

# Use dygraphs to chart the growth of $100,000 in the portfolio.
dygraph(dollar_growth_mvp_short , main = "Growth of $100,000 Invested in MVP_short") %>%
  dyAxis("y", label = "100,000$")

#   to calculate SharpeRatio 
sr_mvp_short <- round(SharpeRatio(mvp_monthly_return, Rf = rf), 4)[1,]
var_mvp_short <- -VaR(mvp_monthly_return, method = 'historical', p=0.95)
es_mvp_short <- -ES(mvp_monthly_return, method = 'historical', p=0.95)
sprintf('SharpeRatio: %f', sr_mvp_short)
sprintf('Value at Risk: %f', var_mvp_short)
sprintf('Expected shortfall: %f', es_mvp_short)






# Weight of MVP without short
plot(mvp_nos)

# to calculate the monthly returns on the portfolio
mvp_noshort_monthly_return<- Return.portfolio(portfolioReturns, weights = mvp_nos$weights)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(mvp_noshort_monthly_return, main = "MVP_not_short Monthly Return") %>%
  dyAxis("y", label = "%")

# Add the wealth.index = TRUE argument and, instead of returning monthly returns,
# the function will return the growth of $100,000 invested in the portfolio.
dollar_growth_mvp_noshort <- Return.portfolio(portfolioReturns, weights = mvp_nos$weights, wealth.index = TRUE)

# Use dygraphs to chart the growth of $100,000 in the portfolio.
dygraph(dollar_growth_mvp_noshort , main = "Growth of $100,000 Invested in MVP_not_short") %>%
  dyAxis("y", label = "100,000$")

# to calculate SharpeRatio 
sr_mvp_noshort <- round(SharpeRatio(mvp_noshort_monthly_return, Rf = rf), 4)[1,]
var_mvp_noshort <- -VaR(mvp_noshort_monthly_return, method = 'historical', p=0.95)
es_mvp_noshort <- -ES(mvp_noshort_monthly_return, method = 'historical', p=0.95)
sprintf('SharpeRatio: %f', sr_mvp_noshort)
sprintf('Value at Risk: %f', var_mvp_noshort)
sprintf('Expected shortfall: %f', es_mvp_noshort)




# Weight ofTangency Portfolio with short
plot(tp_s)

# to calculate the monthly returns on the portfolio
tp_monthly_return<- Return.portfolio(portfolioReturns, weights = tp_s$weights)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(tp_monthly_return, main = "Tangency Portfolio_short Monthly Return") %>%
  dyAxis("y", label = "%")

# Add the wealth.index = TRUE argument and, instead of returning monthly returns,
# the function will return the growth of $100,000 invested in the portfolio.
dollar_growth_tp_short <- Return.portfolio(portfolioReturns, weights = tp_s$weights, wealth.index = TRUE)

# Use dygraphs to chart the growth of $100,000 in the portfolio.
dygraph(dollar_growth_tp_short , main = "Growth of $100,000 Invested in Tangency Portfolio_short") %>%
  dyAxis("y", label = "100,000$")

# to calculate SharpeRatio 
sr_tp_short <- round(SharpeRatio(tp_monthly_return, Rf = rf), 4)[1,]
var_tp_short <- -VaR(tp_monthly_return, method = 'historical', p=0.95)
es_tp_short <- -ES(tp_monthly_return, method = 'historical', p=0.95)
sprintf('SharpeRatio: %f', sr_tp_short)
sprintf('Value at Risk: %f', var_tp_short)
sprintf('Expected shortfall: %f', es_tp_short)





# Weight of Tangency Portfolio without short
plot(tp_nos)

# to calculate the monthly returns on the portfolio
tp_noshort_monthly_return<- Return.portfolio(portfolioReturns, weights = tp_nos$weights)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(tp_noshort_monthly_return, main = "Tangency Portfolio_no short Monthly Return") %>%
  dyAxis("y", label = "%")

# Add the wealth.index = TRUE argument and, instead of returning monthly returns,
# the function will return the growth of $100,000 invested in the portfolio.
dollar_growth_tp_noshort <- Return.portfolio(portfolioReturns, weights = tp_nos$weights, wealth.index = TRUE)

# Use dygraphs to chart the growth of $100,000 in the portfolio.
dygraph(dollar_growth_tp_noshort , main = "Growth of $100,000 Invested in Tangency Portfolio_no short") %>%
  dyAxis("y", label = "100,000$")

# to calculate SharpeRatio 
sr_tp_noshort <- round(SharpeRatio(tp_noshort_monthly_return, Rf = rf), 4)[1,]
var_tp_noshort <- -VaR(tp_noshort_monthly_return, method = 'historical', p=0.95)
es_tp_noshort <- -ES(tp_noshort_monthly_return, method = 'historical', p=0.95)
sprintf('SharpeRatio: %f', sr_tp_noshort)
sprintf('Value at Risk: %f', var_tp_noshort)
sprintf('Expected Shortfall: %f', es_tp_noshort)




# Sharpe Ratio, VaR, ES of each assets 
sr_var_es <- matrix(NA, nrow = 15, ncol = 3)
for (i in 1:15) {
  x <- data.matrix(portfolioReturns)[,i]
  
  sr_var_es[i,1] <- round(SharpeRatio(x, Rf = rf), 4)[1,]
  sr_var_es[i,2] <- -VaR(x,method = 'historical',p=0.95)
  sr_var_es[i,3] <- -ES(x,method = 'historical',p=0.95)
}

colnames(sr_var_es) <- c('Sharpe Ratio', 'VaR', 'ES')
rownames(sr_var_es) <- symbol


# Sharpe Ratio, VaR, ES of each portfolio
p_return <- cbind.data.frame(mvp_monthly_return, mvp_noshort_monthly_return,
                             tp_monthly_return,tp_noshort_monthly_return)
colnames(p_return) <- c('mvp_monthly_return', 'mvp_noshort_monthly_return'
                        , 'tp_monthly_return', 'tp_noshort_monthly_return')

port_sr <- matrix(NA, nrow = 4, ncol = 3)
for (i in 1:4) {
  x <- data.matrix(p_return)[,i]
  
  port_sr[i,1] <- round(SharpeRatio(x, Rf = rf), 4)[1,]
  # port_sr[i,2] <- round(SharpeRatio(x, Rf = 0), 4)[2,]
  # port_sr[i,3] <- round(SharpeRatio(x, Rf = 0), 4)[3,]
  port_sr[i,2] <- -VaR(x,method = 'historical',p=0.95)
  port_sr[i,3] <- -ES(x,method = 'historical',p=0.95)
}
colnames(port_sr) <- c('Sharpe Ratio', 'VaR', 'ES')
rownames(port_sr) <- c('mvp_monthly_return', 'mvp_noshort_monthly_return'
                       , 'tp_monthly_return', 'tp_noshort_monthly_return')

p_sr <- rbind.data.frame(sr_var_es, port_sr)
p_index <- c(seq(1,19))
cbind(p_index,p_sr)




# Set the constraints (short not allowed)
cons <- "Longonly"
spec <- portfolioSpec(portfolio=list
                      (nFrontierPoints = 100, 
                        riskFreeRate=rf))

# Calculate efficient frontier without short
frontier <- portfolioFrontier(as.timeSeries(portfolioReturns),spec=spec,
                              constraints = cons)
# Plot the efficient frontier without short
frontierPlot(frontier, 
             pch = 19,
             cex = 0.5,
             xlim=c(0,0.15),
             ylim=c(-0.02,0.035))
grid()
abline(h = 0, col = "grey30")
abline(v = 0, col = "grey30")
minvariancePoints(frontier, pch = 19, col = "red",cex = 1.3)
tangencyPoints(frontier, pch = 19, col = "blue",cex = 1.3)
tangencyLines(frontier, col = "red",lwd=2)

legend("topleft",c('efficient frontier without short',"Tangency portfolio","Min-variance-Portfolio"),
       
       pch=c("*","*","*"),col=c('white',"blue","red"),
       pt.cex=c(2,2,2),
       cex = c(0.6,0.6,0.6)
)
for (i in 1:15) {
  text(sd_vect[i],mean_vect[i],i)
}

#---------------------------------------------------------------#
# Set the constraints (short allowed)
cons <- "Short"

# Calculate efficient frontier with short
shortSpec <- portfolioSpec(portfolio=list
                           (nFrontierPoints = 100, 
                             riskFreeRate=rf))
setSolver(shortSpec) <- "solveRshortExact"
frontier <- portfolioFrontier(as.timeSeries(portfolioReturns),spec=shortSpec,
                              constraints = cons)
# Plot the efficient frontier with short
frontierPlot(frontier, 
             pch = 19,
             cex = 0.5,
             xlim=c(0,0.15),
             ylim=c(-0.02,0.035))
grid()
abline(h = 0, col = "grey30")
abline(v = 0, col = "grey30")
minvariancePoints(frontier, pch = 19, col = "red",cex = 1.3)
tangencyPoints(frontier, pch = 19, col = "blue",cex = 1.3)
tangencyLines(frontier, col = "red",lwd=2)

legend("topleft",c('efficient frontier with short',"Tangency portfolio","Min-variance-Portfolio"),
       
       pch=c("*","*","*"),col=c('white',"blue","red"),
       pt.cex=c(2,2,2),
       cex = c(0.6,0.6,0.6)
)
for (i in 1:15) {
  text(sd_vect[i],mean_vect[i],i)
}



sprintf('The annually mean of MVP with short is %f percent',mvp_s$er*12*100)
sprintf('The annually mean of MVP without short is %f percent',mvp_nos$er*12*100)
sprintf('The annually risk of MVP with short is %f',mvp_s$sd*sqrt(12))
sprintf('The annually risk of MVP without short is %f',mvp_nos$sd*sqrt(12))




# efficient.portfolio with expected return of 0.5% per month
spec <- portfolioSpec(portfolio=list
                      (targetReturn=0.005))
cons <- 'Longonly'
port_er0.5 <- efficientPortfolio(as.timeSeries(portfolioReturns), spec = spec, 
                                 constraints = cons)
port_er0.5
# how much is invested in each of asstes in this efficient portfolio
cat('For invested in each assets(Total 1 dollar):', '\n','   
    we do not invest in AAPL,AAL,LMT,MSFT,MU and TIF', '\n',
    '   We invest in AEP 0.206973', '\n',
    '   We invest in F 0.076768', '\n',
    '   We invest in FB 0.018301', '\n',
    '   We invest in JPM 0.079025', '\n',
    '   We invest in KO 0.280255', '\n',
    '   We invest in M 0.066547', '\n',
    '   We invest in NKE 0.006506', '\n',
    '   We invest in SPG 0.056684', '\n',
    '   We invest in T 0.208941')




sprintf('The monthly risk of efficient.portfolio without short is %f',0.0269)
print('The  monthly 5% value-at-risk of efficient.portfolio without short is 3240')
print('The  monthly 5%  expected shortfall of efficient.portfolio without short is 5640')


# combine the risk free and tangency portfolio without short
riskfree<-rf_rate[-1,]/100
n_portfolioReturns <-cbind(portfolioReturns,riskfree) 

# efficient.portfolio with expected return of 0.5% per month
port_tp_rf <- efficient.portfolio(colMeans(n_portfolioReturns),cov(n_portfolioReturns),0.005,shorts = FALSE)
port_tp_rf

# how much is invested in each of asstes in this efficient portfolio
cat('For invested in each assets(Total 1 dollar):', '\n','   
    we do not invest in AAPL,AAL,AEP, FB,JPM,LMT,MSFT,MU,NKE and TIF', '\n',
    '   We invest in F 0.090736', '\n',
    '   We invest in KO 0.034396', '\n',
    '   We invest in M 0.058590', '\n',
    '   We invest in SPG 0.055266', '\n',
    '   We invest in T 0.087783', '\n',
    '   We invest in riksfree 0.673228')


spec <- portfolioSpec(portfolio=list
                      (targetReturn=0.005))

port_tp_rf2 <- efficientPortfolio(as.timeSeries(n_portfolioReturns), spec = spec, 
                                  constraints = cons)
port_tp_rf2

sprintf('The monthly risk of efficient.portfolio without short is %f',0.0130)
print('The  monthly 5% value-at-risk of efficient.portfolio without short is 1770')
print('The  monthly 5%  expected shortfall of efficient.portfolio without short is 2060')




#------------------------------------------------cutting line 2 ----------------------------------------------------------------------------#\




#PCA

eig = eigen(cor(return.each.asset))
eig

pca = princomp(return.each.asset,cor=TRUE)
pca
pca$loadings

summary(pca)

pcatable <- table(summary(pca))



#------------------------------------------------cutting line 3 ----------------------------------------------------------------------------#\



##Risk Management

####
library(plyr)
library(dplyr)
####

####
data = read.csv("return.csv")
data = data[, names(data) != "X"]
investment = 100000

VaRnormalEqwt <- function(returnVector, prob=.05, 
                          notional=1, expected.return=mean(returnVector), 
                          digits=8)
{
  if(prob > .5) prob <- 1 - prob
  ans <- -qnorm(prob, mean=expected.return, 
                sd=sd(returnVector)) * notional
  signif(ans, digits=digits)
}

#VaRnormalEqwt(data$AAPL)

## 95% VaR Norm
library(cvar)
set.seed(5261)
VaR_ind_norm = investment * apply(data,2,VaRnormalEqwt)
VaR_ind_norm = data.frame(VaR_ind_norm)
rownames(VaR_ind_norm) = names(data)
colnames(VaR_ind_norm) = "VaR"
####

####
## 95% ES Norm

ESnormalEqwt <- function(returnVector, prob=.05, 
                         notional=1, expected.return=mean(returnVector), 
                         digits=8)
{
  if(prob > .5) prob <- 1 - prob
  retsd <- sd(returnVector)
  v <- qnorm(prob, mean=expected.return, sd=retsd) 
  tailExp <- integrate(function(x) 
    x * dnorm(x, mean=expected.return, sd=retsd), 
    -Inf, v)$value / prob
  ans <- -tailExp * notional
  signif(ans, digits=digits)
}

#ESnormalEqwt(data$AAPL, notional=investment)

set.seed(5261)
ES_ind_norm = investment * apply(data,2,ESnormalEqwt)
ES_ind_norm = data.frame(ES_ind_norm)
rownames(ES_ind_norm) = names(data)
colnames(ES_ind_norm) = "ES"
####

####
## 95% VaR Hist
VaRhistEqwt <- function(returnVector, prob=.05, 
                        notional=1, 
                        digits=8)
{
  if(prob > .5) prob <- 1 - prob
  ans <- -quantile(returnVector, probs = prob) * notional
  signif(ans, digits=digits)
}

VaR_ind_hist = investment * apply(data,2,VaRhistEqwt)
VaR_ind_hist = data.frame(VaR_ind_hist)
rownames(VaR_ind_hist) = names(data)
colnames(VaR_ind_hist) = "VaR"
####

####
## 95% ES Hist
EShistEqwt <- function(returnVector, prob=.05, 
                       notional=1,
                       digits=8)
{
  if(prob > .5) prob <- 1 - prob
  v <- quantile(returnVector, probs = prob) * notional
  ans <- -mean(returnVector[(1:length(returnVector))[returnVector< v]]) * notional
  signif(ans, digits=digits)
}

ES_ind_hist = investment * apply(data,2,EShistEqwt)
ES_ind_hist = data.frame(ES_ind_hist)
rownames(ES_ind_hist) = names(data)
colnames(ES_ind_hist) = "ES"
####

####
library(ggplot2)
VaR_table = rbind(VaR_ind_norm,VaR_ind_hist)
#VaR_table$VaR = abs(VaR_table$VaR)
VaR_table$Method = c(rep(c("Normal","Historical"),each = 15))
VaR_table$Stock = c(names(data),names(data))
ggplot(VaR_table, aes(fill=Method, y=VaR, x=Stock)) + 
  geom_bar(position="dodge", stat="identity")
####

####
ES_table = rbind(ES_ind_norm,ES_ind_hist)
ES_table$ES = abs(ES_table$ES)
ES_table$Method = c(rep(c("Normal","Historical"),each = 15))
ES_table$Stock = c(names(data),names(data))
ggplot(ES_table, aes(fill=Method, y=ES, x=Stock)) + 
  geom_bar(position="dodge", stat="identity")
####

####
## Bootstrap
set.seed(5261)
Bootstrap = function(stock, B=100){
  res <- matrix(0, nrow=B, ncol=2, dimnames=list(NULL, c("VaR","ES")))
  for (i in (1:B)){
    x <- sample(stock, replace=TRUE)
    ## VaR
    VaR <- VaRnormalEqwt( x )*investment
    ## ES
    ES <- ESnormalEqwt( x)*investment
    ## Std 
    res[i,"VaR"] <- VaR
    res[i,"ES"] <- ES
  }
  VaR_std <- sd(res[,"VaR"])
  ES_std <- sd(res[,"ES"])
  CI <- apply(res, 2, function(x) quantile(x, probs = c(0.025,0.975)))
  return(list("CI" = CI,"VaR_std"=VaR_std,"ES_std"=ES_std))
}
CI_ind = apply(data,2,Bootstrap)
####


##Copulas
####
library(gridExtra)
for ( i in names(data)){
  t = data.frame( "return" =data[ , i]) 
  assign(paste0("p",i),
         ggplot(t, aes(x = return)) + 
           geom_histogram(aes(y=..density..), colour="black", fill="white",bins = 30)+
           geom_density(alpha=.2, fill="#FF6666")+
           xlab(paste(i,"return"))
  )
}
grid.arrange(pAAPL,pAAL,pAEP ,pF ,pFB ,pJPM ,pKO ,pLMT ,pM ,pMSFT ,pMU ,pNKE ,pSPG ,pT ,pTIF,nrow = 5)
####


####
library(copula)
#library(VineCopula)
d = 15
data_copulas = pobs(data)
ncop = fitCopula(normalCopula(dim = d, dispstr = "un"), data = data_copulas,
                 method = "ml")
tcop = fitCopula(tCopula(dim = d, dispstr = "un"), data = data_copulas,
                 method = "ml")
colcop = fitCopula(archmCopula(dim = d, family = "clayton"), data = data_copulas,
                   method = "ml")
gcop = fitCopula(archmCopula(dim = d, family = "gumbel"), data = data_copulas,
                 method = "ml")
####

####
AIC_calc = function(loglikelihood, paralength){
  return(-2*loglikelihood + 2*paralength)
}
####
####
AIC_table = data.frame("Gaussian Copula AIC"=AIC_calc(ncop@loglik,length(ncop@estimate)),
                       "t Copula AIC" = AIC_calc(tcop@loglik,length(tcop@estimate)),
                       "Clayton Copula AIC" = AIC_calc(colcop@loglik,length(colcop@estimate)),
                       "Gumbel Copula AIC" = AIC_calc(gcop@loglik,length(gcop@estimate))
)
AIC_table
####
####
n = nrow(data_copulas)
BIC_calc = function(loglikelihood, paralength,n = 82){
  return( log(n)*paralength - 2*loglikelihood )
}
####
####
BIC_table = data.frame("Gaussian Copula BIC"=BIC_calc(ncop@loglik,length(ncop@estimate)),
                       "t Copula BIC" = BIC_calc(tcop@loglik,length(tcop@estimate)),
                       "Clayton Copula BIC" = BIC_calc(colcop@loglik,length(colcop@estimate)),
                       "Gumbel Copula BIC" = BIC_calc(gcop@loglik,length(gcop@estimate))
)
BIC_table
####

####
tcop_est = tcop@estimate
tcop_df = tcop_est[length(tcop_est)]
tcop_est = tcop_est[1:length(tcop_est)-1]
tcop_sim = tCopula(dim = d, dispstr = "un", param = tcop_est,df = tcop_df)
rand_t_cop = rCopula(n = n, copula = tcop_sim)
####
####
rand_t_cop = data.frame(rand_t_cop)
colnames(rand_t_cop) = names(data)
cor_mat_sim = cor(rand_t_cop, method = "kendall")
p_sim = corrplot(cor_mat_sim, type="upper", 
                 col=brewer.pal(n=8, name="RdBu"))
####





