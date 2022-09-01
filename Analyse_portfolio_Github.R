install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("imputeTS")
install.packages("PortfolioAnalytics")
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
env.stock <- new.env()
getSymbols.csv(c("COSUMAR1","Bank of Africa1","Lesieur Cristal1","Microdata1"),
               return.class = "xts",
               dir = "C:/Users/Nouhaila/Desktop/portfolio",
               extension="csv",
               env = env.stock)
cours.ajuste <- do.call(merge,eapply(env.stock,Ad))
######log_returns######
cours.logreturns <-na.omit(diff(log(cours.ajuste),lag = 1))
######Annual_returns########
vec.moyenre <- sapply(cours.logreturns,FUN = mean)
vec.annualre <- exp(vec.moyenre*250)-1
vec.annualre
##### definir portfolio weight#######
vec.weight <- c(0.20,0.30,0.20,0.30)####si vous voulez la meme part des actions utiliser c(rep(1/4,each=4))##### 
vec.weight
########portfolio_returnTS########
portfolioReturn <- Return.portfolio(cours.logreturns, weights=vec.weight)
chart.CumReturns(portfolioReturn)
######portfolio return####
var.potre <- crossprod(vec.weight,vec.annualre )
var.potre
####calcul de covariance#####
Mat.vco <- cov(cours.logreturns)*250
Mat.vco
####calcul portfolio_risk###
var.potvar <- vec.weight %*% Mat.vco %*% matrix(t(vec.weight))
var.potvar
var.potvola <-sqrt(var.potvar)
var.potvola
#########benchmarkReturns ########
env.benchmark <- new.env()
benchmarkPrices <- getSymbols.csv("MASI1",
                                  return.class = "xts",
                                  dir = "C:/Users/Nouhaila/Desktop/portfolio",
                                  extension="csv",env = env.benchmark)
MASI.close<- do.call(merge,eapply(env.benchmark,Cl))
benchmarkReturns <- na.omit(ROC(MASI.close, type="discrete"))
########CAPM###########
#####0.02=risk free 250=days of trading########
CAPM.beta(portfolioReturn, benchmarkReturns, 0.02/250)
CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, 0.02/250)
SharpeRatio(portfolioReturn, Rf = 0.02/250, p = 0.95, FUN = "StdDev", annualize = FALSE)
table.AnnualizedReturns(portfolioReturn, Rf= 0.02/250, geometric=TRUE)
#######portfolio optimization#########

portf <- portfolio.spec(colnames(cours.logreturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.30)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")
########ROI package needs to be downloaded with ROI.plugin.quadprog & ROI.plugin.glkp for the optimize.portfolio to work########

install.packages("ROI.plugin.quadprog")
install.packages("ROI.plugin.glkp")
library("ROI.plugin.quadprog")
library("ROI.plugin.glpk")
optPort <- optimize.portfolio(cours.logreturns, portf, optimize_method = "ROI", trace=TRUE )
optPort
