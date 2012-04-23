###################################################
## R Script Calculating Enron Default Prob. -- V.0
###################################################

require(fImport)
require(timeSeries)



## setwd("~/PATH/TO/DATA_FILES")
## |--prc.csv: ENE monthly prices 1999-01 to 2001-12
## |--ltq.csv: ENE Total Liablilities Quarterly 
## |--

## source("options.r")


START <- as.Date("1999-01-01")
END <- as.Date("2001-12-31")

prc.raw <- prc <- read.csv("prc.csv")
ltq <- read.csv("ltq.csv")
mkt.raw <- mkt <- read.csv("sp.csv")

cmr1y <- fredImport("DGS1", from="1999-01-01", to="2001-12-31")  # 1-Year Treasury constant maturity rate
cmr1y.relevant <-tmp <- cut(cmr1y@data, from="1999-01-01", to="2001-12-31")

cpi <- fredImport("CPIAUCSL", from="1999-01-01", to="2001-12-31")  # 1-Year Treasury constant maturity rate
# CPI index data are sampled at monthl start, while other data are sampled at month end, adjustments are made
# to use next month's CPI data for the analysis
cpi <- cut(cpi@data, from="1999-02-01", to="2002-01-31")

#####################################################
## formating data from different sources
#####################################################
prc$DATE <- as.Date(prc$DATE, "%m/%d/%Y")
mkt$DATE <- as.Date( as.character(mkt$DATE), "%Y%m%d" )

prc.raw$DATE <- prc$DATE
mkt.raw$DATE <- mkt$DATE

prc <- subset(prc, DATE>=START & DATE<=END)

## align treasury rate timestamps to ENE prices data

timestamps <- rownames(tmp) %in% as.character(prc$DATE)
## convert percentages, raw data already scaled to yearly
tb1y <- tmp[timestamps, ]/100

timestamps <- as.character(mkt$DATE) %in% as.character(prc$DATE)
mkt <- mkt$sprtrn[timestamps]



## convert LTQ data to timeSereis Object

ltq <- timeSeries(ltq$LTQ, charvec=as.character(ltq$datadate), format="%Y%m%d")

timestamps <- as.Date( rownames(ltq) )
ltq_expand <- c()
for (a in prc$DATE){
    tmp <- which(timestamps>=a)
    b <- tmp[1]
    if (is.na(b)){
# as ltq does not have observations from Q4 2001, use Q3 instead
        b <- length(timestamps)
    }
    ltq_expand <- c(ltq_expand,ltq[b])
}

## SHROUT is measured in thousands, and LTQ is in millions, the following rescaling
## makes the two comparable, the market value of Equity computed will be in $million
## as well with the adjustments. 
data<- cbind(prc$PRC, prc$SHROUT/1000, ltq_expand,tb1y,as.vector(cpi)/cpi[1], mkt)
names(data) <- c("PRC", "SHROUT", "LTQ", "TB1Y", "CPIADJ", "MKTRET")
tsdata <- timeSeries(data, charvec=rownames(data))

attach(data)


## Prepare Data for CAPM 
timestamps <- as.character(mkt.raw$DATE) %in% as.character(prc.raw$DATE)
tmp <- mkt.raw[timestamps,]
mkt.ret <- c()
for (i in prc.raw$DATE){
    b <- which(tmp$DATE==i)
    mkt.ret <- c(mkt.ret, mean(tmp$sprtrn[b]))
}
CAPM <- cbind(returns(prc.raw$PRC), mkt.ret)
row.names(CAPM) <- prc.raw$DATE




#####################################################
## Preparing for Computation
#####################################################

REPEAT <- 1000                # number of iterations
iter <- 0                     # iterations
E <- PRC*SHROUT               # market value of equity in $ million
A <- (E + LTQ)*CPIADJ         # market value of the entire firm, adjusted by inflation
LOGR <- returns(A)            # LOG returns of Asset Value, monthly 
LOGR.cut <- LOGR[2:12]        # Do not use information in the future 
sigma <-sqrt(var(LOGR.cut)*12)# Std. of LOG returns, scaled to yearly
T <- rep(1,nrow(data))        # Vector of time to maturity in years
STOP <- 0.0000000001          # Critiria for convergence 


bs.call <- function(option.price,volatility,strike.price,Stock.price,TTM,rate) {
## reversed Black-Scholes formula, all arguments except volatility in vector form
## returns Black-Scholes Stock Price from Option Price,  TTM and volatility is YEARS !! 
    d1 <- (log(Stock.price/strike.price) + (rate + volatility^2/2)*TTM)/(volatility * sqrt(TTM))
    d2 <- d1 - (volatility * sqrt(TTM))
    return ((option.price+strike.price*pnorm(d2)*exp(-TTM*rate))/pnorm(d1))
}


#####################################################
## Computing
#####################################################
DIFF <- 99    
Last.A <- A

while (iter<REPEAT & DIFF>STOP) {

    Last.A <- A
    A <- bs.call(option.price=E,volatility=sigma,strike.price=LTQ,Stock.price=A,TTM=T,rate=TB1Y)
    LOGR <- returns(A)                # LOG returns of Asset Value, monthly 
    sigma <- c()
    for (i in 1:24){
        sigma[i] <- sqrt(var(LOGR[2:(12+i-1)])*12)   # Std. of LOG returns, scaled to yearly
    }
    iter <- iter + 1
    DIFF <- (abs(A-Last.A))
    DIFF <- DIFF[!is.na(DIFF)]
    DIFF <- min(DIFF)
    print(DIFF)

}


#####################################################
## CAPM estimation of expected Returns
#####################################################

# risk-free rate using historical average of 1 year treasury returns
RF <- rep(mean(TB1Y), nrow(CAPM))
CAPM <- as.data.frame(cbind(CAPM, RF))
names(CAPM) <- c("ENE","MKT","RF")

capm_model <- (ENE-RF)~(MKT-RF)  # using log returns of asset value
data <- data[as.Date(rownames(data))>=as.Date("2000-01-01"), ]

K <- nrow(CAPM) - nrow(data)

beta <- c()
rmkt <- c()
for (i in 1:nrow(data)) {
    capm <- CAPM[2, ]
    regdata <- as.data.frame(CAPM[1:K+i-1, ])
    fit <- lm(capm_model, data=regdata)
    B <- fit$coef[2]
    beta <- c(beta,B)
    rmkt <- c(rmkt, mean(regdata[,2]))
}

ER <- beta*rmkt + mean(RF)               # expected return on asset value
DRIFT <- log(1+ER)                              


#####################################################
## Computing Default Prob. 
#####################################################

prob <- function(asset.value, liability, drift, TTM, volatility) {
    return (pnorm((log(liability/asset.value)-(drift-volatility^2)*TTM)/(volatility*sqrt(TTM))))
}

P <- prob(A,LTQ,DRIFT,T,sigma)
