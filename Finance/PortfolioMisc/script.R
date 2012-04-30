require("tseries")
require("timeSeries")
require("fImport")

source("../../utils.R")

compression <- list(monthly="m", weekly="w", daily="d")


monthly <- monthly %||% new.env()
daily <- daily %||% new.env()
weekly <- weekly %||% new.env()

testing <- testing %||% new.env()


settings.historical <- list(start ='2008-01-20', end='2012-01-20', 
	                    quote='AdjClose')

settings.testing <- list(start ='2012-01-21', end='2012-03-20', 
	                    quote='AdjClose', 
			    compression="d")

syms <- syms %||% read.csv("symbols.csv", 
	          header=T, 
		  quote = "", 
		  check.names=F)    # empty data.frame with only header

# various precomplied portfolios, MVP, MVE etc..
portfolios <- portfolios %||% read.csv("optim.csv", header=T)

scaleRf <- function(freq) {

# please try different scalar values (say, daily=365, 220 etc.) and compare the results
    scalar <- list(yearly=1, monthly=12, weekly=52, daily=252)


    if (class(freq) == "environment") {
		if (identical(freq, monthly)){
			i <- "monthly"
		}
		if (identical(freq, weekly)){
			i <- "weekly"
		}
		if (identical(freq, daily)){
			i <- "daily"
		}
		if (identical(freq, testing)){
			i <- "daily"
		}
    } else {
        i <- freq
    }
    return (1/scalar[[i]])

}

fetchHistorical <- function() { 

## For eqch frequency, store the data in the associated environment
    
    for (compr in names(compression)) {
	env <- get(compr)

	for (symbol in names(syms)) {
	    .args <- settings.historical
	    .args$compression <- compression[[compr]]
	    .args$instrument <- symbol
	    stock <- do.call(get.hist.quote, 
			     args = .args, 
			     quote=TRUE)
	    .name <- paste("stocks",symbol, sep=".")
	    assign(.name, stock, envir=env)
	}
        
    }

    names(syms) 

}
  

fetchHistorical <- function() { 

## For eqch frequency, store the data in the associated environment
    
    for (compr in names(compression)) {
	env <- get(compr)

	for (symbol in names(syms)) {
	    .args <- settings.historical
	    .args$compression <- compression[[compr]]
	    .args$instrument <- symbol
	    stock <- do.call(get.hist.quote, 
			     args = .args, 
			     quote=TRUE)
	    .name <- paste("stocks",symbol, sep=".")
	    assign(.name, stock, envir=env)
	}
        
    }

    names(syms) 

}

fetchTesting <- function() {

# testing is a seperate environment to store data in the testing period

    for (symbol in names(syms)) {
	.args <- settings.testing
	.args$instrument <- symbol
	stock <- do.call(get.hist.quote, 
			 args = .args, 
			 quote=TRUE)
	.name <- paste("stocks",symbol, sep=".")
	assign(.name, stock, envir=testing)
    }

}

formatData <- function(env) {
    .args <- as.list( paste("stocks", names(syms), sep=".") ) 
    .args <- lapply(.args, function(.name){
        get(.name, envir=env)	            
    })
    .args$method = "match"
    
    env$data <- do.call(cleanse, .args)

    colnames(env$data) <- names(syms)
}

fitCapm <- function(env) {
    data <- env$data
    # removing the first row of NA's 
    rets <- returns(data)[-1, ]
    beta <- function(v) {
	if (identical(v, rets[, 1])) return (c(0,1))
	v <- cleanse(v, RF*scaleRf(env), method="match")
	v1 <- v[, 1] - v[, 2]
	mkt <- rets[, 1] - v[, 2]
        fit <- lm(v1~mkt)
	return (fit$coef)
    }

    betas <- apply(rets, 2, beta)
    names(betas) <- names(syms)

    env$CAPM.Betas <- betas
    
}

## Fama French Three Factor Data

ff <- function() {
    url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily.zip"
    tmp <- tempfile()
    download.file(url, tmp)
    file.list <- unzip(tmp, list=TRUE)
    ff_daily_factors <-
        read.fwf(unzip(tmp, files=as.character(file.list[1,1])),
		 widths=c(8,8,8,8,10), 
		 header=F,
		 stringsAsFactors=FALSE, skip=5)

    names(ff_daily_factors) <- c("date", "mktrf", "smb", "hml", "rf")


    ff_daily_factors <- clean.matrix(ff_daily_factors)   # Remove NA's 

    ff_daily_factors$date <- as.Date(ff_daily_factors$date, format="%Y%m%d")

    zoo(ff_daily_factors, ff_daily_factors$date)
}

fitApt <- function(env, factors, label="APT") {
    data <- env$data
    # removing the first row of NA's
    rets <- returns(data)[-1, ]
    if (nrow(data) != nrow(factors)) return (-1)
    beta <- function(v) {
	v <- cleanse(v, RF*scaleRf(env), method="match")
	# excess returns 
	v <- v[, 1] - v[, 2]
        # the factors are typically matched by time stamp with price series
        # the first row needs to be removed, as the returns on that time period 
        # is not aviable
	if (length(v) != nrow(factors)) {
	    factors <- factors[-1, ]
	}
	reg.data <- as.data.frame(cbind(v, factors))
	names(reg.data) <- c(
	    "Return", 
	    paste("F", c(1:ncol(factors)), sep=".")
    	)

	f <- paste(names(reg.data)[-1], collapse="+")
	f <- paste(c("Return~", f), collapse="")
	f <- as.formula(f)
        fit <- lm(f, data=reg.data)
	return (fit$coef)
    }

    betas <- apply(rets, 2, beta)

    .name = paste(label, "Betas", sep=".")

    env[[.name]] <- betas
    
}

fitFF <- function(env){
    factors <- FFfactors
    factors <- cleanse(x=factors, y=env$data[, 1], method="match")
    factors <- factors[, 2:5]
    factors <- apply(factors, 2, as.numeric)
    # market excess returns 
    factors[, 1] <- factors[, 1] - factors[, 4]*scaleRf(env)
    return (fitApt(env, factors[, 1:3], label="FF"))
}

# a sample APT with market(SP500), ETF "USO" and "TIP" as factors
fitSampleAPT <- function(env){

    factors <- list()
    for (symbol in c("^GSPC", "USO", "TIP")) {
	.args <- settings.historical
	.args$compression <- compression[[deparse(substitute(env))]]
	.args$instrument <- symbol
	etf <- do.call(get.hist.quote, 
			 args = .args, 
			 quote=TRUE)
	.name <- paste("etf",symbol, sep=".")
	factors[[.name]] <- etf
    }

    factors <- as.data.frame(factors)
    factors <- cleanse(factors, RF*scaleRf(env), method="match")
    factors <- apply(factors, 2, function(v) (v - factors[, 4])) 

    return (fitApt(env, factors[, 1:3], label="SApt"))

}

forecastReturn <- function(freq, model, portfolio, factors) {
   
    port <- portfolio / sum(portfolio)
    label <- paste(model, "Betas", sep=".")
    params <- freq[[label]]
    cnst <- rep(1, nrow(factors))
    zeros <- cnst - 1
    f <- as.matrix(cbind(cnst, factors))


    for (sym in names(port)) {
	x <- colnames(params)
	filter <- x[x==sym]
	param <- params[, filter]
	rets <- f %*% param
	zeros <- zeros + rets * port[[sym]]
    }
    
    match <- cleanse(zeros, RF, method="match")
    match[, 1] + match[, 2]
}

forecast.CAPM <- function(freq, portfolio) {
    mktrt <- testing$data[, 1]
    mktrt <- cleanse(mktrt, RF, method="match")
    mktrt <- mktrt[, 1] - mktrt[, 2]
    mktrt <- returns(mktrt)


    return (forecastReturn(freq, "CAPM", portfolio, mktrt))
} 

forecast.FF <- function(freq, portfolio) {
    factors <- FFfactors[, 2:5]
    factors <- cleanse(factors, testing$data[, 1], method="match")
    factors <- apply(factors, 2, as.numeric)
    factors[, 1] <- factors[, 1] - factors[, 4]*scaleRf(freq)


    return (forecastReturn(freq, "FF", portfolio, factors[, 1:3]))
} 

forecast.SApt <- function(freq, portfolio) {

    factors <- list()
    for (symbol in c("^GSPC", "USO", "TIP")) {
	.args <- settings.testing
	.args$instrument <- symbol
	stock <- do.call(get.hist.quote, 
			 args = .args, 
			 quote=TRUE)
	factors[[symbol]] <- stock
    }

    factors <- as.data.frame(factors)
    factors <- cleanse(factors, RF*scaleRf(freq), method="match")
    factors <- apply(factors, 2, function(v) (v - factors[, 4])) 


    return (forecastReturn(freq, "SApt", portfolio, factors[, 1:3]))
} 

actual <- function(portfolio) {
    
    rets <- as.matrix(returns(testing$data))[, 1:11]
    
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets %*% as.matrix(port)
}

### This Script takes a lot of time to run, so set flags for different stages, to avoid
### repeating previous steps, in case of any small changes to the program requires 
### another run. 

if (!exists("analysisReady") || is.null(analysisReady) ) {

    fetchHistorical()
    fetchTesting()
    
    FFfactors <- ff()
    RF <- zoo(as.numeric(FFfactors[, 5]), index(FFfactors))

    formatData(monthly)
    formatData(weekly)
    formatData(daily)
    formatData(testing)

    analysisReady <- TRUE

}

if (!exists("modelReady") || is.null(modelReady) ) {

    fitCapm(monthly)
    fitCapm(weekly)
    fitCapm(daily)

    fitFF(monthly)
    fitFF(weekly)
    fitFF(daily)

    fitSampleAPT(monthly)
    fitSampleAPT(weekly)
    fitSampleAPT(daily)

    modelReady <- TRUE
}


if (!exists("testingReady") || is.null(testingReady) ) {

    port.types <- unique(portfolios$Label)


    port.mat <- apply(as.matrix(port.types), 1, function(type){
		d <- subset(portfolios, Label==type & Compression=="w")$Weight
		as.matrix(d)
	    })

    colnames(port.mat) <- port.types
    rownames(port.mat) <- sub("MKT", "SPY", portfolios$Symbol[1:11])

    port.singles <- diag(rep(1, 11))
    colnames(port.singles) <- portfolios$Symbol[1:11]

    port.mat <- cbind(port.mat, port.singles)

    monthly$CAMP.fore <- apply(port.mat, 2, function(v) forecast.CAPM(monthly, v))
    monthly$FF.fore <- apply(port.mat, 2, function(v) forecast.FF(monthly, v))
    monthly$SApt.fore <- apply(port.mat, 2, function(v) forecast.SApt(monthly, v))
    
    weekly$CAMP.fore <- apply(port.mat, 2, function(v) forecast.CAPM(weekly, v))
    weekly$FF.fore <- apply(port.mat, 2, function(v) forecast.FF(weekly, v))
    weekly$SApt.fore <- apply(port.mat, 2, function(v) forecast.SApt(weekly, v))

    daily$CAMP.fore <- apply(port.mat, 2, function(v) forecast.CAPM(daily, v))
    daily$FF.fore <- apply(port.mat, 2, function(v) forecast.FF(daily, v))
    daily$SApt.fore <- apply(port.mat, 2, function(v) forecast.SApt(daily, v))
    
    Actual <- apply(port.mat, 2, actual)

    testingReady <- TRUE
}

sharpe <- function(portfolio, env) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)

	rf <- cleanse(RF*scaleRf(env), data[,1],  method="match") 
	rf <- rf[, 1]
	rf <- rf[-1]

	.exp <- mean(rets - rf)
	.std <- sd(as.vector(rets - rf))

	return (.exp/.std)


}

# weekly$sharpes <- apply(port.mat, 2, function(v) sharpe(v, weekly))
# daily$sharpes <- apply(port.mat, 2, function(v) sharpe(v, daily))
# monthly$sharpes <- apply(port.mat, 2, function(v) sharpe(v, monthly))
# testing$sharpes <- apply(port.mat, 2, function(v) sharpe(v, testing))


describeMat <- function(mat) {
    return (apply(mat, 2, function(v){
        describe(v)$counts;
    }));
}

geoMat <- function(mat) {
    return (apply(mat, 2, function(v){
	    prod(v+1)^(1/length(v))
	}))
}

bigPlot <- function() {
    for (freq in c(monthly, daily, weekly)) {
	    for (j in 1:6){
			x <- freq$CAMP.fore[, j]
			y <- Actual[, j]
			y <- y[-1]
			print(x)
			print(y)
			name = names(port.mat)[j]
	        plot(x, y, ylab="Actual", xlab="CAPM Forecast", main = name)	
		}
	}

}

bigPlot2 <- function() {
    for (freq in c(monthly, daily, weekly)) {
	    for (j in 1:6){
			x <- freq$FF.fore[, j]
			y <- Actual[, j]
			print(x)
			print(y)
			name = names(port.mat)[j]
	        plot(x, y, ylab="Actual", xlab="CAPM Forecast", main = name)	
		}
	}

}

bigPlot3 <- function() {
    for (freq in c(monthly, daily, weekly)) {
	    for (j in 1:6){
			x <- freq$SApt.fore[, j]
			y <- Actual[, j]
			print(x)
			print(y)
			name = names(port.mat)[j]
	        plot(x, y, ylab="Actual", xlab="CAPM Forecast", main = name)	
		}
	}

}

treynor <- function(portfolio, env) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)

	rf <- cleanse(RF*scaleRf(env), data[,1],  method="match") 
	rf <- rf[, 1]
	rf <- rf[-1]

	port2 <- t(as.matrix(portfolio))
	print(port)
	.betas <- as.matrix(monthly$CAPM.Betas[2,1:11 ])
	print(.betas)

	.exp <- mean(rets - rf)
	.beta <- t(port) %*% .betas

	return (.exp/.beta)


}
# testing$treynors <- apply(port.mat, 2, function(v) treynor(v, testing))

# png("plot7.png", width=700)
# x <- colnames(port.mat)
# y <- testing$treynors
# qplot(x=reorder(x, index(y)), y=y, main="Treynors's Measure", geom="bar", fill=factor(y))
# dev.off()

# port.betas <- apply(port.mat, 2, function(v){
#    b <- as.numeric(tb)	
#    return (t(as.matrix(b)) %*% as.matrix(v))		
# })

regroup <- sort(colnames(testing$data[, 1:11]), index.return=T)$ix

jenson <- function(portfolio, env){
    alphas = env$CAPM.Betas[1, 1:11]
	alphas <- as.matrix(alphas[regroup])
	return (t(alphas) %*% as.matrix(portfolio))
}

jensons <- apply(port.mat, 2, function(v) jenson(v, testing))

im <- function(portfolio, env) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
	rm <- rets[ ,1]
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)


	.exp <- mean(rets - rm)
	.std <- sd(as.vector(rets - rm))

	return (.exp/.std)

}

ims <- apply(port.mat, 2, function(v) im(v, testing))

m2 <- function(portfolio, env) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
	rm <- rets[ ,1]
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)

	rf <- cleanse(RF*scaleRf(env), data[,1],  method="match") 
	rf <- rf[, 1]
	rf <- rf[-1]


    r1 <- mean(rets - rf)
	r2 <- mean(rm - rf)

	std1 <- sd(rm)
	std2 <- sd(rets)

	


	return (r1*std1/std2 - r2)


}

m2s <- apply(port.mat, 2, function(v) m2(v, testing))

tm <- function(portfolio, env, p.value=F) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
	rm <- rets[ ,1]
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)

	rf <- cleanse(RF*scaleRf(env), data[,1],  method="match") 
	rf <- rf[, 1]
	rf <- rf[-1]


	r1 <- rets - rf
	bb <- rm - rf
	gg <- bb*bb
	


	if (p.value) {
	    return (summary(lm(r1~bb+gg))$coef[ ,4])
	}
	return (lm(r1~bb+gg)$coef)



}


hm <- function(portfolio, env, p.value=F) {

	data <- env$data;

    rets <- as.matrix(returns(data))[, 1:11]
	rets <- rets[-1, ]
    
	rm <- rets[ ,1]
    syms <- colnames(rets)
    port <- matrix(ncol=1, nrow=length(portfolio))

    for (i in 1:11) {
        port[i,1] <- portfolio[which(names(portfolio)==syms[i])] 
    }
     
    rets <- rets %*% as.matrix(port)

	rf <- cleanse(RF*scaleRf(env), data[,1],  method="match") 
	rf <- rf[, 1]
	rf <- rf[-1]


	r1 <- rets - rf
	r2 <- rm - rf
    f <- r2 < 0
	f <- -r2*f;

	if (p.value) {
	    return (summary(lm(r1~r2+f))$coef[ ,4])
	}

	return (lm(r1~r2+f)$coef)



}

hms <- apply(port.mat, 2, function(v) hm(v, daily))
hms2 <- apply(port.mat, 2, function(v) hm(v, weekly))
hms3 <- apply(port.mat, 2, function(v) hm(v, monthly))
phms <- apply(port.mat, 2, function(v) hm(v, daily, T))
phms2 <- apply(port.mat, 2, function(v) hm(v, weekly, T))
phms3 <- apply(port.mat, 2, function(v) hm(v, monthly, T))

allhms <- rbind(hms, hms2, hms3)
p.hms <- rbind(phms, phms2, phms3)


tms <- apply(port.mat, 2, function(v) tm(v, daily))
tms2 <- apply(port.mat, 2, function(v) tm(v, weekly))
tms3 <- apply(port.mat, 2, function(v) tm(v, monthly))
ptms <- apply(port.mat, 2, function(v) tm(v, daily, T))
ptms2 <- apply(port.mat, 2, function(v) tm(v, weekly, T))
ptms3 <- apply(port.mat, 2, function(v) tm(v, monthly, T))

alltms <- rbind(tms, tms2, tms3)
p.tms <- rbind(ptms, ptms2, ptms3)
