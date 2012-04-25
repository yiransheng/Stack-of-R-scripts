############################################################
# R Code for Basic Stock Analysis
# Written by Yiran Sheng @ Chrysanthemumcapital.com
############################################################

## Get Data 
fetch <- function( symb, ibeg=FALSE, iend=FALSE, makedataframe = FALSE ){
    o <- get.hist.quote(symb, quote='Open')
    c <- get.hist.quote(symb, quote='Close')
    a <- get.hist.quote(symb, quote='AdjClose')
    cut <- function(s, d, start=TRUE){
        ds <- as.Date(index(s))
        tmp <- s
        if (start){
            try ( tmp <- s[ds>=as.Date(d)] )
        }
        else{
            try ( tmp <- s[ds<=as.Date(d)] )
        }
        return (tmp) 
    }
    if (ibeg != FALSE){
        o <- cut(o, ibeg, TRUE)
        c <- cut(c, ibeg, TRUE)
        a <- cut(a, ibeg, TRUE)
    }
    if (iend != FALSE){
        o <- cut(o, iend, FALSE)
        c <- cut(c, iend, FALSE)
        a <- cut(a, iend, FALSE)
    }
    
    results <- cbind(o,c,a)
    if (makedataframe){
        results <- as.data.frame(results)       
    }
    return(results)

}



rtn <- function ( v, period = 1, takelog = FALSE, vix = FALSE){
#######################################################
# Calculate the return series for a given price series
# Take vector or data.frame as input
# Adjust return with vix, if vix is supplied as a vector
######################################################
    if (class(v) == 'numeric') { s = v }
    else if (class(v) == 'data.frame') {
	s = v$Adj.Close
    }
    else {
	s <- as.vector( v )
    }
    
    if (period == 1){
	sNA <- NA
	sLG <- c(sNA, s)
        vi <- vix
        if (vix) {
	    vi <- vix
	}
    }
    else {
	n <- ceiling( length(s) / period )   
	b <- c(1, rep(0, period - 1))
	l <- rep(b, n)
	l <- l[1: length(s)]
	s <- s[l == 1]
	sNA <- NA
	sLG <- c(sNA, s)
	if (vix) {
	    vi <- vix[l == 1]
	} 
    }

    sLG <- sLG[1:length(s)]

    if (takelog) {
	r <- log(s) - log(sLG)
    }
    else {
	r <- ( s - sLG )/ sLG
    }
    if (vix) {
        r <- r/vi*(as.numeric(mean(vi)))
    }

    return (r)
}

hrtn <- function ( v, timespan = 1 ){
###################################################
# Google Finance Style Normalizing Price Series
###################################################

    if (class(v) == 'numeric') { s = v }
    else if (class(v) == 'data.frame') {
	s = v$Adj.Close
    }
    else {
	s <- as.vector( v )
    }
    ibeg <- length(s) - timespan
    iend <- length(s)
    s <- s[ibeg: iend]
    r <- s / as.numeric(s[1])
    r <- r - 1

    return (r) 
}


volesti <- function ( v, period = 30, vix = FALSE ){
###############################################################
# Estimating the volatility based on different time interval
###############################################################

    sigma <- rep(0,period)	
    for (i in 1:period){
		r <- rtn(v, period = i, takelog = TRUE, vix = vix)
		r <- r[!is.na(r)]
		sigma[i] <- sqrt(var(r))*sqrt(252/i)
	}
    return (sigma)
}

rtnesti <- function ( v, period = 30, vix = FALSE ){
###############################################################
# Estimating the expected return based on different time interval
###############################################################

    rt <- rep(0,period)	
    for (i in 1:period){
		r <- rtn(v, period = i, takelog = TRUE, vix = vix)
		r <- r[!is.na(r)]
		rt[i] <- mean(r)*252/i
	}
    return (rt)
}



rwtest <- function ( v, q=2, takelog = TRUE ){
##################################################################################
# The Random Walk Test for Stock Series                                      #####
##################################################################################
# Based on Lo and Mankinlay (A Random Walk Down Wall Street)                 #####
##################################################################################
    if (class(v) == 'numeric') { s = v }
    else if (class(v) == 'data.frame') {
	s = v$Adj.Close
    }
    else {
	s <- as.vector( v )
    }
    
    if (takelog){
        s <- log(s)
    }
    
    n <- floor((length(s)-1)/q)
    iend <- n*q + 1
    s <- s[1:iend]
    

# Estimating the mean
    mu <- as.numeric(mean(diff(s)))
# Estimating sigma_a^2
    tmp <- diff(s)
    s_a <- var(tmp)
    print (s_a)
# Estimating sigma_c^2
    m <- q*(n*q - q + 1)*(1 - (1/n))
    sNA <- rep(NA, q)
    sLG <- c(sNA, s)
    sLG <- sLG[1:length(s)]
    e <- (s - sLG) - q*mu
    e <- e[!is.na(e)]
# e is a vector contains the value of X_t - X_{t-q} - q*mu
    e <- as.matrix(e)
    SE <- t(e) %*% e
    s_c <- SE/m
    s_c <- as.numeric(s_c)
    print(s_c)
# Calculatin M-statistics, and standize
    mstat <- s_c/s_a -1
    z <- sqrt(n*q)*mstat/(sqrt(2*(2*q-1)*(q-1)/(3*q)))
print ('Z-stat for the Test is:')
    return (z)
}



hrwtest <- function ( v, q=2, takelog = TRUE, returnvar = FALSE ){
##################################################################################
# The Random Walk Test for Stock Series, allowing for hecteroskedatic series #####
##################################################################################
# Based on Lo and Mankinlay (A Random Walk Down Wall Street)                 #####
##################################################################################
    if (class(v) == 'numeric') { s = v }
    else if (class(v) == 'data.frame') {
	s = v$Adj.Close
    }
    else {
	s <- as.vector( v )
    }
    
    if (takelog){
        s <- log(s)
    }
    
    n <- floor((length(s)-1)/q)
    iend <- n*q + 1
    s <- s[1:iend]
    

# Estimating the mean
    mu <- as.numeric(mean(diff(s)))
# Estimating sigma_a^2
    tmp <- diff(s)
    s_a <- var(tmp)
    print ('Variance - 1:')
    print (s_a)
# Estimating sigma_c^2
    m <- q*(n*q - q + 1)*(1 - (1/n))
    sNA <- rep(NA, q)
    sLG <- c(sNA, s)
    sLG <- sLG[1:length(s)]
    e <- (s - sLG) - q*mu
    e <- e[!is.na(e)]
# e is a vector contains the value of X_t - X_{t-q} - q*mu
    e <- as.matrix(e)
    SE <- t(e) %*% e
    s_c <- SE/m
    s_c <- as.numeric(s_c)
    print ('Variance - 2:')
    print(s_c)
# Calculatin M-statistics, and standize
    mstat <- s_c/s_a -1
    z <- sqrt(n*q)*mstat/(sqrt(2*(2*q-1)*(q-1)/(3*q)))
    print ('Z1:')
    print(z)

# Estimating delta_j and theta
theta_q <- 0
for (j in 1:(q-1)){
    ib <- j+2 
    ie <- n*q+1
    su <- 0
    for ( k in ib:ie ){
        tmp <- ( (s[k]-s[k-1]-mu)^2 )*( (s[k-j]-s[k-j-1]-mu)^2 )
        su <- su+tmp
    }
    su <- su*n*q
    sd <- (s_a * n*q )^2
    theta_q <- theta_q + (su/sd)*( (2*(q-j)/q)^2 )
}
# Calculating Z-stat
    z <- sqrt(n*q)*mstat/sqrt(theta_q)
    print ('Z2:')
    print(z)
    if ( returnvar ){
        return(max(s_a, s_c))
    } else {
        return (z)
    }
}

# Volatility 
hist.vol <- function( tk, backward = 100 ){
    dateNow <- Sys.Date()
    vols <- rep(0, backward)
    for (j in 1:backward){
        ibeg <- dateNow - j*7
        print (ibeg)
        s <- get.hist.quote(tk, quote='AdjClose', compression='w', start = ibeg)
        vols[j] <- sqrt(  hrwtest(s, 4, returnvar = TRUE) * 52  )
    }
    return (vols)
}

############################################
# Cointegration Test For Spread Trade
# Very Simple, very. 
############################################

coint <- function( tk1, tk2, startdate = FALSE, span = 'd' ){
    #library(zoo)
    #library(tseries)
    #library(timeDate)

    if ( startdate == FALSE ){
        startdate = Sys.Date() - 500
    }

	g1 <- get.hist.quote(tk1, quote = 'AdjClose', start = startdate, compression = span )
	g2 <- get.hist.quote(tk2, quote = 'AdjClose', start = startdate, compression = span )

	t.zoo <- merge(g1, g2, all=FALSE)
	t <- as.data.frame(t.zoo)

	cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")

	m <- lm(g1 ~ g2 + 0, data=t)
	beta <- coef(m)[1]

	cat("Assumed hedge ratio is", beta, "\n")

	sprd <- g1 - as.numeric(beta) * g2     

	ht <- adf.test(sprd, alternative="stationary", k=0)

	cat("ADF p-value is", ht$p.value, "\n")

	if (ht$p.value < 0.05) {
	    cat("The spread is likely mean-reverting\n")
	} else {
	    cat("The spread is not mean-reverting.\n")
        }

        sprd2 <- sprd - min(sprd) + .5

        hrwtest(sprd2, 4)
        return (sprd)
}

############################
# Spread Construction ######
############################

sprd2 <- function (v1, v2, hedgeratio = 'reg' ){

## v1 and v2 need to have the same length
## sprd = v1 - v2 * hedgeratio

    if (class(v1) == 'numeric') { s1 = v1 }
    else if (class(v1) == 'data.frame') {
	s1 = v1$Adj.Close
    }
    else {
	s1 <- as.vector( v1 )
    }
    if (class(v2) == 'numeric') { s2 = v2 }
    else if (class(v2) == 'data.frame') {
	s2 = v2$Adj.Close
    }
    else {
	s2 <- as.vector( v2 )
    }
    
    if (hedgeratio == 'reg'){
        m <- lm(s1 ~ s2 + 0)
	beta <- coef(m)[1]
    }
    else if (hedgeratio == 'init'){
        beta <- as.numeric(s1[1]/s2[1])
    }
    else {
        beta <- hedgeratio
    }

    cat("Assumed hedge ratio is", beta, "\n")
    
    
    sprd <- s1 - s2 * as.numeric(beta)
    ht <- adf.test(sprd, alternative="stationary", k=0)
    cat("ADF p-value is", ht$p.value, "\n")

	if (ht$p.value < 0.05) {
	    cat("The spread is likely mean-reverting\n")
	} else {
	    cat("The spread is not mean-reverting.\n")
        }

    return (sprd)
}


##########################
## TS Rolling Forecast
##########################

rfcst <- function ( v, fcst=FALSE, ahead = 5 ){
## fcst is the forecast function, which takes a vector and rolling period as parameters
    if (class(v) == 'numeric') { s = v }
    else if (class(v) == 'data.frame') {
	s = v$Adj.Close
    }
    else {
	s <- as.vector( v )
    }
    mv0 <- c(s, c(1:ahead))
    mv0 <- mv0*NA
    mv <- mv0 
    for (i in 2: ahead){
        mv <- cbind(mv, mv0) 
    }
    if (fcst == FALSE){
        fcst <- function (v, a){
            fit <- auto.arima(v)
            f <- as.vector((forecast(fit)$mean))
            f <- f[1:a]
            return (f)
        }
    }
    for (j in 10:length(v)){
        smp <- s[1:j]
        for (k in 1:ahead){
            mv[j+k,k] <- fcst(smp, ahead)[k]
        } 
    }
    plot(mv[ ,1], type='l')
    for (k in 2:ahead){
        lines(mv[,k])
    }
    lines(s, col='red')
    return (mv)
}

tsstrat <- function ( s, strat ){
    y <- as.vector(s)
    y <- y[!is.na(y)]
    n <- length(y)
    init <- 30
    p0 <- 1
    cash <- max(abs(y))*10
    v0 <- p0*sr[init] + cash
    vv <- c(v0)
    ibeg <- 1
    for (i in init:n){        
        v <- y[ibeg:i]
        f <- auto.arima(v)
        fcst <- forecast(f)$mean
        mr <- rep(y[i], length(fcst))
        p <- sign(fcst - mr)
        if (sum(p)>0){
            fcst2 <- forecast(f)$lower[ ,1]
            os <- (fcst - fcst2)/fcst
            os <- abs(os)
            p <- p*os
        }
        else{
            fcst2 <- forecast(f)$upper[ ,1]
            os <- (fcst - fcst2)/fcst
            os <- abs(os)
            p <- p*os
        }
        #cat(i, '\n', p, '\n')
        new <- p*strat
        cash <- as.numeric( cash - sr[i] * as.numeric(sum(new)) )
        p0 <- as.numeric( p0 + sum(new) )
        vi <- p0 * y[i] + cash
        if ( (vi-v0)/v0< -.03  ){
            cash <- cash + p0*y[i]
            p0 <- 0
            vi <- p0 * y[i] + cash
            v0 <- vi
            ibeg <- ibeg + 10
        }
        if ( (vi - v0)/v0 > .1){
            v0 <- vi
        }
        vv <- c(vv, vi)
        rr <- rtn(vv)
        rr <- rr[!is.na(rr)]
        ddown <- sign(rr[length(rr)])
        while (ddown < 0 & (length(rr)+ddown)>0 & r[length(rr)+ddown]<0){
            ddown <- ddown - 1
            if (ddown < -5){
                ibeg <- ibeg + 1
            }            
        }
        cat('\n Down Days:', ddown, 'Data Range:', ibeg, 'Position: ', p0, 'Account Value:', vi)        
    }
    return (vv)
}

##########################
# Leveraged ETF Model
##########################

etest <- function( v1, v2 ){
    v <- rep(0, length(v1))
    dangerAlert <- - hrtn(v1, length(v1)) * hrtn(v2, length(v2))
    beta <- v1[1]/v2[1]
    a <- v1[1]
    for (t in 1:length(v1)){        
        r1 <- rtn(v1[1:t])
        r2 <- rtn(v2[1:t])
        r1 <- r1[!is.na(r1)]
        r2 <- r2[!is.na(r2)]
        r <- (r1 - r2)/2
        mu1 <- as.numeric(mean(r1))
        mu2 <- as.numeric(mean(r2))
        mu <- as.numeric(mean(r))
        vol <- as.numeric(var(r))
        v[t] <- a * exp(-.5 * vol * t )
        v[t] <- v[t]*( (1+mu1)^t + (1+mu2)^t ) 
        dangerAlert[t] <- dangerAlert[t] /t /t
        dangerAlert[t] <- dangerAlert[t] * 2 * choose(t,2)
    }
    cat (beta, mu1, mu2, mu) 
    rlizd <- v1 + v2*as.numeric(beta)
    difr <- rlizd - v
    plot(cbind(v1,v2,difr, v,rlizd,dangerAlert))

    fit <- lm(rlizd ~ v + 0)
    print(fit)
    difr <- as.vector(difr)
    difr <- difr[!is.na(difr)]
    print(acf(as.vector(difr), plot = FALSE))
    return(v)

}

sprdetf <- function ( r, vol, t){
    f1 <- exp(-.5* (vol^2) * t / 252 )
    f2 <- ( 1 + r)^t + (1-r)^t
    return (2 - f1*f2)
}

siml <- function(mu, vol, t, noise = 0.01, scl = 3 ){
    e <- vol^2/252
    e <- as.numeric(sqrt(e))
    r1 <- rnorm(t, mu, e)
    eps <- rnorm(t, 0, e*noise)
    r2 <- -r1 + eps
    f1 <- c(-1)
    f2 <- c(-1)
    for (j in 1:t){
        z1 <- f1[length(f1)]
        z2 <- f2[length(f2)]
        f1 <- c(f1, z1*(1+r1[j]))
        f2 <- c(f2, z2*(1+r2[j]))
    }
    f <- f1+f2
    if (scl){
        f <- c(-scl, f, -1.5)
    }
    return(f)
}

######## Error Correction Model #########

ecm <- function( v1, v2, takelog=TRUE, lag=1 ){

        if (ncol(v1)>1) {
            yy <- as.vector(v1[ ,1])
            xx <- as.vector(v1[ ,2])
        } else {
            xx <- as.vector(v1)
            yy <- as.vector(v2)
        }

        yy <- yy/yy[1]
        xx <- xx/xx[1]
     
        if (takelog){
            xx <- log(xx)
            yy <- log(yy)
        }

        diffxy <- yy - xx
        zmodel1 <- Lag(diffxy)
        diffxy <- Lag(diffxy)
        if (lag > 1) {
            for (k in 2:lag){
                diffxy <- Lag(diffxy)
                zmodel1 <- cbind(zmodel1, diffxy)
            }
            zmodel1 <- zmodel1[2:nrow(zmodel1), ]
        } else {
            zmodel1 <- zmodel1[2:length(zmodel1)]
        }
 
        names(zmodel1) <- paste(rep("lag",lag), c(1:lag), sep=".")

        zmodel2 = -1 * zmodel1
        
	ymodel1 = diff(xx)
	ymodel2 = diff(yy)


	model1 = lm(ymodel1 ~ zmodel1)
	model2 = lm(ymodel2 ~ zmodel2)

	pv1 <- coef(summary(model1))[2,4]
        pv2 <- coef(summary(model2))[2,4]
        print(summary(model1))
        print(summary(model2))
        coint <- c(pv1, pv2)
        names(coint) <- c("p-value.1","p-value.2")

	return(coint)
}


b <- 0
signal1 <- 0.08
signal2 <- -0.1
a <- c(b)

n <- length(r1)
w <- 1
p <-c(w)
pbench <- pair1[ ,1]
pbench <- pbench/332
for (i in 1:n){
    w <- w+a[i]*w*(r1[i]) 
    if (r2[i]-r1[i]>signal1) {
        tmp <- 2
    } 
    else if (r2[i]-r1[i]<signal2) {
        tmp <- 0
    }
    else {
        tmp <-0.1
    }
    a <- c(a, tmp)   
    p <- c(p,w) 
}
comp <- cbind(pbench, p)




