#### R Code For analyzing AAPL vs. QCOM ####

library("Hmisc")
library("tseries")

## Global Variables
date_start <- '2011-01-01'
date_end <- format(Sys.time(), "%Y-%m-%d")

subperiods <- c('2011-10-01','2012-01-13')

subs <- function(k,s) {
    v <- index(s)
    nodes <- c(v[1], as.Date(subperiods), tail(v,1))
    b <- k+1
    a <- k
    v_cut <- v[(v<=nodes[b] & v>nodes[a])]
    return (v_cut)
}

## matches the time index of all returned symbols
getsymbols <- function(syms, freq="d", quote="AdjClose") {
    raw <- get.hist.quote(syms[1], start=date_start, end=date_end, compression=freq, quote=quote)
    dates <- index(raw)
    for (sym in syms[2:length(syms)]){
        tmp <- get.hist.quote(sym, start=date_start, end=date_end, compression=freq, quote="Close")
        dates <- dates[dates %in% index(tmp)] 
        raw = raw[dates]
        tmp = tmp[dates]
        raw <- cbind(raw, tmp)
    }
    names(raw) <- syms
    return (raw)
}

get_return <- function(data){
    ptmp <- as.matrix(data)
    ret <- (ptmp[2:nrow(ptmp),] - ptmp[1:nrow(ptmp)-1,])/ptmp[1:nrow(ptmp)-1,]
    return (ret)
}


## get prices 
pair <- getsymbols(c("aapl","qcom"))
sub1 <- pair[index(pair)%in%subs(1,pair)]
sub2 <- pair[index(pair)%in%subs(2,pair)]
sub3 <- pair[index(pair)%in%subs(3,pair)]
sub23 <- rbind(sub2, sub3)



ret <- get_return(pair)
ret1 <- get_return(sub1)
ret2 <- get_return(sub2)
ret3 <- get_return(sub3)
ret23 <- get_return(sub23)

ret <- as.data.frame(ret)
ret1 <- as.data.frame(ret1)
ret2 <- as.data.frame(ret2)
ret3 <- as.data.frame(ret3)
ret23 <- as.data.frame(ret23)
names(ret) <- names(pair)
names(ret1) <- names(pair)
names(ret2) <- names(pair)
names(ret3) <- names(pair)
names(ret23) <- names(pair)


model1 <- aapl ~ qcom
fit <- lm(model1, data=ret)
fit1 <- lm(model1, data=ret1)
fit2 <- lm(model1, data=ret2)
fit3 <- lm(model1, data=ret3)
fit4 <- lm(model1, data=rbind(ret2,ret3))


ecmtable <- function(pair){
    ret<-ecm(pair)

    return (xtable(as.table(ret)))
} 


###

pos <- rep(0, length(hr))
for (i in 1:length(hr)){
    if(!is.na(lar[i]) & lar[i]>=0.0111){
        pos[i] = -1
    } else if (!is.na(lar[i]) & lar[i]<=-0.0106){
        pos[i] = 1
    }
}

