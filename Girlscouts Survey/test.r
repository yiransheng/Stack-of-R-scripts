### Custom OR operator
"%||%" <- function(e1, e2, except=c(NA,F)) {

    exception <- tryCatch( is.null(e1) ||                # handles NULL   
	                   any(apply(array(except), 1, identical, y=e1)),  
	                   error=function(e) T)
    if (exception) {
        return (e2)
    } else {
        return (e1)
    }

}


syms <- tk[,1] %||% c("SPY","AAPL","GOOG","QQQ","ORCL")

raw <- raw %||% (function(start){
    hdr <- c(syms[1])
    ret <- NULL
    d1 <- get.hist.quote(syms[1], quote="Volume", start=start)	
    timeStamps <- index(d1)
    for (j in 2:length(syms)){
        d <- tryCatch(get.hist.quote(syms[j], quote="Volume", start=start), 
	              error = function(e) F)
	if (!identical(d, F)){
	    d <- d[index(d) %in% timeStamps]
	    timeStamps <- timeStamps[timeStamps %in% index(d)]
	    d1 <- d1[timeStamps]
	    if (is.null(ret)){
		ret <- cbind(d1,d)
	    } else {
		ret <- cbind(ret,d)	
	    }
	    d1 <- d
	    hdr <- c(hdr, as.character(syms[j]))
	}
    }
    names(ret) <- hdr
    return (ret)
})("2011-01-01")

nraw <- nrow(raw)

formation <- raw[1:floor(nraw/2), ]
testing <- raw[ceil(nraw/2):nraw, ]

avg.vol <- apply(formation, 2, mean)


nws <- list()
cm <- c()
cm.each <- names(raw)
for (i in 1:nrow(testing)){
    el1 <- c()
    el2 <- c()
    for (j in 1:ncol(testing)){
        for (k in 1:ncol(testing)){
            if (testing[i,j]>=avg.vol[j]*1.2 &
		(j != k) &
		testing[i,k]>=avg.vol[k]*1.2) {
	        el1 <- c(el1, j)
	        el2 <- c(el2, k)
	    }
	}
    }
    if (length(el1)){
	e.l <- cbind(el1, el2)
	e.l <- as.matrix(e.l)
	nws[[i]] <- a <- graph.edgelist(e.l,directed=F)
	cent <- evcent(a)$vector   
	cm <- c(cm, abs(skewness(cent)))
    } else {
        cm <- c(cm, 0) 
    }
}

