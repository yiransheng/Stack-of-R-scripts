
################################################
### namespace
################################################

utils = new.env()

################################################
### Short cuts for commonly used functions
################################################

util$as.n <- as.numeric
util$as.c <- as.character


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


