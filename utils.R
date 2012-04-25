
################################################
### Authored by Yiran Sheng @ yiransheng.com
################################################


################################################
### namespace
################################################

utils = new.env()

################################################
### Short cuts for commonly used functions
################################################

utils$as.n <- as.numeric
utils$as.c <- as.character
utils$as.m <- as.matrix
utils$as.d <- as.data.frame
utils$as.t <- as.table
utils$as.a <- as.array
utils$is.nna <- function (x) !is.na(x)

utils$doc <- function(package) {
# use: doc(base), without quote
    .text <- deparse(substitute(package))
    do.call(library, list(help=.text))
}


################################################
### Custom "or" / "||" operator, 
### capable of checking if a variable is defined
### assign value only if the variable is undefined
### or NA or FALSE or NULL.  
### Example: 
###   data <- data %||% read.csv("largefile.csv")
################################################

utils[["%||%"]] <- function(e1, e2, except=c(NA,F)) {

    exception <- tryCatch( is.null(e1) ||                # handles NULL   
	                   any(apply(array(except), 1, identical, y=e1)),  
	                   error=function(e) T)
    if (exception) {
        return (e2)
    } else {
        return (e1)
    }

}

################################################
### Miscs
################################################

utils$cache <- function(variable) {
    if ( !("./cache" %in% list.dirs()) ) {
        dir.create("cache")
    }
    save(list = variable,
         envir = .GlobalEnv,
         file = file.path('cache',
         paste(variable, '.RData', sep = '')))
}
utils$cache.get <- function(variable) {
    filename = file.path('cache', 
	       paste(variable, '.RData', sep = ''))
    load(filename, envir = .GlobalEnv)
}

utils$whichever <- function(indices, 
	                    size=length(indices), 
			    inverse = F) {
    # reverse which
    ret = rep(inverse,size)
    ret[indices] = !inverse
    return (ret)
}

utils$dump.na <- function(x) x[!is.na(x)]

utils$clean.matrix <- function(x) {
# remove all rows containing NA in a matrix like object
    keep <- apply(x,2,utils$is.nna)
    keep <- apply(keep,1,prod) == 1
    return (x[keep,])
}

utils$cleanse <- function(...){
# Format data, returns a matrix
    args <- list(...)
    method = utils[["%||%"]](args$method, "trunctuate")
    Index = utils[["%||%"]](args$index, index)
    args <- args[!(names(args) %in% c("method", "index"))]
    args <- lapply(args, as.matrix)
    args.length <- length(args)
    x <- args[[1]]
    if (args.length==1){
        return (utils$clean.matrix(x))
    }
    if (method == "match"){
        indices <- Index(x)
    } else if (method == "trunctuate") {
        min_row <- nrow(x) 
    } else {
        stop("method not supported...")
    }
    for (i in 2:args.length) {
	if (method == "trunctuate") {
            min_row <- min(min_row, nrow(args[[i]])) 
            x <- cbind(x[1:min_row, ], args[[i]][1:min_row, ]) 	
	} else if (method == "match") {
	    y <- args[[i]]
	    intersectx <- indices %in% Index(y) 
	    intersecty <- Index(y) %in% indices 
	    x <- cbind(x[intersectx, ], y[intersecty, ])
	    indices <- indices[intersectx]
	}
    }

    utils$clean.matrix(x)
}


if ("utils" %in% search()) detach("utils")

attach(utils)
