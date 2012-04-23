###################################
####   Troop Leader Survery    ####
###################################
# by:Yiran Sheng @ yiransheng.com #
###################################


library("Hmisc")
library("lmtest")
library("mlogit")
library("dgof")
library("timeSeries")
library("network")
library("RMallow")
library("flexclust")


### Initializing ####
opar <- par()


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

d1 <- d1 %||% read.csv("file2.csv", na.strings=c("","NA","N/A"))

### Removing row 1 (question text) ###
d1.res <- d1.res %||% d1[2:nrow(d1), ]
qids <- qids %||% d1[1,]
qtext <- qtext %||% apply(qids, 1, as.character)

## Column one of data is unique ID, which cannot 
## be NA (Corrupted data), thus will be removed
d1.res <- d1.res[!is.na(d1.res[,1]), ]
## Global Variables and functions
N = nrow(d1.res)

qcol <- function(i,j=F,text=F, qid="1") {
    if (!j){
    # quick reference for sub questions
	return (names(d1)>=paste("Q",i,"_",sep="") & 
		names(d1)<=paste("Q",i,".Z",sep=""))
    }
    label <- paste(i,j, sep="_")
    if (text) label <- paste(label, "TEXT", sep="_")
    label <- paste("Q",qid,".",label, sep="")
    return (label)
}

cleanse <- function(data){
    keep <- apply(data,2,is.na)
    keep <- !keep
    keep <- apply(keep,1,prod) == 1
    return (data[keep,])
}

output <- list()


#### Q27 Ranking Data for Camping Opts

colrange <- qcol(27)
ignore <- grep("<strong>", qtext[colrange])
d27 <- d1.res[, colrange]
d27.bold <- d27[, col(d27[1,])==ignore]
d27.reg <- d27[, col(d27[1,])!=ignore]

opts <- qtext[colrange]
ids <- c(1:length(opts))
opts <- opts[which(ids!=ignore)]
opts <- gsub("Below is a list of items you selected as being an important part of your camping experience. By sele...-","",opts)

## for unranked activities, assign random ranks to them 
## after all the ranked activities
format_row <- function(v) {
    s <- as.numeric(v)
    ranked <- unique(s)
    ranked <- ranked[!is.na(ranked)]
# randomize ranks
    not_ranked <- is.na(s)
    n <- c(1:sum(not_ranked))
    if (length(ranked)){
        n <- n+max(ranked)
    } else {
        return (rep(NA, length(v)))
    }
    s[not_ranked] <- sample(n, sum(not_ranked))

    return (s)
}


# Monte-Carlo Simulation
d27.ran <- d27.ran %||% (function(e) { 
    
    x <- matrix(ncol=ncol(d27.reg), nrow=e)

    for (i in 1:e) {
	d27.ran <- t(apply(d27.reg, 1, format_row))
	keep <- apply(d27.ran, 1, any)
	keep <- !is.na(keep)
	d27.ran <- d27.ran[keep, ]
	x[i, ] <- apply(d27.ran, 2, mean)
    }

    return (d27.ran)

})(1)

b <- d27.ran

d27.clust <- d27.clust %||% kcca(b,k=5, family=kccaFamily(dist=AllKendall))
d27.clust2 <- d27.clust2 %||% kcca(b,k=2, family=kccaFamily(dist=AllKendall))

e <- attr(d27.clust, "centers")

list_center <- function(v) {
    opts[sort(v, index.return=T)$ix]
}

d27.centers <- apply(e,1,list_center)

write.csv(d27.centers, file="ranking_centers.csv")


#### Q16, challenges 

colrange <- qcol(16)

d16 <- d1.res[, colrange]
d16.count <- function(v) {
    s <- v[!is.na(v)]
    length(s)
}

d16.regr <- d16.regr %||% apply(as.matrix(d1.res), 2, as.numeric)

d16.regr$CHA <- apply(d16,1,d16.count)


d16.model <- (CHA~Q5+Q4+Q6+Q10)
