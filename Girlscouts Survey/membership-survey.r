###################################
#### Girlscouts Member Survery ####
###################################
# by:Yiran Sheng @ yiransheng.com #
###################################


library("Hmisc")
library("lmtest")
library("mlogit")
library("dgof")
library("timeSeries")

d1 <- read.csv("file1.csv", na.strings=c("","NA","N/A"))

### Initializing ####
opar <- par()
### Removing row 1 (question text) ###
d1.res <- d1[2:nrow(d1), ]
qids <- d1[1,]

## column one of data is unique ID, which cannot be NA (Corrupted data), thus will be removed
d1.res <- d1.res[!is.na(d1.res[,1]), ]
## Global Variables and functions
N = nrow(d1.res)

qcol <- function(i,j=F,text=F, qid="1") {
    if (!j){
    # quick reference for sub questions
        return (names(d1)>=paste("Q",i,"_",sep="") & names(d1)<=paste("Q",i,".Z",sep=""))
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


#####################################################################
#
#  General Notes:
#  * To get the description for a single column question, use
#    qids$Qx  x as the question integer id/number (10, 28...)
#  * To get the column range of a composite questions, use
#    qids[qcol(x)], x is the question integer id/number (12, 13...)
#  * All results, stats relevant for reporting purposes are either 
#    rendered as figures ("M_fig_****.png"), or appended to the list
#    output. Check output in the console mode of R to see details 
#    once the entire script is run.
#  * Store and backup this file with the data files (file1.csv etc.)
#
######################################################################

### Q46 How old are you? ####

dmgrph.age <- as.numeric( d1.res$Q46[!is.na(d1.res$Q46)] )
out <- describe(dmgrph.age)
output$Q46 <- out



png(filename="M_fig_age_summary.png")
hist(dmgrph.age, main="Respondants Age Distribution (Member Survey)", xlab="Age")
dev.off()

## Q6.1 Experience as a Girl Scout ##

dmgrph.levels.preset <- c("Daisy","Brownie","Junior","Cadette","Unspecified")
colrange <- which(names(d1.res) >= "Q6" & names(d1.res) <="Q6Z")
d1.level <- d1.res[,colrange]
dmgrph.levels.other <- unique(d1.level$Q6.1_5_TEXT)

dmgrph.levels <- c(dmgrph.levels.preset, as.character(dmgrph.levels.other))

dmgrph.levels <- unique(dmgrph.levels)
dmgrph.levels <- dmgrph.levels[!is.na(dmgrph.levels)]

q6col <- function(i,j,text=F){
    return (qcol(i,j,text,qid="6"))
}


level.counts <- c()
level.time <- c("<=1yr","2-3yr","4-5yr",">=6yr")
# troop size
level.size <- c("<=5","6-9","10-13",">=14")

for (i in 1:5){
    level.counts <- c(   level.counts, length(  which(  d1.res[[q6col(1,i)]]=="1")  )     )
    time <- d1.res[[q6col(2,i)]]
    time <- as.numeric(time[!is.na(time)])
    size <- d1.res[[q6col(3,i)]]
    size <- as.numeric(size[!is.na(size)])
    time_count <- c()
    size_count <- c()
    for (j in 1:4){
        time_count<-c(time_count, sum(as.numeric(time)==j))
        size_count<-c(size_count, sum(as.numeric(size)==j))
    }
    level.time <- cbind(level.time, time_count)
    
    level.size <- cbind(level.size, size_count)
}

level.size <- as.data.frame(level.size)
level.time <- as.data.frame(level.time)

names(level.size) <- c("", dmgrph.levels.preset)
names(level.time) <- c("", dmgrph.levels.preset)

row.names(level.size)<-level.size[,1]
level.size<-level.size[,2:ncol(level.size)]

row.names(level.time)<-level.time[,1]
level.time<-level.time[,2:ncol(level.time)]

tmp <- c(N-sum(level.counts), level.counts)
names(tmp) <- c("Not a Member", dmgrph.levels.preset)
dmgrph.levels.distr <- tmp/N*100



## plot 
png(filename="M_fig_memberhip_summary.png",width=500)
barplot(dmgrph.levels.distr, main="Girl Scout Levels Distribution", xlab="Level", ylab="Percentage")
dev.off()

q6plot <- function(data,main=NULL,xlab=NULL,ylab=NULL){
    i <- apply(data,2,as.numeric)
    row.names(i) <- row.names(data)
    total <- apply(i,2,sum)
    for (j in 1:ncol(i)){
        tmp <- i[,j]/total[j]*100
        print(tmp)
        barplot(tmp,main=paste(main,colnames(i)[j],sep=":"),xlab=xlab,ylab=ylab)
    }
    return (i)
}

# Oops, overides the R aggregate function here, too lazy to fix that, 
# made a copy of the original function. 

aggr <- aggregate

aggregate <- function(level.size){
    a <- apply(level.size,2,as.numeric)
    a <- apply(a,1,sum)
    a <- a/sum(a)*100
    a <- as.table(a)
    rownames(a)<-row.names(level.size)
    return (a)
}



png(filename="M_fig_memberhip_detail.png",width=1200,height=700)
par(mfrow=c(3,4))
q6plot(level.time[1:4],main="Girl Scout Membership Length by Level", xlab="Years",ylab="Percentage")
q6plot(level.size[1:4],main="Girl Scout Group Size by Level", xlab="Group Size",ylab="Percentage")
barplot(aggregate(level.time),main="Girl Scout Membership Length, Aggregating all Levels",xlab="Years",ylab="Percentage")
barplot(aggregate(level.size),main="Girl Scout Group Size, Aggregating all Levels",xlab="Group Size",ylab="Percentage")
dev.off()

par(mfrow=c(1,1))
#### Q25, why girls don't go camping with their troops ? ###
y<-d1.res$Q25
y<-y[!is.na(y)]

y.reasons <- c("I don't feel comfortable going without my parents","I don't like to go camping","I go to a different summer camp","I like doing activities outside, but I don't like camping","I would be away from home for too long","I'm scared to camp with people I don't know","My parents don't allow me to go camping without them","My troop has never been camping")

y.desc <- describe(y)
tmp <- rbind(y.reasons,y.desc$values)
tmp <- t(tmp)
colnames(tmp) <- c("Reasons why Never Gone Camping", "Frequency", "%")
output$Q25 <- tmp

#### Q20 ####

q20col <- function(i,j,text=F){
    return (qcol(i,j,text,qid="20"))
}
### formated data, from survey server 
data20 <- read.csv("report_q20.csv")
camp <- data20$Yes/data20$Responses
names(camp) <- data20$Question
camp <- camp*100

png(filename="M-camping-type.png", width=600)
par(oma=c(0,15,5,5),las=2)
barplot(camp,horiz=T,main="Girl Scouts Camping Experience by Type",xlab="Percentage of Girls for each type")
dev.off()

data20 <- read.csv("report_q20_2.csv")
d20.numeric <- apply(data20[3:9],2,as.numeric)
q20.normalize <- function(v){
    return (v/v[7])
}
d20.normalize <- apply(d20.numeric, 1, q20.normalize)
d20.time <- apply(d20.normalize,1,mean)[1:6]
d20.time <- d20.time*100
names(d20.time)<-c("1~2\ndays","3~4\ndays","5~6\ndays","1~2\nweeks","3~4\nweeks","Over\n1 month")
png(filename="M-camping-time.png", width=600)
par(opar)
barplot(d20.time, main="Girl Scouts Camping Experience Time Distribution",xlab="Percentage of girls for each duration")
dev.off()




### Outdoor activities Q9,Q23,Q12,Q13,Q15 ###
d23 <- d1.res[qcol(23)]
d23.clean <- apply(cleanse(d23), 2, as.numeric) 
q23.corr <- cor(d23.clean)

for (i in 1:nrow(q23.corr)){
    for (j in 1:ncol(q23.corr)){
        cor.test(d23.clean[,i], d23.clean[,j], method = c("pearson"))$p.value -> p
        if (p>0.05){
            q23.corr[i,j]<-q23.corr[j,i] <- 0
        }
    }
}

q23.model1 <- (Q23_5~Q23_1+Q23_2+Q23_3+Q23_4+Q23_6)
d23.clean <- as.data.frame(d23.clean)
q23.fit1 <- lm(q23.model1, data=d23.clean)
# q23.fit2 <- mlogit(q23.model1, data=d23.clean)



#### Q13, detailed activities analysis


### Formating Data    
d13.1 <- read.csv("report_q13_1.csv")
d13.2 <- read.csv("report_q13_2.csv")
d13.3 <- read.csv("report_q13_3.csv")

d13.all <- cbind(d13.1,d13.2,d13.3)
acts <- d13.all$Question
d13 <- d1.res[qcol(13)]
#done before
d13.db <- d13[,1:90]
#like
d13.lk <- d13[,91:180]
#with girl scouts
d13.wg <- d13[,181:270]

# 1 means have done it before, 2 means haven't done it before
d13.db.choices <- c(1,2)
# 1 means would like to do it with girl scouts, 2 means would not like to 
d13.wg.choices <- c(1,2,3)
# rating of activities on a 1-5 scale
d13.lk.choices <- c(1,2,3,4,5)


#### Chisq-test for done before vs. rating, cross-tab ####    
d13.dblk = list()
nind <- c()
nindc <- c()
nindc2 <- c()

z <- function(v){
    s <- sum(v)
    return (c(1:5)*v/s)
}
cat("These activities showed a improvement of rating, after being done:\n")
for (k in 1:90) {
    label <- as.character(k)
    d13.dblk[[label]] <-matrix(ncol=2,nrow=5)
    for (i in 1:2){
	for (j in 1:5){
	    filter = d13.db[,k]==d13.db.choices[i] & d13.lk[,k] == d13.lk.choices[j]
	    filter.nomissing <- !is.na(filter)
	    filter = filter & filter.nomissing
	    n <- sum(filter)
	    d13.dblk[[label]][j,i] = n
	}
    }
    p<-chisq.test(d13.dblk[[label]])$p.value
    if (!is.nan(p) & p<0.05){
        m <- d13.dblk[[label]]
	d13.dblk.diff <- nind <- c(nind, k)
	ALL <- apply(m,2,sum)
	five <- m[5, ]/ALL
	change2 <- five[1] - five[2]
	m <- apply(m,2,z)
	change <- apply(m,2,sum)
	change <- (change[1] - change[2])  
	d13.dblk.avg <- nindc <- c(nindc, change)
	d13.dblk.five <- nindc2 <- c(nindc2, change2)
        cat(acts[as.numeric(k)], "\nGirls love it ",change," ", change2, "\n")
    }
}

cat("END of Done Before vs. Ratings, \n\n\n ######################\n")
m <- matrix(ncol=6, nrow=length(nind))
for (j in 1:length(nind)){
    a <- d13.dblk[[j]]
    m[j,1] <- sum(z(a[, 1])) 
    m[j,2] <- sum(z(a[, 2])) 
    five <- a[5, ]/apply(a,2,sum)
    m[j,3] <- five[1]
    m[j,4] <- five[2]
    m[j,5] <- - m[j,2] + m[j,1]
    m[j,6] <- - m[j,4] + m[j,3]
}

d13.dblk.tab <- as.table(m)
rownames(d13.dblk.tab) <- acts[nind]
colnames(d13.dblk.tab) <- c("Avg Rating, Done Before","Avg Rating, Haven't Done Before", "5 Star %, Done Before", "5 Star %, Haven't Done Before", "Improvement in Avg Rating", "Improvement in 5 Star %")

#### Chisq-test for done before with girl scouts vs. rating, cross-tab ####    
d13.wglk = list()
nind <- c()
nind2 <- c()
nindc <- c()
nindc2 <- c()
nindc3 <- c()
nindc4 <- c()
cat("These activities showed a improvement of rating, after being done with Girl Scouts:\n")
for (k in 1:90) {
    label <- as.character(k)
    d13.wglk[[label]] <-matrix(ncol=3,nrow=5)
    for (i in 1:3){
	for (j in 1:5){
	    filter = d13.wg[,k]==d13.wg.choices[i] & d13.lk[,k] == d13.lk.choices[j]
	    filter.nomissing <- !is.na(filter)
	    filter = filter & filter.nomissing
	    n <- sum(filter)
	    d13.wglk[[label]][j,i] <- n
	}
    }
    p<-chisq.test(d13.wglk[[label]])$p.value
    if (!is.nan(p) & p<0.05){

	tmp <- m <- d13.wglk[[label]]
	m <- apply(m,2,z)
	ALL <- apply(tmp,2,sum)
	if (ALL[3]>40){
	    d13.wglk.diff <- nind <- c(nind, k)
	    five <- m[5, ]/ALL
            change21 <- change2 <- five[3] - five[1]
	    d13.wglk.five_wl <- nindc2 <- c(nindc2, change2)
            change22 <- change2 <- five[3] - five[2]
	    d13.wglk.five_wn <- nindc3 <- c(nindc3, change2)
            change <- apply(m,2,sum)
            change11 <- (change[3] - change[1])  
	    d13.wglk.avg_wl <- nindc <- c(nindc, change11)
            change12 <- (change[3] - change[2])  
	    d13.wglk.avg_wn <- nindc4 <- c(nindc4, change12)
	    cat(acts[as.numeric(k)], ":->\nGirls' reactions:")
            cat("\nChange in avg rating vs. would like to do it with gs: ", change11)
            cat("\nChange in avg rating vs. would not like to do it with gs: ", change12)
            cat("\nChange in five star % vs. would like to do it with gs: ", change21)
            cat("\nChange in five star % vs. would not like to do it with gs: ", change22)
	    cat("\n------------------------------------------------------------------------\n")
	}
    }
}

cat("END of Done with Girl Scouts vs. Ratings, \n\n\n ######################\n")

m <- matrix(ncol=6, nrow=length(nind))
for (j in 1:length(nind)){
    a <- d13.wglk[[j]]
    m[j,1] <- sum(z(a[, 1])) 
    m[j,2] <- sum(z(a[, 3])) 
    five <- a[5, ]/apply(a,2,sum)
    m[j,3] <- five[1]
    m[j,4] <- five[3]
    m[j,5] <- m[j,2] - m[j,1]
    m[j,6] <- m[j,4] - m[j,3]
}

d13.wglk.tab1 <- as.table(m)
rownames(d13.wglk.tab1) <- acts[nind]
colnames(d13.wglk.tab1) <- c("Avg Rating, Would Like","Avg Rating, Already Done with Girl Scouts", "5 Star %, Would Like", "5 Star %, Already Done with Girl Scouts", "Improvement in Avg Rating", "Improvement in 5 Star %")

m <- matrix(ncol=6, nrow=length(nind))
for (j in 1:length(nind)){
    a <- d13.wglk[[j]]
    m[j,1] <- sum(z(a[, 2])) 
    m[j,2] <- sum(z(a[, 3])) 
    five <- a[5, ]/apply(a,2,sum)
    m[j,3] <- five[2]
    m[j,4] <- five[3]
    m[j,5] <- m[j,2] - m[j,1]
    m[j,6] <- m[j,4] - m[j,3]
}

d13.wglk.tab2 <- as.table(m)
rownames(d13.wglk.tab2) <- acts[nind]
colnames(d13.wglk.tab2) <- c("Avg Rating, Would Not Like","Avg Rating, Already Done with Girl Scouts", "5 Star %, Would Not Like", "5 Star %, Already Done with Girl Scouts", "Improvement in Avg Rating", "Improvement in 5 Star %")


output$Q13.1 <- d13.dblk.tab
output$Q13.2 <- d13.wglk.tab1
output$Q13.3 <- d13.wglk.tab2


#### Q10 and Q28 Comparision 

colrange <- which(names(d1.res) == "Q10" | names(d1.res) =="Q28")
d10_28 <- d1.res[, colrange]
rowrange <- !( is.na(d10_28[,1]) | is.na(d10_28[, 2]) )
d10_28.clean <- cmp <- apply(d10_28[rowrange, ], 2, as.numeric)

Change.of.Attitude <- as.table(rbind(c(-4:4), hist(cmp[,2] - cmp[,1])$count))

output$Q10_Q28.COA <- Change.of.Attitude
output$Q10_Q28.TEST <- cvm.test(cmp[,2],ecdf(cmp[,1]), simulate.p.value=T)

#### Q12 

colrange <- qcol(12)
d12 <- d1.res[, colrange]
filter <- function(v){
    sum(as.numeric(!is.na(v) & v=="1"))
}
d12.count <- apply(d12, 2, filter)

d12.count <- d12.count/sum(d12.count) * 100

d12.tab <- as.table(cbind(as.character(acts), d12.count))
row.names(d12.tab) <- c()
colnames(d12.tab) <- c("Outdoor Activities", "Overall Preference Score")

output$Q12 <- d12.tab

#### Q29

d29 <- read.csv("report_camp_opts.csv")
