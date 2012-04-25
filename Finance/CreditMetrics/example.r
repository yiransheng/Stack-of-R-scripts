require("ggplot2")

CM_grades <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "Default")

CM_trans_mat_1y <- matrix(ncol=8, nrow=7)
row.names(CM_trans_mat_1y) <- CM_grades[1:7]
colnames(CM_trans_mat_1y)  <- CM_grades[1:8]

CM_trans_mat_1y[1, ] <- c(90.81, 8.33, 0.68, 0.06, 0.12, 0.00, 0.00, 0.00) / 100
CM_trans_mat_1y[2, ] <- c(0.70, 90.65, 7.79, 0.64, 0.06, 0.14, 0.02, 0.00) / 100
CM_trans_mat_1y[3, ] <- c(0.09, 2.27, 91.05, 5.52, 0.74, 0.26, 0.01, 0.06) / 100
CM_trans_mat_1y[4, ] <- c(0.02, 0.33, 5.95, 86.93, 5.30, 1.17, 0.12, 0.18) / 100
CM_trans_mat_1y[5, ] <- c(0.03, 0.14, 0.67, 7.73, 80.53, 8.84, 1.00, 1.06) / 100
CM_trans_mat_1y[6, ] <- c(0.00, 0.11, 0.24, 0.43, 6.48, 83.46, 4.07, 5.20) / 100
CM_trans_mat_1y[7, ] <- c(0.22, 0.00, 0.22, 1.30, 2.38, 11.24, 64.86, 19.79) / 100

CM_recovery <- matrix(ncol=2, nrow=5)
CM_recovery[1, ] <- c(53.80, 26.86) / 100
CM_recovery[2, ] <- c(51.13, 25.45) / 100
CM_recovery[3, ] <- c(38.52, 23.81) / 100
CM_recovery[4, ] <- c(32.74, 20.18) / 100
CM_recovery[5, ] <- c(17.09, 10.90) / 100

CM_FZC <- matrix(ncol=4, nrow=7)
CM_FZC[1, ] <- c(3.60, 4.17, 4.73, 5.12) / 100
CM_FZC[2, ] <- c(3.66, 4.22, 4.78, 5.17) / 100
CM_FZC[3, ] <- c(3.72, 4.32, 4.93, 5.32) / 100
CM_FZC[4, ] <- c(4.10, 4.67, 5.25, 5.63) / 100
CM_FZC[5, ] <- c(5.55, 6.02, 6.78, 7.27) / 100
CM_FZC[6, ] <- c(6.05, 7.02, 8.03, 8.52) / 100
CM_FZC[7, ] <- c(15.05, 15.02, 14.03, 13.52) / 100

row.names(CM_FZC) <- CM_grades[1:7]

bond1 <- list(grade="BB", coupon=0.05, maturity=5, par=100, seniority=1) # senior secured
bond2 <- list(grade="AA", coupon=0.06, maturity=3, par=100, seniority=1) # senior secured

# at the end of first year
credit.metrics <- function(grade, coupon, maturity, par, seniority) {

    CM <- list()
    id <- which(CM_grades==grade)
    probs <- CM_trans_mat_1y[id, ]
    cash_flow <- rep(par*coupon, maturity)
    cash_flow[maturity] <- cash_flow[maturity] + par
    Value <- c()
    for (i in 1:7){
        fzc <- c(0, CM_FZC[i, ])
	fzc <- fzc[1:maturity]
	Value <- c(Value, pv(cash_flow, fzc))
    }

    onDefault <- CM_recovery[seniority, ]
    Value <- c(Value, onDefault[1]*par)

    CM$id <- id
    CM$probs <- probs
    CM$Value <- Value
    CM$Recovery.std <- onDefault[2]
    CM$Default.prob <- tail(probs[], 1)

    return(CM)

}

pv <- function(cash_flow, rates) {
    if (length(cash_flow) != length(rates)) return 
    time <- c(1:length(cash_flow))
    time <- time - 1
    df <- (1+rates)^(-time)
    pvs <- df*cash_flow
    sum(pvs)
}


CM.bond1 <- do.call(credit.metrics, bond1)
CM.bond2 <- do.call(credit.metrics, bond2)

Value <- CM.bond1$Value
Probs. <- CM.bond1$probs
CM.bond1$Value.mean <- Value.mean <- sum(Value * Probs.)
Value.var <- sum((Value-Value.mean)^2*Probs.)
CM.bond1$Value.adjStd <- sqrt( Value.var+CM.bond1$Default.prob*(CM.bond1$Recovery.std*bond1$par)^2 )

Value <- CM.bond2$Value
Probs. <- CM.bond2$probs
CM.bond2$Value.mean <- Value.mean <- sum(Value * Probs.)
Value.var <- sum((Value-Value.mean)^2*Probs.)
CM.bond2$Value.adjStd <- sqrt( Value.var+CM.bond2$Default.prob*(CM.bond2$Recovery.std*bond2$par)^2 )


# construct portfolio data

## Assuming normal distribution of year end values

Portfolio.Value.mean <- CM.bond1$Value.mean + CM.bond2$Value.mean
Portfolio.Value.adjStd <- sqrt(CM.bond1$Value.adjStd^2 + CM.bond2$Value.adjStd^2)

## Calculate var assuming normal distribution of bond values

.nVar <- function(prob) {
    qnorm(prob, mean=Portfolio.Value.mean, sd=Portfolio.Value.adjStd)
} 

Value <- c()
Probs. <- c()
for (i in 1:8){
    for (j in 1:8){
        Value <- c(Value, CM.bond1$Value[i]+CM.bond2$Value[j])
        Probs. <- c(Probs., CM.bond1$probs[i]*CM.bond2$probs[j])
    }
}


Probs. <- Probs.[sort(Value, index.return=T)$ix]
Probs. <- as.vector(Probs.)

n <- length(Probs.)


Value <- sort(Value)


m <- diag(rep(0,n))
m[lower.tri(m, diag=T)] <- 1
Probs.cumu <- m %*% as.matrix(Probs.)
Probs.cumu <- as.vector(Probs.cumu)

.Var <- function(prob) {
    if (prob <0 || prob>1) return 
    .these <- which(Probs.cumu >= prob)
    .this <- .these[1] 
    cat("Index: ", .this, "\n")
    Value[.this]
}

CDF <- function(value) {
    those <- which(Value<=value)
    prob <- sum(Probs.[those]) 
    prob
}

PDF <- function(value1, value2=value1-10) {
    prob1 <- CDF(value1)
    prob2 <- CDF(value2)
    abs(prob1-prob2)
}



#### Plots

png(filename="plot1.1.png")
b <- CM.bond1$probs
qplot(x=reorder(CM_grades, c(1:length(b))), y=b, geom="bar", fill=factor(b), xlab="Letter Grade", ylab="Prob. Bond1")
dev.off()

png(filename="plot2.2.png")
b <- CM.bond2$probs
qplot(x=reorder(CM_grades, c(1:length(b))), y=b, geom="bar", fill=factor(b), xlab="Letter Grade", ylab="Prob. Bond2")
dev.off()


png(filename="plot3.png")
b <- Value
qplot(y=b, geom="bar", fill=factor(b), xlab="Senarios", ylab="Portfolio Value")
dev.off()

png(filename="plot4.png")
zx <- seq(50, 250, by=.2)
zy <- apply(array(zx), 1, CDF)
zy <- as.vector(zy)
qplot(x=zx, y=zy, xlab="Value", ylab="CDF")
dev.off()

png(filename="plot6.png")
zx <- seq(50, 250, by=10)
zy <- apply(array(zx), 1, PDF)
zy <- as.vector(zy)
qplot(x=zx, y=zy, xlab="Value", geom=c("smooth","bar"),  ylab="PDF")
dev.off()



x <- c()
y <- c()
for (i in seq(1,100, by=0.1)){
    x <- c(x, .Var(i/100))  
    y <- c(y, .nVar(i/100))
}

x <- (x[1:length(x)-1] - Portfolio.Value.mean) / Portfolio.Value.adjStd 
y <- (y[1:length(y)-1] - Portfolio.Value.mean) / Portfolio.Value.adjStd 

png("qq.png")
plot(y,x, main="QQPlot", xlab="Red line indicates Normal Distribution", ylab="")
lines(y,x)
lines(y, y, type="l", col="red")
dev.off()
