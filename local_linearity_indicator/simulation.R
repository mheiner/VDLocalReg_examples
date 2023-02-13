set.seed(1234)
#
# Simulated data: two predictors (x1 & x2) and 4 blocks. Blocks are driven
# by x1 only. Response is quadratically linked to x1 and linearly to x2,
# and I add a lot of noise. A third variable is just noise.
#
source("indicator.R")
nsim <- 500
n <- 200
pvalue.sim1.list <- NULL
R2.sim1.list <- NULL
adjR2.sim1.list <- NULL
for (ii in 1:nsim) {
  x1 <- seq(-2,2,length=n)
  x2 <- runif(n,min=-3, max=3)
  x3 <- runif(n)
  y <- rep(0,n)
  for (i in 1:trunc(n/4)) {
    y[i] <- rnorm(1,mean=1+10*x1[i]^2+x2[i]/2, sd=10)
  }
  for (i in (trunc(n/4)+1):(trunc(n/2)))  {
    y[i] <- rnorm(1,mean=2+10*x1[i]^2-x2[i], sd=6)
  }
  for (i in (trunc(n/2)+1):(trunc(3*n/4)))  {
    y[i] <- rnorm(1,mean=-1-20*x1[i]^2+2*x2[i], sd=10)
  }
  for (i in (trunc(3*n/4)+1):n)  {
    y[i] <- rnorm(1,mean=-2+20*x1[i]^2-2*x2[i], sd=8)
  }
#
# Got data; now compute indicator
#
  X <- cbind(y,x1,x2,x3)
  ind.sim1 <- indicator.f(X, wantvif=FALSE)
  if (!is.null(ind.sim1)) {
    pvalue.sim1.list <- c(pvalue.sim1.list, ind.sim1$average.pvalue)
    R2.sim1.list <- c(R2.sim1.list, ind.sim1$average.R2)
    adjR2.sim1.list <- c(adjR2.sim1.list, ind.sim1$average.adj.R2)
    cat(ii,fill=TRUE)
  }
}
#
# Another simulation: x1, x2 and x3 as earlier, but now neither is related to y
#
nsim <- 500
n <- 200
pvalue.sim2.list <- NULL
R2.sim2.list <- NULL
adjR2.sim2.list <- NULL
for (ii in 1:nsim) {
  x1 <- seq(-2,2,length=n)
  x2 <- runif(n,min=-3, max=3)
  x3 <- runif(n)
  y <- rep(0,n)
  for (i in 1:trunc(n/4)) {
    y[i] <- rnorm(1,mean=20, sd=10)
  }
  for (i in (trunc(n/4)+1):(trunc(n/2)))  {
    y[i] <- rnorm(1,mean=-20, sd=6)
  }
  for (i in (trunc(n/2)+1):(trunc(3*n/4)))  {
    y[i] <- rnorm(1,mean=30, sd=10)
  }
  for (i in (trunc(3*n/4)+1):n)  {
    y[i] <- rnorm(1,mean=-30, sd=8)
  }
  #
  # Got data; now compute indicator
  #
  X <- cbind(y,x1,x2)
  ind.sim2 <- indicator.f(X, wantvif=FALSE)
  if (!is.null(ind.sim2)) {
    pvalue.sim2.list <- c(pvalue.sim2.list, ind.sim2$average.pvalue)
    R2.sim2.list <- c(R2.sim2.list, ind.sim2$average.R2)
    adjR2.sim2.list <- c(adjR2.sim2.list, ind.sim2$average.adj.R2)
    cat(ii,fill=TRUE)
  }
}

#
# One more: three predictors (x1 & x2 & x3) and 4 blocks. Blocks are driven
# by x1 only. Response is quadratically linked to x2 and linearly to x3,
# and I add a lot of noise, so clustering variable is unrelated to y
#
source("indicator.R")
nsim <- 500
n <- 200
pvalue.sim3.list <- NULL
R2.sim3.list <- NULL
adjR2.sim3.list <- NULL
for (ii in 1:nsim) {
  x1 <- seq(-2,2,length=n)
  x2 <- runif(n,min=-3, max=3)
  x3 <- runif(n)
  y <- rep(0,n)
  for (i in 1:trunc(n/4)) {
    y[i] <- rnorm(1,mean=1+10*x2[i]^2+x3[i]/2, sd=10)
  }
  for (i in (trunc(n/4)+1):(trunc(n/2)))  {
    y[i] <- rnorm(1,mean=2+10*x2[i]^2-x3[i], sd=6)
  }
  for (i in (trunc(n/2)+1):(trunc(3*n/4)))  {
    y[i] <- rnorm(1,mean=-1-20*x2[i]^2+2*x3[i], sd=10)
  }
  for (i in (trunc(3*n/4)+1):n)  {
    y[i] <- rnorm(1,mean=-2+20*x2[i]^2-2*x3[i], sd=8)
  }
  #
  # Got data; now compute indicator
  #
  X <- cbind(y,x1,x2,x3)
  ind.sim3 <- indicator.f(X, wantvif=FALSE)
  if (!is.null(ind.sim1)) {
    pvalue.sim3.list <- c(pvalue.sim3.list, ind.sim3$average.pvalue)
    R2.sim3.list <- c(R2.sim3.list, ind.sim3$average.R2)
    adjR2.sim3.list <- c(adjR2.sim3.list, ind.sim3$average.adj.R2)
    cat(ii,fill=TRUE)
  }
}


library("ggplot2")

R2df <- data.frame(scenario=as.factor(c(rep(1, length(R2.sim1.list)), rep(2, length(R2.sim2.list)), rep(3, length(R2.sim3.list)))), R2=c(R2.sim1.list, R2.sim2.list, R2.sim3.list))

ggplot(R2df, aes(x=scenario, y=R2, color=scenario)) + geom_boxplot()


pval_df <- data.frame(scenario=as.factor(c(rep(1, length(pvalue.sim1.list)), rep(2, length(pvalue.sim2.list)), rep(3, length(pvalue.sim3.list)))), pvalue=c(pvalue.sim1.list, pvalue.sim2.list, pvalue.sim3.list))

ggplot(pval_df, aes(x=scenario, y=pvalue, color=scenario)) + geom_boxplot()


adjR2df <- data.frame(scenario=as.factor(c(rep(1, length(adjR2.sim1.list)), rep(2, length(adjR2.sim2.list)), rep(3, length(adjR2.sim3.list)))), adjR2=c(adjR2.sim1.list, adjR2.sim2.list, adjR2.sim3.list))

ggplot(adjR2df, aes(x=scenario, y=adjR2, color=scenario)) + geom_boxplot()


