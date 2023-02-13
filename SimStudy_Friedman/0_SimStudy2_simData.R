library("mice") # necessary for the ampute function use version 3.12.0

# number of covariates.
# ncov <- 10
ncov <- p_use

# missType <- "MNAR"
# missType \in {"MAR", "MNAR"}

# perMiss <- 0.1
# perMiss \in {0, 0.1, 0.25, 0.5}


# nobs <- 100

sessionInfo()


cat("perMiss = ", perMiss, "\n")
cat("missType = ", missType, "\n")
cat("dataType = ", data.type, "\n")

set.seed(( ( (data.type-1)*2 + ifelse(missType=="MAR", 1, 2)-1)*3 + 4*perMiss)*nobs + ii)

cat("seed = ", ( ( (data.type-1)*2 + ifelse(missType=="MAR", 1, 2)-1)*3 + 4*perMiss)*nobs + ii, "\n")
cat("ndata =================================================================== ", ii, "\n")
print(date())

X <- matrix(runif(2*nobs*ncov, 0, 1), nrow=2*nobs, byrow=TRUE)


Xall <- Xmiss <- X

# select datapoints that will be classified as missing
if(perMiss > 0){
  miss.patterns <- 1-diag(ncov)
  miss.prob.type <- sample(c("LEFT","MID","TAIL","RIGHT"),ncov, replace=TRUE)
  for(cc in 1:(ncov)){
    Xmiss[,cc] <- ampute(Xall, prop=perMiss, mech=missType, patterns=miss.patterns[cc,], type=miss.prob.type[cc])$amp[,cc]
  }
  missmat <- matrix(0, nrow=2*nobs, ncol=(ncov))
  missmat[is.na(Xmiss)] <- 1
}


train <- sample(1:(2*nobs), nobs)
Xmat <- Xmiss[train,]
Xpred <- Xmiss[-train,]


if(data.type==1){
  y <- 10*sin(pi*(X[,1]*X[,2])) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5] + rnorm(2*nobs)
}
if(data.type==2){
  y <- 10*sin(pi*(X[,1]*X[,2])) + 20*(X[,3] - 0.5)^2 + 10*X[,4] + 5*X[,5] + rnorm(2*nobs, 0, exp(X[,1]))
}

ytn <- y[train]
ytt <- y[-train]

n_tn <- length(ytn)
n_tt <- length(ytt)

nmis <- apply(Xmiss, 1, function(x) sum(is.na(x)))
datname <- "SimStudy2"
