indicator.f <- function(X, wantvif=FALSE) {
#
# data X is a matrix, first column contains y, remaining ones the covariates
#
# First, eliminate rows with NAs (if any)
#
  X <- X[!is.na(rowSums(X)),] 
  p <- ncol(X)-1
#
# Now use mclust. Number of clusters in maxclust, cluster indicators in clusters
# and cluster sizes in clust.sizes
#
  if(! "mclust" %in% tolower((.packages()))){
  library("mclust")
   }
  if(! "car" %in% tolower((.packages()))){
    library("car")
   }
  out <- Mclust(X)
  maxclust <- out$G
  clusters <- out$classification
  clust.sizes <- table(clusters)
  panic <- FALSE
  if (maxclust!=length(clust.sizes)) { # weird case, break!
    panic <- TRUE
  }
#
# For each cluster with size > p+1 fit regression
# 
  if (panic) {
    output <- NULL
  }
  else {
    regr.list <- NULL
    clustsizes.list <- NULL
    R2.list <- NULL
    adj.R2.list <- NULL
    if(wantvif==TRUE) vif.list <- NULL
    clust.index <- 0
    for (i in 1:maxclust) {
      if (clust.sizes[i]>p+1) {
        clust.index <- clust.index + 1
        y.aux <- X[clusters==i,1]
        x.aux <- X[clusters==i,-1]
        data.aux <- data.frame(y.aux,x.aux)
        regr.aux <- lm(y.aux ~ ., data=data.aux)
        f.summary <- summary(regr.aux)$fstatistic
        regr.list <- c(regr.list,1-pf(f.summary[1],f.summary[2],f.summary[3]))
        clustsizes.list <- c(clustsizes.list,clust.sizes[i])
        R2.list <- c(R2.list, summary(regr.aux)$r.squared)
        adj.R2.list <- c(adj.R2.list, summary(regr.aux)$adj.r.squared)
        if(wantvif==TRUE) vif.list <- cbind(vif.list, vif(regr.aux))
 #     browser()
      }
    }
    ave.pvalue <- sum(regr.list*clustsizes.list)/sum(clustsizes.list)
    ave.R2 <- sum(clustsizes.list*R2.list)/sum(clustsizes.list)
    ave.adj.R2 <- sum(clustsizes.list*adj.R2.list)/sum(clustsizes.list)
    if(wantvif==TRUE) ave.vif <- rowSums(vif.list%*%clustsizes.list/sum(clustsizes.list))
    if(wantvif==TRUE) {
	    output <- list("regr.pvalues"=regr.list,
		     "R2"=R2.list,
		     "adj.R2"=adj.R2.list,
                 "cluster.sizes"=clustsizes.list,
                 "VIF.list"=vif.list,
                 "average.pvalue"=ave.pvalue,
                 "average.R2"=ave.R2,
                 "average.adj.R2"=ave.adj.R2,
                 "average.VIF"=ave.vif)
	  }
    else {
	  output <- list("regr.pvalues"=regr.list,
		       "R2"=R2.list,
		       "adj.R2"=adj.R2.list,
                   "cluster.sizes"=clustsizes.list,
                    "average.pvalue"=ave.pvalue,
                 "average.R2"=ave.R2,
                 "average.adj.R2"=ave.adj.R2)
    }
  }
  return(output)
#
# The output includes p-values for the regression significance test in all valid cases,
# clusters sizes, and an average of the p-values, weighted by cluster size.
# It also computes similar quantities based on R^2 coefficient.
  # Exception for weird case: return NULL
  #
}
