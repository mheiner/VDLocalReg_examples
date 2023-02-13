#
# Example of using function indicator.f with Boston Housing data
#
rm(list=ls())
set.seed(230123)

source("indicator.R")
# library("mclust")
# ?mclustModelNames


library(MASS)

## Pattern sub
dat.ps = read.csv("../SUPPORT/support2.csv", head=TRUE)[,c("sps", "pafi", "meanbp", 
    "wblc", "alb", "resp", "temp", "hrt", "bili", "crea", "sod")]
head(dat.ps)

(vars_keep = 1:ncol(dat.ps))
vars_keep = c("sps", "meanbp", "hrt", "bili", "pafi", "crea", "alb")

X_ps = as.matrix(dat.ps[, vars_keep])
head(X_ps)
sum(apply(X_ps, 1, function(x) !any(is.na(x))))

ind.ps = indicator.f(X_ps)
print(ind.ps)


## Boston (BARTm paper example)
require("MASS")
vars_keep = c("rm", "crim", "lstat", "nox", "tax", "age", "indus", "rad")

Xc = scale(Boston)
Xuse = Xc[,c("medv", vars_keep)]

ind.values = indicator.f(Xuse)
print(ind.values)
