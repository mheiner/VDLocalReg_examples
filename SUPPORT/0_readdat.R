
# Here is some data back ground given by the authors.
#
# The Study to Understand Prognoses and Preferences for
# Outcomes and Risks of Treatments (SUPPORT) was a
# multi-center, two phase study, of 9105 patients.  The
# primary goal of the study was to model survival over a
# 180-day period in seriously ill hospitalized adults.  A
# component of the SUPPORT prognostic model was the SUPPORT
# day 3 Physiology score (SPS), a risk score created to
# account for various sources of health varition and
# co-morbidities.  The SPS can range from 0 to 100 and is a
# function of the following covariates (all of
# which are included in their analysis).
#
# partial pressure of oxygen in the arterial blood (pafi)
# mean blood pressure (meanbp)
# white blood counts (wblc)
# albumin (alb)
# APACHE III repiration score (resp)
# temperature (temp)
# heart rate per minute (hrt)
# bilirubin (bili)
# creatin (crea)
# sodium (sod)
#
# The response is (sps)

dat0 <- read.csv("support2.csv", header=TRUE)[,c("sps", "pafi", "meanbp", "wblc", 
                 "alb", "resp", "temp", "hrt", "bili", "crea", "sod")]
#
# According to the authors there are 23 patterns ranging
# N = 3842 to N = 1 patients that display them.
#
# There is one subject with missing response and another
# with all missing covariates.
#
# I anticipate removing the subject with a missing response.
#
#
#
# Fit the PPMx Missing model to the data


## process and explore
datname = "SUPPORT"

dim(dat0)
indx_y_miss = which(is.na(dat0$sps))
dat0[indx_y_miss,]

dat = dat0[-indx_y_miss,]
dim(dat)

(p = ncol(dat) - 1)
X = dat[,2:(p+1)]
Xsc = scale(X)
ysc = scale(dat[,1])[,1]

(N = length(ysc))
stopifnot(nrow(Xsc) == N)

nmis = apply(X, 1, function(x) sum(is.na(x)))
table(nmis)

apply(X, 2, function(x) 100.0*mean(is.na(x)))

mispat = factor( apply(X, 1, function(x) paste0(which(is.na(x)), collapse="-")) )
table(mispat)
levels(mispat)


if (p_use == 6) {
  vars_keep = c("meanbp", "hrt", "bili", "pafi", "crea", "alb")
} else if (p_use == 10) {
  vars_keep = 1:p  
}

nmis = apply(X[, vars_keep], 1, function(x) sum(is.na(x)))
table(nmis)
apply(X[, vars_keep], 2, function(x) 100.0*mean(is.na(x)))
mispat = factor( apply(X[, vars_keep], 1, function(x) paste0(which(is.na(x)), collapse="-")) )
table(mispat)
levels(mispat)
