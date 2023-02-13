library("MASS")

datname = "Old_Faithful"

dat0 = geyser

lags = embed(dat0$waiting, 3)

tail(dat0)
tail(lags)

dat = data.frame(y=lags[,1], waiting1=lags[,2], waiting2=lags[,3], duration1=dat0$duration[-c(1, nrow(dat0))])
head(dat); head(dat0)
tail(dat); tail(dat0)

dat$duration1[which(dat$duration1 %in% c(2.0, 3.0, 4.0))] = NA

y = dat$y + runif(length(dat$y), -1.0, 1.0) # to avoid identical rounded values
y_mn = mean(y)
y_sd = sd(y)

ysc = scale(y)[,1]

X = dat[,-1]
X_mn = colMeans(X, na.rm=TRUE)
X_sd = apply(X, 2, sd, na.rm=TRUE)

Xsc = scale(X)

Xsc

(N = length(y))

n_tn = 200
(n_tt = N - n_tn)

indx_tn = sample(N, size=n_tn, replace=FALSE) |> sort()
head(indx_tn); tail(indx_tn)

indx_remaining = setdiff(1:N, indx_tn)
indx_tt = sample(indx_remaining, size=n_tt, replace=FALSE) |> sort()
rm(indx_remaining)

head(indx_tt); tail(indx_tt)
nmis = apply(Xsc, 1, function(x) sum(is.na(x)))

if (p_use == 3) {
  vars_keep = c("waiting1", "waiting2", "duration1")
} else if (p_use == 2) {
  vars_keep = c("waiting1", "duration1")
} else if (p_use == 1) {
  vars_keep = c("waiting1", "waiting2")
}
