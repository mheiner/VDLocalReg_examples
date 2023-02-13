require("MASS")

datname = "Boston"
vars_keep = c("rm", "crim", "lstat", "nox", "tax", "age", "indus", "rad")
p_use = length(vars_keep)

y = Boston$medv
ysc = as.vector(scale(y))
(N = length(y))

Xsc = scale(Boston)
(nx = length(Xsc[, vars_keep]))

mis_indx = sample(nx, size=floor(nx*perc_miss/100.0), replace=FALSE) # will be different for each replicated data set
Xsc[, vars_keep][mis_indx] = NA
Xsc
mean(is.na(Xsc[, vars_keep]))

n_tn = 400
(n_tt = N - n_tn)

indx_tn = sample(N, size=n_tn, replace=FALSE) |> sort()
head(indx_tn); tail(indx_tn)

indx_remaining = setdiff(1:N, indx_tn)
indx_tt = sample(indx_remaining, size=n_tt, replace=FALSE) |> sort()
rm(indx_remaining)
head(indx_tt); tail(indx_tt)

nmis = apply(Xsc, 1, function(x) sum(is.na(x)))