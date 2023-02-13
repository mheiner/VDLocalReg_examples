n_tn = 500
n_tt = 1000

indx_tn = sample(N, size=n_tn, replace=FALSE, prob=(nmis+1)) |> sort()
table(nmis[indx_tn])
table(mispat[indx_tn])
head(indx_tn); tail(indx_tn)

indx_remaining = setdiff(1:N, indx_tn)
indx_tt = sample(indx_remaining, size=n_tt, replace=FALSE, prob=(nmis[indx_remaining] + 1)) |> sort()
rm(indx_remaining)

table(nmis[indx_tt])
table(mispat[indx_tt])
head(indx_tt); tail(indx_tt)
