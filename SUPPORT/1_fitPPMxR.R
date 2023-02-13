args <- commandArgs(TRUE)
# args <- c("Mean", 6, 1.0, 0.2, "NNiChisq_indep", 0.05, 1, 220709)

modtype <- as.character(args[1]) # one of "Mean", "Reg"
p_use <- as.numeric(args[2]) # number of covariates
alph <- as.numeric(args[3]) # cohesion concentration parameter
sigupper <- as.numeric(args[4]) # upper limit on error variance
sim_type <- as.character(args[5]) # similarity type
s0 <- as.numeric(args[6]) # point estimate of similarity stdev
ii <- as.numeric(args[7]) # data replicate (subset seed)
datenow <- as.numeric(args[8]) # date (fitting seed)

(mesg = paste0("modtype", modtype, "_p", p_use, "_alph", alph, "_sigupper", sigupper, "_simType", sim_type, "_s0_", s0, "_ii", ii, "_date", datenow))


## for running PPMxR
library("JuliaCall")

julia = julia_setup(JULIA_HOME="../../julia-1.8.5/bin")
# julia = julia_setup(JULIA_HOME="/Applications/Julia-1.8.app/Contents/Resources/julia/bin/") # local path

julia_install_package_if_needed("https://github.com/mheiner/ProductPartitionModels.jl")
julia_install_package_if_needed("Dates")
julia_installed_package("ProductPartitionModels")
julia_library("ProductPartitionModels")
julia_library("Dates")
source("../PPMx_JuliaFunctions.R")


## get data
source("0_readdat.R")
set.seed(ii)
source("0_subsampleData.R")
set.seed(datenow + ii)


## MCMC setup
n_draws <- 100e3
n_burn <- 50e3
n_thin <- 50
(n_keep <- (n_draws - n_burn) / n_thin)


### fit PPMxR
base_init = list(mu0=0.0, sig0=0.5, tau0=0.1, sig_upper=sigupper)
base_prior = list(mu0_mean=0.0, mu0_sd=2.0, sig0_upper=5.0)

if (sim_type == "NN") {
} else if (sim_type == "NNiG_indep") {
} else if (sim_type == "NNiChisq_indep") {
  similarity_vals = list(m0=0.0, sc_prec0=0.1, nu0=4.0, s20=s0^2, type=sim_type)
}

PPMxR = fit_PPMx(ysc[indx_tn], Xsc[indx_tn, vars_keep], Xsc[indx_tt, vars_keep], # Xpred = NULL for no prediciton
                 nburn = n_burn, nkeep = n_keep, nthin = n_thin, # add thinning
                 pred_insamp = TRUE,
                 # progressfile=paste0("PPMx_progress/", datafile, ".txt"),
                 sampling_model = modtype,
                 cohesion = list(alpha = alph), 
                 similarity = similarity_vals,
                 baseline = base_init, 
                 baseline_prior = base_prior, 
                 upd_beta = TRUE,
                 y_grid = ysc[indx_tt], crossxy = FALSE
)

PPMxR$resid = colMeans(PPMxR$Ypred_insamp) - ysc[indx_tn]
PPMxR$predErr = colMeans(PPMxR$Pred[[1]]$Ypred) - ysc[indx_tt]

PPMxR$msqErr = mean(PPMxR$resid^2)
PPMxR$msqPredErr = mean(PPMxR$predErr^2)

PPMxR$mabsErr = mean(abs(PPMxR$resid))
PPMxR$mabsPredErr = mean(abs(PPMxR$predErr))

PPMxR$mld = mean(PPMxR$logDens_insamp)
PPMxR$mpld = mean(PPMxR$Pred[[1]]$PPlogDens)

PPMxR$resid_qtile = sapply(1:n_tn, function(i) (rank(c(ysc[indx_tn][i], (PPMxR$Ypred_insamp[,i])))[1] - 0.5) / (nrow(PPMxR$Ypred_insamp) + 1) )
PPMxR$pred_qtile = sapply(1:n_tt, function(i) (rank(c(ysc[indx_tt][i], (PPMxR$Pred[[1]]$Ypred[,i])))[1] - 0.5) / (nrow(PPMxR$Pred[[1]]$Ypred) + 1) )



PPMxR$n_keep = n_keep
PPMxR$n_thin = n_thin
rm(n_keep, n_thin)

mse = PPMxR$msqErr
mspe = PPMxR$msqPredErr

mae = PPMxR$mabsErr
mape = PPMxR$mabsPredErr

mld = PPMxR$mld
mpld = PPMxR$mpld

ksd = ks.test(PPMxR$resid_qtile, "punif")$statistic
kspd = ks.test(PPMxR$pred_qtile, "punif")$statistic


cat("\n\nIn-sample MSE:", mse, "\n")
cat("Out-of-sample MSE:", mspe, "\n")

cat("\n\nIn-sample MlogDens:", mld, "\n")
cat("Out-of-sample MpredLogDens:", mpld, "\n")

save(mse, mspe, mae, mape, mld, mpld, ksd, kspd, mesg, indx_tn, indx_tt, p_use, nmis, ii, datenow, datname, 
     file=paste0("postsim/summary_", datname, "_PPMxR_", mesg, ".rda"))
save(PPMxR, mesg, indx_tn, indx_tt, p_use, nmis, ii, datenow, file=paste0("postsim/fit_", datname, "_PPMxR_", mesg, ".rda"))


quit(save="no")
