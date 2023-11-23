args <- commandArgs(TRUE)

# args <- c("MCAR", "Additive", "Reg", 2, 1.0, 1.0, "NNiChisq_indep", 0.2, 1, 230829)

missType <- args[1]
dataType <- args[2]

modtype <- as.character(args[3]) # one of "Mean", "Reg"
p_use <- as.numeric(args[4]) # number of covariates
alph <- as.numeric(args[5]) # cohesion concentration parameter
sigupper <- as.numeric(args[6]) # upper limit on error variance
sim_type <- as.character(args[7]) # similarity type
s0 <- as.numeric(args[8]) # point estimate of similarity stdev
ii <- as.numeric(args[9]) # data replicate (subset seed)
datenow <- as.numeric(args[10]) # date (fitting seed)

nobs <- 750

(mesg = paste0("missType", missType, "_dataType", dataType, 
               "_modtype", modtype, "_p", p_use, "_alph", alph, "_sigupper", sigupper, "_simType", sim_type, "_s0_", s0, 
               "_ii", ii, "_date", datenow))


## for running PPMxR
library("JuliaCall")
julia = julia_setup(JULIA_HOME="../../julia-1.8.5/bin")
# julia = julia_setup(JULIA_HOME="/Applications/Julia-1.8.app/Contents/Resources/julia/bin/") # local path
# julia_install_package_if_needed("https://github.com/mheiner/ProductPartitionModels.jl")
# julia_install_package_if_needed("Dates")
# julia_installed_package("ProductPartitionModels")
julia_library("ProductPartitionModels")
julia_library("Dates")
source("../PPMx_JuliaFunctions.R")


## get data
set.seed(ii)
source("sim_data.R")
set.seed(datenow + ii)

## MCMC setup
n_draws <- 100e3
n_burn <- 50e3
n_thin <- 50
(n_keep <- (n_draws - n_burn) / n_thin)


### fit PPMxR
base_init = list(mu0=mean(y), sig0=sd(y), tau0=0.1, sig_upper=sigupper)
base_prior = list(mu0_mean=mean(y), mu0_sd=2.0*sd(y), sig0_upper=5.0*sd(y))

if (sim_type == "NN") {
  # similarity_vals = list(sd=sqrt(0.05), m0=0.5, sd0=sqrt(0.05), type="NN")
} else if (sim_type == "NNiG_indep") {
  # similarity_vals = list(m0=0.5, sc_prec0=2.0, a0=4.0, b0=0.2, type=sim_type)
} else if (sim_type == "NNiChisq_indep") {
  # similarity_vals = list(m0=0.5, sc_prec0=0.1, nu0=4.0, s20=s0^2, type=sim_type)
  similarity_vals = list(m0=0.5, sc_prec0=0.1, nu0=10.0, s20=s0^2, type=sim_type)
}

PPMxR = fit_PPMx(ytn, Xmat, Xpred, # Xpred = NULL for no prediciton
                 nburn = n_burn, nkeep = n_keep, nthin = n_thin, # add thinning
                 pred_insamp = TRUE,
                 # progressfile=paste0("PPMx_progress/", datafile, ".txt"),
                 sampling_model = modtype,
                 cohesion = list(alpha = alph), 
                 similarity = similarity_vals,
                 baseline = base_init, # normal (mean), Dirichlet-Laplace (coeff), unif (sd); if a param has a prior, this is just supplying the initial value
                 baseline_prior = base_prior, # default for mu0_sd is 100.0 (used on simulated data), but this is what was done for the ozone analyisis with PPMx (and X was scaled)
                 upd_beta = TRUE,
                 y_grid = ytt, crossxy = FALSE
)

PPMxR$resid = colMeans(PPMxR$Ypred_insamp) - ytn
PPMxR$predErr = colMeans(PPMxR$Pred[[1]]$Ypred) - ytt

PPMxR$msqErr = mean(PPMxR$resid^2)
PPMxR$msqPredErr = mean(PPMxR$predErr^2)

PPMxR$mabsErr = mean(abs(PPMxR$resid))
PPMxR$mabsPredErr = mean(abs(PPMxR$predErr))

PPMxR$mld = mean(PPMxR$logDens_insamp)
PPMxR$mpld = mean(PPMxR$Pred[[1]]$PPlogDens)

PPMxR$resid_qtile = sapply(1:n_tn, function(i) (rank(c(ytn[i], (PPMxR$Ypred_insamp[,i])))[1] - runif(1, 0.49, 0.50)) / (nrow(PPMxR$Ypred_insamp) + 1) )
PPMxR$pred_qtile = sapply(1:n_tt, function(i) (rank(c(ytt[i], (PPMxR$Pred[[1]]$Ypred[,i])))[1] - runif(1, 0.49, 0.50)) / (nrow(PPMxR$Pred[[1]]$Ypred) + 1) )


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

nclus = mean(PPMxR$nclus)

cat("\n\nIn-sample MSE:", mse, "\n")
cat("Out-of-sample MSE:", mspe, "\n")

cat("\n\nIn-sample MlogDens:", mld, "\n")
cat("Out-of-sample MpredLogDens:", mpld, "\n")

save(mse, mspe, mae, mape, mld, mpld, ksd, kspd, nclus, mesg, p_use, ii, datenow, dataType, 
     file=paste0("postsim/summary_", dataType, "_PPMxR_", mesg, ".rda"))
save(PPMxR, mesg, p_use, ii, datenow, file=paste0("postsim/fit_", dataType, "_PPMxR_", mesg, ".rda"))

quit(save="no")
