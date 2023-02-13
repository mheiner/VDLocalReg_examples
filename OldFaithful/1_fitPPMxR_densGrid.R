args <- commandArgs(TRUE)
# args <- c("Reg", 1, 1.0, 0.5, "NNiChisq_indep", 0.5, 1, 220816)

modtype <- as.character(args[1]) # one of "Mean", "Reg"
p_use <- as.numeric(args[2]) # number of covariates
alph <- as.numeric(args[3]) # cohesion concentration parameter
sigupper <- as.numeric(args[4]) # upper limit on error variance
sim_type <- as.character(args[5]) # similarity type
s0 <- as.numeric(args[6]) # point estimate of similarity stdev
ii <- as.numeric(args[7]) # data replicate (subset seed)
datenow <- as.numeric(args[8]) # date (fitting seed)

(mesg <- paste0("modtype", modtype, "_p", p_use, "_alph", alph, "_sigupper", sigupper, "_simType", sim_type, "_s0_", s0, "_ii", ii, "_date", datenow))


## for running PPMxR
library("JuliaCall")

# this will require a correct path to Julia
# julia <- julia_setup(JULIA_HOME="../../julia-1.8.5/bin")
julia <- julia_setup(JULIA_HOME="/Applications/Julia-1.8.app/Contents/Resources/julia/bin/") # local path

julia_install_package_if_needed("https://github.com/mheiner/ProductPartitionModels.jl")
julia_install_package_if_needed("Dates")

julia_installed_package("ProductPartitionModels")

julia_library("ProductPartitionModels")
julia_library("Dates")
source("../PPMx_JuliaFunctions.R")


## get data
set.seed(ii)
source("0_subsampleData.R")
set.seed(datenow + ii)


## MCMC setup
n_draws <- 5e3 # paper examples use 100e3
n_burn <- 1e3 # paper examples use 50e3
n_thin <- 4 # paper examples use 50

(n_keep <- (n_draws - n_burn) / n_thin)

n_pred <- 200 # paper examples use 200
n_ygrid <- 300 # paper examples use 300
Xpred <- list()
y_grid <- list()
crossxy <- list()


## set up prediction grids at typical values of X
if (p_use == 1) { # means waiting1 and waiting2
  
  for (l in 1:4) {
    Xpred[[l]] <- matrix(0, nrow=n_pred, ncol=2)
    colnames(Xpred[[l]]) <- vars_keep
    
    if (l %% 2 == 0) {
      Xpred[[l]][,1] <- 0.5
    } else {
      Xpred[[l]][,1] <- -1.5
    }
    
    if ((l+1) %% 2 == 0) {
      Xpred[[l]][,2] <- -1.5
    } else {
      Xpred[[l]][,2] <- 0.5
    }
    
    Xpred[[l]][,floor((l+1)/2)] <- seq(-2.5, 2.5, length=n_pred)
    
    y_grid[[l]] <- seq(-2.5, 2.5, length=n_ygrid)
    crossxy[[l]] <- TRUE
  }
  Xpred[[5]] <- matrix(NA, nrow=n_pred, ncol=2)
  colnames(Xpred[[5]]) <- vars_keep
  Xpred[[5]][,1] <- seq(-2.5, 2.5, length=n_pred)
  y_grid[[5]] <- seq(-2.5, 2.5, length=n_ygrid)
  crossxy[[5]] <- TRUE
  
  
  Xpred[[6]] <- matrix(NA, nrow=n_pred, ncol=2)
  colnames(Xpred[[6]]) <- vars_keep
  Xpred[[6]][,2] <- seq(-2.5, 2.5, length=n_pred)
  y_grid[[6]] <- seq(-2.5, 2.5, length=n_ygrid)
  crossxy[[6]] <- TRUE
  
} else if (p_use == 2) {
  
  for (l in 1:(2*p_use)) {
    Xpred[[l]] <- matrix(0, nrow=n_pred, ncol=p_use)
    colnames(Xpred[[l]]) <- vars_keep
    
    if (l %% 2 == 0) {
      Xpred[[l]][,1] <- 0.5
    } else {
      Xpred[[l]][,1] <- -1.5
    }
    
    if ((l+1) %% 2 == 0) {
      Xpred[[l]][,2] <- -1.25
    } else {
      Xpred[[l]][,2] <- 0.75
    }
    
    Xpred[[l]][,floor((l+1)/2)] <- seq(-2.5, 2.5, length=n_pred)
    
    y_grid[[l]] <- seq(-2.5, 2.5, length=n_ygrid)
    crossxy[[l]] <- TRUE
  }
  
  Xpred[[2*p_use + 1]] <- matrix(NA, nrow=n_pred, ncol=p_use)
  colnames(Xpred[[2*p_use + 1]]) <- vars_keep
  Xpred[[2*p_use + 1]][,1] <- seq(-2.5, 2.5, length=n_pred)
  y_grid[[2*p_use + 1]] <- seq(-2.5, 2.5, length=n_ygrid)
  crossxy[[2*p_use + 1]] <- TRUE
  
  Xpred[[2*p_use + 2]] <- matrix(NA, nrow=n_pred, ncol=p_use)
  colnames(Xpred[[2*p_use + 2]]) <- vars_keep
  Xpred[[2*p_use + 2]][,2] <- seq(-2.5, 2.5, length=n_pred)
  y_grid[[2*p_use + 2]] <- seq(-2.5, 2.5, length=n_ygrid)
  crossxy[[2*p_use + 2]] <- TRUE
}

### fit PPMxR
base_init <- list(mu0=0.0, sig0=0.5, tau0=0.1, sig_upper=sigupper) # tau0 and sig_upper will be fixed; mu0, sig0 are sampled in MCMC
base_prior <- list(mu0_mean=0.0, mu0_sd=2.0, sig0_upper=4.0)

if (sim_type == "NN") {
} else if (sim_type == "NNiG_indep") {
} else if (sim_type == "NNiChisq_indep") {
  similarity_vals <- list(m0=0.0, sc_prec0=0.1, nu0=4.0, s20=s0^2, type=sim_type)
}

PPMxR <- fit_PPMx(ysc, Xsc[,vars_keep], Xpred, # Xpred = NULL for no prediciton
                 nburn = n_burn, nkeep = n_keep, nthin = n_thin,
                 pred_insamp = TRUE,
                 sampling_model = modtype,
                 cohesion = list(alpha = alph), 
                 similarity = similarity_vals,
                 baseline = base_init, 
                 baseline_prior = base_prior, 
                 upd_beta = TRUE,
                 y_grid = y_grid, crossxy = crossxy
)

PPMxR$n_keep <- n_keep
PPMxR$n_thin <- n_thin
rm(n_keep, n_thin)

save(PPMxR, mesg, p_use, nmis, ii, datenow, datname, file=paste0("postsim/fitDensGrid_", datname, "_PPMxR_", mesg, ".rda"))

quit(save="no")
