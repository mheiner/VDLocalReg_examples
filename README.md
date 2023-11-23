# VDLocalReg Examples

Code to recreate results in Heiner, Page, and Quintana (2023+) in R. In the code, the VDReg method is called PPMx and the VDLReg method is called PPMxR.
The scripts throughout require Julia (version 1.8.5+) and the [ProductPartitionModels.jl](https://github.com/mheiner/ProductPartitionModels.jl) Julia package; integration with R relies on the [JuliaCall](https://cran.r-project.org/web/packages/JuliaCall/index.html) R package. 
Benchmarking code can be found in the test folder of the [ProductPartitionModels.jl](https://github.com/mheiner/ProductPartitionModels.jl) GitHub repository.

## Folders and their contents
Each of the `NHANES`, `OldFaithful`, `BostonHousing`,  `MNAR_experiment`, `SimStudy_Friedman`, and `SUPPORT` folders contain a similar progression of R scripts, with prefix `0` indicating preprocessing code, prefix `1` indicating code used to fit models, and prefix `2` for postprocessing. The folders also contain `.sh` scripts for large-scale fitting across multiple replicated training/test pairs or simulations. See Misc below before attempting to run `1_fit_competitors.R`.

We recommend starting with the `OldFaithful` example, which exemplifies the most tasks with the most comprehensive commenting in the code.

The `local_linearity_indicator` folder contains code for the screening method described in the paper.

The utility file `runPPMxR_JuliaFunctions.R` contains wrapper functions that facilitate fitting the PPMx models in R. It should remain in the top folder.

## Misc
To run the pattern submodel (PSM) method, download the [MissingDataAndPrediction](https://github.com/sarahmercaldo/MissingDataAndPrediction) GitHub repository, move `PKMSfunctions.R` into this directory and rename it `Mercaldo_PMKSfunctions.R`. To run the SUPPORT analyses, also move `support2.csv` into the `SUPPORT` folder.

## Fitting a VDLReg model in R
The primary function for fitting and predition with VDLReg is `fit_PPMx`, which calls Julia and is defined in `runPPMxR_JuliaFunctions.R`. Its usage is demonstrated below

    fit <- fit_PPMx(y, X, Xpred, # Xpred = NULL for no prediciton
                    nburn = 1000, nkeep = 5000, nthin = 40, # MCMC options
                    pred_insamp = TRUE, # keep posterior predictive draws related to X
                    sampling_model = "Reg", # "Reg" for VDLReg, "Mean" for VDReg
                    cohesion = list(alpha = 1.0), 
                    similarity = list(m0=0.0, sc_prec0=0.1, nu0=4.0, s20=0.5^2, type="NNiChisq_indep"),
                    baseline = list(mu0=0.0, sig0=0.5, tau0=0.1, sig_upper=10.0), # tau0, sig_upper are fixed
                    baseline_prior = list(mu0_mean=0.0, mu0_sd=2.0, sig0_upper=4.0), 
                    y_grid = y_grid,
                    crossxy = crossxy
                    )

`Xpred` is a matrix or list of matrices at which to predict (may contain missing values coded as `NA`). `ygrid` is a vector or list of vectors supplying response values at which to evaluate the log predictive density for covariate values in `Xpred`. If both are lists, `Xpred[[1]]` corresponds with `ygrid[[1]]`, `Xpred[[2]]` corresponds with `ygrid[[2]]`, and so forth. `crossxy` is a boolean variable or list of booleans indicating whether the corresponding `ygrid` should be "crossed" with its corresponding `Xpred`. If they are crossed, log predictive density values are evaluated for all values in `ygrid` for *each* row in the corresponding `Xpred`.

The resulting `fit` object is a list containing the original data (`y` and `X`) and posterior samples of 
- overall log likelihood (`llik`), 
- observation-specific log likelihood (`llik_mat`), 
- number of clusters (`nclus`), 
- cluster allocations (`Si`), 
- other model parameters indexed by observation number, 
- posterior predictive draws
    - `YPred` for draws on the observation level
    - `Cpred` for cluster allocations
    - `Mpred` for means of the predictive distributions

See the [ProductPartitionModels.jl](https://github.com/mheiner/ProductPartitionModels.jl) documentation and `runPPMxR_JuliaFunctions.R` for additional model options.