args <- commandArgs(TRUE)

# args <- c("MCAR", "Additive", 2, 1, 230829)
missType <- args[1]
dataType <- args[2]
p_use <- as.numeric(args[3])
ii = as.numeric(args[4])
datenow = as.numeric(args[5])

nobs = 750

(mesg = paste0("missType", missType, "_dataType", dataType, 
               "_p", p_use,
               "_ii", ii, "_date", datenow))

library(tidyverse)
library(missForest)
library(randomForest)
library(bartMachine) # use version 1.2.6
library(mvtnorm)
library(mice) # necessary for the ampute function use version 3.12.0
library(mi) # necessary for multiple imputation use version 1.0
library(betareg) # need to eventually change data type for imputations use version 3.1-4
library(rstanarm)
library(parallel)

source('../Mercaldo_PMKSfunctions.R') # These are PSM functions

# Function to fit linear models when using imputed datasets.
reg.fit.pred <- function(x, y, train, ncov){
  out <- NULL
  dtn <- data.frame(y=y,x=x[train,1:(ncov)])
  lm.fit <- lm(y ~ ., data=dtn)
  dtt <- data.frame(x=x[-train,1:(ncov)])
  preds <- predict(lm.fit, dtt)
  out$coef <- lm.fit$coefficients
  out$fitted <- lm.fit$fitted.values
  out$preds <- preds
  out$sig <- summary(lm.fit)$sigma
  out
}

stanreg.fit.pred <- function(x, y, train, ncov) {
  out <- NULL
  dtn <- data.frame(y=y, x=x[train, 1:ncov])
  fit <- stan_glm(y ~ ., data=dtn, family=gaussian())
  dtt <- data.frame(x=x[-train, 1:ncov])
  sig_draws <- as.matrix(fit, pars="sigma")[,1]
  
  pp_insamp <- posterior_predict(fit, newdata=dtn)
  pp <- posterior_predict(fit, newdata=dtt)
  
  out$fitted <- colMeans(pp_insamp)
  out$preds <- colMeans(pp)
  
  out$fitted_sim <- pp_insamp
  out$preds_sim <- pp
  out$sigma_sim <- sig_draws
  out$nsim <- length(sig_draws)
  
  out
}


competitors = c("RF", "BART", "MI", "PSM", "NULL")
(n_compet = length(competitors))

mse <- numeric(n_compet)
mspe <- numeric(n_compet)

mae <- numeric(n_compet)
mape <- numeric(n_compet)

mld <- numeric(n_compet)
mpld <- numeric(n_compet)

ksd <- numeric(n_compet)
kspd <- numeric(n_compet)

names(mse) <- names(mspe) <- competitors
names(mae) <- names(mape) <- competitors
names(mld) <- names(mpld) <- competitors
names(ksd) <- names(kspd) <- competitors


## get data
set.seed(ii)
source("sim_data.R")
set.seed(datenow + ii)

(ncov <- ncol(Xmat))
colnames(Xmat) <- colnames(Xpred) <- paste0("V", 1:ncov)



sessionInfo()


# Null model

(ytn_hat = mean(ytn))
(ytn_sd = sd(ytn))
mse["NULL"] <- mean((ytn - ytn_hat)^2)
mspe["NULL"] <- mean((ytt - ytn_hat)^2)

mae["NULL"] <- mean(abs(ytn - ytn_hat))
mape["NULL"] <- mean(abs(ytt - ytn_hat))

mld["NULL"] <- mean(dnorm(ytn, ytn_hat, sd=ytn_sd, log=TRUE))
mpld["NULL"] <- mean(dnorm(ytt, ytn_hat, sd=ytn_sd, log=TRUE))


# Random Forest Imputation
Xsc_imp0 = cbind(rbind(Xmat, Xpred), trash=runif(n)) # randomForest requires at least 3 covariates...
Xsc_imp_rf = missForest(Xsc_imp0)$ximp

Xsc_imp_tn = Xsc_imp_rf[1:n_tn, 1:p_use]
Xsc_imp_tt = Xsc_imp_rf[-c(1:n_tn), 1:p_use]

fit_rf = randomForest(Xsc_imp_tn, ytn, xtest=Xsc_imp_tt, ytest=ytt, importance=TRUE)

mse["RF"] <- mean((ytn - fit_rf$predicted)^2)
mspe["RF"] <- mean((ytt - fit_rf$test$predicted)^2)

mae["RF"] <- mean(abs(ytn - fit_rf$predicted))
mape["RF"] <- mean(abs(ytt - fit_rf$test$predicted))

mld["RF"] <- mean(dnorm(ytn, fit_rf$predicted, sd=sqrt(fit_rf$mse[length(fit_rf$mse)]), log=TRUE))
mpld["RF"] <- mean(dnorm(ytt, fit_rf$predicted, sd=sqrt(fit_rf$mse[length(fit_rf$mse)]), log=TRUE))



# Use the BARTm function
bart_m <- bartMachine(data.frame(Xmat), ytn, use_missing_data=TRUE,
                      use_missing_data_dummies_as_covars=TRUE, run_in_sample=TRUE)

y_hat_train <- bart_m$y_hat_train
bart_pred_insamp <- bart_machine_get_posterior(bart_m, data.frame(Xmat))
bart_pred <- bart_machine_get_posterior(bart_m, data.frame(Xpred))
bart_yhat_test <- predict(bart_m, data.frame(Xpred))
bart_sig2samp <- get_sigsqs(bart_m)
bart_nsim <- length(bart_sig2samp)

mse["BART"] <- mean((ytn - y_hat_train)^2)
mspe["BART"] <- mean((ytt - bart_yhat_test)^2)

mae["BART"] <- mean(abs(ytn - y_hat_train))
mape["BART"] <- mean(abs(ytt - bart_yhat_test))


# incorporates posterior uncertainty
mld["BART"] <- mean(sapply(1:bart_nsim, function(i) dnorm(ytn, bart_pred_insamp$y_hat_posterior_samples[,i], sqrt(bart_sig2samp[i]), log=TRUE)))
mpld["BART"] <- mean(sapply(1:bart_nsim, function(i) dnorm(ytt, bart_pred$y_hat_posterior_samples[,i], sqrt(bart_sig2samp[i]), log=TRUE)))


resid_qtile_bart <- sapply(1:n_tn, function(i) (rank(c(ytn[i], (bart_pred_insamp$y_hat_posterior_samples[i,] + rnorm(bart_nsim, 0, sd=sqrt(bart_sig2samp)))))[1] - runif(1, 0.49, 0.50)) / (bart_nsim + 1) )
pred_qtile_bart <- sapply(1:n_tt, function(i) (rank(c(ytt[i], (bart_pred$y_hat_posterior_samples[i,] + rnorm(bart_nsim, 0, sd=sqrt(bart_sig2samp)))))[1] - runif(1, 0.49, 0.50)) / (bart_nsim + 1) )
ksd["BART"] <- ks.test(resid_qtile_bart, "punif")$statistic
kspd["BART"] <- ks.test(pred_qtile_bart, "punif")$statistic



# Multiple Imputation
cat("imputation", "\n")

n_chains_mi = 10
Xmi <- missing_data.frame(rbind(Xmat, Xpred))
Xmi <- change(Xmi, y=colnames(Xmat), what="type", to="continuous")
mi.data <- tryCatch(mi::complete(mi(Xmi, verbose=FALSE, n.chains=n_chains_mi)))

# mi.fits <- lapply(mi.data, reg.fit.pred, y=ytn, train=1:n_tn, ncov=ncov)
mi.fits <- mclapply(mi.data, stanreg.fit.pred, y=ytn, train=1:n_tn, ncov=ncov, mc.cores=n_chains_mi)

mi.ispred <- apply(matrix(unlist(lapply(mi.fits, function(x) x[["fitted"]])), nrow=n_chains_mi, byrow=TRUE), 2, mean)
mi.pred <- apply(matrix(unlist(lapply(mi.fits, function(x) x[["preds"]])), nrow=n_chains_mi, byrow=TRUE), 2, mean)

mse["MI"] <- mean((ytn - mi.ispred)^2)
mspe["MI"] <- mean((ytt - mi.pred)^2)

mae["MI"] <- mean(abs(ytn - mi.ispred))
mape["MI"] <- mean(abs(ytt - mi.pred))

mld["MI"] <- lapply(mi.fits, function(x) {
    mean(sapply(1:x$nsim, function(i) dnorm(ytn, x$fitted_sim[i,], x$sigma_sim[i], log=TRUE)))
  } ) %>% unlist() %>% mean()

mpld["MI"] <- lapply(mi.fits, function(x) {
  mean(sapply(1:x$nsim, function(i) dnorm(ytt, x$preds_sim[i,], x$sigma_sim[i], log=TRUE)))
} ) %>% unlist() %>% mean()

fitted_sim_MI <- lapply(mi.fits, function(x) x$fitted_sim) %>% do.call(rbind, .)
preds_sim_MI <- lapply(mi.fits, function(x) x$preds_sim) %>% do.call(rbind, .)

resid_qtile_MI <- sapply(1:n_tn, function(i) (rank(c(ytn[i], (fitted_sim_MI[,i])))[1] - runif(1, 0.49, 0.50)) / (nrow(fitted_sim_MI) + 1) )
# plot(((1:n_tn)-0.5)/n_tn, sort(resid_qtile_MI), xlab="theoretical quantiles", ylab="model (is) pred. quantiles"); abline(0,1, lty=2)
pred_qtile_MI = sapply(1:n_tt, function(i) (rank(c(ytt[i], (preds_sim_MI[,i])))[1] - runif(1, 0.49, 0.50)) / (nrow(preds_sim_MI) + 1) )
# plot(((1:n_tt)-0.5)/n_tt, sort(pred_qtile_MI), xlab="theoretical quantiles", ylab="model pred. quantiles"); abline(0,1, lty=2)
ksd["MI"] <- ks.test(resid_qtile_MI, "punif")$statistic
kspd["MI"] <- ks.test(pred_qtile_MI, "punif")$statistic


# This is the pattern submodel approach
xeq <- paste(colnames(Xmat)[-ncov], "+", collapse=" ")
xeq2 <- paste(xeq, colnames(Xmat)[ncov], collapse=" ")
(ex.fit <- paste("y ~", xeq2, collapse=""))

train.dat <- data.frame(y=ytn, Xmat)
test.dat <- data.frame(y=ytt, Xpred)

nobsperrow <- apply(t(apply(train.dat, 1, function(x)!is.na(x))), 1, sum)
# What I am doing here is skipping datasets where there is not at least
# one covariate vector that is complete.  Because if there isn't one,
# then the pmks method produces an error. I use 2 as the response is
# always available
print(nobsperrow)

if (sum(nobsperrow == ncol(train.dat)) > (ncov)) {
  
  ex.pmks <- pmks(DATA=train.dat, model=ex.fit, logistic=FALSE)
  
  pred.pmks.train <- tryCatch(predict.sm(prediction.data=train.dat,
                                         model=ex.fit, pmks.object=ex.pmks, logistic = FALSE),
                              error = function(e) {NA})
  
  pred.pmks.test <- tryCatch(predict.sm(prediction.data=test.dat,
                                        model=ex.fit, pmks.object=ex.pmks, logistic = FALSE),
                             error = function(e) {NA})
  
  diff.train <- unlist(sapply(pred.pmks.train, function(x) x$lin.pred - x$truth ))
  diff.test <- unlist(sapply(pred.pmks.test, function(x) x$lin.pred - x$truth ))
  ldens.train <- sapply(pred.pmks.train, function(x) {
      dnorm(x$truth, x$lin.pred, sd=sqrt(summary(x$mod)$dispersion), log=TRUE) 
    }) %>% unlist()
  ldens.test <- sapply(pred.pmks.test, function(x) {
    dnorm(x$truth, x$lin.pred, sd=sqrt(summary(x$mod)$dispersion), log=TRUE) 
  }) %>% unlist()
} else {
  diff.train <- NA # This procedure produced NAs on occassion
  diff.test <- NA
  ldens.train <- NA
  ldens.test <- NA
}

mse["PSM"] <- mean((diff.train)^2)
mspe["PSM"] <- mean((diff.test)^2)

mae["PSM"] <- mean(abs(diff.train))
mape["PSM"] <- mean(abs(diff.test))

mld["PSM"] <- mean(ldens.train)
mpld["PSM"] <- mean(ldens.test)


save(mse, mspe, mae, mape, mld, mpld, ksd, kspd, p_use, ii, datenow, 
     file=paste0("postsim/summary_", dataType, "_competitors_", mesg, ".rda"))

quit(save="no")