rm(list=ls())

library("bartMachine")
library("tidyverse")
library("ggplot2")


dates <- 220816

files0 <- list.files("postsim/")

files_use <- files0[grep(paste0("fitDensGrid_.*", dates, ".rda"), files0)]
head(files_use)
length(files_use)
files_use

i <- 1
files_use[i]
load(paste0("postsim/", files_use[i]))
source("0_subsampleData.R")

ls()
str(PPMxR)

library("coda")
plot(as.mcmc(PPMxR$nclus))
factor(PPMxR$nclus, levels=1:max(PPMxR$nclus)) %>% table() %>% prop.table() %>% as.data.frame()

plot(as.mcmc(PPMxR$sig[,1:5]))

library("salso")
rhohat <- salso(PPMxR$Si)

dim(PPMxR$beta)
plot(as.mcmc(PPMxR$beta[,10,1]))

pdf(file=paste0("plots/beta_pm_OldFaithful_", mesg, ".pdf"), height=5, width=7)
for (pp in 1:p_use) {
  plot(colMeans(PPMxR$beta[,,pp]), col=rhohat, pch=20, main=paste0("posterior means: beta", pp), ylab="", xlab="obs")
  abline(h=0, lty=2)
}
dev.off()


summary(colMeans(PPMxR$beta[,,1]))

plot(colMeans(PPMxR$Mpred_insamp), PPMxR$y); abline(0,1, lty=2)


## predictive density
stopifnot(p_use == 2) # WARNING: this code will break with three covariates


fitbart <- TRUE

l <- 1 # 1:4, slices of data with panel covariate observed
head(PPMxR$Pred[[l]]$Xpred)
(varname <- vars_keep[floor((l+1)/2)])
(other_var <- vars_keep[-floor((l+1)/2)])
rnd_digits <- 1*(other_var == "duration1")

ppld_mean <- apply(PPMxR$Pred[[l]]$PPlogDens, c(2,3), mean)
str(ppld_mean)


if (fitbart) {
  bart_m <- bartMachine(data.frame(PPMxR$X), PPMxR$y, use_missing_data=TRUE,
                        use_missing_data_dummies_as_covars=TRUE, run_in_sample=TRUE)
  bart_pred <- predict(bart_m, data.frame(PPMxR$Pred[[l]]$Xpred)) # M_<var> refers to the missing indicator
}

xyg <- expand.grid(PPMxR$Pred[[l]]$Xpred[,floor((l+1)/2)], PPMxR$Pred[[l]]$y_grid)
colnames(xyg) <- c("x", "y")
xyg$dy <- c(exp(ppld_mean))
head(xyg)

xygd_plot <- ggplot(xyg, aes(x=x, y=y, fill=dy, alpha=dy)) + geom_tile() + theme_bw() +
  scale_fill_gradient2(low="red", mid="orange", high="yellow", midpoint=1e-2) + theme(legend.position="none")

xygd_plot2 <- xygd_plot + xlab(paste0(varname)) +
  geom_line(data=data.frame(x=PPMxR$Pred[[l]]$Xpred[,floor((l+1)/2)], y=colMeans(PPMxR$Pred[[l]]$Mpred), dy=0), aes(x=x, y=y), 
            col="red4", alpha=0.5, lwd=1) +
  geom_point(data=data.frame(x=PPMxR$X[,floor((l+1)/2)], y=PPMxR$y, 
                             others=unname(apply(PPMxR$X[,-floor((l+1)/2), drop=FALSE], 1, 
                                                 function(w) sum(abs(w - PPMxR$Pred[[l]]$Xpred[1, -floor((l+1)/2)] ))))), 
             aes(x=x, y=y, size=exp(-others), alpha=exp(-others)), fill="black") + scale_size_continuous(range=c(0,3)) +
  ggtitle(paste0(other_var, " = ", round(PPMxR$Pred[[l]]$Xpred[1, other_var]*X_sd[other_var] + X_mn[other_var], rnd_digits) )) +
  scale_y_continuous(breaks=-2:2, labels=round(-2:2*y_sd + y_mn)) + 
  scale_x_continuous(breaks=-2:2, labels=round(-2:2*X_sd[varname] + X_mn[varname]))

if (fitbart) {
 xygd_plot2 + geom_line(data=data.frame(x=PPMxR$Pred[[l]]$Xpred[,floor((l+1)/2)], y=bart_pred, dy=0), aes(x=x, y=y), col="red4", alpha=0.5, lwd=1, lty=2)
} else {
  xygd_plot2
}

head(PPMxR$Pred[[l]]$Xpred)
mesg

ggsave(file=paste0("plots/predDens_", mesg, "_variable", varname, "_", l, ".pdf"), height=3.25, width=4.5)


## slices of data with panel covariate missing
l <- 5 # 5:6
head(PPMxR$Pred[[l]]$Xpred)
(varname <- vars_keep[which(!is.na(PPMxR$Pred[[l]]$Xpred[1,]))])
(other_var <- setdiff(vars_keep, varname))


ppld_mean <- apply(PPMxR$Pred[[l]]$PPlogDens, c(2,3), mean)
str(ppld_mean)

if (fitbart) {
  bart_m <- bartMachine(data.frame(PPMxR$X), PPMxR$y, use_missing_data=TRUE,
                        use_missing_data_dummies_as_covars=TRUE, run_in_sample=TRUE)
  bart_pred <- predict(bart_m, data.frame(PPMxR$Pred[[l]]$Xpred)) # M_<var> refers to the missing indicator
}

xyg <- expand.grid(PPMxR$Pred[[l]]$Xpred[,varname], PPMxR$Pred[[l]]$y_grid)
colnames(xyg) <- c("x", "y")
xyg$dy <- c(exp(ppld_mean))
head(xyg)

xygd_plot <- ggplot(xyg, aes(x=x, y=y, fill=dy, alpha=dy)) + geom_tile() + theme_bw() +
  scale_fill_gradient2(low="red", mid="orange", high="yellow", midpoint=1e-2) + theme(legend.position="none")

xygd_plot2 <- xygd_plot + xlab(paste0(varname)) +
  geom_line(data=data.frame(x=PPMxR$Pred[[l]]$Xpred[,varname], y=colMeans(PPMxR$Pred[[l]]$Mpred), dy=0), aes(x=x, y=y), 
            col="red4", alpha=0.5, lwd=1) +
  geom_point(data=data.frame(x=PPMxR$X[which(!is.na(PPMxR$X[,other_var])),varname], y=PPMxR$y[which(!is.na(PPMxR$X[,other_var]))], dy=0), 
             aes(x=x, y=y), fill="black", alpha=0.6, size=0.8) +
  geom_point(data=data.frame(x=PPMxR$X[which(is.na(PPMxR$X[,other_var])), varname], y=PPMxR$y[which(is.na(PPMxR$X[,other_var]))], dy=0), 
             aes(x=x, y=y), alpha=0.6, size=2.0, pch=1) +
  ggtitle(paste0(other_var, " = ", 
                 round(PPMxR$Pred[[l]]$Xpred[1, other_var]*X_sd[other_var] + X_mn[other_var]) )) +
  scale_y_continuous(breaks=-2:2, labels=round(-2:2*y_sd + y_mn)) + 
  scale_x_continuous(breaks=-2:2, labels=round(-2:2*X_sd[varname] + X_mn[varname]))

if (fitbart) {
  xygd_plot2 + geom_line(data=data.frame(x=PPMxR$Pred[[l]]$Xpred[,varname], y=bart_pred, dy=0), aes(x=x, y=y), col="red4", alpha=0.5, lwd=1, lty=2)
} else {
  xygd_plot2
}

head(PPMxR$Pred[[l]]$Xpred)
mesg

ggsave(file=paste0("plots/predDens_", mesg, "_variable", varname, "_", l, ".pdf"), height=3.25, width=4.5)


## quantile residuals
y_tn <- PPMxR$y
y_tt <- colMeans(PPMxR$Pred[[1]]$Ypred) - PPMxR$predErr

n_tn <- length(y_tn)
n_tt <- length(y_tt)

PPMxR$resid_qtile <- sapply(1:n_tn, function(i) (rank(c(y_tn[i], (PPMxR$Ypred_insamp[,i])))[1] - 0.5) / (nrow(PPMxR$Ypred_insamp) + 1) )

plot(sort(PPMxR$resid_qtile)); abline(0, 1/n_tn, lty=2)


library("plotly")
datcc <- data.frame(cbind(y=PPMxR$y, PPMxR$X)) %>% drop_na
head(datcc)
plot_ly() %>% add_markers(x=datcc$waiting1, y=datcc$duration1, z=datcc$y)
plot_ly() %>% add_markers(x=datcc$waiting1, y=datcc$waiting2, z=datcc$y)
