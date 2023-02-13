### after fitting MI in 1_fit_competitors.R

pdf(file=paste0("plots/predQtile_competitors_OldFaithful_puse", p_use, "_ii", ii, "_", datenow, ".pdf"), width=5, height=4)
plot(sort(pred_qtile_bart), ylab="Prediction Quantile", main=paste0("BARTm\nK-S Stat = ", round(kspd["BART"], 3))); abline(0, 1/n_tt)
hist(pred_qtile_bart, xlab="Prediction Quantile", main=paste0("BARTm\nK-S Stat = ", round(kspd["BART"], 3)))

plot(sort(pred_qtile_MI), ylab="Prediction Quantile", main=paste0("MI\nK-S Stat = ", round(kspd["MI"], 3))); abline(0, 1/n_tt)
hist(pred_qtile_MI, xlab="Prediction Quantile", main=paste0("MI\nK-S Stat = ", round(kspd["MI"], 3)))
dev.off()

kspd


### run on server with existing model fits
datenow <- 220816
p_use <- 1
ii <- 1
modtype <- "Reg"

for (p_use in 1:3) {
  for (ii in c(1,50,100)) {
    for (modtype in c("Reg", "Mean")) {
      load(paste0("postsim/fit_Old_Faithful_PPMxR_modtype", modtype, "_p", p_use, "_alph1_sigupper0.5_simTypeNNiChisq_indep_s0_0.5_ii", ii, "_date", datenow, ".rda"))
      
      (kspd <- ks.test(PPMxR$pred_qtile, "punif")$statistic)
      if (modtype == "Reg") {modname <- "VDLReg"}
      if (modtype == "Mean") {modname <- "VDReg"}
      
      pdf(file=paste0("plots/predQtile_modtype", modtype, "_OldFaithful_puse", p_use, "_ii", ii, "_", datenow, ".pdf"), width=5, height=4)
      plot(sort(PPMxR$pred_qtile), ylab="Prediction Quantile", main=paste0(modname, "\nK-S Stat = ", round(kspd, 3))); abline(0, 1/length(PPMxR$pred_qtile))
      hist(PPMxR$pred_qtile, xlab="Prediction Quantile", main=paste0(modname, "\nK-S Stat = ", round(kspd, 3)))
      dev.off()
    }
  }
}

ytt <- PPMxR$predErr - colMeans(PPMxR$Pred[[1]]$Ypred)
plot(ytt, PPMxR$predErr); abline(0,1)

#####
