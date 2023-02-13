### after fitting MI in 1_fit_competitors.R

pdf(file=paste0("plots/predQtile_competitors_Boston_pMiss", perc_miss, "_ii", ii, "_", datenow, ".pdf"), width=5, height=4)
plot(sort(pred_qtile_bart), ylab="Prediction Quantile", main=paste0("BARTm\nK-S Stat = ", round(kspd["BART"], 3))); abline(0, 1/n_tt)
hist(pred_qtile_bart, xlab="Prediction Quantile", main=paste0("BARTm\nK-S Stat = ", round(kspd["BART"], 3)))

plot(sort(pred_qtile_MI), ylab="Prediction Quantile", main=paste0("MI\nK-S Stat = ", round(kspd["MI"], 3))); abline(0, 1/n_tt)
hist(pred_qtile_MI, xlab="Prediction Quantile", main=paste0("MI\nK-S Stat = ", round(kspd["MI"], 3)))
dev.off()

kspd


### run on server with PPMxR fits
perc_miss = 25
ii = 100
datenow = 220813
modtype = "Reg"
modtype = "Mean"

load(paste0("postsim/fit_Boston_PPMxR_modtype", modtype, "_pMiss", perc_miss, "_alph3_sigupper0.4_simTypeNNiChisq_indep_s0_0.1_ii", ii, "_date", datenow, ".rda"))

(kspd = ks.test(PPMxR$pred_qtile, "punif")$statistic)
if (modtype == "Reg") {modname = "VDLReg"}
if (modtype == "Mean") {modname = "VDReg"}

pdf(file=paste0("plots/predQtile_modtype", modtype, "_Boston_pMiss", perc_miss, "_ii", ii, "_", datenow, ".pdf"), width=5, height=4)
plot(sort(PPMxR$pred_qtile), ylab="Prediction Quantile", main=paste0(modname, "\nK-S Stat = ", round(kspd, 3))); abline(0, 1/length(PPMxR$pred_qtile))
hist(PPMxR$pred_qtile, xlab="Prediction Quantile", main=paste0(modname, "\nK-S Stat = ", round(kspd, 3)))
dev.off()

#####
