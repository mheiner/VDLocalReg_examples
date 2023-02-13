rm(list=ls())
library("tidyverse")
library("scales")

## dates
dates_c = 220813
dates = 220813


files0 = list.files("postsim/")
length(files0)

files_comp = files0[grep(paste0("summary_Boston_competitors_pMiss.*", dates_c, ".rda"), files0)]
head(files_comp)
length(files_comp)

files_ppmx = files0[grep(paste0("summary_Boston_PPMxR_modtypeMean_p.*_alph3_sigupper0.4_simTypeNNiChisq_indep_s0_0.1_.*_date", dates, ".rda"), files0)]
head(files_ppmx)
length(files_ppmx)

files_ppmxR = files0[grep(paste0("summary_Boston_PPMxR_modtypeReg_p.*_alph3_sigupper0.4_simTypeNNiChisq_indep_s0_0.1_.*_date", dates, ".rda"), files0)]
head(files_ppmxR)
length(files_ppmxR)

(n_rep = 100)
(n_files = length(files_ppmx))

mtds = c("RF", "BART", "MI", "PSM", "PPMx", "PPMxR")
pmissVals = c(0, 10, 25, 50)

MSE = data.frame(ii=rep(1:n_rep, times=length(pmissVals)), pMiss=rep(pmissVals, each=n_rep), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, null=NA, PPMxR=NA)
MSPE = data.frame(ii=rep(1:n_rep, times=length(pmissVals)), pMiss=rep(pmissVals, each=n_rep), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, null=NA, PPMxR=NA)

MAE = MSE
MAPE = MSPE

MLD = MSE
MPLD = MSPE

KSD = MSE
KSPD = MSPE

for ( i in 1:n_files ) {
  
  if (i <= length(files_comp)) {
    load(paste0("postsim/", files_comp[i]))
    tabrow = which(MSE$ii == ii & MSE$pMiss == perc_miss)

    MSE[tabrow, "RF"] = mse["RF"]
    MSPE[tabrow, "RF"] = mspe["RF"]
    
    MSE[tabrow, "BART"] = mse["BART"]
    MSPE[tabrow, "BART"] = mspe["BART"]
    
    MSE[tabrow, "MI"] = mse["MI"]
    MSPE[tabrow, "MI"] = mspe["MI"]
    
    MSE[tabrow, "PSM"] = mse["PSM"]
    MSPE[tabrow, "PSM"] = mspe["PSM"]
    
    MSE[tabrow, "null"] = mse["NULL"]
    MSPE[tabrow, "null"] = mspe["NULL"]

    MAE[tabrow, "RF"] = mae["RF"]
    MAPE[tabrow, "RF"] = mape["RF"]
    
    MAE[tabrow, "BART"] = mae["BART"]
    MAPE[tabrow, "BART"] = mape["BART"]
    
    MAE[tabrow, "MI"] = mae["MI"]
    MAPE[tabrow, "MI"] = mape["MI"]
    
    MAE[tabrow, "PSM"] = mae["PSM"]
    MAPE[tabrow, "PSM"] = mape["PSM"]
    
    MAE[tabrow, "null"] = mae["NULL"]
    MAPE[tabrow, "null"] = mape["NULL"]
        
    MLD[tabrow, "BART"] = mld["BART"]
    MPLD[tabrow, "BART"] = mpld["BART"]
    
    MLD[tabrow, "MI"] = mld["MI"]
    MPLD[tabrow, "MI"] = mpld["MI"]
        
    KSD[tabrow, "BART"] = ksd["BART"]
    KSPD[tabrow, "BART"] = kspd["BART"]
    
    KSD[tabrow, "MI"] = ksd["MI"]
    KSPD[tabrow, "MI"] = kspd["MI"]

  }

  if (i <= length(files_ppmx)) {
    load(paste0("postsim/", files_ppmx[i]))
    perc_miss = stringr::str_extract(strsplit(mesg, "_")[[1]][2], "\\d+") |> as.numeric()
    tabrow = which(MSE$ii == ii & MSE$pMiss == perc_miss)
        
    MSE[tabrow, "PPMx"] = mse
    MSPE[tabrow, "PPMx"] = mspe
    
    MAE[tabrow, "PPMx"] = mae
    MAPE[tabrow, "PPMx"] = mape
    
    MLD[tabrow, "PPMx"] = mld
    MPLD[tabrow, "PPMx"] = mpld
  
    KSD[tabrow, "PPMx"] = ksd
    KSPD[tabrow, "PPMx"] = kspd
  }
  
  if (i <= length(files_ppmxR)) {
    load(paste0("postsim/", files_ppmxR[i]))
    perc_miss = stringr::str_extract(strsplit(mesg, "_")[[1]][2], "\\d+") |> as.numeric()
    tabrow = which(MSE$ii == ii & MSE$pMiss == perc_miss)
    
    MSE[tabrow, "PPMxR"] = mse
    MSPE[tabrow, "PPMxR"] = mspe
    
    MAE[tabrow, "PPMxR"] = mae
    MAPE[tabrow, "PPMxR"] = mape
    
    KSD[tabrow, "PPMxR"] = ksd
    KSPD[tabrow, "PPMxR"] = kspd
    
    MLD[tabrow, "PPMxR"] = mld
    MPLD[tabrow, "PPMxR"] = mpld
  }
  
  cat(i, "\r")
}
rm(tabrow)

head(MSE)
head(MSPE); tail(MSPE)
head(MAPE); tail(MAPE)
head(MPLD); tail(MPLD)
head(KSD); tail(KSD)
head(KSPD); tail(KSPD)

boxplot(MSE[, mtds])
boxplot(MSPE[, mtds])


MSE_long = pivot_longer(MSE, cols=all_of(mtds), names_to="model", values_to="mse") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MSPE_long = pivot_longer(MSPE, cols=all_of(mtds), names_to="model", values_to="mspe") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MAE_long = pivot_longer(MAE, cols=all_of(mtds), names_to="model", values_to="mae") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MAPE_long = pivot_longer(MAPE, cols=all_of(mtds), names_to="model", values_to="mape") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MLD_long = pivot_longer(MLD, cols=all_of(mtds), names_to="model", values_to="mld") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MPLD_long = pivot_longer(MPLD, cols=all_of(mtds), names_to="model", values_to="mpld") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

KSD_long = pivot_longer(KSD, cols=all_of(mtds), names_to="model", values_to="ksd") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
KSPD_long = pivot_longer(KSPD, cols=all_of(mtds), names_to="model", values_to="kspd") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


ggplot(MSE_long, aes(x=as.factor(pMiss), y=mse, fill=model)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("MSE") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Boston housing (p=8)") + 
  ggtitle(paste0("Boston housing (p=8); null mse ", round(mean(MSE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mse_Boston_", dates, ".pdf"), width=5, height=3.5)


ggplot(MSPE_long, aes(x=as.factor(pMiss), y=mspe, fill=model)) + ylim(c(0, 1)) + theme_bw() +
  geom_boxplot() + ylab("MSPE") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Boston housing (p=8)") + 
  ggtitle(paste0("Boston housing (p=8); null mspe ", round(mean(MSPE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mspe_Boston_", dates, ".pdf"), width=5, height=3.5)


ggplot(MAE_long, aes(x=as.factor(pMiss), y=mae, fill=model)) + theme_bw() + #ylim(c(0, 1)) + 
  geom_boxplot() + ylab("MAE") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Boston housing (p=8)") + 
  ggtitle(paste0("Boston housing (p=8); null mae ", round(mean(MAE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mae_Boston_", dates, ".pdf"), width=5, height=3.5)


ggplot(MAPE_long, aes(x=as.factor(pMiss), y=mape, fill=model)) + theme_bw() + #ylim(c(0, 1)) + 
  geom_boxplot() + ylab("MAPE") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Boston housing (p=8)") + 
  ggtitle(paste0("Boston housing (p=8); null mape ", round(mean(MAPE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mape_Boston_", dates, ".pdf"), width=5, height=3.5)


ggplot(MLD_long, aes(x=as.factor(pMiss), y=-2*mld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("mean deviance") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Ozone") + 
  ggtitle(paste0("Boston housing (p=8)")) + scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mld_Boston_", dates, ".pdf"), width=5, height=3.5)

ggplot(MPLD_long, aes(x=as.factor(pMiss), y=-2*mpld, fill=model)) + theme_bw() + ylim(c(0, 5)) +
  geom_boxplot() + ylab("Mean Pred. Deviance") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Ozone") + 
  ggtitle(paste0("Boston housing (p=8)")) + scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mpld_Boston_", dates, ".pdf"), width=5, height=3.5)


ggplot(KSD_long, aes(x=as.factor(pMiss), y=ksd, fill=model)) + theme_bw() + ylim(c(0.0, 0.30)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Ozone") + 
  ggtitle(paste0("Boston housing (p=8); residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[ c(2,3,4,5) ])

ggsave(paste0("plots/ksd_Boston_", dates, ".pdf"), width=5, height=3.5)

ggplot(KSPD_long, aes(x=as.factor(pMiss), y=kspd, fill=model)) + theme_bw() + ylim(c(0.0, 0.30)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("% Missing") + labs(fill="Model") + #ggtitle("Ozone") + 
  ggtitle(paste0("Boston housing (p=8); predictive residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/kspd_Boston_", dates, ".pdf"), width=5, height=3.5)






MSPErat = MSPE
head(MSPErat)
for (m in mtds) {
  MSPErat[,m] = MSPE[, m] / MSPE[,"PPMxR"]
}
head(MSPE)
head(MSPErat)

MSPErat_long = pivot_longer(MSPErat, cols=all_of(mtds), names_to="model", values_to="mspe_ratio") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MSPErat_long)

ggplot(MSPErat_long, aes(x=as.factor(pMiss), y=mspe_ratio, fill=model)) + theme_bw() + ylim(c(0, 3)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MSPE Ratio to PPMxR") + xlab("% Missing") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mspeRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



MAPErat = MAPE
head(MAPErat)
for (m in mtds) {
  MAPErat[,m] = MAPE[, m] / MAPE[,"PPMxR"]
}
head(MAPE)
head(MAPErat)

MAPErat_long = pivot_longer(MAPErat, cols=all_of(mtds), names_to="model", values_to="mape_ratio") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MAPErat_long)

ggplot(MAPErat_long, aes(x=as.factor(pMiss), y=mape_ratio, fill=model)) + theme_bw() + ylim(c(0.5, 2.5)) + geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MAPE Ratio to PPMxR") + xlab("% Missing") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mapeRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



MPLDrat = MPLD
head(MPLDrat)
for (m in mtds) {
  MPLDrat[,m] = MPLD[, m] / MPLD[,"PPMxR"]
}
head(MPLD)
head(MPLDrat)

MPLDrat_long = pivot_longer(MPLDrat, cols=all_of(mtds), names_to="model", values_to="mpld_ratio") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MPLDrat_long)

ggplot(MPLDrat_long, aes(x=as.factor(pMiss), y=mpld_ratio, fill=model)) + theme_bw() + ylim(c(0.5, 2.5)) + geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MPLD Ratio to PPMxR") + xlab("% Missing") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mpldRatio_", datname, "_", dates, ".pdf"), width=8, height=6)



KSPDrat = KSPD
head(KSPDrat)
for (m in mtds) {
  KSPDrat[,m] = KSPD[, m] / KSPD[,"PPMxR"]
}
head(KSPD)
head(KSPDrat)

KSPDrat_long = pivot_longer(KSPDrat, cols=all_of(mtds), names_to="model", values_to="kspd_ratio") %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(KSPDrat_long)

ggplot(KSPDrat_long, aes(x=as.factor(pMiss), y=kspd_ratio, fill=model)) + theme_bw() + ylim(c(0.5, 2.5)) + geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("KSPD Ratio to PPMxR") + xlab("% Missing") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/kspdRatio_", datname, "_", dates, ".pdf"), width=8, height=6)



ggplot(MSPE, aes(x=BART, y=PPMxR, col=as.factor(pMiss))) + geom_point() + geom_abline(intercept=0, slope=1, lty=2) + labs(col="% Missing") + ggtitle("MSPE, Boston housing")
ggplot(MSPE, aes(x=BART, y=RF, col=as.factor(pMiss))) + geom_point() + geom_abline(intercept=0, slope=1, lty=2) + labs(col="% Missing") + ggtitle("MSPE, Boston housing")
ggplot(MSPE, aes(x=PPMxR, y=PPMx, col=as.factor(pMiss))) + geom_point() + geom_abline(intercept=0, slope=1, lty=2) + labs(col="% Missing") + ggtitle("MSPE, Boston housing")
