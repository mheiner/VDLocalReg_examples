rm(list=ls())
library("tidyverse")
library("scales")

## dates
dates_c = 220813
dates = 220813
sigupper = 0.9; s0 = 0.5
## paper only has replicates from 1-20, 61-100


files0 = list.files("postsim/")

files_comp = files0[grep(paste0("summary_SUPPORT_competitors_p.*", dates_c, ".rda"), files0)]
head(files_comp)
length(files_comp)

files_ppmx = files0[grep(paste0("summary_SUPPORT_PPMxR_modtypeMean_p.*_alph1_sigupper", sigupper, "_simTypeNNiChisq_indep_s0_", s0, "_.*_date", dates, ".rda"), files0)]
head(files_ppmx); tail(files_ppmx)
length(files_ppmx)

files_ppmxR = files0[grep(paste0("summary_SUPPORT_PPMxR_modtypeReg_p.*_alph1_sigupper", sigupper, "_simTypeNNiChisq_indep_s0_", s0, "_.*_date", dates, ".rda"), files0)]
head(files_ppmxR)
length(files_ppmxR)

(n_rep = 100) # keep it at 100 because index goes up to 100
(n_rep == length(files_ppmx) / 2)
(n_rep == length(files_ppmxR) / 2)

ps = c(6, 10)
mtds = c("RF", "BART", "MI", "PSM", "PPMx", "PPMxR")

MSE = data.frame(p=rep(ps, each=n_rep), ii=rep(1:n_rep, times=length(ps)), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, PPMxR=NA, null=NA, avgMiss=NA)
MSPE = data.frame(p=rep(ps, each=n_rep), ii=rep(1:n_rep, times=length(ps)), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, PPMxR=NA, null=NA, avgMiss=NA)

MAE = MSE
MAPE = MSPE

MLD = MSE
MPLD = MSPE

KSD = MSE
KSPD = MSPE

for ( i in 1:max(length(files_comp), length(files_ppmx), length(files_ppmxR)) ) {
  
  if (i <= length(files_comp)) {
    load(paste0("postsim/", files_comp[i]))
    tabrow = which(MSE$ii == ii & MSE$p == p_use)
    
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
    tabrow = which(MSE$ii == ii & MSE$p == p_use)
    
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
    tabrow = which(MSE$ii == ii & MSE$p == p_use)
    
    MSE[tabrow, "PPMxR"] = mse
    MSPE[tabrow, "PPMxR"] = mspe
    
    MAE[tabrow, "PPMxR"] = mae
    MAPE[tabrow, "PPMxR"] = mape
    
    MLD[tabrow, "PPMxR"] = mld
    MPLD[tabrow, "PPMxR"] = mpld
    
    KSD[tabrow, "PPMxR"] = ksd
    KSPD[tabrow, "PPMxR"] = kspd
    
    MSE[tabrow, "avgMiss"] = mean(nmis[indx_tn])
    MSPE[tabrow, "avgMiss"] = mean(nmis[indx_tn])
  }
  
  cat(i, "\r")
}

head(MSE)
head(MSPE); tail(MSPE)
head(MAPE); tail(MAPE)
head(MPLD); tail(MPLD)
head(KSD); tail(KSD)
head(KSPD); tail(KSPD)


MSE_long = pivot_longer(MSE, cols=all_of(mtds), names_to="model", values_to="mse") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MSPE_long = pivot_longer(MSPE, cols=all_of(mtds), names_to="model", values_to="mspe") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MAE_long = pivot_longer(MAE, cols=all_of(mtds), names_to="model", values_to="mae") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MAPE_long = pivot_longer(MAPE, cols=all_of(mtds), names_to="model", values_to="mape") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MLD_long = pivot_longer(MLD, cols=all_of(mtds), names_to="model", values_to="mld") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
MPLD_long = pivot_longer(MPLD, cols=all_of(mtds), names_to="model", values_to="mpld") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

KSD_long = pivot_longer(KSD, cols=all_of(mtds), names_to="model", values_to="ksd") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
KSPD_long = pivot_longer(KSPD, cols=all_of(mtds), names_to="model", values_to="kspd") %>% filter(ii <= 20 | ii > 60) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


ggplot(MSE_long, aes(x=as.factor(p), y=mse, fill=model)) + theme_bw() + # ylim(c(0, 2)) +
  geom_boxplot() + ylab("MSE") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; null mse ", round(mean(MSE$null, na.rm=T), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mse_SUPPORT_", dates, ".pdf"), width=5, height=3.5)

ggplot(MSPE_long, aes(x=as.factor(p), y=mspe, fill=model)) + theme_bw() + ylim(c(0.5, 2)) +
  geom_boxplot() + ylab("MSPE") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; null mspe ", round(mean(MSPE$null, na.rm=T), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mspe_SUPPORT_", dates, ".pdf"), width=5, height=3.5)


ggplot(MAE_long, aes(x=as.factor(p), y=mae, fill=model)) + theme_bw() + # ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("MAE") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; null mae ", round(mean(MAE$null, na.rm=T), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mae_SUPPORT_", dates, ".pdf"), width=5, height=3.5)

ggplot(MAPE_long, aes(x=as.factor(p), y=mape, fill=model)) + theme_bw() + #ylim(c(0.25, 1.0)) +
  geom_boxplot() + ylab("MAPE") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; null mape ", round(mean(MAPE$null, na.rm=T), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mape_SUPPORT_", dates, ".pdf"), width=5, height=3.5)


ggplot(MLD_long, aes(x=as.factor(p), y=-2*mld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("mean deviance") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT")) + scale_fill_manual(values=hue_pal()(length(mtds))[1:4]) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mld_SUPPORT_", dates, ".pdf"), width=5, height=3.5)

ggplot(MPLD_long, aes(x=as.factor(p), y=-2*mpld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("Mean Pred. Deviance") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT")) + scale_fill_manual(values=hue_pal()(length(mtds))[1:4])  + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mpld_SUPPORT_", dates, ".pdf"), width=5, height=3.5)

ggplot(KSD_long, aes(x=as.factor(p), y=ksd, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[1:4]) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/ksd_SUPPORT_", dates, ".pdf"), width=5, height=3.5)

ggplot(KSPD_long, aes(x=as.factor(p), y=kspd, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("No. of Covariates") + labs(fill="Model") + #ggtitle("SUPPORT") + 
  ggtitle(paste0("SUPPORT; predictive residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[1:4]) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/kspd_SUPPORT_", dates, ".pdf"), width=5, height=3.5)



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

ggplot(MSPErat_long, aes(x=as.factor(p), y=mspe_ratio, fill=model)) + theme_bw() + ylim(c(0.75, 1.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MSPE Ratio to VDLReg") + xlab("No. of Covariates") + labs(fill="Model") + ggtitle(datname) + 
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

ggplot(MAPErat_long, aes(x=as.factor(p), y=mape_ratio, fill=model)) + theme_bw() + #ylim(c(0.5, 2.5)) + 
	geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MAPE Ratio to VDLReg") + xlab("No. of Covariates") + labs(fill="Model") + ggtitle(datname) + 
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

ggplot(MPLDrat_long, aes(x=as.factor(p), y=mpld_ratio, fill=model)) + theme_bw() + # ylim(c(0.5, 2.5)) + 
	geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MPD Ratio to VDLReg") + xlab("No. of Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])

ggsave(paste0("plots/mpldRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



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

ggplot(KSPDrat_long, aes(x=as.factor(p), y=kspd_ratio, fill=model)) + theme_bw() + # ylim(c(0.5, 5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("KSPD Ratio to VDLReg") + xlab("No. of Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,3,4,5)])
  
ggsave(paste0("plots/kspdRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



(indx_p = which(MSPE$p == 6))
(indx_p = which(MSPE$p == 10))

plot(MSE[indx_p, "BART"], MSE[indx_p, "PPMxR"]); abline(0, 1, lty=2)
plot(MSE[indx_p, "RF"], MSE[indx_p, "PPMxR"]); abline(0, 1, lty=2)
plot(MSE[indx_p, "PPMx"], MSE[indx_p, "PPMxR"]); abline(0, 1, lty=2)

plot(MSPE[indx_p, "BART"], MSPE[indx_p, "PPMx"]); abline(0, 1, lty=2)
plot(MSPE[indx_p, "BART"], MSPE[indx_p, "PPMxR"]); abline(0, 1, lty=2)
plot(MSPE[indx_p, "BART"], MSPE[indx_p, "RF"]); abline(0, 1, lty=2)

plot(MSPE[indx_p, "PPMx"], MSPE[indx_p, "PPMxR"]); abline(0, 1, lty=2)

plot(MSPE[indx_p, "RF"], MSPE[indx_p, "PPMxR"]); abline(0, 1, lty=2)

