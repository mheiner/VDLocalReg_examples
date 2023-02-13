rm(list=ls())
library("tidyverse")
library("ggplot2")
library("scales")

datname <- "Old_Faithful"

dates <- 220816 # used for runs in the paper


files0 <- list.files("postsim/")

files_comp <- files0[grep(paste0("summary_", datname, "_competitors_p.*", dates, ".rda"), files0)]
head(files_comp)
length(files_comp)

files_ppmx <- files0[grep(paste0("summary_", datname, "_PPMxR_modtypeMean_p.*_alph1_sigupper0.5_simTypeNNiChisq_indep_s0_0.5_.*_date", dates, ".rda"), files0)]
head(files_ppmx); tail(files_ppmx)
length(files_ppmx)

files_ppmxR <- files0[grep(paste0("summary_", datname, "_PPMxR_modtypeReg_p.*_alph1_sigupper0.5_simTypeNNiChisq_indep_s0_0.5_.*_date", dates, ".rda"), files0)]
head(files_ppmxR)
length(files_ppmxR)

(n_rep <- 100)
(n_rep == length(files_ppmx))
(n_rep == length(files_ppmxR))

ps <- c(1, 2, 3)
mtds <- c("RF", "BART", "MI", "PSM", "PPMx", "PPMxR")

MSE <- data.frame(p=rep(ps, each=n_rep), ii=rep(1:n_rep, times=length(ps)), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, PPMxR=NA, null=NA, avgMiss=NA)
MSPE <- data.frame(p=rep(ps, each=n_rep), ii=rep(1:n_rep, times=length(ps)), RF=NA, BART=NA, MI=NA, PSM=NA, PPMx=NA, PPMxR=NA, null=NA, avgMiss=NA)

MAE <- MSE
MAPE <- MSPE

MLD <- MSE
MPLD <- MSPE

MDLD <- MSE
MDPLD <- MSPE

KSD <- MSE
KSPD <- MSPE

for ( i in 1:max( length(files_ppmxR), length(files_ppmx), length(files_comp)) ) {
  
  if (i <= length(files_comp)) {
    load(paste0("postsim/", files_comp[i]))
    tabrow <- which(MSE$ii == ii & MSE$p == p_use)
    
    MSE[tabrow, "RF"] <- mse["RF"]
    MSPE[tabrow, "RF"] <- mspe["RF"]
    
    MSE[tabrow, "BART"] <- mse["BART"]
    MSPE[tabrow, "BART"] <- mspe["BART"]
    
    MSE[tabrow, "MI"] <- mse["MI"]
    MSPE[tabrow, "MI"] <- mspe["MI"]
    
    MSE[tabrow, "PSM"] <- mse["PSM"]
    MSPE[tabrow, "PSM"] <- mspe["PSM"]
  
    MSE[tabrow, "null"] <- mse["NULL"]
    MSPE[tabrow, "null"] <- mspe["NULL"]
    
    MAE[tabrow, "RF"] <- mae["RF"]
    MAPE[tabrow, "RF"] <- mape["RF"]
    
    MAE[tabrow, "BART"] <- mae["BART"]
    MAPE[tabrow, "BART"] <- mape["BART"]
    
    MAE[tabrow, "MI"] <- mae["MI"]
    MAPE[tabrow, "MI"] <- mape["MI"]
    
    MAE[tabrow, "PSM"] <- mae["PSM"]
    MAPE[tabrow, "PSM"] <- mape["PSM"]
    
    MAE[tabrow, "null"] <- mae["NULL"]
    MAPE[tabrow, "null"] <- mape["NULL"]
        
    MLD[tabrow, "BART"] <- mld["BART"]
    MPLD[tabrow, "BART"] <- mpld["BART"]
    
    MLD[tabrow, "MI"] <- mld["MI"]
    MPLD[tabrow, "MI"] <- mpld["MI"]
    
    MDLD[tabrow, "BART"] <- mdld["BART"]
    MDPLD[tabrow, "BART"] <- mdpld["BART"]
    
    MDLD[tabrow, "MI"] <- mdld["MI"]
    MDPLD[tabrow, "MI"] <- mdpld["MI"]
    
    KSD[tabrow, "BART"] <- ksd["BART"]
    KSPD[tabrow, "BART"] <- kspd["BART"]
    
    KSD[tabrow, "MI"] <- ksd["MI"]
    KSPD[tabrow, "MI"] <- kspd["MI"]
    
  }

  if (i <= length(files_ppmx)) {
    load(paste0("postsim/", files_ppmx[i]))
    tabrow <- which(MSE$ii == ii & MSE$p == p_use)
    
    MSE[tabrow, "PPMx"] <- mse
    MSPE[tabrow, "PPMx"] <- mspe
    
    MAE[tabrow, "PPMx"] <- mae
    MAPE[tabrow, "PPMx"] <- mape

    MLD[tabrow, "PPMx"] <- mld
    MPLD[tabrow, "PPMx"] <- mpld
    
    MDLD[tabrow, "PPMx"] <- mdld
    MDPLD[tabrow, "PPMx"] <- mdpld

    KSD[tabrow, "PPMx"] <- ksd
    KSPD[tabrow, "PPMx"] <- kspd
  }
  
  if (i <= length(files_ppmxR)) {
    load(paste0("postsim/", files_ppmxR[i]))
    tabrow <- which(MSE$ii == ii & MSE$p == p_use)

    MSE[tabrow, "PPMxR"] <- mse
    MSPE[tabrow, "PPMxR"] <- mspe
    
    MAE[tabrow, "PPMxR"] <- mae
    MAPE[tabrow, "PPMxR"] <- mape

    MLD[tabrow, "PPMxR"] <- mld
    MPLD[tabrow, "PPMxR"] <- mpld

    MDLD[tabrow, "PPMxR"] <- mdld
    MDPLD[tabrow, "PPMxR"] <- mdpld
    
    KSD[tabrow, "PPMxR"] <- ksd
    KSPD[tabrow, "PPMxR"] <- kspd
    
    MSE[tabrow, "avgMiss"] <- mean(nmis[indx_tn])
    MSPE[tabrow, "avgMiss"] <- mean(nmis[indx_tn])
  }
  
  cat(i, "\r")
}

head(MSE)
head(MSPE); tail(MSPE)
head(MAPE); tail(MAPE)
head(MPLD); tail(MPLD)
head(MDPLD); tail(MDPLD)
head(KSD); tail(KSD)
head(KSPD); tail(KSPD)

MSE_long <- pivot_longer(MSE, cols=all_of(mtds), names_to="model", values_to="mse") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MSPE_long <- pivot_longer(MSPE, cols=all_of(mtds), names_to="model", values_to="mspe") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


MAE_long <- pivot_longer(MAE, cols=all_of(mtds), names_to="model", values_to="mae") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MAPE_long <- pivot_longer(MAPE, cols=all_of(mtds), names_to="model", values_to="mape") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


MLD_long <- pivot_longer(MLD, cols=all_of(mtds), names_to="model", values_to="mld") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MPLD_long <- pivot_longer(MPLD, cols=all_of(mtds), names_to="model", values_to="mpld") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


MDLD_long <- pivot_longer(MDLD, cols=all_of(mtds), names_to="model", values_to="mdld") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

MDPLD_long <- pivot_longer(MDPLD, cols=all_of(mtds), names_to="model", values_to="mdpld") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


KSD_long <- pivot_longer(KSD, cols=all_of(mtds), names_to="model", values_to="ksd") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

KSPD_long <- pivot_longer(KSPD, cols=all_of(mtds), names_to="model", values_to="kspd") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))


ggplot(MSE_long, aes(x=pf, y=mse, fill=model)) + theme_bw() + # ylim(c(0, 2)) +
  geom_boxplot() + ylab("MSE") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; null mse ", round(mean(MSE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mse_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(MSPE_long, aes(x=pf, y=mspe, fill=model)) + theme_bw() + # ylim(c(0, 2)) +
  geom_boxplot() + ylab("MSPE") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; null mspe ", round(mean(MSPE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mspe_", datname, "_", dates, ".pdf"), width=5, height=3.5)


ggplot(MAE_long, aes(x=pf, y=mae, fill=model)) + theme_bw() + # ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("MAE") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; null mae ", round(mean(MAE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mae_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(MAPE_long, aes(x=pf, y=mape, fill=model)) + theme_bw() + # ylim(c(0.25, 1.0)) +
  geom_boxplot() + ylab("MAPE") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; null mape ", round(mean(MAPE$null), 1))) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mape_", datname, "_", dates, ".pdf"), width=5, height=3.5)


ggplot(MLD_long, aes(x=pf, y=-2*mld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("mean deviance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mld_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(MPLD_long, aes(x=pf, y=-2*mpld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("Mean Pred. Deviance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mpld_", datname, "_", dates, ".pdf"), width=5, height=3.5)


ggplot(MDLD_long, aes(x=pf, y=-2*mdld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("median deviance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mdld_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(MDPLD_long, aes(x=pf, y=-2*mdpld, fill=model)) + theme_bw() + #ylim(c(0.25, 1.5)) +
  geom_boxplot() + ylab("median pred. deviance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mdpld_", datname, "_", dates, ".pdf"), width=5, height=3.5)


ggplot(KSD_long, aes(x=pf, y=ksd, fill=model)) + theme_bw() + ylim(c(0, 0.25)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/ksd_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(KSPD_long, aes(x=pf, y=kspd, fill=model)) + theme_bw() + ylim(c(0, 0.25)) +
  geom_boxplot() + ylab("Kolmogorov-Smirnov Distance") + xlab("Covariates") + labs(fill="Model") + #ggtitle(datname) + 
  ggtitle(paste0(datname, "; predictive residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/kspd_", datname, "_", dates, ".pdf"), width=5, height=3.5)





MSPErat <- MSPE
head(MSPErat)
for (m in mtds) {
  MSPErat[,m] <- MSPE[, m] / MSPE[,"PPMxR"]
}
head(MSPE)
head(MSPErat)

MSPErat_long <- pivot_longer(MSPErat, cols=all_of(mtds), names_to="model", values_to="mspe_ratio") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MSPErat_long)

ggplot(MSPErat_long, aes(x=pf, y=mspe_ratio, fill=model)) + theme_bw() + #ylim(c(0, 3)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MSPE Ratio to VDLReg") + xlab("Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])
  
ggsave(paste0("plots/mspeRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)




MAPErat <- MAPE
head(MAPErat)
for (m in mtds) {
  MAPErat[,m] <- MAPE[, m] / MAPE[,"PPMxR"]
}
head(MAPE)
head(MAPErat)

MAPErat_long <- pivot_longer(MAPErat, cols=all_of(mtds), names_to="model", values_to="mape_ratio") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MAPErat_long)

ggplot(MAPErat_long, aes(x=pf, y=mape_ratio, fill=model)) + theme_bw() + #ylim(c(0.5, 2.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MAPE Ratio to VDLReg") + xlab("Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)])

ggsave(paste0("plots/mapeRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



MPLDrat <- MPLD
head(MPLDrat)
for (m in mtds) {
  MPLDrat[,m] <- MPLD[, m] / MPLD[,"PPMxR"]
}
head(MPLD)
head(MPLDrat)

MPLDrat_long <- pivot_longer(MPLDrat, cols=all_of(mtds), names_to="model", values_to="mpld_ratio") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MPLDrat_long)

ggplot(MPLDrat_long, aes(x=pf, y=mpld_ratio, fill=model)) + theme_bw() + # ylim(c(0.5, 2.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MPD Ratio to VDLReg") + xlab("Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mpldRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)

ggplot(MPLDrat_long, aes(x=pf, y=log(mpld_ratio), fill=model)) + theme_bw() + # ylim(c(0.5, 2.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MPLD Ratio to VDLReg") + xlab("Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[2:5])





MDPLDrat <- MDPLD
head(MDPLDrat)
for (m in mtds) {
  MDPLDrat[,m] <- MDPLD[, m] / MDPLD[,"PPMxR"]
}
head(MDPLD)
head(MDPLDrat)

MDPLDrat_long <- pivot_longer(MDPLDrat, cols=all_of(mtds), names_to="model", values_to="mdpld_ratio") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(MDPLDrat_long)

ggplot(MDPLDrat_long, aes(x=pf, y=mdpld_ratio, fill=model)) + theme_bw() + # ylim(c(0.5, 2.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("MDPLD Ratio to VDLReg") + xlab("Covariates") + labs(fill="Model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/mdpldRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)



KSPDrat <- KSPD
head(KSPDrat)
for (m in mtds) {
  KSPDrat[,m] <- KSPD[, m] / KSPD[,"PPMxR"]
}
head(KSPD)
head(KSPDrat)

KSPDrat_long <- pivot_longer(KSPDrat, cols=all_of(mtds), names_to="model", values_to="kspd_ratio") %>% 
  mutate(pf = factor(as.character(p), labels=c("w1, w2", "w1, d1", "w1, w2, d1"))) %>% 
  mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>% 
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))
head(KSPDrat_long)

ggplot(KSPDrat_long, aes(x=pf, y=kspd_ratio, fill=model)) + theme_bw() + #ylim(c(0.5, 2.5)) + 
  geom_hline(yintercept=1, lty=2) +
  geom_boxplot() + ylab("KSPD Ratio to VDLReg") + xlab("Covariates") + labs(fill="model") + ggtitle(datname) + 
  scale_fill_manual(values=hue_pal()(length(mtds))[2:5])

ggsave(paste0("plots/kspdRatio_", datname, "_", dates, ".pdf"), width=5, height=3.5)










plot(MSE[,"BART"], MSE[,"PPMxR"]); abline(0, 1, lty=2)
plot(MSE[,"RF"], MSE[,"PPMxR"]); abline(0, 1, lty=2)
plot(MSE[,"PPMx"], MSE[,"PPMxR"]); abline(0, 1, lty=2)

plot(MSPE[,"BART"], MSPE[,"PPMx"]); abline(0, 1, lty=2)
plot(MSPE[,"BART"], MSPE[,"PPMxR"]); abline(0, 1, lty=2)
plot(MSPE[,"BART"], MSPE[,"RF"]); abline(0, 1, lty=2)

plot(MSPE[,"PPMx"], MSPE[,"PPMxR"]); abline(0, 1, lty=2)

