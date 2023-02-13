rm(list=ls())
library("tidyverse")
library("scales")

# s0 = 0.1
s0 = 0.2 # better VDReg, VDLReg performance

dte = 230123 # sample size 300
# dte = 230204 # sample size 1000

df <- read.table(paste0("results_SimStudy2_s0_", s0, "_", dte, ".csv"), sep=",", header=TRUE)

mtds <- c("RF", "BART", "MI", "PSM", "PPMx", "PPMxR")



df$model <- df$method

df <- df %>% mutate(model = gsub("PPMxR", "VDLReg", model)) %>% mutate(model = gsub("PPMx", "VDReg", model)) %>%
  mutate(model = gsub("BART", "BARTm", model)) %>% 
  mutate(model = factor(model, levels=c("MI", "PSM", "RF", "BARTm", "VDReg", "VDLReg")))

# Put mpld on the deviance scale
df$value[df$metric=="mpld"] <- -2*(df$value[df$metric=="mpld"])
df$value[df$metric=="mld"] <- -2*(df$value[df$metric=="mld"])

# remove a few outliers that make it hard for MSPE comparisons
df <- df %>% filter((value < 30) | !(metric %in% c("mse", "mspe")))



ggplot(df[df$metric %in% c("mspe", "mpld","kspd") & !is.na(df$model) & df$missType=="MAR",], 
       aes(x=as.factor(pMiss), y=value, fill=model)) + 
      #  geom_boxplot(aes(color=model)) + # comment out for black outlines
       geom_boxplot() + 
       ylab(NULL) + xlab("% Missing")  + 
       facet_grid(factor(metric, levels = c("mspe", "mpld", "kspd")) ~ dataType, 
             scales="free_y", switch = "y",
             labeller = as_labeller(c(mpld = "Mean Pred. Deviance", mspe = "MSPE", kspd = "Goodness of Fit",
                                      "1" = "Constant Variance", "2" = "Not Constant Variance"))) +
       theme_bw(base_size=15) +
      scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) +
  # scale_color_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) + # comment out for black outlines
  labs(fill = "Model", color="Model") +
  theme(strip.background = element_blank(), strip.placement = "outside") 

ggsave(paste0("plots/friedmanMAR_s0", s0, "_", dte, ".pdf"), width=10, height=9)



ggplot(df[df$metric %in% c("mspe", "mpld", "kspd") & !is.na(df$model) & df$missType=="MNAR",], 
       aes(x=as.factor(pMiss), y=value, fill=model)) + 
  # geom_boxplot(aes(color=model)) + # comment out for black outlines
  geom_boxplot() + 
  ylab(NULL) + xlab("% Missing")  + 
  facet_grid(factor(metric, levels = c("mspe", "mpld", "kspd")) ~ dataType, 
             scales="free_y", switch = "y",
             labeller = as_labeller(c(mpld = "Mean Pred. Deviance", mspe = "MSPE", kspd = "Goodness of Fit", 
                                      "1" = "Constant Variance", "2" = "Not Constant Variance"))) +
  theme_bw(base_size=15) +
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) +
  # scale_color_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) + # comment out for black outlines
  labs(fill = "Model", color="Model") +
  theme(strip.background = element_blank(), strip.placement = "outside") 

ggsave(paste0("plots/friedmanMNAR_s0", s0, "_", dte, ".pdf"), width=10, height=9)


ggplot(df[df$metric %in% c("nclus") & !is.na(df$model),], 
       aes(x=as.factor(pMiss), y=value, fill=model)) + 
  geom_boxplot() + 
  ylab("Mean number of clusters") + 
  xlab("% Missing")  + 
  facet_grid(factor(missType, levels=c("MAR", "MNAR")) ~ dataType, 
             labeller = as_labeller(c("MAR" = "MAR", "MNAR" = "MNAR",
                                      "1" = "Constant Variance", "2" = "Not Constant Variance"))) +
  theme_bw(base_size=15) +
  scale_fill_manual(values=hue_pal()(length(mtds))[c(4,5)]) +
  labs(fill = "Model", color="Model") +
  theme(strip.background = element_blank()
  )

ggsave(paste0("plots/friedman_nclust_s0", s0, "_", dte, ".pdf"), width=10, height=9)




## in-sample metrics
ggplot(df[df$metric %in% c("mse", "mld","ksd") & !is.na(df$model) & df$missType=="MAR",], 
       aes(x=as.factor(pMiss), y=value, fill=model)) + 
  # geom_boxplot(aes(color=model)) + # comment out for black outlines
  geom_boxplot() + 
  ylab(NULL) + xlab("% Missing")  + 
  facet_grid(factor(metric, levels = c("mse", "mld", "ksd")) ~ dataType, 
             scales="free_y", switch = "y",
             labeller = as_labeller(c(mld = "Mean Deviance", mse = "MSE", ksd = "Goodness of Fit",
                                      "1" = "Constant Variance", "2" = "Not Constant Variance"))) +
  theme_bw(base_size=15) +
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) +
  # scale_color_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) + # comment out for black outlines
  labs(fill = "Model", color="Model") +
  theme(strip.background = element_blank(), strip.placement = "outside") 

ggsave(paste0("plots/friedmanMAR_inSample_s0", s0, "_", dte, ".pdf"), width=10, height=9)



ggplot(df[df$metric %in% c("mse", "mld","ksd") & !is.na(df$model) & df$missType=="MNAR",], 
       aes(x=as.factor(pMiss), y=value, fill=model)) + 
  # geom_boxplot(aes(color=model)) + # comment out for black outlines
  geom_boxplot() + 
  ylab(NULL) + xlab("% Missing")  + 
  facet_grid(factor(metric, levels = c("mse", "mld", "ksd")) ~ dataType, 
             scales="free_y", switch = "y",
             labeller = as_labeller(c(mld = "Mean Deviance", mse = "MSE", ksd = "Goodness of Fit",
                                      "1" = "Constant Variance", "2" = "Not Constant Variance"))) +
  theme_bw(base_size=15) +
  scale_fill_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) +
  # scale_color_manual(values=hue_pal()(length(mtds))[c(2,1,6,3,4,5)]) + # comment out for black outlines
  labs(fill = "Model", color="Model") +
  theme(strip.background = element_blank(), strip.placement = "outside") 

ggsave(paste0("plots/friedmanMNAR_inSample_s0", s0, "_", dte, ".pdf"), width=10, height=9)
