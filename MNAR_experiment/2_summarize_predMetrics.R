rm(list=ls())
library("tidyverse")

## dates
# dates = 230829 # n = 750
dates = 230906 # n = 150
# s0 = 0.1
s0 = 0.2


files0 = list.files("postsim/")
length(files0)

files_comp = files0[grep(paste0("summary_.*_competitors_.*", dates, ".rda"), files0)]
head(files_comp)
length(files_comp)

files_ppmx = files0[grep(paste0("summary_.*_PPMxR_.*modtypeMean_.*_alph1_sigupper1_simTypeNNiChisq_indep_s0_", s0, "_.*_date", dates, ".rda"), files0)]
head(files_ppmx)
length(files_ppmx)

files_ppmxR = files0[grep(paste0("summary_.*_PPMxR_.*modtypeReg_.*_alph1_sigupper1_simTypeNNiChisq_indep_s0_", s0, "_.*_date", dates, ".rda"), files0)]
head(files_ppmxR)
length(files_ppmxR)

(n_files = length(files_comp) + length(files_ppmx) + length(files_ppmxR))
n_files * 16 # estimate number of rows in the long results file

mtds = c("RF", "BART", "MI", "PSM", "PPMx", "PPMxR", "NULL")
mtds_comp = setdiff(mtds, c("PPMx", "PPMxR"))
mtds_comp_use = list()
mtds_comp_use[["mse"]] = mtds_comp_use[["mspe"]] = mtds_comp_use[["mae"]] = mtds_comp_use[["mape"]] = mtds_comp
mtds_comp_use[["mld"]] = mtds_comp_use[["mpld"]] = mtds_comp_use[["ksd"]] = mtds_comp_use[["kspd"]] = c("BART", "MI")
mtds_comp_use[["nclus"]] = c("PPMx", "PPMxR")
mtds_comp_use
missTypes = c("MCAR", "MAR", "MNAR")
dataTypes = c("Additive", "Product")
(n_types = prod(length(missTypes), length(dataTypes)))
metrics = c("mse", "mspe", "mae", "mape", "mld", "mpld", "ksd", "kspd", "nclus")


results = data.frame(dataType=rep(NA, n_files*16), missType=NA, rep=NA, metric=NA, method=NA, value=NA)
head(results); tail(results)
str(results)

i = 1
for(ff in files_comp) {
  load(paste0("postsim/", ff))
  strsp = strsplit(ff, "_")[[1]]
  dataType_now = gsub("dataType", "", strsp[grep("dataType", strsp)])
  missType_now = gsub("missType", "", strsp[grep("missType", strsp)])
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in setdiff(metrics, "nclus") ) {
    for(mtd in mtds_comp_use[[mt]]) {
      results[i, "dataType"] = dataType_now
      results[i, "missType"] = missType_now
      results[i, "rep"] = rep_now
      results[i, "method"] = mtd
      results[i, "metric"] = mt
      results[i, "value"] = get(mt)[mtd]
      i = i + 1
      if(i %% 1000 == 0) cat(i, "\n")
    } 
  }
}
for (ff in files_ppmx) {
  load(paste0("postsim/", ff))
  strsp = strsplit(ff, "_")[[1]]
  dataType_now = gsub("dataType", "", strsp[grep("dataType", strsp)])
  missType_now = gsub("missType", "", strsp[grep("missType", strsp)])
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in metrics) {
    results[i, "dataType"] = dataType_now
    results[i, "missType"] = missType_now
    results[i, "rep"] = rep_now
    results[i, "method"] = "PPMx"
    results[i, "metric"] = mt
    results[i, "value"] = get(mt)
    i = i + 1
    if(i %% 1000 == 0) cat(i, "\n")
  }
}
for (ff in files_ppmxR) {
  load(paste0("postsim/", ff))
  strsp = strsplit(ff, "_")[[1]]
  dataType_now = gsub("dataType", "", strsp[grep("dataType", strsp)])
  missType_now = gsub("missType", "", strsp[grep("missType", strsp)])
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in metrics) {
    results[i, "dataType"] = dataType_now
    results[i, "missType"] = missType_now
    results[i, "rep"] = rep_now
    results[i, "method"] = "PPMxR"
    results[i, "metric"] = mt
    results[i, "value"] = get(mt)
    i = i + 1
    if(i %% 1000 == 0) cat(i, "\n")
  }
}

i
str(results)
head(results); tail(results)
allNA_indx = apply(results, 1, function(x) all(is.na(x))) %>% which
head(allNA_indx); tail(allNA_indx)
if (length(allNA_indx) > 0) {
  results = results[-allNA_indx,]
}
str(results)
head(results); tail(results)

write.csv(file=paste0("results_s0_", s0, "_", dates, ".csv"), results, row.names=FALSE)
# write.csv(file=paste0("results_s0_0.25_", dates, ".csv"), results, row.names=FALSE)

table(results$dataType, results$missType, results$metric)





rm(list=ls())
library("tidyverse")
library("scales")

dates = 230829
dates = 230906

# s0 = 0.1
s0 = 0.2
# s0 = 0.25 # appears to be preferred, especially for PPMx. PPMxR not much affected out of sample.

results = read.csv(paste0("results_s0_", s0, "_", dates, ".csv"), head=TRUE)
(mtds = unique(results$method))
results$missType = factor(results$missType, levels = c("MCAR", "MAR", "MNAR"))

str(results)
head(results); tail(results)


dataType_now = "Additive"
dataType_now = "Product"

# missType_now = "MCAR"
# missType_now = "MAR"
# missType_now = "MNAR"

missType_now = c("MCAR", "MAR", "MNAR")

results_now = results %>% filter(dataType==dataType_now, missType %in% missType_now, method != "NULL")

summary_null = results %>% filter(dataType==dataType_now, missType %in% missType_now, method == "NULL") %>% group_by(metric) %>% summarise(null=mean(value))
dim(results_now)

ggplot(results_now %>% filter(metric=="nclus"), aes(x=missType, y=value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("Number of Clusters") + xlab("missType")



ggplot(results_now %>% filter(metric=="mse"), aes(x=missType, y=value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("MSE") + xlab("") + 
  ggtitle(paste0("null mse ", round(filter(summary_null, metric=="mse")$null, 1)))

ggsave(paste0("plots/mse_dataType", dataType_now, "_s0_", s0, "_", dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="mspe"), aes(x=missType, y=value, fill=method)) + # ylim(c(3, 35)) + 
  theme_bw() +
  geom_boxplot() + ylab("MSPE") + xlab("") + 
  ggtitle(paste0("null mspe ", round(filter(summary_null, metric=="mspe")$null, 1)))

ggsave(paste0("plots/mspe_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="mae"), aes(x=missType, y=value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("MAE") + xlab("") + 
  ggtitle(paste0("null mae ", round(filter(summary_null, metric=="mae")$null, 1)))

ggsave(paste0("plots/mae_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="mape"), aes(x=missType, y=value, fill=method)) + # ylim(c(1, 5.5)) + 
  theme_bw() +
  geom_boxplot() + ylab("MAPE") + xlab("") + 
  ggtitle(paste0("null mape ", round(filter(summary_null, metric=="mape")$null, 1)))

ggsave(paste0("plots/mape_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="mld"), aes(x=missType, y=-2*value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("mean deviance") + xlab("") + 
  scale_fill_manual(values=hue_pal()(length(mtds)-1)[1:4])

ggsave(paste0("plots/deviance_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="mpld"), aes(x=missType, y=-2*value, fill=method)) + #ylim(c(3, 15)) + 
  theme_bw() +
  geom_boxplot() + ylab("mean pred. deviance") + xlab("") + 
  scale_fill_manual(values=hue_pal()(length(mtds)-1)[1:4])

ggsave(paste0("plots/predDeviance_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)


ggplot(results_now %>% filter(metric=="ksd"), aes(x=missType, y=value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("Kolmogorov-Smirnov distance") + xlab("") +
  ggtitle(paste0("Residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds)-1)[1:4])

ggsave(paste0("plots/GoFqqResid_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)

ggplot(results_now %>% filter(metric=="kspd"), aes(x=missType, y=value, fill=method)) + # ylim(c(0, 75)) + 
  theme_bw() +
  geom_boxplot() + ylab("Kolmogorov-Smirnov distance") + xlab("") +
  ggtitle(paste0("Predictive residual quantile-quantile")) + scale_fill_manual(values=hue_pal()(length(mtds)-1)[1:4])

ggsave(paste0("plots/GoFqqPred_dataType", dataType_now, "_s0_", s0, "_" , dates, ".pdf"), width=8, height=6)
