rm(list=ls())
library("tidyverse")

## dates
dates = 230912 # n = 400
dates = 230913 # n = 400, jitter response
s0 = 0.4


files0 = list.files("postsim/")
length(files0)

files_comp = files0[grep(paste0("summary_.*_competitors_.*", dates, ".rda"), files0)]
head(files_comp)
length(files_comp)

files_ppmx = files0[grep(paste0("summary_.*_PPMxR_.*modtypeMean_.*_s0_", s0, "_.*_date", dates, ".rda"), files0)]
head(files_ppmx)
length(files_ppmx)

files_ppmxR = files0[grep(paste0("summary_.*_PPMxR_.*modtypeReg_.*_s0_", s0, "_.*_date", dates, ".rda"), files0)]
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
metrics = c("mse", "mspe", "mae", "mape", "mld", "mpld", "ksd", "kspd", "nclus")
# metrics = c("mse", "mspe", "mae", "mape", "mld", "mpld", "ksd", "kspd")


results = data.frame(rep=rep(NA, n_files*16), metric=NA, method=NA, value=NA)
head(results); tail(results)
str(results)

i = 1
for(ff in files_comp) {
  load(paste0("postsim/", ff))
  strsp = strsplit(ff, "_")[[1]]
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in setdiff(metrics, "nclus") ) {
    for(mtd in mtds_comp_use[[mt]]) {
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
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in metrics) {
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
  rep_now = gsub("ii", "", strsp[grep("ii", strsp)]) %>% as.numeric
  for(mt in metrics) {
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

write.csv(file = paste0("results_s0_", s0, "_", dates, ".csv"), results, row.names=FALSE)
# write.csv(file=paste0("results_s0_0.25_", dates, ".csv"), results, row.names=FALSE)

table(results$metric)

