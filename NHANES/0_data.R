library("NHANES")
library("tidyverse")

n_tot <- 1000
(n_tt <- n_tot - n_tn)

datname <- "NHANES"

variables <- c("Age", "AgeMonths", "Poverty", "HomeRooms", "BMI", "Pulse", "BPSysAve", "BPDiaAve", "TotChol", "DaysMentHlthBad", "SleepHrsNight", 
               "Gender", "Race1", "MaritalStatus", "PregnantNow", "Diabetes", "PhysActive", "SmokeNow")

dat0 <- as.data.frame(NHANES[,variables]) %>% filter(Age >= 18)
dim(dat0)
dat0$AgeMonths[which(is.na(dat0$AgeMonths))] <- 0
dat0$AgeMonths <- dat0$Age*12 + (dat0$AgeMonths %% 12)

miss_mat0 <- is.na(dat0)

covariates <- c("AgeMonths", "Poverty", "HomeRooms", "BMI", "Pulse", "TotChol", "DaysMentHlthBad", "SleepHrsNight")
response <- "BPSysAve"


indx_y_obs <- which(!is.na(dat0[,response]))

nmis0 <- miss_mat0[indx_y_obs, covariates] |> rowSums()
table(nmis0)

prob_use <- 5*nmis0 + 1
indx_use <- sample(length(indx_y_obs), size = n_tot, replace = FALSE, prob = prob_use)

miss_patterns0 <- apply(1*miss_mat0[indx_y_obs[indx_use], covariates], 1, function(x) paste(x, collapse=""))
table(miss_patterns0)

dat <- dat0[indx_y_obs[indx_use], c(response, covariates)]
head(dat)

dat$BPSysAve_jit <- jitter(dat[,"BPSysAve"], factor = 1.2)
dat$Poverty_logit <- qlogis(1.0 - (dat[,"Poverty"] + 0.1)/5.2) %>% jitter(., amount = 0.1)
dat$HomeRooms_jit <- jitter(dat[,"HomeRooms"])
dat$Pulse_jit <- jitter(dat[,"Pulse"])
dat$DaysMentHlthBad_log <- jitter(log(dat[,"DaysMentHlthBad"] + 1))
dat$SleepHrsNight_jit <- jitter(dat[,"SleepHrsNight"])
# plot(dat$SleepHrsNight_jit)

head(dat)
dim(dat)

y_csc <- scale(dat$BPSysAve_jit)[,1]
X_csc <- dat[,c("AgeMonths", "Poverty_logit", "HomeRooms_jit", "BMI", "Pulse_jit", "TotChol", "DaysMentHlthBad_log", "SleepHrsNight_jit")] |> scale()
head(X_csc)
colMeans(X_csc, na.rm = TRUE)
apply(X_csc, 2, sd, na.rm = TRUE)
# hist(dat$Poverty_logit, breaks = 50)

miss_mat <- is.na(X_csc)
mean(miss_mat)
# barplot(colMeans(miss_mat))
colMeans(miss_mat)

nmis <- miss_mat |> rowSums()


miss_patterns <- apply(1*miss_mat, 1, function(x) paste(x, collapse=""))
table(miss_patterns)
length(unique(miss_patterns))

indx_tn <- sample(n_tot, size = n_tn, replace = FALSE)
indx_tt <- setdiff(1:n_tot, indx_tn)

y_tn <- y_csc[indx_tn]
y_tt <- y_csc[-indx_tn]

X_tn <- X_csc[indx_tn,]
X_tt <- X_csc[-indx_tn,]

str(y_tt)
dim(X_tt)

miss_patterns_tn <- apply(1*miss_mat[indx_tn,], 1, function(x) paste(x, collapse=""))
table(miss_patterns_tn)
length(unique(miss_patterns_tn))
