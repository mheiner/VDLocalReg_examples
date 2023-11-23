rm(list = ls())

library("NHANES")
library("tidyverse")
library("GGally")
library("corrplot")

head(NHANES)

variables <- c("AgeMonths", "Poverty", "HomeRooms", "BMI", "Pulse", "BPSysAve", "BPDiaAve", "TotChol", "DaysMentHlthBad", "SleepHrsNight")

dat0 <- as.data.frame(NHANES[,variables])

summary(dat0)
ggpairs(dat0)

miss_mat <- is.na(dat0)
str(miss_mat)
corrplot(cor(miss_mat))
barplot(colMeans(miss_mat))

jacc <- function(x, y) {
  sum_bth <- sum(x & y)
  sum_bth / (sum(x) + sum(y) - sum_bth)
}

corr_jacc <- function(x) {
  n <- ncol(x)
  out <- matrix(0, nrow = n, ncol = n)
  rownames(out) <- colnames(out) <- colnames(x)
  for (i in 1:n) {
    for (j in 1:i) {
      out[i,j] <- jacc(x[,i], x[,j])
    }
  }
  (out + t(out)) - diag(n)
}

corr_jacc(miss_mat)
corrplot(corr_jacc(miss_mat), diag = FALSE,  add = TRUE, type = 'lower', method = 'number', 
         col = 'black',
         tl.pos = 'n', cl.pos = 'n')
corrplot(corr_jacc(miss_mat), col.lim = c(0,1), add = TRUE, type = "lower", method = "number", col = "black", diag = FALSE)


plot(dat0[,"BPSysAve"]) # striation

summary(dat0[,"DaysMentHlthBad"])
hist(dat0[,"DaysMentHlthBad"])
hist(log(dat0[,"DaysMentHlthBad"] + 1)) 
hist(qlogis((dat0[,"DaysMentHlthBad"] + 1.0)/31.0)) # drop?

hist(dat0[,"BPDiaAve"])
NHANES %>% select(c("BPDia1", "BPDia2", "BPDia3", "BPDiaAve")) %>% filter(BPDiaAve == 0) %>% print(n = "all") # set 0s to NA
dat0[which(dat0$BPDiaAve == 0), "BPDiaAve"] <- NA

hist(dat0[,"Poverty"])
hist(-dat0[,"Poverty"]/5.0)
hist(qlogis(1.0 - dat0[,"Poverty"]/5.0))
summary(dat0[,"Poverty"])

hist(dat0[,"SleepHrsNight"])
plot(dat0[,"SleepHrsNight"])

hist(dat0[,"HomeRooms"])
plot(dat0[,"HomeRooms"])

covariates <- c("AgeMonths", "Poverty", "HomeRooms", "BMI", "Pulse", "TotChol", "DaysMentHlthBad", "SleepHrsNight")

response <- "BPSysAve"

indx_y_obs <- which(!is.na(dat0[,response]))

nmis <- miss_mat[indx_y_obs, covariates] |> rowSums()
table(nmis)

prob_use <- 5*nmis + 1
indx_use <- sample(length(indx_y_obs), size = 800, replace = FALSE, prob = prob_use)

table(nmis[indx_use])
mean(miss_mat[indx_y_obs[indx_use], covariates])
barplot(colMeans(miss_mat[indx_y_obs[indx_use], covariates]))

1*miss_mat[indx_y_obs[indx_use], covariates]

miss_patterns <- apply(1*miss_mat[indx_y_obs[indx_use], covariates], 1, function(x) paste(x, collapse=""))
table(miss_patterns)


dat <- dat0[indx_y_obs[indx_use], c(response, covariates)]
head(dat)
ggpairs(dat)

dat$Poverty_logit <- qlogis(1.0 - dat[,"Poverty"]/5.0)
dat$HomeRooms_jit <- jitter(dat[,"HomeRooms"])
dat$Pulse_jit <- jitter(dat[,"Pulse"])
dat$DaysMentHlthBad_log <- jitter(log(dat[,"DaysMentHlthBad"] + 1))
dat$SleepHrsNight_jit <- jitter(dat[,"SleepHrsNight"])
plot(dat$SleepHrsNight_jit)

