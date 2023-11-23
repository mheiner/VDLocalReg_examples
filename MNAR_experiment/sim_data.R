# set.seed(1)
# dataType <- "Additive"
# dataType <- "Product"
# missType <- "MNAR"
# nobs <- 500

f_smooth = function(x) {
  ## from Friedberg et al (2021)
  log(1.0 + exp(7.0 * (x - 0.7)))
}

# curve(f_smooth(x), from = 0, to = 1)

n = 1500
X = cbind(x1 = runif(n), x2 = runif(n))
# plot(X)

if (dataType == "Additive") {
  sd_e = 0.15
  mn = -0.5*X[,"x1"] + 0.5*f_smooth(X[,"x2"])
} else if (dataType == "Product") {
  sd_e = 0.08
  mn = -0.5*sin(2.5*X[,"x1"] - 0.4) * (1.0 + 0.5*f_smooth(X[,"x2"]))
}

y = mn + sd_e*rnorm(n)

sd_e / sd(y)

## missing (x2 only)

Xm = X

## MCAR

if (missType == "MCAR") {
  mis = runif(n) < 0.4
  mean(mis)
} else if (missType == "MAR") {
  mis = runif(n) < (1.2*X[,"x1"]^2)
  mean(mis)
} else if (missType == "MNAR") {
  mis = runif(n) < (1.2*X[,"x2"]^2)
  mean(mis)
}

Xm[which(mis), "x2"] = NA

ytn = y[1:nobs]
Xmat = Xm[1:nobs,]

ytt = y[(nobs+1):n]
Xpred = Xm[(nobs+1):n,]

n_tn = nobs
n_tt = n - nobs



## Visualize
# library("plotly")
# dat_plt = data.frame(x1=X[,1], x2=X[,2], y=y, mis=mis)
# dat_plt$status = c("observed", "missing")[dat_plt$mis + 1]

# # plot_ly() %>% add_markers(x=dat_plt$x1, y=dat_plt$x2, z=dat_plt$y)

# plot_ly(data=dat_plt, type="scatter3d", x=~x1, y=~x2, z=~y, color=~mis, mode="markers")

# library("ggplot2")
# ggplot(dat_plt, aes(x = x1, y = x2, color = mis)) + geom_point()


# library("viridis")
# p0 = ggplot(dat=dat_plt, aes(x=x1, y=x2, col=y, shape=status)) +
  # geom_point(size = 2) +
  # theme_bw() +
  # # scale_color_viridis(option="magma", limits=range(y)) +
  # scale_color_viridis(option="viridis", limits=range(y)) +
  # scale_shape_manual(values=c(1,19)) +
  # labs(x = expression(x[1]), y = expression(x[2]), fill="y") +
  # guides(shape = guide_legend(title = expression(x[2]))) +
  # labs(col = "y") +
  # ggtitle(ifelse(dataType == "Additive", "Additive", "Interaction"))

# p0

# ggsave(paste0("plots/data2d_", dataType, "_MNAR.pdf"), width = 4.5, height = 3.25)


# p1 = ggplot(dat=dat_plt, aes(x=x1, y=y, col=x2, shape=status)) +
  # geom_point(size = 2) +
  # theme_bw() +
  # # scale_color_viridis(option="magma", limits=range(y)) +
  # scale_color_viridis(option="viridis", limits=range(dat_plt$x2)) +
  # scale_shape_manual(values=c(1,19)) +
  # labs(x = expression(x[1]), y = expression(y), fill=expression(x[2])) +
  # guides(shape = guide_legend(title = expression(x[2]))) +
  # labs(col = expression(x[2])) +
  # ggtitle(ifelse(dataType == "Additive", "Additive", "Interaction"))

# p1

# ggsave(paste0("plots/data2d_x1y_", dataType, "_MNAR.pdf"), width = 4.5, height = 3.25)

# 
# 
# ngrid = 201
# x1_seq = seq(0, 1, len = ngrid)
# xgrid = expand.grid(x1 = x1_seq, x2 = x1_seq)
# mn_mat = matrix(NA, nrow=ngrid, ncol=ngrid); dimnames(mn_mat) = list(x1_seq, x1_seq)
# 
# for (i in 1:ngrid) {
#   for (j in 1:ngrid) {
#     mn_mat[i,j] = -0.5*x1_seq[i] + 0.5*f_smooth(x1_seq[j])
#   }
# }
# 
# for (i in 1:ngrid) {
#   for (j in 1:ngrid) {
#     mn_mat[i,j] = -0.5*sin(2.5*x1_seq[i] - 0.4) * (1.0 + 0.5*f_smooth(x1_seq[j]))
#   }
# }
# 
# library("viridis")
# p2 = ggplot(data.frame(m=c(mn_mat), x1=rep(x1_seq, times=ngrid), x2=rep(x1_seq, each=ngrid)), aes(x=x1, y=x2, fill=m)) +
#   geom_raster() + scale_fill_viridis(option="magma", limits=range(mn_mat)) + theme_bw() +
#   labs(x = expression(x[1]), y = expression(x[2]), fill="Mean Surface")
# 
# p2
