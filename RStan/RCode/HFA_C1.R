#========== clear work space ===========
remove(list=ls())
#=========== enact packages ============
library("rstan")
library("parallel")
#======================= threading and avoid recompile ========================
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#===================== Working Directory ======================================
getwd()
setwd("~")
remove(list = ls()) # clear the current work space
#========================Processing input data ================================
#saveRDS(object=HFA, file="~/Dropbox/Stan/Data/HFA.rds")
HFA <- readRDS(file="~/Dropbox/Stan/Data/HFA.rds")
HFA
nrow(HFA)




HFA$yd <- HFA$yH - HFA$yG
length(unique(HFA$xL))
dl <- list(N = nrow(HFA), yg = HFA$yG, yh=HFA$yH, 
            xc = as.integer(levels(HFA$xC)[HFA$xC]),
           C=length(unique(HFA$xC)))

#================ compile stan model ========================================
remove(sm)
sm = stan_model("~/Dropbox/Stan/Models/HFA_C1.stan")
#======================== sampling ==========================================
remove(fit)
fit = sampling(object = sm, data=dl, 
               init="random", 
               control=list(adapt_delta = 0.95), 
               chains=4, 
               iter=1000, warmup=500, thin=1)
#========================== plot =======================================================
traceplot(fit, pars=c("delta_S"))
pairs(fit, pars=c("tauL_G","tauL_H"))

plot(fit, pars=c("delta_S"))
plot(fit, pars=c("delta_C"))
plot(fit, pars=c("yp"))
#======================== extract samples from stanfit object ==========================
sims = extract(fit, permuted=TRUE)
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

mse_l=c()
class(mse_l)
for (i in 1:nrow(mtx)){
  mse_l <- c(mse_l, mse(HFA$yd,mtx[i,]))
}
mean(mse_l)

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])