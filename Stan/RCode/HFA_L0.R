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
dl <- list(N = nrow(HFA), yg = HFA$yG, yh=HFA$yH, L=length(unique(HFA$xL)), xl=as.integer(levels(HFA$xL)[HFA$xL]))
#================ compile stan model ========================================
remove(sm)
sm = stan_model("~/Dropbox/Stan/Models/HFA_L00.stan")
#======================== sampling ==========================================
remove(fit)
fit = sampling(object = sm, data=dl, 
               init="random", 
               control=list(adapt_delta = 0.95), 
               chains=1, 
               iter=2000, warmup=1000, thin=1)
#========================== plot =======================================================
traceplot(fit, pars=c("delta_S"))
plot(fit, pars=c("delta_S"))
plot(fit, pars=c("delta_L"))
plot(fit, pars=c("yp[1]","yp[1122]"))
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