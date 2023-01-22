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
#NLP741X1138Hits$hits
#df_NLP = NLP741X1138Hits
#saveRDS(object=df_NLP, file="~/Dropbox/Stan/Data/NLP_DF")
df <- readRDS(file="~/Dropbox/Stan/Data/NLP_DF")
df

  df$u <- as.integer(df$uid)
  df$p = as.integer(df$pid)
  df$y = as.integer(df$hits)
df
  #fitdistr(lambda1$x, densfun = "log-normal")
dl = list(N = length(df$y), y = df$y, U=length(unique(df$uid)), xu=df$u)
dl

  lambdaU <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=mean)
  sigmaU <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=sd)
  sigmaU$x[is.na(sigmaU$x)] <- 0.05
  
initVals = list(mu = log(mean(lambdaU$x)),tau=mean(sigmaU$x), mu_p=log(lambdaU$x) ) 
initVals
#============================================ Model ======================================================
remove(sm741)
sm741 = stan_model("~/Dropbox/Stan/Models/NLP_U0.stan")
#=========================================================================================================
remove(fit741)
fit741 = sampling(object = sm741, data=dl, 
                init=list(initVals), 
                control=list(adapt_delta = 0.95), chains=1, iter=1000, warmup=500, thin=1)
#====================================== Plotting =========================================================
traceplot(fit741, pars=c("mu","tau"))
pairs(fit741, pars=c("mu","tau"))
print(fit741)
plot(fit741, pars = c("yp[1]","yp[2]","yp[3]","yp[4]","yp[5]","yp[12345]"), show_density=FALSE)
summary(fit741)

sims = extract(fit741, permuted=TRUE)

class(sims)
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters
mse_l=c()
class(mse_l)
for (i in 1:nrow(mtx)){
 mse_l <- c(mse_l, mse(df$y,mtx[i,]))
}

mse_l

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])
#=============================================================