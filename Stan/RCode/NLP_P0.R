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

getNLP_DL <- function(df){
  #unique(df$uid) # 1138 users

  #nrow(df) # 26302 rows
  #ncol(df) # 3 columns
  df$u <- as.integer(df$uid)
  df$p = as.integer(df$pid)
  df$y = as.integer(df$hits)

  #max_p <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=max)
  #df$yt = df$y / max_p$x[df$p]
  #head(df)

  #lambda1 <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=mean)
  #lambda3 <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=sd)
  #lambda3$x[is.na(lambda3$x)] <- 0.05
  #lambda3$x

  #fitdistr(lambda1$x, densfun = "log-normal")

  #dl_1 = list(N = length(df$y), y = df$yt, P=length(unique(df$pid)), xp=df$p, mxp = max_p$x)
  return (list(N = length(df$y), y = df$y, P=length(unique(df$pid)), xp=df$p))
}

dl_1 <- getNLP_DL(df)
#============================================ Model ======================================================
remove(sm741)
sm741 = stan_model("~/Dropbox/Stan/Models/NLP_P0.stan")
#=========================================================================================================
remove(fit741)
fit741 = sampling(object = sm741, data=dl_1, 
                init=list(list(mu=mean(lambda1$x),tau = mean(lambda3$x) ,mu_p = lambda1$x)), 
                control=list(adapt_delta = 0.95), chains=1, iter=1000, warmup=500, thin=1)
#====================================== Plotting =========================================================
traceplot(fit741, pars=c("mu","tau","mse"))
pairs(fit741, pars=c("mu","tau"))
print(fit741)
plot(fit741, pars = c("lambda[1]","lambda[741]"), show_density=FALSE)
plot(fit741, pars = c("yp[1]","yp[2]","yp[3]","yp[4]","yp[5]","yp[12345]"), show_density=FALSE)
plot(fit741, pars = c("mse"), show_density=TRUE)
summary(fit741)

sims = extract(fit741, permuted=TRUE)

class(sims)
sims$lambda
sims$mse
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])
#=============================================================
max_mtx = max(mtx)
for (i in 1:ncol(mtx)){
  if (max(mtx[,i]) == max_mtx) {
    print(i)
  }
}
#===========================
col = 20431
mtx[,col]
df$y[col]
df$p[col]
#=========================================================