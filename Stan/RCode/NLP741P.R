library("rstan")
library("parallel")
#======================= threading and avoid recompile ========================
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#=================================Processing input data =============================================
NLP741X1138Hits$hits
df = NLP741X1138Hits

unique(df$uid) # 1138 users

nrow(df) # 26302 rows
ncol(df) # 3 columns
df$p = as.integer(df$pid)
df$y = as.integer(df$hits)

lambda1 <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=mean)
lambda1
dl_1 = list(N = length(df$y), y = df$y, P=length(unique(df$pid)), xp=df$p)
#============================================ Model ======================================================
remove(sm741)
sm741 = stan_model("NLP741_1.stan")
#=========================================================================================================
remove(fit741)
fit741 = sampling(object = sm741, data=dl_1, 
                init=list(list(lambda =lambda1$x), list(lambda=lambda1$x), list(lambda=lambda1$x), list(lambda=lambda1$x)), 
                control=list(adapt_delta = 0.95), chains=4, iter=999, warmup=444, thin=1)
#========================================================================================================
traceplot(fit741, pars=c("mse"))
pairs(fit741, pars=c("lambda[1]","lambda[2]", "lambda[741]"))
print(fit741)
plot(fit741, pars = c("lambda[1]","lambda[741]"), show_density=FALSE)
plot(fit741, pars = c("yp[1]","yp[2]","yp[3]","yp[4]","yp[12345]"), show_density=FALSE)
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