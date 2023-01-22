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
df$u = as.integer(df$uid)
df$y = as.integer(df$hits)

lambda2 <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=mean)
lambda2
dl_2 = list(N = length(df$y), y = df$y, U=length(unique(df$uid)), xu=df$u)
#============================================ Model ======================================================
remove(sm1138)
sm1138 = stan_model("NLP1138.stan")
#=========================================================================================================
remove(fit1138)
fit1138 = sampling(object = sm1138, data=dl_2, 
                init=list(list(lambda =lambda2$x), list(lambda=lambda2$x), list(lambda=lambda2$x), list(lambda=lambda2$x)), 
                control=list(adapt_delta = 0.95), chains=4, iter=999, warmup=444, thin=1)
#========================================================================================================
traceplot(fit1138, pars=c("mse"))
pairs(fit1138, pars=c("lambda[1]", "lambda[1138]"))
print(fit1138)
plot(fit1138, pars = c("lambda[133]","lambda[1138]"), show_density=FALSE)
plot(fit1138, pars = c("yp[133]","yp[2]","yp[3]","yp[4]","yp[12345]"), show_density=FALSE)
plot(fit1138, pars = c("mse"), show_density=TRUE)
summary(fit1138)

sims = extract(fit1138, permuted=TRUE)

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
col = 24896
mtx[,col]
df$y[col]
df$p[col]
#=========================================================
pars1 = paste("yp[", col,"]", sep='')
pars1
pars2 = paste("lambda[",df$p[col],"]", sep='')
plot(fit741, pars=c(pars1))
plot(fit741, pars=c(pars2))
#========================================================