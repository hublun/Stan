library("rstan")
library("parallel")
#======================= threading and avoid recompile ========================
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#=================================Processing input data =============================================
NLP741X1138Hits$hits
df = NLP741X1138Hits

unique(df$pid) # 741 pages
unique(df$uid) # 1138 users

nrow(df) # 26302 rows
ncol(df) # 3 columns

df$y = as.integer(df$hits)

N = length(df$y) # total number of data points
N
y = df$y

lamba0 = mean(df$y) # initial value for lambda rates

dl0 = list(N = N, y = y)
#============================================ Model ======================================================
remove(sm0)
sm0 = stan_model("NLP0.stan")
#=========================================================================================================
remove(fit0)
fit0 = sampling(object = sm0, data=dl0, 
                      init=list(list(lambda =lamba0), list(lambda=lamba0), list(lambda=lamba0), list(lambda=lamba0)), 
                control=list(adapt_delta = 0.95),
                      chains=4, iter=999, warmup=444, thin=1)
#========================================================================================================
traceplot(fit0, pars=c("lambda", "mse"))
pairs(fit0, pars=c("lambda","mse"))
print(fit0)
plot(fit0, pars = c("lambda"), show_density=FALSE)
plot(fit0, pars = c("yp[1]","yp[12345]"), show_density=FALSE)
plot(fit0, pars = c("mse"), show_density=TRUE)
summary(fit0)

sims = extract(fit0, permuted=TRUE) # extract with no chain dimension

fdf = as.data.frame(fit0)
fdf$mse
class(sims)

sims$lambda
sims$mse
sims$yp[1:N]
length(sims$yp)/4/N
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters
#=========================================================
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

