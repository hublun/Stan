library("rstan")
library("parallel")
#======================= threading and avoid recompile ========================
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#===================== Working Directory ======================================
getwd()
setwd(getwd())
remove(list = ls()) # clear the current work space
#========================Processing input data ================================
#NLP741X1138Hits$hits
#df_NLP = NLP741X1138Hits
#saveRDS(object=df_NLP, file="~/Dropbox/Stan/Data/NLP_DF")
df <- readRDS(file=paste(getwd(), "/Stan/Data/NLP_DF", sep=""))
df

  df$u <- as.integer(df$uid)
  df$p = as.integer(df$pid)
  df$y = as.integer(df$hits)

dl0 <- list(N = length(df$y), y = df$y)
dl0

  lambda_p <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=mean)
  sigma_p  <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=sd)
  sigma_p$x[is.na(sigma_p$x)] <- 0.01

  lambda_u <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=mean)
  sigma_u  <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=sd)
  sigma_u$x[is.na(sigma_u$x)] <- 0.01
  
initValues0 <- list(mu=log(mean(df$y)))
initValues0
#===================== load the doc2vec matrix ===============================

as.matrix()

#===================== Model =================================================
remove(sm)
sm = stan_model(paste(getwd(), "/Stan/Models/NLP0_0.stan", sep=""))
#=========================================================================================================
remove(fit)
fit = sampling(object = sm, data=dl0, 
                init=list(initValues0), 
                control=list(adapt_delta = 0.95), chains=1, iter=1000, warmup=500, thin=1)
#====================================== Plotting =========================================================
traceplot(fit, pars=c("mu"))
pairs(fit, pars=c("mu","yp[1]"))
print(fit)
plot(fit, pars = c("yp[1]","yp[2]","yp[3]","yp[4]","yp[5]","yp[12345]"), show_density=FALSE)

summary(fit)

sims = extract(fit, permuted=TRUE)
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

yp <- mtx[2,]
y <- df$y

library(Metrics)
mse(y, yp) # Mean Square Error 

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])
#=============================================================
