library("rstan")
library("parallel")
library(Metrics) # assessing MSE
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


  lambda_p <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=mean)
  sigma_p  <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=sd)
  sigma_p$x[is.na(sigma_p$x)] <- 0.01
  sigma_p$x <- sigma_p$x+0.01

    
  lambda_u <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=mean)
  sigma_u  <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=sd)
  sigma_u$x[is.na(sigma_u$x)] <- 0.01
  sigma_u$x <- sigma_u$x+0.01

dl <- list(
          N = length(df$y), 
          y = df$y, 
          xp = df$p,
          xu=df$u, 
          m=mt, 
          P=length(unique(df$p)), 
          V=ncol(mt),
          U=length(unique(df$u)), 
          MUP = log(lambda_p$x), 
          MUU = log(lambda_u$x),
          sigma_P = sigma_p$x,
          sigma_U = sigma_u$x
          )


#===================== load the doc2vec matrix ===============================
NLP740X222 <- read_csv("Stan/Data/NLP740X100.csv", col_names = FALSE)
mt = as.matrix(NLP740X222)
nrow(mt)
ncol(mt)
#===================== Model =================================================
remove(sm)
sm = stan_model(paste(getwd(), "/Stan/Models/NLP741X222PVU_MinMin.stan", sep=""))
#=========================================================================================================
remove(fit)
fit = sampling(
              object = sm, 
              data=dl, 
              init="random", 
              control=list(adapt_delta = 0.95), 
              chains=4, 
              iter=666, 
              warmup=333, 
              thin=1)
#====================================== Plotting =========================================================
#traceplot(fit, pars=c("beta_P", "beta_U"))
#pairs(fit, pars=c("MU","sigma"))
df$ind <- rownames(df)
pl = which(df$u==1038)
pl
ind
print(fit)
pnames[df$p==4]

pnames <- names(fit)
pnames
l = length(pnames)
l
beg = l-nrow(df)-1
pl+beg
end = l-1
plot(fit, pars = c(pnames[c(pl+beg)]))

#summary(fit)
#=========================================================
sims = extract(fit, permuted=TRUE)
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

yp <- mtx[321,]
y <- df$y

#library(Metrics)
mse(y, yp) # Mean Square Error 

MSE_sum=0
for (i in 1:nrow(mtx)){
  MSE_sum = MSE_sum + mse(mtx[i,], y)
  print(MSE_sum/i)
}

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])
#=============================================================
