library("rstan")
library("parallel")
library(Metrics) # assessing MSE
library(readr) # read in csv file
#======================= threading and avoid recompile ========================
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')
#===================== Working Directory ======================================
getwd()
setwd(getwd())
remove(list = ls()) # clear the current work space
remove(sfit)
#========================Processing input data ================================
#NLP741X1138Hits$hits
#df_NLP = NLP741X1138Hits
#saveRDS(object=ptp, file=paste(getwd(), "/Dropbox/Stan/Data/Page_Topic_Loadings", sep=""))
#df <- readRDS(file=paste(getwd(), "/Dropbox/Stan/Data/NLP_DF", sep=""))
df <- puu # from 20200105Dat_hits.R

  df$y = as.integer(df$n)
  sum(df$y)
  N=nrow(df)
  N
  xu=df$u
  xp=df$p
  PL = length(unique(df$p))
  PL
  UL = length(unique(df$u))
  UL
  hits = matrix(rep(0,PL*UL),nrow=PL,ncol=UL)
  hits[,480]
  for (i in 1:N){
      hits[xp[i],xu[i]]=df$y[i]
  }

  
  Sum_p <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=sum)
  Sum_p

  Sum_u <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=sum)
  Sum_u[order(Sum_u$x, decreasing = TRUE),]
  
  Sum_u$x[1120]
  fat = which(df$u==1120)
  df$y[fat]
  df$p[fat]
  df[fat,]
  hits[,480]
  
#===================== load the doc2vec matrix ===============================
#mtxl <- readRDS(file=paste(getwd(), "/Dropbox/Stan/Data/Page_Topic_Loadings", sep=""))
mtxl <- ptp
mtxl[741,]
for (i in 1:nrow(mtxl)){
  for (j in 1:ncol(mtxl)){
    if (mtxl[i,j]<0.05){
      mtxl[i,j] = 0
    } 
  }
}
for (i in 1:PL){
  mtxl[i,] = mtxl[i,]*Sum_p$x[i]
}
#=========== subsetting hits ======================================
#duel <- function(beg, end){  
#==================== Stan Model Compiling =============================================
sm = stan_model(paste(getwd(), "/Dropbox/Stan/Models/NLP20200103_Poisson.stan", sep=""))
#=======================================================================================
  ui=c(1120)  
  yt=hits[,ui]
  miss = 49
  sumu = Sum_u$x[ui]
#====================== prepare datalist =====================================
dl <- list(
          N = length(df$y), 
          y = yt, 
          lambda=mtxl,
          xp=df$p,
          xu=df$u,
          P=PL, 
          V=ncol(mtxl),
          U = UL,
          TU = sumu,
          Up = miss 
          )
#===================== Model =================================================
sfit = sampling(
                object=sm,
                data = dl,
                init = "random",
                control=list(adapt_delta = 0.95),
                chains=1, 
                iter=666, 
                warmup=333, 
                thin=1
                #cores = 4,
                #verbose = TRUE
              )
#====================== Plotting =========================
summary(sfit, pars=c("mse"))
plot(sfit, pars=c("theta"))

tpk = 4
dox <- ap_doc[ap_doc$t==tpk,]
dox

x = dox[order(dox$gamma, decreasing = TRUE),]$d
y = puu[puu$u==i,]$p

setdiff(x,y)[1:10] # recommendated pages for user u
#=========================================================

