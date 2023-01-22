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
saveRDS(object=ptp, file=paste(getwd(), "/Dropbox/Stan/Data/Page_Topic_Loadings", sep=""))
df <- readRDS(file=paste(getwd(), "/Dropbox/Stan/Data/NLP_DF", sep=""))
df

  df$u <- as.integer(df$uid)
  df$p = as.integer(df$pid)
  df$y = as.integer(df$hits)
  N=nrow(df)
  N
  xu=df$uid
  xp=df$pid
  PL = length(unique(df$p))
  PL
  UL = length(unique(df$u))
  UL
  hits = matrix(rep(0,PL*UL),nrow=PL,ncol=UL)
  hits[,1]
  for (i in 1:N){
      hits[xp[i],xu[i]]=df$y[i]
  }

  
  Sum_p <- aggregate.data.frame(x=df$y, by=list(df$p), FUN=sum)
  Sum_p

  Sum_u <- aggregate.data.frame(x=df$y, by=list(df$u), FUN=sum)
  Sum_u[order(Sum_u$x, decreasing = TRUE),]
  
  Sum_u$x[480]
  fat = which(df$u==480)
  df$y[fat]
  df$p[fat]
  df[fat,]
  hits[,480]
"259"#===================== load the doc2vec matrix ===============================
#mtxl <- readRDS(file=paste(getwd(), "/Dropbox/Stan/Data/Page_Topic_Loadings", sep=""))
mtxl_555 <- ptp
mtxl <- mtxl_555
mtxl[1,]
for (i in 1:PL){
  mtxl[i,] = mtxl[i,]*Sum_p$x[i]/sum(df$y)
}
#=========== subsetting hits ======================================
#duel <- function(beg, end){  
#==================== Stan Model Compiling =============================================
sm = stan_model(paste(getwd(), "/Dropbox/Stan/Models/NLP20200101_DP20.stan", sep=""))
#=======================================================================================
#  print(paste("MSE[ 1 ]----> Page",beg, seq=" "))
#  print(paste("MSE[", end-beg+1,"] ----> Page", end, seq=" "))
  #cols=seq(from=471, 
   #        to=480, 
    #       by=1)
  i=480
  #i=i+1
  cols=c(i)
  cols
  yt=hits[,cols]
  sumU = Sum_u$x[cols]
#====================== prepare datalist =====================================
dl <- list(
          N = length(df$y), 
          y = yt, 
          lambda=mtxl, 
          P=PL, 
          V=ncol(mtxl),
          TU = sumU
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
#====================== Plotting =============================================
  traceplot(sfit, pars=c("mse"))
  summary(sfit, pars=c("mse"))
#=========================================================

