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
#========================Processing input data ================================
#NLP741X1138Hits$hits
#df_NLP = NLP741X1138Hits
#saveRDS(object=ptp, file=paste(getwd(), "/Stan/Data/Page_Topic_Loadings", sep=""))
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
  Sum_u
  
  i=c(1,2)
  yt=hits[,i]
  sumU = Sum_u$x[i]
#===================== load the doc2vec matrix ===============================
mtxl <- readRDS(file=paste(getwd(), "/Dropbox/Stan/Data/Page_Topic_Loadings", sep=""))
mtxl[1,]
for (i in 1:PL){
  mtxl[i,] = mtxl[i,]*Sum_p$x[i]
}
#====================== prepare datalist =====================================
dl <- list(
          N = length(df$y), 
          y = yt, 
          lambda=mtxl, 
          P=PL, 
          V=ncol(mtxl),
          U=ncol(yt), 
          TU = sumU
          )
#===================== Model =================================================
sfit = stan(
                file = paste(getwd(), "/Dropbox/Stan/Models/NLP20200101_DP20_1.stan", sep=""),
                model_name = "NLP20200101_DP",
                data = dl,
                init = "random",
                control=list(adapt_delta = 0.95),
                chains=3, 
                iter=666, 
                warmup=444, 
                thin=1,
                cores = 3,
                #verbose = TRUE
              )
#====================== Plotting =============================================
traceplot(sfit, pars=c("pta"))
#pairs(fit, pars=c("MU","sigma"))
pl = which(df$u==103) # index
pl
pnames <- names(sfit)
pnames[beg+pl]
as.character(df$p[pl])

l = length(pnames)
beg = l-nrow(df)-1

plot(sfit, pars = c("pta"), est_color="#123456", fill_color="#123456") + 
  labs(title = "Posterior Probability Plots" ,caption="Predicted hits for webpage 103")+
  labs(subtitle = "yp only", tag="Y")
  #scale_y_discrete(sec.axis=sec_axis(~., breaks=1:length(pl), labels = levels(pl)[pl]))
  #scale_y_discrete(breaks=df$u[pl])
  #coord_flip() #+ theme_Posterior
plot(sfit, pars=c("MSE"), show_density=TRUE)
#print(sfit)
#summary(fit)
#=========================================================
