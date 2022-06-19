#========== clear work space ===========
remove(list=ls())
#=========== enact packages ============
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
#saveRDS(object=HFA, file="~/Dropbox/Stan/Data/HFA.rds")
HFA <- readRDS(file="D:/Dropbox/Dropbox/Stan/Data/HFA.rds")
HFA
nrow(HFA)
HFA$L = as.integer(HFA$LID)
#HFA$Club[HFA$xC==78] <- "SV_Darmstadt_98"
club_names <- unique(HFA$Club)
club_names
#length(club_names)
#HFA$yd <- HFA$yH - HFA$yG

dl_sc <- list(N = nrow(HFA), yg = HFA$yG, yh=HFA$yH, 
              xc = as.integer(levels(HFA$xC)[HFA$xC]),
              C=length(unique(HFA$xC)))

#================ compile stan model ========================================
remove(sm)
sm = stan_model("D:/Dropbox/Dropbox/Stan/Models/HFA2_SC3.stan")
#======================== sampling ==========================================
remove(fit)
fit = sampling(object = sm, data=dl_sc, 
               init="random", 
               control=list(adapt_delta = 0.95), 
               chains=4, 
               iter=1234, warmup=789, thin=2)
#========================== plot =======================================================
traceplot(fit, pars=c("delta"))
pairs(fit, pars=c("sigmag_att","sigmah_att"))
plot(fit, pars=c("delta"), show_density=TRUE) + theme_Posterior


plot(fit, pars=c(names(fit)[c(2448, 2467, 2489, 2524, 2525,2530,2542)]),
     fill_color="#123489",
     est_color = "#ffffff",     
     )+ 
  geom_vline(xintercept = 0, linetype=3, size=1) + theme_Posterior


names(fit)[2445:2543]




plot(fit, pars=c("delta"), ci_level = 0.90, outer_level=0.95, 
     color="red",
     fill_color="#123489",
     est_color = "#ffffff",
     show_density=TRUE)  + theme_Posterior

plot(fit, pars=names(fit)[2524:2543], ci_level = 0.9, outer_level=0.95,
     fill_color="#123489",
     est_color = "#ffffff",
     ) + 
  #geom_vline(xintercept = 0, linetype=3, size=1) +
  coord_flip()+ 
  theme_Posterior

plot(fit, pars=c("att_h"))
plot(fit, pars=c("att_g"))
#======================== extract samples from stanfit object ==========================
class(names(fit)[2446:2543])

#cat(names(fit)[2446:2543], sep='","')
c(names(fit)[2446:2543])

pl = paste('"',pl, '"' , sep="")

names(fit)[2445] <- "Sport"

names(fit)[2446:2543]

club_names = unique(HFA$Club)

for (i in 2446:2543){
 names(fit)[i] <- club_names[i-2445] 
}


#================= mse ========================
sims = extract(fit, permuted=TRUE)
#=========================================================
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

#==============Predictive Model Checking =================
library(HDInterval)
library(foreach)

inbetween <- function(i){
  hd = hdi(mtx[,i], credMass = 0.5)
  if (HFA$yd > hd[2] || HFA$yd <hd[1])
    return (1)
  else
    return (0)
}

included <- foreach(i=1:ncol(mtx), .combine = "c") %do% 
  inbetween(i)

sum(included) / length(included)
#==============================




mse_l=c()
class(mse_l)
for (i in 1:nrow(mtx)){
  mse_l <- c(mse_l, mse(HFA$yd,mtx[i,]))
}
mean(mse_l)

max(mtx)
min(mtx)
mean(mtx)
median(mtx)
max(mtx[,2])