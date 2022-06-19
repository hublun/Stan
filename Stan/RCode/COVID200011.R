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
library(readr)
#Bundesliga2019_20 <- read_csv("Downloads/Bundesliga_2019-2020 - 2019-20.csv")
#write.csv(HFA, file="HFA.csv")
#saveRDS(object=Bundesliga2019_20, file="~/Dropbox/Stan/Data/COVID.rds")
# CVD <- readRDS(file="~/Dropbox/Stan/Data/COVID.rds")
# CVD
# teams = levels(as.factor(unique(CVD$Host)))
# class(teams)
# str(teams)
# teams[1]
# match(CVD$Host, teams)
# which(teams=="AUG")
# CVD$host=match(CVD$Host, teams)
# CVD$visitor = match(CVD$Visitor,teams)
# head(CVD)
# tail(CVD)
# ngames = nrow(CVD)
# CVD$gdate = as.Date(CVD$Date, tryFormats = "%m/%d/%Y")
# CVDs <- CVD[with(CVD, order(CVD$gdate)),]

# CVDs$hgp <- c(); #initialization of new column
# CVDs$vgp <- c();
# # compute games played so far this season
# for(g in 1:ngames){
#   CVDs$hgp[g] = sum(CVDs$host[1:g]==CVDs$host[g]) + sum(CVDs$visitor[1:g]==CVDs$host[g])
#   CVDs$vgp[g] = sum(CVDs$host[1:g] == CVDs$visitor[g]) + sum(CVDs$visitor[1:g] == CVDs$visitor[g])
# }
# finishing data preprocessing
#saveRDS(object=CVDs, file="~/Dropbox/Stan/Data/Sorted_Bundesliga_2019_20.rds")
#===============================================================================
# retrieving processed data file into HFA dataframe
HFA <- readRDS(file="~/Data/Sorted_Bundesliga_2019_20.rds")

#HFA$L = as.integer(HFA$LID)
#HFA$Club[HFA$xC==78] <- "SV_Darmstadt_98"
#club_names <- unique(HFA$Club)
#club_names
#length(club_names)
#HFA$yd <- HFA$yH - HFA$yG
ngames = nrow(HFA) 
ngames

nteams = length(unique(HFA$host))
nteams

HFA$COVID+1

dl <- list(
  ng = ngames,
  hg = HFA$HG,
  vg = HFA$VG,
  cv = HFA$COVID+1,
  ht = HFA$host,
  vt = HFA$visitor,
  nt = nteams
)

#dl_sc <- list(N = nrow(HFA), yg = HFA$yG, yh=HFA$yH, 
 #             xc = as.integer(levels(HFA$xC)[HFA$xC]),
  #            C=length(unique(HFA$xC)))
#dl_slc <- list(N = nrow(HFA), 
   #            yg = HFA$yG, 
    #           yh=HFA$yH,
     #          xl = as.integer(levels(HFA$xL)[HFA$xL]),
      #         L = length(unique(HFA$xL)),
       #        xc = as.integer(levels(HFA$xC)[HFA$xC]),
        #       C = length(unique(HFA$xC)))
#dl_slc
#================ compile stan model ========================================
remove(sm)
sm = stan_model("~/Dropbox/Stan/Models/COVID_HFA.stan")
#======================== sampling ==========================================
remove(fit)
fit = sampling(object = sm, data=dl, 
               init="random", 
               control=list(adapt_delta = 0.95), 
               chains=4, 
               iter=999, warmup=666, thin=1)
#========================== plot =======================================================
traceplot(fit, pars=c("hfa_d"))
pairs(fit, pars=c("hfas","hfa_d"))
summary(fit)

plot(fit, 
     pars=names(fit)[c(1,2)], 
     show_density=TRUE,
     fill_color="#998811",
     est_color="#ffffff",
     ci_level=0.9, outer_level=0.95) +
geom_vline(xintercept = 0, linetype=3, size=0.5) + theme_Posterior


plot(fit, 
     pars=c("hfa_d"), 
     show_density=TRUE,
     fill_color="#998811",
     est_color="#ffffff",
     ci_level=0.9, outer_level=0.95) + 
  geom_vline(xintercept = 0, linetype=3, size=0.5) + theme_Posterior

names(fit)


HFAL = names(fit)[3:7]

#names(fit)[2:99] <- club_names
names(fit)[1] <- "Regular"
names(fit)[2]<-'Pandemic'
plot(fit, pars=HFAL, ci_level = 0.70, outer_level=0.90, 
     color="red",
     fill_color="#123489",
     est_color = "#ffffff",
     show_density=FALSE)  + theme_Posterior

plot(fit, pars=HFAC, ci_level = 0.70, outer_level=0.90, 
     color="red",
     fill_color="#123489",
     est_color = "#ffffff",
     show_density=FALSE) + theme_Posterior

plot(fit, pars=names(fit)[2524:2543], ci_level = 0.8, outer_level=0.95,
     fill_color="#123489",
     est_color = "#ffffff",
     ) + 
  #geom_vline(xintercept = 0, linetype=3, size=1) +
  coord_flip()+ 
  theme_Posterior


#======================== extract samples from stanfit object ==========================
install.packages("latex2exp")
library(latex2exp)
TeX("$\alpha$")
class(names(fit)[2446:2543])

#cat(names(fit)[2446:2543], sep='","')
c(names(fit)[2446:2543])

pl = paste('"',pl, '"' , sep="")

names(fit)[2445] <- "Sport"

names(fit)[3:7] <- League_Names
League_Names <- unique(HFA$League)
League_Names


club_names = unique(HFA$Club)
names(fit)[2:99] <- club_names

for (i in 2446:2543){
 names(fit)[i] <- club_names[i-2445] 
}


#================= mse ======================================================
sims = extract(fit, permuted=TRUE)
class(sims$yp)
mtx = sims$yp
nrow(mtx) # iterations
ncol(mtx) # number of parameters

#==============Predictive Model Checking =================
library(HDInterval)
library(foreach)

inbetween <- function(i){
  hd = hdi(mtx[,i], credMass = 0.35)
  if (HFA$yd > hd[2] || HFA$yd <hd[1])
    return (0)
  else
    return (1)
}

included <- foreach(i=1:ncol(mtx), .combine = "c") %do% 
  inbetween(i)

sum(included) / length(included)
#============================================================
#install.packages("Metrics")
library("Metrics")



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