#================== paralell computing multiple core =========
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#============================================================
 
#===========Plot Gamma ================

#===========compile model ========================
remove(Stan_Model_3L)

stanDso = stan_model("Top5_Model_0.stan") 

sm011 =  stan_model("Top5_Model_011.stan") 

sm11 = stan_model("Top5_Model_11.stan") 

sm2 = stan_model("Top5_Model_2.stan") 

Stan_Model_3L = stan_model("HFA_Stan_Model_3_Levels_201906.stan")
#===============  Data Feed ======================
HWS <- ET5[,c(1,2,3,4,6,7)] # import from raw data
head(HWS)

HWS$xL = factor(HWS$LID, levels = unique(HWS$LID), labels = c("1","2","3","4","5"))
HWS$League <- toupper(HWS$League)
HWS$xC = factor(HWS$CID, levels = unique(HWS$CID), labels=1:98)       
HWS$xS = factor(HWS$Season, levels=unique(HWS$Season), labels = 1:16)

HWS$yH = as.integer(HWS$MostHomeGoals)
HWS$yG = as.integer(HWS$MostAwayGoals)


hist(HWS$yH)
hist(HWS$yG)

hist(HWS$yDiff[HWS$xS==5])
HWS$League[HWS$xL==2]

#==== prepare data list for MCMC ==========

N=length(HWS$yH)
yh= HWS$yH
yg = HWS$yG
x1 = HWS$xC
x2 = HWS$xL 
xc = as.integer(levels(x1))[x1]
xc
xl = as.integer(levels(x2))[x2]
xl
ydiff = HWS$yH - HWS$yG
ydiff  
nc =  length(unique(xc))
nl =  length(unique(x2))
nl
#=============================================================
dataList0 = list(yH = HWS$yH, yG = HWS$yG, N = length(HWS$yH))

dataList01 = list(hgdiff=ydiff, N = length(HWS$yH))

dl2 = list(y1 = yg, y2 = yh, xL=xc, xH = xl,  
                N = N, Nl = nc, Nh = nl)

dl11 = list(yH = yh, yG = yg, xC = xc, N = length(HWS$yH), Nc=nc)
dl12 = list(yH = yh, yG = yg, xC = xl, N = length(HWS$yH), Nc=nl)

dataList_3L = list(y1 = yh, y2 = yg, xL=xc, xH = xl, N = N, Nl = nc, Nh = nl) # 2019.4 3 level model data list
#============================================================
remove(Stan_Model_3L, stanfit_3L)
#========

stanfit_3L = sampling(object = Stan_Model_3L, data=dataList_3L, 
                 init=0.1, control=list(adapt_delta = 0.95),
                 chains=4, iter=888, warmup=444, thin=1)


fit11 = sampling(object = sm011, data=dl12, 
               init=0.1, control=list(adapt_delta = 0.95),
               chains=4, iter=888, warmup=444, thin=1)

class(stanfit_3L)

traceplot(stanfit_3L)
summary(stanfit_3L)

# access and cgange parameter names for display
pairs(stanfit_3L)
names(stanfit_3L)
# stan plot functions
#names(stanfit_3L)[127] #name of the first parameter
#names(stanfit_3L)[127] <- "Sport"

plot(stanfit_3L, plotfun="rhat")
plot(stanfit_3L, plotfun="trace", pars=c("beta_01"))
      
#============== plot sport level diff =======================================
index_club = 211
names(stanfit_3L)[index_club] #name of the League parameter
names(stanfit_3L)[index_club] <- "Soccer"






plot_sport <- plot(stanfit_3L, ci_level = 0.95, point_est ="mean", est_color = "#ffffff",
     
  show_outer_line = TRUE, outer_level = 0.99,
     
  pars=c("delta_top"),  # sport level effect
     
  show_density=TRUE, fill_color="#123489") +
  
    geom_vline(xintercept = 0, linetype=2) + 
  
    #xlab("shaded 95% CI and outline 99% CI")+ylab("") +
  
    scale_x_continuous(#name = label,
                     expand = c(0,0), # no expansion buffer 
      breaks = seq(-0.8, 1.6, 0.2), limits=c(-0.5, 1.1)) +
  
    scale_y_discrete("Soccer", labels= c("Soccer"="Sport")) +
    #scale_y_continuous(#name = label,
    #expand = c(0,0), # no expansion buffer 
    #breaks = seq(0.95, 1.8, 0.1), limits=c(0.95, 1.82)) +  

      theme_light(base_size=22)#theme_Posterior

p0 = plot_sport +
  #coord_flip()  + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()
                                  #axis.text.x = element_text(face = 'bold')
        )  
#================== plot league level impact diff ===========================
names(stanfit_3L)[521:525] <- pars.names <- c("La_Liga","Serie_A","Ligue_1","Bundesliga","EPL")

names(stanfit_3L)[521:525]

plot_leagues = plot(stanfit_3L, ci_level = 0.95, point_est ="mean", est_color = "#ffffff",
     
     show_outer_line = TRUE, outer_level = 0.99,
     
     pars=pars.names,  # sport level effect
     
     show_density=TRUE, fill_color="#123489") +
  
  geom_vline(xintercept = 0, linetype=3, size=1) + 
  
  #xlab("shaded 95% CI and outline 99% CI")+ylab("") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-0.8, 1.2, 0.2), limits=c(-0.5, 1.1)) +
  
  scale_y_discrete("Soccer", labels= c("Soccer"="Sport")) +  
  #scale_y_continuous(#name = label,
  #expand = c(0,0), # no expansion buffer 
  #breaks = seq(0.95, 1.8, 0.1), limits=c(0.95, 1.82)) +  
  
  theme_light(base_size = 22) #+ coord_flip()  

p1 = plot_leagues +  theme(axis.title.x = element_blank(), axis.title.y = element_blank() 
                      #axis.text.x = element_text(face = 'bold')
                      
                      )  
#================== Multiplot  ===========================
library(gridExtra)



grid.arrange(p11, p12, p13,p14, p15,
             layout_matrix = matrix(c(1,2,3,4,5), ncol=1, byrow=TRUE))

#========================================================================== 
#pars.la_liga = c("Sporting_Gijon", "Barcelona", "Real_Madrid", "Granada", "Atletico_Madrid", "Osasuna", "Leganes", "Sevilla",
#            "Deportivo_La_Coruña", "Villarreal", "Real_Betis", "Real_Sociedad", "Las_Palmas", "Celta_Vigo", "Athletic_Bilbao",
#            "Valencia", "	Espanyol", "	Eibar", "Alaves", "Malaga")   
     
#Serie_A.pars = c("Juventus", "AS_Roma", "Napoli", "Atalanta", "	Lazio", "AC_Milan", "Internazionale", "Fiorentina", "Torino", "Sampdoria",
#                 "Cagliari", "Sassuolo", "Udinese", "Chievo_Verona", "Bologna", "Genoa", "Crotone", "Empoli", "Palermo", "US_Pescara")

pars.Bundesliga = c("Bayern_Munich", "RB_Leipzig", "Borussia_Dortmund", "TSG_Hoffenheim", "FC_Cologne", "Hertha_Berlin", "SC_Freiburg",
                    "Werder_Bremen", "Borussia_Monchengladbach", "Schalke_04", "Eintracht_Frankfurt", "Bayer_Leverkusen", "FC_Augsburg",
                    "Hamburg_SV", "Mainz", "Vfl_Wolfsburg", "FC_Ingolstadt_04", "SV_Darmstadt_98")  
index_club = 616
names(stanfit_3L)[index_club] #name of the club parameter
names(stanfit_3L)[index_club] <- "Stoke_City"

pars.EPL = c("Chelsea", "Tottenham_Hotspur", "Manchester_City", "Liverpool", "Arsenal", "Manchester_United", "Everton", "Southampton",
             "AFC_Bournemouth", "West_Bromwich_Albion", "West_Ham_United", "Leicester_City", "Stoke_City")


par_names.la_liga <- names(stanfit_3L)[526:545]
par_names.Serie_a <- names(stanfit_3L)[546:565]
par_names.ligue_1 <- names(stanfit_3L)[566:585]
par_names.bundesliga <- names(stanfit_3L)[586:603]
par_names.epl <- names(stanfit_3L)[603:623]

plot_bundesliga = plot(stanfit_3L, ci_level = 0.95, point_est ="mean", est_color = "#ffffff", show_outer_line = FALSE, outer_level = 0.99,
     
     pars=par_names.bundesliga,  show_density=FALSE, fill_color="#123489") +
  
  geom_vline(xintercept = 0, linetype=2) + 
  
  xlab("shaded 95% CI and outline 99% CI")+ylab("") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1.6, 1.6, 0.4), limits=c(-1.2, 1.2)) +

  scale_y_discrete("Soccer", labels= c("Soccer"="Sport")) +  
    
  #scale_y_continuous(#name = label,
  #expand = c(0,0), # no expansion buffer 
  #breaks = seq(0.95, 1.8, 0.1), limits=c(0.95, 1.82)) +  
  
  theme_light(base_size = 22)#theme_Posterior  

p14 = plot_bundesliga + coord_flip() + theme(axis.title.x = element_blank(), axis.title.y = element_blank() 
                           #axis.text.x = element_text(face = 'bold')
                           
)  

p14
#====================== extract data from stanfit object ====================
get_posterior_mean(fit0)

post <- extract(fit11)

post$ydiff_rep

tdiff = t(post$ydiff_rep)

dim(tdiff)


#nrow(post$delta_h)
#shinystan for plots and analysis

launch_shinystan(fit11)
  #======== convert stan format to coda format ========== 

#================= setting up STAN =================
Sys.which("g++")
Sys.getenv('PATH')
system('g++ -v')
system('where make')
Sys.which('g++')
Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

pkgbuild::has_build_tools(debug = TRUE)

 


M <- file.path(Sys.getenv("HOME"), ".R", ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
file.edit(M)


fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 )

remove.packages("rstan")
remove.packages('Rcpp')
#====================================================