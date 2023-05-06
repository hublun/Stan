library(dplyr)
library(rstan)
library(bayesplot)
library(Metrics)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#========================================================================
data_list <- readRDS(file = paste0(getwd(),'/Documents/Github',
                          '/Stan/Factorization/factor_data_list.rds', sep = ""))

data_list$N
data_list$P
data_list$D
data_list$Y

#------------------------------------------------------------------------
remove(model)
model = stan_model(paste0(getwd(),'/Documents/Github' ,
                          '/Stan/RStan/Models/factor_mvn.stan'))
#------------------------------------------------------------------------
init_func <- function(D, P) {
  init.values <- list(
    list(L_t = rep(0, 24) + runif(1, -.1, .1)),
    list(L_d = rep(.5, D) + runif(1, -.1, .1)),
    list(psi = rep(.2, P) + runif(1,-.1,.1)),
    list(sigma_psi = 0.15 + runif(1,-.1,.1)),
    list(mu_psi = 0.2 + runif(1, -.1, .1)),
    list(sigma_lt = 0.5 + runif(1, -.1,.1)),
    list(mu_lt = 0.0 + runif(1, -.1,.1))
  )
  
  return(init.values);
}

value <- init_func(data_list$D, data_list$P)
value
#--------------------------------
remove(fit)
fit <- sampling(object = model,
                data = data_list,
                init = init_func(data_list$D, data_list$P), #"random",
                control = list(adapt_delta = 0.95),
                chains = 4,
                iter = 888,
                warmup = 555,
                thin = 1,
                verbose = TRUE)
#=================================================

s <- summary(fit)
dfs <- data.frame(s$summary)

result <- filter(dfs, Rhat > 5.5)
row.names(result)

names(fit)
traceplot(fit, pars=c("L_d"))
#-------------------------------------------------
theme_Posterior = theme(
  axis.line.x = element_line(arrow=arrow(length=unit(0.05, "cm")), lineend = "butt"),
  panel.background = element_rect(fill="transparent"), 
  panel.border = element_rect(fill="transparent"),
  plot.background = element_rect(fill = "transparent"),
  #panel.spacing.y = unit(1,"lines"),
  plot.margin = unit(c(1,1,1,1), "cm"), 
  #
  axis.title.y = element_blank(), 
  axis.text.y = element_text(color = "grey20", size = 8, angle = 0, hjust = 1, vjust = 0, face = "plain"),
  axis.text.x = element_text(color="grey20",hjust=1, vjust=1, size=8, angle=50, face="plain"), 
  axis.ticks.y = element_blank()
)
#---------------------------
plot(fit,
     pars = names(fit)[c(1:24)],
     show_density = FALSE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior

plot(fit,
     pars = c('L_d'),
     show_density = FALSE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior

plot(fit,
     pars = c("mse"),
     show_density = TRUE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  #geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior

plot(fit,
     pars = c("mu_psi"),
     show_density = TRUE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  #geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior
#----------------------------

color_scheme_set('viridisA')
bayesplot_theme_set(theme_classic())

draws = extract(fit)
y = data_list$Y
str(y)
yrep = draws$yp
str(yrep)

y =as.vector(Reshape(y, 1))
yrep = Reshape(yrep, 1332)

ncol(yrep)
length(y)

ppc_dens_overlay(y=y, yrep = yrep)
ppc_ribbon(y[1:1000], yrep[,1:1000], prob_outer = 0.95, prob = 0.5, alpha = 0.4)
ppc_intervals(y[1:10], yrep[,1:10], linewidth = 1)
#========= MSE ===========
mses = c()
for (i in 1:dim(yrep)[1]){
    mses<-c(mses, mse(y, yrep[i,]))
  }
rmse = sqrt(mean(mses))
rmse
#===========================================