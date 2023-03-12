library(dplyr)
library(rstan)
library(bayesplot)
library(Metrics)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#========================================================================
data_list <- readRDS(file = paste0(getwd(),#'/Documents/Github',
                          '/Stan/Factorization/factor_data_list.rds', sep = ""))
#------------------------------------------------------------------------
remove(model)
model = stan_model(paste0(getwd(),#'/Documents/Github' ,
                          '/Stan/RStan/Models/factor_mvn.stan'))
#------------------------------------------------------------------------
remove(fit)
fit <- sampling(object = model,
                data = data_list,
                init = "random",
                control = list(adapt_delta = 0.95),
                chains = 4,
                iter = 888,
                warmup = 555,
                thin = 1,
                verbose = TRUE)
#=================================================

s <- summary(fit)
dfs <- data.frame(s$summary)

result <- filter(dfs, Rhat > 1.1)
row.names(result)

names(fit)
traceplot(fit, pars=c("delta_mu"))
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
     pars = names(fit)[c(1:3)],
     show_density = FALSE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior

plot(fit,
     pars = c('delta_sigma'),
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
     pars = c("delta_omega_prior", 'gamma_omega_prior'),
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
y = data_list$y
yrep = draws$y_pred
dim(yrep)

ppc_dens_overlay(y=y, yrep = yrep)
ppc_ribbon(y[1:20], yrep[,1:20], prob_outer = 0.95, prob = 0.5, alpha = 0.4)
ppc_intervals(y[1:2], yrep[,1:2], linewidth = 1)
#========= MSE ===========
mses = c()
for (i in 1:dim(yrep)[1]){
    mses<-c(mses, mse(data_list$y, yrep[i,]))
  }
rmse = sqrt(mean(mses))
rmse
#===========================================