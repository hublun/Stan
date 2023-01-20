library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

data_list <- readRDS(file = paste0(getwd(),'/Documents/Github' ,
                          '/Stan/Factorization/data_list.rds', sep = ""))
#-----------------------------------
remove(model)
model = stan_model(paste0(getwd(),'/Documents/Github' ,'/Stan/Stan/Models/factorization.stan'))
#-----------------------------------
remove(fit)
fit <- sampling(object = model,
                data = data_list,
                init = "random",
                control = list(adapt_delta = 0.95),
                chains = 4,
                iter = 500,
                warmup = 300,
                thin = 2,
                verbose = TRUE)
#=================================================
summary(fit)
names(fit)
traceplot(fit, pars=c("gammas[1,1]"))
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
     pars = names(fit)[c(1:20)],
     show_density = FALSE,
     fill_color = "#998811",
     est_color = "#ffffff",
     ci_level = 0.9, outer_level=0.95) +
  geom_vline(xintercept = 0, linetype = 3, linewidth = 0.5) + 
  theme_Posterior
