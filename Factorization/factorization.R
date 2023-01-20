library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

data_list <- readRDS(file = paste0(getwd(),'/Documents/Github' ,
                          '/Stan/Factorization/data_list.rds', sep = ""))

model = stan_model(paste0(getwd(),'/Documents/Github' ,'/Stan/Stan/Models/factorization.stan'))
remove(fit)
fit <- sampling(object = model,
                data = data_list,
                init = "random",
                control = list(adapt_delta = 0.95),
                chains = 4,
                iter = 1000,
                warmup = 500,
                thin = 1,
                verbose = TRUE)

summary(fit)
names(fit)
traceplot(fit, pars=c("gammas"))

