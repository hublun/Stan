library(MASS)
library(Matrix)
library(trialr)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(111)
N <- 100
group_1 <- paste0('i', 1:N)

J <- 20
group_2 <- paste0('j', 1:J)

K <- 5

predictors <- expand.grid(group_1=group_1, group_2=group_2)
X_mat <- sparse.model.matrix(~ factor(group_1) + factor(group_2) -1, data=predictors)
predictors_as_numeric <- cbind(as.numeric(factor(predictors[,1])), as.numeric(factor(predictors[,2])))
betas <- matrix(rnorm(n=ncol(X_mat), 0,2))
linear_predictors = X_mat %*% betas  # y

gamma_omega <- rlkjcorr(n = 1, K = K, eta = 0.9)#gamma_omega_prior)
delta_omega <- rlkjcorr(n=1, K=K, eta=0.5)

gammas <- mvrnorm(n=N, mu=rep(0,K), Sigma = gamma_omega)
deltas <- mvrnorm(n=J, mu=rep(0,K), Sigma = delta_omega)

factor_terms = matrix(NA, nrow=nrow(linear_predictors), ncol=1)

for (i in 1:nrow(predictors)){
    g1 <- as.character(predictors[i,1])
    g1 <- as.numeric(substring(g1, 2, nchar(g1)))

    g2 <- as.character(predictors[i,2])
    g2 <- as.numeric(substring(g2, 2, nchar(g2)))

    #print(paste0(g1, g2))
    factor_terms[i, ] =  matrix(gammas[g1,], nrow=1) %*% matrix(deltas[g2,], ncol=1)   
}



y <- linear_predictors + factor_terms + rnorm(n=nrow(linear_predictors), 0, 0.1)

data_list = list(
  N = N,
  J = J,
  K = K,
  X = predictors_as_numeric,
  y = y[,1],
  beta_sigma = 0.05,
  y_sigma = 0.02
)

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

