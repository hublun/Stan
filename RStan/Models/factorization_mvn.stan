data {
    /* ... declarations ... */
    int<lower=0> N; // number of group 1 observations
    int<lower=0> J; // number of group 2 observations
    int<lower=0> K; // numner of latent dimensions
    int X[(N*J), 2]; // covariate matrix
    vector[(N*J)] y;

    //real<lower=0> beta_sigma;
    //real<lower=0> y_sigma;
}

parameters {
    /* ... declarations ... */
    real<lower=0> beta_sigma;
    real<lower=0> factor_sigma;
    real<lower=0> y_sigma;
    vector[N] g1_betas; 
    vector[J] g2_betas;
    //matrix[N, K] gammas; // group 1 factor
    matrix[J, K] deltas; // group 2 factors
    //--------------- mvn --------------
    real<lower=0> gamma_sigma_prior;
    real<lower=0> gamma_omega_prior;
    matrix[N, K] gamma_mu;
    vector<lower=0>[K] gamma_sigma;
    cholesky_factor_corr[K] gamma_omega;
    //matrix[K, N] gamma_a;
    
    //---------------------------------
}

transformed parameters {
    /* ... declarations ... statements ... */
    matrix[N, K] gammas;

    vector[N*J] linear_predictors;
    
    
    for (i in 1: N*J){
        linear_predictors[i] = g1_betas[X[i,1]] + g2_betas[X[i,2]] + (gammas[X[i,1]] * deltas[X[i,2]]');
    }
}

model {
   /* ... declarations ... statements ... */
    beta_sigma ~ lognormal(0, 1);
    factor_sigma ~ lognormal(0, 0.1);
    y_sigma ~ gamma(1, 0.1);

    g1_betas ~ normal(0, beta_sigma);
    g2_betas ~ normal(0, beta_sigma);
    //-------------------------------
    gamma_sigma_prior ~ gamma(1, 0.1);
    gamma_sigma ~ normal(0, gamma_sigma_prior);
    gamma_omega_prior ~ gamma(1, 0.1);
    gamma_omega ~ lkj_corr_cholesky(2);    

    //to_vector(gamma_a) ~ std_normal();
    //-------------
    for (n in 1:N){
      gamma_mu[n,] ~ normal(0,1);
      gammas[n,] ~ multi_normal_cholesky(gamma_mu[n,], gamma_omega);
    }
    //-------------------------------
    for (j in 1:J){
        deltas[j,] ~ normal(rep_vector(0, K), factor_sigma);
    }

    y ~ normal(linear_predictors, y_sigma);
}

generated quantities {
    real y_pred[N*J];
    real mse;
    y_pred = normal_rng(linear_predictors, y_sigma);
    mse =squared_distance(to_vector(y_pred), y) / (N*J);
}
