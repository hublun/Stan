data {
    /* ... declarations ... */
    int<lower=0> N; // number of group 1 observations
    int<lower=0> J; // number of group 2 observations
    int<lower=0> K; // numner of latent dimensions
    int X[(N*J), 2]; // covariate matrix
    vector[(N*J)] y;
    real<lower=0> beta_sigma;
    real<lower=0> y_sigma;
}

parameters {
    /* ... declarations ... */
    vector[N] g1_betas; 
    vector[J] g2_betas;
    matrix[N, K] gammas; // group 1 factor
    matrix[J, K] deltas; // group 2 factors
}

transformed parameters {
    /* ... declarations ... statements ... */
    vector[N*J] linear_predictors;
    for (i in 1: N*J){
        linear_predictors[i] = g1_betas[X[i,1]] + g2_betas[X[i,2]] + (gammas[X[i,1]] * deltas[X[i,2]]');
    }
}

model {
   /* ... declarations ... statements ... */
    g1_betas ~ normal(0, beta_sigma);
    g2_betas ~ normal(0, beta_sigma);

    for (n in 1:N){
        gammas[n,] ~  normal(rep_vector(0, K), 1);
    }
    for (j in 1:J){
        deltas[j,] ~ normal(rep_vector(0, K), 1);
    }

    y ~ normal(linear_predictors, y_sigma);
}

generated quantities {
    real y_pred[N*J];
    real mse;
    y_pred = normal_rng(linear_predictors, y_sigma);
    mse =squared_distance(to_vector(y_pred), y) / (N*J);
}