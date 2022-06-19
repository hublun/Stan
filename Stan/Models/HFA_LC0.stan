//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // # of observations
  int yg[N]; // MGG
  int yh[N]; // home MHG
  int L; // numer of leagues
  int xl[N];// league index
  int C; // number of clubs
  int xc[N]; // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real muS_G;
  real muS_H;
  
  real muL_G;
  real muL_H;
  real <lower=0> tauL_G;
  real <lower=0> tauL_H;
  
  real muC_G;
  real muC_H;
  real <lower=0> tauC_G;
  real <lower=0> tauC_H;

  
  vector[L] mul_g;
  vector[L] mul_h; 
  
  vector[C] muc_g;
  vector[C] muc_h; 
}

transformed parameters{
  real<lower=0> theta_g[N];
  real<lower=0> theta_h[N];
  real delta_S;
  vector[L] delta_L;
  vector[C] delta_C;
  
  for (i in 1:N){
    theta_g[i] = exp(muS_G + mul_g[xl[i]] + muc_g[xc[i]]);
    theta_h[i] = exp(muS_H + mul_h[xl[i]] + muc_h[xc[i]]);
  }
  
  delta_S = muS_H - muS_G;
  delta_L = mul_h - mul_g;
  delta_C = muc_h - muc_g;
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  muS_G ~ normal(0, 1);
  muS_H ~ normal(0, 1);
  
  muL_G ~ normal(0, 1);
  muL_H ~ normal(0, 1);
  tauL_G ~ gamma(1, 1);
  tauL_H ~ gamma(1, 1);

  muC_G ~ normal(0, 1);
  muC_H ~ normal(0, 1);
  tauC_G ~ gamma(1, 1);
  tauC_H ~ gamma(1, 1);  
  
  mul_g ~ normal(muL_G, tauL_G);
  mul_h ~ normal(muL_H, tauL_H);
  muc_g ~ normal(muC_G, tauC_G);
  muc_h ~ normal(muC_H, tauC_H);
  
  yg ~ poisson(theta_g);
  yh ~ poisson(theta_h);
}

//==============================
generated quantities {
  int yp[N];
  for (i in 1:N){
    yp[i] = poisson_rng(theta_h[i]) - poisson_rng(theta_g[i]);    
  }
}
