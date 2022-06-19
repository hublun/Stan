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
  //int C; // number of clubs
  //int xc[N] // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real muS_G;
  real muS_H;
  
  //real muL_G;
  //real muL_H;
  //real <lower=0> tauL_G;
  //real <lower=0> tauL_H;
  
  vector[L] mul_g;
  vector[L] mul_h; 
  
}

transformed parameters{
  real<lower=0> theta_g[N];
  real<lower=0> theta_h[N];
  real delta_S;
  vector[L] delta_L;
  
  for (i in 1:N){
    theta_g[i] = exp(muS_G + mul_g[xl[i]] -mean(mul_g));
    theta_h[i] = exp(muS_H + mul_h[xl[i]]-mean(mul_h));
  }
  
  delta_S = muS_H - muS_G;
  delta_L = mul_h - mul_g;
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  muS_G ~ normal(0, 1);
  muS_H ~ normal(0, 1);
  //muL_G ~ normal(0, 1);
  //muL_H ~ normal(0, 1);
  //tauL_G ~ gamma(1, 1);
  //tauL_H ~ gamma(1, 1);
  
  mul_g ~ normal(0, 1);
  mul_h ~ normal(0, 1);  
  
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
