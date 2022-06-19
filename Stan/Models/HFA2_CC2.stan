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
  //int L; // numer of leagues
  //int xl[N];// league index
  int C; // number of clubs
  int xc[N]; // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[C] hfa;
  real muh;
  real <lower=0> tauh;
  real mu_att;
  real <lower=0> tau_att;
  vector[C] att;

}

transformed parameters{
  real<lower=0> theta_g[N];
  real<lower=0> theta_h[N];

  
  for (i in 1:N){
    theta_g[i] = exp(att[xc[i]]);
    theta_h[i] = exp(hfa[xc[i]] + att[xc[i]]);
  }
  
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  muh ~ cauchy(0, 1);
  tauh ~ cauchy(0, 5);
  mu_att ~ cauchy(0, 1);
  tau_att ~ cauchy(0, 5);

  hfa ~ normal(muh, tauh);
  att ~ normal(mu_att, tau_att);

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
