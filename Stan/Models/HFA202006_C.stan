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
  int L; // number of leagues
  int xl[N]; // league index
  int C; // number of clubs
  int xc[N]; // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {

  real muo;
  //real <lower=0> sigmao;
  //real <lower=0> sigmal;
  //real <lower=0> sigmac;  
  
  //real hfas;
  //vector[L] hfal; // league specific hfa
  vector[C] hfac; // team specific hfa
  vector[C] off; // team offensive capabilities

}

transformed parameters{
  real<lower=0> theta_g[N];
  real<lower=0> theta_h[N];

  
  for (i in 1:N){
    theta_g[i] = exp(off[xc[i]]);
    theta_h[i] = exp(hfac[xc[i]] + off[xc[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  muo ~ cauchy(0, 5);
  //sigmao ~ gamma(1, 1);
  off ~ normal(muo, 1);
  //hfas ~ normal(0,1);
  //hfal ~ normal(0,1);
  hfac ~ normal(0,1);

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
