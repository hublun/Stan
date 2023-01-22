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
  int<lower=0> ng; // # of observations
  int<lower=0> nt; //
  int ht[ng];
  int vt[ng];
  //==========================
  int hg[ng]; // host goals
  int vg[ng]; // guest goals
  //==========================
  int cv[ng]; // indicating games played pre or after COVID breakout
  
  //int L; // number of leagues
  //int xl[N]; // league index
  //int C; // number of clubs
  //int xc[N]; // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {

  //real muo;
  //real mud;
  //real <lower=0> sigmao;
  //real <lower=0> sigmal;
  //real <lower=0> sigmac;  
  
  real hfas[2];
  //vector[L] hfal; // league specific hfa
  //vector[C] hfac; // team specific hfa
  vector[nt] off; // team offensive capabilities
  vector[nt] dff; // team defensive capabilities
}

transformed parameters{
  real hfa_d;
  real<lower=0> theta_v[ng];
  real<lower=0> theta_h[ng];
  
  
  hfa_d = hfas[1]-hfas[2];
  
  for (i in 1:ng){
    theta_v[i] = exp(off[vt[i]]+dff[ht[i]]);
    theta_h[i] = exp(hfas[cv[i]] + off[ht[i]]+dff[vt[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  //muo ~ cauchy(0, 2);
  //mud ~ cauchy(0, 2);
  //sigmao ~ gamma(1, 1);
  off ~ normal(0, 1);
  dff ~ normal(0, 1);
  //hfas ~ normal(0,1);
  //hfal ~ normal(0,1);
  hfas ~ normal(0,1);

  vg ~ poisson(theta_v);
  hg ~ poisson(theta_h);
}
//==============================
generated quantities {
  //int yp[N];
  /*for (i in 1:N){
    yp[i] = poisson_rng(theta_h[i]) - poisson_rng(theta_g[i]);    
  }*/
}
