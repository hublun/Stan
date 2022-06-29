//
// This Stan program defines a simple model, with two
// vectors of scores 'y' modeled as poissonly distributed
// with rate 'theta', which further follows logNormal distribution
//governed by mu and sigma.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a few vectors 'y' of length 'N'.
data {
  int<lower=1> G; // # of games played during the 2020 Bundesliga season
  int<lower=2> T; // number of teams in Bundesliga
  array[G] int xh; // hosting team identities
  array[G] int xv; // visitor identities
  //==========================
  array[G] int yh; // host goals
  array[G] int yg; // guest goals
  //==========================
  array<lower=1, upper=2>[G] int COV; // indicating games played pre or after COVID breakout
}

// The parameters accepted by the model. 
parameters {

  //real muo;
  //real mud;
  //real <lower=0> sigmao;
  //real <lower=0> sigmal;
  //real <lower=0> sigmac;  
  
//   real hfas[2];
//   vector[nt] off; // team offensive capabilities
//   vector[nt] dff; // team defensive capabilities
}

transformed parameters{
//   real hfa_d;
//   real<lower=0> theta_v[ng];
//   real<lower=0> theta_h[ng];
  
  
//   hfa_d = hfas[1]-hfas[2];
  
//   for (i in 1:ng){
//     theta_v[i] = exp(off[vt[i]]+dff[ht[i]]);
//     theta_h[i] = exp(hfas[cv[i]] + off[ht[i]]+dff[vt[i]]);
//   }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  //muo ~ cauchy(0, 2);
  //mud ~ cauchy(0, 2);
  //sigmao ~ gamma(1, 1);
//   off ~ normal(0, 1);
//   dff ~ normal(0, 1);
//   //hfas ~ normal(0,1);
//   //hfal ~ normal(0,1);
//   hfas ~ normal(0,1);

//   vg ~ poisson(theta_v);
//   hg ~ poisson(theta_h);
}
//==============================
generated quantities {
  //int yp[N];
  /*for (i in 1:N){
    yp[i] = poisson_rng(theta_h[i]) - poisson_rng(theta_g[i]);    
  }*/
}
