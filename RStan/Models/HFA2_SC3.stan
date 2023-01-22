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
  int C; // number of clubs
  int xc[N]; // club index
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {

  real muh_att;
  real <lower=0> sigmah_att;
  
  real mug_att;
  real <lower=0> sigmag_att;  
  

  vector[C] att_g;
  vector[C] att_h;

}

transformed parameters{
  real<lower=0> theta_g[N];
  real<lower=0> theta_h[N];
  real delta;
  
  vector[C] delta_c;
  delta = muh_att - mug_att;
  delta_c = att_h - att_g;
  
  for (i in 1:N){
    theta_g[i] = exp(att_g[xc[i]]);
    theta_h[i] = exp(att_h[xc[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  mug_att ~ cauchy(0, 5);
  sigmag_att ~ gamma(1, 1);
  muh_att ~ cauchy(0, 5);
  sigmah_att ~ gamma(1, 1);
  
  att_g ~ normal(mug_att, sigmag_att);
  att_h ~ normal(muh_att, sigmah_att);

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
