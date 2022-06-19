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
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu_G;
  real mu_H;
  //tau_G;
  //tau_H;
  //real tau;
  //real<lower=0> sigma;
}

transformed parameters{
  real<lower=0> theta_g;
  real<lower=0> theta_h;
  real delta;
  theta_g = exp(mu_G);
  theta_h = exp(mu_H);
  delta = mu_H - mu_G;
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu_G ~ normal(0, 0.1);
  mu_H ~ normal(0, 0.1);
  yg ~ poisson(theta_g);
  yh ~ poisson(theta_h);
}

//==============================
generated quantities {
  int yp[N];
  for (i in 1:N){
    yp[i] = poisson_rng(theta_h)-poisson_rng(theta_g);    
  }
}
