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
  int xh[G]; // hosting team identities
  int xv[G]; // visitor identities
  //==========================
  int yh[G]; // host goals
  int yv[G]; // guest goals
  //==========================
  int<lower=1, upper=2> COV[G]; // indicating games played pre or after COVID breakout
}

// The parameters accepted by the model. 
parameters {

  real mu_O;
  real mu_D;
  real <lower=0> sigma_O;
  real <lower=0> sigma_D; 
  
  vector[2] delta; // HFA 0 regular and 1 for under pandemic influence
  vector[T] lambda_O_t; // team offensive capabilities
  vector[T] lambda_D_t; // team defensive capabilities
}

transformed parameters{
//   real hfa_d;
vector<lower=0>[G] theta_V;
vector<lower=0>[G] theta_H;
  
  for (i in 1:G){
    theta_V[i] = exp(lambda_O_t[xv[i]] - lambda_D_t[xh[i]]);
    theta_H[i] = exp(delta[COV[i]] + lambda_O_t[xh[i]] - lambda_D_t[xv[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

// priors
mu_O ~ normal(0, 0.1);
mu_D ~ normal(0, 0.1);
delta ~ normal(0, 0.1);
sigma_O ~ gamma(0.05, 0.05);
sigma_D ~ gamma(0.05, 0.05);
// logNormal part of the model
lambda_O_t ~ normal(mu_O, sigma_O);
lambda_D_t ~ normal(mu_D, sigma_D);

yh ~ poisson(theta_H);
yv ~ poisson(theta_V);

}
//==============================
generated quantities {
  int yp_H[G];
  int yp_V[G];
  //for (i in 1:N){
  yp_H = poisson_rng(theta_H); 
  yp_V = poisson_rng(theta_V);    
  //}
}
