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
  int<lower=0> N; // number of data points
  int<lower=0> P; // number of pages
  int<lower=1> U; // number of usrs
  int xp[N];  // index page
  int xu[N]; // index of users
  int y[N];   // page clicks
  //vector[P] mxp; // max hits per page
  //real shape; // gamma shape
  //real rate;  // gamma rate
}

/*transformed data{
  vector[N] yt;
  for (i in 1:N){
    yt[i] = y[i]/mxp[xp[i]];
  }
}*/

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real muP; // grand mean for mu_p
  real muU;
  real tau_p;
  real tau_u;
  //real alpha;
  vector[P] mu_p; // click rate for each page
  vector[U] mu_u; // click rate per usr
  //real intercept;
}

transformed parameters{
  real<lower=0> theta[N];
  for (i in 1:N) {
    theta[i]= exp(mu_p[xp[i]] + mu_u[xu[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

model {
// priots for mean and scale
  muP ~normal(0, 0.01);
  muU ~ normal(0, 0.01);
  tau_p ~gamma(0.1,0.1);
  tau_u ~ gamma(0.01, 0.01);
  
  mu_p ~ normal(muP, tau_p);
  mu_u ~ normal(muU, tau_u);

  y ~ poisson(theta);
}

//==============================
generated quantities {
  int yp[N];
  //real mse;
  yp = poisson_rng(theta);    
  //mse = dot_self(yp)/N;
}

