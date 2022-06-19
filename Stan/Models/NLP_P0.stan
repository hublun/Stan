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
  int xp[N];
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
  real mu;
  real tau;
  vector[P] mu_p; // click rate for each page

}

transformed parameters{
  vector[N] theta;
  for (i in 1:N) {
    theta[i] = exp(mu_p[xp[i]]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

model {
  //alpha ~ uniform(0, 1.0);
  mu ~normal(0, 0.1);
  tau ~gamma(0.1,0.1);
  mu_p ~ normal(mu, tau);
  y ~ poisson(theta);
}

//==============================
generated quantities {
  int yp[N];
  for (i in 1:N){
    yp[i] = poisson_rng(theta[i]);    
  }
}

