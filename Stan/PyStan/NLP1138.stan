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
  int<lower=0> U; // number of pages
  int xu[N];
  int y[N];   // page clicks
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real <lower=0> lambda[U]; // click rate for each page
  //real<lower=0> sigma;
}

transformed parameters{
  vector[N] theta;
  for (i in 1:N){
    theta[i] = lambda[xu[i]];
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  lambda ~ cauchy(0, 50);
  y ~ poisson(theta);
}

//==============================
generated quantities {
  vector[N] yp;
  real mse;
  for (i in 1:N){
    yp[i] = poisson_rng(theta[i])-y[i];    
  }
  mse = dot_self(yp)/N;
}
