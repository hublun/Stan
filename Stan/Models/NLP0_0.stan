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
  int<lower=0> N;
  int y[N];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  //real tau;
  //real<lower=0> sigma;
}

transformed parameters{
  real<lower=0> theta;
  theta = exp(mu);
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu ~ normal(0, 0.1);
  y ~ poisson(theta);
}

//==============================
generated quantities {
  int yp[N];
  for (i in 1:N){
    yp[i] = poisson_rng(theta);    
  }
}
