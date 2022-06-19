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
  int<lower=0> N; // number ofobservations
  int<lower=0> P; // number of pages
  int<lower=0> V; // vector dimension 
  int<lower=0> U; // total number of users
  int xp[N]; // page index numbers
  int xu[N]; // user index
  matrix[P, V] m; // row_vectors of document embeddings
  int y[N];  // hits 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  matrix[V, U] mu;
  real MU;
  real<lower=0> sigma;
}

transformed parameters{
  matrix[P, U] delta; 
  real<lower=0> theta[N];
  vector[P] theta_P;
  vector[U] theta_U;
  
  delta = m*mu;
  for (i in 1:P){
    theta_P[i] = sum(delta[i,]);
  }
  
  for (i in 1:U){
    theta_U[i] = sum(delta[,i]);
  }  
  
  
  for (i in 1:N){
    theta[i] = exp(theta_P[xp[i]] + theta_U[xu[i]]);    
  }
  
  
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  MU ~ normal(0,1);
  sigma ~ gamma(1,1);
  for(i in 1:V){
    for(j in 1:U){
      mu[i,j] ~ normal(MU, sigma);   
    }
  }
  
  y ~ poisson(theta);
}

//==============================
generated quantities {
  int yp[N];
  //for (i in 1:N){
    yp = poisson_rng(theta);    
  //}
}
