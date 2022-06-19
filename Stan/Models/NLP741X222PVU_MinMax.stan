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
  vector[P] MUP;
  vector[U] MUU;
  vector[P] sigma_P;
  vector[U] sigma_U;
  matrix[P, V] m; // row_vectors of document embeddings
  int y[N];  // hits 
}

transformed data {
  matrix[N,2] eta;
  for (i in 1:N){
    eta[i,1] = MUP[xp[i]] >= MUU[xu[i]]?MUU[xu[i]] : MUP[xp[i]];
    eta[i,2] = sigma_P[xp[i]] <= sigma_U[xu[i]]?sigma_P[xp[i]]: sigma_U[xu[i]];
  }
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real beta_P[P]; // page rates
  //real beta_U[U]; // user rates 
  real beta[N];
}

transformed parameters{

  real<lower=0> theta[N];
  theta = exp(beta);
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {

  for (i in 1:N){
   beta[i] ~ normal(eta[i,1], eta[i,2]); 
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
