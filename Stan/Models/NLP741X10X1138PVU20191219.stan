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

transformed data {}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  matrix[P, V] alpha;
  matrix[U, V] beta;
}

transformed parameters{
  matrix[P, V] pha;
  vector[N] eta;
  real<lower=0> theta[N];
  
  pha = m .* alpha;
  
  for (i in 1:N){
    eta[i] = dot_product(row(pha, xp[i]), row(beta, xu[i]));
    theta[i] = exp(eta[i]);
  }
        
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  for (i in 1:P){
    //alpha[i,]
    row(alpha, i)~normal(MUP[i], sigma_P[i]);
  }
  for (i in 1:U){
    //beta[i,]
    row(beta,i)~normal(MUU[i], sigma_U[i]);
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
