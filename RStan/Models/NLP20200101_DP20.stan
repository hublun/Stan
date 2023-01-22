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
  int TU; // Total hits by user
  matrix[P, V] lambda; // page - topic loadings
  int y[P];  // hits 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  simplex[V] pta;
}

transformed parameters{
  vector[P] phi;
  for (i in 1:P){
    phi[i] = dot_product(lambda[i,],pta);
  }
  phi=phi/sum(phi);
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    pta ~ dirichlet(rep_vector(0.2, V));
    y ~ multinomial(phi);
}

generated quantities{
  int yp[P];
  vector[P] se;
  real mse;
  yp = multinomial_rng(phi, TU);
  for (i in 1:P){
    se[i] = square(yp[i]-y[i]);
  }
  mse = mean(se);
}

