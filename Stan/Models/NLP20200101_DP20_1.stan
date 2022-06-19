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
  int TU[U]; // Total hits by user
  matrix[P, V] lambda; // page - topic loadings
  int y[P,U];  // hits 
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  simplex[V] pta[U];
}

transformed parameters{
  matrix[P,U] phi;
  for (i in 1:P){
    for (j in 1:U){
      phi[i,j] = dot_product(lambda[i,],pta[j]);
    }
  }
  for (j in 1:U){
    phi[,j]=phi[,j]/sum(phi[,j]);
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
    for (i in 1:U){
     pta[i] ~ dirichlet(rep_vector(0.1, V));
     y[,i] ~ multinomial(phi[,i]);
    }
}

generated quantities{
  int yp[P,U];
  matrix[P,U] se;
  real MSE[U];
  for (i in 1:U){
   yp[,i] = multinomial_rng(phi[,i], TU[i]);
  }
  for (i in 1:P){
    for (j in 1:U){
      se[i,j] = square(yp[i,j]-y[i,j]);
    }
  }
  for (i in 1:U){
   MSE[i] = mean(se[,i]);
  }
}

