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
  simplex[V] theta;
}

transformed parameters{
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  vector[V] log_theta = log(theta);
  vector[V] lps = log_theta;
  for (i in 1:V){
    lps[i] += multinomial_lpmf(y|lambda[,i]);
  target += log_sum_exp(lps);
  }
}

generated quantities{
  int yp[P, V];
  vector[P] se;
  real mse;
  for (i in 1:V){
     yp[,i] = multinomial_rng(lambda[,i], TU);
  }
  for (i in 1:P){
    real row_s = 0;
    for(j in 1:V){
      row_s += theta[j]*yp[i,j];
    }
    se[i] = square(row_s-y[i]);
  }
  mse=mean(se);
}
