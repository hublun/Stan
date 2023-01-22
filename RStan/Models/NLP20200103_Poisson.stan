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
  int<lower=0> U;
  int xp[N];
  int xu[N];// page index
  int TU;
  int Up; // index of hits to be predicted
  matrix[P, V] lambda; // page - topic loadings
  int y[P];  // hits 
}

transformed data{
  int ym[P];
  ym= y;
  ym[Up]=0;
}
// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  simplex[V] theta;
}

transformed parameters{
  vector[P] rate;

  for (i in 1:P){
      rate[i] = dot_product(lambda[i,], theta);
  }
  rate = rate/sum(rate);
  

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  theta ~ dirichlet(rep_vector(0.1, V));
  y ~ multinomial(rate);
}

generated quantities{
  real mse;
  int yp[P];
  vector[P] se;
  yp = multinomial_rng(rate, TU);
  for (i in 1:P){
    se[i] = square(yp[i] - y[i]);
  }
  mse=mean(se);
}
