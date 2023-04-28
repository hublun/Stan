data {
    /* ... declarations ... */
    int<lower = 1> N; // number observations 
    int<lower = 1> P; // number of dimensions of y
    int<lower = 1> D; // numner of latent dimensions
    matrix[N, P] Y;   // data
}

transformed data {
    int<lower=1> M;
    vector[P] mu;
    M = D*(P-D) + D*(D-1)/2; // number of non-zero loadings
    mu = rep_vector(0.0, P);
}

parameters {
    /* ... declarations ... */
    vector[M] L_t; // lower diagonal elements of L_t
    vector<lower = 0>[D] L_d; // lower diagonal elements of L-d
    vector<lower=0>[P] psi; // vector of variance
    real<lower=0> mu_psi;
    real<lower=0> sigma_psi;
    real mu_lt;
    real<lower=0> sigma_lt;
}

transformed parameters {
  /* ... declarations ... statements ... */
  cholesky_factor_cov[P,D] L; // lower triangular factor loadings Matrix
  cov_matrix[P] Q; // Covariance mat
  //---------
  {
    int idx1;
    int idx2;
    real zero;
    zero = 0;
    for (i in 1:P){
      for (j in (i+1):D){
        idx1 = idx1 +1;
        L[i,j] = zero;
      }
    }
    //------
    for (j in 1:D){
      L[j,j] = L_d[j];
      for (i in (j+1):P){
        idx2 = idx2 +1;
        L[i,j] = L_t[idx2];
      }
    }
    
  }
  //---------
}

model {
   /* ... declarations ... statements ... */

    //-------------------------------------
}

generated quantities {

}
