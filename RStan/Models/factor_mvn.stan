data {
    /* ... declarations ... */
    int<lower = 1> N; // number observations 
    int<lower = 1> P; // number of dimensions of y
    int<lower = 1> L; // numner of latent dimensions
    matrix[N, P] Y;   // data
}

transformed data {
    int<lower=1> M;
    M = L*(P-L) + L*(L-1) /2; // number of non-zero loadings
}

parameters {
    /* ... declarations ... */
    vector[M] L_t; // lower diagonal elements of L_t
    vector<lower = 0>[L] L_d; // lower diagonal elements of L-d 
}

transformed parameters {
    /* ... declarations ... statements ... */

}

model {
   /* ... declarations ... statements ... */

    //-------------------------------------
}

generated quantities {

}
