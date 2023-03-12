data {
    /* ... declarations ... */
    int<lower = 1> N; // number observations 
    int<lower = 1> P; // number of dimensions of y
    int<lower = 1> L; // numner of latent dimensions
    matrix[N, P] Y;   // data
}

parameters {
    /* ... declarations ... */
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
