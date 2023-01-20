functions {
   /* ... function declarations and definitions ... */
}

data {
   /* ... declarations ... */
   int N; // number of observations
   int<lower=0, upper=3> G; // number of groups
   vector[N] time;
   vector[N] weight;
   int<lower=0, upper=1> event[N];
   int<lower=1, upper=3> gid[N];
}

parameters {
   /* ... declarations ... */
   real a;
   vector[G] a_group;
   vector[G] b_group;
   real<lower=0> k;
}

transformed parameters {
   /* ... declarations ... statements ... */
   vector[N] mu = exp(a+a_group[gid] + b_group[gid] .* weight);
   vector[N] lambda = mu / tgamma(1+1/k);
}

model {
   /* ... declarations ... statements ... */
   // Priors
   a ~ normal(0,1);
   a_group ~ normal(0,1);
   b_group ~ normal(0,1);
   k ~ gamma(2, 0.5);
   // Likelihood
}

generated quantities {
   /* ... declarations ... statements ... */
   vector[N] pred;
   for (i in 1:N){
      pred[i] = weibull_rng(k, lambda[i]);
   }
   //=============================
}
