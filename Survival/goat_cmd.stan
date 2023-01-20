functions {
   /* ... function declarations and definitions ... */
}

data {
   /* ... declarations ... */
   int N; // number of observations
   int<lower=0, upper=3> G; // number of groups
   vector[N] time;
   vector[N] weight;
   array[N] int<lower=0, upper=1> event;
   array[N] int<lower=1, upper=3> gid;
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
   for (i in 1:N){
      if (event[i]==1) {
            time[i] ~ weibull(k, lambda[i]);
         }
      else {
            target += weibull_lccdf(time[i] | k, lambda[i]);
      }
   }
   //===================================
}

generated quantities {
   /* ... declarations ... statements ... */
   vector[N] pred;
   for (i in 1:N){
      pred[i] = weibull_rng(k, lambda[i]);
   }
   //=============================
   vector[N] mu_1 = exp(a + a_group[1]+b_group[1] .* weight);
   vector[N] mu_2 = exp(a + a_group[2]+b_group[2] .* weight);
   vector[N] mu_3 = exp(a + a_group[3]+b_group[3] .* weight);

   vector[N] diff_13 = log(mu_1) - log(mu_3);
   vector[N] diff_12 = log(mu_1) - log(mu_2);
   vector[N] diff_23 = log(mu_2) - log(mu_3);
   //========================================
}
