data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
}

parameters {
  real alpha;
  vector[K] beta;
  real sigma;  // same as before just removed <lower=0>
}

model {
  sigma ~ exponential(1);
  alpha ~ normal(0, 10);
  for (k in 1:K) beta[k] ~ normal(0, 5);
  for (n in 1:N){
    y[n] ~ normal(X[n, ] * beta + alpha, sigma);
  }
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N){
   real y_hat = X[n, ] * beta + alpha;
   y_rep[n] = normal_rng(y_hat, sigma);
  }
}
