// single line comment

/*
  multi line comment
*/


/*
  stan files in rstudio will have a 'check' button that will call Stan's parser
  this will be useful to check syntax errors

  acts as a linter?
*/

// observed variables
data {
  // Dimentions

  //lower = 1 is telling stan that minimum value is 1
  // this lower = 1 can be omited, but it's used as a sanity check
  // lower=1, upper=10000000
  // this is an integer N
  int<lower=1> N; // number of rows

  // number of predictors (covariates)
  int<lower=1> K; // number of predictors

    // Variables

    matrix[N, K] X;
    vector[N] y;

    // real mean_alpha; // see model block
}

// unobserved variables
parameters {
  // blocks for unobserved quantities (not passed from R or Python)
  // it's analogous to the data block, but for the unobserved quantities
  real alpha; // intercept in regression model

  vector[K] beta; // regression coefficients
  // can also say vector<lower=0>[K] beta; if you need to actually constrain
  // but this is probably something you don't want to do?

  real<lower=0> sigma; // this constraint is required
}

// log + log
model {
  // where the magic happens
  // there is flexibility in the order you specify things

  // exponential distribution with rate of 1
  sigma ~ exponential(1); // just showing a distribution, not used as an actual prior

  // alpha is normal with mean 0, and SD 10
  // note it's STANDARD DEVIATION, not variance or precision, etc
  // since stan doesn't care about conjugency
  // SD may also be refered to as 'scale'
  alpha ~ normal(0, 10); // these numbers could've been declared in the data block
  // alpha ~ normal(mean_alpha, 10)

  for (k in 1:K) beta[k] ~ normal(0, 5); // putting the same prior on each beta_k
  // for (idx in 1:K) beta[idx] ~ normal(0, 5);

  // predictor for that person (row)
  // times the vector beta
  for (n in 1:N){
    y[n] ~ normal(X[n, ] * beta + alpha, sigma); // this is matrix multiplication
    // normal(X[n]) // will also get the row
    // but you need the coma if you wanted the column
    // normal(X[, n]) if you wanted the nth column
  }
}

// simulate from generative model
// optional, but highly recommended
// default in stan is 4 chains,
// each chain has 1000 observations
generated quantities {
  // this example is in-sample

  // exactly the same dimension as y
  // aka yppc for y posterior predictive check
  vector[N] y_rep; // rep for replication

  for (n in 1:N){

    // this variable is locally scoped
    // becuase it's defined within the for loop
   real y_hat = X[n, ] * beta + alpha; // local/temp variable

   // equlivilant to rnorm function in R
   y_rep[n] = normal_rng(y_hat, sigma);
  }
}
