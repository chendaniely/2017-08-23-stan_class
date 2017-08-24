library("rstan")
library("bayesplot")
library("shinystan")

# Simulate data and fit linear regression model ---------------------------

# Today we'll simulate data in R but tomorrow we'll see how to do it within
# Stan itself.

# Parameter values for simulating data
alpha <- 1
beta <- c(-2, 1, 2)
sigma <- 2

# Data (outcome y, predictors X)
x <- runif(100, -1, 1);
X <- cbind(x, x^2, x^3)
y <- alpha + X %*% beta + rnorm(100, 0, sigma)
y <- as.vector(y)
standata <- list(N = nrow(X), K = ncol(X), y = y, X = X)

# Translate our Stan program to C++ and compile
mod <- stan_model("src/day1/010-linear-regression.stan")

# Fit the model with MCMC
fit1 <-
  sampling(
    mod,
    data = standata,
    # these are the default values below
    iter = 2000,
    warmup = 1000,
    chains = 4,
    cores = 4 ## set the number of cores
  )

# Extract post-warmup posterior draws of model parameters
#
# Note: we'll use as.array here, but there are other methods for extracting
# parameters draws that we'll see over the next few days. The vignette
# "Accessing the contents of a stanfit object" that comes with the rstan package
# is a good resource (also available at
# http://mc-stan.org/rstan/articles/stanfit_objects.html).

draws <- as.array(fit1)
dim(draws) # (iter - warmup) x chains x parameters/generated-quantities array
dimnames(draws)

# Quickly plot some parameter estimates using bayesplot package
# (tomorrow we'll do a lot more plotting and looking at results)
mcmc_intervals(
  draws,
  pars = c("alpha", "sigma", "beta[1]", "beta[2]", "beta[3]"),
  prob = 0.5,
  prob_outer = 0.9
)

mcmc_areas(
  draws,
  regex_pars = "beta",
  prob = 0.5,
  prob_outer = 1
)



# Warning messages --------------------------------------------------------

# We'll now intentionally trigger some warning messages so that we can talk
# about them now before we see them many times over the next few days. This
# document at http://mc-stan.org/misc/warnings.html is a useful resource for
# brief explanations of the various warning and error messages you get from
# Stan.

# Warnings about divergent transitions
# note: never set adapt_delta this low! (just for demonstration purposes)
fit1b <- sampling(mod, data = standata, control = list(adapt_delta = 0.4))

# I don't have shinystan installed because of PKI
# launch_shinystan(fit1b)


# Warnings about treedepth
# note: never set max_treedepth this low! (just for demonstration purposes)
fit1c <- sampling(mod, data = standata, control = list(max_treedepth = 5))

# delete these lines from day 1, they were added by accident
# check_divergences(fit1c)
# check_treedepth(fit1c)


# Now remove the <lower=0> next to 'sigma' in the parameters block of your
# linear-regression.stan file to trigger a different error/warning.
# We need to recompile the model before fitting it again since this time
# we've actually changed the underlying Stan code.
mod2 <- stan_model("src/day1/020-linear-regression.stan")
fit2 <- sampling(mod2, data = standata, init = 0)


