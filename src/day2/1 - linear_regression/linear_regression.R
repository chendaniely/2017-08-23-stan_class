############################################################
# Initial setup
############################################################
rm(list = ls())

library(rstan)
library(bayesplot)
stopifnot(packageVersion("bayesplot") < "1.3.0")
# Run install.packages("bayesplot") if your version is < 1.3.0

# caches your compiled models
rstan_options(auto_write = TRUE)

# automatically parallelizes chains
options(mc.cores = parallel::detectCores())

# will be in the next version of rstan
source('src/stan_utility.R')

############################################################
# Create data
############################################################

fit <- stan(file = 'src/day2/generate_data.stan',
            iter = 1,
            chains = 1,
            seed = 194838,
            algorithm = "Fixed_param")

N <- 25
M <- 3
X <- extract(fit)$X[1,,] # can also use as.array, or as.matrix
y <- extract(fit)$y[1,]

# dump it into a text file
stan_rdump(c("N", "M", "X", "y"),
           file = "output/day2/linear_regression.data.R")

############################################################
# Fit initial Stan program
############################################################

input_data <- read_rdump("output/day2/linear_regression.data.R")

# this random seed is not from R's set.seed
fit <- stan(file = 'src/day2/1 - linear_regression/linear_regression1.stan',
            data = input_data,
            seed = 4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit) # check divergences

params = extract(fit)

# Plot marginal posteriors
par(mfrow = c(3, 2))

hist(params$sigma, main = "", xlab = "sigma")
abline(v = 1,col = 2,lty = 1)

hist(params$alpha, main = "", xlab = "alpha")
abline(v = 10,col = 2,lty = 1)

hist(params$beta[,1], main = "", xlab = "beta[1]")
abline(v = 5,col = 2,lty = 1)

hist(params$beta[,2], main = "", xlab = "beta[2]")
abline(v = -3,col = 2,lty = 1)

hist(params$beta[,3], main = "", xlab = "beta[3]")
abline(v = 2,col = 2,lty = 1)

# Plot marginal posteriors and 'true' value recovery using bayesplot
mcmc_recover_hist(
    as.matrix(fit, pars = c("sigma", "alpha", "beta")),
    true = c(1, 10, 5, -3, 2),
    facet_args = list(ncol = 2)
)

# Plot a few visual posterior predictive checks
par(mfrow = c(2, 2))

hist(params$y_ppc[,5], main = "", xlab = "y[5]")
abline(v = input_data$y[5], col = 2, lty = 1)

hist(params$y_ppc[,10], main = "", xlab = "y[10]")
abline(v = input_data$y[10], col = 2, lty = 1)

hist(params$y_ppc[,15], main = "", xlab = "y[15]")
abline(v = input_data$y[15], col = 2, lty = 1)

hist(params$y_ppc[,20], main = "", xlab = "y[20]")
abline(v = input_data$y[20], col = 2, lty = 1)

# Same visual check using bayesplot
y_ppcs <- as.matrix(fit, pars = "y_ppc")
mcmc_recover_hist(
    y_ppcs[, c(5, 10, 15, 20)],
    true = input_data$y[c(5, 10, 15, 20)]
)

# Plot distribution of y against 50 draws from the
# posterior predictive distribution
ppc_dens_overlay(
    y = input_data$y,
    yrep = y_ppcs[sample(nrow(y_ppcs), 50),]
)

############################################################
# Fit with vectorized Stan program
############################################################

fit <- stan(file = 'src/day2/1 - linear_regression/linear_regression2.stan',
            data = input_data,
            seed = 4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

params = extract(fit)

# Plot marginal posteriors
par(mfrow = c(3, 2))

hist(params$sigma, main = "", xlab = "sigma")
abline(v = 1,col = 2,lty = 1)

hist(params$alpha, main = "", xlab = "alpha")
abline(v = 10,col = 2,lty = 1)

hist(params$beta[,1], main = "", xlab = "beta[1]")
abline(v = 5,col = 2,lty = 1)

hist(params$beta[,2], main = "", xlab = "beta[2]")
abline(v = -3,col = 2,lty = 1)

hist(params$beta[,3], main = "", xlab = "beta[3]")
abline(v = 2,col = 2,lty = 1)

# Same plot using bayesplot package
mcmc_recover_hist(
    as.matrix(fit, pars = c("sigma", "alpha", "beta")),
    true = c(1, 10, 5, -3, 2),
    facet_args = list(ncol = 2)
)

# Plot a few visual posterior predictive checks
par(mfrow = c(2, 2))

hist(params$y_ppc[,5], main = "", xlab = "y[5]")
abline(v = input_data$y[5], col = 2, lty = 1)

hist(params$y_ppc[,10], main = "", xlab = "y[10]")
abline(v = input_data$y[10], col = 2, lty = 1)

hist(params$y_ppc[,15], main = "", xlab = "y[15]")
abline(v = input_data$y[15], col = 2, lty = 1)

hist(params$y_ppc[,20], main = "", xlab = "y[20]")
abline(v = input_data$y[20], col = 2, lty = 1)

# Same visual check using bayesplot
y_ppcs <- as.matrix(fit, pars = "y_ppc")
mcmc_recover_hist(
    y_ppcs[, c(5, 10, 15, 20)],
    true = input_data$y[c(5, 10, 15, 20)]
)

# Plot distribution of y against 50 draws from the
# posterior predictive distribution
ppc_dens_overlay(
    y = input_data$y,
    yrep = y_ppcs[sample(nrow(y_ppcs), 50),]
)
