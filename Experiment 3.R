set.seed(123)

# Simulation parameters
n <- 100
B <- 1000
alpha <- 0.05

# Count the number of runs
count_runs <- function(x) {
  1 + sum(x[-1] != x[-length(x)])
}

# Runs Test for a binary sequence
runs_test <- function(x) {

  n1 <- sum(x == 1)
  n0 <- sum(x == 0)
  n <- length(x)

  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }

  R <- count_runs(x)

  ER <- 1 + (2 * n1 * n0) / n

  VR <- (2 * n1 * n0 * (2 * n1 * n0 - n)) /
        (n^2 * (n - 1))

  Z <- (R - ER) / sqrt(VR)

  2 * pnorm(-abs(Z))
}

# Generate independent random outcomes
generate_random <- function(n) {
  rbinom(n, size = 1, prob = 0.5)
}

# Generate streaky outcomes
generate_streaky <- function(n) {

  x <- numeric(n)
  x[1] <- rbinom(1, size = 1, prob = 0.5)

  for (i in 2:n) {
    if (runif(1) < 0.8) {
      x[i] <- x[i - 1]
    } else {
      x[i] <- 1 - x[i - 1]
    }
  }

  x
}

# Simulate p-values
p_random <- replicate(
  B,
  runs_test(generate_random(n))
)

p_streaky <- replicate(
  B,
  runs_test(generate_streaky(n))
)

# Calculate rejection rates
rejection_random <- mean(
  p_random < alpha,
  na.rm = TRUE
)

rejection_streaky <- mean(
  p_streaky < alpha,
  na.rm = TRUE
)

simulation_results <- data.frame(
  Model = c("Independent Random", "Streaky"),
  Rejection_Rate = c(
    rejection_random,
    rejection_streaky
  )
)

print(simulation_results)

# Plot rejection rates
barplot(
  c(rejection_random, rejection_streaky),
  names.arg = c("Independent Random", "Streaky"),
  ylim = c(0, 1),
  ylab = "Proportion of Rejected Tests",
  main = "Monte Carlo Simulation of the Runs Test"
)

abline(h = alpha, lty = 2)
