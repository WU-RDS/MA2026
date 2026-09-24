# ======================================================================
# Marketing Analytics — Chapter 3: Uncertainty
# Companion code to Chapter 3 of the course book. The chapter explains
# every step in detail; this script collects the code in one place.
# ======================================================================


# ----------------------------------------------------------------------
# 0. Setup
# ----------------------------------------------------------------------

library(tidyverse)
options(scipen = 999)


# ======================================================================
# 3.3 Sampling variation
# ======================================================================

# A simulated population: monthly listening time (hours) of all
# 25,000 WU students. Because we simulate it, we know the true values.
set.seed(321)
population <- rnorm(25000, mean = 50, sd = 10)

mean(population)   # population mean (mu)
sd(population)     # population standard deviation (sigma)

ggplot(tibble(hours = population), aes(hours)) +
  geom_histogram(bins = 60, fill = "#dbe7f3", color = "white") +
  geom_vline(xintercept = mean(population), color = "#0a3d6e", linewidth = 1) +
  labs(x = "Hours per month", y = "Number of students") +
  theme_minimal()

# One random sample of 100 students ...
set.seed(6789)
sample_1 <- sample(population, size = 100)
mean(sample_1)

# ... and another one: a different mean (sampling variation)
sample_2 <- sample(population, size = 100)
mean(sample_2)

# The sampling distribution: the means of many samples
set.seed(12345)
sample_means <- replicate(20000, mean(sample(population, size = 100)))

mean(sample_means)   # close to the population mean
sd(sample_means)     # the standard error

ggplot(tibble(mean = sample_means), aes(mean)) +
  geom_histogram(bins = 50, fill = "#0f5da8", color = "white") +
  geom_vline(xintercept = mean(population), color = "#b03a2e", linetype = "dashed") +
  labs(x = "Sample mean (hours per month)", y = "Number of samples") +
  theme_minimal()


# ======================================================================
# 3.4 The standard error
# ======================================================================

# Standard error from the formula: sigma / sqrt(n)
sd(population) / sqrt(100)

# The square-root law: 4x the sample size, half the standard error
tibble(n = c(25, 100, 400, 1600)) |>
  mutate(se = sd(population) / sqrt(n))

# In practice we do not know sigma -> estimate it with the sample SD
sd(sample_1) / sqrt(length(sample_1))


# ======================================================================
# 3.5 The central limit theorem
# ======================================================================

# A right-skewed population (most students listen a moderate amount,
# a few listen a lot)
set.seed(321)
population_skewed <- rgamma(25000, shape = 2, scale = 10)

ggplot(tibble(hours = population_skewed), aes(hours)) +
  geom_histogram(bins = 60, fill = "#dbe7f3", color = "white") +
  labs(x = "Hours per month", y = "Number of students") +
  theme_minimal()

# Sample means for different sample sizes: the larger n, the more
# normal (and the narrower) the distribution of the sample means
set.seed(12345)
clt <- map_dfr(c(1, 5, 30, 100), function(size) {
  tibble(n    = paste("n =", size),
         mean = replicate(10000, mean(sample(population_skewed, size = size))))
}) |>
  mutate(n = fct_inorder(n))

ggplot(clt, aes(mean)) +
  geom_histogram(bins = 50, fill = "#0f5da8", color = "white") +
  facet_wrap(~ n, nrow = 1, scales = "free") +
  labs(x = "Sample mean (hours per month)", y = "Number of samples") +
  theme_minimal()


# ======================================================================
# 3.6 Confidence intervals
# ======================================================================

# Critical values
qnorm(0.975)           # normal distribution: 1.96
qt(0.975, df = 99)     # t-distribution with n = 100: 1.98
qt(0.975, df = 9)      # t-distribution with n = 10: 2.26

# How often does a 95% confidence interval contain the true mean?
# (100 samples, 100 intervals)
set.seed(12345)
intervals <- map_dfr(1:100, function(i) {
  x  <- sample(population, size = 100)
  se <- sd(x) / sqrt(100)
  tibble(sample = i,
         lower  = mean(x) - qt(0.975, 99) * se,
         upper  = mean(x) + qt(0.975, 99) * se)
}) |>
  mutate(contains_mu = lower <= mean(population) & upper >= mean(population))

count(intervals, contains_mu)   # about 95 of 100


# ======================================================================
# 3.8 Uncertainty in R: working with one sample
# ======================================================================

# A random sample of 100 students from the (unknown) skewed population
set.seed(6789)
listening <- tibble(hours = sample(population_skewed, size = 100))

# --- Confidence interval for a mean, step by step -----------------------
n    <- nrow(listening)
xbar <- mean(listening$hours)
se   <- sd(listening$hours) / sqrt(n)     # standard error of the mean
crit <- qt(0.975, df = n - 1)             # critical value (95%)

c(mean = xbar, se = se, lower = xbar - crit * se, upper = xbar + crit * se)

# --- The same in one line --------------------------------------------------
t.test(listening$hours)$conf.int

# --- Confidence intervals for several groups ------------------------------
# (the account type is simulated here, for illustration only)
listening <- listening |>
  mutate(account = sample(c("paid", "free"), size = n(), replace = TRUE, prob = c(0.4, 0.6)))

listening |>
  group_by(account) |>
  summarise(
    n     = n(),
    mean  = mean(hours),
    se    = sd(hours) / sqrt(n),
    lower = mean - qt(0.975, n - 1) * se,
    upper = mean + qt(0.975, n - 1) * se
  )

# --- Confidence interval for a share ----------------------------------------
# Share of students who listen more than 30 hours per month
p_hat <- mean(listening$hours > 30)
se_p  <- sqrt(p_hat * (1 - p_hat) / n)
c(share = p_hat, lower = p_hat - 1.96 * se_p, upper = p_hat + 1.96 * se_p)

prop.test(x = sum(listening$hours > 30), n = n)$conf.int

# --- The margin of error of a poll -----------------------------------------
# 1,000 respondents, share of about 50%
qnorm(0.975) * sqrt(0.5 * 0.5 / 1000)     # 0.031 = 3.1 percentage points

# --- Planning the sample size -----------------------------------------------
# Respondents needed for a margin of error of +/- 5 percentage points
me_target <- 0.05
ceiling(qnorm(0.975)^2 * 0.5 * 0.5 / me_target^2)
