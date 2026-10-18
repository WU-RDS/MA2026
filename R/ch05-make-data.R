# ------------------------------------------------------------------
# ch05-make-data.R
# Generates the two synthetic data sets of Chapter 5 and writes them
# to data/. Run once; the chapter and the slides read the CSV files.
# The playlist data (data/playlist_did.csv) are real and come from the
# WU-RDS difference-in-differences workshop (github.com/WU-RDS/did).
# ------------------------------------------------------------------

library(tidyverse)

# --- 1. Smart Mix as an opt-in feature (observational data) -----------
# 2,000 users of the streaming service. Smart Mix was switched on by
# the users themselves. True effect of Smart Mix on listening hours: +3.
set.seed(505)
n <- 2000
enthusiasm <- rnorm(n)                                   # unobserved!
tenure     <- sample(1:60, n, replace = TRUE)            # months as a customer
device     <- sample(c("iOS", "Android", "Desktop"), n, replace = TRUE, prob = c(0.45, 0.40, 0.15))
premium    <- rbinom(n, 1, plogis(-0.9 + 0.7 * enthusiasm + 0.012 * tenure))
age        <- pmin(pmax(round(rnorm(n, 34, 11)), 18), 70)
prior_hours <- round(exp(2.65 + 0.40 * enthusiasm + 0.10 * premium + 0.004 * tenure + rnorm(n, 0, 0.30)), 1)
smart_mix  <- rbinom(n, 1, plogis(-1.1 + 0.8 * enthusiasm + 0.5 * premium + 0.3 * (device == "iOS")))
hours <- round(exp(2.75 + 0.40 * enthusiasm + 0.10 * premium + 0.004 * tenure + rnorm(n, 0, 0.30)) +
                 3 * smart_mix - 2.0 * (device == "Android") - 4.0 * (device == "Desktop"), 1)
hours <- pmax(hours, 0.5)
churn <- rbinom(n, 1, plogis(0.4 - 0.07 * hours - 0.6 * premium - 0.02 * tenure - 0.3 * smart_mix))

smartmix_obs <- tibble(
  user_id = 1:n, smart_mix, hours, prior_hours, tenure, premium, device, age, churn
)
write_csv(smartmix_obs, "data/smartmix_obs.csv")

# --- 2. Promotion campaigns (multiplicative DGP) -----------------------
# 500 promotion campaigns for new releases: spend on promotion, number of
# playlist placements, the artist's followers, and streams in the first
# four weeks. True elasticities: spend 0.30, followers 0.50;
# each playlist placement adds about 12% (log-level coefficient 0.12).
set.seed(506)
m <- 500
followers  <- round(exp(rnorm(m, 10.5, 1.3)))             # artist followers
spend      <- round(exp(rnorm(m, 8.2, 0.9)), -1)          # promotion spend in euros
placements <- rpois(m, 3)                                 # playlist placements
genre      <- sample(c("Pop", "HipHop/Rap", "Rock", "Electro/Dance"), m, replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.15))
log_streams <- 3.2 + 0.30 * log(spend) + 0.50 * log(followers) + 0.12 * placements +
  c(Pop = 0, `HipHop/Rap` = 0.25, Rock = -0.15, `Electro/Dance` = 0.05)[genre] + rnorm(m, 0, 0.45)
campaigns <- tibble(
  campaign_id = 1:m, genre, spend, placements, followers,
  streams = round(exp(log_streams))
)
write_csv(campaigns, "data/campaigns.csv")
