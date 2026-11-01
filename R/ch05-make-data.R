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
# Upgrade to Premium within three months (free users only): a full S-curve in hours
upgraded <- rbinom(n, 1, plogis(-4.5 + 0.16 * hours))
upgraded[premium == 1] <- NA

smartmix_obs <- tibble(
  user_id = 1:n, smart_mix, hours, prior_hours, tenure, premium, device, age, churn, upgraded
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


# --- 3. Coding-block data sets --------------------------------------------

# (a) Podcast promotion: 400 episodes. Multiplicative DGP (spend elasticity 0.35,
#     follower elasticity 0.45), so that both a simple regression (block 1) and
#     a log-log model (block 3) can be fitted to the same data.
set.seed(511)
k <- 400
host_followers <- round(exp(rnorm(k, 9.5, 1.0)))
promo_spend    <- round(exp(rnorm(k, 6.8, 0.8)), -1)            # euros, roughly 200 to 10,000
downloads      <- round(exp(2.8 + 0.35 * log(promo_spend) + 0.45 * log(host_followers) + rnorm(k, 0, 0.4)))
podcast_promo <- tibble(episode_id = 1:k, promo_spend, host_followers, downloads)
write_csv(podcast_promo, "data/podcast_promo.csv")

# (b) Loyalty program at an online shop: 1,500 customers. Customers opted into
#     the program themselves (self-selection via an unobserved "affinity");
#     prior-year spending is an observed proxy. True effect of membership: +40 euros.
set.seed(512)
n2 <- 1500
affinity   <- rnorm(n2)                                          # unobserved
tenure_y   <- sample(1:10, n2, replace = TRUE)
device2    <- sample(c("Mobile", "Desktop"), n2, replace = TRUE, prob = c(0.6, 0.4))
spend_prev <- round(exp(5.3 + 0.45 * affinity + 0.03 * tenure_y + rnorm(n2, 0, 0.35)))
member     <- rbinom(n2, 1, plogis(-0.9 + 0.9 * affinity + 0.05 * tenure_y))
spending   <- round(exp(5.4 + 0.45 * affinity + 0.03 * tenure_y + rnorm(n2, 0, 0.3)) + 40 * member - 20 * (device2 == "Mobile"))
loyalty_program <- tibble(customer_id = 1:n2, member, spending, spend_prev, tenure = tenure_y, device = device2)
write_csv(loyalty_program, "data/loyalty_program.csv")

# (c) App reinstall: 1,200 users who uninstalled the app received a win-back
#     offer with a randomly assigned discount (0 to 60%). Outcome: reinstalled
#     within 30 days. Full S-curve in the discount; prior usage shifts the curve.
set.seed(513)
n3 <- 1200
discount   <- sample(seq(0, 60, by = 5), n3, replace = TRUE)
prior_use  <- round(exp(rnorm(n3, 2.3, 0.6)))                      # sessions in the last month before uninstalling
reinstall  <- rbinom(n3, 1, plogis(-3.6 + 0.11 * discount + 0.04 * prior_use))
app_reinstall <- tibble(user_id = 1:n3, discount, prior_use, reinstalled = reinstall)
write_csv(app_reinstall, "data/app_reinstall.csv")
