# ------------------------------------------------------------------
# ch06-make-data.R
# Generates the three synthetic data sets of Chapter 6 and writes them
# to data/. Run once; the chapter and the slides read the CSV files.
# ------------------------------------------------------------------

library(tidyverse)

# --- 1. Smart Mix, second wave: a randomized experiment with covariates
#        and a measured mediator ------------------------------------------
# 1,200 users, randomly assigned to Smart Mix (1) or control (0).
# Mediator: number of new artists discovered in the four weeks.
# The effect of Smart Mix is heterogeneous: larger for free users and
# for users with little prior listening. Part of it runs through discovery.
set.seed(606)
n <- 1200
smart_mix   <- rbinom(n, 1, 0.5)
premium     <- rbinom(n, 1, 0.4)
tenure      <- sample(1:60, n, replace = TRUE)
device      <- sample(c("iOS", "Android", "Desktop"), n, replace = TRUE, prob = c(0.45, 0.40, 0.15))
prior_hours <- round(exp(2.7 + 0.1 * premium + rnorm(n, 0, 0.45)), 1)
direct      <- 3.0 - 0.09 * (prior_hours - 20) - 1.5 * premium          # direct effect, varies by user
new_artists <- round(pmax(2 + 4 * smart_mix + 0.03 * prior_hours + rnorm(n, 0, 2.5), 0))
hours       <- round(pmax(0.8 * prior_hours + 2 * premium + 0.03 * tenure +
                            direct * smart_mix + 0.3 * new_artists + rnorm(n, 0, 5), 0.5), 1)
smartmix_wave2 <- tibble(user_id = 1:n, smart_mix, premium, tenure, device, prior_hours, new_artists, hours)
write_csv(smartmix_wave2, "data/smartmix_wave2.csv")

# --- 2. A 2 x 3 survey experiment: ad designs --------------------------------
# 360 respondents rate the creativity of an ad for the streaming service
# (0-100). Factor 1: who made the design (Human / AI / Human + AI).
# Factor 2: whether the source was disclosed (Revealed / Not revealed).
set.seed(607)
cells <- expand_grid(source = c("Human", "AI", "Human + AI"),
                     disclosure = c("Revealed", "Not revealed"))
cell_means <- c(`Human|Revealed` = 63, `AI|Revealed` = 45, `Human + AI|Revealed` = 45,
                `Human|Not revealed` = 55, `AI|Not revealed` = 63, `Human + AI|Not revealed` = 63)
ad_designs <- cells |>
  slice(rep(1:n(), each = 60)) |>
  mutate(respondent = row_number(),
         creativity = round(pmin(pmax(cell_means[paste(source, disclosure, sep = "|")] + rnorm(n(), 0, 15), 0), 100))) |>
  select(respondent, source, disclosure, creativity)
write_csv(ad_designs, "data/ad_designs.csv")

# --- 3. Promotion campaigns, second year ----------------------------------
# 600 campaigns. The spend elasticity differs by genre (continuous x
# categorical) and increases with the artist's follower base
# (continuous x continuous).
set.seed(608)
m <- 600
genre      <- sample(c("Pop", "HipHop/Rap", "Rock", "Electro/Dance"), m, replace = TRUE, prob = c(0.4, 0.3, 0.15, 0.15))
followers  <- round(exp(rnorm(m, 10.5, 1.2)))
spend      <- round(exp(rnorm(m, 8.2, 0.9)), -1)
placements <- rpois(m, 3)
genre_el   <- c(Pop = 0.30, `HipHop/Rap` = 0.45, Rock = 0.20, `Electro/Dance` = 0.30)[genre]
genre_lvl  <- c(Pop = 0, `HipHop/Rap` = 0.1, Rock = -0.1, `Electro/Dance` = 0.05)[genre]
log_streams <- 3.4 + genre_lvl + genre_el * (log(spend) - 8.2) + 0.5 * (log(followers) - 10.5) +
  0.08 * (log(spend) - 8.2) * (log(followers) - 10.5) + 0.12 * placements + rnorm(m, 0, 0.45)
promo <- tibble(campaign_id = 1:m, genre, spend, placements, followers, streams = round(exp(log_streams)))
write_csv(promo, "data/promo_campaigns.csv")
