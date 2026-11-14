# ------------------------------------------------------------------
# ch06-make-data.R
# Generates the synthetic data sets of Chapter 6 (three for the chapter,
# two for the coding block) and writes them to data/. Run once; the
# chapter, the slides and the R scripts read the CSV files.
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

# --- 4. Coding block, case 1: the climate-neutral shipping label -------------
# A/B test at an online shop: 1,000 customers of the shop's customer panel
# (for whom a sustainability score from an earlier survey is available) were
# randomly shown a "climate-neutral shipping" label at checkout (label = 1)
# or not (label = 0). Outcome: order value in euros. The label effect is
# larger for new customers (moderator: segment) and grows with the
# customer's sustainability score (moderator: eco_score, 1-7).
set.seed(609)
n4 <- 1000
label     <- rbinom(n4, 1, 0.5)
segment   <- sample(c("New", "Returning"), n4, replace = TRUE, prob = c(0.4, 0.6))
eco_score <- pmin(pmax(round(rnorm(n4, 4.2, 1.4)), 1), 7)
new       <- as.numeric(segment == "New")
label_effect <- -4 + 2.5 * eco_score + 5 * new                 # effect of the label, varies by customer
order_value  <- round(pmax(55 + 12 * (1 - new) + 1.5 * eco_score + label * label_effect + rnorm(n4, 0, 20), 8), 2)
green_label <- tibble(customer_id = 1:n4, label, segment, eco_score, order_value)
write_csv(green_label, "data/green_label.csv")

# --- 5. Coding block, case 2: the story behind the coffee ------------------
# A survey experiment of the kind you run in your group project: 320
# respondents saw one of two product descriptions of a specialty coffee -
# a factual one ("Facts") or one telling the story of the farm ("Story") -
# and then stated the perceived authenticity of the brand (mean of three
# 1-7 items, the mediator) and their willingness to pay for a 250 g pack
# (euros). Coffee involvement (1-7) was measured BEFORE the manipulation.
set.seed(610)
n5 <- 320
version      <- sample(c("Facts", "Story"), n5, replace = TRUE)
story        <- as.numeric(version == "Story")
involvement  <- pmin(pmax(round(rnorm(n5, 4.3, 1.3)), 1), 7)
authenticity <- round(pmin(pmax(2.9 + 1.2 * story + 0.2 * involvement + rnorm(n5, 0, 0.9), 1), 7), 2)
wtp          <- round(pmax(2 + 0.6 * story + 0.9 * authenticity + 0.5 * involvement + rnorm(n5, 0, 1.5), 1), 2)
coffee_story <- tibble(respondent = 1:n5, version, involvement, authenticity, wtp)
write_csv(coffee_story, "data/coffee_story.csv")
