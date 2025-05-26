## Baseball Framing

library(tidyverse)
library(abdwr3edata)
library(baseballr)


##### Data acqusition - commented out

# This is now needed to get up to date baseballr
#devtools::install_github("BillPetti/baseballr")

# unfortunately the data 'taken' was not included.
#data(package = "abdwr3edata")

#data_large = here::here("data_large")
#statcast_csv = here::here(data_large, "statcast_csv")
#statcast_season(2022, dir = statcast_csv )

#sc2022 <- statcast_csv  |>
#  statcast_read_csv(pattern = "2022.+\\.csv")

#write_rds(sc2022, here::here(data_large, "sc2022.rds") )

# sc2022 <- read_rds(here::here(data_large, "sc2022.rds"))
#
# sc2022 |>
#   group_by(game_type) |>
#   summarize(
#     num_games = n_distinct(game_pk),
#     num_pitches = n(),
#     num_hr = sum(events == "home_run", na.rm = TRUE)
#   )
#
#
# sc2022 <- sc2022 |>
#   mutate(
#     Outcome = case_match(
#       description,
#       c("ball", "blocked_ball", "pitchout",
#         "hit_by_pitch") ~ "ball",
#       c("swinging_strike", "swinging_strike_blocked",
#         "foul", "foul_bunt", "foul_tip",
#         "hit_into_play",  "missed_bunt" ) ~ "swing",
#       "called_strike" ~ "called_strike"),
#     Home = ifelse(inning_topbot == "Bot", 1, 0),
#     Count = paste(balls, strikes, sep = "-")
#   )
#
# taken <- sc2022 |>
#   filter(Outcome != "swing")
#
# taken_select <- select(
#   taken, pitch_type, release_speed,
#   description, stand, p_throws, Outcome,
#   plate_x, plate_z, fielder_2, # was fielder_2_1 ?
#   pitcher, batter, Count, Home, zone
# )
#
# write_rds(
#   taken_select,
#   here::here("data/sc_taken_2022.rds"),
#   compress = "xz"
# )
sc_taken <- read_rds(here::here("data/sc_taken_2022.rds"))
set.seed(12345)
taken <- sample_n(sc_taken, 50000)


plate_width <- 17 + 2 * (9/pi)
k_zone_plot <- ggplot(
  NULL, aes(x = plate_x, y = plate_z)
) +
  geom_rect(
    xmin = -(plate_width/2)/12,
    xmax = (plate_width/2)/12,
    ymin = 1.5,
    ymax = 3.6, color = "blue", alpha = 0
  ) +
  coord_equal() +
  scale_x_continuous(
    "Horizontal location (ft.)",
    limits = c(-2, 2)
  ) +
  scale_y_continuous(
    "Vertical location (ft.)",
    limits = c(0, 5)
  )

crc_fc <- c("#2905a1" ,"#e41a1c" ,"#4daf4a" ,"#984ea3")

k_zone_plot %+%
  sample_n(taken, size = 2000) +
  aes(color = Outcome) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = crc_fc)


## Statcast Zones

zones <- taken |>
  group_by(zone) |>
  summarize(
    N = n(),
    right_edge = min(1.5, max(plate_x)),
    left_edge = max(-1.5, min(plate_x)),
    top_edge = min(5, quantile(plate_z, 0.95, na.rm = TRUE)),
    bottom_edge = max(0, quantile(plate_z, 0.05, na.rm = TRUE)),
    strike_pct = sum(Outcome == "called_strike") / n(),
    plate_x = mean(plate_x),
    plate_z = mean(plate_z)
  )

library(ggrepel)
k_zone_plot %+% zones +
  geom_rect(
    aes(
      xmax = right_edge, xmin = left_edge,
      ymax = top_edge, ymin = bottom_edge,
      fill = strike_pct, alpha = strike_pct
    ),
    color = "lightgray"
  ) +
  geom_text_repel(
    size = 3,
    aes(
      label = round(strike_pct, 2),
      color = strike_pct < 0.5
    )
  ) +
  scale_fill_gradient(low = "gray70", high = "blue") +
  scale_color_manual(values = crc_fc) +
  guides(color = FALSE, alpha = FALSE)

## 7.4 Modeling called strike percentage

library(mgcv)
strike_mod <- gam(
  Outcome == "called_strike" ~ s(plate_x, plate_z),
  family = binomial,
  data = taken
)
library(broom)
hats <- strike_mod |>
  augment(type.predict = "response")

k_zone_plot %+% sample_n(hats, 10000) +
  geom_point(aes(color = .fitted), alpha = 0.1) +
  scale_color_gradient(low = "gray70", high = "blue")


hand_mod <- gam(
  Outcome == "called_strike" ~
    p_throws + stand + s(plate_x, plate_z),
  family = binomial,
  data = taken
)

library(modelr)

hand_grid <- taken |>
  data_grid(
    plate_x = seq_range(plate_x, n = 100),
    plate_z = seq_range(plate_z, n = 100),
    p_throws,
    stand
  )
hand_grid_hats <- hand_mod |>
  augment(type.predict = "response", newdata = hand_grid)

diffs <- hand_grid_hats |>
  group_by(plate_x, plate_z) |>
  summarize(
    N = n(),
    .fitted = sd(.fitted),
    .groups = "drop"
  )

k_zone_plot %+% diffs +
  geom_tile(aes(fill = .fitted), alpha = 0.7) +
  scale_fill_gradient(low = "gray92", high = "blue")

# 7.5 Modeling catcher framing
taken <- taken |>
  filter(
    is.na(plate_x) == FALSE,
    is.na(plate_z) == FALSE
  ) |>
  mutate(
    strike_prob = predict(
      strike_mod,
      type = "response"
    )
  )

library(lme4)
mod_a <- glmer(
  Outcome == "called_strike" ~
    strike_prob + (1|fielder_2),
  data = taken,
  family = binomial
)

c_effects <- mod_a |>
  ranef() |>
  as_tibble() |>
  transmute(
    id = as.numeric(levels(grp)),
    effect = condval
  )

# add pitcher names?

# master_id <- baseballr::chadwick_player_lu() |>
#   mutate(
#     mlb_name = paste(name_first, name_last),
#     mlb_id = key_mlbam
#   ) |>
#   select(mlb_id, mlb_name) |>
#   filter(!is.na(mlb_id))
# saveRDS(master_id, "data/master_id.rds")
master_id <- readRDS("data/master_id.rds")

c_effects <- c_effects |>
  left_join(
    select(master_id, mlb_id, mlb_name),
    join_by(id == mlb_id)
  ) |>
  arrange(desc(effect))

c_effects |> slice_head(n = 6)

# wierdly i get different results... might have to do with
# the fielder variable change?

mod_b <- glmer(
  Outcome == "called_strike" ~ strike_prob +
    (1|fielder_2) +
    (1|batter) + (1|pitcher),
  data = taken,
  family = binomial
)

c_effects <- mod_b |>
  ranef() |>
  as_tibble() |>
  filter(grpvar == "fielder_2") |>
  transmute(
    id = as.numeric(as.character(grp)),
    effect = condval
  )
c_effects <- c_effects |>
  left_join(
    select(master_id, mlb_id, mlb_name),
    join_by(id == mlb_id)
  ) |>
  arrange(desc(effect))

# Also different results here!
c_effects |> slice_head(n = 6)


#####
# Exercises
####
# 5. home vs awa

fit <- glm(
  Outcome == "called_strike" ~ Home,
  data = taken, family = binomial
)

exp(coef(fit))
# result is home of 0.975, only slightly lower odds at home.


tidy_fit <- broom::tidy(fit,
                        conf.int = TRUE,
                        exponentiate = TRUE)

ggplot(tidy_fit, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  labs(
    x = NULL, y = "Odds Ratio (log scale)",
    title = "Effect on Odds of Called Strike"
  ) +
  theme_minimal()

# Note you can ignore the intercept, it is just the baseline
# for away player

# Another way
taken$HomeFactor <- factor(taken$Home, labels = c("Away", "Home"))

fit2 <- glm(Outcome == "called_strike" ~ 0 + HomeFactor,
            data = taken, family = binomial)
summary(fit2)
exp(coef(fit2))

tidy_fit2 <- tidy(fit2, conf.int = TRUE, exponentiate = TRUE)

ggplot(tidy_fit2, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  #geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = NULL, y = "Odds of Called Strike",
    title = "Odds of Called Strike: Home vs Away"
  ) +
  theme_minimal()

