library(tidyverse)
library(abdwr3edata)
library(baseballr)

data(retro2016)
# or
#retro2016 <- read_rds(here::here("data/retro2016.rds"))
pbp2016 <- retro2016 |>
  mutate(pseq = str_remove_all(pitch_seq_tx, "[.>123N+*]"))


# add run states as in chapter 5
pbp2016 <- pbp2016 |>
  retrosheet_add_states()

erm_2016 <- read_rds(here::here("data/erm2016.rds"))


# add in run_value
pbp2016 <- pbp2016 |>
  left_join(
    erm_2016, join_by(bases, outs_ct)
  ) |>
  rename(rv_start = mean_run_value) |>
  left_join(
    erm_2016, join_by(new_bases == bases, new_outs == outs_ct)
  ) |>
  rename(rv_end = mean_run_value) |>
  replace_na(list(rv_end = 0)) |>
  mutate(run_value = rv_end - rv_start + runs_scored)

# add counts
pbp2016 <- pbp2016 |>
  retrosheet_add_counts()

# Now we have a beginning and ending run value associated with each play,
# and we know whether the at-bat moved through each
# of the 12 possible counts.

pbp2016 |>
  select(
    game_id, event_id, run_value, c00, c10, c20,
    c11, c01, c30, c21, c31, c02, c12, c22, c32
  ) |>
  slice_head(n = 5)

# start out ahead vs behind.
pbp2016 |>
  filter(c10 == 1 | c01 == 1) |>
  group_by(c10, c01) |>
  summarize(
    num_ab = n(),
    mean_run_value = mean(run_value)
  )
# result 0.07 runs.
# this seems obvious to me, more likely to get out if you have a strike already
# ?


pbp_counts <- pbp2016 |>
  select(starts_with("c"), run_value)

# Convert to long form
pbp_counts_tidy <- pbp_counts |>
  pivot_longer(
    cols = -run_value,
    names_to = "count",
    values_to = "passed_thru"
  )

run_value_by_count <- pbp_counts_tidy |>
  filter(passed_thru) |>
  group_by(count) |>
  summarize(
    num_ab = n(),
    value = mean(run_value)
  )

run_value_by_count <- run_value_by_count |>
  mutate(
    balls = str_sub(count, 2, 2),
    strikes = str_sub(count, 3, 3)
  )

count_plot <- run_value_by_count |>
  ggplot(aes(x = strikes, y = balls, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2(
    "xRV", low = "grey10", high = "blue",
    mid = "white",
  )
count_plot

# from this one can see which counts favor
# the hitter vs the pitcher.

# Investigate path dependancy...


###############
# Umpire ()
# no need to load the data is in the package!

data("umpires")
sample_n(umpires,10)

pred_area <- expand_grid(
  px = seq(-2, 2, by = 0.1),
  pz = seq(0, 6, by = 0.1)
)

umpires_rhb <- umpires |>
  filter(
    batter_hand == "R",
    balls == 0 & strikes == 0 |
      balls == 3 & strikes == 0 |
      balls == 0 & strikes == 2
  )

ump_counts <- umpires_rhb |>
  mutate(count = paste(balls, strikes, sep = "-")) |>
  group_by(count)

counts <- ump_counts |>
  group_keys() |>
  pull(count)

ump_count_fits <- ump_counts |>
  group_split() |>
  map(sample_n, 3000) |>
  map(~loess(
    called_strike ~ px + pz, data = .x,
    control = loess.control(surface = "direct"))
  ) |>
  map(predict, newdata = pred_area) |>
  map(~tibble(pred_area, fit = as.numeric(.x))) |>
  set_names(nm = counts) |>
  list_rbind(names_to = "count") |>
  mutate(
    balls = str_sub(count, 1, 1),
    strikes = str_sub(count, 3, 3)
  )

k_zone_plot <-   ggplot(mapping = aes(x = px, y = pz)) +
  geom_rect(
    xmin = -0.947, xmax = 0.947, ymin = 1.5,
    ymax = 3.6, fill = "lightgray", alpha = 0.01
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


crc_fc = c( "#2905a1" ,"#e41a1c", "#4daf4a", "#984ea3")
k_zone_plot %+% filter(ump_count_fits, fit < 0.6 & fit > 0.4) +
  geom_contour(
    aes(z = fit, color = count, linetype = count),
    binwidth = 0.1
  ) +  scale_color_manual(values = crc_fc)

###########
#. Excersize 4

# Umpire’s Strike Zone

#By drawing a contour plot, compare the umpire’s strike
#zone for left-handed and right-handed batters.
#Use only the rows of the data frame where the
#pitch type is a four-seam fastball.

umpire_ff = umpires |> filter(pitch_type == "FF") |>
  group_by(batter_hand)

batter_hands = umpire_ff |> group_keys() |> pull(batter_hand)
ump_hand_fits <- umpire_ff   |>
  group_split() |>
  map(sample_n, 3000) |>
  map(~loess(
    called_strike ~ px + pz, data = .x,
    control = loess.control(surface = "direct"))
  ) |>
  map(predict, newdata = pred_area) |>
  map(~tibble(pred_area, fit = as.numeric(.x))) |>
  set_names(nm = batter_hands ) |>
  list_rbind(names_to = "batter_hand")

k_zone_plot %+% filter(ump_hand_fits, fit < 0.6 & fit > 0.4) +
  geom_contour(
    aes(z = fit, color = batter_hand, linetype = batter_hand),
    binwidth = 0.1
  ) +  scale_color_manual(values = crc_fc)

