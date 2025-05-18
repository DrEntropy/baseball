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

# no need to load the data is in the package!

data("umpires")
data("cabrera")
data("verlander")
