## TO BE DONE
## At least one exercise!

# Create erm2016
retro2016 <- read_rds(here::here("data/retro2016.rds"))

retro2016 <- retro2016 |>
  mutate(
    runs_before = away_score_ct + home_score_ct,
    half_inning = paste(game_id, inn_ct, bat_home_id),
    runs_scored =
      (bat_dest_id > 3) + (run1_dest_id > 3) +
      (run2_dest_id > 3) + (run3_dest_id > 3)
  )


half_innings <- retro2016 |>
  group_by(half_inning) |>
  summarize(
    outs_inning = sum(event_outs_ct),
    runs_inning = sum(runs_scored),
    runs_start = first(runs_before),
    max_runs = runs_inning + runs_start
  )

retro2016 <- retro2016 |>
  inner_join(half_innings, by = "half_inning") |>
  mutate(runs_roi = max_runs - runs_before)


retro2016 <- retro2016 |>
  mutate(
    bases = paste0(
      if_else(base1_run_id == "", 0, 1),
      if_else(base2_run_id == "", 0, 1),
      if_else(base3_run_id == "", 0, 1)
    ),
    state = paste(bases, outs_ct)
  )
retro2016 <- retro2016 |>
  mutate(
    is_runner1 = as.numeric(
      run1_dest_id == 1 | bat_dest_id == 1
    ),
    is_runner2 = as.numeric(
      run1_dest_id == 2 | run2_dest_id == 2 |
        bat_dest_id == 2
    ),
    is_runner3 = as.numeric(
      run1_dest_id == 3 | run2_dest_id == 3 |
        run3_dest_id == 3 | bat_dest_id == 3
    ),
    new_outs = outs_ct + event_outs_ct,
    new_bases = paste0(is_runner1, is_runner2, is_runner3),
    new_state = paste(new_bases, new_outs)
  )


changes2016 <- retro2016 |>
  filter(state != new_state | runs_scored > 0)

changes2016_complete <- changes2016 |>
  filter(outs_inning == 3)

erm_2016 <- changes2016_complete |>
  group_by(bases, outs_ct) |>
  summarize(mean_run_value = mean(runs_roi))

erm_2016 |>
  pivot_wider(
    names_from = outs_ct,
    values_from = mean_run_value,
    names_prefix = "Outs="
  )

erm_2016 |> write_rds(
  file = here::here("data/erm2016.rds"), compress = "xz")

##




