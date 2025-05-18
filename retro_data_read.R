library(tidyverse)
library(abdwr3edata)
library(baseballr)

# uses chadwick cli
retro_data <- baseballr::retrosheet_data(
  here::here("data_large/retrosheet"),
  c(2016)
)

#


retro2016 <- retro_data |>
  pluck("2016") |>
  pluck("events")

retro2016 |>
  write_rds(
    file = here::here("data/retro2016.rds"),
    compress = "xz"
  )


retro2016_roster <- retro_data |> pluck("2016") |> pluck("rosters")

retro2016_roster |> write_rds(
     file = here::here("data/retro2016_roster.rds"), compress = "xz")

