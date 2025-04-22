library(Lahman)
library(tidyverse)
library(ggthemes)

crcblue <- "#2905a1"

ws <- SeriesPost |>
  filter(yearID >= 1903, round == "WS", wins + losses < 8)
ggplot(ws, aes(x = wins + losses)) +
  geom_bar(fill = crcblue) +
  labs(x = "Number of games", y = "Frequency")



hr_rates <- function(age, hr, ab) {
  rates <- round(100 * hr / ab, 1)
  list(x = age, y = rates)
}

# mantle stuff
HR <- c(13, 23, 21, 27, 37, 52, 34, 42, 31, 40, 54)
AB <- c(341, 549, 461, 543, 517, 533, 474, 519, 541, 527, 514)
Age <- 19 : 29

mantle_hr_rates <- hr_rates(Age, HR,AB)
Mantle <- tibble(
  Age, HR, AB, Rates = mantle_hr_rates$y
)

write_csv(Mantle, here::here("data/mantle.csv"))

# splitting applying and combining

Batting_60 <- Batting |>
         filter(yearID >= 1960, yearID <= 1969)

Batting |>
  filter(yearID >= 1960, yearID <= 1969) |>
  group_by(playerID) |>
  summarize(HR = sum(HR)) |>
  arrange(desc(HR)) |>
  slice(1:4)

hr_leader <- function(data) {
  data |>
    group_by(playerID) |>
    summarize(HR = sum(HR)) |>
    arrange(desc(HR)) |>
    slice(1)
}



Batting_decade <- Batting |>
  mutate(decade = 10 * floor(yearID / 10)) |>
  group_by(decade)


decades <- Batting_decade |>
  group_keys() |>
  pull("decade")


Batting_decade |>
  group_split() |>
  map(hr_leader) |>
  set_names(decades) |>
  bind_rows(.id = "decade")


# Shorter version
hr_leaders_by_decade <- Batting %>%
  mutate(decade = 10 * floor(yearID / 10)) %>%
  group_by(decade, playerID) %>%
  summarize(HR = sum(HR), .groups = "drop_last") %>%
  slice_max(order_by = HR, n = 1, with_ties = FALSE) %>%
  ungroup()


long_careers <- Batting |>
  group_by(playerID) |>
  summarize(
    tAB = sum(AB, na.rm = TRUE),
    tHR = sum(HR, na.rm = TRUE),
    tSO = sum(SO, na.rm = TRUE)
  ) |> filter(tAB >= 5000)


ggplot(long_careers, aes(x = tHR / tAB, y = tSO / tAB)) +
  geom_point() + geom_smooth(color = crcblue)


## Exercises

#1 - Skip

#2

outcomes <- c("Single","Out","Out","Single","Out","Double","Out","Walk","Out","Single")
f.outcomes <- factor(
  outcomes,
  levels = c("Out", "Walk", "Single", "Double")
)
table(f.outcomes)

#5
library(Lahman)
data(Pitching)

career_pitching <- Pitching |>
  group_by(playerID) |>
  summarize(
    SO = sum(SO, na.rm = TRUE),
    BB = sum(BB, na.rm = TRUE),
    IPouts = sum(IPouts, na.rm = TRUE),
    midYear = median(yearID, na.rm = TRUE)
  )

# I skipped the inner join, not sure what the point of that is here

career_10000 <-   career_pitching  |>
                  filter(IPouts >= 10000)


ggplot(career_10000, aes(x = midYear, y = SO / BB)) +
  geom_point() + geom_smooth(color = crcblue)

# The plot shows that SO/BB was lowest around 1920 (deadball era). During this time pitchers
# pitched to contact rather then striking out.


