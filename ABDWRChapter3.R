# Chapter 3
# hof batting

#remotes::install_github("beanumber/abdwr3edata")

library(tidyverse)
library(abdwr3edata)

hof <- hof_batting


hof <- hof |>
  mutate(
    MidCareer = (From + To) / 2,
    Era = cut(
      MidCareer,
      breaks = c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
      labels = c(
        "19th Century", "Dead Ball", "Lively Ball", "Integration",
        "Expansion", "Free Agency", "Long Ball"
      )
    )
  )


hof_eras <- hof |>
  group_by(Era) |>
  summarize(N = n())

ggplot(hof, aes(Era)) +
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers")


ggplot(hof, aes(x = OPS)) +
  geom_histogram()


ggplot(hof, aes(MidCareer, OPS)) +
  geom_point() +
  geom_smooth()


library(ggrepel)
ggplot(hof, aes(MidCareer, OPS)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(
    data = filter(hof, OPS > 1.05 | OPS < .5),
    aes(MidCareer, OPS, label = Player), color = "red"
  )


(p <- ggplot(hof, aes(OBP, SLG)) + geom_point())
(p <- p +
    xlab("On Base Percentage") +
    ylab("Slugging Percentage"))

# Add OPS lines
(p <- p +
    geom_abline(
      slope = -1,
      intercept = seq(0.7, 1, by = 0.1),
      color = "red"
    )
)

p +
  annotate(
    "text", angle = -20,
    x = rep(0.31, 4) ,
    y = seq(0.4, 0.7, by = 0.1) + 0.02,
    label = paste("OPS = ", seq(0.7, 1, by = 0.1)),
    color = "red"
  )


hof <- hof |>
  mutate(hr_rate = HR / AB)

ggplot(hof, aes(hr_rate, Era)) +
  geom_boxplot(color = "brown", fill = "orange") +
  geom_jitter(height = 0.1)

##  Comparing Ruth, Aaron, Bonds, and A-Rod
library(Lahman)
PlayerInfo <- People |>
  filter(
    playerID %in% c(
      "ruthba01", "aaronha01", "bondsba01", "rodrial01"
    )
  ) |>
  mutate(
    mlb_birthyear = if_else(
      birthMonth >= 7, birthYear + 1, birthYear
    ),
    Player = paste(nameFirst, nameLast)
  ) |>
  select(playerID, Player, mlb_birthyear)


# join to batting info, using adjusted year to compute age the way MLB does it
HR_data <- Batting |>
  inner_join(PlayerInfo, by = "playerID") |>
  mutate(Age = yearID - mlb_birthyear) |>
  select(Player, Age, HR) |>
  group_by(Player) |>
  mutate(cHR = cumsum(HR))

crc_fc <- c("#2905a1", "#e41a1c", "#4daf4a" ,"#984ea3")

ggplot(HR_data, aes(x = Age, y = cHR, color = Player)) +
  geom_line() +
  scale_color_manual(values = crc_fc)


# 1998 home run race - Skipping for now, I dont want to deal with the CLI

### Exercises
#
#
#
####

# plan is to do a selection : 1-5 ,7

# Exercise 1

hof_pitching <- hof_pitching |>
  mutate(
    BF_group = cut(
      BF,
      c(0, 10000, 15000, 20000, 30000),
      labels = c("Less than 10000", "(10000, 15000)",
                 "(15000, 20000)", "more than 20000")
    )
  )

bf_counts <- hof_pitching |>
     group_by(BF_group) |>
     summarize(N = n())

ggplot(hof_pitching, aes(x = BF_group)) + geom_bar()

# Exercise 2
# WAR histogram

ggplot(hof_pitching, aes(x = WAR)) + geom_histogram()

hof_pitching |>
    filter(WAR > 120) |> select("...2")
# Cy Young and Walter Johnson

# Exercise 3 WAR / season

hof_pitching <- hof_pitching |>
  mutate(WAR_Season = WAR / Yrs)


ggplot(hof_pitching, aes(x = WAR_Season, y= BF_group)) + geom_jitter(height=.1)
ggplot(hof_pitching, aes(x = WAR_Season, y= BF_group)) + geom_boxplot(color = "brown", fill = "orange")

# There does seem to be a positive relation between batters faced and WAR/season in HOF data

# Exercise 4

hof_pitching <- hof_pitching |>
  mutate(MidYear = (From + To) / 2)
hof_pitching.recent <- hof_pitching |>
  filter(MidYear >= 1960)
hof_pitching.recent <- hof_pitching.recent |> arrange(desc(WAR_Season))

ggplot(hof_pitching.recent, aes(x=WAR_Season, y=...2)) +
                 geom_point(color = "blue")

# Exercise 5
ggplot(hof_pitching, aes(x = MidYear, y= WAR_Season)) + geom_point() + geom_smooth()

# Seems like war season was going down and then levelled off in around 1925

# Exercise 7
data(Teams)
Teams <- Teams  |>
           mutate(win_pct = W / (W + L)) |>
           filter(yearID == 2023)

library(mlbplotR)




team_mapping <- c(
  CHA = "CWS",
  CHN = "CHC",
  KCA = "KC",
  NYA = "NYY",
  NYN = "NYM",
  TBA = "TB",
  SLN = "STL",
  SDN = "SD",
  SFN = "SF",
  LAN = "LAD",
  WAS = "WSH",
  ARI = "AZ"
 )

# Apply recoding
Teams <- Teams |>
  mutate(teamID = recode(as.character(teamID), !!!team_mapping))


ggplot(Teams, aes(x= ERA, y = win_pct, team_abbr = teamID)) + geom_point() +
  geom_mlb_scoreboard_logos(width = .05)

