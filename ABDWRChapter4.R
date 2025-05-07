#########
# Chapter 4 - exercise 2


library(tidyverse)
library(abdwr3edata)
library(Lahman)

# 19th century teams

my_teams <- Teams |>
  filter(yearID >= 1800, yearID < 1900) |>
  select(teamID, yearID, lgID, G, W, L, R, RA) |>
  mutate(RD = R - RA, Wpct = W / (W + L))
my_teams |>
  slice_tail(n = 6)


ggplot(my_teams, aes(x = RD, y = Wpct)) +
  geom_point() +
  scale_x_continuous("Run differential") +
  scale_y_continuous("Winning percentage")

# Add logs

my_teams <- my_teams |>
  mutate(
    logWratio = log(W / L),
    logRratio = log(R / RA)
  )

# Some teams won no games at all ??
my_teams <- my_teams |> filter(W != 0)

# fit
pytFit <- lm(logWratio ~ 0 + logRratio, data = my_teams)
pytFit

# compute residuals
exp = coef(pytFit)
my_teams <- my_teams |>
  mutate(Wpct_pyt = R ^ exp / (R ^ exp + RA ^ exp)) |>
  mutate(residuals_pyt = Wpct - Wpct_pyt)

base_plot <- ggplot(my_teams, aes(x = RD, y = residuals_pyt)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlab("Run differential") + ylab("Residual")

highlight_teams <- my_teams |>
  arrange(desc(abs(residuals_pyt))) |>
  slice_head(n = 8)

library(ggrepel)
base_plot +
  geom_point(data = highlight_teams, color = "blue") +
  geom_text_repel(
    data = highlight_teams, color = "blue",
    aes(label = paste(teamID, yearID))
  )


# "By inspecting the residual plot of your fitted model
#from (a), did the great and poor teams in the 19th century
#do better or worse than one would expect on the basis of
#their run differentials?

# I dont see anyone doing much better or worse on the extremes...
# I guess the best did slighlty better then predicted by pythag
# and the worse did slightly worse then predicted by pythag, where
# by best and worse I mean in terms of run differential.


# Plot vs predicted win percentage
# Note that if you plot vs actual win percentage you will
# see patterns, but these are not real. Residuals are not
# in general independant of the response!  See ROS 11.2

base_plot2 <- ggplot(my_teams, aes(x = Wpct_pyt, y = residuals_pyt)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = 3) +
  xlab("Wpct") + ylab("Residual")

base_plot2 +
  geom_point(data = highlight_teams, color = "blue") +
  geom_text_repel(
    data = highlight_teams, color = "blue",
    aes(label = paste(teamID, yearID))
  )
