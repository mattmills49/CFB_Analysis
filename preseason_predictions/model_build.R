#' # Building a Pre-Season Prediction Model
#' 
#' We want a model that can give us an estimate of a team's chances of winning
#' a game given the location and opponent. To do this we need to find the 
#' relationship between the variables we have and the liklihood of a team 
#' winning the game. We also need to see how correlated the variables are.
#' 
#' There are also some data quality issues we need to address:
#' 
#' 1. Some teams did not play a game in the previous season so they don't have 
#' a team strength estimate  
#' 2. Some teams we don't have any recruit information on
#' 3. Some teams have 0 recruit class points or had only 1 or 2 recruits signed. 
#' 
#' We also need to plan how we want to use our data to build a model. We have 10
#' seasons of data to use. Of course this shrinks if we want to include multiple
#' years of past data in our model. Also the 2015 season does not have include
#' neutral site information which makes me want to throw it out. That means we
#' should use the 2013-2014 seasons as validation with the 2006-2012 seasons to
#' train our models. 
#' 
#+
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(viridis)
library(tidyr)
library(purrr)
library(broom)
options(dplyr.width = Inf)

past_schedules <- read_csv(file = "Datasets/cfb_schedule_05_15.csv",
                         col_types = cols(Date = col_date(format = "%b %d, %Y"))) %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Points, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))
  
preseason_info <- read_csv(file = "Datasets/team_preseason_info.csv") 

class_draft <- select(preseason_info, Year, Team, Class_Points, Points)

team_draft_recruit_info <- class_draft %>%
  inner_join(mutate(class_draft, Year = Year + 1), by = c("Year", "Team")) %>%
  inner_join(mutate(class_draft, Year = Year + 2), by = c("Year", "Team")) %>%
  filter(Year %in% seq(2006, 2012)) %>%
  rename(Recruit_n = Class_Points.x,
         Draft_n = Points.x,
         Recruit_n_1 = Class_Points.y,
         Draft_n_1 = Points.y,
         Recruit_n_2 = Class_Points,
         Draft_n_2 = Points)

team_draft_recruit_info %>%
  gather(Recruit_Years, Recruit_Points, -Year, -Team, -contains("Draft")) %>%
  gather(Draft_Years, Draft_Points, -Year, -Team, -Recruit_Years, -Recruit_Points) %>%
  filter(!is.na(Recruit_Points)) %>%
  ggplot(aes(x = Recruit_Points, y = Draft_Points)) +
  geom_point() +
  geom_smooth() +
  facet_grid(Recruit_Years ~ Draft_Years, switch = "y")

draft_recruit_pca <- prcomp(~ . - Year - Team, data = filter(team_draft_recruit_info, !is.na(Recruit_n)), center = T, scale = T)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(draft_recruit_pca$rotation, variable = row.names(draft_recruit_pca$rotation))
p + geom_text(data = loadings, aes(x = PC1, y = PC2, label = variable, colour = variable)) +
  coord_fixed(ratio = 1) +
  labs(x = "PC1", y = "PC2")



team_game_info <- past_schedules %>%
  filter(Season != 2005) %>%
  select(Season, Home, Away, Home_MOV, Neutral) %>%
  left_join(preseason_info, by = c("Home" = "Team", "Season" = "Year")) %>%
  left_join(preseason_info, by = c("Away" = "Team", "Season" = "Year"))



ggplot(aes(x = Class_Points.x, y = Class_Points.y, color = factor(Home_MOV > 0)), data = team_game_info) +
  geom_point(alpha = .6) +
  scale_color_viridis(option = "C", discrete = T) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  guide_legend(title = "Home Team Wins") +
  xlab("Home Team Freshman Class Points") +
  ylab("Away Team Freshman Class Points") +
  ggtitle("Affect of Incoming Class on Winning a Game")

