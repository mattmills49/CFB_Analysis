#' # CFB Point Differential Charts
#'
#+
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
options(dplyr.width = Inf)
source("~/Documents/CFB Data/WinProbPoster/Win_Prob_Shiny/data/AddTime.r")

play_df <- read_csv(file = "~/Dropbox/2015week14/play.csv")
conf_df <- read_csv(file = "~/Dropbox/2015week14/conference.csv")
team_df <- read_csv(file = "~/Dropbox/2015week14/team.csv")

names(play_df) <- str_replace_all(names(play_df), " ", "_")
names(conf_df) <- str_replace_all(names(conf_df), " ", "_")
names(team_df) <- str_replace_all(names(team_df), " ", "_")
names(team_df)[2] <- "Team_Name"
names(conf_df)[2] <- "Conf_Name"

play_df <- AddTime(play_df)
play_df <- select(play_df, -scoreTextVector, -driveTextVector)

play_df <- left_join(play_df, team_df, by = c("Offense_Team_Code" = "Team_Code")) %>%
  left_join(team_df, by = c("Defense_Team_Code" = "Team_Code"))
play_df <- left_join(play_df, conf_df, by = c("Conference_Code.x" = "Conference_Code")) %>%
  left_join(conf_df, by = c("Conference_Code.y" = "Conference_Code"))

names(play_df) <- str_replace_all(names(play_df), fixed(".x"), "_Off") %>%
  str_replace_all(fixed(".y"), "_Def")

filter(play_df, Team_Name_Off == "Florida State") %>%
  mutate(Time = New.Clock + (4 - Period_Number) * 900,
         Point_Diff = Offense_Points - Defense_Points) %>%
  ggplot(aes(x = Time, y = Point_Diff, group = Team_Name_Def, color = Point_Diff)) +
  geom_line()

