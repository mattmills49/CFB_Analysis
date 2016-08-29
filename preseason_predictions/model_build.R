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
#' years of past data in our model. Also the 2015 season does not have 
#' neutral site information which makes me want to throw it out. That means we
#' should use the 2013-2014 seasons as validation with the 2006-2012 seasons to
#' train our models. The problem with this is we will still need the 2015 team
#' strength values to make predictions about 2016....hhmm. I guess it won't be
#' that big of a deal
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
library(helpers)
options(dplyr.width = Inf)

theme_mells <- theme_bw() + theme(axis.title = element_text(face = "bold"), title = element_text(face = "bold"))

past_schedules <- read_csv(file = "Datasets/cfb_schedule_05_15.csv",
                         col_types = cols(Date = col_date(format = "%b %d, %Y"))) %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Points, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))
  
preseason_info <- read_csv(file = "Datasets/team_preseason_info.csv") 

class_draft <- select(preseason_info, Year, Team, Class_Points, Points)
# organize the last three recruit and draft classes
team_draft_recruit_info <- class_draft %>%
  left_join(mutate(class_draft, Year = Year + 1), by = c("Year", "Team")) %>%
  left_join(mutate(class_draft, Year = Year + 2), by = c("Year", "Team")) %>%
  rename(Recruit_n = Class_Points.x,
         Draft_n = Points.x,
         Recruit_n_1 = Class_Points.y,
         Draft_n_1 = Points.y,
         Recruit_n_2 = Class_Points,
         Draft_n_2 = Points)

team_draft_recruit_info %>%
  filter(Year <= 2012, Year >= 2008) %>%
  gather(Recruit_Years, Recruit_Points, -Year, -Team, -contains("Draft")) %>%
  gather(Draft_Years, Draft_Points, -Year, -Team, -Recruit_Years, -Recruit_Points) %>%
  filter(!is.na(Recruit_Points)) %>%
  ggplot(aes(x = Recruit_Points, y = Draft_Points)) +
  geom_point() +
  geom_smooth() +
  facet_grid(Recruit_Years ~ Draft_Years, switch = "y") + 
  xlab("Recruiting Points") +
  ylab("Draft Value Lost") +
  ggtitle("Relationship Between Draft Value and Recruiting Points") +
  theme_mells

draft_recruit_pca <- prcomp(~ . - Year - Team, data = team_draft_recruit_info, center = T, scale = T)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(draft_recruit_pca$rotation, variable = row.names(draft_recruit_pca$rotation))
p + geom_text(data = loadings, aes(x = PC1, y = PC2, label = variable, colour = PC3)) +
  coord_fixed(ratio = 1) +
  labs(x = "PC1", y = "PC2")

((as.matrix(tail(team_draft_recruit_info[, -c(1, 2)])) - matrix(draft_recruit_pca$center, nrow = 6, ncol = 6, byrow = T)) / matrix(draft_recruit_pca$scale, nrow = 6, ncol = 6, byrow = T)) %*% draft_recruit_pca$rotation

names(preseason_info)[2] <- "Team_Strength_Last_Year"
historic_team_strength <- select(preseason_info, Year, Team_Strength_Last_Year, Team)

team_strength_info <- historic_team_strength %>%
  left_join(mutate(historic_team_strength, Year = Year + 1), by = c("Year", "Team")) %>%
  left_join(mutate(historic_team_strength, Year = Year + 2), by = c("Year", "Team")) %>%
  left_join(mutate(historic_team_strength, Year = Year + 3), by = c("Year", "Team")) %>%
  rename(Team_Strength_n_1 = Team_Strength_Last_Year.x,
         Team_Strength_n_2 = Team_Strength_Last_Year.y,
         Team_Strength_n_3 = Team_Strength_Last_Year.x.x,
         Team_Strength_n_4 = Team_Strength_Last_Year.y.y)

team_strength_info %>% 
  filter(Year <= 2012) %>%
  gather(Year_Gap, SRS, -Team, -Year, -Team_Strength_n_1) %>%
  mutate(Year_label = factor(Year_Gap, labels = c("1 Season Ago", "2 Seasons Ago", "3 Seasons Ago"))) %>%
  ggplot(aes(x = SRS, y = Team_Strength_n_1)) +
  geom_point() +
  geom_smooth(size = 2) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~Year_label) +
  xlab("SRS in Previous Seasons") +
  ylab("Team SRS") +
  ggtitle("Team Strength Relationship Between Seasons") +
  theme_mells

# Missing Values

ggplot(aes(x = Team_Strength_n_1), data = filter(team_strength_info, Year <= 2012)) +
  geom_histogram(color = "black", binwidth = 5, boundary = 0) +
  theme_mells

filter(team_strength_info, is.na(Team_Strength_n_2), Year > 2006, Year <= 2012) %>%
  summarize(mean_strength = mean(Team_Strength_n_1))
# Missing Team Strength means -21.25
filter(team_strength_info, is.na(Team_Strength_n_2), !is.na(Team_Strength_n_3), Year <= 2012) %>% summarize(mean_3 = mean(Team_Strength_n_3), n = n())
# Missing previous season = -21.759

filter(team_draft_recruit_info, Year > 2006, Year <= 2012) %>% 
  ggplot(aes(x = Recruit_n)) +
  geom_histogram()

cleaned_team_info <- left_join(team_strength_info, team_draft_recruit_info, by = c("Year", "Team"))

model_data <- left_join(past_schedules, cleaned_team_info, by = c("Home" = "Team", "Season" = "Year")) %>%
  filter(Season > 2005) %>%
  left_join(cleaned_team_info, by = c("Away" = "Team", "Season" = "Year"))

names(model_data) <- str_replace_all(names(model_data), fixed(".x"), "_Home")
names(model_data) <- str_replace_all(names(model_data), fixed(".y"), "_Away")

model_data <- model_data %>%
  mutate(Team_Strength_n_1_Home = ifelse(is.na(Team_Strength_n_1_Home), -21.5, Team_Strength_n_1_Home),
         Team_Strength_n_2_Home = ifelse(is.na(Team_Strength_n_2_Home), -21.5, Team_Strength_n_2_Home),
         Team_Strength_n_3_Home = ifelse(is.na(Team_Strength_n_3_Home), -21.5, Team_Strength_n_3_Home),
         Team_Strength_n_1_Away = ifelse(is.na(Team_Strength_n_1_Away), -21.5, Team_Strength_n_1_Away),
         Team_Strength_n_2_Away = ifelse(is.na(Team_Strength_n_2_Away), -21.5, Team_Strength_n_2_Away),
         Team_Strength_n_3_Away = ifelse(is.na(Team_Strength_n_3_Away), -21.5, Team_Strength_n_3_Away),
         Recruit_n_Home = ifelse(Recruit_n_Home < 100 | is.na(Recruit_n_Home), 100, Recruit_n_Home),
         Recruit_n_1_Home = ifelse(Recruit_n_1_Home < 100 | is.na(Recruit_n_1_Home), 100, Recruit_n_1_Home),
         Recruit_n_2_Home = ifelse(Recruit_n_2_Home < 100 | is.na(Recruit_n_2_Home), 100, Recruit_n_2_Home),
         Recruit_n_Away = ifelse(Recruit_n_Away < 100 | is.na(Recruit_n_Away), 100, Recruit_n_Away),
         Recruit_n_1_Away = ifelse(Recruit_n_1_Away < 100 | is.na(Recruit_n_1_Away), 100, Recruit_n_1_Away),
         Recruit_n_2_Away = ifelse(Recruit_n_2_Away < 100 | is.na(Recruit_n_2_Away), 100, Recruit_n_2_Away),
         Draft_n_Home = ifelse(is.na(Draft_n_Home), 0, Draft_n_Home),
         Draft_n_1_Home = ifelse(is.na(Draft_n_1_Home), 0, Draft_n_1_Home),
         Draft_n_2_Home = ifelse(is.na(Draft_n_2_Home), 0, Draft_n_2_Home),
         Draft_n_Away = ifelse(is.na(Draft_n_Away), 0, Draft_n_Away),
         Draft_n_1_Away = ifelse(is.na(Draft_n_1_Away), 0, Draft_n_1_Away),
         Draft_n_2_Away = ifelse(is.na(Draft_n_2_Away), 0, Draft_n_2_Away))

model_data$Train <- model_data$Season <= 2012
model_data$Home_win <- 1 * (model_data$Home_MOV > 0)

library(mgcv)

gam_formula <- function(dependent_var, independent_vars){
  vapply(independent_vars, function(x) str_c("s(", x, ")"), character(1)) %>%
    str_c(collapse = " + ") %>%
    str_c(dependent_var, " ~ ", .) %>%
    as.formula
}

game_gam <- gam(formula = Home_win ~ Neutral + s(Team_Strength_n_1_Home) + s(Team_Strength_n_2_Home) + 
    s(Team_Strength_n_3_Home) + s(Recruit_n_Home) + s(Draft_n_Home) + 
                s(Recruit_n_1_Home) + s(Draft_n_1_Home) + s(Recruit_n_2_Home) + 
                s(Draft_n_2_Home) + s(Team_Strength_n_1_Away) + s(Team_Strength_n_2_Away) + 
                s(Team_Strength_n_3_Away) + s(Recruit_n_Away) + s(Draft_n_Away) + 
                s(Recruit_n_1_Away) + s(Draft_n_1_Away) + s(Recruit_n_2_Away) + 
                s(Draft_n_2_Away),
                family = "binomial",
                data = filter(model_data, Train, Season >= 2008))

model_data$preds <- as.numeric(predict(game_gam, newdata = model_data, type = "response"))

with(filter(model_data, !Train), prop.table(table(preds > .5, Home_win), margin = 1))

model_data %>%
  filter(!Train) %>%
  mutate(pred_group = cut_width(preds, width = .1)) %>%
  group_by(pred_group) %>%
  summarize(win_pct = mean(Home_win), pred = mean(preds)) %>%
  ggplot(aes(x = pred, y = win_pct)) +
  geom_line(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_mells +
  scale_x_continuous(name = "Predicted Probability", labels = function(x) paste0(x * 100, "%")) +
  scale_y_continuous(name = "Actual Home Team Win %", labels = function(x) paste0(x * 100, "%")) +
  ggtitle("Calibration Plot for Preseason Prediction Model")

schedule_2016 <- read_csv("Datasets/cfb_schedule_16.csv")
schedule_2016$Season = 2016
  
pred_data <- left_join(schedule_2016, cleaned_team_info, by = c("Home" = "Team", "Season" = "Year")) %>%
  left_join(cleaned_team_info, by = c("Away" = "Team", "Season" = "Year"))

names(pred_data) <- str_replace_all(names(pred_data), fixed(".x"), "_Home")
names(pred_data) <- str_replace_all(names(pred_data), fixed(".y"), "_Away")

pred_data <- pred_data %>%
  mutate(Team_Strength_n_1_Home = ifelse(is.na(Team_Strength_n_1_Home), -21.5, Team_Strength_n_1_Home),
         Team_Strength_n_2_Home = ifelse(is.na(Team_Strength_n_2_Home), -21.5, Team_Strength_n_2_Home),
         Team_Strength_n_3_Home = ifelse(is.na(Team_Strength_n_3_Home), -21.5, Team_Strength_n_3_Home),
         Team_Strength_n_1_Away = ifelse(is.na(Team_Strength_n_1_Away), -21.5, Team_Strength_n_1_Away),
         Team_Strength_n_2_Away = ifelse(is.na(Team_Strength_n_2_Away), -21.5, Team_Strength_n_2_Away),
         Team_Strength_n_3_Away = ifelse(is.na(Team_Strength_n_3_Away), -21.5, Team_Strength_n_3_Away),
         Recruit_n_Home = ifelse(Recruit_n_Home < 100 | is.na(Recruit_n_Home), 100, Recruit_n_Home),
         Recruit_n_1_Home = ifelse(Recruit_n_1_Home < 100 | is.na(Recruit_n_1_Home), 100, Recruit_n_1_Home),
         Recruit_n_2_Home = ifelse(Recruit_n_2_Home < 100 | is.na(Recruit_n_2_Home), 100, Recruit_n_2_Home),
         Recruit_n_Away = ifelse(Recruit_n_Away < 100 | is.na(Recruit_n_Away), 100, Recruit_n_Away),
         Recruit_n_1_Away = ifelse(Recruit_n_1_Away < 100 | is.na(Recruit_n_1_Away), 100, Recruit_n_1_Away),
         Recruit_n_2_Away = ifelse(Recruit_n_2_Away < 100 | is.na(Recruit_n_2_Away), 100, Recruit_n_2_Away),
         Draft_n_Home = ifelse(is.na(Draft_n_Home), 0, Draft_n_Home),
         Draft_n_1_Home = ifelse(is.na(Draft_n_1_Home), 0, Draft_n_1_Home),
         Draft_n_2_Home = ifelse(is.na(Draft_n_2_Home), 0, Draft_n_2_Home),
         Draft_n_Away = ifelse(is.na(Draft_n_Away), 0, Draft_n_Away),
         Draft_n_1_Away = ifelse(is.na(Draft_n_1_Away), 0, Draft_n_1_Away),
         Draft_n_2_Away = ifelse(is.na(Draft_n_2_Away), 0, Draft_n_2_Away))

pred_data$preds <- as.numeric(predict(game_gam, newdata = pred_data, type = "response"))
pred_data <- select(pred_data, Home, Away, Week, preds, everything())
