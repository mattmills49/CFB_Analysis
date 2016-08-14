#' # Developing Model Attributes
#' 
#' We need to combine information about team strength, recruiting talent, and
#' the talent lost to the draft for each team going in to the next season. 
#' 
#' ### Team Strength
#' 
#' I'm going to use the same method from my team_strength post.
#' 
#+
library(readr) 
library(purrr)  
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
options(dplyr.width = Inf)

raw_schedule <- read_csv(file = "Datasets/cfb_schedule_05_15.csv",
                         col_types = cols(Date = col_date(format = "%b %d, %Y")))

results <- raw_schedule %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Points, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))

organize_game_results <- function(df, ...){
  home_teams <- df %>%
    select(Season, Game_Num, Home, Home_MOV, ...) %>% 
    mutate(involved = 1) %>% 
    rename(Team = Home)
  away_teams <- df %>%
    select(Season, Game_Num, Away, Home_MOV, ...) %>% 
    mutate(involved = -1) %>% 
    rename(Team = Away)
  return(bind_rows(home_teams, away_teams))
}

season_team_values <- results %>%
  organize_game_results %>%
  group_by(Season) %>%
  nest() %>% 
  mutate(regression_df = map(data, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num, data = .)),
         team_values = map(model, tidy)) %>%
  unnest(team_values) %>%
  mutate(term = str_replace_all(term, fixed("`"), "")) %>%
  filter(term != "(Intercept)") %>%
  group_by(Season) %>%
  mutate(value = estimate - mean(estimate)) %>%
  ungroup

# head(select(season_team_values, Season, term, value))
#   Season               term     value
#    <dbl>              <chr>     <dbl>
# 1   2005          Air Force  2.970405
# 2   2005              Akron -1.147388
# 3   2005            Alabama 18.109597
# 4   2005 Alabama-Birmingham  2.343557
# 5   2005  Appalachian State -5.513553
# 6   2005            Arizona 11.464412
#' It's important to note that this is the rankings after the season is 
#' complete. So the 2005 rankings go in to the 2006 season. 
#' 
#' ### Recruit Rankings
#' 
#+

recruit_ranks <- read_csv(file = "Datasets/247_recruit_rankings_05_16.csv")
# head(recruit_ranks)
#            Team Recruits Class_Points Conference  Year
#           <chr>    <int>        <dbl>      <chr> <int>
# 1 Florida State       20       270.91        ACC  2005
# 2         Miami       15       237.42        ACC  2005
# 3      Virginia       23       205.09        ACC  2005
# 4       Clemson       25       202.65        ACC  2005
# 5 Virginia Tech       23       200.45        ACC  2005
# 6    N.C. State       19       182.28        ACC  2005

#' These are the recruit rankings in the February of the year, so the 2005
#' rankings go in to the 2005 season. 
#' 
#' ### Draft Rankings
#' 
#+
draft_picks <- read_csv(file = "Datasets/nfl_draft_05_16.csv")
names(draft_picks)[ncol(draft_picks)] <- "College"
draft_picks$College <- str_replace_all(draft_picks$College, fixed("St."), "State")

pick_value <- read_csv("~/Documents/CFB_Analysis/Datasets/DraftValue.csv") 

team_draft_info <- left_join(draft_picks, pick_value, by = c("Pick")) %>%
  arrange(Year, Pick) %>%
  fill(Draft_Value) %>%
  group_by(Year, College) %>%
  summarize(Picks = n(), Points = sum(Draft_Value), QB = 1 * (any(Pos == "QB"))) %>%
  ungroup

# head(arrange(team_draft_info, desc(Points)))
#    Year     College Picks Points    QB
#   <int>       <chr> <int>  <dbl> <dbl>
# 1  2016    Ohio St.    12  151.2     1
# 2  2010    Oklahoma     7  112.9     1
# 3  2006    Ohio St.     9  111.5     0
# 4  2008         USC    10  107.1     1
# 5  2006         USC    11  102.8     1
# 6  2015 Florida St.    11  101.8     1

#' The draft Year is the April/May of the year, so the 2005 rankings go in to 
#' the 2005 season. 
#' 
#' ### Combine them
#+

team_names <- read_csv(file = "Datasets/team_name_master_list.csv")

season_team_values$Year <- season_team_values$Season + 1

team_preseason_info <- left_join(select(season_team_values, Year, term, value), team_names, by = c("term" = "Team_Schedule")) %>%
  left_join(recruit_ranks, by = c("Year" = "Year", "Team_Recruit" = "Team")) %>%
  left_join(team_draft_info, by = c("Year" = "Year", "Team_Draft" = "College")) %>%
  select(-term, -Team_Recruit, -Team_Draft) %>%
  mutate(Picks = ifelse(is.na(Picks), 0, Picks),
         Points = ifelse(is.na(Points), 0, Points),
         QB = ifelse(is.na(QB), 0, QB)) %>%
  rename(Team_Strengh_Last_Year = value,
         QB_Drafted = QB)

  
# head(arrange(filter(team_preseason_info, Team == "Florida State"), desc(Year)))
#    Year Team_Strengh_Last_Year          Team Recruits Class_Points
#   <dbl>                  <dbl>         <chr>    <int>        <dbl>
# 1  2016               28.41029 Florida State       25       295.44
# 2  2015               24.22463 Florida State       20       285.69
# 3  2014               48.43902 Florida State       29       286.77
# 4  2013               27.60563 Florida State       22       262.44
# 5  2012               21.33503 Florida State       19       287.73
# 6  2011               24.89470 Florida State       29       296.41
#   Conference Picks Points QB_Drafted
#        <chr> <dbl>  <dbl>      <dbl>
# 1        ACC     2   32.9          0
# 2        ACC    11  101.8          1
# 3        ACC     7   54.2          0
# 4        ACC    11   75.9          1
# 5        ACC     4    9.8          0
# 6        ACC     3   28.0          1

write_csv(team_preseason_info, path = "Datasets/team_preseason_info.csv")
