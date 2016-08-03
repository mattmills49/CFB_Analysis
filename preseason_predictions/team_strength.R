#' # Generate Team Quality Metrics
#' 
#' In order to build a prediction model for next season I need to know how good
#' each team was in the previous season(s). A decent and simple way to measure
#' team quality is to use the [Massey rankings](http://public.gettysburg.edu/~cwessell/RankingPage/massey.pdf), which basically just finds the
#' points above average that each team contributes to the margin of victory of 
#' each game using [Ordinary Least Squares Regression])https://en.wikipedia.org/wiki/Ordinary_least_squares). We will get into more complicated measures later on in this post. 
#' 
#' To do this, for each season, I need to organize the data so that each row 
#' corresponds to a game. The dependent variable (what we are trying to predict)
#' is the margin of victory from the view of the home team. The 
#' independent variables needed will be dummay variables indicating who is home 
#' (using a + 1) and who is away (using a -1). Normally you wold also include a
#' dummy variable for neutral site games, however the 2015 season on CFB 
#' Reference doesn't list any info on neutral site games. So even though we have
#' this information for every other season I'm going to have to exclude it since
#' the overall goal of this project is to predict the coming seasons results, so 
#' we can't just ignore we don't have it for this year.
#' 
#' Let's get started by reading in the data I've pulled from CFB Reference that
#' includes the results of every FBS football game over the last 10 seasons.
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

raw_schedule <- read_csv(file = "Datasets/cfb_schedule_05_15.csv"
                    col_types = cols(Date = col_date(format = "%b %d, %Y")))

## Need to add the points for the home and away teams and find what season each
## game belongs to

results <- raw_schedule %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Points, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))

#+ echo = F
knitr::kable(select(results, Season, Week, Home, Away, Home_Points, Away_Points, Home_MOV))

#' Now comes the tricky part. I need to turn this long data frame of game
#' results into a wide format that includes the team names as variable names.
#' I've written the following helper function to "stack" the teams in each 
#' game so that we don't have to worry about having two team name columns.

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

#' I'm going to do a bit of R Kung Fu but here is my basic approach:
#' * ["Nest"](https://blog.rstudio.org/2016/02/02/tidyr-0-4-0/) the stacked team
#' data so that the data is grouped by Season. Since each season's data is just
#' stored as a list in a column of the data frame we can use `purrr`'s
#' `map` function to apply functions to each season of data seperately.
#' * Use the `spread` function from the `tidyr` package to go from a tall to 
#' wide format
#' * Use a linear regression to calculate the Massey Rankings
#' * Use the `tidy` function from the `broom` package to extract the coefficient
#' values (which are the actual team rankings)
#' * Then unnest each season to get one data frame for our results

season_team_values <- results %>%
  organize_game_results %>%
  group_by(Season) %>%
  nest() %>% 
  mutate(regression_df = map(data, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num, data = .)),
         team_values = map(model, tidy)) %>%
  unnest(team_values) %>%
  mutate(Team = str_replace_all(term, fixed("`"), ""))

knitr::kable(head(season_team_values))

#' Hopefully that wasn't too complicated
#' 
#' ### Problems
#' 
#' If you inspect the results you'll see that there are some areas where we could
#' improve our results. Here is a list off the top of my head:
#' 
#' 1. We don't adjust for blowouts at all
#' 2. We don't adjust for teams who only play 1 or 2 games all season. For
#' example, Portland State beat two very bad teams in 2015 but those were their
#' only games so they are rated as one of the best teams in the country. We 
#' could just consider all teams who only showed up 1-2 times as one team, sort
#' of as a placeholder for "FCS" teams (the lower division of CFB).
#' 3. We don't regularize our results at all
#' 4. We don't include any team priors from the previous season.
#' 
#' While I certainly believe all of these areas could help improve our results
#' I'd like a way to know for sure. Since the goal of this post is to determine
#' how good each team was in a season we can just test that. If our team ratings
#' accurately predict the results of games between teams then we can assume that
#' the ratings reflect the true quality of teams.
#' 
#' What we don't want to do is use the same underlying games to both generate and
#' test the accuracy of the team rankings, then we would be rewarding methods that
#' overfit to the season results and not the true underlying team quality. So we
#' will have to generate our team rankings and then test their accuracy on 
#' different sets of games. I'm going to do this using cross validation which is 
#' a popular way to test the accuracy of models while avoiding overfitting. 
#' 
#' Let's find the accuracy of this base method we have already used. 
#+ echo = F
safe_pred <- function(mod, df){
  coef_names <- coef(mod) %>%
    names %>%
    keep(~ .x != "(Intercept)") %>%
    str_replace_all(fixed("`"), "")
  
  col_names <- colnames(select(df, -Game_Num, -Home_MOV, -fold_id))
  
  ## add columns not in the test data
  
  missing_teams <- coef_names[!(coef_names %in% col_names)]
  extra_cols <- matrix(0, nrow = nrow(df), ncol = length(missing_teams))
  full_df <- bind_cols(df, data.frame(extra_cols))
  names(full_df)[seq(ncol(df) + 1, ncol(df) + length(missing_teams))] <- missing_teams
  
  ## remove columns not in the trained_model
  
  no_fit_teams <- col_names[!(col_names %in% coef_names)]
  good_data <- select(full_df, -one_of(no_fit_teams))
  good_rows <- rowSums(good_data[, -c(1:3)]) == 0
  good_data <- good_data[good_rows, ]
  
  good_data$pred <- suppressWarnings(predict(mod, newdata = good_data))
  return(good_data)
}

#+ 
K <- 4
cv_results <- results %>%
  mutate(fold_id = sample(1:K, size = n(), replace = T)) %>%
  organize_game_results(fold_id) %>% 
  group_by(Season) %>%
  nest() %>% 
  crossing(fold = 1:K) %>% 
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num - fold_id, data = .)),
         preds = map2(model, test_df, safe_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred)^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error))

mean(cv_results$mean_mse)

#' So that's our baseline Mean Squared Error.  




  


