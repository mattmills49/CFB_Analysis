#' # Game Info
#' 
#' We want the following information at a game level:
#' 
#' - ID
#' - Home Team
#' - Away Team
#' - Home Points
#' - Away Points
#' - Neutral Site
#' - Date
#' - Week
#' - Season
#' 
#+
library(purrr)
library(magrittr)
library(rjson)

get_game_info <- function(json_file){
  team_locations <- map_chr(json_file[["teams"]], function(x) x[["homeAway"]])
  team_scores <- map_chr(json_file[["teams"]], function(x) x[["score"]]) %>% as.integer
  team_ids <- map_chr(json_file[["teams"]], function(x) x[["id"]])
  names(team_ids) <- team_locations
  names(team_scores) <- team_locations
  
  game_info <- data.frame(game_id = json_file[["id"]], stringsAsFactors = F)
  game_info$home_team <- team_ids["home"]
  game_info$away_team <- team_ids["away"]
  game_info$home_score <- team_scores["home"]
  game_info$away_score <- team_scores["away"]
  game_info$neutral_site <- json_file[["competitions"]][[1]][["neutralSite"]]
  game_info$date <- json_file[["competitions"]][[1]][["date"]]
  game_info$week <- json_file[["week"]]
  game_info$season <- json_file[["season"]][["year"]]
  game_info$season_type <- json_file[["season"]][["type"]]
  
  return(game_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/game_info.rds"

years <- 2015:2016

all_files <- map2_chr(file_locations, years, sprintf) %>% 
  map(list.files, full.names = T) %>%
  map(~ sprintf("%s/full", .x)) %>%
  map(function(x){
    map(x, list.files, full.names = T) %>% flatten_chr
  })

game_files <- all_files %>%
  map(function(x){
    map(x, ~ fromJSON(file = .x))
  })

game_info <- game_files %>%
  map(function(x){
    map_dfr(x, get_game_info)
  })
# game_id home_team away_team home_score away_score neutral_site              date
# 1 400876038      2636       167         20         23         TRUE 2016-12-17T19:00Z
# 2 400876039        21       248         34         10         TRUE 2016-12-17T20:30Z
# 3 400876040      2649      2026         28         31         TRUE 2016-12-17T22:30Z
# week season season_type
# 1    1   2016           3
# 2    1   2016           3
# 3    1   2016           3

saved_files <- map2_chr(save_files, years, sprintf) %>%
  map2(game_info, ~ saveRDS(.y, file = .x))

