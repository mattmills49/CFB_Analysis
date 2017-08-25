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
  team_names <- map_chr(json_file[["teams"]], function(x) x[["team"]][["nickname"]])
  names(team_names) <- team_locations
  names(team_scores) <- team_locations
  
  game_info <- data.frame(game_id = json_file[["id"]], stringsAsFactors = F)
  game_info$home_team <- team_names["home"]
  game_info$away_team <- team_names["away"]
  game_info$home_score <- team_scores["home"]
  game_info$away_score <- team_scores["away"]
  game_info$neutral_site <- json_file[["competitions"]][[1]][["neutralSite"]]
  game_info$date <- json_file[["competitions"]][[1]][["date"]]
  game_info$week <- json_file[["week"]]
  game_info$season <- json_file[["season"]][["year"]]
  game_info$season_type <- json_file[["season"]][["type"]]
  
  game_info$week[game_info$season_type == 3] <- 16
  
  game_info$date <- as.POSIXct(game_info$date, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
  
  return(game_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/game_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x))
  
  game_info <- game_files %>%
    map_dfr(get_game_info)
  #     game_id     home_team      away_team home_score away_score neutral_site
  # 1 400852668    New Mexico        Arizona         37         45         TRUE
  # 2 400852683 Georgia State San Jose State         16         27         TRUE
  # 3 400852684          Utah            BYU         35         28         TRUE
  #                date week season season_type
  # 2015-12-19 19:00:00   16   2015           3
  # 2015-12-20 00:00:00   16   2015           3
  # 2015-12-19 20:30:00   16   2015           3

  saved_file <- sprintf(save_files, y) %>%
    saveRDS(game_info, file = .)
}
