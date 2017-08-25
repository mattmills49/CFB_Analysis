#' # Team Info
#' 
#' We want the following information at a team level:
#' 
#' - ID
#' - Main Color
#' - Alt Color
#' - Logo URL
#+ 
library(purrr)
library(magrittr)
library(rjson)
library(dplyr)

extract_team_color_info <- function(team_list){
  team_info <- data.frame(team_id = team_list[["id"]], stringsAsFactors = F)
  team_info$color <- team_list[["team"]][["color"]]
  team_info$alternate <- team_list[["team"]][["alternateColor"]]
  team_info$logo <- team_list[["team"]][["logos"]][[1]][["href"]]
  
  return(team_info)
}

get_team_color_info <- function(json_list){
  teams <- json_list[["teams"]]
  
  team_info <- map_dfr(teams, extract_team_color_info)
  return(team_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/team_color_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x))
  
  team_color_info <- game_files %>%
    map_dfr(get_team_color_info) %>%
    distinct()
  #   team_id  color alternate                                               logo
  # 1     167 000000    231f20  http://a.espncdn.com/i/teamlogos/ncaa/500/167.png
  # 2      12 002449    00205b   http://a.espncdn.com/i/teamlogos/ncaa/500/12.png
  # 3    2247 1e539a    ebebeb http://a.espncdn.com/i/teamlogos/ncaa/500/2247.png
  
  saved_file <- sprintf(save_files, y) %>%
    saveRDS(team_color_info, file = .)
}