#' # Team Info
#' 
#' We want the following information at a team level:
#' 
#' - ID
#' - Full Name
#' - Nickname
#' - Abbreviation
#+ 
library(purrr)
library(magrittr)
library(rjson)
library(dplyr)

extract_team_info <- function(team_list){
  team_info <- data.frame(team_id = team_list[["id"]], stringsAsFactors = F)
  team_info$full_name <- team_list[["team"]][["displayName"]]
  team_info$name <- team_list[["team"]][["name"]]
  team_info$nickname <- team_list[["team"]][["nickname"]]
  team_info$abbreviation <- team_list[["team"]][["abbreviation"]]
  return(team_info)
}

get_team_info <- function(json_list){
  teams <- json_list[["teams"]]
  
  team_info <- map_dfr(teams, extract_team_info)
  return(team_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/team_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x))
  
  team_info <- game_files %>%
    map_dfr(get_team_info) %>%
    distinct()
  #   team_id                  full_name        name        nickname abbreviation
  # 1    2636 UT San Antonio Roadrunners Roadrunners            UTSA         UTSA
  # 2     167           New Mexico Lobos       Lobos      New Mexico          UNM
  # 3      21     San Diego State Aztecs      Aztecs San Diego State         SDSU
  
  saved_file <- sprintf(save_files, y) %>%
    saveRDS(team_info, file = .)
}