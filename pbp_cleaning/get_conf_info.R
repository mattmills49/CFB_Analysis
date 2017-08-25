#' # Team Conference Info
#' 
#' We want the following information at a team level:
#' 
#' - ID
#' - Conference
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

get_conf_info <- function(json_list){
  teams <- json_list[["teams"]]
  
  team_info <- map_dfr(teams, extract_team_info)
  conf_info <- data.frame(team_id = team_info$team_id, stringsAsFactors = F)
  conf_info$conference <- json_list[["competitions"]][[1]][["groups"]][["name"]]
  conf_info$abbreviation <- json_list[["competitions"]][[1]][["groups"]][["shortName"]]
  
  return(conf_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/conference_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x)) %>%
    discard(~ is.null(.x[["competitions"]][[1]][["groups"]]))
  
  team_conf_info <- game_files %>%
    map_dfr(get_conf_info) %>%
    distinct()
  #   team_id               conference abbreviation
  # 1      36 Mountain West Conference           MW
  # 2    2440 Mountain West Conference           MW
  # 3     333  Southeastern Conference          SEC
  saved_file <- sprintf(save_files, y) %>%
    saveRDS(team_conf_info, file = .)
}