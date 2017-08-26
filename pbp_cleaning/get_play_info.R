#' # Play Info
#' 
#' Ok this one is going to be a little tricky. Each play contains a lot of info
#' so we want to both capture as much as we can but also keep the files succinct.
#' I think we are going to have 3 files; play info, run info, and pass info. The
#' play info will contain some simple summary info:
#' 
#' - ID
#' - Spot
#' - Down
#' - Distance
#' - Quarter
#' - Time
#' - Offense
#' - Defense
#' - Current Score
#' - Yards Gained
#' - Play Type
#' - Result?
#' 
#' And then we can have more info in the run/pass files like;
#' 
#' - Sack
#' - Completed/Incomplete
#' - Int
#' - TD
#' - QB
#' - Receiver
#' 
#' and
#' 
#' - Fumble
#' - TD
#' - Runner
#+

extract_play_info <- function(play_list){
  play_info <- data.frame(play_id = play_list[["id"]], stringsAsFactors = F)
  play_info$quarter <- play_list[["period"]]
  play_info$home_score <- play_list[["homeScore"]]
  play_info$away_score <- play_list[["awayScore"]]
  play_info$type <- play_list[["type"]][["text"]]
  play_info$start_clock <- play_list[["clock"]][["displayValue"]]
  play_info$start_down <- play_list[["start"]][["down"]]
  play_info$start_distance <- play_list[["start"]][["distance"]]
  play_info$start_spot <- play_list[["start"]][["yardLine"]]
  play_info$start_team <- play_list[["start"]][["team"]][["id"]]
  play_info$start_yards_to_endzone <- play_list[["start"]][["yardsToEndzone"]]
  play_info$end_down <- play_list[["end"]][["down"]]
  play_info$end_distance <- play_list[["end"]][["distance"]]
  play_info$end_spot <- play_list[["end"]][["yardLine"]]
  play_info$end_team <- play_list[["end"]][["team"]][["id"]]
  play_info$end_yards_to_endzone <- play_list[["end"]][["yardsToEndzone"]]
  play_info$yards_gained <- play_list[["statYardage"]]
  play_info$desc <- play_list[["text"]]
  return(play_info)
}

extract_drive_id <- function(drive_list){
  play_info <- map_dfr(drive_list[["plays"]], extract_play_info)
  play_info$drive_id <- drive_list[["id"]]
  return(play_info)
}

get_play_info <- function(json_list){
  drives <- json_list %>%
    extract2("drives") %>%
    extract2("previous")
  
  play_info <- drives %>%
    map_dfr(extract_drive_id)
  
  play_info$game_id <- json_list[["id"]]
  return(play_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/play_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x))
  
  play_info <- game_files %>%
    map_dfr(get_play_info)
  
  saved_file <- sprintf(save_files, y) %>%
    saveRDS(play_info, file = .)
}