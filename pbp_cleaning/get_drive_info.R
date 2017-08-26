#' # Drive Info
#' 
#' We want the following information at a drive level:
#' 
#' - ID
#' - Offense Team
#' - Yards Gained
#' - Time Elapsed
#' - Result
#' - Number of Plays
#' - Game ID
#' 
#' And then the starting and ending:
#' 
#' - Quarter
#' - Time
#' - Spot
#' 
#+
library(purrr)
library(magrittr)
library(rjson)
library(dplyr)

extract_drive_info <- function(drive_list){
  drive_info <- data.frame(drive_id = drive_list[["id"]], stringsAsFactors = F)
  drive_info$start_quarter <- drive_list[["start"]][["period"]][["number"]]
  drive_info$start_spot <- drive_list[["start"]][["yardLine"]]
  drive_info$start_time <- drive_list[["start"]][["clock"]][["displayValue"]]
  drive_info$start_text <- drive_list[["start"]][["text"]]
  drive_info$offense <- drive_list[["team"]][["nickname"]]
  drive_info$description <- drive_list[["description"]]
  drive_info$yards_gained <- drive_list[["yards"]]
  drive_info$time_elapsed <- drive_list[["timeElapsed"]]
  drive_info$result <- drive_list[["result"]]
  drive_info$num_plays <- drive_list[["offensivePlays"]]
  drive_info$end_quarter <- drive_list[["end"]][["period"]][["number"]]
  drive_info$end_spot <- drive_list[["end"]][["yardLine"]]
  drive_info$end_time <- drive_list[["end"]][["clock"]][["displayValue"]]
  drive_info$end_text <- drive_list[["end"]][["text"]]
  return(drive_info)
}

get_drive_info <- function(json_file){
  drives <- json_file %>%
    extract2("drives") %>%
    extract2("previous")
  
  drive_info <- map_dfr(drives, extract_drive_info)
  drive_info$game_id <- json_file[["id"]]
  return(drive_info)
}

file_locations <- "/Users/MM/Documents/CFB/Clean PBP JSONs/%i_PBP"

save_files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/drive_info.rds"

years <- 2015:2016

for(y in years){
  
  all_files <- sprintf(file_locations, y) %>%
    list.files(full.names = T) %>%
    map_chr(~ sprintf("%s/full", .x)) %>%
    map(list.files, full.names = T) %>%
    flatten_chr
  
  game_files <- all_files %>%
    map(~ fromJSON(file = .x))
  
  drive_info <- game_files %>%
    map_dfr(get_drive_info)
  #     drive_id start_quarter start_spot start_time start_text             description 
  # 1 4008526681             1         15      15:00     UNM 15  3 plays, 7 yards, 2:04 
  # 2 4008526682             1         70      12:56    ARIZ 30  2 plays, 7 yards, 0:19 
  # 3 4008526683             1         63      12:37    ARIZ 37 5 plays, 17 yards, 2:45 
  # yards_gained time_elapsed result num_plays end_quarter end_spot end_time end_text   game_id
  #            7         2:04   PUNT         3           1       22    12:56   UNM 22 400852668
  #            7         0:19 FUMBLE         2           1       63    12:37  ARIZ 37 400852668
  #           17         2:45     FG         5           1       80     9:52  ARIZ 20 400852668
  
  saved_file <- sprintf(save_files, y) %>%
    saveRDS(drive_info, file = .)
}