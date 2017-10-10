#' # Run/Pass Info
#' 
#' The biggest issue with this is going to be parsing the play types and 
#' descriptions from the larger play file. 
#' 
#' We have a `type` and a `desc` field. Some problems:
#' 
#' Steps:
#' 
#' 1. Filter out extra descriptions in an extra column
#'   + those that have extra desc in parenthesis
#' 2. Go through the types to classify them
#' 
#' - PATs are included in the description. So if the play scores there is this
#' extra information which may containt information about a play. 
#+
library(dplyr)
library(stringr)
library(purrr)
years <- 2015:2016
plays <- map2_chr("~/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/play_info.rds", years, sprintf) %>%
  map_dfr(readRDS)

plays$extra <- plays$desc %>%
  str_extract_all("\\(.*\\)", simplify = T) %>% 
  drop

plays$clean_desc <- plays$desc %>%
  str_replace_all("\\(.*\\)", "") %>%
  str_trim()

desc_type <- case_when(
  str_detect(tolower(plays$clean_desc), "run") ~ "Run",
  str_detect(tolower(plays$clean_desc), "rush") ~ "Rush",
  str_detect(tolower(plays$clean_desc), "pass") ~ "Pass",
  str_detect(tolower(plays$clean_desc), "sack") ~ "sack",
  TRUE ~ "Other")

desc_table <- table(type = plays$type, desc = desc_type)

play_types <- with(plays, case_when(
  str_detect(type, "Fumble Recovery") & str_detect(desc, "rush | run") ~ "rush",
  str_detect(type, "Fumble Recovery") & str_detect(desc, "pass | sack") ~ "pass"
)


play_types <- plays %>%
  group_by(type) %>% 
  mutate(n = n()) %>%
  select(type, n, desc) %>%
  sample_n(1) %>%
  arrange(desc(n))

head(play_types)

common_words <- plays %>%
  with(str_split(desc, pattern = " ")) %>%
  flatten_chr %>%
  table %>%
  sort(decreasing = T) %>%
  tibble::enframe(name = "Word", value = "Count")

head(common_words)

#' ### Assumptions
#' 
#' 1. If `desc` contains `rush` or `run` it is a `Run` `type`
#+
plays %>%
  filter(str_detect(desc, pattern = "run | rush")) %>%
  count(type) %>%
  arrange(desc(n)) %>%
  tail

plays %>%
  filter(!str_detect(desc, pattern = "run | rush")) %>%
  count(type) %>%
  arrange(desc(n)) %>%
  tail
#' 
#' #' So it looks like there are very few plays that are runs according to the 
#' `desc`` but not `type` and vice-versa. 
#' 
#' 2. if `sack` appears in the `desc` it is a `sack` `type`
#+
plays %>%
  filter(str_detect(desc, pattern = "sack")) %>%
  count(type)

plays %>%
  filter(type == "Sack") %>%
  summarize(contain_sack = mean(str_detect(desc, pattern = "sack"), na.rm = T))

#' 
#' So some of the sacks are also listed as fumbles, penalties, safeties, but all
#' sacks are sacks. 
#' 
#' So I think we can seperate the data into two pieces of information; a type
#' and result. So the type will be run, pass, kick, etc... and the result will
#' be fumble, int, sack, etc...
#' 
#' Problems:
#' 
#' - Blocked Punt

#'
#+


run_words <- plays %>%
  filter(str_detect(desc, "rush | run")) %>%
  with(str_split(desc, pattern = " ")) %>%
  flatten_chr %>%
  table %>%
  sort(decreasing = T) %>%
  tibble::enframe(name = "Word", value = "Count")

plays %>%
  filter(str_detect(desc, "rush | run")) %>%
  filter(str_detect(desc, "KICK")) %>% View





#+
library(purrr)
library(magrittr)
library(rjson)
library(dplyr)

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