library(rjson)
library(purrr)
library(stringr)
library(magrittr)

test_file <- fromJSON(file = "~/Documents/CFB/Clean PBP JSONs/2016_PBP/Week 1/full/400868979 - FSU vs Ole Miss.json")

plays <- lapply(test_file$drives, function(x){
  lapply(x, function(y){
    lapply(y$plays, function(z){
      z[["text"]]
    })
  })
})

play2 <- flatten_chr(flatten(flatten(plays)))

run_lgl <- str_detect(play2, "run|rush")
pass_lgl <- str_detect(play2, "pass")

get_drive_info <- function(drive_list){
  drive_info <- data.frame(drive_id = drive_list[["id"]], stringsAsFactors = F)
  drive_info$start_quarter <- drive_list[["start"]][["period"]][["number"]]
  drive_info$start_spot <- drive_list[["start"]][["yardLine"]]
  drive_info$start_time <- drive_list[["start"]][["clock"]][["displayValue"]]
  drive_info$start_text <- drive_list[["start"]][["text"]]
  drive_info$offense <- drive_list[["team"]][["shortDisplayName"]]
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

parse_play_info <- function(play_list){
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

get_play_info <- function(drive_list){
  map_dfr(drive_list[["plays"]], parse_play_info)
}

get_team_info <- function(team_list){
  team_info <- data.frame(team_id = team_list[["id"]], stringsAsFactors = F)
  team_info$full_name <- team_list[["team"]][["displayName"]]
  team_info$name <- team_list[["team"]][["name"]]
  team_info$nickname <- team_list[["team"]][["nickname"]]
  team_info$abbreviation <- team_list[["team"]][["abbreviation"]]
  team_info$home_away <- team_list[["homeAway"]]
  return(team_info)
}

get_team_colors <- function(team_list){
  team_info <- data.frame(team_id = team_list[["id"]], stringsAsFactors = F)
  team_info$color <- team_list[["team"]][["color"]]
  team_info$alternate <- team_list[["team"]][["alternateColor"]]
  team_info$logo <- team_list[["team"]][["logos"]][[1]][["href"]]
  return(team_info)
}

parse_file <- function(json_file){
  drives <- json_file %>%
    extract2("drives") %>%
    extract2("previous")
  
  drive_info <- map_dfr(drives, get_drive_info)
  play_info <- map_dfr(drives, get_play_info)
  team_info <- map_dfr(json_file[["teams"]], get_team_info)
  color_info <- map_dfr(json_file[["teams"]], get_team_colors)
  
  team_locations <- map_chr(json_file[["teams"]], function(x) x[["homeAway"]])
  team_scores <- map_chr(json_file[["teams"]], function(x) x[["score"]]) %>% as.integer
  team_ids <- map_chr(json_file[["teams"]], function(x) x[["id"]])
  names(team_ids) <- team_locations
  names(team_scores) <- team_locations
  
  game_info <- data.frame(game_id = json_file[["id"]])
  game_info$season <- json_file[["season"]][["year"]]
  game_info$season_type <- json_file[["season"]][["type"]]
  game_info$week <- json_file[["week"]]
  game_info$date <- json_file[["competitions"]][[1]][["date"]]
  game_info$home_team <- team_ids["home"]
  game_info$away_team <- team_ids["away"]
  game_info$home_score <- team_scores["home"]
  game_info$away_score <- team_scores["away"]
  game_info$neutral_site <- json_file[["competitions"]][[1]][["neutralSite"]]
  
  conf_info <- list()
  if(!is.null(json_file[["competitions"]][[1]][["groups"]])){
    conf_info <- team_info
    conf_info$conference <- json_file[["competitions"]][[1]][["groups"]][["name"]]
    conf_info$conf_name <- json_file[["competitions"]][[1]][["groups"]][["shortName"]]
  }
  
  return(list(drive_info, play_info, team_info, color_info, game_info, conf_info))
}

week1_files <- list.files(path = "~/Documents/CFB/Clean PBP JSONs/2016_PBP/Week 1/full/", full.names = T)

safe_parse <- safely(parse_file)
all_games <- map(week1_files, ~ fromJSON(file = .x)) %>%
  map(safe_parse)

test_errors <- map_lgl(all_games, function(x) is.null(x[["error"]]))
