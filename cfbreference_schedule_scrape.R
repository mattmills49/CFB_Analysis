##############################################
#                                            #
# College Football Reference Schedule Scrape #
#                                            #
##############################################
library(rvest)
library(dplyr)

### 1. Get Old Schedules ###

base_url <- "http://www.sports-reference.com/cfb/years/%i-schedule.html"

year_list <- seq(from = 2005, to = 2015, by = 1)

schedule_list <- list()
for(year in year_list){
  year_url <- sprintf(base_url, year)
  year_schedule <- read_html(year_url) %>%
    html_node("#schedule") %>%
    html_table
  Sys.sleep(1)
  num_cols <- ncol(year_schedule)
  if (num_cols == 10) {
    names(year_schedule) <- c("Game_Num", "Week", "Date", "Day", "Winner", "Winner_Points", "Location", "Loser", "Loser_Points", "Notes")
  } else if (num_cols == 11) {
    names(year_schedule) <- c("Game_Num", "Week", "Date", "Time", "Day", "Winner", "Winner_Points", "Location", "Loser", "Loser_Points", "Notes")
  } else if (num_cols == 12) {
    names(year_schedule) <- c("Game_Num", "Week", "Date", "Time", "Day", "Winner", "Winner_Points", "Location", "Loser", "Loser_Points", "TV", "Notes")
  } else stop("Don't recognize number of columns")
  schedule_list[[as.character(year)]] <- year_schedule
}


year_names <- lapply(schedule_list, names)
first_names <- year_names[[1]]

schedule_df <- bind_rows(schedule_list, .id = "Season") %>%
  filter(Game_Num != "Rk") %>%
  mutate(Game_Num = as.numeric(Game_Num),
         Week = as.numeric(Week),
         Winner_Points = as.numeric(Winner_Points),
         Loser_Points = as.numeric(Loser_Points),
         Winner = str_replace_all(Winner, "[0-9]+", ""),
         Loser = str_replace_all(Loser, "[0-9]+", ""),
         Winner = str_trim(str_replace_all(Winner, fixed("()"), "")),
         Loser = str_trim(str_replace_all(Loser, fixed("()"), "")), 
         Home = ifelse(Location == "@", Loser, Winner),
         Away = ifelse(Location != "@", Loser, Winner),
         Neutral = 1 * (Notes != ""))

schedule_df <- schedule_df[, c(first_names, "Home", "Away", "Neutral")]

write.csv(schedule_df, file = "cfb_schedule_05_15.csv", row.names = F)

### 2. Get New Schedule ###

new_url <- sprintf(base_url, 2016)
new_schedule <- read_html(year_url) %>%
  html_node("#schedule") %>%
  html_table
names(new_schedule) <- c("Game_Num", "Week", "Date", "Time", "Day", "Home", "Home_Points", "Location", "Away", "Away_Points", "TV", "Notes")
new_schedule <- filter(new_schedule, Game_Num != "Rk") %>%
  select(-TV, -Away_Points, -Home_Points) %>%
  mutate(Neutral = 1 * (Notes != ""))

write.csv(new_schedule, file = "cfb_schedule_16.csv", row.names = F)

