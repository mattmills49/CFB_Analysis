#################
#               #
# Team Matching #
#               #
#################
library(readr)
library(dplyr)
library(stringr)

recruits <- read_csv("Datasets/247_recruit_rankings_05_16.csv")
schedule <- read_csv("Datasets/cfb_schedule_05_15.csv")
draft <- read_csv("Datasets/nfl_draft_05_16.csv")
names(draft)[23] <- c("College")
draft$College <- str_replace_all(draft$College, fixed("St."), "State")

master_team <- data_frame(Team = unique(c(schedule$Home, schedule$Away)))
master_team <- recruits %>%
  select(Team) %>%
  mutate(Team_Recruit = Team) %>%
  distinct %>%
  left_join(master_team, ., by = "Team")

bad_recruits <- anti_join(recruits, master_team, by = c("Team" = "Team_Recruit")) %>%
  count(Team) %>%
  arrange(desc(n))

fix_recruit_names <- c("Bowling Green" = "Bowling Green State",
                       "FIU" = "Florida International",
                       "LSU" = "Louisiana State",
                       "N.C. State" = "North Carolina State",
                       "Ole Miss" = "Mississippi",
                       "SMU" = "Southern Methodist",
                       "Southern Miss" = "Southern Mississippi",
                       "TCU" = "Texas Christian",
                       "UAB" = "Alabama-Birmingham",
                       "UCF" = "Central Florida",
                       "UNLV" = "Nevada-Las Vegas",
                       "USC" = "Southern California",
                       "USF" = "South Florida",
                       "UTEP" = "Texas-El Paso",
                       "Miami" = "Miami (FL)")

recruits$Fix_Team <- ifelse(is.na(fix_recruit_names[recruits$Team]), recruits$Team, fix_recruit_names[recruits$Team])

master_team <- data_frame(Team = unique(c(schedule$Home, schedule$Away)))
master_team <- recruits %>%
  select(Fix_Team) %>%
  mutate(Team_Recruit = Fix_Team) %>%
  distinct %>%
  left_join(master_team, ., by = c("Team" = "Fix_Team"))
                       
master_team <- draft %>%
  select(College) %>%
  mutate(Team_Draft = College) %>%
  distinct %>%
  left_join(master_team, ., by = c("Team" = "College"))

bad_drafts <- anti_join(draft, master_team, by = c("College" = "Team_Draft")) %>%
  count(College) %>%
  arrange(desc(n))

fix_draft_names <- c("USC" = "Southern California",
                     "LSU" = "Louisiana State",
                     "TCU" = "Texas Christian",
                     "Boston Col." = "Boston College",
                     "BYU" = "Brigham Young",
                     "SMU" = "Southern Methodist",
                     "Southern Miss" = "Southern Mississippi",
                     "West. Michigan" = "Western Michigan",
                     "West. Kentucky" = "Western Kentucky",
                     "UNLV" = "Nevada-Las Vegas",
                     "Middle Tenn. State" = "Middle Tennessee State",
                     "Ala-Birmingham" = "Alabama-Birmingham",
                     "Bowling Green" = "Bowling Green State",
                     "East. Michigan" = "Eastern Michigan",
                     "East. Washington" = "Eastern Washington",
                     "NW State (LA)" = "Northwestern State",
                     "East. Illinois" = "Eastern Illinois",
                     "La-Monroe" = "Louisiana-Monroe",
                     "La-Lafayette" = "Louisiana-Lafayette",
                     "Tenn-Chattanooga" = "Chattanooga")

draft$Fix_Team <- ifelse(is.na(fix_draft_names[draft$College]), draft$College, fix_draft_names[draft$College])

master_team <- data_frame(Team = unique(c(schedule$Home, schedule$Away)), Team_Schedule = unique(c(schedule$Home, schedule$Away)))
master_team <- recruits %>%
  select(Team, Fix_Team) %>%
  rename(Team_Recruit = Team) %>%
  distinct %>%
  left_join(master_team, ., by = c("Team" = "Fix_Team"))

master_team <- draft %>%
  select(College, Fix_Team) %>%
  rename(Team_Draft = College) %>%
  distinct %>%
  left_join(master_team, ., by = c("Team" = "Fix_Team"))

write.csv(master_team, file = "Datasets/team_name_master_list.csv", row.names = F)
