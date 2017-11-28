library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)

years <- 2011:2017
files <- "/Users/MM/Documents/fb_analysis/pbp_cleaning/cleaned_files/%i/%s.rds"

drives <- map(years, ~ sprintf(files, .x, "drive_info")) %>% 
  map(readRDS) %>%
  map2_dfr(years, ~ mutate(.x, Year = .y))
games <- map(years, ~ sprintf(files, .x, "game_info")) %>% map_dfr(readRDS)

missing_values <- map_dbl(drives, ~ mean(is.na(.x)))

select_if(drives, is.numeric) %>% map(summary)

clean_drives <- drives %>%
  filter(start_spot <= 100,
         end_spot <= 100,
         !is.na(result),
         !is.na(end_spot),
         start_quarter <= 4)
  mutate(drive_num = as.numeric(str_replace(drive_id, game_id, "")))
  
ggplot(aes(x = start_spot), data = clean_drives) + 
  geom_histogram(binwidth = 5, boundary = 0)

clean_drive_info <- games %>%
  select(game_id, home_abbr, away_abbr) %>%
  gather("type", "team", -game_id) %>%
  mutate(type = str_replace_all(type, fixed("_abbr"), "")) %>%
  left_join(clean_drives, by = c("game_id", "team" = "offense_short"))

ggplot(aes(x = start_spot), data = clean_drive_info) +
  geom_histogram(binwidth = 5, boundary = 0) +
  facet_wrap(~type)

ggplot(aes(x = end_spot), data = clean_drive_info) +
  geom_histogram(binwidth = 5, boundary = 0) +
  facet_wrap(~type)

clean_drive_info <- clean_drive_info %>%
  mutate(start_spot = ifelse(type == "home", 100 - start_spot, start_spot),
         end_spot = ifelse(type == "home", 100 - end_spot, end_spot))
    
ggplot(aes(x = start_spot, y = end_spot), data = clean_drive_info[runif(nrow(clean_drive_info)) < .1, ]) +
  geom_point(alpha = .1)
  
clean_drive_info %>%
  count(start_spot, end_spot) %>%
  arrange(desc(n)) %>%
  head()

drive_results <- clean_drive_info %>%
  group_by(game_id) %>%
  arrange(drive_num) %>%
  mutate(prev_result = lag(result),
         prev_result = ifelse(drive_num == 1, "KICKOFF", prev_result)) %>%
  ungroup %>%
  mutate(drive_ind = 1 * ((result %in% c("TD", "RUSHING TD", "PASSING TD")) | !str_detect(result, "TD")), 
         points = case_when(result %in% c("TD", "RUSHING TD", "PASSING TD") ~ 6.94,
                            result %in% c("FG", "FG GOOD") ~ 3,
                            TRUE ~ 0))

ggplot(aes(x = start_spot, y = end_spot), data = filter(drive_results, drive_ind == 1)) +
  geom_point(alpha = .1)

drive_results %>%
  filter(drive_ind == 1) %>%
  ggplot(aes(x = start_spot, y = points, color = as.factor(Year))) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 7), se = F) +
  viridis::scale_color_viridis(discrete = T)

drive_results %>%
  filter(drive_ind == 1) %>%
  ggplot(aes(x = start_spot, y = points)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 7), se = F)


    
    