####################################
#                                  # 
# 247 Team Recruit Rankings Scrape #
#                                  #
####################################

library(rvest)
library(stringr)

base_url <- "http://www.247sports.com/Season/%i-Football/CompositeTeamRankings"

year_list <- seq(from = 2008, to = 2016, by = 1)

recruit_matrix <- matrix("", nrow = 1, ncol = 6)
for(year in year_list){
  year_url <- sprintf(base_url, year)
  conf_list <- read_html(year_url) %>% 
    html_nodes(".conference_lst a") %>% 
    html_text %>% 
    tail(-1)
  year_url <- str_c(year_url, "?Conference=%s")
  for(conf in conf_list){
    conf_url <- sprintf(year_url, conf)
    conf_values <- read_html(conf_url) %>% 
      html_nodes(".team_itm span , .playerinfo_blk a") %>% 
      html_text %>%
      str_trim %>%
      matrix(ncol = 4, byrow = T) %>%
      cbind(conf, year)
    recruit_matrix <- rbind(recruit_matrix, conf_values)
    Sys.sleep(1)
  }
}

recruit_matrix <- recruit_matrix[-1, ]
recruit_df <- data.frame(Team = recruit_matrix[, 1],
                         Recruits = as.numeric(str_extract_all(recruit_matrix[, 2], "[0-9]+", simplify = T)[, 1]),
                         Total_Points = as.numeric(recruit_matrix[, 4]),
                         Conference = recruit_matrix[, 5],
                         Year = recruit_matrix[, 6], 
                         stringsAsFactors = F)

write.csv(recruit_df, "247_recruit_rankings_08_16.csv", row.names = F)


