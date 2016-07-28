####################################
#                                  # 
# 247 Team Recruit Rankings Scrape #
#                                  #
####################################

library(rvest) # For web scraping
library(stringr) # For string processing

base_url <- "http://www.247sports.com/Season/%i-Football/CompositeTeamRankings"

year_list <- seq(from = 2005, to = 2016, by = 1)
conf_list <- c("ACC", "Big-12", "AAC", "Big-Ten", "C-USA", "IND", "MAC", "M-West", "Pac-12", "SEC", "SBC")

# initialize the matrix to append teams to
recruit_matrix <- matrix("", nrow = 1, ncol = 6) 
for(year in year_list){
  year_url <- sprintf(base_url, year)
  year_url <- str_c(year_url, "?Conference=%s")
  for(conf in conf_list){
    conf_url <- sprintf(year_url, conf)
    conf_values <- read_html(conf_url) %>% 
      html_nodes(".team_itm span , .playerinfo_blk a") %>% # from the Inspector Gadget tool
      html_text %>%
      str_trim %>%
      matrix(ncol = 4, byrow = T) %>%
      cbind(conf, year)
    recruit_matrix <- rbind(recruit_matrix, conf_values)
    Sys.sleep(1) # wait a second to not throttle the servers at 247
  }
}

# remove the first empty row
recruit_matrix <- recruit_matrix[-1, ]
recruit_df <- data.frame(Team = recruit_matrix[, 1],
                         Recruits = as.numeric(str_extract_all(recruit_matrix[, 2], "[0-9]+", simplify = T)[, 1]),
                         Class_Points = as.numeric(recruit_matrix[, 4]),
                         Conference = recruit_matrix[, 5],
                         Year = recruit_matrix[, 6], 
                         stringsAsFactors = F)

write.csv(recruit_df, "247_recruit_rankings_05_16.csv", row.names = F)

###################
#                 #
# Data Validation #
#                 #
###################
library(ggplot2)
library(dplyr)
recruit_df <- read.csv("247_recruit_rankings_08_16.csv", as.is = T)
# 1. What is the spread of recruits signed in a year?
ggplot(aes(x = Recruits), data = recruit_df) +
  geom_histogram(binwidth = 5, boundary = 0, color = "black") +
  theme_bw()

## There are some classes with 0-5 and some with greater than 40, what is going on?
#
# head(arrange(recruit_df, desc(Recruits)))
#       Team Recruits Class_Points Conference Year
# 1     Army       49        79.82        IND 2009
# 2     Army       49       116.25        IND 2016
# 3      UAB       48       159.91      C-USA 2016
# 4 Nebraska       47       205.91    Big-Ten 2008
# 5     Army       47        94.48        IND 2010
# 6  Memphis       44       125.88        AAC 2013
#
## On Nebraska's 2008 page it lists a ton of extra recruits from 2008 that actually enrolled in 2011. Not sure what to do about that. 
#
# head(arrange(recruit_df, Recruits))
#                Team Recruits Class_Points Conference Year
# 1  Western Kentucky        1            0      C-USA 2008
# 2              UTEP        1            0      C-USA 2008
# 3     Massachusetts        1            0        MAC 2008
# 4 Appalachian State        1            0        SBC 2008
# 5  Western Kentucky        1            0      C-USA 2009
# 6  Georgia Southern        1            0        SBC 2009
#
# These are all teams that were FCS at the time. 

# 2. How many teams per year are there?
teams_per_year <- recruit_df %>%
  group_by(Year) %>%
  summarize(Teams = n_distinct(Team), Teams_with_recruits = sum(Class_Points > 0))
# head(teams_per_year)
#    Year Teams Teams_with_recruits
#   (int) (int)               (int)
# 1  2008   121                 117
# 2  2009   124                 116
# 3  2010   126                 121
# 4  2011   128                 122
# 5  2012   129                 129
#
# That looks about right. 

