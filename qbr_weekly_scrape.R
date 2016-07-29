###############################
#                             #
# Weekly ESPN QBR Data Scrape #
#                             #
###############################

library(rvest)
library(stringr)
library(dplyr)
library(purrr)
options(dplyr.width = Inf)

baseurl <- "http://espn.go.com/ncf/qbr/_/year/%i/seasontype/2/type/player-week/page/%i/week/%i"

year_list <- seq(from = 2005, to = 2015, by = 1)

## ESPN's servers are super sensitive to scraping a lot of pages at once. 
## Beacuse of that I'm gonna break this off into a function to get one year of
## data at each time AND include a lot of pauses in the data. 
## Also it the kickouts get faster the more you query so, I think, you can't
## just keep requesting the URL until you get a good one. But, maybe I'm wrong

qbr_grab <- function(year, base_url, pause = 4){
  qbr_list <- list()
  for(week in 1:16){
    for(page in 1:3){
      Sys.sleep(pause)
      page_url <- sprintf(base_url, year, page, week)
      page_get <- GET(url = page_url, user_agent("httr"))
      if (page_get$all_headers[[1]]$status != 200) {
        Sys.sleep(10)
        page_get <- GET(url = page_url, user_agent("new_agent"))
      }
      page_df <-  read_html(page_get) %>%
        html_node(".tablehead") %>%
        html_table(header = T)
      if (nrow(page_df) > 0) {
        page_df <- filter(page_df, RK != "RK") %>%
          mutate(Year = year, Week = week)
        qbr_list <- c(qbr_list, list(page_df))
      } else break
    }
  }
  
  
  for(page in 1:2){
    Sys.sleep(pause)
    page_url <- sprintf("http://espn.go.com/ncf/qbr/_/year/%i/type/player-week/page/%i", year, page)
    page_df <- GET(url = page_url, user_agent("httr")) %>% 
      read_html(page_url) %>%
      html_node(".tablehead") %>%
      html_table(header = T) %>%
      filter(RK != "RK") %>%
      mutate(Year = year, Week = 17) 
    qbr_list <- c(qbr_list, list(page_df))
  }
  
  return(qbr_list)
}

qbr_05 <- qbr_grab(2005, base_url = baseurl)
qbr_06 <- qbr_grab(2006, base_url = baseurl)
qbr_07 <- qbr_grab(2007, base_url = baseurl)
qbr_08 <- qbr_grab(2008, base_url = baseurl)
qbr_09 <- qbr_grab(2009, base_url = baseurl)
qbr_10 <- qbr_grab(2010, base_url = baseurl)
qbr_11 <- qbr_grab(2011, base_url = baseurl, pause = 6)
qbr_12 <- qbr_grab(2012, base_url = baseurl, pause = 5)
qbr_13 <- qbr_grab(2013, base_url = baseurl, pause = 5)
qbr_14 <- qbr_grab(2014, base_url = baseurl, pause = 5)
qbr_15 <- qbr_grab(2015, base_url = baseurl, pause = 6)

temp_list <- list(qbr_05, qbr_06, qbr_07, qbr_08, qbr_09, qbr_10, qbr_11, qbr_12, qbr_13, qbr_14, qbr_15)
save(temp_list, file = "temp_list.rdata")

qbr_list <- c(qbr_05, qbr_06, qbr_07, qbr_08, qbr_09, qbr_10, qbr_11, qbr_12, qbr_13, qbr_14, qbr_15)

fix_df <- function(x) {
  names(x) <- str_replace_all(names(x), " ", "_")
  x[, -c(2, 3)] <- lapply(x[, -c(2, 3)], as.numeric)
  return(x)
}

qbr_df <- map(qbr_list, fix_df) %>%
  bind_rows

# Get Team and Opponent
qbr_df$Team <- str_extract(qbr_df$PLAYER, "[A-Z]{2,}")
qbr_df$Opponent <- str_extract(qbr_df$RESULT, "[A-Z]{2,}")
# Get Player Name
qbr_df$QB <- map_chr(qbr_df$PLAYER, ~ str_split_fixed(.x, ",", n = 2)[, 1])
qbr_df$QBshort <- qbr_df$QB %>% # dirty for now but good enough
  str_replace_all(fixed("."), "") %>%
  str_replace_all(" Jr", "") %>%
  str_split(" ") %>% 
  map_chr(~ str_c(tail(.x, 1), str_sub(.x[1], 1, 1), sep = ", "))
# Get Offense Points
# To do that we need to know who won
qbr_df$Win <- str_sub(qbr_df$RESULT, 1, 1)
winning_points <- str_extract_all(qbr_df$RESULT, "[0-9]+") %>% map_chr(~ .x[1]) %>% as.numeric
losing_points <- str_extract_all(qbr_df$RESULT, "[0-9]+") %>% map_chr(~ .x[2]) %>% as.numeric
qbr_df$Points_For <- ifelse(qbr_df$Win == "W", winning_points, losing_points) # There is probably a better way to do this but I couldn't think of one immediately

write.csv(qbr_df, file = "qbr_weekly_05_15.csv", row.names = F)
