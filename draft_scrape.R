#######################################
#                                     #
# Pro Football Reference Draft Scrape #
#                                     #
#######################################
library(rvest)
library(dplyr)
base_url <- "http://www.pro-football-reference.com/years/%i/draft.htm"

year_list <- seq(from = 2005, to = 2016, by = 1)

draft_list <- list()
for(year in year_list){
  year_url <- sprintf(base_url, year)
  year_draft <- read_html(year_url) %>%
    html_node("#drafts") %>%
    html_table
  Sys.sleep(1)
  draft_list[[as.character(year)]] <- year_draft
}

draft_df <- bind_rows(draft_list, .id = "Year")
draft_df <- draft_df[, -(ncol(draft_df))]

write.csv(draft_df, file = "nfl_draft_05_16.csv", row.names = F)
