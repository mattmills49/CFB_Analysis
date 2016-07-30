# CFB_Analysis

This repo contains some scripts and data for my college football analytics work. 

## Data Sets

* [247_recruit_rankings_05_16.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/247_recruit_rankings_05_16.csv): Contains the [247 composite team rankings](http://247sports.com/Season/2008-Football/CompositeTeamRankings) for classes signed form years 2008 - 2016 for all current FBS teams. 
* [cfb_schedule_05_15.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/cfb_schedule_05_15.csv): Contains the FBS schedule and results for the 2005-2015 regular seasons (the 2015 regular season's title game happened in 2016) from [cfb reference](http://www.sports-reference.com/cfb/years/2005-schedule.html).
* [cfb_schedule_16.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/cfb_schedule_16.csv): Contains the upcoming FBS schedule from college football reference. 
* [nfl_draft_05_16.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/nfl_draft_05_16.csv) contains the last 11 years of draft picks from [pro football reference](http://www.pro-football-reference.com/years/2005/draft.htm).
* [DraftValue.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/DraftValue.csv): Chase Stuart's [Draft Value charts](http://www.footballperspective.com/draft-value-chart/)
* [team_name_master_list.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/team_name_master_list.csv): Provides a common naming convention for teams across the different files. 
* [qbr_weekly_05_15.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/Datasets/qbr_weekly_05_15.csv): The weekly QBR and EPA data from ESPN for college QBs from 2005-2015. 

## R Scripts

* [247_recruit_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/Scrapers/247_recruit_scrape.R): R script to pull years of team rankings from 247 team rankings. If you want more information than I pulled (like the number of 5 stars in a class) you'll have to change the inspector gadget css to pull.
* [cfbreference_schedule_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/Scrapers/cfbreference_schedule_scrape.R): R script to download multiple years of schedule from the cfb reference shcedule and results page. In addition you can get the upcoming season's schedule. Historic schedules and upcoming schedules are a little different so require different steps.
* [draft_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/Scrapers/draft_scrape.R): R script to draft pick data. 
* [team_cleanup.R](https://github.com/mattmills49/CFB_Analysis/blob/master/Scrapers/team_cleanup.R): All the different sources use a different naming convention. This script tries to find all the matches between the three so that you can link records across different data sets. 
* [qbr_weekly_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/Scrapers/qbr_weekly_scrape.R): WARNING; the ESPN servers hate scraping so you have to space the requests out a lot so if you want to download this data on your own be warned. 

