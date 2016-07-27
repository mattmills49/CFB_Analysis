# CFB_Analysis

This repo contains some scripts and data for my college football analytics work. 

## Data Sets

* [247_recruit_rankings_08_16.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/247_recruit_rankings_08_16.csv): Contains the 247 composite team rankings for classes signed form years 2008 - 2016 for all current FBS teams. 
* [cfb_schedule_05_15.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/cfb_schedule_05_15.csv): Contains the FBS schedule and results for the 2005-2015 regular seasons (the 2015 regular season's title game happened in 2016) from cfb reference. 
* [cfb_schedule_16.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/cfb_schedule_16.csv): Contains the upcoming FBS schedule from college football reference. 
* [DraftPicksandValues1015.csv](https://raw.githubusercontent.com/mattmills49/CFB_Analysis/master/DraftPicksandValues1015.csv) contains the last 5 years of draft picks with Chase Stuart's Draft Value Chart

## R Scripts

* [247_recruit_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/247_recruit_scrape.R): R script to pull years of team rankings from 247 team rankings. If you want more information than I pulled (like the number of 5 stars in a class) you'll have to change the inspector gadget css to pull.
* [cfbreference_schedule_scrape.R](https://github.com/mattmills49/CFB_Analysis/blob/master/cfbreference_schedule_scrape.R): R script to download multiple years of schedule from the cfb reference shcedule and results page. In addition you can get the upcoming season's schedule. Historic schedules and upcoming schedules are a little different so require different steps. 

