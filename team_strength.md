# Generate Team Quality Metrics

In order to build a prediction model for next season I need to know how good
each team was in the previous season(s). A decent and simple way to measure
team quality is to use the [Massey rankings](http://public.gettysburg.edu/~cwessell/RankingPage/massey.pdf), which basically just finds the
points above average that each team contributes to the margin of victory of 
each game using [Ordinary Least Squares Regression](https://en.wikipedia.org/wiki/Ordinary_least_squares). We will get into more complicated measures later on in this post. 

To do this, for each season, I need to organize the data so that each row 
corresponds to a game. The dependent variable (what we are trying to predict)
is the margin of victory from the view of the home team. The 
independent variables needed will be dummay variables indicating who is home 
(using a + 1) and who is away (using a -1). Normally you wold also include a
dummy variable for neutral site games, however the 2015 season on CFB 
Reference doesn't list any info on neutral site games. So even though we have
this information for every other season I'm going to have to exclude it since
the overall goal of this project is to predict the coming seasons results, so 
we can't just ignore we don't have it for this year.

Let's get started by reading in the data I've pulled from CFB Reference that
includes the results of every FBS football game over the last 10 seasons.



```r
library(readr) 
library(purrr)  
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(broom)
options(dplyr.width = Inf)

raw_schedule <- read_csv(file = "Datasets/cfb_schedule_05_15.csv",
                    col_types = cols(Date = col_date(format = "%b %d, %Y")))
```

```
## Error: 'Datasets/cfb_schedule_05_15.csv' does not exist in current working directory ('/Users/MM/Documents/fb_analysis/preseason_predictions').
```

```r
## Need to add the points for the home and away teams and find what season each
## game belongs to

results <- raw_schedule %>%
  mutate(Home_Points = ifelse(Winner == Home, Winner_Points, Loser_Points),
         Away_Points = ifelse(Winner == Home, Loser_Points, Winner_Points),
         Home_MOV = Home_Points - Away_Points,
         Season = ifelse(month(Date) == 1, year(Date) - 1, year(Date))) %>%
  filter(!is.na(Home_MOV))
```

| Season| Week|Home                   |Away                        | Home_Points| Away_Points| Home_MOV|
|------:|----:|:----------------------|:---------------------------|-----------:|-----------:|--------:|
|   2005|    1|Arizona State          |Temple                      |          63|          16|       47|
|   2005|    1|Cincinnati             |Eastern Michigan            |          28|          26|        2|
|   2005|    1|Connecticut            |Buffalo                     |          38|           0|       38|
|   2005|    1|Marshall               |William & Mary              |          36|          24|       12|
|   2005|    1|Tulsa                  |Minnesota                   |          10|          41|      -31|
|   2005|    1|Louisiana-Monroe       |Northwestern State          |          23|          27|       -4|
|   2005|    1|Houston                |Oregon                      |          24|          38|      -14|
|   2005|    1|South Carolina         |Central Florida             |          24|          15|        9|
|   2005|    1|Toledo                 |Western Illinois            |          62|          14|       48|
|   2005|    1|Wake Forest            |Vanderbilt                  |          20|          24|       -4|
|   2005|    1|Washington State       |Idaho                       |          38|          26|       12|
|   2005|    1|Central Michigan       |Indiana                     |          13|          20|       -7|
|   2005|    1|Utah                   |Arizona                     |          27|          24|        3|
|   2005|    1|Washington             |Air Force                   |          17|          20|       -3|
|   2005|    1|Alabama                |Middle Tennessee State      |          26|           7|       19|
|   2005|    1|Arkansas               |Missouri State              |          49|          17|       32|
|   2005|    1|Southern Methodist     |Baylor                      |          23|          28|       -5|
|   2005|    1|Brigham Young          |Boston College              |           3|          20|      -17|
|   2005|    1|California             |Sacramento State            |          41|           3|       38|
|   2005|    1|Clemson                |Texas A&M                   |          25|          24|        1|
|   2005|    1|Colorado               |Colorado State              |          31|          28|        3|
|   2005|    1|East Carolina          |Duke                        |          24|          21|        3|
|   2005|    1|Florida                |Wyoming                     |          32|          14|       18|
|   2005|    1|Georgia                |Boise State                 |          48|          13|       35|
|   2005|    1|Auburn                 |Georgia Tech                |          14|          23|       -9|
|   2005|    1|Illinois               |Rutgers                     |          33|          30|        3|
|   2005|    1|Iowa                   |Ball State                  |          56|           0|       56|
|   2005|    1|Iowa State             |Illinois State              |          32|          21|       11|
|   2005|    1|Kansas                 |Florida Atlantic            |          30|          19|       11|
|   2005|    1|Kansas State           |Florida International       |          35|          21|       14|
|   2005|    1|Maryland               |Navy                        |          23|          20|        3|
|   2005|    1|Michigan               |Northern Illinois           |          33|          17|       16|
|   2005|    1|Michigan State         |Kent State                  |          49|          14|       35|
|   2005|    1|Mississippi State      |Murray State                |          38|           6|       32|
|   2005|    1|Missouri               |Arkansas State              |          44|          17|       27|
|   2005|    1|Nebraska               |Maine                       |          25|           7|       18|
|   2005|    1|Northwestern           |Ohio                        |          38|          14|       24|
|   2005|    1|Pittsburgh             |Notre Dame                  |          21|          42|      -21|
|   2005|    1|Ohio State             |Miami (OH)                  |          34|          14|       20|
|   2005|    1|Oklahoma State         |Montana State               |          15|          10|        5|
|   2005|    1|Oregon State           |Portland State              |          41|          14|       27|
|   2005|    1|Penn State             |South Florida               |          23|          13|       10|
|   2005|    1|San Jose State         |Eastern Washington          |          35|          24|       11|
|   2005|    1|Hawaii                 |Southern California         |          17|          63|      -46|
|   2005|    1|Tennessee              |Alabama-Birmingham          |          17|          10|        7|
|   2005|    1|Texas                  |Louisiana-Lafayette         |          60|           3|       57|
|   2005|    1|Oklahoma               |Texas Christian             |          10|          17|       -7|
|   2005|    1|New Mexico State       |Texas-El Paso               |          17|          34|      -17|
|   2005|    1|Troy                   |Cal Poly                    |          27|          10|       17|
|   2005|    1|San Diego State        |UCLA                        |          21|          44|      -23|
|   2005|    1|Virginia               |Western Michigan            |          31|          19|       12|
|   2005|    1|Wisconsin              |Bowling Green State         |          56|          42|       14|
|   2005|    1|Kentucky               |Louisville                  |          24|          31|       -7|
|   2005|    1|North Carolina State   |Virginia Tech               |          16|          20|       -4|
|   2005|    1|Syracuse               |West Virginia               |           7|          15|       -8|
|   2005|    1|Florida State          |Miami (FL)                  |          10|           7|        3|
|   2005|    1|Memphis                |Mississippi                 |           6|          10|       -4|
|   2005|    1|New Mexico             |Nevada-Las Vegas            |          24|          22|        2|
|   2005|    2|Florida Atlantic       |Oklahoma State              |           3|          23|      -20|
|   2005|    2|Ohio                   |Pittsburgh                  |          16|          10|        6|
|   2005|    2|Nevada                 |Washington State            |          21|          55|      -34|
|   2005|    2|Air Force              |San Diego State             |          41|          29|       12|
|   2005|    2|Alabama                |Southern Mississippi        |          30|          21|        9|
|   2005|    2|Troy                   |Alabama-Birmingham          |           7|          27|      -20|
|   2005|    2|Arizona                |Northern Arizona            |          31|          12|       19|
|   2005|    2|Arkansas State         |Tennessee-Martin            |          56|           7|       49|
|   2005|    2|Auburn                 |Mississippi State           |          28|           0|       28|
|   2005|    2|Baylor                 |Samford                     |          48|          14|       34|
|   2005|    2|Boston College         |Army                        |          44|           7|       37|
|   2005|    2|Ball State             |Bowling Green State         |          31|          40|       -9|
|   2005|    2|Brigham Young          |Eastern Illinois            |          45|          10|       35|
|   2005|    2|Washington             |California                  |          17|          56|      -39|
|   2005|    2|Miami (OH)             |Central Michigan            |          37|          38|       -1|
|   2005|    2|Maryland               |Clemson                     |          24|          28|       -4|
|   2005|    2|Colorado               |New Mexico State            |          39|           0|       39|
|   2005|    2|Connecticut            |Liberty                     |          59|           0|       59|
|   2005|    2|Eastern Michigan       |Louisiana-Lafayette         |          31|          10|       21|
|   2005|    2|Florida                |Louisiana Tech              |          41|           3|       38|
|   2005|    2|Florida State          |Citadel                     |          62|          10|       52|
|   2005|    2|Fresno State           |Weber State                 |          55|          17|       38|
|   2005|    2|Georgia                |South Carolina              |          17|          15|        2|
|   2005|    2|Georgia Tech           |North Carolina              |          27|          21|        6|
|   2005|    2|Houston                |Sam Houston State           |          31|          10|       21|
|   2005|    2|Illinois               |San Jose State              |          40|          19|       21|
|   2005|    2|Indiana                |Nicholls State              |          35|          31|        4|
|   2005|    2|Iowa State             |Iowa                        |          23|           3|       20|
|   2005|    2|Kansas                 |Appalachian State           |          36|           8|       28|
|   2005|    2|Marshall               |Kansas State                |          19|          21|       -2|
|   2005|    2|Kent State             |Southeast Missouri State    |          33|          12|       21|
|   2005|    2|Kentucky               |Idaho State                 |          41|          29|       12|
|   2005|    2|Arizona State          |Louisiana State             |          31|          35|       -4|
|   2005|    2|Michigan State         |Hawaii                      |          42|          14|       28|
|   2005|    2|Minnesota              |Colorado State              |          56|          24|       32|
|   2005|    2|Nebraska               |Wake Forest                 |          31|           3|       28|
|   2005|    2|Nevada-Las Vegas       |Idaho                       |          34|          31|        3|
|   2005|    2|Missouri               |New Mexico                  |          35|          45|      -10|
|   2005|    2|Middle Tennessee State |North Texas                 |           7|          14|       -7|
|   2005|    2|Northwestern           |Northern Illinois           |          38|          37|        1|
|   2005|    2|Michigan               |Notre Dame                  |          10|          17|       -7|
|   2005|    2|Oklahoma               |Tulsa                       |          31|          15|       16|
|   2005|    2|Oregon                 |Montana                     |          47|          14|       33|
|   2005|    2|Oregon State           |Boise State                 |          30|          27|        3|
|   2005|    2|Penn State             |Cincinnati                  |          42|          24|       18|
|   2005|    2|Purdue                 |Akron                       |          49|          24|       25|
|   2005|    2|Rutgers                |Villanova                   |          38|           6|       32|
|   2005|    2|South Florida          |Florida A&M                 |          37|           3|       34|
|   2005|    2|Southern Methodist     |Texas Christian             |          21|          10|       11|
|   2005|    2|Navy                   |Stanford                    |          38|          41|       -3|
|   2005|    2|Syracuse               |Buffalo                     |          31|           0|       31|
|   2005|    2|Ohio State             |Texas                       |          22|          25|       -3|
|   2005|    2|Texas Tech             |Florida International       |          56|           3|       53|
|   2005|    2|Toledo                 |Western Michigan            |          56|          23|       33|
|   2005|    2|UCLA                   |Rice                        |          63|          21|       42|
|   2005|    2|Utah                   |Utah State                  |          31|           7|       24|
|   2005|    2|Arkansas               |Vanderbilt                  |          24|          28|       -4|
|   2005|    2|Duke                   |Virginia Tech               |           0|          45|      -45|
|   2005|    2|West Virginia          |Wofford                     |          35|           7|       28|
|   2005|    2|Wisconsin              |Temple                      |          65|           0|       65|
|   2005|    2|Wyoming                |Louisiana-Monroe            |          38|           0|       38|
|   2005|    3|Texas Christian        |Utah                        |          23|          20|        3|
|   2005|    3|Texas-El Paso          |Houston                     |          44|          41|        3|
|   2005|    3|Middle Tennessee State |Akron                       |           7|          17|      -10|
|   2005|    3|South Carolina         |Alabama                     |          14|          37|      -23|
|   2005|    3|Alabama-Birmingham     |Jacksonville State          |          35|          28|        7|
|   2005|    3|Arizona State          |Northwestern                |          52|          21|       31|
|   2005|    3|Auburn                 |Ball State                  |          63|           3|       60|
|   2005|    3|Army                   |Baylor                      |          10|          20|      -10|
|   2005|    3|California             |Illinois                    |          35|          20|       15|
|   2005|    3|Stanford               |California-Davis            |          17|          20|       -3|
|   2005|    3|Cincinnati             |Western Carolina            |           7|           3|        4|
|   2005|    3|Duke                   |Virginia Military Institute |          40|          14|       26|
|   2005|    3|Florida                |Tennessee                   |          16|           7|        9|
|   2005|    3|Boston College         |Florida State               |          17|          28|      -11|
|   2005|    3|Georgia                |Louisiana-Monroe            |          44|           7|       37|
|   2005|    3|Georgia Tech           |Connecticut                 |          28|          13|       15|
|   2005|    3|Indiana                |Kentucky                    |          38|          14|       24|
|   2005|    3|Iowa                   |Northern Iowa               |          45|          21|       24|
|   2005|    3|Kansas                 |Louisiana Tech              |          34|          14|       20|
|   2005|    3|Louisiana-Lafayette    |Northwestern State          |          49|          28|       21|
|   2005|    3|Louisville             |Oregon State                |          63|          27|       36|
|   2005|    3|Memphis                |Chattanooga                 |          59|          14|       45|
|   2005|    3|Clemson                |Miami (FL)                  |          30|          36|       -6|
|   2005|    3|Kent State             |Miami (OH)                  |          10|          27|      -17|
|   2005|    3|Michigan               |Eastern Michigan            |          55|           0|       55|
|   2005|    3|Notre Dame             |Michigan State              |          41|          44|       -3|
|   2005|    3|Minnesota              |Florida Atlantic            |          46|           7|       39|
|   2005|    3|Mississippi State      |Tulane                      |          21|          14|        7|
|   2005|    3|Missouri               |Troy                        |          52|          21|       31|
|   2005|    3|Nebraska               |Pittsburgh                  |           7|           6|        1|
|   2005|    3|Nevada                 |Nevada-Las Vegas            |          22|          14|        8|
|   2005|    3|New Mexico             |New Mexico State            |          38|          21|       17|
|   2005|    3|North Carolina State   |Eastern Kentucky            |          54|          10|       44|
|   2005|    3|Northern Illinois      |Tennessee Tech              |          42|           3|       39|
|   2005|    3|Ohio State             |San Diego State             |          27|           6|       21|
|   2005|    3|Oklahoma State         |Arkansas State              |          20|          10|       10|
|   2005|    3|Oregon                 |Fresno State                |          37|          34|        3|
|   2005|    3|Penn State             |Central Michigan            |          40|           3|       37|
|   2005|    3|Arizona                |Purdue                      |          24|          31|       -7|
|   2005|    3|Buffalo                |Rutgers                     |           3|          17|      -14|
|   2005|    3|South Florida          |Central Florida             |          31|          14|       17|
|   2005|    3|Southern California    |Arkansas                    |          70|          17|       53|
|   2005|    3|Southern Mississippi   |McNeese State               |          48|          20|       28|
|   2005|    3|Texas                  |Rice                        |          51|          10|       41|
|   2005|    3|Texas A&M              |Southern Methodist          |          66|           8|       58|
|   2005|    3|Texas Tech             |Sam Houston State           |          80|          21|       59|
|   2005|    3|Temple                 |Toledo                      |          17|          42|      -25|
|   2005|    3|North Texas            |Tulsa                       |           2|          54|      -52|
|   2005|    3|UCLA                   |Oklahoma                    |          41|          24|       17|
|   2005|    3|Vanderbilt             |Mississippi                 |          31|          23|        8|
|   2005|    3|Syracuse               |Virginia                    |          24|          27|       -3|
|   2005|    3|Virginia Tech          |Ohio                        |          45|           0|       45|
|   2005|    3|Wake Forest            |East Carolina               |          44|          34|       10|
|   2005|    3|Washington             |Idaho                       |          34|           6|       28|
|   2005|    3|Washington State       |Grambling State             |          48|           7|       41|
|   2005|    3|Maryland               |West Virginia               |          19|          31|      -12|
|   2005|    3|Western Michigan       |Southern Illinois           |          34|          28|        6|
|   2005|    3|North Carolina         |Wisconsin                   |           5|          14|       -9|
|   2005|    3|Air Force              |Wyoming                     |          28|          29|       -1|
|   2005|    4|Boise State            |Bowling Green State         |          48|          20|       28|
|   2005|    4|Florida Atlantic       |Louisiana-Monroe            |          21|          28|       -7|
|   2005|    4|Texas A&M              |Texas State                 |          44|          31|       13|
|   2005|    4|Utah                   |Air Force                   |          38|          35|        3|
|   2005|    4|New Mexico State       |California                  |          13|          41|      -28|
|   2005|    4|Army                   |Iowa State                  |          21|          28|       -7|
|   2005|    4|Akron                  |Northern Illinois           |          48|          42|        6|
|   2005|    4|Alabama                |Arkansas                    |          24|          13|       11|
|   2005|    4|Oregon State           |Arizona State               |          24|          42|      -18|
|   2005|    4|Arkansas State         |Florida International       |          66|          24|       42|
|   2005|    4|Auburn                 |Western Kentucky            |          37|          14|       23|
|   2005|    4|Clemson                |Boston College              |          13|          16|       -3|
|   2005|    4|Central Florida        |Marshall                    |          23|          13|       10|
|   2005|    4|Colorado State         |Nevada                      |          42|          21|       21|
|   2005|    4|Central Michigan       |Eastern Michigan            |          20|          23|       -3|
|   2005|    4|Kentucky               |Florida                     |          28|          49|      -21|
|   2005|    4|Mississippi State      |Georgia                     |          10|          23|      -13|
|   2005|    4|Idaho                  |Hawaii                      |           0|          24|      -24|
|   2005|    4|Kansas State           |North Texas                 |          54|           7|       47|
|   2005|    4|Wake Forest            |Maryland                    |          12|          22|      -10|
|   2005|    4|Miami (FL)             |Colorado                    |          23|           3|       20|
|   2005|    4|Illinois               |Michigan State              |          14|          61|      -47|
|   2005|    4|Minnesota              |Purdue                      |          42|          35|        7|
|   2005|    4|North Carolina State   |North Carolina              |          24|          31|       -7|
|   2005|    4|Washington             |Notre Dame                  |          17|          36|      -19|
|   2005|    4|Ohio                   |Kent State                  |          35|          32|        3|
|   2005|    4|Ohio State             |Iowa                        |          31|           6|       25|
|   2005|    4|Northwestern           |Penn State                  |          29|          34|       -5|
|   2005|    4|Pittsburgh             |Youngstown State            |          41|           0|       41|
|   2005|    4|San Diego State        |San Jose State              |          52|          21|       31|
|   2005|    4|South Carolina         |Troy                        |          45|          20|       25|
|   2005|    4|South Florida          |Louisville                  |          45|          14|       31|
|   2005|    4|Oregon                 |Southern California         |          13|          45|      -32|
|   2005|    4|Brigham Young          |Texas Christian             |          50|          51|       -1|
|   2005|    4|Texas Tech             |Indiana State               |          63|           7|       56|
|   2005|    4|Texas-El Paso          |New Mexico                  |          21|          13|        8|
|   2005|    4|Southern Methodist     |Tulane                      |          10|          31|      -21|
|   2005|    4|Tulsa                  |Memphis                     |          37|          31|        6|
|   2005|    4|Utah State             |Nevada-Las Vegas            |          31|          24|        7|
|   2005|    4|Vanderbilt             |Richmond                    |          37|          13|       24|
|   2005|    4|Virginia               |Duke                        |          38|           7|       31|
|   2005|    4|Virginia Tech          |Georgia Tech                |          51|           7|       44|
|   2005|    4|West Virginia          |East Carolina               |          20|          15|        5|
|   2005|    4|Temple                 |Western Michigan            |          16|          19|       -3|
|   2005|    4|Wisconsin              |Michigan                    |          23|          20|        3|
|   2005|    4|Mississippi            |Wyoming                     |          14|          24|      -10|
|   2005|    4|Louisiana State        |Tennessee                   |          27|          30|       -3|
|   2005|    5|Fresno State           |Toledo                      |          44|          14|       30|
|   2005|    5|Miami (OH)             |Cincinnati                  |          44|          16|       28|
|   2005|    5|Colorado State         |Air Force                   |          41|          23|       18|
|   2005|    5|Rutgers                |Pittsburgh                  |          37|          29|        8|
|   2005|    5|Alabama                |Florida                     |          31|           3|       28|
|   2005|    5|Alabama-Birmingham     |Rice                        |          45|          26|       19|
|   2005|    5|Auburn                 |South Carolina              |          48|           7|       41|
|   2005|    5|Hawaii                 |Boise State                 |          41|          44|       -3|
|   2005|    5|Boston College         |Ball State                  |          38|           0|       38|
|   2005|    5|Bowling Green State    |Temple                      |          70|           7|       63|
|   2005|    5|California             |Arizona                     |          28|           0|       28|
|   2005|    5|Louisiana-Lafayette    |Central Florida             |          21|          24|       -3|
|   2005|    5|Akron                  |Central Michigan            |          17|          31|      -14|
|   2005|    5|Oklahoma State         |Colorado                    |           0|          34|      -34|
|   2005|    5|Army                   |Connecticut                 |          13|          47|      -34|
|   2005|    5|Eastern Michigan       |Kent State                  |          27|          20|        7|
|   2005|    5|Florida International  |Florida A&M                 |          23|           6|       17|
|   2005|    5|Florida State          |Syracuse                    |          38|          14|       24|
|   2005|    5|Tulsa                  |Houston                     |          23|          30|       -7|
|   2005|    5|Idaho                  |Utah State                  |          27|          13|       14|
|   2005|    5|Iowa                   |Illinois                    |          35|           7|       28|
|   2005|    5|Mississippi State      |Louisiana State             |           7|          37|      -30|
|   2005|    5|Louisiana Tech         |New Mexico State            |          34|          14|       20|
|   2005|    5|Louisiana-Monroe       |Arkansas State              |          31|          27|        4|
|   2005|    5|Louisville             |Florida Atlantic            |          61|          10|       51|
|   2005|    5|Marshall               |Southern Methodist          |          16|          13|        3|
|   2005|    5|Maryland               |Virginia                    |          45|          33|       12|
|   2005|    5|Memphis                |Texas-El Paso               |          27|          20|        7|
|   2005|    5|Miami (FL)             |South Florida               |          27|           7|       20|
|   2005|    5|Michigan State         |Michigan                    |          31|          34|       -3|
|   2005|    5|Vanderbilt             |Middle Tennessee State      |          15|          17|       -2|
|   2005|    5|Duke                   |Navy                        |          21|          28|       -7|
|   2005|    5|Nebraska               |Iowa State                  |          27|          20|        7|
|   2005|    5|San Jose State         |Nevada                      |          23|          30|       -7|
|   2005|    5|North Carolina         |Utah                        |          31|          17|       14|
|   2005|    5|Purdue                 |Notre Dame                  |          28|          49|      -21|
|   2005|    5|Oklahoma               |Kansas State                |          43|          21|       22|
|   2005|    5|Stanford               |Oregon                      |          20|          44|      -24|
|   2005|    5|Oregon State           |Washington State            |          44|          33|       11|
|   2005|    5|Penn State             |Minnesota                   |          44|          14|       30|
|   2005|    5|San Diego State        |Brigham Young               |          31|          10|       21|
|   2005|    5|Arizona State          |Southern California         |          28|          38|      -10|
|   2005|    5|East Carolina          |Southern Mississippi        |           7|          33|      -26|
|   2005|    5|Tennessee              |Mississippi                 |          27|          10|       17|
|   2005|    5|Missouri               |Texas                       |          20|          51|      -31|
|   2005|    5|Texas A&M              |Baylor                      |          16|          13|        3|
|   2005|    5|Texas Christian        |New Mexico                  |          49|          28|       21|
|   2005|    5|Texas Tech             |Kansas                      |          30|          17|       13|
|   2005|    5|Tulane                 |Southeastern Louisiana      |          28|          21|        7|
|   2005|    5|UCLA                   |Washington                  |          21|          17|        4|
|   2005|    5|West Virginia          |Virginia Tech               |          17|          34|      -17|
|   2005|    5|Wake Forest            |Clemson                     |          31|          27|        4|
|   2005|    5|Western Michigan       |Buffalo                     |          31|          21|       10|
|   2005|    5|Wisconsin              |Indiana                     |          41|          24|       17|
|   2005|    5|Wyoming                |Nevada-Las Vegas            |          42|          17|       25|
|   2005|    6|North Texas            |Troy                        |          10|          13|       -3|
|   2005|    6|Northern Illinois      |Miami (OH)                  |          38|          27|       11|
|   2005|    6|Louisiana-Lafayette    |Florida Atlantic            |          10|          28|      -18|
|   2005|    6|Georgia Tech           |North Carolina State        |          14|          17|       -3|
|   2005|    6|Connecticut            |Syracuse                    |          26|           7|       19|
|   2005|    6|Buffalo                |Akron                       |           7|          13|       -6|
|   2005|    6|Arkansas               |Louisiana-Monroe            |          44|          15|       29|
|   2005|    6|Western Michigan       |Ball State                  |          57|          60|       -3|
|   2005|    6|Iowa State             |Baylor                      |          13|          23|      -10|
|   2005|    6|Boise State            |Portland State              |          21|          14|        7|
|   2005|    6|Boston College         |Virginia                    |          28|          17|       11|
|   2005|    6|Bowling Green State    |Ohio                        |          38|          14|       24|
|   2005|    6|New Mexico             |Brigham Young               |          24|          27|       -3|
|   2005|    6|Central Florida        |Memphis                     |          38|          17|       21|
|   2005|    6|Army                   |Central Michigan            |          10|          14|       -4|
|   2005|    6|Colorado               |Texas A&M                   |          41|          20|       21|
|   2005|    6|Colorado State         |Utah                        |          21|          17|        4|
|   2005|    6|East Carolina          |Rice                        |          41|          28|       13|
|   2005|    6|Florida                |Mississippi State           |          35|           9|       26|
|   2005|    6|Florida State          |Wake Forest                 |          41|          24|       17|
|   2005|    6|New Mexico State       |Fresno State                |           7|          37|      -30|
|   2005|    6|Tennessee              |Georgia                     |          14|          27|      -13|
|   2005|    6|Houston                |Tulane                      |          35|          14|       21|
|   2005|    6|Indiana                |Illinois                    |          36|          13|       23|
|   2005|    6|Purdue                 |Iowa                        |          17|          34|      -17|
|   2005|    6|Kansas State           |Kansas                      |          12|           3|        9|
|   2005|    6|Vanderbilt             |Louisiana State             |           6|          34|      -28|
|   2005|    6|Louisiana Tech         |Hawaii                      |          46|          14|       32|
|   2005|    6|Louisville             |North Carolina              |          69|          14|       55|
|   2005|    6|Temple                 |Maryland                    |           7|          38|      -31|
|   2005|    6|Miami (FL)             |Duke                        |          52|           7|       45|
|   2005|    6|Michigan               |Minnesota                   |          20|          23|       -3|
|   2005|    6|Mississippi            |Citadel                     |          27|           7|       20|
|   2005|    6|Oklahoma State         |Missouri                    |          31|          38|       -7|
|   2005|    6|Navy                   |Air Force                   |          27|          24|        3|
|   2005|    6|Nevada                 |Idaho                       |          62|          14|       48|
|   2005|    6|Nevada-Las Vegas       |San Diego State             |          13|          10|        3|
|   2005|    6|Northwestern           |Wisconsin                   |          51|          48|        3|
|   2005|    6|Arizona State          |Oregon                      |          17|          31|      -14|
|   2005|    6|Penn State             |Ohio State                  |          17|          10|        7|
|   2005|    6|Pittsburgh             |Cincinnati                  |          38|          20|       18|
|   2005|    6|South Carolina         |Kentucky                    |          44|          16|       28|
|   2005|    6|Southern California    |Arizona                     |          42|          21|       21|
|   2005|    6|Alabama-Birmingham     |Southern Methodist          |          27|          28|       -1|
|   2005|    6|Washington State       |Stanford                    |          21|          24|       -3|
|   2005|    6|Texas                  |Oklahoma                    |          45|          12|       33|
|   2005|    6|Wyoming                |Texas Christian             |          14|          28|      -14|
|   2005|    6|Nebraska               |Texas Tech                  |          31|          34|       -3|
|   2005|    6|Toledo                 |Eastern Michigan            |          30|           3|       27|
|   2005|    6|Southern Mississippi   |Tulsa                       |          17|          34|      -17|
|   2005|    6|UCLA                   |California                  |          47|          40|        7|
|   2005|    6|Utah State             |San Jose State              |          24|          17|        7|
|   2005|    6|Virginia Tech          |Marshall                    |          41|          14|       27|
|   2005|    6|Rutgers                |West Virginia               |          14|          27|      -13|
|   2005|    7|Arkansas State         |Louisiana-Lafayette         |          39|          36|        3|
|   2005|    7|North Carolina State   |Clemson                     |          10|          31|      -21|
|   2005|    7|Tulane                 |Texas-El Paso               |          21|          45|      -24|
|   2005|    7|Air Force              |Nevada-Las Vegas            |          42|           7|       35|
|   2005|    7|Mississippi            |Alabama                     |          10|          13|       -3|
|   2005|    7|Arkansas               |Auburn                      |          17|          34|      -17|
|   2005|    7|Boise State            |San Jose State              |          38|          21|       17|
|   2005|    7|Boston College         |Wake Forest                 |          35|          30|        5|
|   2005|    7|Buffalo                |Bowling Green State         |           7|          27|      -20|
|   2005|    7|Brigham Young          |Colorado State              |          24|          14|       10|
|   2005|    7|Central Michigan       |Ohio                        |          37|          10|       27|
|   2005|    7|Cincinnati             |Connecticut                 |          28|          17|       11|
|   2005|    7|Southern Methodist     |East Carolina               |          17|          24|       -7|
|   2005|    7|Fresno State           |Utah State                  |          53|          21|       32|
|   2005|    7|Vanderbilt             |Georgia                     |          17|          34|      -17|
|   2005|    7|Duke                   |Georgia Tech                |          10|          35|      -25|
|   2005|    7|Hawaii                 |New Mexico State            |          49|          28|       21|
|   2005|    7|Iowa                   |Indiana                     |          38|          21|       17|
|   2005|    7|Louisiana State        |Florida                     |          21|          17|        4|
|   2005|    7|Louisiana-Monroe       |Troy                        |          27|           3|       24|
|   2005|    7|Marshall               |Alabama-Birmingham          |          20|          19|        1|
|   2005|    7|Houston                |Memphis                     |          20|          35|      -15|
|   2005|    7|Temple                 |Miami (FL)                  |           3|          34|      -31|
|   2005|    7|Miami (OH)             |Akron                       |          51|          23|       28|
|   2005|    7|Michigan               |Penn State                  |          27|          25|        2|
|   2005|    7|Florida Atlantic       |Middle Tennessee State      |          14|          35|      -21|
|   2005|    7|Missouri               |Iowa State                  |          27|          24|        3|
|   2005|    7|Navy                   |Kent State                  |          34|          31|        3|
|   2005|    7|Baylor                 |Nebraska                    |          14|          23|       -9|
|   2005|    7|Nevada                 |Louisiana Tech              |          37|          27|       10|
|   2005|    7|Wyoming                |New Mexico                  |          24|          27|       -3|
|   2005|    7|Florida International  |North Texas                 |          10|          13|       -3|
|   2005|    7|Northern Illinois      |Eastern Michigan            |          24|           8|       16|
|   2005|    7|Purdue                 |Northwestern                |          29|          34|       -5|
|   2005|    7|Ohio State             |Michigan State              |          35|          24|       11|
|   2005|    7|Kansas                 |Oklahoma                    |           3|          19|      -16|
|   2005|    7|Oregon                 |Washington                  |          45|          21|       24|
|   2005|    7|California             |Oregon State                |          20|          23|       -3|
|   2005|    7|Pittsburgh             |South Florida               |          31|          17|       14|
|   2005|    7|Syracuse               |Rutgers                     |           9|          31|      -22|
|   2005|    7|Utah                   |San Diego State             |          19|          28|       -9|
|   2005|    7|Notre Dame             |Southern California         |          31|          34|       -3|
|   2005|    7|Southern Mississippi   |Central Florida             |          52|          31|       21|
|   2005|    7|Arizona                |Stanford                    |          16|          20|       -4|
|   2005|    7|Texas                  |Colorado                    |          42|          17|       25|
|   2005|    7|Texas A&M              |Oklahoma State              |          62|          23|       39|
|   2005|    7|Texas Christian        |Army                        |          38|          17|       21|
|   2005|    7|Texas Tech             |Kansas State                |          59|          20|       39|
|   2005|    7|Ball State             |Toledo                      |          14|          34|      -20|
|   2005|    7|Rice                   |Tulsa                       |          21|          41|      -20|
|   2005|    7|Washington State       |UCLA                        |          41|          44|       -3|
|   2005|    7|Virginia               |Florida State               |          26|          21|        5|
|   2005|    7|West Virginia          |Louisville                  |          46|          44|        2|
|   2005|    7|Minnesota              |Wisconsin                   |          34|          38|       -4|
|   2005|    8|Troy                   |Florida International       |          18|          13|        5|
|   2005|    8|Maryland               |Virginia Tech               |           9|          28|      -19|
|   2005|    8|Central Florida        |Tulane                      |          34|          24|       10|
|   2005|    8|Alabama-Birmingham     |Southern Mississippi        |          28|          37|       -9|
|   2005|    8|Alabama                |Tennessee                   |           6|           3|        3|
|   2005|    8|Arkansas State         |Florida Atlantic            |           3|           0|        3|
|   2005|    8|Akron                  |Army                        |           0|          20|      -20|
|   2005|    8|Utah State             |Boise State                 |          21|          45|      -24|
|   2005|    8|California             |Washington State            |          42|          38|        4|
|   2005|    8|Clemson                |Temple                      |          37|           7|       30|
|   2005|    8|Colorado               |Kansas                      |          44|          13|       31|
|   2005|    8|Colorado State         |Wyoming                     |          39|          31|        8|
|   2005|    8|Duke                   |Florida State               |          24|          55|      -31|
|   2005|    8|Idaho                  |Fresno State                |          10|          40|      -30|
|   2005|    8|Georgia                |Arkansas                    |          23|          20|        3|
|   2005|    8|San Jose State         |Hawaii                      |          38|          45|       -7|
|   2005|    8|Mississippi State      |Houston                     |          16|          28|      -12|
|   2005|    8|Iowa State             |Oklahoma State              |          37|          10|       27|
|   2005|    8|Louisiana State        |Auburn                      |          20|          17|        3|
|   2005|    8|Louisiana Tech         |North Texas                 |          40|          14|       26|
|   2005|    8|Middle Tennessee State |Louisiana-Lafayette         |          10|          13|       -3|
|   2005|    8|Cincinnati             |Louisville                  |          22|          46|      -24|
|   2005|    8|Memphis                |East Carolina               |          27|          24|        3|
|   2005|    8|Eastern Michigan       |Miami (OH)                  |          23|          24|       -1|
|   2005|    8|Iowa                   |Michigan                    |          20|          23|       -3|
|   2005|    8|Mississippi            |Kentucky                    |          13|           7|        6|
|   2005|    8|Missouri               |Nebraska                    |          41|          24|       17|
|   2005|    8|Rice                   |Navy                        |           9|          41|      -32|
|   2005|    8|San Diego State        |New Mexico                  |          24|          47|      -23|
|   2005|    8|North Carolina         |Virginia                    |           7|           5|        2|
|   2005|    8|Kent State             |Northern Illinois           |           3|          34|      -31|
|   2005|    8|Michigan State         |Northwestern                |          14|          49|      -35|
|   2005|    8|Notre Dame             |Brigham Young               |          49|          23|       26|
|   2005|    8|Ohio                   |Ball State                  |          38|          21|       17|
|   2005|    8|Indiana                |Ohio State                  |          10|          41|      -31|
|   2005|    8|Oklahoma               |Baylor                      |          37|          30|        7|
|   2005|    8|Arizona                |Oregon                      |          21|          28|       -7|
|   2005|    8|Illinois               |Penn State                  |          10|          63|      -53|
|   2005|    8|Pittsburgh             |Syracuse                    |          34|          17|       17|
|   2005|    8|Connecticut            |Rutgers                     |          24|          26|       -2|
|   2005|    8|South Carolina         |Vanderbilt                  |          35|          28|        7|
|   2005|    8|Washington             |Southern California         |          24|          51|      -27|
|   2005|    8|Stanford               |Arizona State               |          45|          35|       10|
|   2005|    8|Texas                  |Texas Tech                  |          52|          17|       35|
|   2005|    8|Kansas State           |Texas A&M                   |          28|          30|       -2|
|   2005|    8|Air Force              |Texas Christian             |          10|          48|      -38|
|   2005|    8|Texas-El Paso          |Marshall                    |          31|           3|       28|
|   2005|    8|Toledo                 |Buffalo                     |          38|          15|       23|
|   2005|    8|Tulsa                  |Southern Methodist          |          20|          13|        7|
|   2005|    8|UCLA                   |Oregon State                |          51|          28|       23|
|   2005|    8|Nevada-Las Vegas       |Utah                        |          32|          42|      -10|
|   2005|    8|Wake Forest            |North Carolina State        |          27|          19|        8|
|   2005|    8|Bowling Green State    |Western Michigan            |          14|          45|      -31|
|   2005|    8|Wisconsin              |Purdue                      |          31|          20|       11|
|   2005|    9|Virginia Tech          |Boston College              |          30|          10|       20|
|   2005|    9|New Mexico             |Colorado State              |          25|          35|      -10|
|   2005|    9|Bowling Green State    |Akron                       |          14|          24|      -10|
|   2005|    9|Alabama                |Utah State                  |          35|           3|       32|
|   2005|    9|Oregon State           |Arizona                     |          27|          29|       -2|
|   2005|    9|Arizona State          |Washington                  |          44|          20|       24|
|   2005|    9|Auburn                 |Mississippi                 |          27|           3|       24|
|   2005|    9|Northern Illinois      |Ball State                  |          17|          31|      -14|
|   2005|    9|Boise State            |Nevada                      |          49|          14|       35|
|   2005|    9|Brigham Young          |Air Force                   |          62|          41|       21|
|   2005|    9|East Carolina          |Central Florida             |          20|          30|      -10|
|   2005|    9|Central Michigan       |Toledo                      |          21|          17|        4|
|   2005|    9|Syracuse               |Cincinnati                  |          16|          22|       -6|
|   2005|    9|Kansas State           |Colorado                    |          20|          23|       -3|
|   2005|    9|Florida                |Georgia                     |          14|          10|        4|
|   2005|    9|Florida State          |Maryland                    |          35|          27|        8|
|   2005|    9|Hawaii                 |Fresno State                |          13|          27|      -14|
|   2005|    9|Georgia Tech           |Clemson                     |          10|           9|        1|
|   2005|    9|New Mexico State       |Idaho                       |          37|          38|       -1|
|   2005|    9|Texas A&M              |Iowa State                  |          14|          42|      -28|
|   2005|    9|Kansas                 |Missouri                    |          13|           3|       10|
|   2005|    9|Kentucky               |Mississippi State           |          13|           7|        6|
|   2005|    9|Louisiana State        |North Texas                 |          56|           3|       53|
|   2005|    9|Louisiana Tech         |San Jose State              |          31|          14|       17|
|   2005|    9|Louisiana-Lafayette    |Troy                        |          31|          28|        3|
|   2005|    9|Tulane                 |Marshall                    |          26|          27|       -1|
|   2005|    9|Miami (FL)             |North Carolina              |          34|          16|       18|
|   2005|    9|Temple                 |Miami (OH)                  |          14|          41|      -27|
|   2005|    9|Northwestern           |Michigan                    |          17|          33|      -16|
|   2005|    9|Michigan State         |Indiana                     |          46|          15|       31|
|   2005|    9|North Carolina State   |Southern Mississippi        |          21|          17|        4|
|   2005|    9|Buffalo                |Ohio                        |          20|          34|      -14|
|   2005|    9|Minnesota              |Ohio State                  |          31|          45|      -14|
|   2005|    9|Nebraska               |Oklahoma                    |          24|          31|       -7|
|   2005|    9|Penn State             |Purdue                      |          33|          15|       18|
|   2005|    9|Rutgers                |Navy                        |          31|          21|       10|
|   2005|    9|Tennessee              |South Carolina              |          15|          16|       -1|
|   2005|    9|Southern California    |Washington State            |          55|          13|       42|
|   2005|    9|Oklahoma State         |Texas                       |          28|          47|      -19|
|   2005|    9|San Diego State        |Texas Christian             |          20|          23|       -3|
|   2005|    9|Baylor                 |Texas Tech                  |           0|          28|      -28|
|   2005|    9|Rice                   |Texas-El Paso               |          31|          38|       -7|
|   2005|    9|Stanford               |UCLA                        |          27|          30|       -3|
|   2005|    9|Duke                   |Wake Forest                 |           6|          44|      -38|
|   2005|    9|Western Michigan       |Kent State                  |          44|          14|       30|
|   2005|    9|Illinois               |Wisconsin                   |          24|          41|      -17|
|   2005|   10|Memphis                |Alabama-Birmingham          |          20|          37|      -17|
|   2005|   10|West Virginia          |Connecticut                 |          45|          13|       32|
|   2005|   10|Louisville             |Pittsburgh                  |          42|          20|       22|
|   2005|   10|Ohio                   |Toledo                      |          21|          30|       -9|
|   2005|   10|Mississippi State      |Alabama                     |           0|          17|      -17|
|   2005|   10|Arizona                |UCLA                        |          52|          14|       38|
|   2005|   10|Washington State       |Arizona State               |          24|          27|       -3|
|   2005|   10|Air Force              |Army                        |          24|          27|       -3|
|   2005|   10|Kentucky               |Auburn                      |          27|          49|      -22|
|   2005|   10|Ball State             |Akron                       |          23|          17|        6|
|   2005|   10|Boise State            |New Mexico State            |          56|           6|       50|
|   2005|   10|Kent State             |Bowling Green State         |          14|          24|      -10|
|   2005|   10|Nevada-Las Vegas       |Brigham Young               |          14|          55|      -41|
|   2005|   10|Central Florida        |Houston                     |          31|          29|        2|
|   2005|   10|Clemson                |Duke                        |          49|          20|       29|
|   2005|   10|Colorado               |Missouri                    |          41|          12|       29|
|   2005|   10|Florida                |Vanderbilt                  |          49|          42|        7|
|   2005|   10|Louisiana-Monroe       |Florida International       |          29|          31|       -2|
|   2005|   10|Fresno State           |San Jose State              |          45|           7|       38|
|   2005|   10|Georgia Tech           |Wake Forest                 |          30|          17|       13|
|   2005|   10|Iowa State             |Kansas State                |          45|          17|       28|
|   2005|   10|Kansas                 |Nebraska                    |          40|          15|       25|
|   2005|   10|Louisiana State        |Appalachian State           |          24|           0|       24|
|   2005|   10|Utah State             |Louisiana Tech              |          17|          27|      -10|
|   2005|   10|North Texas            |Louisiana-Lafayette         |          28|          31|       -3|
|   2005|   10|Virginia Tech          |Miami (FL)                  |           7|          27|      -20|
|   2005|   10|Miami (OH)             |Buffalo                     |          54|          13|       41|
|   2005|   10|Middle Tennessee State |Arkansas State              |          45|           7|       38|
|   2005|   10|Indiana                |Minnesota                   |          21|          42|      -21|
|   2005|   10|Navy                   |Tulane                      |          49|          21|       28|
|   2005|   10|Nevada                 |Hawaii                      |          38|          28|       10|
|   2005|   10|North Carolina         |Boston College              |          16|          14|        2|
|   2005|   10|Florida State          |North Carolina State        |          15|          20|       -5|
|   2005|   10|Central Michigan       |Northern Illinois           |          28|          31|       -3|
|   2005|   10|Northwestern           |Iowa                        |          28|          27|        1|
|   2005|   10|Notre Dame             |Tennessee                   |          41|          21|       20|
|   2005|   10|Ohio State             |Illinois                    |          40|           2|       38|
|   2005|   10|Oregon                 |California                  |          27|          20|        7|
|   2005|   10|Washington             |Oregon State                |          10|          18|       -8|
|   2005|   10|Penn State             |Wisconsin                   |          35|          14|       21|
|   2005|   10|Purdue                 |Michigan State              |          28|          21|        7|
|   2005|   10|Arkansas               |South Carolina              |          10|          14|       -4|
|   2005|   10|Rutgers                |South Florida               |          31|          45|      -14|
|   2005|   10|Southern California    |Stanford                    |          51|          21|       30|
|   2005|   10|Southern Methodist     |Rice                        |          27|           7|       20|
|   2005|   10|Baylor                 |Texas                       |           0|          62|      -62|
|   2005|   10|Texas Christian        |Colorado State              |          33|           6|       27|
|   2005|   10|Texas Tech             |Texas A&M                   |          56|          17|       39|
|   2005|   10|Texas-El Paso          |Tulsa                       |          41|          38|        3|
|   2005|   10|Troy                   |Florida Atlantic            |          28|          14|       14|
|   2005|   10|Utah                   |Wyoming                     |          43|          13|       30|
|   2005|   10|Virginia               |Temple                      |          51|           3|       48|
|   2005|   10|Eastern Michigan       |Western Michigan            |          36|          44|       -8|
|   2005|   11|Marshall               |Southern Mississippi        |          24|          27|       -3|
|   2005|   11|Cincinnati             |West Virginia               |           0|          38|      -38|
|   2005|   11|Fresno State           |Boise State                 |          27|           7|       20|
|   2005|   11|Louisville             |Rutgers                     |          56|           5|       51|
|   2005|   11|Mississippi            |Arkansas                    |          17|          28|      -11|
|   2005|   11|Arkansas State         |Troy                        |           9|           3|        6|
|   2005|   11|Army                   |Massachusetts               |          34|          27|        7|
|   2005|   11|Georgia                |Auburn                      |          30|          31|       -1|
|   2005|   11|Eastern Michigan       |Ball State                  |          25|          26|       -1|
|   2005|   11|Boston College         |North Carolina State        |          30|          10|       20|
|   2005|   11|Wyoming                |Brigham Young               |          21|          35|      -14|
|   2005|   11|Kent State             |Buffalo                     |           6|          10|       -4|
|   2005|   11|Alabama-Birmingham     |Central Florida             |          21|          27|       -6|
|   2005|   11|Clemson                |Florida State               |          35|          14|       21|
|   2005|   11|Florida Atlantic       |North Texas                 |          26|          23|        3|
|   2005|   11|Hawaii                 |Utah State                  |          50|          23|       27|
|   2005|   11|Wisconsin              |Iowa                        |          10|          20|      -10|
|   2005|   11|Iowa State             |Colorado                    |          30|          16|       14|
|   2005|   11|Vanderbilt             |Kentucky                    |          43|          48|       -5|
|   2005|   11|Alabama                |Louisiana State             |          13|          16|       -3|
|   2005|   11|Idaho                  |Louisiana Tech              |          38|          41|       -3|
|   2005|   11|Louisiana-Lafayette    |Florida International       |          28|           7|       21|
|   2005|   11|Middle Tennessee State |Louisiana-Monroe            |          31|          34|       -3|
|   2005|   11|North Carolina         |Maryland                    |          30|          33|       -3|
|   2005|   11|Wake Forest            |Miami (FL)                  |          17|          47|      -30|
|   2005|   11|Michigan               |Indiana                     |          41|          14|       27|
|   2005|   11|Minnesota              |Michigan State              |          41|          18|       23|
|   2005|   11|Missouri               |Baylor                      |          31|          16|       15|
|   2005|   11|Nebraska               |Kansas State                |          27|          25|        2|
|   2005|   11|New Mexico State       |Nevada                      |          24|          48|      -24|
|   2005|   11|Utah                   |New Mexico                  |          27|          31|       -4|
|   2005|   11|Notre Dame             |Navy                        |          42|          21|       21|
|   2005|   11|Ohio State             |Northwestern                |          48|           7|       41|
|   2005|   11|Oklahoma               |Texas A&M                   |          36|          30|        6|
|   2005|   11|Oklahoma State         |Texas Tech                  |          24|          17|        7|
|   2005|   11|Washington State       |Oregon                      |          31|          34|       -3|
|   2005|   11|Pittsburgh             |Connecticut                 |          24|           0|       24|
|   2005|   11|Purdue                 |Illinois                    |          37|           3|       34|
|   2005|   11|Rice                   |Tulane                      |          42|          34|        8|
|   2005|   11|Colorado State         |San Diego State             |          10|          30|      -20|
|   2005|   11|South Carolina         |Florida                     |          30|          22|        8|
|   2005|   11|Syracuse               |South Florida               |           0|          27|      -27|
|   2005|   11|California             |Southern California         |          10|          35|      -25|
|   2005|   11|Oregon State           |Stanford                    |          17|          20|       -3|
|   2005|   11|Tennessee              |Memphis                     |          20|          16|        4|
|   2005|   11|Texas                  |Kansas                      |          66|          14|       52|
|   2005|   11|Texas Christian        |Nevada-Las Vegas            |          51|           3|       48|
|   2005|   11|Texas-El Paso          |Texas Southern              |          45|           0|       45|
|   2005|   11|Tulsa                  |East Carolina               |          45|          13|       32|
|   2005|   11|UCLA                   |Arizona State               |          45|          35|       10|
|   2005|   11|Virginia               |Georgia Tech                |          27|          17|       10|
|   2005|   11|Arizona                |Washington                  |          14|          38|      -24|
|   2005|   11|Western Michigan       |Central Michigan            |          31|          24|        7|
|   2005|   11|Houston                |Southern Mississippi        |          27|          24|        3|
|   2005|   12|Akron                  |Ohio                        |          27|           3|       24|
|   2005|   12|Miami (OH)             |Bowling Green State         |          14|          42|      -28|
|   2005|   12|Toledo                 |Northern Illinois           |          17|          35|      -18|
|   2005|   12|New Mexico             |Air Force                   |          24|          42|      -18|
|   2005|   12|Texas-El Paso          |Alabama-Birmingham          |          23|          35|      -12|
|   2005|   12|Arkansas               |Mississippi State           |          44|          10|       34|
|   2005|   12|Army                   |Arkansas State              |          38|          10|       28|
|   2005|   12|Auburn                 |Alabama                     |          28|          18|       10|
|   2005|   12|Baylor                 |Oklahoma State              |          44|          34|       10|
|   2005|   12|Boise State            |Idaho                       |          70|          35|       35|
|   2005|   12|Maryland               |Boston College              |          16|          31|      -15|
|   2005|   12|Stanford               |California                  |           3|          27|      -24|
|   2005|   12|Rice                   |Central Florida             |          28|          31|       -3|
|   2005|   12|Ball State             |Central Michigan            |          24|          31|       -7|
|   2005|   12|South Carolina         |Clemson                     |           9|          13|       -4|
|   2005|   12|Nevada-Las Vegas       |Colorado State              |          27|          31|       -4|
|   2005|   12|Marshall               |East Carolina               |          29|          34|       -5|
|   2005|   12|Buffalo                |Eastern Michigan            |          14|          38|      -24|
|   2005|   12|Florida International  |Western Kentucky            |          38|          35|        3|
|   2005|   12|Georgia                |Kentucky                    |          45|          13|       32|
|   2005|   12|Miami (FL)             |Georgia Tech                |          10|          14|       -4|
|   2005|   12|Iowa                   |Minnesota                   |          52|          28|       24|
|   2005|   12|Kansas State           |Missouri                    |          36|          28|        8|
|   2005|   12|Mississippi            |Louisiana State             |           7|          40|      -33|
|   2005|   12|North Texas            |Louisiana-Monroe            |          19|          24|       -5|
|   2005|   12|Southern Mississippi   |Memphis                     |          22|          24|       -2|
|   2005|   12|Navy                   |Temple                      |          38|          17|       21|
|   2005|   12|Utah State             |Nevada                      |          24|          30|       -6|
|   2005|   12|North Carolina         |Duke                        |          24|          21|        3|
|   2005|   12|North Carolina State   |Middle Tennessee State      |          24|           3|       21|
|   2005|   12|Illinois               |Northwestern                |          21|          38|      -17|
|   2005|   12|Notre Dame             |Syracuse                    |          34|          10|       24|
|   2005|   12|Michigan               |Ohio State                  |          21|          25|       -4|
|   2005|   12|Oregon                 |Oregon State                |          56|          14|       42|
|   2005|   12|Michigan State         |Penn State                  |          22|          31|       -9|
|   2005|   12|Indiana                |Purdue                      |          14|          41|      -27|
|   2005|   12|San Diego State        |Wyoming                     |          34|          21|       13|
|   2005|   12|San Jose State         |New Mexico State            |          27|          10|       17|
|   2005|   12|South Florida          |Cincinnati                  |          31|          16|       15|
|   2005|   12|Southern California    |Fresno State                |          50|          42|        8|
|   2005|   12|Houston                |Southern Methodist          |          24|          29|       -5|
|   2005|   12|Texas Tech             |Oklahoma                    |          23|          21|        2|
|   2005|   12|Tulane                 |Tulsa                       |          14|          38|      -24|
|   2005|   12|Brigham Young          |Utah                        |          34|          41|       -7|
|   2005|   12|Tennessee              |Vanderbilt                  |          24|          28|       -4|
|   2005|   12|Virginia               |Virginia Tech               |          14|          52|      -38|
|   2005|   12|Washington             |Washington State            |          22|          26|       -4|
|   2005|   12|Ohio                   |Miami (OH)                  |           7|          38|      -31|
|   2005|   13|Bowling Green State    |Toledo                      |          41|          44|       -3|
|   2005|   13|Northern Illinois      |Western Michigan            |          42|           7|       35|
|   2005|   13|Akron                  |Kent State                  |          35|           3|       32|
|   2005|   13|West Virginia          |Pittsburgh                  |          45|          13|       32|
|   2005|   13|Arizona State          |Arizona                     |          23|          20|        3|
|   2005|   13|Louisiana State        |Arkansas                    |          19|          17|        2|
|   2005|   13|Colorado               |Nebraska                    |           3|          30|      -27|
|   2005|   13|Texas A&M              |Texas                       |          29|          40|      -11|
|   2005|   13|Hawaii                 |Wisconsin                   |          24|          41|      -17|
|   2005|   13|North Texas            |Arkansas State              |          24|          31|       -7|
|   2005|   13|Louisiana Tech         |Boise State                 |          13|          30|      -17|
|   2005|   13|Connecticut            |South Florida               |          15|          10|        5|
|   2005|   13|East Carolina          |Alabama-Birmingham          |          31|          23|        8|
|   2005|   13|Florida                |Florida State               |          34|           7|       27|
|   2005|   13|Florida International  |Florida Atlantic            |          52|           6|       46|
|   2005|   13|Georgia Tech           |Georgia                     |           7|          14|       -7|
|   2005|   13|Houston                |Rice                        |          35|          18|       17|
|   2005|   13|Kansas                 |Iowa State                  |          24|          21|        3|
|   2005|   13|Louisiana-Monroe       |Louisiana-Lafayette         |          21|          54|      -33|
|   2005|   13|Louisville             |Syracuse                    |          41|          17|       24|
|   2005|   13|Memphis                |Marshall                    |          26|           3|       23|
|   2005|   13|Miami (FL)             |Virginia                    |          25|          17|        8|
|   2005|   13|Troy                   |Middle Tennessee State      |           7|          17|      -10|
|   2005|   13|Mississippi State      |Mississippi                 |          35|          14|       21|
|   2005|   13|Nevada                 |Fresno State                |          38|          35|        3|
|   2005|   13|North Carolina State   |Maryland                    |          20|          14|        6|
|   2005|   13|Stanford               |Notre Dame                  |          31|          38|       -7|
|   2005|   13|Oklahoma               |Oklahoma State              |          42|          14|       28|
|   2005|   13|Rutgers                |Cincinnati                  |          44|           9|       35|
|   2005|   13|San Jose State         |Idaho                       |          26|          18|        8|
|   2005|   13|Southern Methodist     |Texas-El Paso               |          40|          27|       13|
|   2005|   13|Southern Mississippi   |Tulane                      |          26|           7|       19|
|   2005|   13|Kentucky               |Tennessee                   |           8|          27|      -19|
|   2005|   13|New Mexico State       |Utah State                  |          21|          24|       -3|
|   2005|   13|Virginia Tech          |North Carolina              |          30|           3|       27|
|   2005|   14|Akron                  |Northern Illinois           |          31|          30|        1|
|   2005|   14|Fresno State           |Louisiana Tech              |          28|          40|      -12|
|   2005|   14|Florida International  |Middle Tennessee State      |          35|          31|        4|
|   2005|   14|Florida State          |Virginia Tech               |          27|          22|        5|
|   2005|   14|Georgia                |Louisiana State             |          34|          14|       20|
|   2005|   14|Hawaii                 |San Diego State             |          49|          38|       11|
|   2005|   14|Connecticut            |Louisville                  |          20|          30|      -10|
|   2005|   14|Navy                   |Army                        |          42|          23|       19|
|   2005|   14|Southern California    |UCLA                        |          66|          19|       47|
|   2005|   14|Texas                  |Colorado                    |          70|           3|       67|
|   2005|   14|Central Florida        |Tulsa                       |          27|          44|      -17|
|   2005|   14|South Florida          |West Virginia               |          13|          28|      -15|
|   2005|   15|Southern Mississippi   |Arkansas State              |          31|          19|       12|
|   2005|   15|Toledo                 |Texas-El Paso               |          45|          13|       32|
|   2005|   15|California             |Brigham Young               |          35|          28|        7|
|   2005|   15|Navy                   |Colorado State              |          51|          30|       21|
|   2005|   15|Kansas                 |Houston                     |          42|          13|       29|
|   2005|   15|Nevada                 |Central Florida             |          49|          48|        1|
|   2005|   15|Memphis                |Akron                       |          38|          31|        7|
|   2005|   16|Arizona State          |Rutgers                     |          45|          40|        5|
|   2005|   16|Clemson                |Colorado                    |          19|          10|        9|
|   2005|   16|Boise State            |Boston College              |          21|          27|       -6|
|   2005|   16|Nebraska               |Michigan                    |          32|          28|        4|
|   2005|   16|Oklahoma               |Oregon                      |          17|          14|        3|
|   2005|   16|Utah                   |Georgia Tech                |          38|          10|       28|
|   2005|   16|Louisiana State        |Miami (FL)                  |          40|           3|       37|
|   2005|   16|Missouri               |South Carolina              |          38|          31|        7|
|   2005|   16|UCLA                   |Northwestern                |          50|          38|       12|
|   2005|   16|Virginia               |Minnesota                   |          34|          31|        3|
|   2005|   16|North Carolina State   |South Florida               |          14|           0|       14|
|   2005|   16|Texas Christian        |Iowa State                  |          27|          24|        3|
|   2005|   16|Tulsa                  |Fresno State                |          31|          24|        7|
|   2005|   17|Alabama                |Texas Tech                  |          13|          10|        3|
|   2005|   17|Florida                |Iowa                        |          31|          24|        7|
|   2005|   17|Ohio State             |Notre Dame                  |          34|          20|       14|
|   2005|   17|Virginia Tech          |Louisville                  |          35|          24|       11|
|   2005|   17|West Virginia          |Georgia                     |          38|          35|        3|
|   2005|   17|Wisconsin              |Auburn                      |          24|          10|       14|
|   2005|   18|Penn State             |Florida State               |          26|          23|        3|
|   2005|   18|Southern California    |Texas                       |          38|          41|       -3|
|   2006|    1|Arizona State          |Northern Arizona            |          35|          14|       21|
|   2006|    1|Ball State             |Eastern Michigan            |          38|          20|       18|
|   2006|    1|Boise State            |Sacramento State            |          45|           0|       45|
|   2006|    1|Central Michigan       |Boston College              |          24|          31|       -7|
|   2006|    1|Buffalo                |Temple                      |           9|           3|        6|
|   2006|    1|Connecticut            |Rhode Island                |          52|           7|       45|
|   2006|    1|Iowa State             |Toledo                      |          45|          43|        2|
|   2006|    1|Louisiana-Monroe       |Alcorn State                |          24|           6|       18|
|   2006|    1|Middle Tennessee State |Florida International       |           7|           6|        1|
|   2006|    1|Kent State             |Minnesota                   |           0|          44|      -44|
|   2006|    1|New Mexico State       |Southeastern Louisiana      |          30|          15|       15|
|   2006|    1|Miami (OH)             |Northwestern                |           3|          21|      -18|
|   2006|    1|Oregon State           |Eastern Washington          |          56|          17|       39|
|   2006|    1|Mississippi State      |South Carolina              |           0|          15|      -15|
|   2006|    1|San Diego State        |Texas-El Paso               |          27|          34|       -7|
|   2006|    1|Tulsa                  |Stephen F. Austin           |          45|           7|       38|
|   2006|    1|Fresno State           |Nevada                      |          28|          19|        9|
|   2006|    1|Alabama                |Hawaii                      |          25|          17|        8|
|   2006|    1|Arizona                |Brigham Young               |          16|          13|        3|
|   2006|    1|Arkansas State         |Army                        |          14|           6|        8|
|   2006|    1|Auburn                 |Washington State            |          40|          14|       26|
|   2006|    1|Central Florida        |Villanova                   |          35|          16|       19|
|   2006|    1|Cincinnati             |Eastern Kentucky            |          31|           0|       31|
|   2006|    1|Clemson                |Florida Atlantic            |          54|           6|       48|
|   2006|    1|Colorado State         |Weber State                 |          30|           6|       24|
|   2006|    1|Florida                |Southern Mississippi        |          34|           7|       27|
|   2006|    1|Georgia                |Western Kentucky            |          48|          12|       36|
|   2006|    1|Rice                   |Houston                     |          30|          31|       -1|
|   2006|    1|Illinois               |Eastern Illinois            |          42|          17|       25|
|   2006|    1|Indiana                |Western Michigan            |          39|          20|       19|
|   2006|    1|Iowa                   |Montana                     |          41|           7|       34|
|   2006|    1|Kansas                 |Northwestern State          |          49|          18|       31|
|   2006|    1|Kansas State           |Illinois State              |          24|          23|        1|
|   2006|    1|Louisiana State        |Louisiana-Lafayette         |          45|           3|       42|
|   2006|    1|Maryland               |William & Mary              |          27|          14|       13|
|   2006|    1|Michigan               |Vanderbilt                  |          27|           7|       20|
|   2006|    1|Michigan State         |Idaho                       |          27|          17|       10|
|   2006|    1|Missouri               |Murray State                |          47|           7|       40|
|   2006|    1|Colorado               |Montana State               |          10|          19|       -9|
|   2006|    1|Navy                   |East Carolina               |          28|          23|        5|
|   2006|    1|Nebraska               |Louisiana Tech              |          49|          10|       39|
|   2006|    1|Nevada-Las Vegas       |Idaho State                 |          54|          10|       44|
|   2006|    1|North Carolina State   |Appalachian State           |          23|          10|       13|
|   2006|    1|Georgia Tech           |Notre Dame                  |          10|          14|       -4|
|   2006|    1|Ohio                   |Tennessee-Martin            |          29|           3|       26|
|   2006|    1|Ohio State             |Northern Illinois           |          35|          12|       23|
|   2006|    1|Oklahoma               |Alabama-Birmingham          |          24|          17|        7|
|   2006|    1|Oklahoma State         |Missouri State              |          52|          10|       42|
|   2006|    1|Oregon                 |Stanford                    |          48|          10|       38|
|   2006|    1|Penn State             |Akron                       |          34|          16|       18|
|   2006|    1|Pittsburgh             |Virginia                    |          38|          13|       25|
|   2006|    1|New Mexico             |Portland State              |           6|          17|      -11|
|   2006|    1|Purdue                 |Indiana State               |          60|          35|       25|
|   2006|    1|Duke                   |Richmond                    |           0|          13|      -13|
|   2006|    1|North Carolina         |Rutgers                     |          16|          21|       -5|
|   2006|    1|South Florida          |McNeese State               |          41|          10|       31|
|   2006|    1|Arkansas               |Southern California         |          14|          50|      -36|
|   2006|    1|Tennessee              |California                  |          35|          18|       17|
|   2006|    1|Texas                  |North Texas                 |          56|           7|       49|
|   2006|    1|Texas A&M              |Citadel                     |          35|           3|       32|
|   2006|    1|Texas Tech             |Southern Methodist          |          35|           3|       32|
|   2006|    1|Troy                   |Alabama State               |          38|           0|       38|
|   2006|    1|UCLA                   |Utah                        |          31|          10|       21|
|   2006|    1|Virginia Tech          |Northeastern                |          38|           0|       38|
|   2006|    1|Wake Forest            |Syracuse                    |          20|          10|       10|
|   2006|    1|Washington             |San Jose State              |          35|          29|        6|
|   2006|    1|West Virginia          |Marshall                    |          42|          10|       32|
|   2006|    1|Bowling Green State    |Wisconsin                   |          14|          35|      -21|
|   2006|    1|Wyoming                |Utah State                  |          38|           7|       31|
|   2006|    1|Louisville             |Kentucky                    |          59|          28|       31|
|   2006|    1|Mississippi            |Memphis                     |          28|          25|        3|
|   2006|    1|Baylor                 |Texas Christian             |           7|          17|      -10|
|   2006|    1|Miami (FL)             |Florida State               |          10|          13|       -3|
|   2006|    2|Boise State            |Oregon State                |          42|          14|       28|
|   2006|    2|Cincinnati             |Pittsburgh                  |          15|          33|      -18|
|   2006|    2|North Carolina State   |Akron                       |          17|          20|       -3|
|   2006|    2|Alabama                |Vanderbilt                  |          13|          10|        3|
|   2006|    2|Alabama-Birmingham     |East Carolina               |          17|          12|        5|
|   2006|    2|Arizona State          |Nevada                      |          52|          21|       31|
|   2006|    2|Arkansas               |Utah State                  |          20|           0|       20|
|   2006|    2|Army                   |Kent State                  |          17|          14|        3|
|   2006|    2|Mississippi State      |Auburn                      |           0|          34|      -34|
|   2006|    2|Baylor                 |Northwestern State          |          47|          10|       37|
|   2006|    2|Boston College         |Clemson                     |          34|          33|        1|
|   2006|    2|Bowling Green State    |Buffalo                     |          48|          40|        8|
|   2006|    2|Brigham Young          |Tulsa                       |          49|          24|       25|
|   2006|    2|California             |Minnesota                   |          42|          17|       25|
|   2006|    2|Colorado State         |Colorado                    |          14|          10|        4|
|   2006|    2|Florida                |Central Florida             |          42|           0|       42|
|   2006|    2|Florida State          |Troy                        |          24|          17|        7|
|   2006|    2|South Carolina         |Georgia                     |           0|          18|      -18|
|   2006|    2|Georgia Tech           |Samford                     |          38|           6|       32|
|   2006|    2|Houston                |Tulane                      |          45|           7|       38|
|   2006|    2|Ball State             |Indiana                     |          23|          24|       -1|
|   2006|    2|Syracuse               |Iowa                        |          13|          20|       -7|
|   2006|    2|Iowa State             |Nevada-Las Vegas            |          16|          10|        6|
|   2006|    2|Kansas                 |Louisiana-Monroe            |          21|          19|        2|
|   2006|    2|Kansas State           |Florida Atlantic            |          45|           0|       45|
|   2006|    2|Kentucky               |Texas State                 |          41|           7|       34|
|   2006|    2|Louisiana State        |Arizona                     |          45|           3|       42|
|   2006|    2|Temple                 |Louisville                  |           0|          62|      -62|
|   2006|    2|Marshall               |Hofstra                     |          54|          31|       23|
|   2006|    2|Maryland               |Middle Tennessee State      |          24|          10|       14|
|   2006|    2|Memphis                |Chattanooga                 |          33|          14|       19|
|   2006|    2|Miami (FL)             |Florida A&M                 |          51|          10|       41|
|   2006|    2|Michigan               |Central Michigan            |          41|          17|       24|
|   2006|    2|Michigan State         |Eastern Michigan            |          52|          20|       32|
|   2006|    2|Missouri               |Mississippi                 |          34|           7|       27|
|   2006|    2|Navy                   |Massachusetts               |          21|          20|        1|
|   2006|    2|Nebraska               |Nicholls State              |          56|           7|       49|
|   2006|    2|Northwestern           |New Hampshire               |          17|          34|      -17|
|   2006|    2|New Mexico State       |New Mexico                  |          28|          34|       -6|
|   2006|    2|North Texas            |Southern Methodist          |          24|           6|       18|
|   2006|    2|Notre Dame             |Penn State                  |          41|          17|       24|
|   2006|    2|Northern Illinois      |Ohio                        |          23|          35|      -12|
|   2006|    2|Texas                  |Ohio State                  |           7|          24|      -17|
|   2006|    2|Oklahoma               |Washington                  |          37|          20|       17|
|   2006|    2|Arkansas State         |Oklahoma State              |           7|          35|      -28|
|   2006|    2|Fresno State           |Oregon                      |          24|          31|       -7|
|   2006|    2|Purdue                 |Miami (OH)                  |          38|          31|        7|
|   2006|    2|Rutgers                |Illinois                    |          33|           0|       33|
|   2006|    2|San Jose State         |Stanford                    |          35|          34|        1|
|   2006|    2|South Florida          |Florida International       |          21|          20|        1|
|   2006|    2|Southern Mississippi   |Southeastern Louisiana      |          45|           0|       45|
|   2006|    2|Tennessee              |Air Force                   |          31|          30|        1|
|   2006|    2|Texas A&M              |Louisiana-Lafayette         |          51|           7|       44|
|   2006|    2|Texas Christian        |California-Davis            |          46|          13|       33|
|   2006|    2|Texas-El Paso          |Texas Tech                  |          35|          38|       -3|
|   2006|    2|UCLA                   |Rice                        |          26|          16|       10|
|   2006|    2|Utah                   |Northern Arizona            |          45|           7|       38|
|   2006|    2|Virginia               |Wyoming                     |          13|          12|        1|
|   2006|    2|North Carolina         |Virginia Tech               |          10|          35|      -25|
|   2006|    2|Wake Forest            |Duke                        |          14|          13|        1|
|   2006|    2|Washington State       |Idaho                       |          56|          10|       46|
|   2006|    2|West Virginia          |Eastern Washington          |          52|           3|       49|
|   2006|    2|Western Michigan       |Toledo                      |          31|          10|       21|
|   2006|    2|Wisconsin              |Western Illinois            |          34|          10|       24|
|   2006|    3|Middle Tennessee State |Tennessee Tech              |          44|           0|       44|
|   2006|    3|West Virginia          |Maryland                    |          45|          24|       21|
|   2006|    3|Toledo                 |Kansas                      |          37|          31|        6|
|   2006|    3|Alabama                |Louisiana-Monroe            |          41|           7|       34|
|   2006|    3|Arizona                |Stephen F. Austin           |          28|          10|       18|
|   2006|    3|Colorado               |Arizona State               |           3|          21|      -18|
|   2006|    3|Vanderbilt             |Arkansas                    |          19|          21|       -2|
|   2006|    3|Auburn                 |Louisiana State             |           7|           3|        4|
|   2006|    3|Wyoming                |Boise State                 |          10|          17|       -7|
|   2006|    3|Boston College         |Brigham Young               |          30|          23|        7|
|   2006|    3|Florida International  |Bowling Green State         |          28|          33|       -5|
|   2006|    3|California             |Portland State              |          42|          16|       26|
|   2006|    3|Central Michigan       |Akron                       |          24|          21|        3|
|   2006|    3|Florida State          |Clemson                     |          20|          27|       -7|
|   2006|    3|East Carolina          |Memphis                     |          35|          20|       15|
|   2006|    3|Tennessee              |Florida                     |          20|          21|       -1|
|   2006|    3|Georgia                |Alabama-Birmingham          |          34|           0|       34|
|   2006|    3|Georgia Tech           |Troy                        |          35|          20|       15|
|   2006|    3|Hawaii                 |Nevada-Las Vegas            |          42|          13|       29|
|   2006|    3|Houston                |Grambling State             |          42|          22|       20|
|   2006|    3|Idaho                  |Idaho State                 |          27|          24|        3|
|   2006|    3|Iowa                   |Iowa State                  |          27|          17|       10|
|   2006|    3|Kansas State           |Marshall                    |          23|           7|       16|
|   2006|    3|Miami (OH)             |Kent State                  |          14|          16|       -2|
|   2006|    3|Kentucky               |Mississippi                 |          31|          14|       17|
|   2006|    3|Louisiana Tech         |Nicholls State              |          31|          21|       10|
|   2006|    3|Louisville             |Miami (FL)                  |          31|           7|       24|
|   2006|    3|Notre Dame             |Michigan                    |          21|          47|      -26|
|   2006|    3|Pittsburgh             |Michigan State              |          23|          38|      -15|
|   2006|    3|Minnesota              |Temple                      |          62|           0|       62|
|   2006|    3|New Mexico             |Missouri                    |          17|          27|      -10|
|   2006|    3|Stanford               |Navy                        |           9|          37|      -28|
|   2006|    3|Nevada                 |Colorado State              |          28|          10|       18|
|   2006|    3|New Mexico State       |Texas Southern              |          48|          14|       34|
|   2006|    3|North Carolina         |Furman                      |          45|          42|        3|
|   2006|    3|Northern Illinois      |Buffalo                     |          31|          13|       18|
|   2006|    3|Northwestern           |Eastern Michigan            |          14|           6|        8|
|   2006|    3|Ohio State             |Cincinnati                  |          37|           7|       30|
|   2006|    3|Oklahoma State         |Florida Atlantic            |          48|           8|       40|
|   2006|    3|Oregon                 |Oklahoma                    |          34|          33|        1|
|   2006|    3|Penn State             |Youngstown State            |          37|           3|       34|
|   2006|    3|Purdue                 |Ball State                  |          38|          28|       10|
|   2006|    3|Rutgers                |Ohio                        |          24|           7|       17|
|   2006|    3|South Carolina         |Wofford                     |          27|          20|        7|
|   2006|    3|Central Florida        |South Florida               |          17|          24|       -7|
|   2006|    3|Southern California    |Nebraska                    |          28|          10|       18|
|   2006|    3|Indiana                |Southern Illinois           |          28|          35|       -7|
|   2006|    3|Southern Methodist     |Sam Houston State           |          45|          14|       31|
|   2006|    3|Southern Mississippi   |North Carolina State        |          37|          17|       20|
|   2006|    3|Illinois               |Syracuse                    |          21|          31|      -10|
|   2006|    3|Rice                   |Texas                       |           7|          52|      -45|
|   2006|    3|Texas A&M              |Army                        |          28|          24|        4|
|   2006|    3|Texas Christian        |Texas Tech                  |          12|           3|        9|
|   2006|    3|Mississippi State      |Tulane                      |          29|          32|       -3|
|   2006|    3|Tulsa                  |North Texas                 |          28|           3|       25|
|   2006|    3|Utah State             |Utah                        |           0|          48|      -48|
|   2006|    3|Virginia Tech          |Duke                        |          36|           0|       36|
|   2006|    3|Connecticut            |Wake Forest                 |          13|          24|      -11|
|   2006|    3|Washington             |Fresno State                |          21|          20|        1|
|   2006|    3|Washington State       |Baylor                      |          17|          15|        2|
|   2006|    3|Virginia               |Western Michigan            |          10|          17|       -7|
|   2006|    3|Wisconsin              |San Diego State             |          14|           0|       14|
|   2006|    4|Georgia Tech           |Virginia                    |          24|           7|       17|
|   2006|    4|Nevada                 |Northwestern                |          31|          21|       10|
|   2006|    4|Wyoming                |Air Force                   |          24|          31|       -7|
|   2006|    4|Akron                  |North Texas                 |          33|          13|       20|
|   2006|    4|Arkansas               |Alabama                     |          24|          23|        1|
|   2006|    4|Baylor                 |Army                        |          20|          27|       -7|
|   2006|    4|Auburn                 |Buffalo                     |          38|           7|       31|
|   2006|    4|Boise State            |Hawaii                      |          41|          34|        7|
|   2006|    4|Brigham Young          |Utah State                  |          38|           0|       38|
|   2006|    4|California             |Arizona State               |          49|          21|       28|
|   2006|    4|Eastern Michigan       |Central Michigan            |          17|          24|       -7|
|   2006|    4|Clemson                |North Carolina              |          52|           7|       45|
|   2006|    4|Indiana                |Connecticut                 |           7|          14|       -7|
|   2006|    4|Florida                |Kentucky                    |          26|           7|       19|
|   2006|    4|Florida State          |Rice                        |          55|           7|       48|
|   2006|    4|Georgia                |Colorado                    |          14|          13|        1|
|   2006|    4|Houston                |Oklahoma State              |          34|          25|        9|
|   2006|    4|Illinois               |Iowa                        |           7|          24|      -17|
|   2006|    4|Kansas                 |South Florida               |          13|           7|        6|
|   2006|    4|Bowling Green State    |Kent State                  |           3|          38|      -35|
|   2006|    4|Louisiana State        |Tulane                      |          49|           7|       42|
|   2006|    4|Louisiana-Lafayette    |North Carolina A&T          |          48|           7|       41|
|   2006|    4|Kansas State           |Louisville                  |           6|          24|      -18|
|   2006|    4|Maryland               |Florida International       |          14|          10|        4|
|   2006|    4|Michigan               |Wisconsin                   |          27|          13|       14|
|   2006|    4|Alabama-Birmingham     |Mississippi State           |          10|          16|       -6|
|   2006|    4|Missouri               |Ohio                        |          31|           6|       25|
|   2006|    4|Nebraska               |Troy                        |          56|           0|       56|
|   2006|    4|New Mexico             |Texas-El Paso               |          26|          13|       13|
|   2006|    4|North Carolina State   |Boston College              |          17|          15|        2|
|   2006|    4|Ball State             |North Dakota State          |          24|          29|       -5|
|   2006|    4|Northern Illinois      |Indiana State               |          48|          14|       34|
|   2006|    4|Michigan State         |Notre Dame                  |          37|          40|       -3|
|   2006|    4|Ohio State             |Penn State                  |          28|           6|       22|
|   2006|    4|Oklahoma               |Middle Tennessee State      |          59|           0|       59|
|   2006|    4|Oregon State           |Idaho                       |          38|           0|       38|
|   2006|    4|Pittsburgh             |Citadel                     |          51|           6|       45|
|   2006|    4|Purdue                 |Minnesota                   |          27|          21|        6|
|   2006|    4|Rutgers                |Howard                      |          56|           7|       49|
|   2006|    4|San Jose State         |Cal Poly                    |          17|           7|       10|
|   2006|    4|South Carolina         |Florida Atlantic            |          45|           6|       39|
|   2006|    4|Arizona                |Southern California         |           3|          20|      -17|
|   2006|    4|Southern Methodist     |Arkansas State              |          55|           9|       46|
|   2006|    4|Syracuse               |Miami (OH)                  |          34|          14|       20|
|   2006|    4|Tennessee              |Marshall                    |          33|           7|       26|
|   2006|    4|Texas                  |Iowa State                  |          37|          14|       23|
|   2006|    4|Texas A&M              |Louisiana Tech              |          45|          14|       31|
|   2006|    4|Texas Tech             |Southeastern Louisiana      |          62|           0|       62|
|   2006|    4|Toledo                 |McNeese State               |          41|           7|       34|
|   2006|    4|Navy                   |Tulsa                       |          23|          24|       -1|
|   2006|    4|San Diego State        |Utah                        |           7|          38|      -31|
|   2006|    4|Vanderbilt             |Tennessee State             |          38|           9|       29|
|   2006|    4|Virginia Tech          |Cincinnati                  |          29|          13|       16|
|   2006|    4|Mississippi            |Wake Forest                 |           3|          27|      -24|
|   2006|    4|Washington             |UCLA                        |          29|          19|       10|
|   2006|    4|Stanford               |Washington State            |          10|          36|      -26|
|   2006|    4|East Carolina          |West Virginia               |          10|          27|      -17|
|   2006|    4|Western Michigan       |Temple                      |          41|           7|       34|
|   2006|    5|Central Florida        |Southern Mississippi        |          14|          19|       -5|
|   2006|    5|South Carolina         |Auburn                      |          17|          24|       -7|
|   2006|    5|Texas Christian        |Brigham Young               |          17|          31|      -14|
|   2006|    5|South Florida          |Rutgers                     |          20|          22|       -2|
|   2006|    5|Air Force              |New Mexico                  |          24|           7|       17|
|   2006|    5|Alabama-Birmingham     |Troy                        |          21|           3|       18|
|   2006|    5|Florida International  |Arkansas State              |           6|          31|      -25|
|   2006|    5|Baylor                 |Kansas State                |          17|           3|       14|
|   2006|    5|Utah                   |Boise State                 |           3|          36|      -33|
|   2006|    5|Boston College         |Maine                       |          22|           0|       22|
|   2006|    5|Ohio                   |Bowling Green State         |           9|          21|      -12|
|   2006|    5|Oregon State           |California                  |          13|          41|      -28|
|   2006|    5|Cincinnati             |Miami (OH)                  |          24|          10|       14|
|   2006|    5|Clemson                |Louisiana Tech              |          51|           0|       51|
|   2006|    5|Fresno State           |Colorado State              |          23|          35|      -12|
|   2006|    5|Florida                |Alabama                     |          28|          13|       15|
|   2006|    5|Louisiana-Monroe       |Florida Atlantic            |          19|          21|       -2|
|   2006|    5|Mississippi            |Georgia                     |           9|          14|       -5|
|   2006|    5|Virginia Tech          |Georgia Tech                |          27|          38|      -11|
|   2006|    5|Hawaii                 |Eastern Illinois            |          44|           9|       35|
|   2006|    5|Utah State             |Idaho                       |          21|          41|      -20|
|   2006|    5|Michigan State         |Illinois                    |          20|          23|       -3|
|   2006|    5|Iowa State             |Northern Iowa               |          28|          27|        1|
|   2006|    5|Kent State             |Akron                       |          37|          15|       22|
|   2006|    5|Kentucky               |Central Michigan            |          45|          36|        9|
|   2006|    5|Louisiana State        |Mississippi State           |          48|          17|       31|
|   2006|    5|Louisiana-Lafayette    |Eastern Michigan            |          33|          14|       19|
|   2006|    5|Miami (FL)             |Houston                     |          14|          13|        1|
|   2006|    5|Minnesota              |Michigan                    |          14|          28|      -14|
|   2006|    5|North Texas            |Middle Tennessee State      |           0|          35|      -35|
|   2006|    5|Missouri               |Colorado                    |          28|          13|       15|
|   2006|    5|Connecticut            |Navy                        |          17|          41|      -24|
|   2006|    5|Nebraska               |Kansas                      |          39|          32|        7|
|   2006|    5|Nevada-Las Vegas       |Nevada                      |           3|          31|      -28|
|   2006|    5|Ball State             |Northern Illinois           |          28|          40|      -12|
|   2006|    5|Notre Dame             |Purdue                      |          35|          21|       14|
|   2006|    5|Iowa                   |Ohio State                  |          17|          38|      -21|
|   2006|    5|Arizona State          |Oregon                      |          13|          48|      -35|
|   2006|    5|Penn State             |Northwestern                |          33|           7|       26|
|   2006|    5|Pittsburgh             |Toledo                      |          45|           3|       42|
|   2006|    5|Army                   |Rice                        |          14|          48|      -34|
|   2006|    5|San Jose State         |San Diego State             |          31|          10|       21|
|   2006|    5|Washington State       |Southern California         |          22|          28|       -6|
|   2006|    5|Tulane                 |Southern Methodist          |          28|          33|       -5|
|   2006|    5|Syracuse               |Wyoming                     |          40|          34|        6|
|   2006|    5|Memphis                |Tennessee                   |           7|          41|      -34|
|   2006|    5|Texas                  |Sam Houston State           |          56|           3|       53|
|   2006|    5|Texas A&M              |Texas Tech                  |          27|          31|       -4|
|   2006|    5|Texas-El Paso          |New Mexico State            |          44|          38|        6|
|   2006|    5|UCLA                   |Stanford                    |          31|           0|       31|
|   2006|    5|Vanderbilt             |Temple                      |          43|          14|       29|
|   2006|    5|Duke                   |Virginia                    |           0|          37|      -37|
|   2006|    5|Wake Forest            |Liberty                     |          34|          14|       20|
|   2006|    5|Arizona                |Washington                  |          10|          21|      -11|
|   2006|    5|Indiana                |Wisconsin                   |          17|          52|      -35|
|   2006|    6|Tulsa                  |Southern Mississippi        |          20|           6|       14|
|   2006|    6|Marshall               |Central Florida             |          22|          23|       -1|
|   2006|    6|North Carolina State   |Florida State               |          24|          20|        4|
|   2006|    6|Utah                   |Texas Christian             |          20|           7|       13|
|   2006|    6|Middle Tennessee State |Louisville                  |          17|          44|      -27|
|   2006|    6|Alabama                |Duke                        |          30|          14|       16|
|   2006|    6|Alabama-Birmingham     |Memphis                     |          35|          29|        6|
|   2006|    6|Auburn                 |Arkansas                    |          10|          27|      -17|
|   2006|    6|Arkansas State         |Louisiana-Monroe            |          10|           6|        4|
|   2006|    6|Army                   |Virginia Military Institute |          62|           7|       55|
|   2006|    6|Buffalo                |Ball State                  |          25|          55|      -30|
|   2006|    6|Colorado               |Baylor                      |          31|          34|       -3|
|   2006|    6|Boise State            |Louisiana Tech              |          55|          14|       41|
|   2006|    6|Brigham Young          |San Diego State             |          47|          17|       30|
|   2006|    6|California             |Oregon                      |          45|          24|       21|
|   2006|    6|Toledo                 |Central Michigan            |          20|          42|      -22|
|   2006|    6|Cincinnati             |Akron                       |          20|          14|        6|
|   2006|    6|Wake Forest            |Clemson                     |          17|          27|      -10|
|   2006|    6|Colorado State         |Nevada-Las Vegas            |          28|           7|       21|
|   2006|    6|East Carolina          |Virginia                    |          31|          21|       10|
|   2006|    6|Florida                |Louisiana State             |          23|          10|       13|
|   2006|    6|Georgia Tech           |Maryland                    |          27|          23|        4|
|   2006|    6|Hawaii                 |Nevada                      |          41|          34|        7|
|   2006|    6|Idaho                  |New Mexico State            |          28|          20|        8|
|   2006|    6|Illinois               |Indiana                     |          32|          34|       -2|
|   2006|    6|Iowa                   |Purdue                      |          47|          17|       30|
|   2006|    6|Kansas State           |Oklahoma State              |          31|          27|        4|
|   2006|    6|Temple                 |Kent State                  |          17|          28|      -11|
|   2006|    6|Houston                |Louisiana-Lafayette         |          28|          31|       -3|
|   2006|    6|Miami (FL)             |North Carolina              |          27|           7|       20|
|   2006|    6|Michigan               |Michigan State              |          31|          13|       18|
|   2006|    6|Mississippi            |Vanderbilt                  |          17|          10|        7|
|   2006|    6|Texas Tech             |Missouri                    |          21|          38|      -17|
|   2006|    6|Air Force              |Navy                        |          17|          24|       -7|
|   2006|    6|Iowa State             |Nebraska                    |          14|          28|      -14|
|   2006|    6|North Texas            |Florida International       |          25|          22|        3|
|   2006|    6|Notre Dame             |Stanford                    |          31|          10|       21|
|   2006|    6|Ohio                   |Western Michigan            |          27|          20|        7|
|   2006|    6|Ohio State             |Bowling Green State         |          35|           7|       28|
|   2006|    6|Minnesota              |Penn State                  |          27|          28|       -1|
|   2006|    6|Syracuse               |Pittsburgh                  |          11|          21|      -10|
|   2006|    6|Kentucky               |South Carolina              |          17|          24|       -7|
|   2006|    6|South Florida          |Connecticut                 |          38|          16|       22|
|   2006|    6|Southern California    |Washington                  |          26|          20|        6|
|   2006|    6|Georgia                |Tennessee                   |          33|          51|      -18|
|   2006|    6|Texas                  |Oklahoma                    |          28|          10|       18|
|   2006|    6|Kansas                 |Texas A&M                   |          18|          21|       -3|
|   2006|    6|Texas-El Paso          |Southern Methodist          |          24|          21|        3|
|   2006|    6|Tulane                 |Rice                        |          38|          24|       14|
|   2006|    6|UCLA                   |Arizona                     |          27|           7|       20|
|   2006|    6|Utah State             |Fresno State                |          13|          12|        1|
|   2006|    6|Oregon State           |Washington State            |           6|          13|       -7|
|   2006|    6|Mississippi State      |West Virginia               |          14|          42|      -28|
|   2006|    6|Wisconsin              |Northwestern                |          41|           9|       32|
|   2006|    6|New Mexico             |Wyoming                     |          10|          14|       -4|
|   2006|    6|Miami (OH)             |Northern Illinois           |          25|          28|       -3|
|   2006|    7|Air Force              |Colorado State              |          24|          21|        3|
|   2006|    7|Boston College         |Virginia Tech               |          22|           3|       19|
|   2006|    7|Clemson                |Temple                      |          63|           9|       54|
|   2006|    7|Florida Atlantic       |Southern Utah               |          32|           7|       25|
|   2006|    7|Central Florida        |Pittsburgh                  |           7|          52|      -45|
|   2006|    7|Alabama                |Mississippi                 |          26|          23|        3|
|   2006|    7|Stanford               |Arizona                     |           7|          20|      -13|
|   2006|    7|Arkansas               |Southeast Missouri State    |          63|           7|       56|
|   2006|    7|Memphis                |Arkansas State              |          23|          26|       -3|
|   2006|    7|Auburn                 |Florida                     |          27|          17|       10|
|   2006|    7|Bowling Green State    |Eastern Michigan            |          24|          21|        3|
|   2006|    7|Washington State       |California                  |           3|          21|      -18|
|   2006|    7|Central Michigan       |Ball State                  |          18|           7|       11|
|   2006|    7|Colorado               |Texas Tech                  |          30|           6|       24|
|   2006|    7|Connecticut            |Army                        |          21|           7|       14|
|   2006|    7|Duke                   |Florida State               |          24|          51|      -27|
|   2006|    7|Fresno State           |Hawaii                      |          37|          68|      -31|
|   2006|    7|Louisiana Tech         |Idaho                       |          14|          24|      -10|
|   2006|    7|Indiana                |Iowa                        |          31|          28|        3|
|   2006|    7|Kent State             |Toledo                      |          40|          14|       26|
|   2006|    7|Louisiana State        |Kentucky                    |          49|           0|       49|
|   2006|    7|Louisville             |Cincinnati                  |          23|          17|        6|
|   2006|    7|Virginia               |Maryland                    |          26|          28|       -2|
|   2006|    7|Miami (FL)             |Florida International       |          35|           0|       35|
|   2006|    7|Penn State             |Michigan                    |          10|          17|       -7|
|   2006|    7|Mississippi State      |Jacksonville State          |          35|           3|       32|
|   2006|    7|Kansas State           |Nebraska                    |           3|          21|      -18|
|   2006|    7|Nevada-Las Vegas       |New Mexico                  |          36|          39|       -3|
|   2006|    7|Illinois               |Ohio                        |          17|          20|       -3|
|   2006|    7|Michigan State         |Ohio State                  |           7|          38|      -31|
|   2006|    7|Oklahoma               |Iowa State                  |          34|           9|       25|
|   2006|    7|Kansas                 |Oklahoma State              |          32|          42|      -10|
|   2006|    7|Oregon                 |UCLA                        |          30|          20|       10|
|   2006|    7|Washington             |Oregon State                |          17|          27|      -10|
|   2006|    7|Northwestern           |Purdue                      |          10|          31|      -21|
|   2006|    7|Rice                   |Alabama-Birmingham          |          34|          33|        1|
|   2006|    7|Navy                   |Rutgers                     |           0|          34|      -34|
|   2006|    7|San Jose State         |Utah State                  |          21|          14|        7|
|   2006|    7|North Carolina         |South Florida               |          20|          37|      -17|
|   2006|    7|Southern California    |Arizona State               |          28|          21|        7|
|   2006|    7|Southern Methodist     |Marshall                    |          31|          21|       10|
|   2006|    7|Southern Mississippi   |Houston                     |          31|          27|        4|
|   2006|    7|Texas                  |Baylor                      |          63|          31|       32|
|   2006|    7|Texas A&M              |Missouri                    |          25|          19|        6|
|   2006|    7|Texas-El Paso          |Tulane                      |          34|          20|       14|
|   2006|    7|Troy                   |Louisiana-Monroe            |          24|          19|        5|
|   2006|    7|East Carolina          |Tulsa                       |          10|          31|      -21|
|   2006|    7|Georgia                |Vanderbilt                  |          22|          24|       -2|
|   2006|    7|North Carolina State   |Wake Forest                 |          23|          25|       -2|
|   2006|    7|West Virginia          |Syracuse                    |          41|          17|       24|
|   2006|    7|Western Michigan       |Northern Illinois           |          16|          14|        2|
|   2006|    7|Wisconsin              |Minnesota                   |          48|          12|       36|
|   2006|    7|Wyoming                |Utah                        |          31|          15|       16|
|   2006|    7|New Mexico State       |Boise State                 |          28|          40|      -12|
|   2006|    7|Buffalo                |Miami (OH)                  |          31|          38|       -7|
|   2006|    8|Florida Atlantic       |Louisiana-Lafayette         |           0|           6|       -6|
|   2006|    8|Central Michigan       |Bowling Green State         |          31|          14|       17|
|   2006|    8|New Mexico             |Utah                        |          34|          31|        3|
|   2006|    8|Virginia               |North Carolina              |          23|           0|       23|
|   2006|    8|Connecticut            |West Virginia               |          11|          37|      -26|
|   2006|    8|Akron                  |Miami (OH)                  |          24|          13|       11|
|   2006|    8|Arizona State          |Stanford                    |          38|           3|       35|
|   2006|    8|Arkansas               |Mississippi                 |          38|           3|       35|
|   2006|    8|Arkansas State         |North Texas                 |          29|          10|       19|
|   2006|    8|Auburn                 |Tulane                      |          38|          13|       25|
|   2006|    8|Baylor                 |Kansas                      |          36|          35|        1|
|   2006|    8|Idaho                  |Boise State                 |          26|          42|      -16|
|   2006|    8|Florida State          |Boston College              |          19|          24|       -5|
|   2006|    8|Brigham Young          |Nevada-Las Vegas            |          52|           7|       45|
|   2006|    8|California             |Washington                  |          31|          24|        7|
|   2006|    8|Clemson                |Georgia Tech                |          31|           7|       24|
|   2006|    8|East Carolina          |Southern Methodist          |          38|          21|       17|
|   2006|    8|Eastern Michigan       |Toledo                      |          17|          13|        4|
|   2006|    8|Georgia                |Mississippi State           |          27|          24|        3|
|   2006|    8|New Mexico State       |Hawaii                      |          30|          49|      -19|
|   2006|    8|Houston                |Texas-El Paso               |          34|          17|       17|
|   2006|    8|Louisiana State        |Fresno State                |          38|           6|       32|
|   2006|    8|Louisiana Tech         |Utah State                  |          48|          35|       13|
|   2006|    8|Syracuse               |Louisville                  |          13|          28|      -15|
|   2006|    8|Alabama-Birmingham     |Marshall                    |          24|          31|       -7|
|   2006|    8|Maryland               |North Carolina State        |          26|          20|        6|
|   2006|    8|Duke                   |Miami (FL)                  |          15|          20|       -5|
|   2006|    8|Michigan               |Iowa                        |          20|           6|       14|
|   2006|    8|Northwestern           |Michigan State              |          38|          41|       -3|
|   2006|    8|Louisiana-Monroe       |Middle Tennessee State      |          21|          35|      -14|
|   2006|    8|Minnesota              |North Dakota State          |          10|           9|        1|
|   2006|    8|Missouri               |Kansas State                |          41|          21|       20|
|   2006|    8|Nevada                 |San Jose State              |          23|           7|       16|
|   2006|    8|Northern Illinois      |Temple                      |          43|          21|       22|
|   2006|    8|Notre Dame             |UCLA                        |          20|          17|        3|
|   2006|    8|Ohio                   |Buffalo                     |          42|           7|       35|
|   2006|    8|Ohio State             |Indiana                     |          44|           3|       41|
|   2006|    8|Oklahoma               |Colorado                    |          24|           3|       21|
|   2006|    8|Arizona                |Oregon State                |          10|          17|       -7|
|   2006|    8|Penn State             |Illinois                    |          26|          12|       14|
|   2006|    8|Central Florida        |Rice                        |          29|          40|      -11|
|   2006|    8|Pittsburgh             |Rutgers                     |          10|          20|      -10|
|   2006|    8|San Diego State        |Air Force                   |          19|          12|        7|
|   2006|    8|Vanderbilt             |South Carolina              |          13|          31|      -18|
|   2006|    8|Tennessee              |Alabama                     |          16|          13|        3|
|   2006|    8|Nebraska               |Texas                       |          20|          22|       -2|
|   2006|    8|Oklahoma State         |Texas A&M                   |          33|          34|       -1|
|   2006|    8|Army                   |Texas Christian             |          17|          31|      -14|
|   2006|    8|Iowa State             |Texas Tech                  |          26|          42|      -16|
|   2006|    8|Memphis                |Tulsa                       |          14|          35|      -21|
|   2006|    8|Virginia Tech          |Southern Mississippi        |          36|           6|       30|
|   2006|    8|Washington State       |Oregon                      |          34|          23|       11|
|   2006|    8|Ball State             |Western Michigan            |          27|          41|      -14|
|   2006|    8|Purdue                 |Wisconsin                   |           3|          24|      -21|
|   2006|    8|Wyoming                |Colorado State              |          24|           0|       24|
|   2006|    8|Cincinnati             |South Florida               |          23|           6|       17|
|   2006|    9|Virginia Tech          |Clemson                     |          24|           7|       17|
|   2006|    9|Tulsa                  |Texas-El Paso               |          30|          20|       10|
|   2006|    9|Alabama                |Florida International       |          38|           3|       35|
|   2006|    9|Washington             |Arizona State               |          23|          26|       -3|
|   2006|    9|Arkansas               |Louisiana-Monroe            |          44|          10|       34|
|   2006|    9|Mississippi            |Auburn                      |          17|          23|       -6|
|   2006|    9|Miami (OH)             |Ball State                  |          17|          20|       -3|
|   2006|    9|Boston College         |Buffalo                     |          41|           0|       41|
|   2006|    9|Air Force              |Brigham Young               |          14|          33|      -19|
|   2006|    9|San Diego State        |Cal Poly                    |          14|          16|       -2|
|   2006|    9|Cincinnati             |Syracuse                    |          17|           3|       14|
|   2006|    9|Southern Mississippi   |East Carolina               |          17|          20|       -3|
|   2006|    9|Florida                |Georgia                     |          21|          14|        7|
|   2006|    9|Florida Atlantic       |Arkansas State              |          29|           0|       29|
|   2006|    9|Georgia Tech           |Miami (FL)                  |          30|          23|        7|
|   2006|    9|Hawaii                 |Idaho                       |          68|          10|       58|
|   2006|    9|Houston                |Central Florida             |          51|          31|       20|
|   2006|    9|Indiana                |Michigan State              |          46|          21|       25|
|   2006|    9|Iowa                   |Northern Illinois           |          24|          14|       10|
|   2006|    9|Kansas                 |Colorado                    |          20|          15|        5|
|   2006|    9|Kansas State           |Iowa State                  |          31|          10|       21|
|   2006|    9|Mississippi State      |Kentucky                    |          31|          34|       -3|
|   2006|    9|Marshall               |Memphis                     |          41|          27|       14|
|   2006|    9|Maryland               |Florida State               |          27|          24|        3|
|   2006|    9|Michigan               |Northwestern                |          17|           3|       14|
|   2006|    9|Louisiana-Lafayette    |Middle Tennessee State      |          20|          34|      -14|
|   2006|    9|Nevada                 |New Mexico State            |          48|          21|       27|
|   2006|    9|Colorado State         |New Mexico                  |          19|          20|       -1|
|   2006|    9|Navy                   |Notre Dame                  |          14|          38|      -24|
|   2006|    9|Kent State             |Ohio                        |           7|          17|      -10|
|   2006|    9|Ohio State             |Minnesota                   |          44|           0|       44|
|   2006|    9|Missouri               |Oklahoma                    |          10|          26|      -16|
|   2006|    9|Oklahoma State         |Nebraska                    |          41|          29|       12|
|   2006|    9|Oregon                 |Portland State              |          55|          12|       43|
|   2006|    9|Oregon State           |Southern California         |          33|          31|        2|
|   2006|    9|Purdue                 |Penn State                  |           0|          12|      -12|
|   2006|    9|San Jose State         |Louisiana Tech              |          44|          10|       34|
|   2006|    9|Temple                 |Bowling Green State         |          28|          14|       14|
|   2006|    9|South Carolina         |Tennessee                   |          24|          31|       -7|
|   2006|    9|Texas Tech             |Texas                       |          31|          35|       -4|
|   2006|    9|Baylor                 |Texas A&M                   |          21|          31|      -10|
|   2006|    9|Texas Christian        |Wyoming                     |          26|           3|       23|
|   2006|    9|Toledo                 |Akron                       |          35|          20|       15|
|   2006|    9|Troy                   |North Texas                 |          14|           6|        8|
|   2006|    9|Tulane                 |Army                        |          42|          28|       14|
|   2006|    9|Utah                   |Nevada-Las Vegas            |          45|          23|       22|
|   2006|    9|Duke                   |Vanderbilt                  |          28|          45|      -17|
|   2006|    9|Virginia               |North Carolina State        |          14|           7|        7|
|   2006|    9|North Carolina         |Wake Forest                 |          17|          24|       -7|
|   2006|    9|UCLA                   |Washington State            |          15|          37|      -22|
|   2006|    9|Western Michigan       |Eastern Michigan            |          18|          15|        3|
|   2006|    9|Wisconsin              |Illinois                    |          30|          24|        6|
|   2006|    9|Rutgers                |Connecticut                 |          24|          13|       11|
|   2006|   10|Southern Methodist     |Alabama-Birmingham          |          22|           9|       13|
|   2006|   10|Boise State            |Fresno State                |          45|          21|       24|
|   2006|   10|Louisville             |West Virginia               |          44|          34|       10|
|   2006|   10|Army                   |Air Force                   |           7|          43|      -36|
|   2006|   10|Akron                  |Bowling Green State         |          35|          28|        7|
|   2006|   10|Washington State       |Arizona                     |          17|          27|      -10|
|   2006|   10|South Carolina         |Arkansas                    |          20|          26|       -6|
|   2006|   10|Auburn                 |Arkansas State              |          27|           0|       27|
|   2006|   10|Colorado State         |Brigham Young               |           3|          24|      -21|
|   2006|   10|Buffalo                |Kent State                  |          41|          14|       27|
|   2006|   10|California             |UCLA                        |          38|          24|       14|
|   2006|   10|Temple                 |Central Michigan            |          26|          42|      -16|
|   2006|   10|Central Florida        |East Carolina               |          10|          23|      -13|
|   2006|   10|Vanderbilt             |Florida                     |          19|          25|       -6|
|   2006|   10|Florida State          |Virginia                    |          33|           0|       33|
|   2006|   10|North Carolina State   |Georgia Tech                |          23|          31|       -8|
|   2006|   10|Utah State             |Hawaii                      |          10|          63|      -53|
|   2006|   10|Houston                |Tulsa                       |          27|          10|       17|
|   2006|   10|Iowa State             |Kansas                      |          10|          41|      -31|
|   2006|   10|Colorado               |Kansas State                |          21|          34|      -13|
|   2006|   10|Kentucky               |Georgia                     |          24|          20|        4|
|   2006|   10|Tennessee              |Louisiana State             |          24|          28|       -4|
|   2006|   10|North Texas            |Louisiana Tech              |          31|          34|       -3|
|   2006|   10|Marshall               |Tulane                      |          42|          21|       21|
|   2006|   10|Clemson                |Maryland                    |          12|          13|       -1|
|   2006|   10|Michigan               |Ball State                  |          34|          26|        8|
|   2006|   10|Middle Tennessee State |Florida Atlantic            |          35|          14|       21|
|   2006|   10|Minnesota              |Indiana                     |          63|          26|       37|
|   2006|   10|Mississippi            |Northwestern State          |          27|           7|       20|
|   2006|   10|Alabama                |Mississippi State           |          16|          24|       -8|
|   2006|   10|Duke                   |Navy                        |          13|          38|      -25|
|   2006|   10|Nebraska               |Missouri                    |          34|          20|       14|
|   2006|   10|Idaho                  |Nevada                      |           7|          45|      -38|
|   2006|   10|Iowa                   |Northwestern                |           7|          21|      -14|
|   2006|   10|Notre Dame             |North Carolina              |          45|          26|       19|
|   2006|   10|Eastern Michigan       |Ohio                        |          10|          16|       -6|
|   2006|   10|Illinois               |Ohio State                  |          10|          17|       -7|
|   2006|   10|Texas A&M              |Oklahoma                    |          16|          17|       -1|
|   2006|   10|Oregon                 |Washington                  |          34|          14|       20|
|   2006|   10|Oregon State           |Arizona State               |          44|          10|       34|
|   2006|   10|Michigan State         |Purdue                      |          15|          17|       -2|
|   2006|   10|Texas-El Paso          |Rice                        |          31|          37|       -6|
|   2006|   10|New Mexico State       |San Jose State              |          21|          31|      -10|
|   2006|   10|South Florida          |Pittsburgh                  |          22|          12|       10|
|   2006|   10|Stanford               |Southern California         |           0|          42|      -42|
|   2006|   10|Texas                  |Oklahoma State              |          36|          10|       26|
|   2006|   10|Nevada-Las Vegas       |Texas Christian             |          10|          25|      -15|
|   2006|   10|Texas Tech             |Baylor                      |          55|          21|       34|
|   2006|   10|Troy                   |Louisiana-Lafayette         |          42|          28|       14|
|   2006|   10|Miami (FL)             |Virginia Tech               |          10|          17|       -7|
|   2006|   10|Wake Forest            |Boston College              |          21|          14|        7|
|   2006|   10|Western Michigan       |Miami (OH)                  |          27|          24|        3|
|   2006|   10|Wisconsin              |Penn State                  |          13|           3|       10|
|   2006|   10|Wyoming                |San Diego State             |          27|          24|        3|
|   2006|   10|Memphis                |Southern Mississippi        |          21|          42|      -21|
|   2006|   11|Northern Illinois      |Toledo                      |          13|          17|       -4|
|   2006|   11|Akron                  |Buffalo                     |          31|          16|       15|
|   2006|   11|Brigham Young          |Wyoming                     |          55|           7|       48|
|   2006|   11|Rutgers                |Louisville                  |          28|          25|        3|
|   2006|   11|Central Michigan       |Western Michigan            |          31|           7|       24|
|   2006|   11|Alabama-Birmingham     |Texas-El Paso               |          17|          36|      -19|
|   2006|   11|Arizona                |California                  |          24|          20|        4|
|   2006|   11|Arizona State          |Washington State            |          47|          14|       33|
|   2006|   11|Arkansas               |Tennessee                   |          31|          14|       17|
|   2006|   11|San Jose State         |Boise State                 |          20|          23|       -3|
|   2006|   11|Boston College         |Duke                        |          28|           7|       21|
|   2006|   11|Memphis                |Central Florida             |          24|          26|       -2|
|   2006|   11|Clemson                |North Carolina State        |          20|          14|        6|
|   2006|   11|Colorado               |Iowa State                  |          33|          16|       17|
|   2006|   11|Connecticut            |Pittsburgh                  |          46|          45|        1|
|   2006|   11|East Carolina          |Marshall                    |          33|          20|       13|
|   2006|   11|Florida                |South Carolina              |          17|          16|        1|
|   2006|   11|Fresno State           |New Mexico State            |          23|          18|        5|
|   2006|   11|Auburn                 |Georgia                     |          15|          37|      -22|
|   2006|   11|North Carolina         |Georgia Tech                |           0|           7|       -7|
|   2006|   11|Hawaii                 |Louisiana Tech              |          61|          17|       44|
|   2006|   11|Southern Methodist     |Houston                     |          27|          37|      -10|
|   2006|   11|Kansas State           |Texas                       |          45|          42|        3|
|   2006|   11|Kentucky               |Vanderbilt                  |          38|          26|       12|
|   2006|   11|Louisiana State        |Alabama                     |          28|          14|       14|
|   2006|   11|Florida International  |Louisiana-Monroe            |           0|          35|      -35|
|   2006|   11|Maryland               |Miami (FL)                  |          14|          13|        1|
|   2006|   11|Indiana                |Michigan                    |           3|          34|      -31|
|   2006|   11|Arkansas State         |Middle Tennessee State      |          10|          38|      -28|
|   2006|   11|Michigan State         |Minnesota                   |          18|          31|      -13|
|   2006|   11|Eastern Michigan       |Navy                        |          21|          49|      -28|
|   2006|   11|Texas A&M              |Nebraska                    |          27|          28|       -1|
|   2006|   11|Nevada                 |Utah State                  |          42|           0|       42|
|   2006|   11|Louisiana-Lafayette    |North Texas                 |           7|          16|       -9|
|   2006|   11|Air Force              |Notre Dame                  |          17|          39|      -22|
|   2006|   11|Northwestern           |Ohio State                  |          10|          54|      -44|
|   2006|   11|Oklahoma               |Texas Tech                  |          34|          24|       10|
|   2006|   11|Oklahoma State         |Baylor                      |          66|          24|       42|
|   2006|   11|Penn State             |Temple                      |          47|           0|       47|
|   2006|   11|Illinois               |Purdue                      |          31|          42|      -11|
|   2006|   11|Tulsa                  |Rice                        |          38|          41|       -3|
|   2006|   11|San Diego State        |Nevada-Las Vegas            |          21|           7|       14|
|   2006|   11|South Florida          |Syracuse                    |          27|          10|       17|
|   2006|   11|Southern California    |Oregon                      |          35|          10|       25|
|   2006|   11|Tulane                 |Southern Mississippi        |           3|          31|      -28|
|   2006|   11|Washington             |Stanford                    |           3|          20|      -17|
|   2006|   11|New Mexico             |Texas Christian             |          21|          27|       -6|
|   2006|   11|Florida Atlantic       |Troy                        |          17|          24|       -7|
|   2006|   11|UCLA                   |Oregon State                |          25|           7|       18|
|   2006|   11|Utah                   |Colorado State              |          35|          22|       13|
|   2006|   11|Virginia Tech          |Kent State                  |          23|           0|       23|
|   2006|   11|Florida State          |Wake Forest                 |           0|          30|      -30|
|   2006|   11|West Virginia          |Cincinnati                  |          42|          24|       18|
|   2006|   11|Iowa                   |Wisconsin                   |          21|          24|       -3|
|   2006|   12|Toledo                 |Ball State                  |          17|          20|       -3|
|   2006|   12|Bowling Green State    |Miami (OH)                  |           7|           9|       -2|
|   2006|   12|Ohio                   |Akron                       |          17|           7|       10|
|   2006|   12|Pittsburgh             |West Virginia               |          27|          45|      -18|
|   2006|   12|Kent State             |Eastern Michigan            |          14|           6|        8|
|   2006|   12|Northern Illinois      |Central Michigan            |          31|          10|       21|
|   2006|   12|Oregon                 |Arizona                     |          10|          37|      -27|
|   2006|   12|Mississippi State      |Arkansas                    |          14|          28|      -14|
|   2006|   12|Troy                   |Arkansas State              |          26|          33|       -7|
|   2006|   12|Alabama                |Auburn                      |          15|          22|       -7|
|   2006|   12|Boise State            |Utah State                  |          49|          10|       39|
|   2006|   12|Boston College         |Maryland                    |          38|          16|       22|
|   2006|   12|Brigham Young          |New Mexico                  |          42|          17|       25|
|   2006|   12|Cincinnati             |Rutgers                     |          30|          11|       19|
|   2006|   12|Florida                |Western Carolina            |          62|           0|       62|
|   2006|   12|North Texas            |Florida Atlantic            |          16|          17|       -1|
|   2006|   12|Florida State          |Western Michigan            |          28|          20|        8|
|   2006|   12|Fresno State           |Idaho                       |          34|           0|       34|
|   2006|   12|Georgia Tech           |Duke                        |          49|          21|       28|
|   2006|   12|Hawaii                 |San Jose State              |          54|          17|       37|
|   2006|   12|Memphis                |Houston                     |          20|          23|       -3|
|   2006|   12|Iowa State             |Missouri                    |          21|          16|        5|
|   2006|   12|Kansas                 |Kansas State                |          39|          20|       19|
|   2006|   12|Kentucky               |Louisiana-Monroe            |          42|          40|        2|
|   2006|   12|Louisiana State        |Mississippi                 |          23|          20|        3|
|   2006|   12|Florida International  |Louisiana-Lafayette         |           7|          17|      -10|
|   2006|   12|Louisville             |South Florida               |          31|           8|       23|
|   2006|   12|Marshall               |Texas-El Paso               |          49|          21|       28|
|   2006|   12|Minnesota              |Iowa                        |          34|          24|       10|
|   2006|   12|Navy                   |Temple                      |          42|           6|       36|
|   2006|   12|Louisiana Tech         |Nevada                      |           0|          42|      -42|
|   2006|   12|North Carolina         |North Carolina State        |          23|           9|       14|
|   2006|   12|Northwestern           |Illinois                    |          27|          16|       11|
|   2006|   12|Notre Dame             |Army                        |          41|           9|       32|
|   2006|   12|Ohio State             |Michigan                    |          42|          39|        3|
|   2006|   12|Baylor                 |Oklahoma                    |          10|          36|      -26|
|   2006|   12|Stanford               |Oregon State                |           7|          30|      -23|
|   2006|   12|Penn State             |Michigan State              |          17|          13|        4|
|   2006|   12|Purdue                 |Indiana                     |          28|          19|        9|
|   2006|   12|Rice                   |East Carolina               |          18|          17|        1|
|   2006|   12|South Carolina         |Middle Tennessee State      |          52|           7|       45|
|   2006|   12|Southern California    |California                  |          23|           9|       14|
|   2006|   12|Southern Methodist     |Tulsa                       |          34|          24|       10|
|   2006|   12|Southern Mississippi   |Alabama-Birmingham          |          25|          20|        5|
|   2006|   12|Syracuse               |Connecticut                 |          20|          14|        6|
|   2006|   12|Vanderbilt             |Tennessee                   |          10|          39|      -29|
|   2006|   12|Texas Christian        |San Diego State             |          52|           0|       52|
|   2006|   12|Texas Tech             |Oklahoma State              |          30|          24|        6|
|   2006|   12|Tulane                 |Central Florida             |          10|           9|        1|
|   2006|   12|Arizona State          |UCLA                        |          12|          24|      -12|
|   2006|   12|Air Force              |Utah                        |          14|          17|       -3|
|   2006|   12|Virginia               |Miami (FL)                  |          17|           7|       10|
|   2006|   12|Wake Forest            |Virginia Tech               |           6|          27|      -21|
|   2006|   12|Washington State       |Washington                  |          32|          35|       -3|
|   2006|   12|Wisconsin              |Buffalo                     |          35|           3|       32|
|   2006|   12|Nevada-Las Vegas       |Wyoming                     |          26|          34|       -8|
|   2006|   13|Toledo                 |Bowling Green State         |          31|          21|       10|
|   2006|   13|Miami (FL)             |Boston College              |          17|          14|        3|
|   2006|   13|Ball State             |Kent State                  |          30|           6|       24|
|   2006|   13|Buffalo                |Central Michigan            |          28|          55|      -27|
|   2006|   13|Louisiana Tech         |Fresno State                |          27|          34|       -7|
|   2006|   13|Arkansas               |Louisiana State             |          26|          31|       -5|
|   2006|   13|Nebraska               |Colorado                    |          37|          14|       23|
|   2006|   13|Nevada-Las Vegas       |Air Force                   |          42|          39|        3|
|   2006|   13|Eastern Michigan       |Northern Illinois           |           0|          27|      -27|
|   2006|   13|Miami (OH)             |Ohio                        |          24|          34|      -10|
|   2006|   13|Oregon State           |Oregon                      |          30|          28|        2|
|   2006|   13|Texas                  |Texas A&M                   |           7|          12|       -5|
|   2006|   13|Tulsa                  |Tulane                      |          38|           3|       35|
|   2006|   13|Akron                  |Western Michigan            |           0|          17|      -17|
|   2006|   13|Arizona                |Arizona State               |          14|          28|      -14|
|   2006|   13|Nevada                 |Boise State                 |           7|          38|      -31|
|   2006|   13|Utah                   |Brigham Young               |          31|          33|       -2|
|   2006|   13|Central Florida        |Alabama-Birmingham          |          31|          22|        9|
|   2006|   13|Connecticut            |Cincinnati                  |          23|          26|       -3|
|   2006|   13|North Carolina State   |East Carolina               |          16|          21|       -5|
|   2006|   13|Florida State          |Florida                     |          14|          21|       -7|
|   2006|   13|Florida Atlantic       |Florida International       |          31|           0|       31|
|   2006|   13|Georgia                |Georgia Tech                |          15|          12|        3|
|   2006|   13|Hawaii                 |Purdue                      |          42|          35|        7|
|   2006|   13|Louisiana-Lafayette    |Arkansas State              |          28|          13|       15|
|   2006|   13|Louisiana-Monroe       |North Texas                 |          23|           3|       20|
|   2006|   13|Pittsburgh             |Louisville                  |          24|          48|      -24|
|   2006|   13|Texas-El Paso          |Memphis                     |          19|          38|      -19|
|   2006|   13|Mississippi            |Mississippi State           |          20|          17|        3|
|   2006|   13|Missouri               |Kansas                      |          42|          17|       25|
|   2006|   13|New Mexico             |San Diego State             |          41|          14|       27|
|   2006|   13|Utah State             |New Mexico State            |          20|          42|      -22|
|   2006|   13|Duke                   |North Carolina              |          44|          45|       -1|
|   2006|   13|Oklahoma State         |Oklahoma                    |          21|          27|       -6|
|   2006|   13|Rice                   |Southern Methodist          |          31|          27|        4|
|   2006|   13|Rutgers                |Syracuse                    |          38|           7|       31|
|   2006|   13|Idaho                  |San Jose State              |          13|          28|      -15|
|   2006|   13|Clemson                |South Carolina              |          28|          31|       -3|
|   2006|   13|West Virginia          |South Florida               |          19|          24|       -5|
|   2006|   13|Southern California    |Notre Dame                  |          44|          24|       20|
|   2006|   13|Southern Mississippi   |Marshall                    |          42|           7|       35|
|   2006|   13|Tennessee              |Kentucky                    |          17|          12|        5|
|   2006|   13|Colorado State         |Texas Christian             |          14|          45|      -31|
|   2006|   13|Middle Tennessee State |Troy                        |          20|          21|       -1|
|   2006|   13|Virginia Tech          |Virginia                    |          17|           0|       17|
|   2006|   13|Maryland               |Wake Forest                 |          24|          38|      -14|
|   2006|   14|Central Michigan       |Ohio                        |          31|          10|       21|
|   2006|   14|Houston                |Southern Mississippi        |          34|          20|       14|
|   2006|   14|California             |Stanford                    |          26|          17|        9|
|   2006|   14|Florida                |Arkansas                    |          38|          28|       10|
|   2006|   14|Louisiana-Lafayette    |Louisiana-Monroe            |          20|          39|      -19|
|   2006|   14|Louisville             |Connecticut                 |          48|          17|       31|
|   2006|   14|Navy                   |Army                        |          26|          14|       12|
|   2006|   14|New Mexico State       |Louisiana Tech              |          50|          23|       27|
|   2006|   14|Oklahoma               |Nebraska                    |          21|           7|       14|
|   2006|   14|Hawaii                 |Oregon State                |          32|          35|       -3|
|   2006|   14|San Diego State        |Colorado State              |          17|           6|       11|
|   2006|   14|San Jose State         |Fresno State                |          24|          14|       10|
|   2006|   14|Texas Christian        |Air Force                   |          38|          14|       24|
|   2006|   14|Florida International  |Troy                        |          13|          26|      -13|
|   2006|   14|UCLA                   |Southern California         |          13|           9|        4|
|   2006|   14|Wake Forest            |Georgia Tech                |           9|           6|        3|
|   2006|   14|West Virginia          |Rutgers                     |          41|          39|        2|
|   2006|   15|Texas Christian        |Northern Illinois           |          37|           7|       30|
|   2006|   15|Brigham Young          |Oregon                      |          38|           8|       30|
|   2006|   15|Troy                   |Rice                        |          41|          17|       24|
|   2006|   15|New Mexico             |San Jose State              |          12|          20|       -8|
|   2006|   15|South Florida          |East Carolina               |          24|           7|       17|
|   2006|   15|Utah                   |Tulsa                       |          25|          13|       12|
|   2006|   15|Hawaii                 |Arizona State               |          41|          24|       17|
|   2006|   16|Central Michigan       |Middle Tennessee State      |          31|          14|       17|
|   2006|   16|Florida State          |UCLA                        |          44|          27|       17|
|   2006|   16|California             |Texas A&M                   |          45|          10|       35|
|   2006|   16|Oklahoma State         |Alabama                     |          34|          31|        3|
|   2006|   16|Rutgers                |Kansas State                |          37|          10|       27|
|   2006|   16|Kentucky               |Clemson                     |          28|          20|        8|
|   2006|   16|Maryland               |Purdue                      |          24|           7|       17|
|   2006|   16|Oregon State           |Missouri                    |          39|          38|        1|
|   2006|   16|South Carolina         |Houston                     |          44|          36|        8|
|   2006|   16|Texas Tech             |Minnesota                   |          44|          41|        3|
|   2006|   16|Boston College         |Navy                        |          25|          24|        1|
|   2006|   16|Georgia                |Virginia Tech               |          31|          24|        7|
|   2006|   16|Texas                  |Iowa                        |          26|          24|        2|
|   2006|   16|Miami (FL)             |Nevada                      |          21|          20|        1|
|   2006|   17|Auburn                 |Nebraska                    |          17|          14|        3|
|   2006|   17|Boise State            |Oklahoma                    |          43|          42|        1|
|   2006|   17|Penn State             |Tennessee                   |          20|          10|       10|
|   2006|   17|Southern California    |Michigan                    |          32|          18|       14|
|   2006|   17|West Virginia          |Georgia Tech                |          38|          35|        3|
|   2006|   17|Wisconsin              |Arkansas                    |          17|          14|        3|
|   2006|   18|Louisville             |Wake Forest                 |          24|          13|       11|
|   2006|   18|Louisiana State        |Notre Dame                  |          41|          14|       27|
|   2006|   18|Cincinnati             |Western Michigan            |          27|          24|        3|
|   2006|   18|Southern Mississippi   |Ohio                        |          28|           7|       21|
|   2006|   18|Florida                |Ohio State                  |          41|          14|       27|
|   2007|    1|Boise State            |Weber State                 |          56|           7|       49|
|   2007|    1|Cincinnati             |Southeast Missouri State    |          59|           3|       56|
|   2007|    1|Iowa State             |Kent State                  |          14|          23|       -9|
|   2007|    1|Mississippi State      |Louisiana State             |           0|          45|      -45|
|   2007|    1|Louisville             |Murray State                |          73|          10|       63|
|   2007|    1|Ball State             |Miami (OH)                  |          13|          14|       -1|
|   2007|    1|Utah State             |Nevada-Las Vegas            |          16|          23|       -7|
|   2007|    1|New Mexico State       |Southeastern Louisiana      |          35|          14|       21|
|   2007|    1|Oregon State           |Utah                        |          24|           7|       17|
|   2007|    1|Rutgers                |Buffalo                     |          38|           3|       35|
|   2007|    1|Louisiana-Monroe       |Tulsa                       |          17|          35|      -18|
|   2007|    1|Temple                 |Navy                        |          19|          30|      -11|
|   2007|    1|Syracuse               |Washington                  |          12|          42|      -30|
|   2007|    1|Air Force              |South Carolina State        |          34|           3|       31|
|   2007|    1|Akron                  |Army                        |          22|          14|        8|
|   2007|    1|Alabama                |Western Carolina            |          52|           6|       46|
|   2007|    1|Michigan               |Appalachian State           |          32|          34|       -2|
|   2007|    1|Arizona State          |San Jose State              |          45|           3|       42|
|   2007|    1|Arkansas               |Troy                        |          46|          26|       20|
|   2007|    1|Auburn                 |Kansas State                |          23|          13|       10|
|   2007|    1|Boston College         |Wake Forest                 |          38|          28|       10|
|   2007|    1|Minnesota              |Bowling Green State         |          31|          32|       -1|
|   2007|    1|Brigham Young          |Arizona                     |          20|           7|       13|
|   2007|    1|California             |Tennessee                   |          45|          31|       14|
|   2007|    1|North Carolina State   |Central Florida             |          23|          25|       -2|
|   2007|    1|Colorado               |Colorado State              |          31|          28|        3|
|   2007|    1|Duke                   |Connecticut                 |          14|          45|      -31|
|   2007|    1|Florida                |Western Kentucky            |          49|           3|       46|
|   2007|    1|Florida Atlantic       |Middle Tennessee State      |          27|          14|       13|
|   2007|    1|Fresno State           |Sacramento State            |          24|           3|       21|
|   2007|    1|Georgia                |Oklahoma State              |          35|          14|       21|
|   2007|    1|Notre Dame             |Georgia Tech                |           3|          33|      -30|
|   2007|    1|Hawaii                 |Northern Colorado           |          63|           6|       57|
|   2007|    1|Indiana                |Indiana State               |          55|           7|       48|
|   2007|    1|Northern Illinois      |Iowa                        |           3|          16|      -13|
|   2007|    1|Kansas                 |Central Michigan            |          52|           7|       45|
|   2007|    1|Kentucky               |Eastern Kentucky            |          50|          10|       40|
|   2007|    1|Louisiana Tech         |Central Arkansas            |          28|           7|       21|
|   2007|    1|Maryland               |Villanova                   |          31|          14|       17|
|   2007|    1|Miami (FL)             |Marshall                    |          31|           3|       28|
|   2007|    1|Michigan State         |Alabama-Birmingham          |          55|          18|       37|
|   2007|    1|Memphis                |Mississippi                 |          21|          23|       -2|
|   2007|    1|Missouri               |Illinois                    |          40|          34|        6|
|   2007|    1|Nebraska               |Nevada                      |          52|          10|       42|
|   2007|    1|Rice                   |Nicholls State              |          14|          16|       -2|
|   2007|    1|North Carolina         |James Madison               |          37|          14|       23|
|   2007|    1|Northwestern           |Northeastern                |          27|           0|       27|
|   2007|    1|Ohio                   |Gardner-Webb                |          36|          14|       22|
|   2007|    1|Ohio State             |Youngstown State            |          38|           6|       32|
|   2007|    1|Oklahoma               |North Texas                 |          79|          10|       69|
|   2007|    1|Oregon                 |Houston                     |          48|          27|       21|
|   2007|    1|Penn State             |Florida International       |          59|           0|       59|
|   2007|    1|Pittsburgh             |Eastern Michigan            |          27|           3|       24|
|   2007|    1|Toledo                 |Purdue                      |          24|          52|      -28|
|   2007|    1|South Carolina         |Louisiana-Lafayette         |          28|          14|       14|
|   2007|    1|South Florida          |Elon                        |          28|          13|       15|
|   2007|    1|Southern California    |Idaho                       |          38|          10|       28|
|   2007|    1|Southern Mississippi   |Tennessee-Martin            |          35|          13|       22|
|   2007|    1|Texas                  |Arkansas State              |          21|          13|        8|
|   2007|    1|Texas A&M              |Montana State               |          38|           7|       31|
|   2007|    1|Texas Christian        |Baylor                      |          27|           0|       27|
|   2007|    1|Texas-El Paso          |New Mexico                  |          10|           6|        4|
|   2007|    1|Stanford               |UCLA                        |          17|          45|      -28|
|   2007|    1|Vanderbilt             |Richmond                    |          41|          17|       24|
|   2007|    1|Virginia Tech          |East Carolina               |          17|           7|       10|
|   2007|    1|West Virginia          |Western Michigan            |          62|          24|       38|
|   2007|    1|Wisconsin              |Washington State            |          42|          21|       21|
|   2007|    1|Wyoming                |Virginia                    |          23|           3|       20|
|   2007|    1|Clemson                |Florida State               |          24|          18|        6|
|   2007|    1|Southern Methodist     |Texas Tech                  |           9|          49|      -40|
|   2007|    2|Cincinnati             |Oregon State                |          34|           3|       31|
|   2007|    2|Louisville             |Middle Tennessee State      |          58|          42|       16|
|   2007|    2|Rutgers                |Navy                        |          41|          24|       17|
|   2007|    2|Utah                   |Air Force                   |          12|          20|       -8|
|   2007|    2|Vanderbilt             |Alabama                     |          10|          24|      -14|
|   2007|    2|Arizona                |Northern Arizona            |          45|          24|       21|
|   2007|    2|Arizona State          |Colorado                    |          33|          14|       19|
|   2007|    2|Army                   |Rhode Island                |          14|           7|        7|
|   2007|    2|Eastern Michigan       |Ball State                  |          16|          38|      -22|
|   2007|    2|Baylor                 |Rice                        |          42|          17|       25|
|   2007|    2|Boston College         |North Carolina State        |          37|          17|       20|
|   2007|    2|Temple                 |Buffalo                     |           7|          42|      -35|
|   2007|    2|Colorado State         |California                  |          28|          34|       -6|
|   2007|    2|Central Michigan       |Toledo                      |          52|          31|       21|
|   2007|    2|Clemson                |Louisiana-Monroe            |          49|          26|       23|
|   2007|    2|Connecticut            |Maine                       |          38|           0|       38|
|   2007|    2|East Carolina          |North Carolina              |          34|          31|        3|
|   2007|    2|Florida                |Troy                        |          59|          31|       28|
|   2007|    2|Florida State          |Alabama-Birmingham          |          34|          24|       10|
|   2007|    2|Georgia Tech           |Samford                     |          69|          14|       55|
|   2007|    2|Louisiana Tech         |Hawaii                      |          44|          45|       -1|
|   2007|    2|Idaho                  |Cal Poly                    |          20|          13|        7|
|   2007|    2|Illinois               |Western Illinois            |          21|           0|       21|
|   2007|    2|Western Michigan       |Indiana                     |          27|          37|      -10|
|   2007|    2|Iowa                   |Syracuse                    |          35|           0|       35|
|   2007|    2|Kansas                 |Southeastern Louisiana      |          62|           0|       62|
|   2007|    2|Kansas State           |San Jose State              |          34|          14|       20|
|   2007|    2|Kentucky               |Kent State                  |          56|          20|       36|
|   2007|    2|Louisiana State        |Virginia Tech               |          48|           7|       41|
|   2007|    2|Florida International  |Maryland                    |          10|          26|      -16|
|   2007|    2|Michigan State         |Bowling Green State         |          28|          17|       11|
|   2007|    2|Minnesota              |Miami (OH)                  |          41|          35|        6|
|   2007|    2|Tulane                 |Mississippi State           |          17|          38|      -21|
|   2007|    2|Mississippi            |Missouri                    |          25|          38|      -13|
|   2007|    2|Wake Forest            |Nebraska                    |          17|          20|       -3|
|   2007|    2|New Mexico             |New Mexico State            |          44|          34|       10|
|   2007|    2|Iowa State             |Northern Iowa               |          13|          24|      -11|
|   2007|    2|Northwestern           |Nevada                      |          36|          31|        5|
|   2007|    2|Louisiana-Lafayette    |Ohio                        |          23|          31|       -8|
|   2007|    2|Ohio State             |Akron                       |          20|           2|       18|
|   2007|    2|Oklahoma               |Miami (FL)                  |          51|          13|       38|
|   2007|    2|Oklahoma State         |Florida Atlantic            |          42|           6|       36|
|   2007|    2|Michigan               |Oregon                      |           7|          39|      -32|
|   2007|    2|Penn State             |Notre Dame                  |          31|          10|       21|
|   2007|    2|Pittsburgh             |Grambling State             |          34|          10|       24|
|   2007|    2|Purdue                 |Eastern Illinois            |          52|           6|       46|
|   2007|    2|Georgia                |South Carolina              |          12|          16|       -4|
|   2007|    2|Auburn                 |South Florida               |          23|          26|       -3|
|   2007|    2|Northern Illinois      |Southern Illinois           |          31|          34|       -3|
|   2007|    2|Southern Methodist     |North Texas                 |          45|          31|       14|
|   2007|    2|Tennessee              |Southern Mississippi        |          39|          19|       20|
|   2007|    2|Texas                  |Texas Christian             |          34|          13|       21|
|   2007|    2|Texas A&M              |Fresno State                |          47|          45|        2|
|   2007|    2|Texas Tech             |Texas-El Paso               |          45|          31|       14|
|   2007|    2|UCLA                   |Brigham Young               |          27|          17|       10|
|   2007|    2|Virginia               |Duke                        |          24|          13|       11|
|   2007|    2|Washington             |Boise State                 |          24|          10|       14|
|   2007|    2|Washington State       |San Diego State             |          45|          17|       28|
|   2007|    2|Marshall               |West Virginia               |          23|          48|      -25|
|   2007|    2|Western Kentucky       |West Virginia Tech          |          87|           0|       87|
|   2007|    2|Nevada-Las Vegas       |Wisconsin                   |          13|          20|       -7|
|   2007|    2|Wyoming                |Utah State                  |          32|          18|       14|
|   2007|    3|Air Force              |Texas Christian             |          20|          17|        3|
|   2007|    3|Maryland               |West Virginia               |          14|          31|      -17|
|   2007|    3|Troy                   |Oklahoma State              |          41|          23|       18|
|   2007|    3|Alabama                |Arkansas                    |          41|          38|        3|
|   2007|    3|Alabama-Birmingham     |Alcorn State                |          22|           0|       22|
|   2007|    3|Arizona State          |San Diego State             |          34|          13|       21|
|   2007|    3|Arkansas State         |Southern Methodist          |          45|          28|       17|
|   2007|    3|Navy                   |Ball State                  |          31|          34|       -3|
|   2007|    3|Baylor                 |Texas State                 |          34|          27|        7|
|   2007|    3|Boise State            |Wyoming                     |          24|          14|       10|
|   2007|    3|Georgia Tech           |Boston College              |          10|          24|      -14|
|   2007|    3|California             |Louisiana Tech              |          42|          12|       30|
|   2007|    3|Miami (OH)             |Cincinnati                  |          10|          47|      -37|
|   2007|    3|Clemson                |Furman                      |          38|          10|       28|
|   2007|    3|Connecticut            |Temple                      |          22|          17|        5|
|   2007|    3|Northwestern           |Duke                        |          14|          20|       -6|
|   2007|    3|Northern Illinois      |Eastern Michigan            |          19|          21|       -2|
|   2007|    3|Florida                |Tennessee                   |          59|          20|       39|
|   2007|    3|Florida Atlantic       |Minnesota                   |          42|          39|        3|
|   2007|    3|Colorado               |Florida State               |           6|          16|      -10|
|   2007|    3|Georgia                |Western Carolina            |          45|          16|       29|
|   2007|    3|Nevada-Las Vegas       |Hawaii                      |          14|          49|      -35|
|   2007|    3|Tulane                 |Houston                     |          10|          34|      -24|
|   2007|    3|Syracuse               |Illinois                    |          20|          41|      -21|
|   2007|    3|Indiana                |Akron                       |          41|          24|       17|
|   2007|    3|Iowa State             |Iowa                        |          15|          13|        2|
|   2007|    3|Kansas                 |Toledo                      |          45|          13|       32|
|   2007|    3|Kansas State           |Missouri State              |          61|          10|       51|
|   2007|    3|Kent State             |Delaware State              |          38|           7|       31|
|   2007|    3|Kentucky               |Louisville                  |          40|          34|        6|
|   2007|    3|Louisiana State        |Middle Tennessee State      |          44|           0|       44|
|   2007|    3|Louisiana-Lafayette    |McNeese State               |          17|          38|      -21|
|   2007|    3|Memphis                |Jacksonville State          |          35|          14|       21|
|   2007|    3|Miami (FL)             |Florida International       |          23|           9|       14|
|   2007|    3|Michigan               |Notre Dame                  |          38|           0|       38|
|   2007|    3|Michigan State         |Pittsburgh                  |          17|          13|        4|
|   2007|    3|Auburn                 |Mississippi State           |          14|          19|       -5|
|   2007|    3|Missouri               |Western Michigan            |          52|          24|       28|
|   2007|    3|Nevada                 |Nicholls State              |          52|          17|       35|
|   2007|    3|Marshall               |New Hampshire               |          35|          48|      -13|
|   2007|    3|Arizona                |New Mexico                  |          27|          29|       -2|
|   2007|    3|New Mexico State       |Texas-El Paso               |          29|          24|        5|
|   2007|    3|North Carolina State   |Wofford                     |          38|          17|       21|
|   2007|    3|Washington             |Ohio State                  |          14|          33|      -19|
|   2007|    3|Oklahoma               |Utah State                  |          54|           3|       51|
|   2007|    3|Oregon                 |Fresno State                |          52|          21|       31|
|   2007|    3|Oregon State           |Idaho State                 |          61|          10|       51|
|   2007|    3|Penn State             |Buffalo                     |          45|          24|       21|
|   2007|    3|Purdue                 |Central Michigan            |          45|          22|       23|
|   2007|    3|Rutgers                |Norfolk State               |          59|           0|       59|
|   2007|    3|South Carolina         |South Carolina State        |          38|           3|       35|
|   2007|    3|Nebraska               |Southern California         |          31|          49|      -18|
|   2007|    3|East Carolina          |Southern Mississippi        |          21|          28|       -7|
|   2007|    3|Stanford               |San Jose State              |          37|           0|       37|
|   2007|    3|Central Florida        |Texas                       |          32|          35|       -3|
|   2007|    3|Texas A&M              |Louisiana-Monroe            |          54|          14|       40|
|   2007|    3|Rice                   |Texas Tech                  |          24|          59|      -35|
|   2007|    3|Tulsa                  |Brigham Young               |          55|          47|        8|
|   2007|    3|Utah                   |UCLA                        |          44|           6|       38|
|   2007|    3|Vanderbilt             |Mississippi                 |          31|          17|       14|
|   2007|    3|North Carolina         |Virginia                    |          20|          22|       -2|
|   2007|    3|Virginia Tech          |Ohio                        |          28|           7|       21|
|   2007|    3|Wake Forest            |Army                        |          21|          10|       11|
|   2007|    3|Washington State       |Idaho                       |          45|          28|       17|
|   2007|    3|Western Kentucky       |Eastern Kentucky            |          26|           6|       20|
|   2007|    3|Wisconsin              |Citadel                     |          45|          31|       14|
|   2007|    4|Miami (FL)             |Texas A&M                   |          34|          17|       17|
|   2007|    4|Middle Tennessee State |Western Kentucky            |          17|          20|       -3|
|   2007|    4|Tulsa                  |Oklahoma                    |          21|          62|      -41|
|   2007|    4|Akron                  |Kent State                  |          27|          20|        7|
|   2007|    4|Arizona State          |Oregon State                |          44|          32|       12|
|   2007|    4|Auburn                 |New Mexico State            |          55|          20|       35|
|   2007|    4|Buffalo                |Baylor                      |          21|          34|      -13|
|   2007|    4|Boston College         |Army                        |          37|          17|       20|
|   2007|    4|Bowling Green State    |Temple                      |          48|          35|       13|
|   2007|    4|Brigham Young          |Air Force                   |          31|           6|       25|
|   2007|    4|California             |Arizona                     |          45|          27|       18|
|   2007|    4|Central Florida        |Memphis                     |          56|          20|       36|
|   2007|    4|Cincinnati             |Marshall                    |          40|          14|       26|
|   2007|    4|North Carolina State   |Clemson                     |          20|          42|      -22|
|   2007|    4|Colorado               |Miami (OH)                  |          42|           0|       42|
|   2007|    4|Pittsburgh             |Connecticut                 |          14|          34|      -20|
|   2007|    4|Eastern Michigan       |Howard                      |          38|          15|       23|
|   2007|    4|Mississippi            |Florida                     |          24|          30|       -6|
|   2007|    4|North Texas            |Florida Atlantic            |          20|          30|      -10|
|   2007|    4|Alabama                |Georgia                     |          23|          26|       -3|
|   2007|    4|Hawaii                 |Charleston Southern         |          66|          10|       56|
|   2007|    4|Houston                |Colorado State              |          38|          27|       11|
|   2007|    4|Indiana                |Illinois                    |          14|          27|      -13|
|   2007|    4|Kansas                 |Florida International       |          55|           3|       52|
|   2007|    4|Arkansas               |Kentucky                    |          29|          42|      -13|
|   2007|    4|Louisiana State        |South Carolina              |          28|          16|       12|
|   2007|    4|Michigan               |Penn State                  |          14|           9|        5|
|   2007|    4|Notre Dame             |Michigan State              |          14|          31|      -17|
|   2007|    4|Mississippi State      |Gardner-Webb                |          31|          15|       16|
|   2007|    4|Missouri               |Illinois State              |          38|          17|       21|
|   2007|    4|Navy                   |Duke                        |          46|          43|        3|
|   2007|    4|Nebraska               |Ball State                  |          41|          40|        1|
|   2007|    4|Nevada-Las Vegas       |Utah                        |          27|           0|       27|
|   2007|    4|New Mexico             |Sacramento State            |          58|           0|       58|
|   2007|    4|Central Michigan       |North Dakota State          |          14|          44|      -30|
|   2007|    4|Idaho                  |Northern Illinois           |          35|          42|       -7|
|   2007|    4|Ohio State             |Northwestern                |          58|           7|       51|
|   2007|    4|Oklahoma State         |Texas Tech                  |          49|          45|        4|
|   2007|    4|Stanford               |Oregon                      |          31|          55|      -24|
|   2007|    4|Minnesota              |Purdue                      |          31|          45|      -14|
|   2007|    4|San Diego State        |Portland State              |          52|          17|       35|
|   2007|    4|Utah State             |San Jose State              |          20|          23|       -3|
|   2007|    4|South Florida          |North Carolina              |          37|          10|       27|
|   2007|    4|Southern California    |Washington State            |          47|          14|       33|
|   2007|    4|Louisville             |Syracuse                    |          35|          38|       -3|
|   2007|    4|Tennessee              |Arkansas State              |          48|          27|       21|
|   2007|    4|Texas                  |Rice                        |          58|          14|       44|
|   2007|    4|Texas Christian        |Southern Methodist          |          21|           7|       14|
|   2007|    4|Texas-El Paso          |Texas Southern              |          52|          12|       40|
|   2007|    4|Toledo                 |Iowa State                  |          36|          35|        1|
|   2007|    4|Louisiana-Lafayette    |Troy                        |          31|          48|      -17|
|   2007|    4|Tulane                 |Southeastern Louisiana      |          35|          27|        8|
|   2007|    4|UCLA                   |Washington                  |          44|          31|       13|
|   2007|    4|Virginia               |Georgia Tech                |          28|          23|        5|
|   2007|    4|Virginia Tech          |William & Mary              |          44|           3|       41|
|   2007|    4|Wake Forest            |Maryland                    |          31|          24|        7|
|   2007|    4|West Virginia          |East Carolina               |          48|           7|       41|
|   2007|    4|Western Michigan       |Central Connecticut State   |          51|          14|       37|
|   2007|    4|Wisconsin              |Iowa                        |          17|          13|        4|
|   2007|    4|Ohio                   |Wyoming                     |          33|          34|       -1|
|   2007|    5|Arkansas State         |Memphis                     |          35|          31|        4|
|   2007|    5|Boise State            |Southern Mississippi        |          38|          16|       22|
|   2007|    5|South Florida          |West Virginia               |          21|          13|        8|
|   2007|    5|Arizona                |Washington State            |          48|          20|       28|
|   2007|    5|Stanford               |Arizona State               |           3|          41|      -38|
|   2007|    5|Arkansas               |North Texas                 |          66|           7|       59|
|   2007|    5|Army                   |Temple                      |          37|          21|       16|
|   2007|    5|Florida                |Auburn                      |          17|          20|       -3|
|   2007|    5|Ball State             |Buffalo                     |          49|          14|       35|
|   2007|    5|Boston College         |Massachusetts               |          24|          14|       10|
|   2007|    5|Bowling Green State    |Western Kentucky            |          41|          21|       20|
|   2007|    5|New Mexico             |Brigham Young               |          24|          31|       -7|
|   2007|    5|Oregon                 |California                  |          24|          31|       -7|
|   2007|    5|Central Florida        |Louisiana-Lafayette         |          37|          19|       18|
|   2007|    5|Central Michigan       |Northern Illinois           |          35|          10|       25|
|   2007|    5|San Diego State        |Cincinnati                  |          23|          52|      -29|
|   2007|    5|Colorado               |Oklahoma                    |          27|          24|        3|
|   2007|    5|Connecticut            |Akron                       |          44|          10|       34|
|   2007|    5|Houston                |East Carolina               |          35|          37|       -2|
|   2007|    5|Florida State          |Alabama                     |          21|          14|        7|
|   2007|    5|Fresno State           |Louisiana Tech              |          17|           6|       11|
|   2007|    5|Georgia                |Mississippi                 |          45|          17|       28|
|   2007|    5|Georgia Tech           |Clemson                     |          13|           3|       10|
|   2007|    5|Idaho                  |Hawaii                      |          20|          48|      -28|
|   2007|    5|Illinois               |Penn State                  |          27|          20|        7|
|   2007|    5|Iowa                   |Indiana                     |          20|          38|      -18|
|   2007|    5|Texas                  |Kansas State                |          21|          41|      -20|
|   2007|    5|Ohio                   |Kent State                  |          25|          33|       -8|
|   2007|    5|Kentucky               |Florida Atlantic            |          45|          17|       28|
|   2007|    5|Tulane                 |Louisiana State             |           9|          34|      -25|
|   2007|    5|North Carolina State   |Louisville                  |          10|          29|      -19|
|   2007|    5|Rutgers                |Maryland                    |          24|          34|      -10|
|   2007|    5|Miami (FL)             |Duke                        |          24|          14|       10|
|   2007|    5|Miami (OH)             |Syracuse                    |          17|          14|        3|
|   2007|    5|Northwestern           |Michigan                    |          16|          28|      -12|
|   2007|    5|Middle Tennessee State |Florida International       |          47|           6|       41|
|   2007|    5|Navy                   |Air Force                   |          31|          20|       11|
|   2007|    5|Nebraska               |Iowa State                  |          35|          17|       18|
|   2007|    5|Nevada                 |Nevada-Las Vegas            |          27|          20|        7|
|   2007|    5|New Mexico State       |Arkansas-Pine Bluff         |          20|          17|        3|
|   2007|    5|Minnesota              |Ohio State                  |           7|          30|      -23|
|   2007|    5|Oklahoma State         |Sam Houston State           |          39|           3|       36|
|   2007|    5|Purdue                 |Notre Dame                  |          33|          19|       14|
|   2007|    5|San Jose State         |California-Davis            |          34|          14|       20|
|   2007|    5|South Carolina         |Mississippi State           |          38|          21|       17|
|   2007|    5|Washington             |Southern California         |          24|          27|       -3|
|   2007|    5|Texas A&M              |Baylor                      |          34|          10|       24|
|   2007|    5|Texas Christian        |Colorado State              |          24|          12|       12|
|   2007|    5|Texas Tech             |Northwestern State          |          75|           7|       68|
|   2007|    5|Southern Methodist     |Texas-El Paso               |          45|          48|       -3|
|   2007|    5|Troy                   |Louisiana-Monroe            |          24|           7|       17|
|   2007|    5|Tulsa                  |Alabama-Birmingham          |          38|          30|        8|
|   2007|    5|Oregon State           |UCLA                        |          14|          40|      -26|
|   2007|    5|Utah                   |Utah State                  |          34|          18|       16|
|   2007|    5|Vanderbilt             |Eastern Michigan            |          30|           7|       23|
|   2007|    5|Virginia               |Pittsburgh                  |          44|          14|       30|
|   2007|    5|Virginia Tech          |North Carolina              |          17|          10|        7|
|   2007|    5|Toledo                 |Western Michigan            |          28|          42|      -14|
|   2007|    5|Wisconsin              |Michigan State              |          37|          34|        3|
|   2007|    6|Memphis                |Marshall                    |          24|          21|        3|
|   2007|    6|Southern Mississippi   |Rice                        |          29|          31|       -2|
|   2007|    6|South Carolina         |Kentucky                    |          38|          23|       15|
|   2007|    6|Louisville             |Utah                        |          35|          44|       -9|
|   2007|    6|Air Force              |Nevada-Las Vegas            |          31|          14|       17|
|   2007|    6|Western Michigan       |Akron                       |          38|          39|       -1|
|   2007|    6|Alabama                |Houston                     |          30|          24|        6|
|   2007|    6|Washington State       |Arizona State               |          20|          23|       -3|
|   2007|    6|Arkansas               |Chattanooga                 |          34|          15|       19|
|   2007|    6|Army                   |Tulane                      |          20|          17|        3|
|   2007|    6|Auburn                 |Vanderbilt                  |          35|           7|       28|
|   2007|    6|Boston College         |Bowling Green State         |          55|          24|       31|
|   2007|    6|Buffalo                |Ohio                        |          31|          10|       21|
|   2007|    6|Ball State             |Central Michigan            |          38|          58|      -20|
|   2007|    6|Rutgers                |Cincinnati                  |          23|          28|       -5|
|   2007|    6|Baylor                 |Colorado                    |          23|          43|      -20|
|   2007|    6|East Carolina          |Central Florida             |          52|          38|       14|
|   2007|    6|Florida State          |North Carolina State        |          27|          10|       17|
|   2007|    6|Nevada                 |Fresno State                |          41|          49|       -8|
|   2007|    6|Hawaii                 |Utah State                  |          52|          37|       15|
|   2007|    6|Illinois               |Wisconsin                   |          31|          26|        5|
|   2007|    6|Indiana                |Minnesota                   |          40|          20|       20|
|   2007|    6|Kansas State           |Kansas                      |          24|          30|       -6|
|   2007|    6|Louisiana State        |Florida                     |          28|          24|        4|
|   2007|    6|North Texas            |Louisiana-Lafayette         |          29|          38|       -9|
|   2007|    6|Louisiana-Monroe       |Arkansas State              |          30|          13|       17|
|   2007|    6|Maryland               |Georgia Tech                |          28|          26|        2|
|   2007|    6|Kent State             |Miami (OH)                  |          13|          20|       -7|
|   2007|    6|Michigan               |Eastern Michigan            |          33|          22|       11|
|   2007|    6|Mississippi            |Louisiana Tech              |          24|           0|       24|
|   2007|    6|Mississippi State      |Alabama-Birmingham          |          30|          13|       17|
|   2007|    6|Missouri               |Nebraska                    |          41|           6|       35|
|   2007|    6|North Carolina         |Miami (FL)                  |          33|          27|        6|
|   2007|    6|Michigan State         |Northwestern                |          41|          48|       -7|
|   2007|    6|UCLA                   |Notre Dame                  |           6|          20|      -14|
|   2007|    6|Purdue                 |Ohio State                  |           7|          23|      -16|
|   2007|    6|Oklahoma               |Texas                       |          28|          21|        7|
|   2007|    6|Oregon State           |Arizona                     |          31|          16|       15|
|   2007|    6|Penn State             |Iowa                        |          27|           7|       20|
|   2007|    6|Colorado State         |San Diego State             |          20|          24|       -4|
|   2007|    6|San Jose State         |Idaho                       |          28|          20|        8|
|   2007|    6|Florida Atlantic       |South Florida               |          23|          35|      -12|
|   2007|    6|Southern California    |Stanford                    |          23|          24|       -1|
|   2007|    6|Temple                 |Northern Illinois           |          16|          15|        1|
|   2007|    6|Tennessee              |Georgia                     |          35|          14|       21|
|   2007|    6|Texas A&M              |Oklahoma State              |          24|          23|        1|
|   2007|    6|Texas Tech             |Iowa State                  |          42|          17|       25|
|   2007|    6|Texas-El Paso          |Tulsa                       |          48|          47|        1|
|   2007|    6|Toledo                 |Liberty                     |          35|          34|        1|
|   2007|    6|Florida International  |Troy                        |          16|          34|      -18|
|   2007|    6|Middle Tennessee State |Virginia                    |          21|          23|       -2|
|   2007|    6|Clemson                |Virginia Tech               |          23|          41|      -18|
|   2007|    6|Duke                   |Wake Forest                 |          36|          41|       -5|
|   2007|    6|Syracuse               |West Virginia               |          14|          55|      -41|
|   2007|    6|Wyoming                |Texas Christian             |          24|          21|        3|
|   2007|    6|Boise State            |New Mexico State            |          58|           0|       58|
|   2007|    7|Pittsburgh             |Navy                        |          45|          48|       -3|
|   2007|    7|Wake Forest            |Florida State               |          24|          21|        3|
|   2007|    7|San Jose State         |Hawaii                      |          35|          42|       -7|
|   2007|    7|Colorado State         |Air Force                   |          21|          45|      -24|
|   2007|    7|Mississippi            |Alabama                     |          24|          27|       -3|
|   2007|    7|Alabama-Birmingham     |Tulane                      |          26|          21|        5|
|   2007|    7|Arizona State          |Washington                  |          44|          20|       24|
|   2007|    7|Arkansas State         |Louisiana-Lafayette         |          52|          21|       31|
|   2007|    7|Arkansas               |Auburn                      |           7|           9|       -2|
|   2007|    7|Ball State             |Western Kentucky            |          35|          12|       23|
|   2007|    7|Notre Dame             |Boston College              |          14|          27|      -13|
|   2007|    7|Nevada-Las Vegas       |Brigham Young               |          14|          24|      -10|
|   2007|    7|Buffalo                |Toledo                      |          43|          33|       10|
|   2007|    7|Central Michigan       |Army                        |          47|          23|       24|
|   2007|    7|Texas-El Paso          |East Carolina               |          42|          45|       -3|
|   2007|    7|Idaho                  |Fresno State                |          24|          37|      -13|
|   2007|    7|Vanderbilt             |Georgia                     |          17|          20|       -3|
|   2007|    7|Miami (FL)             |Georgia Tech                |          14|          17|       -3|
|   2007|    7|Houston                |Rice                        |          56|          48|        8|
|   2007|    7|Iowa                   |Illinois                    |          10|           6|        4|
|   2007|    7|Kansas                 |Baylor                      |          58|          10|       48|
|   2007|    7|Kansas State           |Colorado                    |          47|          20|       27|
|   2007|    7|Kentucky               |Louisiana State             |          43|          37|        6|
|   2007|    7|Louisiana Tech         |New Mexico State            |          22|          21|        1|
|   2007|    7|Cincinnati             |Louisville                  |          24|          28|       -4|
|   2007|    7|Miami (OH)             |Bowling Green State         |          47|          14|       33|
|   2007|    7|Michigan               |Purdue                      |          48|          21|       27|
|   2007|    7|Michigan State         |Indiana                     |          52|          27|       25|
|   2007|    7|Memphis                |Middle Tennessee State      |           7|          21|      -14|
|   2007|    7|Wyoming                |New Mexico                  |           3|          20|      -17|
|   2007|    7|North Texas            |Louisiana-Monroe            |          31|          21|       10|
|   2007|    7|Northwestern           |Minnesota                   |          49|          48|        1|
|   2007|    7|Ohio                   |Eastern Michigan            |          48|          42|        6|
|   2007|    7|Ohio State             |Kent State                  |          48|           3|       45|
|   2007|    7|Oklahoma               |Missouri                    |          41|          31|       10|
|   2007|    7|Nebraska               |Oklahoma State              |          14|          45|      -31|
|   2007|    7|Oregon                 |Washington State            |          53|           7|       46|
|   2007|    7|California             |Oregon State                |          28|          31|       -3|
|   2007|    7|Penn State             |Wisconsin                   |          38|           7|       31|
|   2007|    7|Syracuse               |Rutgers                     |          14|          38|      -24|
|   2007|    7|North Carolina         |South Carolina              |          15|          21|       -6|
|   2007|    7|South Florida          |Central Florida             |          64|          12|       52|
|   2007|    7|Southern California    |Arizona                     |          20|          13|        7|
|   2007|    7|Southern Mississippi   |Southern Methodist          |          28|           7|       21|
|   2007|    7|Akron                  |Temple                      |          20|          24|       -4|
|   2007|    7|Mississippi State      |Tennessee                   |          21|          33|      -12|
|   2007|    7|Iowa State             |Texas                       |           3|          56|      -53|
|   2007|    7|Stanford               |Texas Christian             |          36|          38|       -2|
|   2007|    7|Texas Tech             |Texas A&M                   |          35|           7|       28|
|   2007|    7|Tulsa                  |Marshall                    |          38|          31|        7|
|   2007|    7|Utah                   |San Diego State             |          23|           7|       16|
|   2007|    7|Virginia               |Connecticut                 |          17|          16|        1|
|   2007|    7|Duke                   |Virginia Tech               |          14|          43|      -29|
|   2007|    7|Northern Illinois      |Western Michigan            |          13|          17|       -4|
|   2007|    7|Boise State            |Nevada                      |          69|          67|        2|
|   2007|    8|Rutgers                |South Florida               |          30|          27|        3|
|   2007|    8|Texas Christian        |Utah                        |          20|          27|       -7|
|   2007|    8|Connecticut            |Louisville                  |          21|          17|        4|
|   2007|    8|Eastern Michigan       |Northwestern                |          14|          26|      -12|
|   2007|    8|Air Force              |Wyoming                     |          20|          12|        8|
|   2007|    8|Alabama                |Tennessee                   |          41|          17|       24|
|   2007|    8|Mississippi            |Arkansas                    |           8|          44|      -36|
|   2007|    8|Western Michigan       |Ball State                  |          23|          27|       -4|
|   2007|    8|Louisiana Tech         |Boise State                 |          31|          45|      -14|
|   2007|    8|Kent State             |Bowling Green State         |          20|          31|      -11|
|   2007|    8|Brigham Young          |Eastern Washington          |          42|           7|       35|
|   2007|    8|Central Florida        |Tulsa                       |          44|          23|       21|
|   2007|    8|Clemson                |Central Michigan            |          70|          14|       56|
|   2007|    8|Nevada-Las Vegas       |Colorado State              |          23|          48|      -25|
|   2007|    8|Kentucky               |Florida                     |          37|          45|       -8|
|   2007|    8|Louisiana-Lafayette    |Florida Atlantic            |          32|          39|       -7|
|   2007|    8|Fresno State           |San Jose State              |          30|           0|       30|
|   2007|    8|Georgia Tech           |Army                        |          34|          10|       24|
|   2007|    8|Alabama-Birmingham     |Houston                     |          10|          49|      -39|
|   2007|    8|Colorado               |Kansas                      |          14|          19|       -5|
|   2007|    8|Louisiana State        |Auburn                      |          30|          24|        6|
|   2007|    8|Louisiana-Monroe       |Florida International       |          28|          14|       14|
|   2007|    8|Rice                   |Memphis                     |          35|          38|       -3|
|   2007|    8|Florida State          |Miami (FL)                  |          29|          37|       -8|
|   2007|    8|Illinois               |Michigan                    |          17|          27|      -10|
|   2007|    8|Middle Tennessee State |Arkansas State              |          24|           7|       17|
|   2007|    8|Missouri               |Texas Tech                  |          41|          10|       31|
|   2007|    8|Utah State             |Nevada                      |          28|          31|       -3|
|   2007|    8|San Diego State        |New Mexico                  |          17|          20|       -3|
|   2007|    8|New Mexico State       |Idaho                       |          45|          31|       14|
|   2007|    8|East Carolina          |North Carolina State        |          20|          34|      -14|
|   2007|    8|Minnesota              |North Dakota State          |          21|          27|       -6|
|   2007|    8|Ohio State             |Michigan State              |          24|          17|        7|
|   2007|    8|Iowa State             |Oklahoma                    |           7|          17|      -10|
|   2007|    8|Oklahoma State         |Kansas State                |          41|          39|        2|
|   2007|    8|Washington             |Oregon                      |          34|          55|      -21|
|   2007|    8|Indiana                |Penn State                  |          31|          36|       -5|
|   2007|    8|Pittsburgh             |Cincinnati                  |          24|          17|        7|
|   2007|    8|Purdue                 |Iowa                        |          31|           6|       25|
|   2007|    8|Notre Dame             |Southern California         |           0|          38|      -38|
|   2007|    8|Arizona                |Stanford                    |          20|          21|       -1|
|   2007|    8|Syracuse               |Buffalo                     |          20|          12|        8|
|   2007|    8|Temple                 |Miami (OH)                  |          24|          17|        7|
|   2007|    8|Baylor                 |Texas                       |          10|          31|      -21|
|   2007|    8|Nebraska               |Texas A&M                   |          14|          36|      -22|
|   2007|    8|Toledo                 |Ohio                        |          43|          40|        3|
|   2007|    8|Troy                   |North Texas                 |          45|           7|       38|
|   2007|    8|Southern Methodist     |Tulane                      |          34|          41|       -7|
|   2007|    8|UCLA                   |California                  |          30|          21|        9|
|   2007|    8|South Carolina         |Vanderbilt                  |           6|          17|      -11|
|   2007|    8|Maryland               |Virginia                    |          17|          18|       -1|
|   2007|    8|Navy                   |Wake Forest                 |          24|          44|      -20|
|   2007|    8|West Virginia          |Mississippi State           |          38|          13|       25|
|   2007|    8|Indiana State          |Western Kentucky            |           7|          56|      -49|
|   2007|    8|Wisconsin              |Northern Illinois           |          44|           3|       41|
|   2007|    8|Marshall               |Southern Mississippi        |          24|          33|       -9|
|   2007|    9|Virginia Tech          |Boston College              |          10|          14|       -4|
|   2007|    9|New Mexico             |Air Force                   |          34|          31|        3|
|   2007|    9|Fresno State           |Boise State                 |          21|          34|      -13|
|   2007|    9|Washington             |Arizona                     |          41|          48|       -7|
|   2007|    9|Arizona State          |California                  |          31|          20|       11|
|   2007|    9|Arkansas               |Florida International       |          58|          10|       48|
|   2007|    9|Auburn                 |Mississippi                 |          17|           3|       14|
|   2007|    9|Buffalo                |Akron                       |          26|          10|       16|
|   2007|    9|Kent State             |Central Michigan            |          32|          41|       -9|
|   2007|    9|Maryland               |Clemson                     |          17|          30|      -13|
|   2007|    9|Texas Tech             |Colorado                    |          26|          31|       -5|
|   2007|    9|Connecticut            |South Florida               |          22|          15|        7|
|   2007|    9|Navy                   |Delaware                    |          52|          59|       -7|
|   2007|    9|East Carolina          |Alabama-Birmingham          |          41|           6|       35|
|   2007|    9|Eastern Michigan       |Western Michigan            |          19|           2|       17|
|   2007|    9|Florida State          |Duke                        |          25|           6|       19|
|   2007|    9|Georgia                |Florida                     |          42|          30|       12|
|   2007|    9|Hawaii                 |New Mexico State            |          50|          13|       37|
|   2007|    9|Texas-El Paso          |Houston                     |          31|          34|       -3|
|   2007|    9|Illinois               |Ball State                  |          28|          17|       11|
|   2007|    9|Iowa                   |Michigan State              |          34|          27|        7|
|   2007|    9|Texas A&M              |Kansas                      |          11|          19|       -8|
|   2007|    9|Kansas State           |Baylor                      |          51|          13|       38|
|   2007|    9|Utah State             |Louisiana Tech              |          21|          31|      -10|
|   2007|    9|Florida Atlantic       |Louisiana-Monroe            |          30|          33|       -3|
|   2007|    9|Louisville             |Pittsburgh                  |          24|          17|        7|
|   2007|    9|Marshall               |Rice                        |          34|          21|       13|
|   2007|    9|Tulane                 |Memphis                     |          27|          28|       -1|
|   2007|    9|Michigan               |Minnesota                   |          34|          10|       24|
|   2007|    9|North Texas            |Middle Tennessee State      |          28|          48|      -20|
|   2007|    9|Kentucky               |Mississippi State           |          14|          31|      -17|
|   2007|    9|Missouri               |Iowa State                  |          42|          28|       14|
|   2007|    9|Nevada                 |Idaho                       |          37|          21|       16|
|   2007|    9|North Carolina State   |Virginia                    |          29|          24|        5|
|   2007|    9|Bowling Green State    |Ohio                        |          27|          38|      -11|
|   2007|    9|Penn State             |Ohio State                  |          17|          37|      -20|
|   2007|    9|Oregon                 |Southern California         |          24|          17|        7|
|   2007|    9|Oregon State           |Stanford                    |          23|           6|       17|
|   2007|    9|Purdue                 |Northwestern                |          35|          17|       18|
|   2007|    9|Tennessee              |South Carolina              |          27|          24|        3|
|   2007|    9|Texas                  |Nebraska                    |          28|          25|        3|
|   2007|    9|Toledo                 |Northern Illinois           |          70|          21|       49|
|   2007|    9|Arkansas State         |Troy                        |           0|          27|      -27|
|   2007|    9|Tulsa                  |Southern Methodist          |          29|          23|        6|
|   2007|    9|Colorado State         |Utah                        |           3|          27|      -24|
|   2007|    9|Vanderbilt             |Miami (OH)                  |          24|          13|       11|
|   2007|    9|Wake Forest            |North Carolina              |          37|          10|       27|
|   2007|    9|Washington State       |UCLA                        |          27|           7|       20|
|   2007|    9|Rutgers                |West Virginia               |           3|          31|      -28|
|   2007|    9|Western Kentucky       |North Carolina Central      |          50|          14|       36|
|   2007|    9|Wisconsin              |Indiana                     |          33|           3|       30|
|   2007|    9|Wyoming                |Nevada-Las Vegas            |          29|          24|        5|
|   2007|    9|Southern Mississippi   |Central Florida             |          17|          34|      -17|
|   2007|   10|Georgia Tech           |Virginia Tech               |           3|          27|      -24|
|   2007|   10|Bowling Green State    |Akron                       |          44|          20|       24|
|   2007|   10|New Mexico State       |Nevada                      |          38|          40|       -2|
|   2007|   10|Ohio                   |Temple                      |          23|           7|       16|
|   2007|   10|Air Force              |Army                        |          30|          10|       20|
|   2007|   10|Arizona                |UCLA                        |          34|          27|        7|
|   2007|   10|Arkansas               |South Carolina              |          48|          36|       12|
|   2007|   10|Arkansas State         |Florida International       |          27|          24|        3|
|   2007|   10|Auburn                 |Tennessee Tech              |          35|           3|       32|
|   2007|   10|Boise State            |San Jose State              |          42|           7|       35|
|   2007|   10|Brigham Young          |Colorado State              |          35|          16|       19|
|   2007|   10|California             |Washington State            |          20|          17|        3|
|   2007|   10|Central Florida        |Marshall                    |          47|          13|       34|
|   2007|   10|South Florida          |Cincinnati                  |          33|          38|       -5|
|   2007|   10|Duke                   |Clemson                     |          10|          47|      -37|
|   2007|   10|Connecticut            |Rutgers                     |          38|          19|       19|
|   2007|   10|Memphis                |East Carolina               |          40|          56|      -16|
|   2007|   10|Florida                |Vanderbilt                  |          49|          22|       27|
|   2007|   10|Boston College         |Florida State               |          17|          27|      -10|
|   2007|   10|Fresno State           |Utah State                  |          38|          27|       11|
|   2007|   10|Georgia                |Troy                        |          44|          34|       10|
|   2007|   10|Minnesota              |Illinois                    |          17|          44|      -27|
|   2007|   10|Indiana                |Ball State                  |          38|          20|       18|
|   2007|   10|Northwestern           |Iowa                        |          17|          28|      -11|
|   2007|   10|Iowa State             |Kansas State                |          31|          20|       11|
|   2007|   10|Kansas                 |Nebraska                    |          76|          39|       37|
|   2007|   10|Alabama                |Louisiana State             |          34|          41|       -7|
|   2007|   10|Idaho                  |Louisiana Tech              |          16|          28|      -12|
|   2007|   10|Miami (OH)             |Buffalo                     |          31|          28|        3|
|   2007|   10|Michigan State         |Michigan                    |          24|          28|       -4|
|   2007|   10|Louisiana-Monroe       |Middle Tennessee State      |          40|          43|       -3|
|   2007|   10|Mississippi            |Northwestern State          |          38|          31|        7|
|   2007|   10|Colorado               |Missouri                    |          10|          55|      -45|
|   2007|   10|Notre Dame             |Navy                        |          44|          46|       -2|
|   2007|   10|North Carolina         |Maryland                    |          16|          13|        3|
|   2007|   10|Miami (FL)             |North Carolina State        |          16|          19|       -3|
|   2007|   10|Ohio State             |Wisconsin                   |          38|          17|       21|
|   2007|   10|Oklahoma               |Texas A&M                   |          42|          14|       28|
|   2007|   10|Oregon                 |Arizona State               |          35|          23|       12|
|   2007|   10|Penn State             |Purdue                      |          26|          19|        7|
|   2007|   10|Pittsburgh             |Syracuse                    |          20|          17|        3|
|   2007|   10|Rice                   |Texas-El Paso               |          56|          48|        8|
|   2007|   10|San Diego State        |Wyoming                     |          27|          24|        3|
|   2007|   10|Southern California    |Oregon State                |          24|           3|       21|
|   2007|   10|Alabama-Birmingham     |Southern Mississippi        |           7|          37|      -30|
|   2007|   10|Tennessee              |Louisiana-Lafayette         |          59|           7|       52|
|   2007|   10|Oklahoma State         |Texas                       |          35|          38|       -3|
|   2007|   10|Texas Christian        |New Mexico                  |          37|           0|       37|
|   2007|   10|Baylor                 |Texas Tech                  |           7|          38|      -31|
|   2007|   10|Toledo                 |Eastern Michigan            |          52|          28|       24|
|   2007|   10|Tulane                 |Tulsa                       |          25|          49|      -24|
|   2007|   10|Virginia               |Wake Forest                 |          17|          16|        1|
|   2007|   10|Stanford               |Washington                  |           9|          27|      -18|
|   2007|   10|Chattanooga            |Western Kentucky            |          21|          28|       -7|
|   2007|   10|Houston                |Southern Methodist          |          38|          28|       10|
|   2007|   11|Western Michigan       |Central Michigan            |          31|          34|       -3|
|   2007|   11|Akron                  |Ohio                        |          48|          37|       11|
|   2007|   11|Brigham Young          |Texas Christian             |          27|          22|        5|
|   2007|   11|West Virginia          |Louisville                  |          38|          31|        7|
|   2007|   11|Eastern Michigan       |Bowling Green State         |          32|          39|       -7|
|   2007|   11|Army                   |Rutgers                     |           6|          41|      -35|
|   2007|   11|Notre Dame             |Air Force                   |          24|          41|      -17|
|   2007|   11|UCLA                   |Arizona State               |          20|          24|       -4|
|   2007|   11|Utah State             |Boise State                 |           0|          52|      -52|
|   2007|   11|Alabama-Birmingham     |Central Florida             |          31|          45|      -14|
|   2007|   11|Cincinnati             |Connecticut                 |          27|           3|       24|
|   2007|   11|Clemson                |Wake Forest                 |          44|          10|       34|
|   2007|   11|South Carolina         |Florida                     |          31|          51|      -20|
|   2007|   11|Florida Atlantic       |Arkansas State              |          34|          31|        3|
|   2007|   11|Georgia                |Auburn                      |          45|          20|       25|
|   2007|   11|Duke                   |Georgia Tech                |          24|          41|      -17|
|   2007|   11|Hawaii                 |Fresno State                |          37|          30|        7|
|   2007|   11|Ohio State             |Illinois                    |          21|          28|       -7|
|   2007|   11|Iowa                   |Minnesota                   |          21|          16|        5|
|   2007|   11|Iowa State             |Colorado                    |          31|          28|        3|
|   2007|   11|Oklahoma State         |Kansas                      |          28|          43|      -15|
|   2007|   11|Vanderbilt             |Kentucky                    |          20|          27|       -7|
|   2007|   11|Louisiana State        |Louisiana Tech              |          58|          10|       48|
|   2007|   11|Middle Tennessee State |Louisiana-Lafayette         |          24|          34|      -10|
|   2007|   11|Louisiana-Monroe       |Grambling State             |          28|          14|       14|
|   2007|   11|Marshall               |East Carolina               |          26|           7|       19|
|   2007|   11|Maryland               |Boston College              |          42|          35|        7|
|   2007|   11|Southern Mississippi   |Memphis                     |          26|          29|       -3|
|   2007|   11|Purdue                 |Michigan State              |          31|          48|      -17|
|   2007|   11|Mississippi State      |Alabama                     |          17|          12|        5|
|   2007|   11|Missouri               |Texas A&M                   |          40|          26|       14|
|   2007|   11|North Texas            |Navy                        |          62|          74|      -12|
|   2007|   11|Nebraska               |Kansas State                |          73|          31|       42|
|   2007|   11|New Mexico             |Colorado State              |          26|          23|        3|
|   2007|   11|North Carolina State   |North Carolina              |          31|          27|        4|
|   2007|   11|Northern Illinois      |Kent State                  |          27|          20|        7|
|   2007|   11|Northwestern           |Indiana                     |          31|          28|        3|
|   2007|   11|Oklahoma               |Baylor                      |          52|          21|       31|
|   2007|   11|Oregon State           |Washington                  |          29|          23|        6|
|   2007|   11|Temple                 |Penn State                  |           0|          31|      -31|
|   2007|   11|Southern Methodist     |Rice                        |          42|          43|       -1|
|   2007|   11|Nevada-Las Vegas       |San Diego State             |          30|          38|       -8|
|   2007|   11|San Jose State         |New Mexico State            |          51|          17|       34|
|   2007|   11|Syracuse               |South Florida               |          10|          41|      -31|
|   2007|   11|California             |Southern California         |          17|          24|       -7|
|   2007|   11|Tennessee              |Arkansas                    |          34|          13|       21|
|   2007|   11|Texas                  |Texas Tech                  |          59|          43|       16|
|   2007|   11|Western Kentucky       |Troy                        |          17|          21|       -4|
|   2007|   11|Tulane                 |Texas-El Paso               |          34|          19|       15|
|   2007|   11|Tulsa                  |Houston                     |          56|           7|       49|
|   2007|   11|Utah                   |Wyoming                     |          50|           0|       50|
|   2007|   11|Miami (FL)             |Virginia                    |           0|          48|      -48|
|   2007|   11|Virginia Tech          |Florida State               |          40|          21|       19|
|   2007|   11|Washington State       |Stanford                    |          33|          17|       16|
|   2007|   11|Wisconsin              |Michigan                    |          37|          21|       16|
|   2007|   12|Ball State             |Toledo                      |          41|          20|       21|
|   2007|   12|Miami (OH)             |Akron                       |           7|           0|        7|
|   2007|   12|Arizona                |Oregon                      |          34|          24|       10|
|   2007|   12|Arkansas State         |North Texas                 |          31|          27|        4|
|   2007|   12|Central Michigan       |Eastern Michigan            |          45|          48|       -3|
|   2007|   12|Nevada                 |Hawaii                      |          26|          28|       -2|
|   2007|   12|Air Force              |San Diego State             |          55|          23|       32|
|   2007|   12|Arkansas               |Mississippi State           |          45|          31|       14|
|   2007|   12|Boise State            |Idaho                       |          58|          14|       44|
|   2007|   12|Clemson                |Boston College              |          17|          20|       -3|
|   2007|   12|Buffalo                |Bowling Green State         |          17|          31|      -14|
|   2007|   12|Wyoming                |Brigham Young               |          10|          35|      -25|
|   2007|   12|Southern Methodist     |Central Florida             |          20|          49|      -29|
|   2007|   12|Colorado State         |Georgia Southern            |          42|          34|        8|
|   2007|   12|Connecticut            |Syracuse                    |          30|           7|       23|
|   2007|   12|Florida                |Florida Atlantic            |          59|          20|       39|
|   2007|   12|Florida State          |Maryland                    |          24|          16|        8|
|   2007|   12|Georgia                |Kentucky                    |          24|          13|       11|
|   2007|   12|Georgia Tech           |North Carolina              |          27|          25|        2|
|   2007|   12|Houston                |Marshall                    |          35|          28|        7|
|   2007|   12|Illinois               |Northwestern                |          41|          22|       19|
|   2007|   12|Indiana                |Purdue                      |          27|          24|        3|
|   2007|   12|Kansas                 |Iowa State                  |          45|           7|       38|
|   2007|   12|Mississippi            |Louisiana State             |          24|          41|      -17|
|   2007|   12|Louisiana Tech         |San Jose State              |          27|          23|        4|
|   2007|   12|Louisiana-Lafayette    |Florida International       |          38|          28|       10|
|   2007|   12|Alabama                |Louisiana-Monroe            |          14|          21|       -7|
|   2007|   12|Memphis                |Alabama-Birmingham          |          25|           9|       16|
|   2007|   12|Michigan State         |Penn State                  |          35|          31|        4|
|   2007|   12|Kansas State           |Missouri                    |          32|          49|      -17|
|   2007|   12|Navy                   |Northern Illinois           |          35|          24|       11|
|   2007|   12|Notre Dame             |Duke                        |          28|           7|       21|
|   2007|   12|Michigan               |Ohio State                  |           3|          14|      -11|
|   2007|   12|Baylor                 |Oklahoma State              |          14|          45|      -31|
|   2007|   12|Washington State       |Oregon State                |          17|          52|      -35|
|   2007|   12|Rutgers                |Pittsburgh                  |          20|          16|        4|
|   2007|   12|South Florida          |Louisville                  |          55|          17|       38|
|   2007|   12|Texas-El Paso          |Southern Mississippi        |          30|          56|      -26|
|   2007|   12|Temple                 |Kent State                  |          24|          14|       10|
|   2007|   12|Tennessee              |Vanderbilt                  |          25|          24|        1|
|   2007|   12|Texas Christian        |Nevada-Las Vegas            |          34|          10|       24|
|   2007|   12|Texas Tech             |Oklahoma                    |          34|          27|        7|
|   2007|   12|Rice                   |Tulane                      |          31|          45|      -14|
|   2007|   12|Army                   |Tulsa                       |          39|          49|      -10|
|   2007|   12|Utah                   |New Mexico                  |          28|          10|       18|
|   2007|   12|New Mexico State       |Utah State                  |          17|          35|      -18|
|   2007|   12|Virginia Tech          |Miami (FL)                  |          44|          14|       30|
|   2007|   12|Wake Forest            |North Carolina State        |          38|          18|       20|
|   2007|   12|Washington             |California                  |          37|          23|       14|
|   2007|   12|Cincinnati             |West Virginia               |          23|          28|       -5|
|   2007|   12|Western Kentucky       |Morehead State              |          52|          12|       40|
|   2007|   12|Iowa                   |Western Michigan            |          19|          28|       -9|
|   2007|   12|Minnesota              |Wisconsin                   |          34|          41|       -7|
|   2007|   13|Troy                   |Middle Tennessee State      |          45|           7|       38|
|   2007|   13|Arizona State          |Southern California         |          24|          44|      -20|
|   2007|   13|Louisiana State        |Arkansas                    |          48|          50|       -2|
|   2007|   13|Bowling Green State    |Toledo                      |          37|          10|       27|
|   2007|   13|Akron                  |Central Michigan            |          32|          35|       -3|
|   2007|   13|Colorado               |Nebraska                    |          65|          51|       14|
|   2007|   13|Colorado State         |Wyoming                     |          36|          28|        8|
|   2007|   13|Hawaii                 |Boise State                 |          39|          27|       12|
|   2007|   13|Mississippi State      |Mississippi                 |          17|          14|        3|
|   2007|   13|Texas A&M              |Texas                       |          38|          30|        8|
|   2007|   13|Auburn                 |Alabama                     |          17|          10|        7|
|   2007|   13|Northern Illinois      |Ball State                  |          21|          27|       -6|
|   2007|   13|Boston College         |Miami (FL)                  |          28|          14|       14|
|   2007|   13|Brigham Young          |Utah                        |          17|          10|        7|
|   2007|   13|Kent State             |Buffalo                     |          23|          30|       -7|
|   2007|   13|Central Florida        |Texas-El Paso               |          36|          20|       16|
|   2007|   13|Syracuse               |Cincinnati                  |          31|          52|      -21|
|   2007|   13|South Carolina         |Clemson                     |          21|          23|       -2|
|   2007|   13|East Carolina          |Tulane                      |          35|          12|       23|
|   2007|   13|Florida                |Florida State               |          45|          12|       33|
|   2007|   13|Florida International  |Florida Atlantic            |          23|          55|      -32|
|   2007|   13|Fresno State           |Kansas State                |          45|          29|       16|
|   2007|   13|Georgia Tech           |Georgia                     |          17|          31|      -14|
|   2007|   13|Houston                |Texas Southern              |          59|           6|       53|
|   2007|   13|Louisiana-Lafayette    |Louisiana-Monroe            |          11|          17|       -6|
|   2007|   13|Marshall               |Alabama-Birmingham          |          46|          39|        7|
|   2007|   13|North Carolina State   |Maryland                    |           0|          37|      -37|
|   2007|   13|Memphis                |Southern Methodist          |          55|          52|        3|
|   2007|   13|Missouri               |Kansas                      |          36|          28|        8|
|   2007|   13|New Mexico             |Nevada-Las Vegas            |          27|           6|       21|
|   2007|   13|North Carolina         |Duke                        |          20|          14|        6|
|   2007|   13|North Texas            |Western Kentucky            |          27|          26|        1|
|   2007|   13|Stanford               |Notre Dame                  |          14|          21|       -7|
|   2007|   13|Ohio                   |Miami (OH)                  |          38|          29|        9|
|   2007|   13|Oklahoma               |Oklahoma State              |          49|          17|       32|
|   2007|   13|San Jose State         |Nevada                      |          27|          24|        3|
|   2007|   13|Pittsburgh             |South Florida               |          37|          48|      -11|
|   2007|   13|Southern Mississippi   |Arkansas State              |          16|          10|        6|
|   2007|   13|Kentucky               |Tennessee                   |          50|          52|       -2|
|   2007|   13|San Diego State        |Texas Christian             |          33|          45|      -12|
|   2007|   13|Rice                   |Tulsa                       |          43|          48|       -5|
|   2007|   13|UCLA                   |Oregon                      |          16|           0|       16|
|   2007|   13|Idaho                  |Utah State                  |          19|          24|       -5|
|   2007|   13|Virginia               |Virginia Tech               |          21|          33|      -12|
|   2007|   13|Vanderbilt             |Wake Forest                 |          17|          31|      -14|
|   2007|   13|Washington             |Washington State            |          35|          42|       -7|
|   2007|   13|West Virginia          |Connecticut                 |          66|          21|       45|
|   2007|   13|Western Michigan       |Temple                      |          16|           3|       13|
|   2007|   14|Louisville             |Rutgers                     |          41|          38|        3|
|   2007|   14|New Mexico State       |Fresno State                |          23|          30|       -7|
|   2007|   14|Arizona State          |Arizona                     |          20|          17|        3|
|   2007|   14|San Diego State        |Brigham Young               |          27|          48|      -21|
|   2007|   14|Central Florida        |Tulsa                       |          44|          25|       19|
|   2007|   14|Central Michigan       |Miami (OH)                  |          35|          10|       25|
|   2007|   14|Troy                   |Florida Atlantic            |          32|          38|       -6|
|   2007|   14|Florida International  |North Texas                 |          38|          19|       19|
|   2007|   14|Hawaii                 |Washington                  |          35|          28|        7|
|   2007|   14|Louisiana State        |Tennessee                   |          21|          14|        7|
|   2007|   14|Navy                   |Army                        |          38|           3|       35|
|   2007|   14|Nevada                 |Louisiana Tech              |          49|          10|       39|
|   2007|   14|Oklahoma               |Missouri                    |          38|          17|       21|
|   2007|   14|Oregon                 |Oregon State                |          31|          38|       -7|
|   2007|   14|West Virginia          |Pittsburgh                  |           9|          13|       -4|
|   2007|   14|Southern California    |UCLA                        |          24|           7|       17|
|   2007|   14|Stanford               |California                  |          20|          13|        7|
|   2007|   14|Virginia Tech          |Boston College              |          30|          16|       14|
|   2007|   15|Utah                   |Navy                        |          35|          32|        3|
|   2007|   15|Florida Atlantic       |Memphis                     |          44|          27|       17|
|   2007|   15|Brigham Young          |UCLA                        |          17|          16|        1|
|   2007|   15|Cincinnati             |Southern Mississippi        |          31|          21|       10|
|   2007|   15|New Mexico             |Nevada                      |          23|           0|       23|
|   2007|   15|East Carolina          |Boise State                 |          41|          38|        3|
|   2007|   16|Purdue                 |Central Michigan            |          51|          48|        3|
|   2007|   16|Texas                  |Arizona State               |          52|          34|       18|
|   2007|   16|Boston College         |Michigan State              |          24|          21|        3|
|   2007|   16|Oregon State           |Maryland                    |          21|          14|        7|
|   2007|   16|Houston                |Texas Christian             |          13|          20|       -7|
|   2007|   16|Mississippi State      |Central Florida             |          10|           3|        7|
|   2007|   16|Penn State             |Texas A&M                   |          24|          17|        7|
|   2007|   16|Wake Forest            |Connecticut                 |          24|          10|       14|
|   2007|   16|Alabama                |Colorado                    |          30|          24|        6|
|   2007|   17|Auburn                 |Clemson                     |          23|          20|        3|
|   2007|   17|California             |Air Force                   |          42|          36|        6|
|   2007|   17|Fresno State           |Georgia Tech                |          40|          28|       12|
|   2007|   17|Kentucky               |Florida State               |          35|          28|        7|
|   2007|   17|Oklahoma State         |Indiana                     |          49|          33|       16|
|   2007|   17|Oregon                 |South Florida               |          56|          21|       35|
|   2007|   18|Georgia                |Hawaii                      |          41|          10|       31|
|   2007|   18|Michigan               |Florida                     |          41|          35|        6|
|   2007|   18|Missouri               |Arkansas                    |          38|           7|       31|
|   2007|   18|Southern California    |Illinois                    |          49|          17|       32|
|   2007|   18|Tennessee              |Wisconsin                   |          21|          17|        4|
|   2007|   18|Texas Tech             |Virginia                    |          31|          28|        3|
|   2007|   18|West Virginia          |Oklahoma                    |          48|          28|       20|
|   2007|   18|Kansas                 |Virginia Tech               |          24|          21|        3|
|   2007|   18|Rutgers                |Ball State                  |          52|          30|       22|
|   2007|   18|Tulsa                  |Bowling Green State         |          63|           7|       56|
|   2007|   18|Louisiana State        |Ohio State                  |          38|          24|       14|
|   2008|    1|Ball State             |Northeastern                |          48|          14|       34|
|   2008|    1|Buffalo                |Texas-El Paso               |          42|          17|       25|
|   2008|    1|Central Michigan       |Eastern Illinois            |          31|          12|       19|
|   2008|    1|Cincinnati             |Eastern Kentucky            |          40|           7|       33|
|   2008|    1|Connecticut            |Hofstra                     |          35|           3|       32|
|   2008|    1|Eastern Michigan       |Indiana State               |          52|           0|       52|
|   2008|    1|Georgia Tech           |Jacksonville State          |          41|          14|       27|
|   2008|    1|Iowa State             |South Dakota State          |          44|          17|       27|
|   2008|    1|Miami (FL)             |Charleston Southern         |          52|           7|       45|
|   2008|    1|South Carolina         |North Carolina State        |          34|           0|       34|
|   2008|    1|Stanford               |Oregon State                |          36|          28|        8|
|   2008|    1|Middle Tennessee State |Troy                        |          17|          31|      -14|
|   2008|    1|Miami (OH)             |Vanderbilt                  |          13|          34|      -21|
|   2008|    1|Baylor                 |Wake Forest                 |          13|          41|      -28|
|   2008|    1|Rice                   |Southern Methodist          |          56|          27|       29|
|   2008|    1|Army                   |Temple                      |           7|          35|      -28|
|   2008|    1|Air Force              |Southern Utah               |          41|           7|       34|
|   2008|    1|Alabama                |Clemson                     |          34|          10|       24|
|   2008|    1|Arizona                |Idaho                       |          70|           0|       70|
|   2008|    1|Arizona State          |Northern Arizona            |          30|          13|       17|
|   2008|    1|Arkansas               |Western Illinois            |          28|          24|        4|
|   2008|    1|Texas A&M              |Arkansas State              |          14|          18|       -4|
|   2008|    1|Auburn                 |Louisiana-Monroe            |          34|           0|       34|
|   2008|    1|Boise State            |Idaho State                 |          49|           7|       42|
|   2008|    1|Kent State             |Boston College              |           0|          21|      -21|
|   2008|    1|Pittsburgh             |Bowling Green State         |          17|          27|      -10|
|   2008|    1|Brigham Young          |Northern Iowa               |          41|          17|       24|
|   2008|    1|San Diego State        |Cal Poly                    |          27|          29|       -2|
|   2008|    1|California             |Michigan State              |          38|          31|        7|
|   2008|    1|Central Florida        |South Carolina State        |          17|           0|       17|
|   2008|    1|Duke                   |James Madison               |          31|           7|       24|
|   2008|    1|East Carolina          |Virginia Tech               |          27|          22|        5|
|   2008|    1|Florida                |Hawaii                      |          56|          10|       46|
|   2008|    1|Georgia                |Georgia Southern            |          45|          21|       24|
|   2008|    1|Houston                |Southern                    |          55|           3|       52|
|   2008|    1|Indiana                |Western Kentucky            |          31|          13|       18|
|   2008|    1|Iowa                   |Maine                       |          46|           3|       43|
|   2008|    1|Kansas                 |Florida International       |          40|          10|       30|
|   2008|    1|Kansas State           |North Texas                 |          45|           6|       39|
|   2008|    1|Louisiana State        |Appalachian State           |          41|          13|       28|
|   2008|    1|Louisiana Tech         |Mississippi State           |          22|          14|        8|
|   2008|    1|Marshall               |Illinois State              |          35|          10|       25|
|   2008|    1|Maryland               |Delaware                    |          14|           7|        7|
|   2008|    1|Minnesota              |Northern Illinois           |          31|          27|        4|
|   2008|    1|Mississippi            |Memphis                     |          41|          24|       17|
|   2008|    1|Missouri               |Illinois                    |          52|          42|       10|
|   2008|    1|Navy                   |Towson                      |          41|          13|       28|
|   2008|    1|Nebraska               |Western Michigan            |          47|          24|       23|
|   2008|    1|Nevada                 |Grambling State             |          49|          13|       36|
|   2008|    1|Nevada-Las Vegas       |Utah State                  |          27|          17|       10|
|   2008|    1|North Carolina         |McNeese State               |          35|          27|        8|
|   2008|    1|Northwestern           |Syracuse                    |          30|          10|       20|
|   2008|    1|Ohio State             |Youngstown State            |          43|           0|       43|
|   2008|    1|Oklahoma               |Chattanooga                 |          57|           2|       55|
|   2008|    1|Washington State       |Oklahoma State              |          13|          39|      -26|
|   2008|    1|Oregon                 |Washington                  |          44|          10|       34|
|   2008|    1|Penn State             |Coastal Carolina            |          66|          10|       56|
|   2008|    1|San Jose State         |California-Davis            |          13|          10|        3|
|   2008|    1|South Florida          |Tennessee-Martin            |          56|           7|       49|
|   2008|    1|Virginia               |Southern California         |           7|          52|      -45|
|   2008|    1|Southern Mississippi   |Louisiana-Lafayette         |          51|          21|       30|
|   2008|    1|Texas                  |Florida Atlantic            |          52|          10|       42|
|   2008|    1|New Mexico             |Texas Christian             |           3|          26|      -23|
|   2008|    1|Texas Tech             |Eastern Washington          |          49|          24|       25|
|   2008|    1|Alabama-Birmingham     |Tulsa                       |          22|          45|      -23|
|   2008|    1|Michigan               |Utah                        |          23|          25|       -2|
|   2008|    1|West Virginia          |Villanova                   |          48|          21|       27|
|   2008|    1|Wisconsin              |Akron                       |          38|          17|       21|
|   2008|    1|Wyoming                |Ohio                        |          21|          20|        1|
|   2008|    1|Colorado               |Colorado State              |          38|          17|       21|
|   2008|    1|Louisville             |Kentucky                    |           2|          27|      -25|
|   2008|    1|Rutgers                |Fresno State                |           7|          24|      -17|
|   2008|    1|UCLA                   |Tennessee                   |          27|          24|        3|
|   2008|    2|Vanderbilt             |South Carolina              |          24|          17|        7|
|   2008|    2|Ball State             |Navy                        |          35|          23|       12|
|   2008|    2|Wyoming                |Air Force                   |           3|          23|      -20|
|   2008|    2|Syracuse               |Akron                       |          28|          42|      -14|
|   2008|    2|Alabama                |Tulane                      |          20|           6|       14|
|   2008|    2|Arizona                |Toledo                      |          41|          16|       25|
|   2008|    2|Arizona State          |Stanford                    |          41|          17|       24|
|   2008|    2|Arkansas               |Louisiana-Monroe            |          28|          27|        1|
|   2008|    2|Arkansas State         |Texas Southern              |          83|          10|       73|
|   2008|    2|Auburn                 |Southern Mississippi        |          27|          13|       14|
|   2008|    2|Baylor                 |Northwestern State          |          51|           6|       45|
|   2008|    2|Washington             |Brigham Young               |          27|          28|       -1|
|   2008|    2|Washington State       |California                  |           3|          66|      -63|
|   2008|    2|Clemson                |Citadel                     |          45|          17|       28|
|   2008|    2|Colorado               |Eastern Washington          |          31|          24|        7|
|   2008|    2|Colorado State         |Sacramento State            |          23|          20|        3|
|   2008|    2|Temple                 |Connecticut                 |           9|          12|       -3|
|   2008|    2|East Carolina          |West Virginia               |          24|           3|       21|
|   2008|    2|Florida                |Miami (FL)                  |          26|           3|       23|
|   2008|    2|Florida Atlantic       |Alabama-Birmingham          |          49|          34|       15|
|   2008|    2|Florida State          |Western Carolina            |          69|           0|       69|
|   2008|    2|Georgia                |Central Michigan            |          56|          17|       39|
|   2008|    2|Boston College         |Georgia Tech                |          16|          19|       -3|
|   2008|    2|Hawaii                 |Weber State                 |          36|          17|       19|
|   2008|    2|Idaho                  |Idaho State                 |          42|          27|       15|
|   2008|    2|Illinois               |Eastern Illinois            |          47|          21|       26|
|   2008|    2|Indiana                |Murray State                |          45|           3|       42|
|   2008|    2|Iowa                   |Florida International       |          42|           0|       42|
|   2008|    2|Iowa State             |Kent State                  |          48|          28|       20|
|   2008|    2|Kansas                 |Louisiana Tech              |          29|           0|       29|
|   2008|    2|Kansas State           |Montana State               |          69|          10|       59|
|   2008|    2|Kentucky               |Norfolk State               |          38|           3|       35|
|   2008|    2|Louisville             |Tennessee Tech              |          51|          10|       41|
|   2008|    2|Michigan               |Miami (OH)                  |          16|           6|       10|
|   2008|    2|Michigan State         |Eastern Michigan            |          42|          10|       32|
|   2008|    2|Middle Tennessee State |Maryland                    |          24|          14|       10|
|   2008|    2|Bowling Green State    |Minnesota                   |          17|          42|      -25|
|   2008|    2|Mississippi State      |Southeastern Louisiana      |          34|          10|       24|
|   2008|    2|Missouri               |Southeast Missouri State    |          52|           3|       49|
|   2008|    2|Nebraska               |San Jose State              |          35|          12|       23|
|   2008|    2|Army                   |New Hampshire               |          10|          28|      -18|
|   2008|    2|North Carolina State   |William & Mary              |          34|          24|       10|
|   2008|    2|Duke                   |Northwestern                |          20|          24|       -4|
|   2008|    2|Notre Dame             |San Diego State             |          21|          13|        8|
|   2008|    2|Ohio State             |Ohio                        |          26|          14|       12|
|   2008|    2|Oklahoma               |Cincinnati                  |          52|          26|       26|
|   2008|    2|Oklahoma State         |Houston                     |          56|          37|       19|
|   2008|    2|Oregon                 |Utah State                  |          66|          24|       42|
|   2008|    2|Penn State             |Oregon State                |          45|          14|       31|
|   2008|    2|Pittsburgh             |Buffalo                     |          27|          16|       11|
|   2008|    2|Purdue                 |Northern Colorado           |          42|          10|       32|
|   2008|    2|Memphis                |Rice                        |          35|          42|       -7|
|   2008|    2|Central Florida        |South Florida               |          24|          31|       -7|
|   2008|    2|Southern Methodist     |Texas State                 |          47|          36|       11|
|   2008|    2|Texas-El Paso          |Texas                       |          13|          42|      -29|
|   2008|    2|New Mexico             |Texas A&M                   |          22|          28|       -6|
|   2008|    2|Texas Christian        |Stephen F. Austin           |          67|           7|       60|
|   2008|    2|Nevada                 |Texas Tech                  |          19|          35|      -16|
|   2008|    2|North Texas            |Tulsa                       |          26|          56|      -30|
|   2008|    2|Utah                   |Nevada-Las Vegas            |          42|          21|       21|
|   2008|    2|Virginia               |Richmond                    |          16|           0|       16|
|   2008|    2|Virginia Tech          |Furman                      |          24|           7|       17|
|   2008|    2|Wake Forest            |Mississippi                 |          30|          28|        2|
|   2008|    2|Eastern Kentucky       |Western Kentucky            |          13|          37|      -24|
|   2008|    2|Western Michigan       |Northern Illinois           |          29|          26|        3|
|   2008|    2|Wisconsin              |Marshall                    |          51|          14|       37|
|   2008|    3|Rutgers                |North Carolina              |          12|          44|      -32|
|   2008|    3|Baylor                 |Washington State            |          45|          17|       28|
|   2008|    3|Louisiana-Monroe       |Alabama A&M                 |          37|          15|       22|
|   2008|    3|South Florida          |Kansas                      |          37|          34|        3|
|   2008|    3|Houston                |Air Force                   |          28|          31|       -3|
|   2008|    3|Alabama                |Western Kentucky            |          41|           7|       34|
|   2008|    3|Mississippi State      |Auburn                      |           2|           3|       -1|
|   2008|    3|Akron                  |Ball State                  |          24|          41|      -17|
|   2008|    3|Boise State            |Bowling Green State         |          20|           7|       13|
|   2008|    3|Brigham Young          |UCLA                        |          59|           0|       59|
|   2008|    3|Buffalo                |Temple                      |          30|          28|        2|
|   2008|    3|Ohio                   |Central Michigan            |          28|          31|       -3|
|   2008|    3|Clemson                |North Carolina State        |          27|           9|       18|
|   2008|    3|Connecticut            |Virginia                    |          45|          10|       35|
|   2008|    3|Duke                   |Navy                        |          41|          31|       10|
|   2008|    3|Tulane                 |East Carolina               |          24|          28|       -4|
|   2008|    3|Florida State          |Chattanooga                 |          46|           7|       39|
|   2008|    3|South Carolina         |Georgia                     |           7|          14|       -7|
|   2008|    3|Illinois               |Louisiana-Lafayette         |          20|          17|        3|
|   2008|    3|Iowa                   |Iowa State                  |          17|           5|       12|
|   2008|    3|Kent State             |Delaware State              |          24|           3|       21|
|   2008|    3|Kentucky               |Middle Tennessee State      |          20|          14|        6|
|   2008|    3|Louisiana State        |North Texas                 |          41|           3|       38|
|   2008|    3|Marshall               |Memphis                     |          17|          16|        1|
|   2008|    3|Maryland               |California                  |          35|          27|        8|
|   2008|    3|Miami (OH)             |Charleston Southern         |          38|          27|       11|
|   2008|    3|Michigan State         |Florida Atlantic            |          17|           0|       17|
|   2008|    3|Minnesota              |Montana State               |          35|          23|       12|
|   2008|    3|Mississippi            |Samford                     |          34|          10|       24|
|   2008|    3|Missouri               |Nevada                      |          69|          17|       52|
|   2008|    3|Nebraska               |New Mexico State            |          38|           7|       31|
|   2008|    3|Arizona State          |Nevada-Las Vegas            |          20|          23|       -3|
|   2008|    3|New Mexico             |Arizona                     |          36|          28|        8|
|   2008|    3|Northwestern           |Southern Illinois           |          33|           7|       26|
|   2008|    3|Notre Dame             |Michigan                    |          35|          17|       18|
|   2008|    3|Washington             |Oklahoma                    |          14|          55|      -41|
|   2008|    3|Oklahoma State         |Missouri State              |          57|          13|       44|
|   2008|    3|Purdue                 |Oregon                      |          26|          32|       -6|
|   2008|    3|Oregon State           |Hawaii                      |          45|           7|       38|
|   2008|    3|Syracuse               |Penn State                  |          13|          55|      -42|
|   2008|    3|San Jose State         |San Diego State             |          35|          10|       25|
|   2008|    3|Southern California    |Ohio State                  |          35|           3|       32|
|   2008|    3|Arkansas State         |Southern Mississippi        |          24|          27|       -3|
|   2008|    3|Tennessee              |Alabama-Birmingham          |          35|           3|       32|
|   2008|    3|Texas Christian        |Stanford                    |          31|          14|       17|
|   2008|    3|Texas Tech             |Southern Methodist          |          43|           7|       36|
|   2008|    3|Eastern Michigan       |Toledo                      |          17|          41|      -24|
|   2008|    3|Troy                   |Alcorn State                |          65|           0|       65|
|   2008|    3|Utah State             |Utah                        |          10|          58|      -48|
|   2008|    3|Vanderbilt             |Rice                        |          38|          21|       17|
|   2008|    3|Virginia Tech          |Georgia Tech                |          20|          17|        3|
|   2008|    3|Idaho                  |Western Michigan            |          28|          51|      -23|
|   2008|    3|Fresno State           |Wisconsin                   |          10|          13|       -3|
|   2008|    3|Wyoming                |North Dakota State          |          16|          13|        3|
|   2008|    4|Louisville             |Kansas State                |          38|          29|        9|
|   2008|    4|Colorado               |West Virginia               |          17|          14|        3|
|   2008|    4|Connecticut            |Baylor                      |          31|          28|        3|
|   2008|    4|Army                   |Akron                       |           3|          22|      -19|
|   2008|    4|Arkansas               |Alabama                     |          14|          49|      -35|
|   2008|    4|Alabama-Birmingham     |Alabama State               |          45|          10|       35|
|   2008|    4|UCLA                   |Arizona                     |          10|          31|      -21|
|   2008|    4|Arkansas State         |Middle Tennessee State      |          31|          14|       17|
|   2008|    4|Indiana                |Ball State                  |          20|          42|      -22|
|   2008|    4|Oregon                 |Boise State                 |          32|          37|       -5|
|   2008|    4|Boston College         |Central Florida             |          34|           7|       27|
|   2008|    4|Brigham Young          |Wyoming                     |          44|           0|       44|
|   2008|    4|Cincinnati             |Miami (OH)                  |          45|          20|       25|
|   2008|    4|Clemson                |South Carolina State        |          54|           0|       54|
|   2008|    4|Colorado State         |Houston                     |          28|          25|        3|
|   2008|    4|Tennessee              |Florida                     |           6|          30|      -24|
|   2008|    4|Toledo                 |Fresno State                |          54|          55|       -1|
|   2008|    4|Arizona State          |Georgia                     |          10|          27|      -17|
|   2008|    4|Georgia Tech           |Mississippi State           |          38|           7|       31|
|   2008|    4|Kansas                 |Sam Houston State           |          38|          14|       24|
|   2008|    4|Auburn                 |Louisiana State             |          21|          26|       -5|
|   2008|    4|Louisiana Tech         |Southeastern Louisiana      |          41|          26|       15|
|   2008|    4|Louisiana-Lafayette    |Kent State                  |          44|          27|       17|
|   2008|    4|Southern Mississippi   |Marshall                    |          27|          34|       -7|
|   2008|    4|Maryland               |Eastern Michigan            |          51|          24|       27|
|   2008|    4|Memphis                |Nicholls State              |          31|          10|       21|
|   2008|    4|Texas A&M              |Miami (FL)                  |          23|          41|      -18|
|   2008|    4|Michigan State         |Notre Dame                  |          23|           7|       16|
|   2008|    4|Minnesota              |Florida Atlantic            |          37|           3|       34|
|   2008|    4|Missouri               |Buffalo                     |          42|          21|       21|
|   2008|    4|Navy                   |Rutgers                     |          23|          21|        2|
|   2008|    4|Nevada-Las Vegas       |Iowa State                  |          34|          31|        3|
|   2008|    4|Texas-El Paso          |New Mexico State            |          33|          34|       -1|
|   2008|    4|North Carolina State   |East Carolina               |          30|          24|        6|
|   2008|    4|Northern Illinois      |Indiana State               |          48|           3|       45|
|   2008|    4|Northwestern           |Ohio                        |          16|           8|        8|
|   2008|    4|Ohio State             |Troy                        |          28|          10|       18|
|   2008|    4|Penn State             |Temple                      |          45|           3|       42|
|   2008|    4|Pittsburgh             |Iowa                        |          21|          20|        1|
|   2008|    4|Purdue                 |Central Michigan            |          32|          25|        7|
|   2008|    4|South Carolina         |Wofford                     |          23|          13|       10|
|   2008|    4|Florida International  |South Florida               |           9|          17|       -8|
|   2008|    4|Stanford               |San Jose State              |          23|          10|       13|
|   2008|    4|Syracuse               |Northeastern                |          30|          21|        9|
|   2008|    4|Texas                  |Rice                        |          52|          10|       42|
|   2008|    4|Southern Methodist     |Texas Christian             |           7|          48|      -41|
|   2008|    4|Texas Tech             |Massachusetts               |          56|          14|       42|
|   2008|    4|Tulane                 |Louisiana-Monroe            |          24|          10|       14|
|   2008|    4|Tulsa                  |New Mexico                  |          56|          14|       42|
|   2008|    4|Air Force              |Utah                        |          23|          30|       -7|
|   2008|    4|Utah State             |Idaho                       |          42|          17|       25|
|   2008|    4|Mississippi            |Vanderbilt                  |          17|          23|       -6|
|   2008|    4|North Carolina         |Virginia Tech               |          17|          20|       -3|
|   2008|    4|Florida State          |Wake Forest                 |           3|          12|       -9|
|   2008|    4|Washington State       |Portland State              |          48|           9|       39|
|   2008|    4|Western Kentucky       |Murray State                |          50|           9|       41|
|   2008|    4|Western Michigan       |Tennessee Tech              |          41|           7|       34|
|   2008|    5|Oregon State           |Southern California         |          27|          21|        6|
|   2008|    5|Tulane                 |Southern Methodist          |          34|          27|        7|
|   2008|    5|Louisville             |Connecticut                 |          21|          26|       -5|
|   2008|    5|Georgia                |Alabama                     |          30|          41|      -11|
|   2008|    5|Auburn                 |Tennessee                   |          14|          12|        2|
|   2008|    5|Ball State             |Kent State                  |          41|          20|       21|
|   2008|    5|Boston College         |Rhode Island                |          42|           0|       42|
|   2008|    5|Wyoming                |Bowling Green State         |          16|          45|      -29|
|   2008|    5|California             |Colorado State              |          42|           7|       35|
|   2008|    5|Central Michigan       |Buffalo                     |          27|          25|        2|
|   2008|    5|Akron                  |Cincinnati                  |          15|          17|       -2|
|   2008|    5|Duke                   |Virginia                    |          31|           3|       28|
|   2008|    5|Toledo                 |Florida International       |          16|          35|      -19|
|   2008|    5|Florida State          |Colorado                    |          39|          21|       18|
|   2008|    5|UCLA                   |Fresno State                |          31|          36|       -5|
|   2008|    5|East Carolina          |Houston                     |          24|          41|      -17|
|   2008|    5|Kansas State           |Louisiana-Lafayette         |          45|          37|        8|
|   2008|    5|Kentucky               |Western Kentucky            |          41|           3|       38|
|   2008|    5|Louisiana State        |Mississippi State           |          34|          24|       10|
|   2008|    5|Clemson                |Maryland                    |          17|          20|       -3|
|   2008|    5|Memphis                |Arkansas State              |          29|          17|       12|
|   2008|    5|Michigan               |Wisconsin                   |          27|          25|        2|
|   2008|    5|Indiana                |Michigan State              |          29|          42|      -13|
|   2008|    5|Florida                |Mississippi                 |          30|          31|       -1|
|   2008|    5|Wake Forest            |Navy                        |          17|          24|       -7|
|   2008|    5|Nevada-Las Vegas       |Nevada                      |          27|          49|      -22|
|   2008|    5|New Mexico State       |New Mexico                  |          24|          35|      -11|
|   2008|    5|Miami (FL)             |North Carolina              |          24|          28|       -4|
|   2008|    5|Eastern Michigan       |Northern Illinois           |           0|          37|      -37|
|   2008|    5|Iowa                   |Northwestern                |          17|          22|       -5|
|   2008|    5|Notre Dame             |Purdue                      |          38|          21|       17|
|   2008|    5|Ohio                   |Virginia Military Institute |          51|          31|       20|
|   2008|    5|Ohio State             |Minnesota                   |          34|          21|       13|
|   2008|    5|Oklahoma               |Texas Christian             |          35|          10|       25|
|   2008|    5|Oklahoma State         |Troy                        |          55|          24|       31|
|   2008|    5|Washington State       |Oregon                      |          14|          63|      -49|
|   2008|    5|Penn State             |Illinois                    |          38|          24|       14|
|   2008|    5|Syracuse               |Pittsburgh                  |          24|          34|      -10|
|   2008|    5|Rice                   |North Texas                 |          77|          20|       57|
|   2008|    5|Rutgers                |Morgan State                |          38|           0|       38|
|   2008|    5|San Diego State        |Idaho                       |          45|          17|       28|
|   2008|    5|Hawaii                 |San Jose State              |          17|          20|       -3|
|   2008|    5|South Carolina         |Alabama-Birmingham          |          26|          13|       13|
|   2008|    5|North Carolina State   |South Florida               |          10|          41|      -31|
|   2008|    5|Washington             |Stanford                    |          28|          35|       -7|
|   2008|    5|Texas                  |Arkansas                    |          52|          10|       42|
|   2008|    5|Texas A&M              |Army                        |          21|          17|        4|
|   2008|    5|Texas-El Paso          |Central Florida             |          58|          13|       45|
|   2008|    5|Tulsa                  |Central Arkansas            |          62|          34|       28|
|   2008|    5|Utah                   |Weber State                 |          37|          21|       16|
|   2008|    5|Nebraska               |Virginia Tech               |          30|          35|       -5|
|   2008|    5|West Virginia          |Marshall                    |          27|           3|       24|
|   2008|    5|Temple                 |Western Michigan            |           3|           7|       -4|
|   2008|    6|Middle Tennessee State |Florida Atlantic            |          14|          13|        1|
|   2008|    6|Boise State            |Louisiana Tech              |          38|           3|       35|
|   2008|    6|Alabama-Birmingham     |Memphis                     |          30|          33|       -3|
|   2008|    6|South Florida          |Pittsburgh                  |          21|          26|       -5|
|   2008|    6|Utah                   |Oregon State                |          31|          28|        3|
|   2008|    6|Utah State             |Brigham Young               |          14|          34|      -20|
|   2008|    6|Marshall               |Cincinnati                  |          10|          33|      -23|
|   2008|    6|Kent State             |Akron                       |          27|          30|       -3|
|   2008|    6|Alabama                |Kentucky                    |          17|          14|        3|
|   2008|    6|Arizona                |Washington                  |          48|          14|       34|
|   2008|    6|Tulane                 |Army                        |          13|          44|      -31|
|   2008|    6|Toledo                 |Ball State                  |           0|          31|      -31|
|   2008|    6|North Carolina State   |Boston College              |          31|          38|       -7|
|   2008|    6|California             |Arizona State               |          24|          14|       10|
|   2008|    6|Central Florida        |Southern Methodist          |          31|          17|       14|
|   2008|    6|Colorado State         |Nevada-Las Vegas            |          41|          28|       13|
|   2008|    6|Bowling Green State    |Eastern Michigan            |          21|          24|       -3|
|   2008|    6|Arkansas               |Florida                     |           7|          38|      -31|
|   2008|    6|North Texas            |Florida International       |          10|          42|      -32|
|   2008|    6|Miami (FL)             |Florida State               |          39|          41|       -2|
|   2008|    6|Georgia Tech           |Duke                        |          27|           0|       27|
|   2008|    6|Fresno State           |Hawaii                      |          29|          32|       -3|
|   2008|    6|Michigan               |Illinois                    |          20|          45|      -25|
|   2008|    6|Iowa State             |Kansas                      |          33|          35|       -2|
|   2008|    6|Louisiana-Monroe       |Louisiana-Lafayette         |          35|          44|       -9|
|   2008|    6|Michigan State         |Iowa                        |          16|          13|        3|
|   2008|    6|Minnesota              |Indiana                     |          16|           7|        9|
|   2008|    6|Nebraska               |Missouri                    |          17|          52|      -35|
|   2008|    6|Air Force              |Navy                        |          27|          33|       -6|
|   2008|    6|Idaho                  |Nevada                      |          14|          49|      -35|
|   2008|    6|New Mexico             |Wyoming                     |          24|           0|       24|
|   2008|    6|New Mexico State       |Alcorn State                |          45|          10|       35|
|   2008|    6|North Carolina         |Connecticut                 |          38|          12|       26|
|   2008|    6|Notre Dame             |Stanford                    |          28|          21|        7|
|   2008|    6|Wisconsin              |Ohio State                  |          17|          20|       -3|
|   2008|    6|Baylor                 |Oklahoma                    |          17|          49|      -32|
|   2008|    6|Oklahoma State         |Texas A&M                   |          56|          28|       28|
|   2008|    6|Purdue                 |Penn State                  |           6|          20|      -14|
|   2008|    6|Mississippi            |South Carolina              |          24|          31|       -7|
|   2008|    6|Southern California    |Oregon                      |          44|          10|       34|
|   2008|    6|Miami (OH)             |Temple                      |          10|          28|      -18|
|   2008|    6|Tennessee              |Northern Illinois           |          13|           9|        4|
|   2008|    6|Colorado               |Texas                       |          14|          38|      -24|
|   2008|    6|Texas Christian        |San Diego State             |          41|           7|       34|
|   2008|    6|Kansas State           |Texas Tech                  |          28|          58|      -30|
|   2008|    6|Southern Mississippi   |Texas-El Paso               |          37|          40|       -3|
|   2008|    6|Tulsa                  |Rice                        |          63|          28|       35|
|   2008|    6|UCLA                   |Washington State            |          28|           3|       25|
|   2008|    6|Vanderbilt             |Auburn                      |          14|          13|        1|
|   2008|    6|Virginia               |Maryland                    |          31|           0|       31|
|   2008|    6|Virginia Tech          |Western Kentucky            |          27|          13|       14|
|   2008|    6|West Virginia          |Rutgers                     |          24|          17|        7|
|   2008|    6|Western Michigan       |Ohio                        |          41|          20|       21|
|   2008|    7|Florida Atlantic       |Troy                        |          17|          30|      -13|
|   2008|    7|Houston                |Alabama-Birmingham          |          45|          20|       25|
|   2008|    7|Wake Forest            |Clemson                     |          12|           7|        5|
|   2008|    7|Memphis                |Louisville                  |          28|          35|       -7|
|   2008|    7|San Diego State        |Air Force                   |          10|          35|      -25|
|   2008|    7|Auburn                 |Arkansas                    |          22|          25|       -3|
|   2008|    7|Arkansas State         |Louisiana-Monroe            |          37|          29|        8|
|   2008|    7|Army                   |Eastern Michigan            |          17|          13|        4|
|   2008|    7|Western Kentucky       |Ball State                  |           7|          24|      -17|
|   2008|    7|Baylor                 |Iowa State                  |          38|          10|       28|
|   2008|    7|Southern Mississippi   |Boise State                 |           7|          24|      -17|
|   2008|    7|Akron                  |Bowling Green State         |          33|          37|       -4|
|   2008|    7|Brigham Young          |New Mexico                  |          21|           3|       18|
|   2008|    7|Central Michigan       |Temple                      |          24|          14|       10|
|   2008|    7|Cincinnati             |Rutgers                     |          13|          10|        3|
|   2008|    7|Florida                |Louisiana State             |          51|          21|       30|
|   2008|    7|Florida International  |Middle Tennessee State      |          31|          21|       10|
|   2008|    7|Fresno State           |Idaho                       |          45|          32|       13|
|   2008|    7|Georgia                |Tennessee                   |          26|          14|       12|
|   2008|    7|Georgia Tech           |Gardner-Webb                |          10|           7|        3|
|   2008|    7|Hawaii                 |Louisiana Tech              |          24|          14|       10|
|   2008|    7|Indiana                |Iowa                        |           9|          45|      -36|
|   2008|    7|Kansas                 |Colorado                    |          30|          14|       16|
|   2008|    7|Texas A&M              |Kansas State                |          30|          44|      -14|
|   2008|    7|North Texas            |Louisiana-Lafayette         |          30|          59|      -29|
|   2008|    7|Miami (FL)             |Central Florida             |          20|          14|        6|
|   2008|    7|Northwestern           |Michigan State              |          20|          37|      -17|
|   2008|    7|Illinois               |Minnesota                   |          20|          27|       -7|
|   2008|    7|Mississippi State      |Vanderbilt                  |          17|          14|        3|
|   2008|    7|Nevada                 |New Mexico State            |          45|          48|       -3|
|   2008|    7|North Carolina         |Notre Dame                  |          29|          24|        5|
|   2008|    7|Northern Illinois      |Miami (OH)                  |          17|          13|        4|
|   2008|    7|Kent State             |Ohio                        |          19|          26|       -7|
|   2008|    7|Ohio State             |Purdue                      |          16|           3|       13|
|   2008|    7|Missouri               |Oklahoma State              |          23|          28|       -5|
|   2008|    7|Oregon                 |UCLA                        |          31|          24|        7|
|   2008|    7|Oregon State           |Washington State            |          66|          13|       53|
|   2008|    7|Wisconsin              |Penn State                  |           7|          48|      -41|
|   2008|    7|San Jose State         |Utah State                  |          30|           7|       23|
|   2008|    7|Kentucky               |South Carolina              |          17|          24|       -7|
|   2008|    7|Southern California    |Arizona State               |          28|           0|       28|
|   2008|    7|Stanford               |Arizona                     |          24|          23|        1|
|   2008|    7|Texas                  |Oklahoma                    |          45|          35|       10|
|   2008|    7|Colorado State         |Texas Christian             |           7|          13|       -6|
|   2008|    7|Texas Tech             |Nebraska                    |          37|          31|        6|
|   2008|    7|Texas-El Paso          |Tulane                      |          24|          21|        3|
|   2008|    7|Michigan               |Toledo                      |          10|          13|       -3|
|   2008|    7|Southern Methodist     |Tulsa                       |          31|          37|       -6|
|   2008|    7|Wyoming                |Utah                        |           7|          40|      -33|
|   2008|    7|Virginia               |East Carolina               |          35|          20|       15|
|   2008|    7|West Virginia          |Syracuse                    |          17|           6|       11|
|   2008|    7|Buffalo                |Western Michigan            |          28|          34|       -6|
|   2008|    8|North Carolina State   |Florida State               |          17|          26|       -9|
|   2008|    8|Texas Christian        |Brigham Young               |          32|           7|       25|
|   2008|    8|Boise State            |Hawaii                      |          27|           7|       20|
|   2008|    8|Nevada-Las Vegas       |Air Force                   |          28|          29|       -1|
|   2008|    8|Eastern Michigan       |Akron                       |          35|          42|       -7|
|   2008|    8|Alabama                |Mississippi                 |          24|          20|        4|
|   2008|    8|Alabama-Birmingham     |Marshall                    |          23|          21|        2|
|   2008|    8|Arizona                |California                  |          42|          27|       15|
|   2008|    8|Boston College         |Virginia Tech               |          28|          23|        5|
|   2008|    8|Buffalo                |Army                        |          27|          24|        3|
|   2008|    8|Central Michigan       |Western Michigan            |          38|          28|       10|
|   2008|    8|Colorado               |Kansas State                |          14|          13|        1|
|   2008|    8|East Carolina          |Memphis                     |          30|          10|       20|
|   2008|    8|Western Kentucky       |Florida Atlantic            |          20|          24|       -4|
|   2008|    8|Georgia                |Vanderbilt                  |          24|          14|       10|
|   2008|    8|Clemson                |Georgia Tech                |          17|          21|       -4|
|   2008|    8|Southern Methodist     |Houston                     |          38|          44|       -6|
|   2008|    8|Illinois               |Indiana                     |          55|          13|       42|
|   2008|    8|Iowa                   |Wisconsin                   |          38|          16|       22|
|   2008|    8|Kentucky               |Arkansas                    |          21|          20|        1|
|   2008|    8|South Carolina         |Louisiana State             |          17|          24|       -7|
|   2008|    8|Louisiana Tech         |Idaho                       |          46|          14|       32|
|   2008|    8|Louisiana-Lafayette    |Arkansas State              |          28|          23|        5|
|   2008|    8|Louisiana-Monroe       |North Texas                 |          35|          23|       12|
|   2008|    8|Louisville             |Middle Tennessee State      |          42|          23|       19|
|   2008|    8|Maryland               |Wake Forest                 |          26|           0|       26|
|   2008|    8|Duke                   |Miami (FL)                  |          31|          49|      -18|
|   2008|    8|Bowling Green State    |Miami (OH)                  |          20|          27|       -7|
|   2008|    8|Iowa State             |Nebraska                    |           7|          35|      -28|
|   2008|    8|Nevada                 |Utah State                  |          44|          17|       27|
|   2008|    8|New Mexico             |San Diego State             |          70|           7|       63|
|   2008|    8|Northern Illinois      |Toledo                      |          38|           7|       31|
|   2008|    8|Northwestern           |Purdue                      |          48|          26|       22|
|   2008|    8|Michigan State         |Ohio State                  |           7|          45|      -38|
|   2008|    8|Oklahoma               |Kansas                      |          45|          31|       14|
|   2008|    8|Oklahoma State         |Baylor                      |          34|           6|       28|
|   2008|    8|Washington             |Oregon State                |          13|          34|      -21|
|   2008|    8|Penn State             |Michigan                    |          46|          17|       29|
|   2008|    8|Navy                   |Pittsburgh                  |          21|          42|      -21|
|   2008|    8|Rice                   |Southern Mississippi        |          45|          40|        5|
|   2008|    8|Rutgers                |Connecticut                 |          12|          10|        2|
|   2008|    8|New Mexico State       |San Jose State              |          14|          31|      -17|
|   2008|    8|South Florida          |Syracuse                    |          45|          13|       32|
|   2008|    8|Washington State       |Southern California         |           0|          69|      -69|
|   2008|    8|Tennessee              |Mississippi State           |          34|           3|       31|
|   2008|    8|Texas                  |Missouri                    |          56|          31|       25|
|   2008|    8|Texas A&M              |Texas Tech                  |          25|          43|      -18|
|   2008|    8|Troy                   |Florida International       |          33|          23|       10|
|   2008|    8|Tulsa                  |Texas-El Paso               |          77|          35|       42|
|   2008|    8|UCLA                   |Stanford                    |          23|          20|        3|
|   2008|    8|Utah                   |Colorado State              |          49|          16|       33|
|   2008|    8|Virginia               |North Carolina              |          16|          13|        3|
|   2008|    9|Temple                 |Ohio                        |          14|          10|        4|
|   2008|    9|Air Force              |New Mexico                  |          23|          10|       13|
|   2008|    9|West Virginia          |Auburn                      |          34|          17|       17|
|   2008|    9|San Jose State         |Boise State                 |          16|          33|      -17|
|   2008|    9|Tennessee              |Alabama                     |           9|          29|      -20|
|   2008|    9|Army                   |Louisiana Tech              |          14|           7|        7|
|   2008|    9|Ball State             |Eastern Michigan            |          38|          16|       22|
|   2008|    9|Brigham Young          |Nevada-Las Vegas            |          42|          35|        7|
|   2008|    9|California             |UCLA                        |          41|          20|       21|
|   2008|    9|Toledo                 |Central Michigan            |          23|          24|       -1|
|   2008|    9|San Diego State        |Colorado State              |          34|          38|       -4|
|   2008|    9|Connecticut            |Cincinnati                  |          40|          16|       24|
|   2008|    9|Vanderbilt             |Duke                        |           7|          10|       -3|
|   2008|    9|Florida                |Kentucky                    |          63|           5|       58|
|   2008|    9|Louisiana-Monroe       |Florida Atlantic            |          28|          29|       -1|
|   2008|    9|Florida State          |Virginia Tech               |          30|          20|       10|
|   2008|    9|Utah State             |Fresno State                |          28|          30|       -2|
|   2008|    9|Louisiana State        |Georgia                     |          38|          52|      -14|
|   2008|    9|Hawaii                 |Nevada                      |          38|          31|        7|
|   2008|    9|Idaho                  |New Mexico State            |          20|          14|        6|
|   2008|    9|Indiana                |Northwestern                |          21|          19|        2|
|   2008|    9|Miami (OH)             |Kent State                  |          21|          54|      -33|
|   2008|    9|Louisville             |South Florida               |          24|          20|        4|
|   2008|    9|Maryland               |North Carolina State        |          27|          24|        3|
|   2008|    9|Memphis                |Southern Mississippi        |          36|          30|        6|
|   2008|    9|Miami (FL)             |Wake Forest                 |          16|          10|        6|
|   2008|    9|Michigan               |Michigan State              |          21|          35|      -14|
|   2008|    9|Purdue                 |Minnesota                   |           6|          17|      -11|
|   2008|    9|Arkansas               |Mississippi                 |          21|          23|       -2|
|   2008|    9|Mississippi State      |Middle Tennessee State      |          31|          22|        9|
|   2008|    9|Missouri               |Colorado                    |          58|           0|       58|
|   2008|    9|Navy                   |Southern Methodist          |          34|           7|       27|
|   2008|    9|Nebraska               |Baylor                      |          32|          20|       12|
|   2008|    9|North Carolina         |Boston College              |          45|          24|       21|
|   2008|    9|Northern Illinois      |Bowling Green State         |          16|          13|        3|
|   2008|    9|Washington             |Notre Dame                  |           7|          33|      -26|
|   2008|    9|Kansas State           |Oklahoma                    |          35|          58|      -23|
|   2008|    9|Arizona State          |Oregon                      |          20|          54|      -34|
|   2008|    9|Ohio State             |Penn State                  |           6|          13|       -7|
|   2008|    9|Tulane                 |Rice                        |          17|          42|      -25|
|   2008|    9|Pittsburgh             |Rutgers                     |          34|          54|      -20|
|   2008|    9|Arizona                |Southern California         |          10|          17|       -7|
|   2008|    9|Texas                  |Oklahoma State              |          28|          24|        4|
|   2008|    9|Iowa State             |Texas A&M                   |          35|          49|      -14|
|   2008|    9|Texas Christian        |Wyoming                     |          54|           7|       47|
|   2008|    9|Kansas                 |Texas Tech                  |          21|          63|      -42|
|   2008|    9|North Texas            |Troy                        |          17|          45|      -28|
|   2008|    9|Georgia Tech           |Virginia                    |          17|          24|       -7|
|   2008|    9|Wisconsin              |Illinois                    |          27|          17|       10|
|   2008|    9|Tulsa                  |Central Florida             |          49|          19|       30|
|   2008|   10|Ohio                   |Buffalo                     |          19|          32|      -13|
|   2008|   10|Marshall               |Houston                     |          37|          23|       14|
|   2008|   10|Cincinnati             |South Florida               |          24|          10|       14|
|   2008|   10|Army                   |Air Force                   |           7|          16|       -9|
|   2008|   10|Alabama                |Arkansas State              |          35|           0|       35|
|   2008|   10|Arkansas               |Tulsa                       |          30|          23|        7|
|   2008|   10|New Mexico State       |Boise State                 |           0|          49|      -49|
|   2008|   10|Bowling Green State    |Kent State                  |          45|          30|       15|
|   2008|   10|Colorado State         |Brigham Young               |          42|          45|       -3|
|   2008|   10|California             |Oregon                      |          26|          16|       10|
|   2008|   10|Indiana                |Central Michigan            |          34|          37|       -3|
|   2008|   10|Boston College         |Clemson                     |          21|          27|       -6|
|   2008|   10|Florida                |Georgia                     |          49|          10|       39|
|   2008|   10|Georgia Tech           |Florida State               |          31|          28|        3|
|   2008|   10|Illinois               |Iowa                        |          27|          24|        3|
|   2008|   10|Kansas                 |Kansas State                |          52|          21|       31|
|   2008|   10|Mississippi State      |Kentucky                    |          13|          14|       -1|
|   2008|   10|Louisiana State        |Tulane                      |          35|          10|       25|
|   2008|   10|Louisiana Tech         |Fresno State                |          38|          35|        3|
|   2008|   10|Louisiana-Lafayette    |Florida International       |          49|          20|       29|
|   2008|   10|Louisiana-Monroe       |Troy                        |          31|          30|        1|
|   2008|   10|Virginia               |Miami (FL)                  |          17|          24|       -7|
|   2008|   10|Michigan State         |Wisconsin                   |          25|          24|        1|
|   2008|   10|Mississippi            |Auburn                      |          17|           7|       10|
|   2008|   10|Baylor                 |Missouri                    |          28|          31|       -3|
|   2008|   10|Navy                   |Temple                      |          33|          27|        6|
|   2008|   10|Western Kentucky       |North Texas                 |          40|          51|      -11|
|   2008|   10|Minnesota              |Northwestern                |          17|          24|       -7|
|   2008|   10|Oklahoma               |Nebraska                    |          62|          28|       34|
|   2008|   10|Oklahoma State         |Iowa State                  |          59|          17|       42|
|   2008|   10|Oregon State           |Arizona State               |          27|          25|        2|
|   2008|   10|Notre Dame             |Pittsburgh                  |          33|          36|       -3|
|   2008|   10|Purdue                 |Michigan                    |          48|          42|        6|
|   2008|   10|Texas-El Paso          |Rice                        |          44|          49|       -5|
|   2008|   10|Idaho                  |San Jose State              |          24|          30|       -6|
|   2008|   10|South Carolina         |Tennessee                   |          27|           6|       21|
|   2008|   10|Southern California    |Washington                  |          56|           0|       56|
|   2008|   10|Southern Mississippi   |Alabama-Birmingham          |          70|          14|       56|
|   2008|   10|Stanford               |Washington State            |          58|           0|       58|
|   2008|   10|Syracuse               |Louisville                  |          28|          21|        7|
|   2008|   10|Texas A&M              |Colorado                    |          24|          17|        7|
|   2008|   10|Nevada-Las Vegas       |Texas Christian             |          14|          44|      -30|
|   2008|   10|Texas Tech             |Texas                       |          39|          33|        6|
|   2008|   10|New Mexico             |Utah                        |          10|          13|       -3|
|   2008|   10|Utah State             |Hawaii                      |          30|          14|       16|
|   2008|   10|Wake Forest            |Duke                        |          33|          30|        3|
|   2008|   10|Connecticut            |West Virginia               |          13|          35|      -22|
|   2008|   10|Western Michigan       |Eastern Michigan            |          31|          10|       21|
|   2008|   10|Wyoming                |San Diego State             |          35|          10|       25|
|   2008|   10|Central Florida        |East Carolina               |          10|          13|       -3|
|   2008|   11|Buffalo                |Miami (OH)                  |          37|          17|       20|
|   2008|   11|Akron                  |Toledo                      |          47|          30|       17|
|   2008|   11|Ball State             |Northern Illinois           |          45|          14|       31|
|   2008|   11|Utah                   |Texas Christian             |          13|          10|        3|
|   2008|   11|Virginia Tech          |Maryland                    |          23|          13|       10|
|   2008|   11|Fresno State           |Nevada                      |          28|          41|      -13|
|   2008|   11|Air Force              |Colorado State              |          38|          17|       21|
|   2008|   11|Louisiana State        |Alabama                     |          21|          27|       -6|
|   2008|   11|Washington State       |Arizona                     |          28|          59|      -31|
|   2008|   11|Washington             |Arizona State               |          19|          39|      -20|
|   2008|   11|Auburn                 |Tennessee-Martin            |          37|          20|       17|
|   2008|   11|Boise State            |Utah State                  |          49|          14|       35|
|   2008|   11|Boston College         |Notre Dame                  |          17|           0|       17|
|   2008|   11|Ohio                   |Bowling Green State         |           3|          28|      -25|
|   2008|   11|Brigham Young          |San Diego State             |          41|          12|       29|
|   2008|   11|West Virginia          |Cincinnati                  |          23|          26|       -3|
|   2008|   11|Colorado               |Iowa State                  |          28|          24|        4|
|   2008|   11|East Carolina          |Marshall                    |          19|          16|        3|
|   2008|   11|Vanderbilt             |Florida                     |          14|          42|      -28|
|   2008|   11|Florida Atlantic       |North Texas                 |          46|          13|       33|
|   2008|   11|Florida International  |Arkansas State              |          22|          21|        1|
|   2008|   11|Florida State          |Clemson                     |          41|          27|       14|
|   2008|   11|Kentucky               |Georgia                     |          38|          42|       -4|
|   2008|   11|New Mexico State       |Hawaii                      |          30|          42|      -12|
|   2008|   11|Houston                |Tulane                      |          42|          14|       28|
|   2008|   11|Iowa                   |Penn State                  |          24|          23|        1|
|   2008|   11|San Jose State         |Louisiana Tech              |           0|          21|      -21|
|   2008|   11|Southern Methodist     |Memphis                     |          26|          31|       -5|
|   2008|   11|Minnesota              |Michigan                    |           6|          29|      -23|
|   2008|   11|Michigan State         |Purdue                      |          21|           7|       14|
|   2008|   11|Middle Tennessee State |Louisiana-Monroe            |          24|          21|        3|
|   2008|   11|Missouri               |Kansas State                |          41|          24|       17|
|   2008|   11|Nebraska               |Kansas                      |          45|          35|       10|
|   2008|   11|Nevada-Las Vegas       |New Mexico                  |          27|          20|        7|
|   2008|   11|North Carolina         |Georgia Tech                |          28|           7|       21|
|   2008|   11|Duke                   |North Carolina State        |          17|          27|      -10|
|   2008|   11|Northwestern           |Ohio State                  |          10|          45|      -35|
|   2008|   11|Texas A&M              |Oklahoma                    |          28|          66|      -38|
|   2008|   11|Oregon                 |Stanford                    |          35|          28|        7|
|   2008|   11|UCLA                   |Oregon State                |           6|          34|      -28|
|   2008|   11|Pittsburgh             |Louisville                  |          41|           7|       34|
|   2008|   11|Rice                   |Army                        |          38|          31|        7|
|   2008|   11|Rutgers                |Syracuse                    |          35|          17|       18|
|   2008|   11|South Carolina         |Arkansas                    |          34|          21|       13|
|   2008|   11|Southern California    |California                  |          17|           3|       14|
|   2008|   11|Central Florida        |Southern Mississippi        |           6|          17|      -11|
|   2008|   11|Texas                  |Baylor                      |          45|          21|       24|
|   2008|   11|Texas Tech             |Oklahoma State              |          56|          20|       36|
|   2008|   11|Louisiana-Lafayette    |Texas-El Paso               |          24|          37|      -13|
|   2008|   11|Troy                   |Western Kentucky            |          17|           7|       10|
|   2008|   11|Wake Forest            |Virginia                    |          28|          17|       11|
|   2008|   11|Western Michigan       |Illinois                    |          23|          17|        6|
|   2008|   11|Indiana                |Wisconsin                   |          20|          55|      -35|
|   2008|   11|Tennessee              |Wyoming                     |           7|          13|       -6|
|   2008|   12|Miami (OH)             |Ball State                  |          16|          31|      -15|
|   2008|   12|Northern Illinois      |Central Michigan            |          30|          33|       -3|
|   2008|   12|Kent State             |Temple                      |          41|          38|        3|
|   2008|   12|Akron                  |Buffalo                     |          40|          43|       -3|
|   2008|   12|Miami (FL)             |Virginia Tech               |          16|          14|        2|
|   2008|   12|Nevada-Las Vegas       |Wyoming                     |          22|          14|        8|
|   2008|   12|Louisville             |Cincinnati                  |          20|          28|       -8|
|   2008|   12|Alabama                |Mississippi State           |          32|           7|       25|
|   2008|   12|Tulane                 |Alabama-Birmingham          |          24|          41|      -17|
|   2008|   12|Arizona State          |Washington State            |          31|           0|       31|
|   2008|   12|Baylor                 |Texas A&M                   |          41|          21|       20|
|   2008|   12|Idaho                  |Boise State                 |          10|          45|      -35|
|   2008|   12|Florida State          |Boston College              |          17|          27|      -10|
|   2008|   12|Air Force              |Brigham Young               |          24|          38|      -14|
|   2008|   12|Marshall               |Central Florida             |          14|          30|      -16|
|   2008|   12|Clemson                |Duke                        |          31|           7|       24|
|   2008|   12|Colorado State         |New Mexico                  |          20|           6|       14|
|   2008|   12|Syracuse               |Connecticut                 |          14|          39|      -25|
|   2008|   12|Florida                |South Carolina              |          56|           6|       50|
|   2008|   12|Florida Atlantic       |Louisiana-Lafayette         |          40|          29|       11|
|   2008|   12|Fresno State           |New Mexico State            |          24|          17|        7|
|   2008|   12|Auburn                 |Georgia                     |          13|          17|       -4|
|   2008|   12|Houston                |Tulsa                       |          70|          30|       40|
|   2008|   12|Iowa                   |Purdue                      |          22|          17|        5|
|   2008|   12|Louisiana State        |Troy                        |          40|          31|        9|
|   2008|   12|Louisiana Tech         |Utah State                  |          45|          38|        7|
|   2008|   12|Maryland               |North Carolina              |          17|          15|        2|
|   2008|   12|Western Kentucky       |Middle Tennessee State      |          10|          21|      -11|
|   2008|   12|Mississippi            |Louisiana-Monroe            |          59|           0|       59|
|   2008|   12|Iowa State             |Missouri                    |          20|          52|      -32|
|   2008|   12|Kansas State           |Nebraska                    |          28|          56|      -28|
|   2008|   12|Nevada                 |San Jose State              |          41|          17|       24|
|   2008|   12|North Carolina State   |Wake Forest                 |          21|          17|        4|
|   2008|   12|Michigan               |Northwestern                |          14|          21|       -7|
|   2008|   12|Navy                   |Notre Dame                  |          21|          27|       -6|
|   2008|   12|Illinois               |Ohio State                  |          20|          30|      -10|
|   2008|   12|Colorado               |Oklahoma State              |          17|          30|      -13|
|   2008|   12|Oregon                 |Arizona                     |          55|          45|       10|
|   2008|   12|Oregon State           |California                  |          34|          21|       13|
|   2008|   12|Penn State             |Indiana                     |          34|           7|       27|
|   2008|   12|South Florida          |Rutgers                     |          16|          49|      -33|
|   2008|   12|Stanford               |Southern California         |          23|          45|      -22|
|   2008|   12|Southern Mississippi   |East Carolina               |          21|           3|       18|
|   2008|   12|Kansas                 |Texas                       |           7|          35|      -28|
|   2008|   12|Texas-El Paso          |Southern Methodist          |          36|          10|       26|
|   2008|   12|Washington             |UCLA                        |           7|          27|      -20|
|   2008|   12|San Diego State        |Utah                        |          14|          63|      -49|
|   2008|   12|Kentucky               |Vanderbilt                  |          24|          31|       -7|
|   2008|   12|Western Michigan       |Toledo                      |          27|          17|       10|
|   2008|   12|Wisconsin              |Minnesota                   |          35|          32|        3|
|   2008|   13|Kent State             |Northern Illinois           |          14|          42|      -28|
|   2008|   13|Central Michigan       |Ball State                  |          24|          31|       -7|
|   2008|   13|Georgia Tech           |Miami (FL)                  |          41|          23|       18|
|   2008|   13|Bowling Green State    |Buffalo                     |          34|          40|       -6|
|   2008|   13|San Jose State         |Fresno State                |          10|          24|      -14|
|   2008|   13|Toledo                 |Miami (OH)                  |          42|          14|       28|
|   2008|   13|Arkansas State         |Florida Atlantic            |          28|          14|       14|
|   2008|   13|Nevada                 |Boise State                 |          34|          41|       -7|
|   2008|   13|Wake Forest            |Boston College              |          21|          24|       -3|
|   2008|   13|California             |Stanford                    |          37|          16|       21|
|   2008|   13|Memphis                |Central Florida             |          21|          28|       -7|
|   2008|   13|Cincinnati             |Pittsburgh                  |          28|          21|        7|
|   2008|   13|Virginia               |Clemson                     |           3|          13|      -10|
|   2008|   13|Wyoming                |Colorado State              |          20|          31|      -11|
|   2008|   13|Alabama-Birmingham     |East Carolina               |          13|          17|       -4|
|   2008|   13|Florida                |Citadel                     |          70|          19|       51|
|   2008|   13|Maryland               |Florida State               |           3|          37|      -34|
|   2008|   13|Hawaii                 |Idaho                       |          49|          17|       32|
|   2008|   13|Houston                |Texas-El Paso               |          42|          37|        5|
|   2008|   13|Minnesota              |Iowa                        |           0|          55|      -55|
|   2008|   13|Kansas State           |Iowa State                  |          38|          30|        8|
|   2008|   13|New Mexico State       |Louisiana Tech              |          31|          35|       -4|
|   2008|   13|Florida International  |Louisiana-Monroe            |          27|          31|       -4|
|   2008|   13|Middle Tennessee State |North Texas                 |          52|          13|       39|
|   2008|   13|Louisiana State        |Mississippi                 |          13|          31|      -18|
|   2008|   13|Mississippi State      |Arkansas                    |          31|          28|        3|
|   2008|   13|North Carolina         |North Carolina State        |          10|          41|      -31|
|   2008|   13|Northwestern           |Illinois                    |          27|          10|       17|
|   2008|   13|Ohio                   |Akron                       |          49|          42|        7|
|   2008|   13|Ohio State             |Michigan                    |          42|           7|       35|
|   2008|   13|Oklahoma               |Texas Tech                  |          65|          21|       44|
|   2008|   13|Arizona                |Oregon State                |          17|          19|       -2|
|   2008|   13|Penn State             |Michigan State              |          49|          18|       31|
|   2008|   13|Purdue                 |Indiana                     |          62|          10|       52|
|   2008|   13|Rice                   |Marshall                    |          35|          10|       25|
|   2008|   13|Rutgers                |Army                        |          30|           3|       27|
|   2008|   13|San Diego State        |Nevada-Las Vegas            |          42|          21|       21|
|   2008|   13|Notre Dame             |Syracuse                    |          23|          24|       -1|
|   2008|   13|Temple                 |Eastern Michigan            |          55|          52|        3|
|   2008|   13|Vanderbilt             |Tennessee                   |          10|          20|      -10|
|   2008|   13|Texas Christian        |Air Force                   |          44|          10|       34|
|   2008|   13|Troy                   |Louisiana-Lafayette         |          48|           3|       45|
|   2008|   13|Tulsa                  |Tulane                      |          56|           7|       49|
|   2008|   13|Utah                   |Brigham Young               |          48|          24|       24|
|   2008|   13|Virginia Tech          |Duke                        |          14|           3|       11|
|   2008|   13|Washington State       |Washington                  |          16|          13|        3|
|   2008|   13|Louisville             |West Virginia               |          21|          35|      -14|
|   2008|   13|Wisconsin              |Cal Poly                    |          36|          35|        1|
|   2008|   13|South Florida          |Connecticut                 |          17|          13|        4|
|   2008|   14|Ball State             |Western Michigan            |          45|          22|       23|
|   2008|   14|Northern Illinois      |Navy                        |           0|          16|      -16|
|   2008|   14|Texas                  |Texas A&M                   |          49|           9|       40|
|   2008|   14|Arizona State          |UCLA                        |          34|           9|       25|
|   2008|   14|Arkansas               |Louisiana State             |          31|          30|        1|
|   2008|   14|Boise State            |Fresno State                |          61|          10|       51|
|   2008|   14|Toledo                 |Bowling Green State         |          10|          38|      -28|
|   2008|   14|East Carolina          |Texas-El Paso               |          53|          21|       32|
|   2008|   14|Eastern Michigan       |Central Michigan            |          56|          52|        4|
|   2008|   14|Buffalo                |Kent State                  |          21|          24|       -3|
|   2008|   14|Mississippi            |Mississippi State           |          45|           0|       45|
|   2008|   14|Nebraska               |Colorado                    |          40|          31|        9|
|   2008|   14|Miami (OH)             |Ohio                        |          26|          41|      -15|
|   2008|   14|Pittsburgh             |West Virginia               |          19|          15|        4|
|   2008|   14|Temple                 |Akron                       |          27|           6|       21|
|   2008|   14|Alabama                |Auburn                      |          36|           0|       36|
|   2008|   14|Central Florida        |Alabama-Birmingham          |           0|          15|      -15|
|   2008|   14|North Texas            |Arkansas State              |          28|          33|       -5|
|   2008|   14|Boston College         |Maryland                    |          28|          21|        7|
|   2008|   14|Cincinnati             |Syracuse                    |          30|          10|       20|
|   2008|   14|Clemson                |South Carolina              |          31|          14|       17|
|   2008|   14|Florida State          |Florida                     |          15|          45|      -30|
|   2008|   14|Florida Atlantic       |Florida International       |          57|          50|        7|
|   2008|   14|Georgia                |Georgia Tech                |          42|          45|       -3|
|   2008|   14|Hawaii                 |Washington State            |          24|          10|       14|
|   2008|   14|Kansas                 |Missouri                    |          40|          37|        3|
|   2008|   14|Memphis                |Tulane                      |          45|           6|       39|
|   2008|   14|Louisiana Tech         |Nevada                      |          31|          35|       -4|
|   2008|   14|Duke                   |North Carolina              |          20|          28|       -8|
|   2008|   14|North Carolina State   |Miami (FL)                  |          38|          28|       10|
|   2008|   14|Oklahoma State         |Oklahoma                    |          41|          61|      -20|
|   2008|   14|Oregon State           |Oregon                      |          38|          65|      -27|
|   2008|   14|Rice                   |Houston                     |          56|          42|       14|
|   2008|   14|Southern California    |Notre Dame                  |          38|           3|       35|
|   2008|   14|Southern Methodist     |Southern Mississippi        |          12|          28|      -16|
|   2008|   14|Tennessee              |Kentucky                    |          28|          10|       18|
|   2008|   14|Texas Tech             |Baylor                      |          35|          28|        7|
|   2008|   14|Marshall               |Tulsa                       |          35|          38|       -3|
|   2008|   14|Utah State             |New Mexico State            |          47|           2|       45|
|   2008|   14|Virginia Tech          |Virginia                    |          17|          14|        3|
|   2008|   14|Wake Forest            |Vanderbilt                  |          23|          10|       13|
|   2008|   15|Louisiana-Lafayette    |Middle Tennessee State      |          42|          28|       14|
|   2008|   15|Rutgers                |Louisville                  |          63|          14|       49|
|   2008|   15|Buffalo                |Ball State                  |          42|          24|       18|
|   2008|   15|Arizona                |Arizona State               |          31|          10|       21|
|   2008|   15|California             |Washington                  |          48|           7|       41|
|   2008|   15|Hawaii                 |Cincinnati                  |          24|          29|       -5|
|   2008|   15|Tulsa                  |East Carolina               |          24|          27|       -3|
|   2008|   15|Florida                |Alabama                     |          31|          20|       11|
|   2008|   15|Florida International  |Western Kentucky            |          27|           3|       24|
|   2008|   15|Navy                   |Army                        |          34|           0|       34|
|   2008|   15|Oklahoma               |Missouri                    |          62|          21|       41|
|   2008|   15|Connecticut            |Pittsburgh                  |          10|          34|      -24|
|   2008|   15|UCLA                   |Southern California         |           7|          28|      -21|
|   2008|   15|Troy                   |Arkansas State              |          35|           9|       26|
|   2008|   15|Virginia Tech          |Boston College              |          30|          12|       18|
|   2008|   15|West Virginia          |South Florida               |          13|           7|        6|
|   2008|   16|Arizona                |Brigham Young               |          31|          21|       10|
|   2008|   16|Colorado State         |Fresno State                |          40|          35|        5|
|   2008|   16|South Florida          |Memphis                     |          41|          14|       27|
|   2008|   16|Wake Forest            |Navy                        |          29|          19|       10|
|   2008|   16|Southern Mississippi   |Troy                        |          30|          27|        3|
|   2008|   17|Texas Christian        |Boise State                 |          17|          16|        1|
|   2008|   17|Hawaii                 |Notre Dame                  |          21|          49|      -28|
|   2008|   17|Florida Atlantic       |Central Michigan            |          24|          21|        3|
|   2008|   17|California             |Miami (FL)                  |          24|          17|        7|
|   2008|   17|Florida State          |Wisconsin                   |          42|          13|       29|
|   2008|   17|West Virginia          |North Carolina              |          31|          30|        1|
|   2008|   17|Louisiana Tech         |Northern Illinois           |          17|          10|        7|
|   2008|   18|Missouri               |Northwestern                |          30|          23|        7|
|   2008|   18|Rutgers                |North Carolina State        |          29|          23|        6|
|   2008|   19|Maryland               |Nevada                      |          42|          35|        7|
|   2008|   19|Oregon                 |Oklahoma State              |          42|          31|       11|
|   2008|   19|Rice                   |Western Michigan            |          38|          14|       24|
|   2008|   19|Houston                |Air Force                   |          34|          28|        6|
|   2008|   19|Kansas                 |Minnesota                   |          42|          21|       21|
|   2008|   19|Georgia Tech           |Louisiana State             |           3|          38|      -35|
|   2008|   19|Oregon State           |Pittsburgh                  |           3|           0|        3|
|   2008|   19|Vanderbilt             |Boston College              |          16|          14|        2|
|   2008|   19|Georgia                |Michigan State              |          24|          12|       12|
|   2008|   19|Iowa                   |South Carolina              |          31|          10|       21|
|   2008|   19|Nebraska               |Clemson                     |          26|          21|        5|
|   2008|   19|Southern California    |Penn State                  |          38|          24|       14|
|   2008|   19|Virginia Tech          |Cincinnati                  |          20|           7|       13|
|   2008|   19|Kentucky               |East Carolina               |          25|          19|        6|
|   2008|   19|Mississippi            |Texas Tech                  |          47|          34|       13|
|   2008|   19|Utah                   |Alabama                     |          31|          17|       14|
|   2008|   19|Connecticut            |Buffalo                     |          38|          20|       18|
|   2008|   19|Texas                  |Ohio State                  |          24|          21|        3|
|   2008|   20|Tulsa                  |Ball State                  |          45|          13|       32|
|   2008|   20|Florida                |Oklahoma                    |          24|          14|       10|
|   2009|    1|Boise State            |Oregon                      |          19|           8|       11|
|   2009|    1|Bowling Green State    |Troy                        |          31|          14|       17|
|   2009|    1|Indiana                |Eastern Kentucky            |          19|          13|        6|
|   2009|    1|Iowa State             |North Dakota State          |          34|          17|       17|
|   2009|    1|Kent State             |Coastal Carolina            |          18|           0|       18|
|   2009|    1|Ball State             |North Texas                 |          10|          20|      -10|
|   2009|    1|North Carolina State   |South Carolina              |           3|           7|       -4|
|   2009|    1|Utah                   |Utah State                  |          35|          17|       18|
|   2009|    1|Temple                 |Villanova                   |          24|          27|       -3|
|   2009|    1|Hawaii                 |Central Arkansas            |          25|          20|        5|
|   2009|    1|Tulane                 |Tulsa                       |          13|          37|      -24|
|   2009|    1|Air Force              |Nicholls State              |          72|           0|       72|
|   2009|    1|Alabama                |Virginia Tech               |          34|          24|       10|
|   2009|    1|Alabama-Birmingham     |Rice                        |          44|          24|       20|
|   2009|    1|Arizona                |Central Michigan            |          19|           6|       13|
|   2009|    1|Arizona State          |Idaho State                 |          50|           3|       47|
|   2009|    1|Arkansas               |Missouri State              |          48|          10|       38|
|   2009|    1|Arkansas State         |Mississippi Valley State    |          61|           0|       61|
|   2009|    1|Eastern Michigan       |Army                        |          14|          27|      -13|
|   2009|    1|Auburn                 |Louisiana Tech              |          37|          13|       24|
|   2009|    1|Wake Forest            |Baylor                      |          21|          24|       -3|
|   2009|    1|Boston College         |Northeastern                |          54|           0|       54|
|   2009|    1|Brigham Young          |Oklahoma                    |          14|          13|        1|
|   2009|    1|Texas-El Paso          |Buffalo                     |          17|          23|       -6|
|   2009|    1|California             |Maryland                    |          52|          13|       39|
|   2009|    1|Central Florida        |Samford                     |          28|          24|        4|
|   2009|    1|Clemson                |Middle Tennessee State      |          37|          14|       23|
|   2009|    1|Ohio                   |Connecticut                 |          16|          23|       -7|
|   2009|    1|East Carolina          |Appalachian State           |          29|          24|        5|
|   2009|    1|Florida                |Charleston Southern         |          62|           3|       59|
|   2009|    1|Fresno State           |California-Davis            |          51|           0|       51|
|   2009|    1|Georgia Tech           |Jacksonville State          |          37|          17|       20|
|   2009|    1|Houston                |Northwestern State          |          55|           7|       48|
|   2009|    1|New Mexico State       |Idaho                       |           6|          21|      -15|
|   2009|    1|Iowa                   |Northern Iowa               |          17|          16|        1|
|   2009|    1|Kansas                 |Northern Colorado           |          49|           3|       46|
|   2009|    1|Kansas State           |Massachusetts               |          21|          17|        4|
|   2009|    1|Kentucky               |Miami (OH)                  |          42|           0|       42|
|   2009|    1|Washington             |Louisiana State             |          23|          31|       -8|
|   2009|    1|Louisiana-Lafayette    |Southern                    |          42|          19|       23|
|   2009|    1|Louisville             |Indiana State               |          30|          10|       20|
|   2009|    1|Marshall               |Southern Illinois           |          31|          28|        3|
|   2009|    1|Michigan               |Western Michigan            |          31|           7|       24|
|   2009|    1|Michigan State         |Montana State               |          44|           3|       41|
|   2009|    1|Syracuse               |Minnesota                   |          20|          23|       -3|
|   2009|    1|Mississippi State      |Jackson State               |          45|           7|       38|
|   2009|    1|Missouri               |Illinois                    |          37|           9|       28|
|   2009|    1|Nebraska               |Florida Atlantic            |          49|           3|       46|
|   2009|    1|Nevada-Las Vegas       |Sacramento State            |          38|           3|       35|
|   2009|    1|North Carolina         |Citadel                     |          40|           6|       34|
|   2009|    1|Northwestern           |Towson                      |          47|          14|       33|
|   2009|    1|Notre Dame             |Nevada                      |          35|           0|       35|
|   2009|    1|Ohio State             |Navy                        |          31|          27|        4|
|   2009|    1|Oklahoma State         |Georgia                     |          24|          10|       14|
|   2009|    1|Oregon State           |Portland State              |          34|           7|       27|
|   2009|    1|Penn State             |Akron                       |          31|           7|       24|
|   2009|    1|Pittsburgh             |Youngstown State            |          38|           3|       35|
|   2009|    1|Purdue                 |Toledo                      |          52|          31|       21|
|   2009|    1|Duke                   |Richmond                    |          16|          24|       -8|
|   2009|    1|South Florida          |Wofford                     |          40|           7|       33|
|   2009|    1|Southern California    |San Jose State              |          56|           3|       53|
|   2009|    1|Southern Methodist     |Stephen F. Austin           |          31|          23|        8|
|   2009|    1|Southern Mississippi   |Alcorn State                |          52|           0|       52|
|   2009|    1|Washington State       |Stanford                    |          13|          39|      -26|
|   2009|    1|Tennessee              |Western Kentucky            |          63|           7|       56|
|   2009|    1|Texas                  |Louisiana-Monroe            |          59|          20|       39|
|   2009|    1|Texas A&M              |New Mexico                  |          41|           6|       35|
|   2009|    1|Texas Tech             |North Dakota                |          38|          13|       25|
|   2009|    1|UCLA                   |San Diego State             |          33|          14|       19|
|   2009|    1|Vanderbilt             |Western Carolina            |          45|           0|       45|
|   2009|    1|West Virginia          |Liberty                     |          33|          20|       13|
|   2009|    1|Virginia               |William & Mary              |          14|          26|      -12|
|   2009|    1|Wisconsin              |Northern Illinois           |          28|          20|        8|
|   2009|    1|Wyoming                |Weber State                 |          29|          22|        7|
|   2009|    1|Colorado               |Colorado State              |          17|          23|       -6|
|   2009|    1|Memphis                |Mississippi                 |          14|          45|      -31|
|   2009|    1|Rutgers                |Cincinnati                  |          15|          47|      -32|
|   2009|    1|Florida State          |Miami (FL)                  |          34|          38|       -4|
|   2009|    2|Georgia Tech           |Clemson                     |          30|          27|        3|
|   2009|    2|Toledo                 |Colorado                    |          54|          38|       16|
|   2009|    2|Akron                  |Morgan State                |          41|           0|       41|
|   2009|    2|Alabama                |Florida International       |          40|          14|       26|
|   2009|    2|Arizona                |Northern Arizona            |          34|          17|       17|
|   2009|    2|Auburn                 |Mississippi State           |          49|          24|       25|
|   2009|    2|Boise State            |Miami (OH)                  |          48|           0|       48|
|   2009|    2|Boston College         |Kent State                  |          34|           7|       27|
|   2009|    2|Tulane                 |Brigham Young               |           3|          54|      -51|
|   2009|    2|California             |Eastern Washington          |          59|           7|       52|
|   2009|    2|Michigan State         |Central Michigan            |          27|          29|       -2|
|   2009|    2|Cincinnati             |Southeast Missouri State    |          70|           3|       67|
|   2009|    2|Colorado State         |Weber State                 |          24|          23|        1|
|   2009|    2|Army                   |Duke                        |          19|          35|      -16|
|   2009|    2|Florida                |Troy                        |          56|           6|       50|
|   2009|    2|Florida State          |Jacksonville State          |          19|           9|       10|
|   2009|    2|Georgia                |South Carolina              |          41|          37|        4|
|   2009|    2|Washington State       |Hawaii                      |          20|          38|      -18|
|   2009|    2|Oklahoma State         |Houston                     |          35|          45|      -10|
|   2009|    2|Illinois               |Illinois State              |          45|          17|       28|
|   2009|    2|Indiana                |Western Michigan            |          23|          19|        4|
|   2009|    2|Iowa State             |Iowa                        |           3|          35|      -32|
|   2009|    2|Texas-El Paso          |Kansas                      |           7|          34|      -27|
|   2009|    2|Louisiana State        |Vanderbilt                  |          23|           9|       14|
|   2009|    2|Louisiana-Lafayette    |Kansas State                |          17|          15|        2|
|   2009|    2|Louisiana-Monroe       |Texas Southern              |          58|           0|       58|
|   2009|    2|Maryland               |James Madison               |          38|          35|        3|
|   2009|    2|Michigan               |Notre Dame                  |          38|          34|        4|
|   2009|    2|Middle Tennessee State |Memphis                     |          31|          14|       17|
|   2009|    2|Minnesota              |Air Force                   |          20|          13|        7|
|   2009|    2|Missouri               |Bowling Green State         |          27|          20|        7|
|   2009|    2|Navy                   |Louisiana Tech              |          32|          14|       18|
|   2009|    2|Nebraska               |Arkansas State              |          38|           9|       29|
|   2009|    2|Ball State             |New Hampshire               |          16|          23|       -7|
|   2009|    2|New Mexico State       |Prairie View A&M            |          21|          18|        3|
|   2009|    2|Connecticut            |North Carolina              |          10|          12|       -2|
|   2009|    2|North Carolina State   |Murray State                |          65|           7|       58|
|   2009|    2|Northern Illinois      |Western Illinois            |          41|           7|       34|
|   2009|    2|Northwestern           |Eastern Michigan            |          27|          24|        3|
|   2009|    2|North Texas            |Ohio                        |          30|          31|       -1|
|   2009|    2|Oklahoma               |Idaho State                 |          64|           0|       64|
|   2009|    2|Oregon                 |Purdue                      |          38|          36|        2|
|   2009|    2|Nevada-Las Vegas       |Oregon State                |          21|          23|       -2|
|   2009|    2|Penn State             |Syracuse                    |          28|           7|       21|
|   2009|    2|Buffalo                |Pittsburgh                  |          27|          54|      -27|
|   2009|    2|Rutgers                |Howard                      |          45|           7|       38|
|   2009|    2|San Diego State        |Southern Utah               |          35|          19|       16|
|   2009|    2|Western Kentucky       |South Florida               |          13|          35|      -22|
|   2009|    2|Ohio State             |Southern California         |          15|          18|       -3|
|   2009|    2|Alabama-Birmingham     |Southern Methodist          |          33|          35|       -2|
|   2009|    2|Southern Mississippi   |Central Florida             |          26|          19|        7|
|   2009|    2|Wyoming                |Texas                       |          10|          41|      -31|
|   2009|    2|Virginia               |Texas Christian             |          14|          30|      -16|
|   2009|    2|Texas Tech             |Rice                        |          55|          10|       45|
|   2009|    2|New Mexico             |Tulsa                       |          10|          44|      -34|
|   2009|    2|Tennessee              |UCLA                        |          15|          19|       -4|
|   2009|    2|San Jose State         |Utah                        |          14|          24|      -10|
|   2009|    2|Virginia Tech          |Marshall                    |          52|          10|       42|
|   2009|    2|Wake Forest            |Stanford                    |          24|          17|        7|
|   2009|    2|Washington             |Idaho                       |          42|          23|       19|
|   2009|    2|West Virginia          |East Carolina               |          35|          20|       15|
|   2009|    2|Wisconsin              |Fresno State                |          34|          31|        3|
|   2009|    3|Miami (FL)             |Georgia Tech                |          33|          17|       16|
|   2009|    3|Fresno State           |Boise State                 |          34|          51|      -17|
|   2009|    3|New Mexico             |Air Force                   |          13|          37|      -24|
|   2009|    3|Alabama                |North Texas                 |          53|           7|       46|
|   2009|    3|Arizona State          |Louisiana-Monroe            |          38|          14|       24|
|   2009|    3|Army                   |Ball State                  |          24|          17|        7|
|   2009|    3|Auburn                 |West Virginia               |          41|          30|       11|
|   2009|    3|Minnesota              |California                  |          21|          35|      -14|
|   2009|    3|Western Kentucky       |Central Arkansas            |           7|          28|      -21|
|   2009|    3|Central Florida        |Buffalo                     |          23|          17|        6|
|   2009|    3|Central Michigan       |Alcorn State                |          48|           0|       48|
|   2009|    3|Oregon State           |Cincinnati                  |          18|          28|      -10|
|   2009|    3|Clemson                |Boston College              |          25|           7|       18|
|   2009|    3|Colorado               |Wyoming                     |          24|           0|       24|
|   2009|    3|Colorado State         |Nevada                      |          35|          20|       15|
|   2009|    3|Baylor                 |Connecticut                 |          22|          30|       -8|
|   2009|    3|Florida                |Tennessee                   |          23|          13|       10|
|   2009|    3|Brigham Young          |Florida State               |          28|          54|      -26|
|   2009|    3|Arkansas               |Georgia                     |          41|          52|      -11|
|   2009|    3|Idaho                  |San Diego State             |          34|          20|       14|
|   2009|    3|Akron                  |Indiana                     |          21|          38|      -17|
|   2009|    3|Iowa                   |Arizona                     |          27|          17|       10|
|   2009|    3|Kent State             |Iowa State                  |          14|          34|      -20|
|   2009|    3|Kansas                 |Duke                        |          44|          16|       28|
|   2009|    3|Kentucky               |Louisville                  |          31|          27|        4|
|   2009|    3|Louisiana State        |Louisiana-Lafayette         |          31|           3|       28|
|   2009|    3|Louisiana Tech         |Nicholls State              |          48|          13|       35|
|   2009|    3|Marshall               |Bowling Green State         |          17|          10|        7|
|   2009|    3|Memphis                |Tennessee-Martin            |          41|          14|       27|
|   2009|    3|Michigan               |Eastern Michigan            |          45|          17|       28|
|   2009|    3|Maryland               |Middle Tennessee State      |          31|          32|       -1|
|   2009|    3|Mississippi            |Southeastern Louisiana      |          52|           6|       46|
|   2009|    3|Vanderbilt             |Mississippi State           |           3|          15|      -12|
|   2009|    3|Missouri               |Furman                      |          52|          12|       40|
|   2009|    3|Nevada-Las Vegas       |Hawaii                      |          34|          33|        1|
|   2009|    3|North Carolina         |East Carolina               |          31|          17|       14|
|   2009|    3|North Carolina State   |Gardner-Webb                |          45|          14|       31|
|   2009|    3|Purdue                 |Northern Illinois           |          21|          28|       -7|
|   2009|    3|Notre Dame             |Michigan State              |          33|          30|        3|
|   2009|    3|Ohio                   |Cal Poly                    |          28|          10|       18|
|   2009|    3|Ohio State             |Toledo                      |          38|           0|       38|
|   2009|    3|Oklahoma               |Tulsa                       |          45|           0|       45|
|   2009|    3|Oklahoma State         |Rice                        |          41|          24|       17|
|   2009|    3|Oregon                 |Utah                        |          31|          24|        7|
|   2009|    3|Penn State             |Temple                      |          31|           6|       25|
|   2009|    3|Pittsburgh             |Navy                        |          27|          14|       13|
|   2009|    3|Rutgers                |Florida International       |          23|          15|        8|
|   2009|    3|South Carolina         |Florida Atlantic            |          38|          16|       22|
|   2009|    3|South Florida          |Charleston Southern         |          59|           0|       59|
|   2009|    3|Southern Mississippi   |Virginia                    |          37|          34|        3|
|   2009|    3|Stanford               |San Jose State              |          42|          17|       25|
|   2009|    3|Syracuse               |Northwestern                |          37|          34|        3|
|   2009|    3|Texas                  |Texas Tech                  |          34|          24|       10|
|   2009|    3|Texas A&M              |Utah State                  |          38|          30|        8|
|   2009|    3|Texas Christian        |Texas State                 |          56|          21|       35|
|   2009|    3|New Mexico State       |Texas-El Paso               |          12|          38|      -26|
|   2009|    3|Troy                   |Alabama-Birmingham          |          27|          14|       13|
|   2009|    3|UCLA                   |Kansas State                |          23|           9|       14|
|   2009|    3|Virginia Tech          |Nebraska                    |          16|          15|        1|
|   2009|    3|Wake Forest            |Elon                        |          35|           7|       28|
|   2009|    3|Washington             |Southern California         |          16|          13|        3|
|   2009|    3|Washington State       |Southern Methodist          |          30|          27|        3|
|   2009|    3|Western Michigan       |Miami (OH)                  |          48|          26|       22|
|   2009|    3|Wisconsin              |Wofford                     |          44|          14|       30|
|   2009|    4|South Carolina         |Mississippi                 |          16|          10|        6|
|   2009|    4|Nevada                 |Missouri                    |          21|          31|      -10|
|   2009|    4|Air Force              |San Diego State             |          26|          14|       12|
|   2009|    4|Alabama                |Arkansas                    |          35|           7|       28|
|   2009|    4|Oregon State           |Arizona                     |          32|          37|       -5|
|   2009|    4|Auburn                 |Ball State                  |          54|          30|       24|
|   2009|    4|Baylor                 |Northwestern State          |          68|          13|       55|
|   2009|    4|Bowling Green State    |Boise State                 |          14|          49|      -35|
|   2009|    4|Boston College         |Wake Forest                 |          27|          24|        3|
|   2009|    4|Brigham Young          |Colorado State              |          42|          23|       19|
|   2009|    4|Central Michigan       |Akron                       |          48|          21|       27|
|   2009|    4|Cincinnati             |Fresno State                |          28|          20|        8|
|   2009|    4|Connecticut            |Rhode Island                |          52|          10|       42|
|   2009|    4|Duke                   |North Carolina Central      |          49|          14|       35|
|   2009|    4|East Carolina          |Central Florida             |          19|          14|        5|
|   2009|    4|Kentucky               |Florida                     |           7|          41|      -34|
|   2009|    4|Georgia                |Arizona State               |          20|          17|        3|
|   2009|    4|Georgia Tech           |North Carolina              |          24|           7|       17|
|   2009|    4|Houston                |Texas Tech                  |          29|          28|        1|
|   2009|    4|Northern Illinois      |Idaho                       |          31|          34|       -3|
|   2009|    4|Penn State             |Iowa                        |          10|          21|      -11|
|   2009|    4|Iowa State             |Army                        |          31|          10|       21|
|   2009|    4|Kansas                 |Southern Mississippi        |          35|          28|        7|
|   2009|    4|Kansas State           |Tennessee Tech              |          49|           7|       42|
|   2009|    4|Kent State             |Miami (OH)                  |          29|          19|       10|
|   2009|    4|Mississippi State      |Louisiana State             |          26|          30|       -4|
|   2009|    4|Florida Atlantic       |Louisiana-Monroe            |          25|          27|       -2|
|   2009|    4|Memphis                |Marshall                    |          16|          27|      -11|
|   2009|    4|Michigan               |Indiana                     |          36|          33|        3|
|   2009|    4|North Texas            |Middle Tennessee State      |          21|          37|      -16|
|   2009|    4|Northwestern           |Minnesota                   |          24|          35|      -11|
|   2009|    4|Navy                   |Western Kentucky            |          38|          22|       16|
|   2009|    4|Nebraska               |Louisiana-Lafayette         |          55|           0|       55|
|   2009|    4|New Mexico             |New Mexico State            |          17|          20|       -3|
|   2009|    4|North Carolina State   |Pittsburgh                  |          38|          31|        7|
|   2009|    4|Purdue                 |Notre Dame                  |          21|          24|       -3|
|   2009|    4|Ohio State             |Illinois                    |          30|           0|       30|
|   2009|    4|Oklahoma State         |Grambling State             |          56|           6|       50|
|   2009|    4|Oregon                 |California                  |          42|           3|       39|
|   2009|    4|Maryland               |Rutgers                     |          13|          34|      -21|
|   2009|    4|San Jose State         |Cal Poly                    |          19|           9|       10|
|   2009|    4|Florida State          |South Florida               |           7|          17|      -10|
|   2009|    4|Southern California    |Washington State            |          27|           6|       21|
|   2009|    4|Stanford               |Washington                  |          34|          14|       20|
|   2009|    4|Syracuse               |Maine                       |          41|          24|       17|
|   2009|    4|Temple                 |Buffalo                     |          37|          13|       24|
|   2009|    4|Tennessee              |Ohio                        |          34|          23|       11|
|   2009|    4|Texas                  |Texas-El Paso               |          64|           7|       57|
|   2009|    4|Texas A&M              |Alabama-Birmingham          |          56|          19|       37|
|   2009|    4|Clemson                |Texas Christian             |          10|          14|       -4|
|   2009|    4|Florida International  |Toledo                      |          31|          41|      -10|
|   2009|    4|Arkansas State         |Troy                        |          27|          30|       -3|
|   2009|    4|Tulane                 |McNeese State               |          42|          32|       10|
|   2009|    4|Tulsa                  |Sam Houston State           |          56|           3|       53|
|   2009|    4|Utah                   |Louisville                  |          30|          14|       16|
|   2009|    4|Utah State             |Southern Utah               |          53|          34|       19|
|   2009|    4|Rice                   |Vanderbilt                  |          17|          36|      -19|
|   2009|    4|Virginia Tech          |Miami (FL)                  |          31|           7|       24|
|   2009|    4|Western Michigan       |Hofstra                     |          24|          10|       14|
|   2009|    4|Wisconsin              |Michigan State              |          38|          30|        8|
|   2009|    4|Wyoming                |Nevada-Las Vegas            |          30|          27|        3|
|   2009|    5|Louisiana Tech         |Hawaii                      |          27|           6|       21|
|   2009|    5|Alabama-Birmingham     |Southern Mississippi        |          30|          17|       13|
|   2009|    5|West Virginia          |Colorado                    |          35|          24|       11|
|   2009|    5|Brigham Young          |Utah State                  |          35|          17|       18|
|   2009|    5|Louisville             |Pittsburgh                  |          10|          35|      -25|
|   2009|    5|Kentucky               |Alabama                     |          20|          38|      -18|
|   2009|    5|Arkansas               |Texas A&M                   |          47|          19|       28|
|   2009|    5|Tennessee              |Auburn                      |          22|          26|       -4|
|   2009|    5|Baylor                 |Kent State                  |          31|          15|       16|
|   2009|    5|Boise State            |California-Davis            |          34|          16|       18|
|   2009|    5|Boston College         |Florida State               |          28|          21|        7|
|   2009|    5|Central Florida        |Memphis                     |          32|          14|       18|
|   2009|    5|Buffalo                |Central Michigan            |          13|          20|       -7|
|   2009|    5|Miami (OH)             |Cincinnati                  |          13|          37|      -24|
|   2009|    5|Marshall               |East Carolina               |          17|          21|       -4|
|   2009|    5|Mississippi State      |Georgia Tech                |          31|          42|      -11|
|   2009|    5|Idaho                  |Colorado State              |          31|          29|        2|
|   2009|    5|Iowa                   |Arkansas State              |          24|          21|        3|
|   2009|    5|Kansas State           |Iowa State                  |          24|          23|        1|
|   2009|    5|Georgia                |Louisiana State             |          13|          20|       -7|
|   2009|    5|Louisiana-Monroe       |Florida International       |          48|          35|       13|
|   2009|    5|Maryland               |Clemson                     |          24|          21|        3|
|   2009|    5|Miami (FL)             |Oklahoma                    |          21|          20|        1|
|   2009|    5|Michigan State         |Michigan                    |          26|          20|        6|
|   2009|    5|Vanderbilt             |Mississippi                 |           7|          23|      -16|
|   2009|    5|Navy                   |Air Force                   |          16|          13|        3|
|   2009|    5|Nevada                 |Nevada-Las Vegas            |          63|          28|       35|
|   2009|    5|Northern Illinois      |Western Michigan            |          38|           3|       35|
|   2009|    5|Purdue                 |Northwestern                |          21|          27|       -6|
|   2009|    5|Notre Dame             |Washington                  |          37|          30|        7|
|   2009|    5|Bowling Green State    |Ohio                        |          37|          44|       -7|
|   2009|    5|Indiana                |Ohio State                  |          14|          33|      -19|
|   2009|    5|Oregon                 |Washington State            |          52|           6|       46|
|   2009|    5|Arizona State          |Oregon State                |          17|          28|      -11|
|   2009|    5|Illinois               |Penn State                  |          17|          35|      -18|
|   2009|    5|San Diego State        |New Mexico State            |          34|          17|       17|
|   2009|    5|South Carolina         |South Carolina State        |          38|          14|       24|
|   2009|    5|Syracuse               |South Florida               |          20|          34|      -14|
|   2009|    5|California             |Southern California         |           3|          30|      -27|
|   2009|    5|Stanford               |UCLA                        |          24|          16|        8|
|   2009|    5|Eastern Michigan       |Temple                      |          12|          24|      -12|
|   2009|    5|Texas Christian        |Southern Methodist          |          39|          14|       25|
|   2009|    5|Texas Tech             |New Mexico                  |          48|          28|       20|
|   2009|    5|Texas-El Paso          |Houston                     |          58|          41|       17|
|   2009|    5|Ball State             |Toledo                      |          30|          37|       -7|
|   2009|    5|Army                   |Tulane                      |          16|          17|       -1|
|   2009|    5|Rice                   |Tulsa                       |          10|          27|      -17|
|   2009|    5|North Carolina         |Virginia                    |           3|          16|      -13|
|   2009|    5|Duke                   |Virginia Tech               |          26|          34|       -8|
|   2009|    5|Wake Forest            |North Carolina State        |          30|          24|        6|
|   2009|    5|Minnesota              |Wisconsin                   |          28|          31|       -3|
|   2009|    5|Florida Atlantic       |Wyoming                     |          28|          30|       -2|
|   2009|    6|Troy                   |Middle Tennessee State      |          31|           7|       24|
|   2009|    6|Missouri               |Nebraska                    |          12|          27|      -15|
|   2009|    6|Nevada                 |Louisiana Tech              |          37|          14|       23|
|   2009|    6|Mississippi            |Alabama                     |           3|          22|      -19|
|   2009|    6|Washington State       |Arizona State               |          14|          27|      -13|
|   2009|    6|Arkansas               |Auburn                      |          44|          23|       21|
|   2009|    6|Army                   |Vanderbilt                  |          16|          13|        3|
|   2009|    6|Kent State             |Bowling Green State         |          35|          36|       -1|
|   2009|    6|Nevada-Las Vegas       |Brigham Young               |          21|          59|      -38|
|   2009|    6|Buffalo                |Gardner-Webb                |          40|           3|       37|
|   2009|    6|Central Michigan       |Eastern Michigan            |          56|           8|       48|
|   2009|    6|North Carolina State   |Duke                        |          28|          49|      -21|
|   2009|    6|Louisiana State        |Florida                     |           3|          13|      -10|
|   2009|    6|Western Kentucky       |Florida International       |          20|          37|      -17|
|   2009|    6|Hawaii                 |Fresno State                |          17|          42|      -25|
|   2009|    6|Florida State          |Georgia Tech                |          44|          49|       -5|
|   2009|    6|Mississippi State      |Houston                     |          24|          31|       -7|
|   2009|    6|San Jose State         |Idaho                       |          25|          29|       -4|
|   2009|    6|Iowa                   |Michigan                    |          30|          28|        2|
|   2009|    6|Kansas                 |Iowa State                  |          41|          36|        5|
|   2009|    6|Louisiana-Lafayette    |North Texas                 |          38|          34|        4|
|   2009|    6|Louisville             |Southern Mississippi        |          25|          23|        2|
|   2009|    6|Tulane                 |Marshall                    |          10|          31|      -21|
|   2009|    6|Memphis                |Texas-El Paso               |          35|          20|       15|
|   2009|    6|Miami (FL)             |Florida A&M                 |          48|          16|       32|
|   2009|    6|Illinois               |Michigan State              |          14|          24|      -10|
|   2009|    6|Minnesota              |Purdue                      |          35|          20|       15|
|   2009|    6|Rice                   |Navy                        |          14|          63|      -49|
|   2009|    6|New Mexico State       |Utah State                  |          20|          17|        3|
|   2009|    6|North Carolina         |Georgia Southern            |          42|          12|       30|
|   2009|    6|Northwestern           |Miami (OH)                  |          16|           6|       10|
|   2009|    6|Akron                  |Ohio                        |           7|          19|      -12|
|   2009|    6|Ohio State             |Wisconsin                   |          31|          13|       18|
|   2009|    6|Oklahoma               |Baylor                      |          33|           7|       26|
|   2009|    6|Texas A&M              |Oklahoma State              |          31|          36|       -5|
|   2009|    6|UCLA                   |Oregon                      |          10|          24|      -14|
|   2009|    6|Oregon State           |Stanford                    |          38|          28|       10|
|   2009|    6|Penn State             |Eastern Illinois            |          52|           3|       49|
|   2009|    6|Pittsburgh             |Connecticut                 |          24|          21|        3|
|   2009|    6|Rutgers                |Texas Southern              |          42|           0|       42|
|   2009|    6|South Carolina         |Kentucky                    |          28|          26|        2|
|   2009|    6|Southern Methodist     |East Carolina               |          28|          21|        7|
|   2009|    6|Temple                 |Ball State                  |          24|          19|        5|
|   2009|    6|Tennessee              |Georgia                     |          45|          19|       26|
|   2009|    6|Texas                  |Colorado                    |          38|          14|       24|
|   2009|    6|Air Force              |Texas Christian             |          17|          20|       -3|
|   2009|    6|Texas Tech             |Kansas State                |          66|          14|       52|
|   2009|    6|Colorado State         |Utah                        |          17|          24|       -7|
|   2009|    6|Virginia               |Indiana                     |          47|           7|       40|
|   2009|    6|Virginia Tech          |Boston College              |          48|          14|       34|
|   2009|    6|Wake Forest            |Maryland                    |          42|          32|       10|
|   2009|    6|Washington             |Arizona                     |          36|          33|        3|
|   2009|    6|Syracuse               |West Virginia               |          13|          34|      -21|
|   2009|    6|Toledo                 |Western Michigan            |          26|          58|      -32|
|   2009|    6|Wyoming                |New Mexico                  |          37|          13|       24|
|   2009|    7|Louisiana-Monroe       |Arkansas State              |          16|          10|        6|
|   2009|    7|Tulsa                  |Boise State                 |          21|          28|       -7|
|   2009|    7|South Florida          |Cincinnati                  |          17|          34|      -17|
|   2009|    7|Rutgers                |Pittsburgh                  |          17|          24|       -7|
|   2009|    7|Air Force              |Wyoming                     |          10|           0|       10|
|   2009|    7|Alabama                |South Carolina              |          20|           6|       14|
|   2009|    7|Arizona                |Stanford                    |          43|          38|        5|
|   2009|    7|Arizona State          |Washington                  |          24|          17|        7|
|   2009|    7|Boston College         |North Carolina State        |          52|          20|       32|
|   2009|    7|Ball State             |Bowling Green State         |          17|          31|      -14|
|   2009|    7|San Diego State        |Brigham Young               |          28|          38|      -10|
|   2009|    7|Buffalo                |Akron                       |          21|          17|        4|
|   2009|    7|UCLA                   |California                  |          26|          45|      -19|
|   2009|    7|Western Michigan       |Central Michigan            |          23|          34|      -11|
|   2009|    7|Clemson                |Wake Forest                 |          38|           3|       35|
|   2009|    7|Colorado               |Kansas                      |          34|          30|        4|
|   2009|    7|Connecticut            |Louisville                  |          38|          25|       13|
|   2009|    7|East Carolina          |Rice                        |          49|          13|       36|
|   2009|    7|Florida                |Arkansas                    |          23|          20|        3|
|   2009|    7|North Texas            |Florida Atlantic            |          40|          44|       -4|
|   2009|    7|Fresno State           |San Jose State              |          41|          21|       20|
|   2009|    7|Vanderbilt             |Georgia                     |          10|          34|      -24|
|   2009|    7|Georgia Tech           |Virginia Tech               |          28|          23|        5|
|   2009|    7|Tulane                 |Houston                     |          16|          44|      -28|
|   2009|    7|Idaho                  |Hawaii                      |          35|          23|       12|
|   2009|    7|Indiana                |Illinois                    |          27|          14|       13|
|   2009|    7|Wisconsin              |Iowa                        |          10|          20|      -10|
|   2009|    7|Iowa State             |Baylor                      |          24|          10|       14|
|   2009|    7|Kansas State           |Texas A&M                   |          62|          14|       48|
|   2009|    7|Eastern Michigan       |Kent State                  |           6|          28|      -22|
|   2009|    7|Auburn                 |Kentucky                    |          14|          21|       -7|
|   2009|    7|Louisiana Tech         |New Mexico State            |          45|           7|       38|
|   2009|    7|Western Kentucky       |Louisiana-Lafayette         |          22|          30|       -8|
|   2009|    7|Central Florida        |Miami (FL)                  |           7|          27|      -20|
|   2009|    7|Michigan               |Delaware State              |          63|           6|       57|
|   2009|    7|Michigan State         |Northwestern                |          24|          14|       10|
|   2009|    7|Mississippi            |Alabama-Birmingham          |          48|          13|       35|
|   2009|    7|Middle Tennessee State |Mississippi State           |           6|          27|      -21|
|   2009|    7|Southern Methodist     |Navy                        |          35|          38|       -3|
|   2009|    7|Utah State             |Nevada                      |          32|          35|       -3|
|   2009|    7|Ohio                   |Miami (OH)                  |          28|           7|       21|
|   2009|    7|Oklahoma State         |Missouri                    |          33|          17|       16|
|   2009|    7|Penn State             |Minnesota                   |          20|           0|       20|
|   2009|    7|Purdue                 |Ohio State                  |          26|          18|        8|
|   2009|    7|Notre Dame             |Southern California         |          27|          34|       -7|
|   2009|    7|Southern Mississippi   |Memphis                     |          36|          16|       20|
|   2009|    7|Temple                 |Army                        |          27|          13|       14|
|   2009|    7|Texas                  |Oklahoma                    |          16|          13|        3|
|   2009|    7|Texas Christian        |Colorado State              |          44|           6|       38|
|   2009|    7|Nebraska               |Texas Tech                  |          10|          31|      -21|
|   2009|    7|Toledo                 |Northern Illinois           |          20|          19|        1|
|   2009|    7|Florida International  |Troy                        |          33|          42|       -9|
|   2009|    7|Nevada-Las Vegas       |Utah                        |          15|          35|      -20|
|   2009|    7|Maryland               |Virginia                    |           9|          20|      -11|
|   2009|    7|West Virginia          |Marshall                    |          24|           7|       17|
|   2009|    8|Texas-El Paso          |Tulsa                       |          28|          24|        4|
|   2009|    8|North Carolina         |Florida State               |          27|          30|       -3|
|   2009|    8|Army                   |Rutgers                     |          10|          27|      -17|
|   2009|    8|Alabama                |Tennessee                   |          12|          10|        2|
|   2009|    8|Arizona                |UCLA                        |          27|          13|       14|
|   2009|    8|Arkansas State         |Florida International       |          27|          10|       17|
|   2009|    8|Eastern Michigan       |Ball State                  |          27|          29|       -2|
|   2009|    8|Hawaii                 |Boise State                 |           9|          54|      -45|
|   2009|    8|California             |Washington State            |          49|          17|       32|
|   2009|    8|Rice                   |Central Florida             |           7|          49|      -42|
|   2009|    8|Bowling Green State    |Central Michigan            |          10|          24|      -14|
|   2009|    8|Cincinnati             |Louisville                  |          41|          10|       31|
|   2009|    8|Miami (FL)             |Clemson                     |          37|          40|       -3|
|   2009|    8|Duke                   |Maryland                    |          17|          13|        4|
|   2009|    8|Mississippi State      |Florida                     |          19|          29|      -10|
|   2009|    8|Louisiana-Lafayette    |Florida Atlantic            |          29|          51|      -22|
|   2009|    8|New Mexico State       |Fresno State                |           3|          34|      -31|
|   2009|    8|Virginia               |Georgia Tech                |           9|          34|      -25|
|   2009|    8|Houston                |Southern Methodist          |          38|          15|       23|
|   2009|    8|Michigan State         |Iowa                        |          13|          15|       -2|
|   2009|    8|Nebraska               |Iowa State                  |           7|           9|       -2|
|   2009|    8|Kansas State           |Colorado                    |          20|           6|       14|
|   2009|    8|Ohio                   |Kent State                  |          11|          20|       -9|
|   2009|    8|Kentucky               |Louisiana-Monroe            |          36|          13|       23|
|   2009|    8|Louisiana State        |Auburn                      |          31|          10|       21|
|   2009|    8|Marshall               |Alabama-Birmingham          |          27|           7|       20|
|   2009|    8|Middle Tennessee State |Western Kentucky            |          62|          24|       38|
|   2009|    8|Mississippi            |Arkansas                    |          30|          17|       13|
|   2009|    8|Navy                   |Wake Forest                 |          13|          10|        3|
|   2009|    8|Nevada                 |Idaho                       |          70|          45|       25|
|   2009|    8|New Mexico             |Nevada-Las Vegas            |          17|          34|      -17|
|   2009|    8|Miami (OH)             |Northern Illinois           |          22|          27|       -5|
|   2009|    8|Northwestern           |Indiana                     |          29|          28|        1|
|   2009|    8|Notre Dame             |Boston College              |          20|          16|        4|
|   2009|    8|Ohio State             |Minnesota                   |          38|           7|       31|
|   2009|    8|Kansas                 |Oklahoma                    |          13|          35|      -22|
|   2009|    8|Baylor                 |Oklahoma State              |           7|          34|      -27|
|   2009|    8|Washington             |Oregon                      |          19|          43|      -24|
|   2009|    8|Michigan               |Penn State                  |          10|          35|      -25|
|   2009|    8|Pittsburgh             |South Florida               |          41|          14|       27|
|   2009|    8|Purdue                 |Illinois                    |          24|          14|       10|
|   2009|    8|Colorado State         |San Diego State             |          28|          42|      -14|
|   2009|    8|South Carolina         |Vanderbilt                  |          14|          10|        4|
|   2009|    8|Southern California    |Oregon State                |          42|          36|        6|
|   2009|    8|Southern Mississippi   |Tulane                      |          43|           6|       37|
|   2009|    8|Stanford               |Arizona State               |          33|          14|       19|
|   2009|    8|Syracuse               |Akron                       |          28|          14|       14|
|   2009|    8|Toledo                 |Temple                      |          24|          40|      -16|
|   2009|    8|Missouri               |Texas                       |           7|          41|      -34|
|   2009|    8|Texas Tech             |Texas A&M                   |          30|          52|      -22|
|   2009|    8|Brigham Young          |Texas Christian             |           7|          38|      -31|
|   2009|    8|Troy                   |North Texas                 |          50|          26|       24|
|   2009|    8|Utah                   |Air Force                   |          23|          16|        7|
|   2009|    8|Utah State             |Louisiana Tech              |          23|          21|        2|
|   2009|    8|West Virginia          |Connecticut                 |          28|          24|        4|
|   2009|    8|Western Michigan       |Buffalo                     |          34|          31|        3|
|   2009|    9|Memphis                |East Carolina               |          19|          38|      -19|
|   2009|    9|Virginia Tech          |North Carolina              |          17|          20|       -3|
|   2009|    9|South Florida          |West Virginia               |          30|          19|       11|
|   2009|    9|Colorado State         |Air Force                   |          16|          34|      -18|
|   2009|    9|Texas-El Paso          |Alabama-Birmingham          |          33|          38|       -5|
|   2009|    9|Arkansas               |Eastern Michigan            |          63|          27|       36|
|   2009|    9|Auburn                 |Mississippi                 |          33|          20|       13|
|   2009|    9|Boise State            |San Jose State              |          45|           7|       38|
|   2009|    9|Boston College         |Central Michigan            |          31|          10|       21|
|   2009|    9|Arizona State          |California                  |          21|          23|       -2|
|   2009|    9|Syracuse               |Cincinnati                  |           7|          28|      -21|
|   2009|    9|Clemson                |Coastal Carolina            |          49|           3|       46|
|   2009|    9|Virginia               |Duke                        |          17|          28|      -11|
|   2009|    9|Florida                |Georgia                     |          41|          17|       24|
|   2009|    9|Florida International  |Louisiana-Lafayette         |          20|          17|        3|
|   2009|    9|Florida State          |North Carolina State        |          45|          42|        3|
|   2009|    9|Fresno State           |Utah State                  |          31|          27|        4|
|   2009|    9|Vanderbilt             |Georgia Tech                |          31|          56|      -25|
|   2009|    9|Houston                |Southern Mississippi        |          50|          43|        7|
|   2009|    9|Idaho                  |Louisiana Tech              |          35|          34|        1|
|   2009|    9|Illinois               |Michigan                    |          38|          13|       25|
|   2009|    9|Iowa                   |Indiana                     |          42|          24|       18|
|   2009|    9|Kent State             |Western Michigan            |          26|          14|       12|
|   2009|    9|Louisiana State        |Tulane                      |          42|           0|       42|
|   2009|    9|Louisville             |Arkansas State              |          21|          13|        8|
|   2009|    9|Wake Forest            |Miami (FL)                  |          27|          28|       -1|
|   2009|    9|Miami (OH)             |Toledo                      |          31|          24|        7|
|   2009|    9|Florida Atlantic       |Middle Tennessee State      |          20|          27|       -7|
|   2009|    9|Minnesota              |Michigan State              |          42|          34|        8|
|   2009|    9|Kentucky               |Mississippi State           |          24|          31|       -7|
|   2009|    9|Colorado               |Missouri                    |          17|          36|      -19|
|   2009|    9|Baylor                 |Nebraska                    |          10|          20|      -10|
|   2009|    9|Nevada                 |Hawaii                      |          31|          21|       10|
|   2009|    9|North Texas            |Western Kentucky            |          68|          49|       19|
|   2009|    9|Northern Illinois      |Akron                       |          27|          10|       17|
|   2009|    9|Notre Dame             |Washington State            |          40|          14|       26|
|   2009|    9|Ball State             |Ohio                        |          17|          20|       -3|
|   2009|    9|Ohio State             |New Mexico State            |          45|           0|       45|
|   2009|    9|Oklahoma               |Kansas State                |          42|          30|       12|
|   2009|    9|Oregon                 |Southern California         |          47|          20|       27|
|   2009|    9|Oregon State           |UCLA                        |          26|          19|        7|
|   2009|    9|Northwestern           |Penn State                  |          13|          34|      -21|
|   2009|    9|Connecticut            |Rutgers                     |          24|          28|       -4|
|   2009|    9|San Diego State        |New Mexico                  |          23|          20|        3|
|   2009|    9|Tulsa                  |Southern Methodist          |          13|          27|      -14|
|   2009|    9|Navy                   |Temple                      |          24|          27|       -3|
|   2009|    9|Tennessee              |South Carolina              |          31|          13|       18|
|   2009|    9|Oklahoma State         |Texas                       |          14|          41|      -27|
|   2009|    9|Texas A&M              |Iowa State                  |          35|          10|       25|
|   2009|    9|Texas Christian        |Nevada-Las Vegas            |          41|           0|       41|
|   2009|    9|Texas Tech             |Kansas                      |          42|          21|       21|
|   2009|    9|Troy                   |Louisiana-Monroe            |          42|          21|       21|
|   2009|    9|Utah                   |Wyoming                     |          22|          10|       12|
|   2009|    9|Wisconsin              |Purdue                      |          37|           0|       37|
|   2009|    9|Central Florida        |Marshall                    |          21|          20|        1|
|   2009|   10|Buffalo                |Bowling Green State         |          29|          30|       -1|
|   2009|   10|Northern Illinois      |Eastern Michigan            |          50|           6|       44|
|   2009|   10|Temple                 |Miami (OH)                  |          34|          32|        2|
|   2009|   10|East Carolina          |Virginia Tech               |           3|          16|      -13|
|   2009|   10|Louisiana Tech         |Boise State                 |          35|          45|      -10|
|   2009|   10|Air Force              |Army                        |          35|           7|       28|
|   2009|   10|Akron                  |Kent State                  |          28|          20|        8|
|   2009|   10|Alabama                |Louisiana State             |          24|          15|        9|
|   2009|   10|Alabama-Birmingham     |Florida Atlantic            |          56|          29|       27|
|   2009|   10|Arizona                |Washington State            |          48|           7|       41|
|   2009|   10|Arkansas               |South Carolina              |          33|          16|       17|
|   2009|   10|Auburn                 |Furman                      |          63|          31|       32|
|   2009|   10|Missouri               |Baylor                      |          32|          40|       -8|
|   2009|   10|Wyoming                |Brigham Young               |           0|          52|      -52|
|   2009|   10|Cincinnati             |Connecticut                 |          47|          45|        2|
|   2009|   10|Clemson                |Florida State               |          40|          24|       16|
|   2009|   10|Colorado               |Texas A&M                   |          35|          34|        1|
|   2009|   10|Florida                |Vanderbilt                  |          27|           3|       24|
|   2009|   10|Idaho                  |Fresno State                |          21|          31|      -10|
|   2009|   10|Georgia                |Tennessee Tech              |          38|           0|       38|
|   2009|   10|Georgia Tech           |Wake Forest                 |          30|          27|        3|
|   2009|   10|Hawaii                 |Utah State                  |          49|          36|       13|
|   2009|   10|Tulsa                  |Houston                     |          45|          46|       -1|
|   2009|   10|Minnesota              |Illinois                    |          32|          35|       -3|
|   2009|   10|Kansas State           |Kansas                      |          17|          10|        7|
|   2009|   10|Kentucky               |Eastern Kentucky            |          37|          12|       25|
|   2009|   10|Arkansas State         |Louisiana-Lafayette         |          18|          21|       -3|
|   2009|   10|North Texas            |Louisiana-Monroe            |           6|          33|      -27|
|   2009|   10|Miami (FL)             |Virginia                    |          52|          17|       35|
|   2009|   10|Michigan State         |Western Michigan            |          49|          14|       35|
|   2009|   10|Middle Tennessee State |Florida International       |          48|          21|       27|
|   2009|   10|Mississippi            |Northern Arizona            |          38|          14|       24|
|   2009|   10|Notre Dame             |Navy                        |          21|          23|       -2|
|   2009|   10|Nebraska               |Oklahoma                    |          10|           3|        7|
|   2009|   10|Nevada-Las Vegas       |Colorado State              |          35|          16|       19|
|   2009|   10|North Carolina         |Duke                        |          19|           6|       13|
|   2009|   10|North Carolina State   |Maryland                    |          38|          31|        7|
|   2009|   10|Iowa                   |Northwestern                |          10|          17|       -7|
|   2009|   10|Penn State             |Ohio State                  |           7|          24|      -17|
|   2009|   10|Iowa State             |Oklahoma State              |           8|          34|      -26|
|   2009|   10|California             |Oregon State                |          14|          31|      -17|
|   2009|   10|Pittsburgh             |Syracuse                    |          37|          10|       27|
|   2009|   10|Michigan               |Purdue                      |          36|          38|       -2|
|   2009|   10|Arizona State          |Southern California         |           9|          14|       -5|
|   2009|   10|Southern Methodist     |Rice                        |          31|          28|        3|
|   2009|   10|Stanford               |Oregon                      |          51|          42|        9|
|   2009|   10|Tennessee              |Memphis                     |          56|          28|       28|
|   2009|   10|Texas                  |Central Florida             |          35|           3|       32|
|   2009|   10|San Diego State        |Texas Christian             |          12|          55|      -43|
|   2009|   10|Western Kentucky       |Troy                        |          20|          40|      -20|
|   2009|   10|Tulane                 |Texas-El Paso               |          45|          38|        7|
|   2009|   10|UCLA                   |Washington                  |          24|          23|        1|
|   2009|   10|Utah                   |New Mexico                  |          45|          14|       31|
|   2009|   10|West Virginia          |Louisville                  |          17|           9|        8|
|   2009|   10|Indiana                |Wisconsin                   |          28|          31|       -3|
|   2009|   10|San Jose State         |Nevada                      |           7|          62|      -55|
|   2009|   11|Buffalo                |Ohio                        |          24|          27|       -3|
|   2009|   11|Central Michigan       |Toledo                      |          56|          28|       28|
|   2009|   11|Miami (OH)             |Bowling Green State         |          14|          35|      -21|
|   2009|   11|Northern Illinois      |Ball State                  |          26|          20|        6|
|   2009|   11|Rutgers                |South Florida               |          31|           0|       31|
|   2009|   11|Cincinnati             |West Virginia               |          24|          21|        3|
|   2009|   11|Akron                  |Temple                      |          17|          56|      -39|
|   2009|   11|Air Force              |Nevada-Las Vegas            |          45|          17|       28|
|   2009|   11|Mississippi State      |Alabama                     |           3|          31|      -28|
|   2009|   11|Memphis                |Alabama-Birmingham          |          21|          31|      -10|
|   2009|   11|Arkansas               |Troy                        |          56|          20|       36|
|   2009|   11|Army                   |Virginia Military Institute |          22|          17|        5|
|   2009|   11|Boise State            |Idaho                       |          63|          25|       38|
|   2009|   11|Virginia               |Boston College              |          10|          14|       -4|
|   2009|   11|New Mexico             |Brigham Young               |          19|          24|       -5|
|   2009|   11|California             |Arizona                     |          24|          16|        8|
|   2009|   11|Central Florida        |Houston                     |          37|          32|        5|
|   2009|   11|North Carolina State   |Clemson                     |          23|          43|      -20|
|   2009|   11|South Carolina         |Florida                     |          14|          24|      -10|
|   2009|   11|Florida Atlantic       |Arkansas State              |          35|          18|       17|
|   2009|   11|Florida International  |North Texas                 |          35|          28|        7|
|   2009|   11|Wake Forest            |Florida State               |          28|          41|      -13|
|   2009|   11|Georgia                |Auburn                      |          31|          24|        7|
|   2009|   11|Duke                   |Georgia Tech                |          10|          49|      -39|
|   2009|   11|Hawaii                 |New Mexico State            |          24|           6|       18|
|   2009|   11|Iowa State             |Colorado                    |          17|          10|        7|
|   2009|   11|Vanderbilt             |Kentucky                    |          13|          24|      -11|
|   2009|   11|Louisiana State        |Louisiana Tech              |          24|          16|        8|
|   2009|   11|Louisiana-Monroe       |Western Kentucky            |          21|          18|        3|
|   2009|   11|Louisville             |Syracuse                    |          10|           9|        1|
|   2009|   11|Purdue                 |Michigan State              |          37|          40|       -3|
|   2009|   11|Middle Tennessee State |Louisiana-Lafayette         |          34|          17|       17|
|   2009|   11|Minnesota              |South Dakota State          |          16|          13|        3|
|   2009|   11|Mississippi            |Tennessee                   |          42|          17|       25|
|   2009|   11|Kansas State           |Missouri                    |          12|          38|      -26|
|   2009|   11|Navy                   |Delaware                    |          35|          18|       17|
|   2009|   11|Kansas                 |Nebraska                    |          17|          31|      -14|
|   2009|   11|Nevada                 |Fresno State                |          52|          14|       38|
|   2009|   11|North Carolina         |Miami (FL)                  |          33|          24|        9|
|   2009|   11|Illinois               |Northwestern                |          16|          21|       -5|
|   2009|   11|Ohio State             |Iowa                        |          27|          24|        3|
|   2009|   11|Oklahoma               |Texas A&M                   |          65|          10|       55|
|   2009|   11|Oklahoma State         |Texas Tech                  |          24|          17|        7|
|   2009|   11|Oregon                 |Arizona State               |          44|          21|       23|
|   2009|   11|Oregon State           |Washington                  |          48|          21|       27|
|   2009|   11|Penn State             |Indiana                     |          31|          20|       11|
|   2009|   11|Pittsburgh             |Notre Dame                  |          27|          22|        5|
|   2009|   11|Rice                   |Tulane                      |          28|          20|        8|
|   2009|   11|Southern Methodist     |Texas-El Paso               |          35|          31|        4|
|   2009|   11|Marshall               |Southern Mississippi        |          20|          27|       -7|
|   2009|   11|Southern California    |Stanford                    |          21|          55|      -34|
|   2009|   11|Baylor                 |Texas                       |          14|          47|      -33|
|   2009|   11|Texas Christian        |Utah                        |          55|          28|       27|
|   2009|   11|Washington State       |UCLA                        |           7|          43|      -36|
|   2009|   11|Utah State             |San Jose State              |          24|           9|       15|
|   2009|   11|Maryland               |Virginia Tech               |           9|          36|      -27|
|   2009|   11|Eastern Michigan       |Western Michigan            |          14|          35|      -21|
|   2009|   11|Wisconsin              |Michigan                    |          45|          24|       21|
|   2009|   11|San Diego State        |Wyoming                     |          27|          30|       -3|
|   2009|   11|Tulsa                  |East Carolina               |          17|          44|      -27|
|   2009|   12|Miami (OH)             |Buffalo                     |          17|          42|      -25|
|   2009|   12|Ball State             |Central Michigan            |           3|          35|      -32|
|   2009|   12|Oklahoma State         |Colorado                    |          31|          28|        3|
|   2009|   12|Utah State             |Boise State                 |          21|          52|      -31|
|   2009|   12|Bowling Green State    |Akron                       |          36|          20|       16|
|   2009|   12|Toledo                 |Eastern Michigan            |          47|          21|       26|
|   2009|   12|Alabama                |Chattanooga                 |          45|           0|       45|
|   2009|   12|Arkansas               |Mississippi State           |          42|          21|       21|
|   2009|   12|North Texas            |Army                        |          13|          17|       -4|
|   2009|   12|Brigham Young          |Air Force                   |          38|          21|       17|
|   2009|   12|Stanford               |California                  |          28|          34|       -6|
|   2009|   12|Central Florida        |Tulane                      |          49|           0|       49|
|   2009|   12|Clemson                |Virginia                    |          34|          21|       13|
|   2009|   12|Notre Dame             |Connecticut                 |          30|          33|       -3|
|   2009|   12|East Carolina          |Alabama-Birmingham          |          37|          21|       16|
|   2009|   12|Florida                |Florida International       |          62|           3|       59|
|   2009|   12|Florida State          |Maryland                    |          29|          26|        3|
|   2009|   12|Fresno State           |Louisiana Tech              |          30|          28|        2|
|   2009|   12|San Jose State         |Hawaii                      |          10|          17|       -7|
|   2009|   12|Houston                |Memphis                     |          55|          14|       41|
|   2009|   12|Iowa                   |Minnesota                   |          12|           0|       12|
|   2009|   12|Georgia                |Kentucky                    |          27|          34|       -7|
|   2009|   12|Louisiana-Lafayette    |Louisiana-Monroe            |          21|          17|        4|
|   2009|   12|Marshall               |Southern Methodist          |          34|          31|        3|
|   2009|   12|Miami (FL)             |Duke                        |          34|          16|       18|
|   2009|   12|Middle Tennessee State |Arkansas State              |          38|          14|       24|
|   2009|   12|Mississippi            |Louisiana State             |          25|          23|        2|
|   2009|   12|Missouri               |Iowa State                  |          34|          24|       10|
|   2009|   12|Nebraska               |Kansas State                |          17|           3|       14|
|   2009|   12|New Mexico State       |Nevada                      |          20|          63|      -43|
|   2009|   12|New Mexico             |Colorado State              |          29|          27|        2|
|   2009|   12|Boston College         |North Carolina              |          13|          31|      -18|
|   2009|   12|Northwestern           |Wisconsin                   |          33|          31|        2|
|   2009|   12|Ohio                   |Northern Illinois           |          38|          31|        7|
|   2009|   12|Michigan               |Ohio State                  |          10|          21|      -11|
|   2009|   12|Arizona                |Oregon                      |          41|          44|       -3|
|   2009|   12|Washington State       |Oregon State                |          10|          42|      -32|
|   2009|   12|Michigan State         |Penn State                  |          14|          42|      -28|
|   2009|   12|Indiana                |Purdue                      |          21|          38|      -17|
|   2009|   12|Rice                   |Texas-El Paso               |          30|          29|        1|
|   2009|   12|South Florida          |Louisville                  |          34|          22|       12|
|   2009|   12|Southern Mississippi   |Tulsa                       |          44|          34|       10|
|   2009|   12|Syracuse               |Rutgers                     |          31|          13|       18|
|   2009|   12|Temple                 |Kent State                  |          47|          13|       34|
|   2009|   12|Tennessee              |Vanderbilt                  |          31|          16|       15|
|   2009|   12|Texas                  |Kansas                      |          51|          20|       31|
|   2009|   12|Texas A&M              |Baylor                      |          38|           3|       35|
|   2009|   12|Wyoming                |Texas Christian             |          10|          45|      -35|
|   2009|   12|Texas Tech             |Oklahoma                    |          41|          13|       28|
|   2009|   12|Troy                   |Florida Atlantic            |          47|          21|       26|
|   2009|   12|UCLA                   |Arizona State               |          23|          13|       10|
|   2009|   12|Utah                   |San Diego State             |          38|           7|       31|
|   2009|   12|Virginia Tech          |North Carolina State        |          38|          10|       28|
|   2009|   13|Western Michigan       |Ball State                  |          17|          22|       -5|
|   2009|   13|Texas A&M              |Texas                       |          39|          49|      -10|
|   2009|   13|Akron                  |Eastern Michigan            |          28|          21|        7|
|   2009|   13|Auburn                 |Alabama                     |          21|          26|       -5|
|   2009|   13|Boise State            |Nevada                      |          44|          33|       11|
|   2009|   13|Bowling Green State    |Toledo                      |          38|          24|       14|
|   2009|   13|Kent State             |Buffalo                     |           6|           9|       -3|
|   2009|   13|Central Michigan       |Northern Illinois           |          45|          31|       14|
|   2009|   13|Cincinnati             |Illinois                    |          49|          36|       13|
|   2009|   13|Colorado               |Nebraska                    |          20|          28|       -8|
|   2009|   13|Ohio                   |Temple                      |          35|          17|       18|
|   2009|   13|Louisville             |Rutgers                     |          14|          34|      -20|
|   2009|   13|Tulsa                  |Memphis                     |          33|          30|        3|
|   2009|   13|West Virginia          |Pittsburgh                  |          19|          16|        3|
|   2009|   13|Colorado State         |Wyoming                     |          16|          17|       -1|
|   2009|   13|Arizona State          |Arizona                     |          17|          20|       -3|
|   2009|   13|Arkansas State         |North Texas                 |          30|          26|        4|
|   2009|   13|Maryland               |Boston College              |          17|          19|       -2|
|   2009|   13|Brigham Young          |Utah                        |          26|          23|        3|
|   2009|   13|Alabama-Birmingham     |Central Florida             |          27|          34|       -7|
|   2009|   13|Connecticut            |Syracuse                    |          56|          31|       25|
|   2009|   13|East Carolina          |Southern Mississippi        |          25|          20|        5|
|   2009|   13|Florida                |Florida State               |          37|          10|       27|
|   2009|   13|Florida Atlantic       |Western Kentucky            |          29|          23|        6|
|   2009|   13|Georgia Tech           |Georgia                     |          24|          30|       -6|
|   2009|   13|Hawaii                 |Navy                        |          24|          17|        7|
|   2009|   13|Houston                |Rice                        |          73|          14|       59|
|   2009|   13|Louisiana State        |Arkansas                    |          33|          30|        3|
|   2009|   13|South Florida          |Miami (FL)                  |          10|          31|      -21|
|   2009|   13|Louisiana-Monroe       |Middle Tennessee State      |          19|          38|      -19|
|   2009|   13|Mississippi State      |Mississippi                 |          41|          27|       14|
|   2009|   13|Kansas                 |Missouri                    |          39|          41|       -2|
|   2009|   13|Nevada-Las Vegas       |San Diego State             |          28|          24|        4|
|   2009|   13|North Carolina State   |North Carolina              |          28|          27|        1|
|   2009|   13|Oklahoma               |Oklahoma State              |          27|           0|       27|
|   2009|   13|San Jose State         |New Mexico State            |          13|          10|        3|
|   2009|   13|South Carolina         |Clemson                     |          34|          17|       17|
|   2009|   13|Southern California    |UCLA                        |          28|           7|       21|
|   2009|   13|Southern Methodist     |Tulane                      |          26|          21|        5|
|   2009|   13|Stanford               |Notre Dame                  |          45|          38|        7|
|   2009|   13|Kentucky               |Tennessee                   |          24|          30|       -6|
|   2009|   13|Texas Christian        |New Mexico                  |          51|          10|       41|
|   2009|   13|Texas Tech             |Baylor                      |          20|          13|        7|
|   2009|   13|Texas-El Paso          |Marshall                    |          52|          21|       31|
|   2009|   13|Louisiana-Lafayette    |Troy                        |          31|          48|      -17|
|   2009|   13|Idaho                  |Utah State                  |          49|          52|       -3|
|   2009|   13|Virginia               |Virginia Tech               |          13|          42|      -29|
|   2009|   13|Duke                   |Wake Forest                 |          34|          45|      -11|
|   2009|   13|Washington             |Washington State            |          30|           0|       30|
|   2009|   14|Western Kentucky       |Arkansas State              |          20|          24|       -4|
|   2009|   14|Oregon                 |Oregon State                |          37|          33|        4|
|   2009|   14|Central Michigan       |Ohio                        |          20|          10|       10|
|   2009|   14|Alabama                |Florida                     |          32|          13|       19|
|   2009|   14|Southern California    |Arizona                     |          17|          21|       -4|
|   2009|   14|Boise State            |New Mexico State            |          42|           7|       35|
|   2009|   14|Pittsburgh             |Cincinnati                  |          44|          45|       -1|
|   2009|   14|Connecticut            |South Florida               |          29|          27|        2|
|   2009|   14|East Carolina          |Houston                     |          38|          32|        6|
|   2009|   14|Florida International  |Florida Atlantic            |          21|          28|       -7|
|   2009|   14|Illinois               |Fresno State                |          52|          53|       -1|
|   2009|   14|Georgia Tech           |Clemson                     |          39|          34|        5|
|   2009|   14|Louisiana Tech         |San Jose State              |          55|          20|       35|
|   2009|   14|Texas                  |Nebraska                    |          13|          12|        1|
|   2009|   14|Washington             |California                  |          42|          10|       32|
|   2009|   14|Rutgers                |West Virginia               |          21|          24|       -3|
|   2009|   14|Hawaii                 |Wisconsin                   |          10|          51|      -41|
|   2009|   15|Navy                   |Army                        |          17|           3|       14|
|   2009|   16|Rutgers                |Central Florida             |          45|          24|       21|
|   2009|   16|Wyoming                |Fresno State                |          35|          28|        7|
|   2009|   16|Middle Tennessee State |Southern Mississippi        |          42|          32|       10|
|   2009|   17|Brigham Young          |Oregon State                |          44|          20|       24|
|   2009|   17|Utah                   |California                  |          37|          27|       10|
|   2009|   17|Southern Methodist     |Nevada                      |          45|          10|       35|
|   2009|   17|Marshall               |Ohio                        |          21|          17|        4|
|   2009|   17|Pittsburgh             |North Carolina              |          19|          17|        2|
|   2009|   17|Southern California    |Boston College              |          24|          13|       11|
|   2009|   17|Clemson                |Kentucky                    |          21|          13|        8|
|   2009|   17|Georgia                |Texas A&M                   |          44|          20|       24|
|   2009|   18|UCLA                   |Temple                      |          30|          21|        9|
|   2009|   18|Wisconsin              |Miami (FL)                  |          20|          14|        6|
|   2009|   18|Idaho                  |Bowling Green State         |          43|          42|        1|
|   2009|   18|Nebraska               |Arizona                     |          33|           0|       33|
|   2009|   18|Air Force              |Houston                     |          47|          20|       27|
|   2009|   18|Iowa State             |Minnesota                   |          14|          13|        1|
|   2009|   18|Navy                   |Missouri                    |          35|          13|       22|
|   2009|   18|Oklahoma               |Stanford                    |          31|          27|        4|
|   2009|   18|Virginia Tech          |Tennessee                   |          37|          14|       23|
|   2009|   18|Auburn                 |Northwestern                |          38|          35|        3|
|   2009|   18|Florida                |Cincinnati                  |          51|          24|       27|
|   2009|   18|Florida State          |West Virginia               |          33|          21|       12|
|   2009|   18|Ohio State             |Oregon                      |          26|          17|        9|
|   2009|   18|Penn State             |Louisiana State             |          19|          17|        2|
|   2009|   18|Arkansas               |East Carolina               |          20|          17|        3|
|   2009|   18|Connecticut            |South Carolina              |          20|           7|       13|
|   2009|   18|Mississippi            |Oklahoma State              |          21|           7|       14|
|   2009|   18|South Florida          |Northern Illinois           |          27|           3|       24|
|   2009|   18|Texas Tech             |Michigan State              |          41|          31|       10|
|   2009|   19|Boise State            |Texas Christian             |          17|          10|        7|
|   2009|   20|Iowa                   |Georgia Tech                |          24|          14|       10|
|   2009|   20|Central Michigan       |Troy                        |          44|          41|        3|
|   2009|   20|Alabama                |Texas                       |          37|          21|       16|
|   2010|    1|Ball State             |Southeast Missouri State    |          27|          10|       17|
|   2010|    1|Buffalo                |Rhode Island                |          31|           0|       31|
|   2010|    1|Central Michigan       |Hampton                     |          33|           0|       33|
|   2010|    1|Alabama-Birmingham     |Florida Atlantic            |          31|          32|       -1|
|   2010|    1|Idaho                  |North Dakota                |          45|           0|       45|
|   2010|    1|Indiana                |Towson                      |          51|          17|       34|
|   2010|    1|Iowa State             |Northern Illinois           |          27|          10|       17|
|   2010|    1|Kent State             |Murray State                |          41|          10|       31|
|   2010|    1|Miami (FL)             |Florida A&M                 |          45|           0|       45|
|   2010|    1|Middle Tennessee State |Minnesota                   |          17|          24|       -7|
|   2010|    1|Nevada                 |Eastern Washington          |          49|          24|       25|
|   2010|    1|Ohio State             |Marshall                    |          45|           7|       38|
|   2010|    1|Rutgers                |Norfolk State               |          31|           0|       31|
|   2010|    1|South Carolina         |Southern Mississippi        |          41|          13|       28|
|   2010|    1|Hawaii                 |Southern California         |          36|          49|      -13|
|   2010|    1|Tulane                 |Southeastern Louisiana      |          27|          21|        6|
|   2010|    1|Utah                   |Pittsburgh                  |          27|          24|        3|
|   2010|    1|Wake Forest            |Presbyterian                |          53|          13|       40|
|   2010|    1|Toledo                 |Arizona                     |           2|          41|      -39|
|   2010|    1|Temple                 |Villanova                   |          31|          24|        7|
|   2010|    1|Air Force              |Northwestern State          |          65|          21|       44|
|   2010|    1|Alabama                |San Jose State              |          48|           3|       45|
|   2010|    1|Arizona State          |Portland State              |          54|           9|       45|
|   2010|    1|Arkansas               |Tennessee Tech              |          44|           3|       41|
|   2010|    1|Eastern Michigan       |Army                        |          27|          31|       -4|
|   2010|    1|Auburn                 |Arkansas State              |          52|          26|       26|
|   2010|    1|Baylor                 |Sam Houston State           |          34|           3|       31|
|   2010|    1|Boston College         |Weber State                 |          38|          20|       18|
|   2010|    1|Brigham Young          |Washington                  |          23|          17|        6|
|   2010|    1|California             |California-Davis            |          52|           3|       49|
|   2010|    1|Central Florida        |South Dakota                |          38|           7|       31|
|   2010|    1|Clemson                |North Texas                 |          35|          10|       25|
|   2010|    1|Colorado               |Colorado State              |          24|           3|       21|
|   2010|    1|Duke                   |Elon                        |          41|          27|       14|
|   2010|    1|Florida                |Miami (OH)                  |          34|          12|       22|
|   2010|    1|Florida State          |Samford                     |          59|           6|       53|
|   2010|    1|Fresno State           |Cincinnati                  |          28|          14|       14|
|   2010|    1|Georgia                |Louisiana-Lafayette         |          55|           7|       48|
|   2010|    1|Georgia Tech           |South Carolina State        |          41|          10|       31|
|   2010|    1|Houston                |Texas State                 |          68|          28|       40|
|   2010|    1|Iowa                   |Eastern Illinois            |          37|           7|       30|
|   2010|    1|Mississippi            |Jacksonville State          |          48|          49|       -1|
|   2010|    1|Kansas State           |UCLA                        |          31|          22|        9|
|   2010|    1|Louisville             |Kentucky                    |          16|          23|       -7|
|   2010|    1|Louisiana State        |North Carolina              |          30|          24|        6|
|   2010|    1|Louisiana Tech         |Grambling State             |          20|           6|       14|
|   2010|    1|Michigan               |Connecticut                 |          30|          10|       20|
|   2010|    1|Michigan State         |Western Michigan            |          38|          14|       24|
|   2010|    1|Mississippi State      |Memphis                     |          49|           7|       42|
|   2010|    1|Missouri               |Illinois                    |          23|          13|       10|
|   2010|    1|Nebraska               |Western Kentucky            |          49|          10|       39|
|   2010|    1|North Carolina State   |Western Carolina            |          48|           7|       41|
|   2010|    1|Kansas                 |North Dakota State          |           3|           6|       -3|
|   2010|    1|Vanderbilt             |Northwestern                |          21|          23|       -2|
|   2010|    1|Notre Dame             |Purdue                      |          23|          12|       11|
|   2010|    1|Ohio                   |Wofford                     |          33|          10|       23|
|   2010|    1|Oklahoma               |Utah State                  |          31|          24|        7|
|   2010|    1|Oklahoma State         |Washington State            |          65|          17|       48|
|   2010|    1|Oregon                 |New Mexico                  |          72|           0|       72|
|   2010|    1|Penn State             |Youngstown State            |          44|          14|       30|
|   2010|    1|San Diego State        |Nicholls State              |          47|           0|       47|
|   2010|    1|South Florida          |Stony Brook                 |          59|          14|       45|
|   2010|    1|Stanford               |Sacramento State            |          52|          17|       35|
|   2010|    1|Akron                  |Syracuse                    |           3|          29|      -26|
|   2010|    1|Tennessee              |Tennessee-Martin            |          50|           0|       50|
|   2010|    1|Rice                   |Texas                       |          17|          34|      -17|
|   2010|    1|Texas A&M              |Stephen F. Austin           |          48|           7|       41|
|   2010|    1|Texas Christian        |Oregon State                |          30|          21|        9|
|   2010|    1|Texas-El Paso          |Arkansas-Pine Bluff         |          31|          10|       21|
|   2010|    1|Troy                   |Bowling Green State         |          30|          27|        3|
|   2010|    1|Virginia               |Richmond                    |          34|          13|       21|
|   2010|    1|West Virginia          |Coastal Carolina            |          31|           0|       31|
|   2010|    1|Nevada-Las Vegas       |Wisconsin                   |          21|          41|      -20|
|   2010|    1|Wyoming                |Southern Utah               |          28|          20|        8|
|   2010|    1|East Carolina          |Tulsa                       |          51|          49|        2|
|   2010|    1|Texas Tech             |Southern Methodist          |          35|          27|        8|
|   2010|    1|Virginia Tech          |Boise State                 |          30|          33|       -3|
|   2010|    1|Maryland               |Navy                        |          17|          14|        3|
|   2010|    2|Mississippi State      |Auburn                      |          14|          17|       -3|
|   2010|    2|Temple                 |Central Michigan            |          13|          10|        3|
|   2010|    2|Houston                |Texas-El Paso               |          54|          24|       30|
|   2010|    2|Marshall               |West Virginia               |          21|          24|       -3|
|   2010|    2|Air Force              |Brigham Young               |          35|          14|       21|
|   2010|    2|Alabama                |Penn State                  |          24|           3|       21|
|   2010|    2|Arizona                |Citadel                     |          52|           6|       46|
|   2010|    2|Arizona State          |Northern Arizona            |          41|          20|       21|
|   2010|    2|Arkansas               |Louisiana-Monroe            |          31|           7|       24|
|   2010|    2|Baylor                 |Buffalo                     |          34|           6|       28|
|   2010|    2|Boston College         |Kent State                  |          26|          13|       13|
|   2010|    2|California             |Colorado                    |          52|           7|       45|
|   2010|    2|Cincinnati             |Indiana State               |          40|           7|       33|
|   2010|    2|Clemson                |Presbyterian                |          58|          21|       37|
|   2010|    2|Connecticut            |Texas Southern              |          62|           3|       59|
|   2010|    2|East Carolina          |Memphis                     |          49|          27|       22|
|   2010|    2|Florida                |South Florida               |          38|          14|       24|
|   2010|    2|Akron                  |Gardner-Webb                |          37|          38|       -1|
|   2010|    2|Army                   |Hawaii                      |          28|          31|       -3|
|   2010|    2|Illinois               |Southern Illinois           |          35|           3|       32|
|   2010|    2|Iowa                   |Iowa State                  |          35|           7|       28|
|   2010|    2|Virginia Tech          |James Madison               |          16|          21|       -5|
|   2010|    2|Kansas                 |Georgia Tech                |          28|          25|        3|
|   2010|    2|Kansas State           |Missouri State              |          48|          24|       24|
|   2010|    2|Kentucky               |Western Kentucky            |          63|          28|       35|
|   2010|    2|Ball State             |Liberty                     |          23|          27|       -4|
|   2010|    2|Vanderbilt             |Louisiana State             |           3|          27|      -24|
|   2010|    2|Louisiana-Lafayette    |Arkansas State              |          31|          24|        7|
|   2010|    2|Louisville             |Eastern Kentucky            |          23|          13|       10|
|   2010|    2|Maryland               |Morgan State                |          62|           3|       59|
|   2010|    2|Miami (OH)             |Eastern Michigan            |          28|          21|        7|
|   2010|    2|Notre Dame             |Michigan                    |          24|          28|       -4|
|   2010|    2|Michigan State         |Florida Atlantic            |          30|          17|       13|
|   2010|    2|Middle Tennessee State |Austin Peay                 |          56|          33|       23|
|   2010|    2|Tulane                 |Mississippi                 |          13|          27|      -14|
|   2010|    2|Missouri               |McNeese State               |          50|           6|       44|
|   2010|    2|Navy                   |Georgia Southern            |          13|           7|        6|
|   2010|    2|Nebraska               |Idaho                       |          38|          17|       21|
|   2010|    2|Nevada                 |Colorado State              |          51|           6|       45|
|   2010|    2|Central Florida        |North Carolina State        |          21|          28|       -7|
|   2010|    2|Northern Illinois      |North Dakota                |          23|          17|        6|
|   2010|    2|Northwestern           |Illinois State              |          37|           3|       34|
|   2010|    2|Ohio State             |Miami (FL)                  |          36|          24|       12|
|   2010|    2|Oklahoma               |Florida State               |          47|          17|       30|
|   2010|    2|Oklahoma State         |Troy                        |          41|          38|        3|
|   2010|    2|Tennessee              |Oregon                      |          13|          48|      -35|
|   2010|    2|Pittsburgh             |New Hampshire               |          38|          16|       22|
|   2010|    2|Purdue                 |Western Illinois            |          31|          21|       10|
|   2010|    2|North Texas            |Rice                        |          31|          32|       -1|
|   2010|    2|Florida International  |Rutgers                     |          14|          19|       -5|
|   2010|    2|New Mexico State       |San Diego State             |          21|          41|      -20|
|   2010|    2|South Carolina         |Georgia                     |          17|           6|       11|
|   2010|    2|Minnesota              |South Dakota                |          38|          41|       -3|
|   2010|    2|Southern California    |Virginia                    |          17|          14|        3|
|   2010|    2|Southern Methodist     |Alabama-Birmingham          |          28|           7|       21|
|   2010|    2|Southern Mississippi   |Prairie View A&M            |          34|           7|       27|
|   2010|    2|UCLA                   |Stanford                    |           0|          35|      -35|
|   2010|    2|Texas                  |Wyoming                     |          34|           7|       27|
|   2010|    2|Texas A&M              |Louisiana Tech              |          48|          16|       32|
|   2010|    2|Texas Christian        |Tennessee Tech              |          62|           7|       55|
|   2010|    2|New Mexico             |Texas Tech                  |          17|          52|      -35|
|   2010|    2|Ohio                   |Toledo                      |          13|          20|       -7|
|   2010|    2|Tulsa                  |Bowling Green State         |          33|          20|       13|
|   2010|    2|Utah                   |Nevada-Las Vegas            |          38|          10|       28|
|   2010|    2|Utah State             |Idaho State                 |          38|          17|       21|
|   2010|    2|Wake Forest            |Duke                        |          54|          48|        6|
|   2010|    2|Washington             |Syracuse                    |          41|          20|       21|
|   2010|    2|Washington State       |Montana State               |          23|          22|        1|
|   2010|    2|Western Michigan       |Nicholls State              |          49|          14|       35|
|   2010|    2|Wisconsin              |San Jose State              |          27|          14|       13|
|   2010|    3|North Carolina State   |Cincinnati                  |          30|          19|       11|
|   2010|    3|Nevada                 |California                  |          52|          31|       21|
|   2010|    3|Southern Mississippi   |Kansas                      |          31|          16|       15|
|   2010|    3|Duke                   |Alabama                     |          13|          62|      -49|
|   2010|    3|Alabama-Birmingham     |Troy                        |          34|          33|        1|
|   2010|    3|Arizona                |Iowa                        |          34|          27|        7|
|   2010|    3|Georgia                |Arkansas                    |          24|          31|       -7|
|   2010|    3|Arkansas State         |Louisiana-Monroe            |          34|          20|       14|
|   2010|    3|Army                   |North Texas                 |          24|           0|       24|
|   2010|    3|Auburn                 |Clemson                     |          27|          24|        3|
|   2010|    3|Wyoming                |Boise State                 |           6|          51|      -45|
|   2010|    3|Bowling Green State    |Marshall                    |          44|          28|       16|
|   2010|    3|Buffalo                |Central Florida             |          10|          24|      -14|
|   2010|    3|Eastern Michigan       |Central Michigan            |          14|          52|      -38|
|   2010|    3|Colorado               |Hawaii                      |          31|          13|       18|
|   2010|    3|Tennessee              |Florida                     |          17|          31|      -14|
|   2010|    3|Florida State          |Brigham Young               |          34|          10|       24|
|   2010|    3|Utah State             |Fresno State                |          24|          41|      -17|
|   2010|    3|North Carolina         |Georgia Tech                |          24|          30|       -6|
|   2010|    3|Idaho                  |Nevada-Las Vegas            |          30|           7|       23|
|   2010|    3|Illinois               |Northern Illinois           |          28|          22|        6|
|   2010|    3|Western Kentucky       |Indiana                     |          21|          38|      -17|
|   2010|    3|Kansas State           |Iowa State                  |          27|          20|        7|
|   2010|    3|Kentucky               |Akron                       |          47|          10|       37|
|   2010|    3|Louisiana State        |Mississippi State           |          29|           7|       22|
|   2010|    3|Memphis                |Middle Tennessee State      |          24|          17|        7|
|   2010|    3|Miami (OH)             |Colorado State              |          31|          10|       21|
|   2010|    3|Michigan               |Massachusetts               |          42|          37|        5|
|   2010|    3|Michigan State         |Notre Dame                  |          34|          31|        3|
|   2010|    3|Missouri               |San Diego State             |          27|          24|        3|
|   2010|    3|Louisiana Tech         |Navy                        |          23|          37|      -14|
|   2010|    3|Washington             |Nebraska                    |          21|          56|      -35|
|   2010|    3|Rice                   |Northwestern                |          13|          30|      -17|
|   2010|    3|Ohio State             |Ohio                        |          43|           7|       36|
|   2010|    3|Oklahoma               |Air Force                   |          27|          24|        3|
|   2010|    3|Oklahoma State         |Tulsa                       |          65|          28|       37|
|   2010|    3|Oregon                 |Portland State              |          69|           0|       69|
|   2010|    3|Oregon State           |Louisville                  |          35|          28|        7|
|   2010|    3|Penn State             |Kent State                  |          24|           0|       24|
|   2010|    3|Purdue                 |Ball State                  |          24|          13|       11|
|   2010|    3|San Jose State         |Southern Utah               |          16|          11|        5|
|   2010|    3|South Carolina         |Furman                      |          38|          19|       19|
|   2010|    3|Minnesota              |Southern California         |          21|          32|      -11|
|   2010|    3|Southern Methodist     |Washington State            |          35|          21|       14|
|   2010|    3|Stanford               |Wake Forest                 |          68|          24|       44|
|   2010|    3|Syracuse               |Maine                       |          38|          14|       24|
|   2010|    3|Temple                 |Connecticut                 |          30|          16|       14|
|   2010|    3|Texas Tech             |Texas                       |          14|          24|      -10|
|   2010|    3|Texas A&M              |Florida International       |          27|          20|        7|
|   2010|    3|Texas Christian        |Baylor                      |          45|          10|       35|
|   2010|    3|Texas-El Paso          |New Mexico State            |          42|          10|       32|
|   2010|    3|Western Michigan       |Toledo                      |          24|          37|      -13|
|   2010|    3|UCLA                   |Houston                     |          31|          13|       18|
|   2010|    3|New Mexico             |Utah                        |          14|          56|      -42|
|   2010|    3|Mississippi            |Vanderbilt                  |          14|          28|      -14|
|   2010|    3|Virginia Tech          |East Carolina               |          49|          27|       22|
|   2010|    3|West Virginia          |Maryland                    |          31|          17|       14|
|   2010|    3|Wisconsin              |Arizona State               |          20|          19|        1|
|   2010|    4|Pittsburgh             |Miami (FL)                  |           3|          31|      -28|
|   2010|    4|Southern Methodist     |Texas Christian             |          24|          41|      -17|
|   2010|    4|Wyoming                |Air Force                   |          14|          20|       -6|
|   2010|    4|Arkansas               |Alabama                     |          20|          24|       -4|
|   2010|    4|Arizona                |California                  |          10|           9|        1|
|   2010|    4|Duke                   |Army                        |          21|          35|      -14|
|   2010|    4|Auburn                 |South Carolina              |          35|          27|        8|
|   2010|    4|Rice                   |Baylor                      |          13|          30|      -17|
|   2010|    4|Boise State            |Oregon State                |          37|          24|       13|
|   2010|    4|Colorado State         |Idaho                       |          36|          34|        2|
|   2010|    4|Connecticut            |Buffalo                     |          45|          21|       24|
|   2010|    4|Florida                |Kentucky                    |          48|          14|       34|
|   2010|    4|Florida State          |Wake Forest                 |          31|           0|       31|
|   2010|    4|Hawaii                 |Charleston Southern         |          66|           7|       59|
|   2010|    4|Houston                |Tulane                      |          42|          23|       19|
|   2010|    4|Indiana                |Akron                       |          35|          20|       15|
|   2010|    4|Iowa                   |Ball State                  |          45|           0|       45|
|   2010|    4|Iowa State             |Northern Iowa               |          27|           0|       27|
|   2010|    4|Kansas                 |New Mexico State            |          42|          16|       26|
|   2010|    4|Kansas State           |Central Florida             |          17|          13|        4|
|   2010|    4|Louisiana State        |West Virginia               |          20|          14|        6|
|   2010|    4|Louisiana-Monroe       |Southeastern Louisiana      |          21|          20|        1|
|   2010|    4|Marshall               |Ohio                        |          24|          23|        1|
|   2010|    4|Maryland               |Florida International       |          42|          28|       14|
|   2010|    4|Michigan               |Bowling Green State         |          65|          21|       44|
|   2010|    4|Michigan State         |Northern Colorado           |          45|           7|       38|
|   2010|    4|Louisiana-Lafayette    |Middle Tennessee State      |          14|          34|      -20|
|   2010|    4|Mississippi            |Fresno State                |          55|          38|       17|
|   2010|    4|Mississippi State      |Georgia                     |          24|          12|       12|
|   2010|    4|Missouri               |Miami (OH)                  |          51|          13|       38|
|   2010|    4|Nebraska               |South Dakota State          |          17|           3|       14|
|   2010|    4|Brigham Young          |Nevada                      |          13|          27|      -14|
|   2010|    4|Nevada-Las Vegas       |New Mexico                  |          45|          10|       35|
|   2010|    4|Rutgers                |North Carolina              |          13|          17|       -4|
|   2010|    4|Georgia Tech           |North Carolina State        |          28|          45|      -17|
|   2010|    4|Florida Atlantic       |North Texas                 |          17|          21|       -4|
|   2010|    4|Minnesota              |Northern Illinois           |          23|          34|      -11|
|   2010|    4|Northwestern           |Central Michigan            |          30|          25|        5|
|   2010|    4|Ohio State             |Eastern Michigan            |          73|          20|       53|
|   2010|    4|Cincinnati             |Oklahoma                    |          29|          31|       -2|
|   2010|    4|Arizona State          |Oregon                      |          31|          42|      -11|
|   2010|    4|Penn State             |Temple                      |          22|          13|        9|
|   2010|    4|San Diego State        |Utah State                  |          41|           7|       34|
|   2010|    4|South Florida          |Western Kentucky            |          24|          12|       12|
|   2010|    4|Washington State       |Southern California         |          16|          50|      -34|
|   2010|    4|Louisiana Tech         |Southern Mississippi        |          12|          13|       -1|
|   2010|    4|Notre Dame             |Stanford                    |          14|          37|      -23|
|   2010|    4|Syracuse               |Colgate                     |          42|           7|       35|
|   2010|    4|Tennessee              |Alabama-Birmingham          |          32|          29|        3|
|   2010|    4|Texas-El Paso          |Memphis                     |          16|          13|        3|
|   2010|    4|Purdue                 |Toledo                      |          20|          31|      -11|
|   2010|    4|Troy                   |Arkansas State              |          35|          28|        7|
|   2010|    4|Tulsa                  |Central Arkansas            |          41|          14|       27|
|   2010|    4|Texas                  |UCLA                        |          12|          34|      -22|
|   2010|    4|Utah                   |San Jose State              |          56|           3|       53|
|   2010|    4|Virginia               |Virginia Military Institute |          48|           7|       41|
|   2010|    4|Boston College         |Virginia Tech               |           0|          19|      -19|
|   2010|    4|Wisconsin              |Austin Peay                 |          70|           3|       67|
|   2010|    5|Oklahoma State         |Texas A&M                   |          38|          35|        3|
|   2010|    5|Utah State             |Brigham Young               |          31|          16|       15|
|   2010|    5|Air Force              |Navy                        |          14|           6|        8|
|   2010|    5|Alabama                |Florida                     |          31|           6|       25|
|   2010|    5|Auburn                 |Louisiana-Monroe            |          52|           3|       49|
|   2010|    5|Central Michigan       |Ball State                  |          17|          31|      -14|
|   2010|    5|Baylor                 |Kansas                      |          55|           7|       48|
|   2010|    5|New Mexico State       |Boise State                 |           0|          59|      -59|
|   2010|    5|Bowling Green State    |Buffalo                     |          26|          28|       -2|
|   2010|    5|San Jose State         |California-Davis            |          13|          14|       -1|
|   2010|    5|Colorado               |Georgia                     |          29|          27|        2|
|   2010|    5|Connecticut            |Vanderbilt                  |          40|          21|       19|
|   2010|    5|Virginia               |Florida State               |          14|          34|      -20|
|   2010|    5|Fresno State           |Cal Poly                    |          38|          17|       21|
|   2010|    5|Wake Forest            |Georgia Tech                |          20|          24|       -4|
|   2010|    5|Hawaii                 |Louisiana Tech              |          41|          21|       20|
|   2010|    5|Western Michigan       |Idaho                       |          13|          33|      -20|
|   2010|    5|Iowa                   |Penn State                  |          24|           3|       21|
|   2010|    5|Iowa State             |Texas Tech                  |          52|          38|       14|
|   2010|    5|Louisiana State        |Tennessee                   |          16|          14|        2|
|   2010|    5|North Texas            |Louisiana-Lafayette         |          27|          28|       -1|
|   2010|    5|Arkansas State         |Louisville                  |          24|          34|      -10|
|   2010|    5|Maryland               |Duke                        |          21|          16|        5|
|   2010|    5|Clemson                |Miami (FL)                  |          21|          30|       -9|
|   2010|    5|Miami (OH)             |Kent State                  |          27|          21|        6|
|   2010|    5|Indiana                |Michigan                    |          35|          42|       -7|
|   2010|    5|Michigan State         |Wisconsin                   |          34|          24|       10|
|   2010|    5|Mississippi            |Kentucky                    |          42|          35|        7|
|   2010|    5|Mississippi State      |Alcorn State                |          49|          16|       33|
|   2010|    5|Nevada-Las Vegas       |Nevada                      |          26|          44|      -18|
|   2010|    5|North Carolina         |East Carolina               |          42|          17|       25|
|   2010|    5|Akron                  |Northern Illinois           |          14|          50|      -36|
|   2010|    5|Minnesota              |Northwestern                |          28|          29|       -1|
|   2010|    5|Boston College         |Notre Dame                  |          13|          31|      -18|
|   2010|    5|Eastern Michigan       |Ohio                        |          17|          30|      -13|
|   2010|    5|Illinois               |Ohio State                  |          13|          24|      -11|
|   2010|    5|Oklahoma               |Texas                       |          28|          20|        8|
|   2010|    5|Oregon                 |Stanford                    |          52|          31|       21|
|   2010|    5|Oregon State           |Arizona State               |          31|          28|        3|
|   2010|    5|Pittsburgh             |Florida International       |          44|          17|       27|
|   2010|    5|South Florida          |Florida Atlantic            |          31|           3|       28|
|   2010|    5|Rice                   |Southern Methodist          |          31|          42|      -11|
|   2010|    5|Southern Mississippi   |Marshall                    |          41|          16|       25|
|   2010|    5|Army                   |Temple                      |          35|          42|       -7|
|   2010|    5|Colorado State         |Texas Christian             |           0|          27|      -27|
|   2010|    5|New Mexico             |Texas-El Paso               |          20|          38|      -18|
|   2010|    5|Rutgers                |Tulane                      |          14|          17|       -3|
|   2010|    5|Memphis                |Tulsa                       |           7|          48|      -41|
|   2010|    5|UCLA                   |Washington State            |          42|          28|       14|
|   2010|    5|North Carolina State   |Virginia Tech               |          30|          41|      -11|
|   2010|    5|Southern California    |Washington                  |          31|          32|       -1|
|   2010|    5|Toledo                 |Wyoming                     |          15|          20|       -5|
|   2010|    6|Middle Tennessee State |Troy                        |          13|          42|      -29|
|   2010|    6|Central Florida        |Alabama-Birmingham          |          42|           7|       35|
|   2010|    6|Kansas State           |Nebraska                    |          13|          48|      -35|
|   2010|    6|Louisiana-Lafayette    |Oklahoma State              |          28|          54|      -26|
|   2010|    6|Rutgers                |Connecticut                 |          27|          24|        3|
|   2010|    6|Air Force              |Colorado State              |          49|          27|       22|
|   2010|    6|Washington             |Arizona State               |          14|          24|      -10|
|   2010|    6|Texas A&M              |Arkansas                    |          17|          24|       -7|
|   2010|    6|North Texas            |Arkansas State              |          19|          24|       -5|
|   2010|    6|Tulane                 |Army                        |          23|          41|      -18|
|   2010|    6|Kentucky               |Auburn                      |          34|          37|       -3|
|   2010|    6|Boise State            |Toledo                      |          57|          14|       43|
|   2010|    6|Brigham Young          |San Diego State             |          24|          21|        3|
|   2010|    6|California             |UCLA                        |          35|           7|       28|
|   2010|    6|Cincinnati             |Miami (OH)                  |          45|           3|       42|
|   2010|    6|Southern Mississippi   |East Carolina               |          43|          44|       -1|
|   2010|    6|Florida International  |Western Kentucky            |          28|          21|        7|
|   2010|    6|Miami (FL)             |Florida State               |          17|          45|      -28|
|   2010|    6|Georgia                |Tennessee                   |          41|          14|       27|
|   2010|    6|Georgia Tech           |Virginia                    |          33|          21|       12|
|   2010|    6|Fresno State           |Hawaii                      |          27|          49|      -22|
|   2010|    6|Penn State             |Illinois                    |          13|          33|      -20|
|   2010|    6|Kent State             |Akron                       |          28|          17|       11|
|   2010|    6|Florida                |Louisiana State             |          29|          33|       -4|
|   2010|    6|Louisiana Tech         |Utah State                  |          24|           6|       18|
|   2010|    6|Louisiana-Monroe       |Florida Atlantic            |          20|          17|        3|
|   2010|    6|Louisville             |Memphis                     |          56|           0|       56|
|   2010|    6|Michigan               |Michigan State              |          17|          34|      -17|
|   2010|    6|Houston                |Mississippi State           |          24|          47|      -23|
|   2010|    6|Missouri               |Colorado                    |          26|           0|       26|
|   2010|    6|Wake Forest            |Navy                        |          27|          28|       -1|
|   2010|    6|Nevada                 |San Jose State              |          35|          13|       22|
|   2010|    6|New Mexico State       |New Mexico                  |          16|          14|        2|
|   2010|    6|North Carolina         |Clemson                     |          21|          16|        5|
|   2010|    6|North Carolina State   |Boston College              |          44|          17|       27|
|   2010|    6|Northern Illinois      |Temple                      |          31|          17|       14|
|   2010|    6|Notre Dame             |Pittsburgh                  |          23|          17|        6|
|   2010|    6|Ohio                   |Bowling Green State         |          49|          25|       24|
|   2010|    6|Ohio State             |Indiana                     |          38|          10|       28|
|   2010|    6|Washington State       |Oregon                      |          23|          43|      -20|
|   2010|    6|Arizona                |Oregon State                |          27|          29|       -2|
|   2010|    6|Northwestern           |Purdue                      |          17|          20|       -3|
|   2010|    6|South Carolina         |Alabama                     |          35|          21|       14|
|   2010|    6|Southern Methodist     |Tulsa                       |          21|          18|        3|
|   2010|    6|Stanford               |Southern California         |          37|          35|        2|
|   2010|    6|South Florida          |Syracuse                    |           9|          13|       -4|
|   2010|    6|Texas Christian        |Wyoming                     |          45|           0|       45|
|   2010|    6|Texas Tech             |Baylor                      |          45|          38|        7|
|   2010|    6|Texas-El Paso          |Rice                        |          44|          24|       20|
|   2010|    6|Iowa State             |Utah                        |          27|          68|      -41|
|   2010|    6|Vanderbilt             |Eastern Michigan            |          52|           6|       46|
|   2010|    6|Virginia Tech          |Central Michigan            |          45|          21|       24|
|   2010|    6|West Virginia          |Nevada-Las Vegas            |          49|          10|       39|
|   2010|    6|Ball State             |Western Michigan            |          16|          45|      -29|
|   2010|    6|Wisconsin              |Minnesota                   |          41|          23|       18|
|   2010|    7|Marshall               |Central Florida             |          14|          35|      -21|
|   2010|    7|Kansas                 |Kansas State                |           7|          59|      -52|
|   2010|    7|West Virginia          |South Florida               |          20|           6|       14|
|   2010|    7|Louisville             |Cincinnati                  |          27|          35|       -8|
|   2010|    7|Alabama                |Mississippi                 |          23|          10|       13|
|   2010|    7|Alabama-Birmingham     |Texas-El Paso               |          21|           6|       15|
|   2010|    7|Washington State       |Arizona                     |           7|          24|      -17|
|   2010|    7|Auburn                 |Arkansas                    |          65|          43|       22|
|   2010|    7|Colorado               |Baylor                      |          25|          31|       -6|
|   2010|    7|San Jose State         |Boise State                 |           0|          48|      -48|
|   2010|    7|Clemson                |Maryland                    |          31|           7|       24|
|   2010|    7|Colorado State         |Nevada-Las Vegas            |          43|          10|       33|
|   2010|    7|East Carolina          |North Carolina State        |          33|          27|        6|
|   2010|    7|Ball State             |Eastern Michigan            |          38|          41|       -3|
|   2010|    7|North Texas            |Florida International       |          10|          34|      -24|
|   2010|    7|Florida State          |Boston College              |          24|          19|        5|
|   2010|    7|Fresno State           |New Mexico State            |          33|          10|       23|
|   2010|    7|Georgia                |Vanderbilt                  |          43|           0|       43|
|   2010|    7|Georgia Tech           |Middle Tennessee State      |          42|          14|       28|
|   2010|    7|Hawaii                 |Nevada                      |          27|          21|        6|
|   2010|    7|Indiana                |Arkansas State              |          36|          34|        2|
|   2010|    7|Michigan               |Iowa                        |          28|          38|      -10|
|   2010|    7|Kentucky               |South Carolina              |          31|          28|        3|
|   2010|    7|Louisiana State        |McNeese State               |          32|          10|       22|
|   2010|    7|Louisiana Tech         |Idaho                       |          48|          35|       13|
|   2010|    7|Western Kentucky       |Louisiana-Monroe            |          30|          35|       -5|
|   2010|    7|Duke                   |Miami (FL)                  |          13|          28|      -15|
|   2010|    7|Central Michigan       |Miami (OH)                  |          20|          27|       -7|
|   2010|    7|Michigan State         |Illinois                    |          26|           6|       20|
|   2010|    7|Florida                |Mississippi State           |           7|          10|       -3|
|   2010|    7|Texas A&M              |Missouri                    |           9|          30|      -21|
|   2010|    7|Navy                   |Southern Methodist          |          28|          21|        7|
|   2010|    7|Virginia               |North Carolina              |          10|          44|      -34|
|   2010|    7|Northern Illinois      |Buffalo                     |          45|          14|       31|
|   2010|    7|Notre Dame             |Western Michigan            |          44|          20|       24|
|   2010|    7|Ohio                   |Akron                       |          38|          10|       28|
|   2010|    7|Oklahoma               |Iowa State                  |          52|           0|       52|
|   2010|    7|Texas Tech             |Oklahoma State              |          17|          34|      -17|
|   2010|    7|Syracuse               |Pittsburgh                  |          14|          45|      -31|
|   2010|    7|Purdue                 |Minnesota                   |          28|          17|       11|
|   2010|    7|Rice                   |Houston                     |          34|          31|        3|
|   2010|    7|Rutgers                |Army                        |          23|          20|        3|
|   2010|    7|San Diego State        |Air Force                   |          27|          25|        2|
|   2010|    7|Southern California    |California                  |          48|          14|       34|
|   2010|    7|Memphis                |Southern Mississippi        |          19|          41|      -22|
|   2010|    7|Temple                 |Bowling Green State         |          28|          27|        1|
|   2010|    7|Nebraska               |Texas                       |          13|          20|       -7|
|   2010|    7|Texas Christian        |Brigham Young               |          31|           3|       28|
|   2010|    7|Toledo                 |Kent State                  |          34|          21|       13|
|   2010|    7|Troy                   |Louisiana-Lafayette         |          31|          24|        7|
|   2010|    7|Tulsa                  |Tulane                      |          52|          24|       28|
|   2010|    7|Wyoming                |Utah                        |           6|          30|      -24|
|   2010|    7|Virginia Tech          |Wake Forest                 |          52|          21|       31|
|   2010|    7|Washington             |Oregon State                |          35|          34|        1|
|   2010|    7|Wisconsin              |Ohio State                  |          31|          18|       13|
|   2010|    8|Oregon                 |UCLA                        |          60|          13|       47|
|   2010|    8|Cincinnati             |South Florida               |          30|          38|       -8|
|   2010|    8|Tennessee              |Alabama                     |          10|          41|      -31|
|   2010|    8|Arizona                |Washington                  |          44|          14|       30|
|   2010|    8|Arkansas               |Mississippi                 |          38|          24|       14|
|   2010|    8|Arkansas State         |Florida Atlantic            |          37|          16|       21|
|   2010|    8|Auburn                 |Louisiana State             |          24|          17|        7|
|   2010|    8|Baylor                 |Kansas State                |          47|          42|        5|
|   2010|    8|Brigham Young          |Wyoming                     |          25|          20|        5|
|   2010|    8|California             |Arizona State               |          50|          17|       33|
|   2010|    8|Central Florida        |Rice                        |          41|          14|       27|
|   2010|    8|Clemson                |Georgia Tech                |          27|          13|       14|
|   2010|    8|East Carolina          |Marshall                    |          37|          10|       27|
|   2010|    8|San Jose State         |Fresno State                |          18|          33|      -15|
|   2010|    8|Kentucky               |Georgia                     |          31|          44|      -13|
|   2010|    8|Utah State             |Hawaii                      |           7|          45|      -38|
|   2010|    8|Southern Methodist     |Houston                     |          20|          45|      -25|
|   2010|    8|Idaho                  |New Mexico State            |          37|          14|       23|
|   2010|    8|Illinois               |Indiana                     |          43|          13|       30|
|   2010|    8|Texas                  |Iowa State                  |          21|          28|       -7|
|   2010|    8|Bowling Green State    |Kent State                  |           6|          30|      -24|
|   2010|    8|Louisville             |Connecticut                 |          26|           0|       26|
|   2010|    8|Boston College         |Maryland                    |          21|          24|       -3|
|   2010|    8|Miami (FL)             |North Carolina              |          33|          10|       23|
|   2010|    8|Northwestern           |Michigan State              |          27|          35|       -8|
|   2010|    8|Middle Tennessee State |Louisiana-Monroe            |          38|          10|       28|
|   2010|    8|Mississippi State      |Alabama-Birmingham          |          29|          24|        5|
|   2010|    8|Missouri               |Oklahoma                    |          36|          27|        9|
|   2010|    8|Navy                   |Notre Dame                  |          35|          17|       18|
|   2010|    8|Oklahoma State         |Nebraska                    |          41|          51|      -10|
|   2010|    8|Northern Illinois      |Central Michigan            |          33|           7|       26|
|   2010|    8|Miami (OH)             |Ohio                        |          13|          34|      -21|
|   2010|    8|Ohio State             |Purdue                      |          49|           0|       49|
|   2010|    8|Minnesota              |Penn State                  |          21|          33|      -12|
|   2010|    8|Pittsburgh             |Rutgers                     |          41|          21|       20|
|   2010|    8|New Mexico             |San Diego State             |          20|          30|      -10|
|   2010|    8|Vanderbilt             |South Carolina              |           7|          21|      -14|
|   2010|    8|Stanford               |Washington State            |          38|          28|       10|
|   2010|    8|West Virginia          |Syracuse                    |          14|          19|       -5|
|   2010|    8|Buffalo                |Temple                      |           0|          42|      -42|
|   2010|    8|Kansas                 |Texas A&M                   |          10|          45|      -35|
|   2010|    8|Texas Christian        |Air Force                   |          38|           7|       31|
|   2010|    8|Colorado               |Texas Tech                  |          24|          27|       -3|
|   2010|    8|Toledo                 |Ball State                  |          31|          24|        7|
|   2010|    8|Texas-El Paso          |Tulane                      |          24|          34|      -10|
|   2010|    8|Utah                   |Colorado State              |          59|           6|       53|
|   2010|    8|Virginia               |Eastern Michigan            |          48|          21|       27|
|   2010|    8|Virginia Tech          |Duke                        |          44|           7|       37|
|   2010|    8|Louisiana-Lafayette    |Western Kentucky            |          21|          54|      -33|
|   2010|    8|Akron                  |Western Michigan            |          10|          56|      -46|
|   2010|    8|Iowa                   |Wisconsin                   |          30|          31|       -1|
|   2010|    9|Boise State            |Louisiana Tech              |          49|          20|       29|
|   2010|    9|North Carolina State   |Florida State               |          28|          24|        4|
|   2010|    9|Connecticut            |West Virginia               |          16|          13|        3|
|   2010|    9|Southern Mississippi   |Alabama-Birmingham          |          49|          50|       -1|
|   2010|    9|UCLA                   |Arizona                     |          21|          29|       -8|
|   2010|    9|Arizona State          |Washington State            |          42|           0|       42|
|   2010|    9|Arkansas               |Vanderbilt                  |          49|          14|       35|
|   2010|    9|Army                   |Virginia Military Institute |          29|           7|       22|
|   2010|    9|Mississippi            |Auburn                      |          31|          51|      -20|
|   2010|    9|Texas                  |Baylor                      |          22|          30|       -8|
|   2010|    9|Boston College         |Clemson                     |          16|          10|        6|
|   2010|    9|Central Michigan       |Bowling Green State         |          14|          17|       -3|
|   2010|    9|Central Florida        |East Carolina               |          49|          35|       14|
|   2010|    9|Colorado State         |New Mexico                  |          38|          14|       24|
|   2010|    9|Navy                   |Duke                        |          31|          34|       -3|
|   2010|    9|Florida                |Georgia                     |          34|          31|        3|
|   2010|    9|Florida Atlantic       |Florida International       |          21|           9|       12|
|   2010|    9|Hawaii                 |Idaho                       |          45|          10|       35|
|   2010|    9|Memphis                |Houston                     |          17|          56|      -39|
|   2010|    9|Illinois               |Purdue                      |          44|          10|       34|
|   2010|    9|Iowa                   |Michigan State              |          37|           6|       31|
|   2010|    9|Iowa State             |Kansas                      |          28|          16|       12|
|   2010|    9|Kent State             |Ball State                  |          33|          14|       19|
|   2010|    9|Louisiana-Monroe       |Troy                        |          28|          14|       14|
|   2010|    9|Marshall               |Texas-El Paso               |          16|          12|        4|
|   2010|    9|Maryland               |Wake Forest                 |          62|          14|       48|
|   2010|    9|Buffalo                |Miami (OH)                  |           9|          21|      -12|
|   2010|    9|Mississippi State      |Kentucky                    |          24|          17|        7|
|   2010|    9|Nebraska               |Missouri                    |          31|          17|       14|
|   2010|    9|Nevada                 |Utah State                  |          56|          42|       14|
|   2010|    9|New Mexico State       |San Jose State              |          29|          27|        2|
|   2010|    9|North Carolina         |William & Mary              |          21|          17|        4|
|   2010|    9|Western Kentucky       |North Texas                 |           6|          33|      -27|
|   2010|    9|Western Michigan       |Northern Illinois           |          21|          28|       -7|
|   2010|    9|Indiana                |Northwestern                |          17|          20|       -3|
|   2010|    9|Ohio                   |Louisiana-Lafayette         |          38|          31|        7|
|   2010|    9|Minnesota              |Ohio State                  |          10|          52|      -42|
|   2010|    9|Oklahoma               |Colorado                    |          43|          10|       33|
|   2010|    9|Kansas State           |Oklahoma State              |          14|          24|      -10|
|   2010|    9|Southern California    |Oregon                      |          32|          53|      -21|
|   2010|    9|Oregon State           |California                  |          35|           7|       28|
|   2010|    9|Penn State             |Michigan                    |          41|          31|       10|
|   2010|    9|Pittsburgh             |Louisville                  |          20|           3|       17|
|   2010|    9|Wyoming                |San Diego State             |          38|          48|      -10|
|   2010|    9|South Carolina         |Tennessee                   |          38|          24|       14|
|   2010|    9|Tulane                 |Southern Methodist          |          17|          31|      -14|
|   2010|    9|Washington             |Stanford                    |           0|          41|      -41|
|   2010|    9|Cincinnati             |Syracuse                    |           7|          31|      -24|
|   2010|    9|Temple                 |Akron                       |          30|           0|       30|
|   2010|    9|Texas A&M              |Texas Tech                  |          45|          27|       18|
|   2010|    9|Nevada-Las Vegas       |Texas Christian             |           6|          48|      -42|
|   2010|    9|Eastern Michigan       |Toledo                      |           7|          42|      -35|
|   2010|    9|Notre Dame             |Tulsa                       |          27|          28|       -1|
|   2010|    9|Air Force              |Utah                        |          23|          28|       -5|
|   2010|    9|Virginia               |Miami (FL)                  |          24|          19|        5|
|   2010|   10|Arkansas State         |Middle Tennessee State      |          51|          24|       27|
|   2010|   10|South Florida          |Rutgers                     |          28|          27|        1|
|   2010|   10|Ohio                   |Buffalo                     |          34|          17|       17|
|   2010|   10|Virginia Tech          |Georgia Tech                |          28|          21|        7|
|   2010|   10|Houston                |Central Florida             |          33|          40|       -7|
|   2010|   10|Central Michigan       |Western Michigan            |          26|          22|        4|
|   2010|   10|Army                   |Air Force                   |          22|          42|      -20|
|   2010|   10|South Carolina         |Arkansas                    |          20|          41|      -21|
|   2010|   10|Auburn                 |Chattanooga                 |          62|          24|       38|
|   2010|   10|Ball State             |Akron                       |          37|          30|        7|
|   2010|   10|Boise State            |Hawaii                      |          42|           7|       35|
|   2010|   10|Wake Forest            |Boston College              |          13|          23|      -10|
|   2010|   10|Brigham Young          |Nevada-Las Vegas            |          55|           7|       48|
|   2010|   10|Washington State       |California                  |          13|          20|       -7|
|   2010|   10|Clemson                |North Carolina State        |          14|          13|        1|
|   2010|   10|Duke                   |Virginia                    |          55|          48|        7|
|   2010|   10|Vanderbilt             |Florida                     |          14|          55|      -41|
|   2010|   10|Western Kentucky       |Florida Atlantic            |          16|          17|       -1|
|   2010|   10|Florida International  |Louisiana-Monroe            |          42|          35|        7|
|   2010|   10|Louisiana Tech         |Fresno State                |          34|          40|       -6|
|   2010|   10|Georgia                |Idaho State                 |          55|           7|       48|
|   2010|   10|Indiana                |Iowa                        |          13|          18|       -5|
|   2010|   10|Kansas                 |Colorado                    |          52|          45|        7|
|   2010|   10|Kansas State           |Texas                       |          39|          14|       25|
|   2010|   10|Kentucky               |Charleston Southern         |          49|          21|       28|
|   2010|   10|Louisiana State        |Alabama                     |          24|          21|        3|
|   2010|   10|Syracuse               |Louisville                  |          20|          28|       -8|
|   2010|   10|Alabama-Birmingham     |Marshall                    |          17|          31|      -14|
|   2010|   10|Miami (FL)             |Maryland                    |          26|          20|        6|
|   2010|   10|Michigan               |Illinois                    |          67|          65|        2|
|   2010|   10|Michigan State         |Minnesota                   |          31|           8|       23|
|   2010|   10|Mississippi            |Louisiana-Lafayette         |          43|          21|       22|
|   2010|   10|East Carolina          |Navy                        |          35|          76|      -41|
|   2010|   10|Iowa State             |Nebraska                    |          30|          31|       -1|
|   2010|   10|Idaho                  |Nevada                      |          17|          63|      -46|
|   2010|   10|New Mexico             |Wyoming                     |          34|          31|        3|
|   2010|   10|Florida State          |North Carolina              |          35|          37|       -2|
|   2010|   10|Oklahoma State         |Baylor                      |          55|          28|       27|
|   2010|   10|Oregon                 |Washington                  |          53|          16|       37|
|   2010|   10|Penn State             |Northwestern                |          35|          21|       14|
|   2010|   10|San Diego State        |Colorado State              |          24|          19|        5|
|   2010|   10|Southern California    |Arizona State               |          34|          33|        1|
|   2010|   10|Tulane                 |Southern Mississippi        |          30|          46|      -16|
|   2010|   10|Stanford               |Arizona                     |          42|          17|       25|
|   2010|   10|Kent State             |Temple                      |          10|          28|      -18|
|   2010|   10|Memphis                |Tennessee                   |          14|          50|      -36|
|   2010|   10|Texas A&M              |Oklahoma                    |          33|          19|       14|
|   2010|   10|Utah                   |Texas Christian             |           7|          47|      -40|
|   2010|   10|Texas Tech             |Missouri                    |          24|          17|        7|
|   2010|   10|Texas-El Paso          |Southern Methodist          |          28|          14|       14|
|   2010|   10|North Texas            |Troy                        |          35|          41|       -6|
|   2010|   10|Tulsa                  |Rice                        |          64|          27|       37|
|   2010|   10|UCLA                   |Oregon State                |          17|          14|        3|
|   2010|   10|Utah State             |New Mexico State            |          27|          22|        5|
|   2010|   10|Purdue                 |Wisconsin                   |          13|          34|      -21|
|   2010|   11|Northern Illinois      |Toledo                      |          65|          30|       35|
|   2010|   11|Bowling Green State    |Miami (OH)                  |          21|          24|       -3|
|   2010|   11|Connecticut            |Pittsburgh                  |          30|          28|        2|
|   2010|   11|Alabama-Birmingham     |East Carolina               |          42|          54|      -12|
|   2010|   11|Buffalo                |Ball State                  |           3|          20|      -17|
|   2010|   11|Idaho                  |Boise State                 |          14|          52|      -38|
|   2010|   11|Air Force              |New Mexico                  |          48|          23|       25|
|   2010|   11|Alabama                |Mississippi State           |          30|          10|       20|
|   2010|   11|Arkansas               |Texas-El Paso               |          58|          21|       37|
|   2010|   11|Kent State             |Army                        |          28|          45|      -17|
|   2010|   11|Auburn                 |Georgia                     |          49|          31|       18|
|   2010|   11|Duke                   |Boston College              |          16|          21|       -5|
|   2010|   11|Colorado State         |Brigham Young               |          10|          49|      -39|
|   2010|   11|Colorado               |Iowa State                  |          34|          14|       20|
|   2010|   11|Florida Atlantic       |Louisiana-Lafayette         |          24|          23|        1|
|   2010|   11|Troy                   |Florida International       |          35|          52|      -17|
|   2010|   11|Florida State          |Clemson                     |          16|          13|        3|
|   2010|   11|Kentucky               |Vanderbilt                  |          38|          20|       18|
|   2010|   11|Louisiana State        |Louisiana-Monroe            |          51|           0|       51|
|   2010|   11|New Mexico State       |Louisiana Tech              |          20|          41|      -21|
|   2010|   11|Marshall               |Memphis                     |          28|          13|       15|
|   2010|   11|Virginia               |Maryland                    |          23|          42|      -19|
|   2010|   11|Georgia Tech           |Miami (FL)                  |          10|          35|      -25|
|   2010|   11|Purdue                 |Michigan                    |          16|          27|      -11|
|   2010|   11|Illinois               |Minnesota                   |          34|          38|       -4|
|   2010|   11|Missouri               |Kansas State                |          38|          28|       10|
|   2010|   11|Navy                   |Central Michigan            |          38|          37|        1|
|   2010|   11|Nebraska               |Kansas                      |          20|           3|       17|
|   2010|   11|Fresno State           |Nevada                      |          34|          35|       -1|
|   2010|   11|Nevada-Las Vegas       |Wyoming                     |          42|          16|       26|
|   2010|   11|North Carolina State   |Wake Forest                 |          38|           3|       35|
|   2010|   11|Middle Tennessee State |North Texas                 |          17|          23|       -6|
|   2010|   11|Northwestern           |Iowa                        |          21|          17|        4|
|   2010|   11|Notre Dame             |Utah                        |          28|           3|       25|
|   2010|   11|Ohio State             |Penn State                  |          38|          14|       24|
|   2010|   11|Oklahoma               |Texas Tech                  |          45|           7|       38|
|   2010|   11|Texas                  |Oklahoma State              |          16|          33|      -17|
|   2010|   11|California             |Oregon                      |          13|          15|       -2|
|   2010|   11|Florida                |South Carolina              |          14|          36|      -22|
|   2010|   11|Louisville             |South Florida               |          21|          24|       -3|
|   2010|   11|Arizona                |Southern California         |          21|          24|       -3|
|   2010|   11|Central Florida        |Southern Mississippi        |          21|          31|      -10|
|   2010|   11|Arizona State          |Stanford                    |          13|          17|       -4|
|   2010|   11|Rutgers                |Syracuse                    |          10|          13|       -3|
|   2010|   11|Tennessee              |Mississippi                 |          52|          14|       38|
|   2010|   11|Baylor                 |Texas A&M                   |          30|          42|      -12|
|   2010|   11|Texas Christian        |San Diego State             |          40|          35|        5|
|   2010|   11|Tulane                 |Rice                        |          54|          49|        5|
|   2010|   11|Houston                |Tulsa                       |          25|          28|       -3|
|   2010|   11|San Jose State         |Utah State                  |          34|          38|       -4|
|   2010|   11|North Carolina         |Virginia Tech               |          10|          26|      -16|
|   2010|   11|Oregon State           |Washington State            |          14|          31|      -17|
|   2010|   11|West Virginia          |Cincinnati                  |          37|          10|       27|
|   2010|   11|Arkansas State         |Western Kentucky            |          35|          36|       -1|
|   2010|   11|Western Michigan       |Eastern Michigan            |          45|          30|       15|
|   2010|   11|Wisconsin              |Indiana                     |          83|          20|       63|
|   2010|   12|Temple                 |Ohio                        |          23|          31|       -8|
|   2010|   12|Akron                  |Miami (OH)                  |          14|          19|       -5|
|   2010|   12|Toledo                 |Bowling Green State         |          33|          14|       19|
|   2010|   12|Nevada-Las Vegas       |Air Force                   |          20|          35|      -15|
|   2010|   12|Alabama                |Georgia State               |          63|           7|       56|
|   2010|   12|Washington             |UCLA                        |          24|           7|       17|
|   2010|   12|Boise State            |Fresno State                |          51|           0|       51|
|   2010|   12|Alabama-Birmingham     |Memphis                     |          31|          15|       16|
|   2010|   12|Mississippi State      |Arkansas                    |          31|          38|       -7|
|   2010|   12|Boston College         |Virginia                    |          17|          13|        4|
|   2010|   12|Brigham Young          |New Mexico                  |          40|           7|       33|
|   2010|   12|Tulane                 |Central Florida             |          14|          61|      -47|
|   2010|   12|Cincinnati             |Rutgers                     |          69|          38|       31|
|   2010|   12|Wake Forest            |Clemson                     |          10|          30|      -20|
|   2010|   12|Colorado               |Kansas State                |          44|          36|        8|
|   2010|   12|Syracuse               |Connecticut                 |           6|          23|      -17|
|   2010|   12|Buffalo                |Eastern Michigan            |          17|          21|       -4|
|   2010|   12|Florida                |Appalachian State           |          48|          10|       38|
|   2010|   12|Louisiana-Lafayette    |Florida International       |          17|          38|      -21|
|   2010|   12|Maryland               |Florida State               |          16|          30|      -14|
|   2010|   12|Georgia Tech           |Duke                        |          30|          20|       10|
|   2010|   12|Hawaii                 |San Jose State              |          41|           7|       34|
|   2010|   12|Utah State             |Idaho                       |           6|          28|      -22|
|   2010|   12|Northwestern           |Illinois                    |          27|          48|      -21|
|   2010|   12|Louisiana State        |Mississippi                 |          43|          36|        7|
|   2010|   12|Louisiana-Monroe       |North Texas                 |          49|          37|       12|
|   2010|   12|Michigan State         |Purdue                      |          35|          31|        4|
|   2010|   12|Western Kentucky       |Middle Tennessee State      |          26|          27|       -1|
|   2010|   12|Iowa State             |Missouri                    |           0|          14|      -14|
|   2010|   12|Navy                   |Arkansas State              |          35|          19|       16|
|   2010|   12|Nevada                 |New Mexico State            |          52|           6|       46|
|   2010|   12|North Carolina         |North Carolina State        |          25|          29|       -4|
|   2010|   12|Ball State             |Northern Illinois           |          21|          59|      -38|
|   2010|   12|Army                   |Notre Dame                  |           3|          27|      -24|
|   2010|   12|Iowa                   |Ohio State                  |          17|          20|       -3|
|   2010|   12|Baylor                 |Oklahoma                    |          24|          53|      -29|
|   2010|   12|Kansas                 |Oklahoma State              |          14|          48|      -34|
|   2010|   12|Oregon State           |Southern California         |          36|           7|       29|
|   2010|   12|Penn State             |Indiana                     |          41|          24|       17|
|   2010|   12|South Florida          |Pittsburgh                  |          10|          17|       -7|
|   2010|   12|Rice                   |East Carolina               |          62|          38|       24|
|   2010|   12|South Carolina         |Troy                        |          69|          24|       45|
|   2010|   12|Southern Methodist     |Marshall                    |          31|          17|       14|
|   2010|   12|Southern Mississippi   |Houston                     |          59|          41|       18|
|   2010|   12|California             |Stanford                    |          14|          48|      -34|
|   2010|   12|Vanderbilt             |Tennessee                   |          10|          24|      -14|
|   2010|   12|Texas                  |Florida Atlantic            |          51|          17|       34|
|   2010|   12|Texas A&M              |Nebraska                    |           9|           6|        3|
|   2010|   12|Texas Tech             |Weber State                 |          64|          21|       43|
|   2010|   12|Tulsa                  |Texas-El Paso               |          31|          28|        3|
|   2010|   12|San Diego State        |Utah                        |          34|          38|       -4|
|   2010|   12|Miami (FL)             |Virginia Tech               |          17|          31|      -14|
|   2010|   12|Louisville             |West Virginia               |          10|          17|       -7|
|   2010|   12|Western Michigan       |Kent State                  |          38|           3|       35|
|   2010|   12|Michigan               |Wisconsin                   |          28|          48|      -20|
|   2010|   12|Wyoming                |Colorado State              |          44|           0|       44|
|   2010|   13|Miami (OH)             |Temple                      |          23|           3|       20|
|   2010|   13|Texas                  |Texas A&M                   |          17|          24|       -7|
|   2010|   13|Akron                  |Buffalo                     |          22|          14|        8|
|   2010|   13|Arizona State          |UCLA                        |          55|          34|       21|
|   2010|   13|Alabama                |Auburn                      |          27|          28|       -1|
|   2010|   13|Kent State             |Ohio                        |          28|           6|       22|
|   2010|   13|Rutgers                |Louisville                  |          13|          40|      -27|
|   2010|   13|Nebraska               |Colorado                    |          45|          17|       28|
|   2010|   13|Nevada                 |Boise State                 |          34|          31|        3|
|   2010|   13|Eastern Michigan       |Northern Illinois           |           3|          71|      -68|
|   2010|   13|Oregon                 |Arizona                     |          48|          29|       19|
|   2010|   13|East Carolina          |Southern Methodist          |          38|          45|       -7|
|   2010|   13|Toledo                 |Central Michigan            |          42|          31|       11|
|   2010|   13|Tulsa                  |Southern Mississippi        |          56|          50|        6|
|   2010|   13|Pittsburgh             |West Virginia               |          10|          35|      -25|
|   2010|   13|Bowling Green State    |Western Michigan            |           7|          41|      -34|
|   2010|   13|Arkansas               |Louisiana State             |          31|          23|        8|
|   2010|   13|Syracuse               |Boston College              |           7|          16|       -9|
|   2010|   13|Memphis                |Central Florida             |          17|          37|      -20|
|   2010|   13|Connecticut            |Cincinnati                  |          38|          17|       21|
|   2010|   13|Florida International  |Arkansas State              |          31|          24|        7|
|   2010|   13|Florida State          |Florida                     |          31|           7|       24|
|   2010|   13|Fresno State           |Idaho                       |          23|          20|        3|
|   2010|   13|Georgia                |Georgia Tech                |          42|          34|        8|
|   2010|   13|New Mexico State       |Hawaii                      |          24|          59|      -35|
|   2010|   13|Purdue                 |Indiana                     |          31|          34|       -3|
|   2010|   13|North Texas            |Kansas State                |          41|          49|       -8|
|   2010|   13|San Jose State         |Louisiana Tech              |          38|          45|       -7|
|   2010|   13|Louisiana-Monroe       |Louisiana-Lafayette         |          22|          23|       -1|
|   2010|   13|Marshall               |Tulane                      |          38|          23|       15|
|   2010|   13|Maryland               |North Carolina State        |          38|          31|        7|
|   2010|   13|Penn State             |Michigan State              |          22|          28|       -6|
|   2010|   13|Middle Tennessee State |Florida Atlantic            |          38|          14|       24|
|   2010|   13|Minnesota              |Iowa                        |          27|          24|        3|
|   2010|   13|Mississippi            |Mississippi State           |          23|          31|       -8|
|   2010|   13|Missouri               |Kansas                      |          35|           7|       28|
|   2010|   13|Duke                   |North Carolina              |          19|          24|       -5|
|   2010|   13|Southern California    |Notre Dame                  |          16|          20|       -4|
|   2010|   13|Ohio State             |Michigan                    |          37|           7|       30|
|   2010|   13|Oklahoma State         |Oklahoma                    |          41|          47|       -6|
|   2010|   13|Rice                   |Alabama-Birmingham          |          28|          23|        5|
|   2010|   13|San Diego State        |Nevada-Las Vegas            |          48|          14|       34|
|   2010|   13|Clemson                |South Carolina              |           7|          29|      -22|
|   2010|   13|Miami (FL)             |South Florida               |          20|          23|       -3|
|   2010|   13|Stanford               |Oregon State                |          38|           0|       38|
|   2010|   13|Tennessee              |Kentucky                    |          24|          14|       10|
|   2010|   13|New Mexico             |Texas Christian             |          17|          66|      -49|
|   2010|   13|Texas Tech             |Houston                     |          35|          20|       15|
|   2010|   13|Troy                   |Western Kentucky            |          28|          14|       14|
|   2010|   13|Utah                   |Brigham Young               |          17|          16|        1|
|   2010|   13|Virginia Tech          |Virginia                    |          37|           7|       30|
|   2010|   13|Vanderbilt             |Wake Forest                 |          13|          34|      -21|
|   2010|   13|California             |Washington                  |          13|          16|       -3|
|   2010|   13|Wisconsin              |Northwestern                |          70|          23|       47|
|   2010|   14|Arizona                |Arizona State               |          29|          30|       -1|
|   2010|   14|Fresno State           |Illinois                    |          25|          23|        2|
|   2010|   14|Miami (OH)             |Northern Illinois           |          26|          21|        5|
|   2010|   14|Auburn                 |South Carolina              |          56|          17|       39|
|   2010|   14|Boise State            |Utah State                  |          50|          14|       36|
|   2010|   14|Central Florida        |Southern Methodist          |          17|           7|       10|
|   2010|   14|South Florida          |Connecticut                 |          16|          19|       -3|
|   2010|   14|Hawaii                 |Nevada-Las Vegas            |          59|          21|       38|
|   2010|   14|Idaho                  |San Jose State              |          26|          23|        3|
|   2010|   14|Florida International  |Middle Tennessee State      |          27|          28|       -1|
|   2010|   14|Louisiana Tech         |Nevada                      |          17|          35|      -18|
|   2010|   14|Oklahoma               |Nebraska                    |          23|          20|        3|
|   2010|   14|Oregon State           |Oregon                      |          20|          37|      -17|
|   2010|   14|Cincinnati             |Pittsburgh                  |          10|          28|      -18|
|   2010|   14|UCLA                   |Southern California         |          14|          28|      -14|
|   2010|   14|Florida Atlantic       |Troy                        |           7|          44|      -37|
|   2010|   14|Virginia Tech          |Florida State               |          44|          33|       11|
|   2010|   14|Washington State       |Washington                  |          28|          35|       -7|
|   2010|   14|West Virginia          |Rutgers                     |          35|          14|       21|
|   2010|   15|Navy                   |Army                        |          31|          17|       14|
|   2010|   16|Brigham Young          |Texas-El Paso               |          52|          24|       28|
|   2010|   16|Northern Illinois      |Fresno State                |          40|          17|       23|
|   2010|   16|Troy                   |Ohio                        |          48|          21|       27|
|   2010|   17|Louisville             |Southern Mississippi        |          31|          28|        3|
|   2010|   17|Boise State            |Utah                        |          26|           3|       23|
|   2010|   17|San Diego State        |Navy                        |          35|          14|       21|
|   2010|   17|Hawaii                 |Tulsa                       |          35|          62|      -27|
|   2010|   17|Florida International  |Toledo                      |          34|          32|        2|
|   2010|   17|Air Force              |Georgia Tech                |          14|           7|        7|
|   2010|   18|Iowa                   |Missouri                    |          27|          24|        3|
|   2010|   18|North Carolina State   |West Virginia               |          23|           7|       16|
|   2010|   18|Illinois               |Baylor                      |          38|          14|       24|
|   2010|   18|Maryland               |East Carolina               |          51|          20|       31|
|   2010|   18|Oklahoma State         |Arizona                     |          36|          10|       26|
|   2010|   18|Army                   |Southern Methodist          |          16|          14|        2|
|   2010|   18|North Carolina         |Tennessee                   |          30|          27|        3|
|   2010|   18|Syracuse               |Kansas State                |          36|          34|        2|
|   2010|   18|Washington             |Nebraska                    |          19|           7|       12|
|   2010|   18|Central Florida        |Georgia                     |          10|           6|        4|
|   2010|   18|Florida State          |South Carolina              |          26|          17|        9|
|   2010|   18|Notre Dame             |Miami (FL)                  |          33|          17|       16|
|   2010|   18|South Florida          |Clemson                     |          31|          26|        5|
|   2010|   18|Alabama                |Michigan State              |          49|           7|       42|
|   2010|   18|Florida                |Penn State                  |          37|          24|       13|
|   2010|   18|Mississippi State      |Michigan                    |          52|          14|       38|
|   2010|   18|Oklahoma               |Connecticut                 |          48|          20|       28|
|   2010|   18|Texas Christian        |Wisconsin                   |          21|          19|        2|
|   2010|   18|Texas Tech             |Northwestern                |          45|          38|        7|
|   2010|   19|Stanford               |Virginia Tech               |          40|          12|       28|
|   2010|   20|Ohio State             |Arkansas                    |          31|          26|        5|
|   2010|   20|Miami (OH)             |Middle Tennessee State      |          35|          21|       14|
|   2010|   20|Louisiana State        |Texas A&M                   |          41|          24|       17|
|   2010|   20|Pittsburgh             |Kentucky                    |          27|          10|       17|
|   2010|   20|Nevada                 |Boston College              |          20|          13|        7|
|   2010|   20|Auburn                 |Oregon                      |          22|          19|        3|
|   2011|    1|Arizona State          |California-Davis            |          48|          14|       34|
|   2011|    1|Idaho                  |Bowling Green State         |          15|          32|      -17|
|   2011|    1|Central Michigan       |South Carolina State        |          21|           6|       15|
|   2011|    1|Florida International  |North Texas                 |          41|          16|       25|
|   2011|    1|Georgia Tech           |Western Carolina            |          63|          21|       42|
|   2011|    1|Kentucky               |Western Kentucky            |          14|           3|       11|
|   2011|    1|Louisville             |Murray State                |          21|           9|       12|
|   2011|    1|Memphis                |Mississippi State           |          14|          59|      -45|
|   2011|    1|Rutgers                |North Carolina Central      |          48|           0|       48|
|   2011|    1|Syracuse               |Wake Forest                 |          36|          29|        7|
|   2011|    1|Temple                 |Villanova                   |          42|           7|       35|
|   2011|    1|Toledo                 |New Hampshire               |          58|          22|       36|
|   2011|    1|Utah                   |Montana State               |          27|          10|       17|
|   2011|    1|Wisconsin              |Nevada-Las Vegas            |          51|          17|       34|
|   2011|    1|Baylor                 |Texas Christian             |          50|          48|        2|
|   2011|    1|Michigan State         |Youngstown State            |          28|           6|       22|
|   2011|    1|Air Force              |South Dakota                |          37|          20|       17|
|   2011|    1|Alabama                |Kent State                  |          48|           7|       41|
|   2011|    1|Arizona                |Northern Arizona            |          41|          10|       31|
|   2011|    1|Arkansas               |Missouri State              |          51|           7|       44|
|   2011|    1|Auburn                 |Utah State                  |          42|          38|        4|
|   2011|    1|Ball State             |Indiana                     |          27|          20|        7|
|   2011|    1|Georgia                |Boise State                 |          21|          35|      -14|
|   2011|    1|Mississippi            |Brigham Young               |          13|          14|       -1|
|   2011|    1|California             |Fresno State                |          36|          21|       15|
|   2011|    1|Central Florida        |Charleston Southern         |          62|           0|       62|
|   2011|    1|Cincinnati             |Austin Peay                 |          72|          10|       62|
|   2011|    1|Clemson                |Troy                        |          43|          19|       24|
|   2011|    1|New Mexico             |Colorado State              |          10|          14|       -4|
|   2011|    1|Connecticut            |Fordham                     |          35|           3|       32|
|   2011|    1|Florida                |Florida Atlantic            |          41|           3|       38|
|   2011|    1|Florida State          |Louisiana-Monroe            |          34|           0|       34|
|   2011|    1|Hawaii                 |Colorado                    |          34|          17|       17|
|   2011|    1|Houston                |UCLA                        |          38|          34|        4|
|   2011|    1|Illinois               |Arkansas State              |          33|          15|       18|
|   2011|    1|Iowa                   |Tennessee Tech              |          34|           7|       27|
|   2011|    1|Iowa State             |Northern Iowa               |          20|          19|        1|
|   2011|    1|Kansas                 |McNeese State               |          42|          24|       18|
|   2011|    1|Kansas State           |Eastern Kentucky            |          10|           7|        3|
|   2011|    1|Louisiana State        |Oregon                      |          40|          27|       13|
|   2011|    1|Michigan               |Western Michigan            |          34|          10|       24|
|   2011|    1|Missouri               |Miami (OH)                  |          17|           6|       11|
|   2011|    1|Navy                   |Delaware                    |          40|          17|       23|
|   2011|    1|Nebraska               |Chattanooga                 |          40|           7|       33|
|   2011|    1|North Carolina         |James Madison               |          42|          10|       32|
|   2011|    1|North Carolina State   |Liberty                     |          43|          21|       22|
|   2011|    1|Northern Illinois      |Army                        |          49|          26|       23|
|   2011|    1|Boston College         |Northwestern                |          17|          24|       -7|
|   2011|    1|New Mexico State       |Ohio                        |          24|          44|      -20|
|   2011|    1|Ohio State             |Akron                       |          42|           0|       42|
|   2011|    1|Oklahoma               |Tulsa                       |          47|          14|       33|
|   2011|    1|Oklahoma State         |Louisiana-Lafayette         |          61|          34|       27|
|   2011|    1|Penn State             |Indiana State               |          41|           7|       34|
|   2011|    1|Pittsburgh             |Buffalo                     |          35|          16|       19|
|   2011|    1|Purdue                 |Middle Tennessee State      |          27|          24|        3|
|   2011|    1|Duke                   |Richmond                    |          21|          23|       -2|
|   2011|    1|Oregon State           |Sacramento State            |          28|          29|       -1|
|   2011|    1|San Diego State        |Cal Poly                    |          49|          21|       28|
|   2011|    1|South Carolina         |East Carolina               |          56|          37|       19|
|   2011|    1|Notre Dame             |South Florida               |          20|          23|       -3|
|   2011|    1|Southern California    |Minnesota                   |          19|          17|        2|
|   2011|    1|Southern Mississippi   |Louisiana Tech              |          19|          17|        2|
|   2011|    1|Stanford               |San Jose State              |          57|           3|       54|
|   2011|    1|Tennessee              |Montana                     |          42|          16|       26|
|   2011|    1|Texas                  |Rice                        |          34|           9|       25|
|   2011|    1|Texas Tech             |Texas State                 |          50|          10|       40|
|   2011|    1|Texas-El Paso          |Stony Brook                 |          31|          24|        7|
|   2011|    1|Tulane                 |Southeastern Louisiana      |          47|          33|       14|
|   2011|    1|Vanderbilt             |Elon                        |          45|          14|       31|
|   2011|    1|Virginia               |William & Mary              |          40|           3|       37|
|   2011|    1|Virginia Tech          |Appalachian State           |          66|          13|       53|
|   2011|    1|Washington             |Eastern Washington          |          30|          27|        3|
|   2011|    1|Washington State       |Idaho State                 |          64|          21|       43|
|   2011|    1|Wyoming                |Weber State                 |          35|          32|        3|
|   2011|    1|Eastern Michigan       |Howard                      |          41|           9|       32|
|   2011|    1|Texas A&M              |Southern Methodist          |          46|          14|       32|
|   2011|    1|West Virginia          |Marshall                    |          34|          13|       21|
|   2011|    1|Maryland               |Miami (FL)                  |          32|          24|        8|
|   2011|    2|Oklahoma State         |Arizona                     |          37|          14|       23|
|   2011|    2|Arizona State          |Missouri                    |          37|          30|        7|
|   2011|    2|Louisville             |Florida International       |          17|          24|       -7|
|   2011|    2|Penn State             |Alabama                     |          11|          27|      -16|
|   2011|    2|Arkansas               |New Mexico                  |          52|           3|       49|
|   2011|    2|Arkansas State         |Memphis                     |          47|           3|       44|
|   2011|    2|Auburn                 |Mississippi State           |          41|          34|        7|
|   2011|    2|Bowling Green State    |Morgan State                |          58|          13|       45|
|   2011|    2|Buffalo                |Stony Brook                 |          35|           7|       28|
|   2011|    2|Colorado               |California                  |          33|          36|       -3|
|   2011|    2|Central Florida        |Boston College              |          30|           3|       27|
|   2011|    2|Clemson                |Wofford                     |          35|          27|        8|
|   2011|    2|Colorado State         |Northern Colorado           |          33|          14|       19|
|   2011|    2|Eastern Michigan       |Alabama State               |          14|           7|        7|
|   2011|    2|Florida                |Alabama-Birmingham          |          39|           0|       39|
|   2011|    2|Florida State          |Charleston Southern         |          62|          10|       52|
|   2011|    2|Middle Tennessee State |Georgia Tech                |          21|          49|      -28|
|   2011|    2|North Texas            |Houston                     |          23|          48|      -25|
|   2011|    2|Idaho                  |North Dakota                |          44|          14|       30|
|   2011|    2|Illinois               |South Dakota State          |          56|           3|       53|
|   2011|    2|Iowa State             |Iowa                        |          44|          41|        3|
|   2011|    2|Kansas                 |Northern Illinois           |          45|          42|        3|
|   2011|    2|Kentucky               |Central Michigan            |          27|          13|       14|
|   2011|    2|Louisiana State        |Northwestern State          |          49|           3|       46|
|   2011|    2|Louisiana Tech         |Central Arkansas            |          48|          42|        6|
|   2011|    2|Kent State             |Louisiana-Lafayette         |          12|          20|       -8|
|   2011|    2|Louisiana-Monroe       |Grambling State             |          35|           7|       28|
|   2011|    2|Marshall               |Southern Mississippi        |          26|          20|        6|
|   2011|    2|Michigan               |Notre Dame                  |          35|          31|        4|
|   2011|    2|Michigan State         |Florida Atlantic            |          44|           0|       44|
|   2011|    2|Mississippi            |Southern Illinois           |          42|          24|       18|
|   2011|    2|Western Kentucky       |Navy                        |          14|          40|      -26|
|   2011|    2|Nebraska               |Fresno State                |          42|          29|       13|
|   2011|    2|Minnesota              |New Mexico State            |          21|          28|       -7|
|   2011|    2|North Carolina         |Rutgers                     |          24|          22|        2|
|   2011|    2|Northwestern           |Eastern Illinois            |          42|          21|       21|
|   2011|    2|Ohio                   |Gardner-Webb                |          30|           3|       27|
|   2011|    2|Ohio State             |Toledo                      |          27|          22|        5|
|   2011|    2|Oregon                 |Nevada                      |          69|          20|       49|
|   2011|    2|Pittsburgh             |Maine                       |          35|          29|        6|
|   2011|    2|Rice                   |Purdue                      |          24|          22|        2|
|   2011|    2|Army                   |San Diego State             |          20|          23|       -3|
|   2011|    2|Georgia                |South Carolina              |          42|          45|       -3|
|   2011|    2|South Florida          |Ball State                  |          37|           7|       30|
|   2011|    2|Southern California    |Utah                        |          23|          14|        9|
|   2011|    2|Southern Methodist     |Texas-El Paso               |          28|          17|       11|
|   2011|    2|Duke                   |Stanford                    |          14|          44|      -30|
|   2011|    2|Syracuse               |Rhode Island                |          21|          14|        7|
|   2011|    2|Akron                  |Temple                      |           3|          41|      -38|
|   2011|    2|Tennessee              |Cincinnati                  |          45|          23|       22|
|   2011|    2|Texas                  |Brigham Young               |          17|          16|        1|
|   2011|    2|Air Force              |Texas Christian             |          19|          35|      -16|
|   2011|    2|Tulane                 |Tulsa                       |           3|          31|      -28|
|   2011|    2|UCLA                   |San Jose State              |          27|          17|       10|
|   2011|    2|Utah State             |Weber State                 |          54|          17|       37|
|   2011|    2|Vanderbilt             |Connecticut                 |          24|          21|        3|
|   2011|    2|Indiana                |Virginia                    |          31|          34|       -3|
|   2011|    2|East Carolina          |Virginia Tech               |          10|          17|       -7|
|   2011|    2|Wake Forest            |North Carolina State        |          34|          27|        7|
|   2011|    2|Washington             |Hawaii                      |          40|          32|        8|
|   2011|    2|Washington State       |Nevada-Las Vegas            |          59|           7|       52|
|   2011|    2|West Virginia          |Norfolk State               |          55|          12|       43|
|   2011|    2|Western Michigan       |Nicholls State              |          38|           7|       31|
|   2011|    2|Wisconsin              |Oregon State                |          35|           0|       35|
|   2011|    2|Wyoming                |Texas State                 |          45|          10|       35|
|   2011|    3|Mississippi State      |Louisiana State             |           6|          19|      -13|
|   2011|    3|Toledo                 |Boise State                 |          15|          40|      -25|
|   2011|    3|Connecticut            |Iowa State                  |          20|          24|       -4|
|   2011|    3|Alabama                |North Texas                 |          41|           0|       41|
|   2011|    3|Arkansas               |Troy                        |          38|          28|       10|
|   2011|    3|Army                   |Northwestern                |          21|          14|        7|
|   2011|    3|Ball State             |Buffalo                     |          28|          25|        3|
|   2011|    3|Baylor                 |Stephen F. Austin           |          48|           0|       48|
|   2011|    3|California             |Presbyterian                |          63|          12|       51|
|   2011|    3|Cincinnati             |Akron                       |          59|          14|       45|
|   2011|    3|Clemson                |Auburn                      |          38|          24|       14|
|   2011|    3|Colorado               |Colorado State              |          28|          14|       14|
|   2011|    3|Boston College         |Duke                        |          19|          20|       -1|
|   2011|    3|Florida                |Tennessee                   |          33|          23|       10|
|   2011|    3|Florida International  |Central Florida             |          17|          10|        7|
|   2011|    3|Fresno State           |North Dakota                |          27|          22|        5|
|   2011|    3|Georgia                |Coastal Carolina            |          59|           0|       59|
|   2011|    3|Georgia Tech           |Kansas                      |          66|          24|       42|
|   2011|    3|Louisiana Tech         |Houston                     |          34|          35|       -1|
|   2011|    3|Illinois               |Arizona State               |          17|          14|        3|
|   2011|    3|Indiana                |South Carolina State        |          38|          21|       17|
|   2011|    3|Western Kentucky       |Indiana State               |          16|          44|      -28|
|   2011|    3|Iowa                   |Pittsburgh                  |          31|          27|        4|
|   2011|    3|Kansas State           |Kent State                  |          37|           0|       37|
|   2011|    3|Louisiana-Lafayette    |Nicholls State              |          38|          21|       17|
|   2011|    3|Kentucky               |Louisville                  |          17|          24|       -7|
|   2011|    3|Memphis                |Austin Peay                 |          27|           6|       21|
|   2011|    3|Miami (FL)             |Ohio State                  |          24|           6|       18|
|   2011|    3|Michigan               |Eastern Michigan            |          31|           3|       28|
|   2011|    3|Minnesota              |Miami (OH)                  |          29|          23|        6|
|   2011|    3|Missouri               |Western Illinois            |          69|           0|       69|
|   2011|    3|Nebraska               |Washington                  |          51|          38|       13|
|   2011|    3|San Jose State         |Nevada                      |          14|          17|       -3|
|   2011|    3|Nevada-Las Vegas       |Hawaii                      |          40|          20|       20|
|   2011|    3|North Carolina         |Virginia                    |          28|          17|       11|
|   2011|    3|North Carolina State   |South Alabama               |          35|          13|       22|
|   2011|    3|Notre Dame             |Michigan State              |          31|          13|       18|
|   2011|    3|Ohio                   |Marshall                    |          44|           7|       37|
|   2011|    3|Florida State          |Oklahoma                    |          13|          23|      -10|
|   2011|    3|Tulsa                  |Oklahoma State              |          33|          59|      -26|
|   2011|    3|Oregon                 |Missouri State              |          56|           7|       49|
|   2011|    3|Temple                 |Penn State                  |          10|          14|       -4|
|   2011|    3|Purdue                 |Southeast Missouri State    |          59|           0|       59|
|   2011|    3|San Diego State        |Washington State            |          42|          24|       18|
|   2011|    3|South Carolina         |Navy                        |          24|          21|        3|
|   2011|    3|South Florida          |Florida A&M                 |          70|          17|       53|
|   2011|    3|Southern California    |Syracuse                    |          38|          17|       21|
|   2011|    3|Southern Methodist     |Northwestern State          |          40|           7|       33|
|   2011|    3|Southern Mississippi   |Southeastern Louisiana      |          52|           6|       46|
|   2011|    3|Arizona                |Stanford                    |          10|          37|      -27|
|   2011|    3|UCLA                   |Texas                       |          20|          49|      -29|
|   2011|    3|Texas A&M              |Idaho                       |          37|           7|       30|
|   2011|    3|Texas Christian        |Louisiana-Monroe            |          38|          17|       21|
|   2011|    3|New Mexico             |Texas Tech                  |          13|          59|      -46|
|   2011|    3|New Mexico State       |Texas-El Paso               |          10|          16|       -6|
|   2011|    3|Alabama-Birmingham     |Tulane                      |          10|          49|      -39|
|   2011|    3|Brigham Young          |Utah                        |          10|          54|      -44|
|   2011|    3|Vanderbilt             |Mississippi                 |          30|           7|       23|
|   2011|    3|Virginia Tech          |Arkansas State              |          26|           7|       19|
|   2011|    3|Wake Forest            |Gardner-Webb                |          48|           5|       43|
|   2011|    3|Maryland               |West Virginia               |          31|          37|       -6|
|   2011|    3|Western Michigan       |Central Michigan            |          44|          14|       30|
|   2011|    3|Wisconsin              |Northern Illinois           |          49|           7|       42|
|   2011|    3|Bowling Green State    |Wyoming                     |          27|          28|       -1|
|   2011|    4|Cincinnati             |North Carolina State        |          44|          14|       30|
|   2011|    4|Brigham Young          |Central Florida             |          24|          17|        7|
|   2011|    4|Air Force              |Tennessee State             |          63|          24|       39|
|   2011|    4|Akron                  |Virginia Military Institute |          36|          13|       23|
|   2011|    4|Alabama                |Arkansas                    |          38|          14|       24|
|   2011|    4|Arizona State          |Southern California         |          43|          22|       21|
|   2011|    4|Arkansas State         |Central Arkansas            |          53|          24|       29|
|   2011|    4|Auburn                 |Florida Atlantic            |          30|          14|       16|
|   2011|    4|Ball State             |Army                        |          48|          21|       27|
|   2011|    4|Baylor                 |Rice                        |          56|          31|       25|
|   2011|    4|Boise State            |Tulsa                       |          41|          21|       20|
|   2011|    4|Boston College         |Massachusetts               |          45|          17|       28|
|   2011|    4|Miami (OH)             |Bowling Green State         |          23|          37|      -14|
|   2011|    4|Clemson                |Florida State               |          35|          30|        5|
|   2011|    4|Utah State             |Colorado State              |          34|          35|       -1|
|   2011|    4|Buffalo                |Connecticut                 |           3|          17|      -14|
|   2011|    4|Duke                   |Tulane                      |          48|          27|       21|
|   2011|    4|East Carolina          |Alabama-Birmingham          |          28|          23|        5|
|   2011|    4|Kentucky               |Florida                     |          10|          48|      -38|
|   2011|    4|Idaho                  |Fresno State                |          24|          48|      -24|
|   2011|    4|Mississippi            |Georgia                     |          13|          27|      -14|
|   2011|    4|Georgia Tech           |North Carolina              |          35|          28|        7|
|   2011|    4|Hawaii                 |California-Davis            |          56|          14|       42|
|   2011|    4|Houston                |Georgia State               |          56|           0|       56|
|   2011|    4|Illinois               |Western Michigan            |          23|          20|        3|
|   2011|    4|Iowa                   |Louisiana-Monroe            |          45|          17|       28|
|   2011|    4|Miami (FL)             |Kansas State                |          24|          28|       -4|
|   2011|    4|Kent State             |South Alabama               |          33|          25|        8|
|   2011|    4|West Virginia          |Louisiana State             |          21|          47|      -26|
|   2011|    4|Florida International  |Louisiana-Lafayette         |          31|          36|       -5|
|   2011|    4|Michigan               |San Diego State             |          28|           7|       21|
|   2011|    4|Michigan State         |Central Michigan            |          45|           7|       38|
|   2011|    4|Mississippi State      |Louisiana Tech              |          26|          20|        6|
|   2011|    4|Wyoming                |Nebraska                    |          14|          38|      -24|
|   2011|    4|Minnesota              |North Dakota State          |          24|          37|      -13|
|   2011|    4|North Texas            |Indiana                     |          24|          21|        3|
|   2011|    4|Northern Illinois      |Cal Poly                    |          47|          30|       17|
|   2011|    4|Pittsburgh             |Notre Dame                  |          12|          15|       -3|
|   2011|    4|Ohio State             |Colorado                    |          37|          17|       20|
|   2011|    4|Oklahoma               |Missouri                    |          38|          28|       10|
|   2011|    4|Texas A&M              |Oklahoma State              |          29|          30|       -1|
|   2011|    4|Arizona                |Oregon                      |          31|          56|      -25|
|   2011|    4|Penn State             |Eastern Michigan            |          34|           6|       28|
|   2011|    4|Rutgers                |Ohio                        |          38|          26|       12|
|   2011|    4|New Mexico             |Sam Houston State           |          45|          48|       -3|
|   2011|    4|San Jose State         |New Mexico State            |          34|          24|       10|
|   2011|    4|South Carolina         |Vanderbilt                  |          21|           3|       18|
|   2011|    4|South Florida          |Texas-El Paso               |          52|          24|       28|
|   2011|    4|Memphis                |Southern Methodist          |           0|          42|      -42|
|   2011|    4|Virginia               |Southern Mississippi        |          24|          30|       -6|
|   2011|    4|Nevada-Las Vegas       |Southern Utah               |          16|          41|      -25|
|   2011|    4|Syracuse               |Toledo                      |          33|          30|        3|
|   2011|    4|Maryland               |Temple                      |           7|          38|      -31|
|   2011|    4|Texas Christian        |Portland State              |          55|          13|       42|
|   2011|    4|Texas Tech             |Nevada                      |          35|          34|        1|
|   2011|    4|Troy                   |Middle Tennessee State      |          38|          35|        3|
|   2011|    4|Oregon State           |UCLA                        |          19|          27|       -8|
|   2011|    4|Marshall               |Virginia Tech               |          10|          30|      -20|
|   2011|    4|Washington             |California                  |          31|          23|        8|
|   2011|    4|Wisconsin              |South Dakota                |          59|          10|       49|
|   2011|    5|Texas-El Paso          |Houston                     |          42|          49|       -7|
|   2011|    5|Pittsburgh             |South Florida               |          44|          17|       27|
|   2011|    5|Brigham Young          |Utah State                  |          27|          24|        3|
|   2011|    5|Navy                   |Air Force                   |          34|          35|       -1|
|   2011|    5|Florida                |Alabama                     |          10|          38|      -28|
|   2011|    5|Arizona State          |Oregon State                |          35|          20|       15|
|   2011|    5|Arkansas               |Texas A&M                   |          42|          38|        4|
|   2011|    5|Western Kentucky       |Arkansas State              |          22|          26|       -4|
|   2011|    5|Army                   |Tulane                      |          45|           6|       39|
|   2011|    5|South Carolina         |Auburn                      |          13|          16|       -3|
|   2011|    5|Boise State            |Nevada                      |          30|          10|       20|
|   2011|    5|Central Michigan       |Northern Illinois           |          48|          41|        7|
|   2011|    5|Miami (OH)             |Cincinnati                  |           0|          27|      -27|
|   2011|    5|Virginia Tech          |Clemson                     |           3|          23|      -20|
|   2011|    5|Florida International  |Duke                        |          27|          31|       -4|
|   2011|    5|Eastern Michigan       |Akron                       |          31|          23|        8|
|   2011|    5|Georgia                |Mississippi State           |          24|          10|       14|
|   2011|    5|North Carolina State   |Georgia Tech                |          35|          45|      -10|
|   2011|    5|Louisiana Tech         |Hawaii                      |          26|          44|      -18|
|   2011|    5|Illinois               |Northwestern                |          38|          35|        3|
|   2011|    5|Kansas State           |Baylor                      |          36|          35|        1|
|   2011|    5|Louisiana State        |Kentucky                    |          35|           7|       28|
|   2011|    5|Louisiana-Lafayette    |Florida Atlantic            |          37|          34|        3|
|   2011|    5|Louisville             |Marshall                    |          13|          17|       -4|
|   2011|    5|Maryland               |Towson                      |          28|           3|       25|
|   2011|    5|Miami (FL)             |Bethune-Cookman             |          45|          14|       31|
|   2011|    5|Michigan               |Minnesota                   |          58|           0|       58|
|   2011|    5|Ohio State             |Michigan State              |           7|          10|       -3|
|   2011|    5|Middle Tennessee State |Memphis                     |          38|          31|        7|
|   2011|    5|Fresno State           |Mississippi                 |          28|          38|      -10|
|   2011|    5|New Mexico             |New Mexico State            |          28|          42|      -14|
|   2011|    5|East Carolina          |North Carolina              |          20|          35|      -15|
|   2011|    5|Purdue                 |Notre Dame                  |          10|          38|      -28|
|   2011|    5|Ohio                   |Kent State                  |          17|          10|        7|
|   2011|    5|Oklahoma               |Ball State                  |          62|           6|       56|
|   2011|    5|Indiana                |Penn State                  |          10|          16|       -6|
|   2011|    5|Syracuse               |Rutgers                     |          16|          19|       -3|
|   2011|    5|Colorado State         |San Jose State              |          31|          38|       -7|
|   2011|    5|Southern California    |Arizona                     |          48|          41|        7|
|   2011|    5|Texas Christian        |Southern Methodist          |          33|          40|       -7|
|   2011|    5|Southern Mississippi   |Rice                        |          48|          24|       24|
|   2011|    5|Stanford               |UCLA                        |          45|          19|       26|
|   2011|    5|Tennessee              |Buffalo                     |          41|          10|       31|
|   2011|    5|Iowa State             |Texas                       |          14|          37|      -23|
|   2011|    5|Kansas                 |Texas Tech                  |          34|          45|      -11|
|   2011|    5|Temple                 |Toledo                      |          13|          36|      -23|
|   2011|    5|Troy                   |Alabama-Birmingham          |          24|          23|        1|
|   2011|    5|Tulsa                  |North Texas                 |          41|          24|       17|
|   2011|    5|Virginia               |Idaho                       |          21|          20|        1|
|   2011|    5|Boston College         |Wake Forest                 |          19|          27|       -8|
|   2011|    5|Utah                   |Washington                  |          14|          31|      -17|
|   2011|    5|Colorado               |Washington State            |          27|          31|       -4|
|   2011|    5|West Virginia          |Bowling Green State         |          55|          10|       45|
|   2011|    5|Connecticut            |Western Michigan            |          31|          38|       -7|
|   2011|    5|Wisconsin              |Nebraska                    |          48|          17|       31|
|   2011|    6|Oregon                 |California                  |          43|          15|       28|
|   2011|    6|Middle Tennessee State |Western Kentucky            |          33|          36|       -3|
|   2011|    6|Fresno State           |Boise State                 |           7|          57|      -50|
|   2011|    6|Alabama                |Vanderbilt                  |          34|           0|       34|
|   2011|    6|Utah                   |Arizona State               |          14|          35|      -21|
|   2011|    6|Arkansas               |Auburn                      |          38|          14|       24|
|   2011|    6|Louisiana-Monroe       |Arkansas State              |          19|          24|       -5|
|   2011|    6|Baylor                 |Iowa State                  |          49|          26|       23|
|   2011|    6|Brigham Young          |San Jose State              |          29|          16|       13|
|   2011|    6|Buffalo                |Ohio                        |          38|          37|        1|
|   2011|    6|Central Florida        |Marshall                    |          16|           6|       10|
|   2011|    6|Clemson                |Boston College              |          36|          14|       22|
|   2011|    6|Akron                  |Florida International       |          17|          27|      -10|
|   2011|    6|Tennessee              |Georgia                     |          12|          20|       -8|
|   2011|    6|Georgia Tech           |Maryland                    |          21|          16|        5|
|   2011|    6|Houston                |East Carolina               |          56|           3|       53|
|   2011|    6|Indiana                |Illinois                    |          20|          41|      -21|
|   2011|    6|Kansas State           |Missouri                    |          24|          17|        7|
|   2011|    6|Louisiana State        |Florida                     |          41|          11|       30|
|   2011|    6|Idaho                  |Louisiana Tech              |          11|          24|      -13|
|   2011|    6|Louisiana-Lafayette    |Troy                        |          31|          17|       14|
|   2011|    6|Miami (OH)             |Army                        |          35|          28|        7|
|   2011|    6|Northwestern           |Michigan                    |          24|          42|      -18|
|   2011|    6|Alabama-Birmingham     |Mississippi State           |           3|          21|      -18|
|   2011|    6|Nebraska               |Ohio State                  |          34|          27|        7|
|   2011|    6|Nevada                 |Nevada-Las Vegas            |          37|           0|       37|
|   2011|    6|North Carolina         |Louisville                  |          14|           7|        7|
|   2011|    6|North Carolina State   |Central Michigan            |          38|          24|       14|
|   2011|    6|North Texas            |Florida Atlantic            |          31|          17|       14|
|   2011|    6|Northern Illinois      |Kent State                  |          40|          10|       30|
|   2011|    6|Notre Dame             |Air Force                   |          59|          33|       26|
|   2011|    6|Oklahoma               |Texas                       |          55|          17|       38|
|   2011|    6|Oklahoma State         |Kansas                      |          70|          28|       42|
|   2011|    6|Oregon State           |Arizona                     |          37|          27|       10|
|   2011|    6|Penn State             |Iowa                        |          13|           3|       10|
|   2011|    6|Purdue                 |Minnesota                   |          45|          17|       28|
|   2011|    6|Rice                   |Memphis                     |          28|           6|       22|
|   2011|    6|Rutgers                |Pittsburgh                  |          34|          10|       24|
|   2011|    6|South Carolina         |Kentucky                    |          54|           3|       51|
|   2011|    6|Navy                   |Southern Mississippi        |          35|          63|      -28|
|   2011|    6|Stanford               |Colorado                    |          48|           7|       41|
|   2011|    6|Tulane                 |Syracuse                    |          34|          37|       -3|
|   2011|    6|Ball State             |Temple                      |           0|          42|      -42|
|   2011|    6|Texas Tech             |Texas A&M                   |          40|          45|       -5|
|   2011|    6|San Diego State        |Texas Christian             |          14|          27|      -13|
|   2011|    6|Toledo                 |Eastern Michigan            |          54|          16|       38|
|   2011|    6|UCLA                   |Washington State            |          28|          25|        3|
|   2011|    6|Utah State             |Wyoming                     |          63|          19|       44|
|   2011|    6|Virginia Tech          |Miami (FL)                  |          38|          35|        3|
|   2011|    6|Wake Forest            |Florida State               |          35|          30|        5|
|   2011|    6|West Virginia          |Connecticut                 |          43|          16|       27|
|   2011|    6|Western Michigan       |Bowling Green State         |          45|          21|       24|
|   2011|    7|Air Force              |San Diego State             |          27|          41|      -14|
|   2011|    7|California             |Southern California         |           9|          30|      -21|
|   2011|    7|San Jose State         |Hawaii                      |          28|          27|        1|
|   2011|    7|Mississippi            |Alabama                     |           7|          52|      -45|
|   2011|    7|Auburn                 |Florida                     |          17|           6|       11|
|   2011|    7|Ohio                   |Ball State                  |          20|          23|       -3|
|   2011|    7|Colorado State         |Boise State                 |          13|          63|      -50|
|   2011|    7|Oregon State           |Brigham Young               |          28|          38|      -10|
|   2011|    7|Cincinnati             |Louisville                  |          25|          16|        9|
|   2011|    7|Maryland               |Clemson                     |          45|          56|      -11|
|   2011|    7|Connecticut            |South Florida               |          16|          10|        6|
|   2011|    7|Memphis                |East Carolina               |          17|          35|      -18|
|   2011|    7|Central Michigan       |Eastern Michigan            |          28|          35|       -7|
|   2011|    7|Duke                   |Florida State               |          16|          41|      -25|
|   2011|    7|Fresno State           |Utah State                  |          31|          21|       10|
|   2011|    7|Vanderbilt             |Georgia                     |          28|          33|       -5|
|   2011|    7|Iowa                   |Northwestern                |          41|          31|       10|
|   2011|    7|Texas Tech             |Kansas State                |          34|          41|       -7|
|   2011|    7|Tennessee              |Louisiana State             |           7|          38|      -31|
|   2011|    7|Louisiana-Lafayette    |North Texas                 |          30|          10|       20|
|   2011|    7|Troy                   |Louisiana-Monroe            |          10|          38|      -28|
|   2011|    7|Marshall               |Rice                        |          24|          20|        4|
|   2011|    7|North Carolina         |Miami (FL)                  |          24|          30|       -6|
|   2011|    7|Kent State             |Miami (OH)                  |           3|           9|       -6|
|   2011|    7|Michigan State         |Michigan                    |          28|          14|       14|
|   2011|    7|Missouri               |Iowa State                  |          52|          17|       35|
|   2011|    7|Nevada                 |New Mexico                  |          49|           7|       42|
|   2011|    7|New Mexico State       |Idaho                       |          31|          24|        7|
|   2011|    7|Northern Illinois      |Western Michigan            |          51|          22|       29|
|   2011|    7|Illinois               |Ohio State                  |           7|          17|      -10|
|   2011|    7|Kansas                 |Oklahoma                    |          17|          47|      -30|
|   2011|    7|Texas                  |Oklahoma State              |          26|          38|      -12|
|   2011|    7|Oregon                 |Arizona State               |          41|          27|       14|
|   2011|    7|Penn State             |Purdue                      |          23|          18|        5|
|   2011|    7|Rutgers                |Navy                        |          21|          20|        1|
|   2011|    7|Mississippi State      |South Carolina              |          12|          14|       -2|
|   2011|    7|Southern Methodist     |Central Florida             |          38|          17|       21|
|   2011|    7|Washington State       |Stanford                    |          14|          44|      -30|
|   2011|    7|Temple                 |Buffalo                     |          34|           0|       34|
|   2011|    7|Texas A&M              |Baylor                      |          55|          28|       27|
|   2011|    7|Tulane                 |Texas-El Paso               |           7|          44|      -37|
|   2011|    7|Bowling Green State    |Toledo                      |          21|          28|       -7|
|   2011|    7|Tulsa                  |Alabama-Birmingham          |          37|          20|       17|
|   2011|    7|Pittsburgh             |Utah                        |          14|          26|      -12|
|   2011|    7|Virginia               |Georgia Tech                |          24|          21|        3|
|   2011|    7|Wake Forest            |Virginia Tech               |          17|          38|      -21|
|   2011|    7|Washington             |Colorado                    |          52|          24|       28|
|   2011|    7|Florida Atlantic       |Western Kentucky            |           0|          20|      -20|
|   2011|    7|Wisconsin              |Indiana                     |          59|           7|       52|
|   2011|    7|Wyoming                |Nevada-Las Vegas            |          41|          14|       27|
|   2011|    8|Arkansas State         |Florida International       |          34|          16|       18|
|   2011|    8|Alabama-Birmingham     |Central Florida             |          26|          24|        2|
|   2011|    8|Arizona                |UCLA                        |          48|          12|       36|
|   2011|    8|Louisville             |Rutgers                     |          16|          14|        2|
|   2011|    8|Syracuse               |West Virginia               |          49|          23|       26|
|   2011|    8|Alabama                |Tennessee                   |          37|           6|       31|
|   2011|    8|Mississippi            |Arkansas                    |          24|          29|       -5|
|   2011|    8|Ball State             |Central Michigan            |          31|          27|        4|
|   2011|    8|Boise State            |Air Force                   |          37|          26|       11|
|   2011|    8|Bowling Green State    |Temple                      |          13|          10|        3|
|   2011|    8|Brigham Young          |Idaho State                 |          56|           3|       53|
|   2011|    8|California             |Utah                        |          34|          10|       24|
|   2011|    8|South Florida          |Cincinnati                  |          34|          37|       -3|
|   2011|    8|Clemson                |North Carolina              |          59|          38|       21|
|   2011|    8|Navy                   |East Carolina               |          35|          38|       -3|
|   2011|    8|Eastern Michigan       |Western Michigan            |          14|          10|        4|
|   2011|    8|Florida State          |Maryland                    |          41|          16|       25|
|   2011|    8|Hawaii                 |New Mexico State            |          45|          34|       11|
|   2011|    8|Houston                |Marshall                    |          63|          28|       35|
|   2011|    8|Iowa                   |Indiana                     |          45|          24|       21|
|   2011|    8|Kansas                 |Kansas State                |          21|          59|      -38|
|   2011|    8|Kentucky               |Jacksonville State          |          38|          14|       24|
|   2011|    8|Louisiana State        |Auburn                      |          45|          10|       35|
|   2011|    8|Utah State             |Louisiana Tech              |          17|          24|       -7|
|   2011|    8|Tulane                 |Memphis                     |          17|          33|      -16|
|   2011|    8|Miami (FL)             |Georgia Tech                |          24|           7|       17|
|   2011|    8|Michigan State         |Wisconsin                   |          37|          31|        6|
|   2011|    8|Florida Atlantic       |Middle Tennessee State      |          14|          38|      -24|
|   2011|    8|Minnesota              |Nebraska                    |          14|          41|      -27|
|   2011|    8|Nevada                 |Fresno State                |          45|          38|        7|
|   2011|    8|Virginia               |North Carolina State        |          14|          28|      -14|
|   2011|    8|North Texas            |Louisiana-Monroe            |          38|          21|       17|
|   2011|    8|Buffalo                |Northern Illinois           |          30|          31|       -1|
|   2011|    8|Akron                  |Ohio                        |          20|          37|      -17|
|   2011|    8|Missouri               |Oklahoma State              |          24|          45|      -21|
|   2011|    8|Colorado               |Oregon                      |           2|          45|      -43|
|   2011|    8|Washington State       |Oregon State                |          21|          44|      -23|
|   2011|    8|Northwestern           |Penn State                  |          24|          34|      -10|
|   2011|    8|Purdue                 |Illinois                    |          21|          14|        7|
|   2011|    8|Notre Dame             |Southern California         |          17|          31|      -14|
|   2011|    8|Southern Mississippi   |Southern Methodist          |          27|           3|       24|
|   2011|    8|Stanford               |Washington                  |          65|          21|       44|
|   2011|    8|Iowa State             |Texas A&M                   |          17|          33|      -16|
|   2011|    8|Texas Christian        |New Mexico                  |          69|           0|       69|
|   2011|    8|Oklahoma               |Texas Tech                  |          38|          41|       -3|
|   2011|    8|Texas-El Paso          |Colorado State              |          31|          17|       14|
|   2011|    8|Toledo                 |Miami (OH)                  |          49|          28|       21|
|   2011|    8|Rice                   |Tulsa                       |          20|          38|      -18|
|   2011|    8|Vanderbilt             |Army                        |          44|          21|       23|
|   2011|    8|Virginia Tech          |Boston College              |          30|          14|       16|
|   2011|    8|Duke                   |Wake Forest                 |          23|          24|       -1|
|   2011|    8|Western Kentucky       |Louisiana-Lafayette         |          42|          23|       19|
|   2011|    9|Florida International  |Troy                        |          23|          20|        3|
|   2011|    9|Pittsburgh             |Connecticut                 |          35|          20|       15|
|   2011|    9|Houston                |Rice                        |          73|          34|       39|
|   2011|    9|Miami (FL)             |Virginia                    |          21|          28|       -7|
|   2011|    9|Texas Christian        |Brigham Young               |          38|          28|       10|
|   2011|    9|New Mexico             |Air Force                   |           0|          42|      -42|
|   2011|    9|Arizona State          |Colorado                    |          48|          14|       34|
|   2011|    9|Vanderbilt             |Arkansas                    |          28|          31|       -3|
|   2011|    9|Arkansas State         |North Texas                 |          37|          14|       23|
|   2011|    9|Army                   |Fordham                     |          55|           0|       55|
|   2011|    9|Auburn                 |Mississippi                 |          41|          23|       18|
|   2011|    9|Maryland               |Boston College              |          17|          28|      -11|
|   2011|    9|Central Florida        |Memphis                     |          41|           0|       41|
|   2011|    9|Akron                  |Central Michigan            |          22|          23|       -1|
|   2011|    9|East Carolina          |Tulane                      |          34|          13|       21|
|   2011|    9|Florida State          |North Carolina State        |          34|           0|       34|
|   2011|    9|Georgia                |Florida                     |          24|          20|        4|
|   2011|    9|Georgia Tech           |Clemson                     |          31|          17|       14|
|   2011|    9|Idaho                  |Hawaii                      |          14|          16|       -2|
|   2011|    9|Texas Tech             |Iowa State                  |           7|          41|      -34|
|   2011|    9|Kent State             |Bowling Green State         |          27|          15|       12|
|   2011|    9|Louisiana Tech         |San Jose State              |          38|          28|       10|
|   2011|    9|Middle Tennessee State |Louisiana-Lafayette         |          20|          45|      -25|
|   2011|    9|Louisville             |Syracuse                    |          27|          10|       17|
|   2011|    9|Marshall               |Alabama-Birmingham          |          59|          14|       45|
|   2011|    9|Miami (OH)             |Buffalo                     |          41|          13|       28|
|   2011|    9|Michigan               |Purdue                      |          36|          14|       22|
|   2011|    9|Minnesota              |Iowa                        |          22|          21|        1|
|   2011|    9|Kentucky               |Mississippi State           |          16|          28|      -12|
|   2011|    9|Texas A&M              |Missouri                    |          31|          38|       -7|
|   2011|    9|Nebraska               |Michigan State              |          24|           3|       21|
|   2011|    9|New Mexico State       |Nevada                      |          34|          48|      -14|
|   2011|    9|Nevada-Las Vegas       |Colorado State              |          38|          35|        3|
|   2011|    9|North Carolina         |Wake Forest                 |          49|          24|       25|
|   2011|    9|Indiana                |Northwestern                |          38|          59|      -21|
|   2011|    9|Notre Dame             |Navy                        |          56|          14|       42|
|   2011|    9|Ohio State             |Wisconsin                   |          33|          29|        4|
|   2011|    9|Kansas State           |Oklahoma                    |          17|          58|      -41|
|   2011|    9|Oklahoma State         |Baylor                      |          59|          24|       35|
|   2011|    9|Oregon                 |Washington State            |          43|          28|       15|
|   2011|    9|Penn State             |Illinois                    |          10|           7|        3|
|   2011|    9|Tennessee              |South Carolina              |           3|          14|      -11|
|   2011|    9|Texas-El Paso          |Southern Mississippi        |          13|          31|      -18|
|   2011|    9|Southern California    |Stanford                    |          48|          56|       -8|
|   2011|    9|Texas                  |Kansas                      |          43|           0|       43|
|   2011|    9|Tulsa                  |Southern Methodist          |          38|           7|       31|
|   2011|    9|UCLA                   |California                  |          31|          14|       17|
|   2011|    9|Utah                   |Oregon State                |          27|           8|       19|
|   2011|    9|Duke                   |Virginia Tech               |          10|          14|       -4|
|   2011|    9|Washington             |Arizona                     |          42|          31|       11|
|   2011|    9|Rutgers                |West Virginia               |          31|          41|      -10|
|   2011|    9|Louisiana-Monroe       |Western Kentucky            |          28|          31|       -3|
|   2011|    9|Western Michigan       |Ball State                  |          45|          35|       10|
|   2011|    9|San Diego State        |Wyoming                     |          27|          30|       -3|
|   2011|   10|Toledo                 |Northern Illinois           |          60|          63|       -3|
|   2011|   10|Ohio                   |Temple                      |          35|          31|        4|
|   2011|   10|Boston College         |Florida State               |           7|          38|      -31|
|   2011|   10|Miami (OH)             |Akron                       |          35|           3|       32|
|   2011|   10|Central Florida        |Tulsa                       |          17|          24|       -7|
|   2011|   10|Kent State             |Central Michigan            |          24|          21|        3|
|   2011|   10|Colorado               |Southern California         |          17|          42|      -25|
|   2011|   10|Air Force              |Army                        |          24|          14|       10|
|   2011|   10|Arkansas               |South Carolina              |          44|          28|       16|
|   2011|   10|Florida Atlantic       |Arkansas State              |          21|          39|      -18|
|   2011|   10|Eastern Michigan       |Ball State                  |          31|          33|       -2|
|   2011|   10|Baylor                 |Missouri                    |          42|          39|        3|
|   2011|   10|Nevada-Las Vegas       |Boise State                 |          21|          48|      -27|
|   2011|   10|California             |Washington State            |          30|           7|       23|
|   2011|   10|Pittsburgh             |Cincinnati                  |          23|          26|       -3|
|   2011|   10|Connecticut            |Syracuse                    |          28|          21|        7|
|   2011|   10|Florida                |Vanderbilt                  |          26|          21|        5|
|   2011|   10|Georgia                |New Mexico State            |          63|          16|       47|
|   2011|   10|Alabama-Birmingham     |Houston                     |          13|          56|      -43|
|   2011|   10|San Jose State         |Idaho                       |          29|          32|       -3|
|   2011|   10|Iowa                   |Michigan                    |          24|          16|        8|
|   2011|   10|Iowa State             |Kansas                      |          13|          10|        3|
|   2011|   10|Kentucky               |Mississippi                 |          30|          13|       17|
|   2011|   10|Alabama                |Louisiana State             |           6|           9|       -3|
|   2011|   10|Fresno State           |Louisiana Tech              |          21|          41|      -20|
|   2011|   10|Louisiana-Lafayette    |Louisiana-Monroe            |          36|          35|        1|
|   2011|   10|West Virginia          |Louisville                  |          35|          38|       -3|
|   2011|   10|Miami (FL)             |Duke                        |          49|          14|       35|
|   2011|   10|Michigan State         |Minnesota                   |          31|          24|        7|
|   2011|   10|Mississippi State      |Tennessee-Martin            |          55|          17|       38|
|   2011|   10|Navy                   |Troy                        |          42|          14|       28|
|   2011|   10|North Carolina State   |North Carolina              |          13|           0|       13|
|   2011|   10|Nebraska               |Northwestern                |          25|          28|       -3|
|   2011|   10|Wake Forest            |Notre Dame                  |          17|          24|       -7|
|   2011|   10|Ohio State             |Indiana                     |          34|          20|       14|
|   2011|   10|Oklahoma               |Texas A&M                   |          41|          25|       16|
|   2011|   10|Oklahoma State         |Kansas State                |          52|          45|        7|
|   2011|   10|Washington             |Oregon                      |          17|          34|      -17|
|   2011|   10|Rice                   |Texas-El Paso               |          41|          37|        4|
|   2011|   10|Rutgers                |South Florida               |          20|          17|        3|
|   2011|   10|San Diego State        |New Mexico                  |          35|           7|       28|
|   2011|   10|Southern Methodist     |Tulane                      |          45|          24|       21|
|   2011|   10|East Carolina          |Southern Mississippi        |          28|          48|      -20|
|   2011|   10|Oregon State           |Stanford                    |          13|          38|      -25|
|   2011|   10|Tennessee              |Middle Tennessee State      |          24|           0|       24|
|   2011|   10|Texas                  |Texas Tech                  |          52|          20|       32|
|   2011|   10|Wyoming                |Texas Christian             |          20|          31|      -11|
|   2011|   10|UCLA                   |Arizona State               |          29|          28|        1|
|   2011|   10|Arizona                |Utah                        |          21|          34|      -13|
|   2011|   10|Hawaii                 |Utah State                  |          31|          35|       -4|
|   2011|   10|Maryland               |Virginia                    |          13|          31|      -18|
|   2011|   10|Western Kentucky       |Florida International       |          10|           9|        1|
|   2011|   10|Wisconsin              |Purdue                      |          62|          17|       45|
|   2011|   11|Bowling Green State    |Northern Illinois           |          14|          45|      -31|
|   2011|   11|Toledo                 |Western Michigan            |          66|          63|        3|
|   2011|   11|Temple                 |Miami (OH)                  |          24|          21|        3|
|   2011|   11|Tulane                 |Houston                     |          17|          73|      -56|
|   2011|   11|Central Michigan       |Ohio                        |          28|          43|      -15|
|   2011|   11|Georgia Tech           |Virginia Tech               |          26|          37|      -11|
|   2011|   11|Syracuse               |South Florida               |          17|          37|      -20|
|   2011|   11|Mississippi State      |Alabama                     |           7|          24|      -17|
|   2011|   11|Memphis                |Alabama-Birmingham          |          35|          41|       -6|
|   2011|   11|Arkansas               |Tennessee                   |          49|           7|       42|
|   2011|   11|Arkansas State         |Louisiana-Lafayette         |          30|          21|        9|
|   2011|   11|Kansas                 |Baylor                      |          30|          31|       -1|
|   2011|   11|Boston College         |North Carolina State        |          14|          10|        4|
|   2011|   11|Brigham Young          |Idaho                       |          42|           7|       35|
|   2011|   11|California             |Oregon State                |          23|           6|       17|
|   2011|   11|Clemson                |Wake Forest                 |          31|          28|        3|
|   2011|   11|Colorado               |Arizona                     |          48|          29|       19|
|   2011|   11|Eastern Michigan       |Buffalo                     |          30|          17|       13|
|   2011|   11|Florida International  |Florida Atlantic            |          41|           7|       34|
|   2011|   11|Florida State          |Miami (FL)                  |          23|          19|        4|
|   2011|   11|Georgia                |Auburn                      |          45|           7|       38|
|   2011|   11|Kansas State           |Texas A&M                   |          53|          50|        3|
|   2011|   11|Akron                  |Kent State                  |           3|          35|      -32|
|   2011|   11|Louisiana State        |Western Kentucky            |          42|           9|       33|
|   2011|   11|Mississippi            |Louisiana Tech              |           7|          27|      -20|
|   2011|   11|Louisiana-Monroe       |Middle Tennessee State      |          42|          14|       28|
|   2011|   11|Illinois               |Michigan                    |          14|          31|      -17|
|   2011|   11|Iowa                   |Michigan State              |          21|          37|      -16|
|   2011|   11|Missouri               |Texas                       |          17|           5|       12|
|   2011|   11|Southern Methodist     |Navy                        |          17|          24|       -7|
|   2011|   11|Penn State             |Nebraska                    |          14|          17|       -3|
|   2011|   11|Nevada                 |Hawaii                      |          42|          28|       14|
|   2011|   11|New Mexico             |Nevada-Las Vegas            |          21|          14|        7|
|   2011|   11|New Mexico State       |Fresno State                |          48|          45|        3|
|   2011|   11|Troy                   |North Texas                 |          33|          38|       -5|
|   2011|   11|Northwestern           |Rice                        |          28|           6|       22|
|   2011|   11|Maryland               |Notre Dame                  |          21|          45|      -24|
|   2011|   11|Texas Tech             |Oklahoma State              |           6|          66|      -60|
|   2011|   11|Stanford               |Oregon                      |          30|          53|      -23|
|   2011|   11|Louisville             |Pittsburgh                  |          14|          21|       -7|
|   2011|   11|Purdue                 |Ohio State                  |          26|          23|        3|
|   2011|   11|Rutgers                |Army                        |          27|          12|       15|
|   2011|   11|Colorado State         |San Diego State             |          15|          18|       -3|
|   2011|   11|South Carolina         |Florida                     |          17|          12|        5|
|   2011|   11|Southern California    |Washington                  |          40|          17|       23|
|   2011|   11|Southern Mississippi   |Central Florida             |          30|          29|        1|
|   2011|   11|Boise State            |Texas Christian             |          35|          36|       -1|
|   2011|   11|Texas-El Paso          |East Carolina               |          22|          17|        5|
|   2011|   11|Tulsa                  |Marshall                    |          59|          17|       42|
|   2011|   11|Utah                   |UCLA                        |          31|           6|       25|
|   2011|   11|Utah State             |San Jose State              |          34|          33|        1|
|   2011|   11|Vanderbilt             |Kentucky                    |          38|           8|       30|
|   2011|   11|Virginia               |Duke                        |          31|          21|       10|
|   2011|   11|Washington State       |Arizona State               |          37|          27|       10|
|   2011|   11|Cincinnati             |West Virginia               |          21|          24|       -3|
|   2011|   11|Minnesota              |Wisconsin                   |          13|          42|      -29|
|   2011|   11|Air Force              |Wyoming                     |          17|          25|       -8|
|   2011|   12|Northern Illinois      |Ball State                  |          41|          38|        3|
|   2011|   12|Bowling Green State    |Ohio                        |          28|          29|       -1|
|   2011|   12|Miami (OH)             |Western Michigan            |          21|          24|       -3|
|   2011|   12|Alabama-Birmingham     |Southern Mississippi        |          34|          31|        3|
|   2011|   12|Memphis                |Marshall                    |          22|          23|       -1|
|   2011|   12|Virginia Tech          |North Carolina              |          24|          21|        3|
|   2011|   12|Iowa State             |Oklahoma State              |          37|          31|        6|
|   2011|   12|Central Michigan       |Toledo                      |          17|          44|      -27|
|   2011|   12|Air Force              |Nevada-Las Vegas            |          45|          17|       28|
|   2011|   12|Alabama                |Georgia Southern            |          45|          21|       24|
|   2011|   12|Arizona State          |Arizona                     |          27|          31|       -4|
|   2011|   12|Arkansas               |Mississippi State           |          44|          17|       27|
|   2011|   12|Middle Tennessee State |Arkansas State              |          19|          45|      -26|
|   2011|   12|Auburn                 |Samford                     |          35|          16|       19|
|   2011|   12|Baylor                 |Oklahoma                    |          45|          38|        7|
|   2011|   12|San Diego State        |Boise State                 |          35|          52|      -17|
|   2011|   12|Brigham Young          |New Mexico State            |          42|           7|       35|
|   2011|   12|Buffalo                |Akron                       |          51|          10|       41|
|   2011|   12|East Carolina          |Central Florida             |          38|          31|        7|
|   2011|   12|Florida                |Furman                      |          54|          32|       22|
|   2011|   12|Louisiana-Monroe       |Florida International       |          17|          28|      -11|
|   2011|   12|Hawaii                 |Fresno State                |          21|          24|       -3|
|   2011|   12|Georgia                |Kentucky                    |          19|          10|        9|
|   2011|   12|Duke                   |Georgia Tech                |          31|          38|       -7|
|   2011|   12|Houston                |Southern Methodist          |          37|           7|       30|
|   2011|   12|Purdue                 |Iowa                        |          21|          31|      -10|
|   2011|   12|Texas                  |Kansas State                |          13|          17|       -4|
|   2011|   12|Kent State             |Eastern Michigan            |          28|          22|        6|
|   2011|   12|Mississippi            |Louisiana State             |           3|          52|      -49|
|   2011|   12|Nevada                 |Louisiana Tech              |          20|          24|       -4|
|   2011|   12|Connecticut            |Louisville                  |          20|          34|      -14|
|   2011|   12|South Florida          |Miami (FL)                  |           3|           6|       -3|
|   2011|   12|Michigan               |Nebraska                    |          45|          17|       28|
|   2011|   12|Michigan State         |Indiana                     |          55|           3|       52|
|   2011|   12|Missouri               |Texas Tech                  |          31|          27|        4|
|   2011|   12|North Carolina State   |Clemson                     |          37|          13|       24|
|   2011|   12|Northwestern           |Minnesota                   |          28|          13|       15|
|   2011|   12|Notre Dame             |Boston College              |          16|          14|        2|
|   2011|   12|Oregon State           |Washington                  |          38|          21|       17|
|   2011|   12|Ohio State             |Penn State                  |          14|          20|       -6|
|   2011|   12|Rice                   |Tulane                      |          19|           7|       12|
|   2011|   12|Rutgers                |Cincinnati                  |          20|           3|       17|
|   2011|   12|San Jose State         |Navy                        |          27|          24|        3|
|   2011|   12|South Carolina         |Citadel                     |          41|          20|       21|
|   2011|   12|Oregon                 |Southern California         |          35|          38|       -3|
|   2011|   12|Stanford               |California                  |          31|          28|        3|
|   2011|   12|Temple                 |Army                        |          42|          14|       28|
|   2011|   12|Tennessee              |Vanderbilt                  |          27|          21|        6|
|   2011|   12|Texas A&M              |Kansas                      |          61|           7|       54|
|   2011|   12|Texas Christian        |Colorado State              |          34|          10|       24|
|   2011|   12|Troy                   |Florida Atlantic            |          34|           7|       27|
|   2011|   12|Texas-El Paso          |Tulsa                       |          28|          57|      -29|
|   2011|   12|UCLA                   |Colorado                    |          45|           6|       39|
|   2011|   12|Washington State       |Utah                        |          27|          30|       -3|
|   2011|   12|Idaho                  |Utah State                  |          42|          49|       -7|
|   2011|   12|Florida State          |Virginia                    |          13|          14|       -1|
|   2011|   12|Wake Forest            |Maryland                    |          31|          10|       21|
|   2011|   12|North Texas            |Western Kentucky            |          21|          31|      -10|
|   2011|   12|Illinois               |Wisconsin                   |          17|          28|      -11|
|   2011|   12|Wyoming                |New Mexico                  |          31|          10|       21|
|   2011|   13|Ohio                   |Miami (OH)                  |          21|          14|        7|
|   2011|   13|Texas A&M              |Texas                       |          25|          27|       -2|
|   2011|   13|Miami (FL)             |Boston College              |          17|          24|       -7|
|   2011|   13|Buffalo                |Bowling Green State         |          28|          42|      -14|
|   2011|   13|Arizona State          |California                  |          38|          47|       -9|
|   2011|   13|Central Florida        |Texas-El Paso               |          31|          14|       17|
|   2011|   13|Utah                   |Colorado                    |          14|          17|       -3|
|   2011|   13|Tulsa                  |Houston                     |          16|          48|      -32|
|   2011|   13|Louisiana State        |Arkansas                    |          41|          17|       24|
|   2011|   13|South Florida          |Louisville                  |          24|          34|      -10|
|   2011|   13|Nebraska               |Iowa                        |          20|           7|       13|
|   2011|   13|Northern Illinois      |Eastern Michigan            |          18|          12|        6|
|   2011|   13|Temple                 |Kent State                  |          34|          16|       18|
|   2011|   13|Ball State             |Toledo                      |          28|          45|      -17|
|   2011|   13|West Virginia          |Pittsburgh                  |          21|          20|        1|
|   2011|   13|Western Michigan       |Akron                       |          68|          19|       49|
|   2011|   13|Colorado State         |Air Force                   |          21|          45|      -24|
|   2011|   13|Auburn                 |Alabama                     |          14|          42|      -28|
|   2011|   13|Arizona                |Louisiana-Lafayette         |          45|          37|        8|
|   2011|   13|Baylor                 |Texas Tech                  |          66|          42|       24|
|   2011|   13|Boise State            |Wyoming                     |          36|          14|       22|
|   2011|   13|Syracuse               |Cincinnati                  |          13|          30|      -17|
|   2011|   13|Connecticut            |Rutgers                     |          40|          22|       18|
|   2011|   13|Florida Atlantic       |Alabama-Birmingham          |          38|          35|        3|
|   2011|   13|Middle Tennessee State |Florida International       |          18|          31|      -13|
|   2011|   13|Florida                |Florida State               |           7|          21|      -14|
|   2011|   13|Georgia Tech           |Georgia                     |          17|          31|      -14|
|   2011|   13|Hawaii                 |Tulane                      |          35|          23|       12|
|   2011|   13|Kentucky               |Tennessee                   |          10|           7|        3|
|   2011|   13|Louisiana Tech         |New Mexico State            |          44|           0|       44|
|   2011|   13|Marshall               |East Carolina               |          34|          27|        7|
|   2011|   13|Michigan               |Ohio State                  |          40|          34|        6|
|   2011|   13|Northwestern           |Michigan State              |          17|          31|      -14|
|   2011|   13|Minnesota              |Illinois                    |          27|           7|       20|
|   2011|   13|Mississippi State      |Mississippi                 |          31|           3|       28|
|   2011|   13|Missouri               |Kansas                      |          24|          10|       14|
|   2011|   13|North Carolina         |Duke                        |          37|          21|       16|
|   2011|   13|North Carolina State   |Maryland                    |          56|          41|       15|
|   2011|   13|Oklahoma               |Iowa State                  |          26|           6|       20|
|   2011|   13|Oregon                 |Oregon State                |          49|          21|       28|
|   2011|   13|Indiana                |Purdue                      |          25|          33|       -8|
|   2011|   13|Nevada-Las Vegas       |San Diego State             |          14|          31|      -17|
|   2011|   13|Fresno State           |San Jose State              |          24|          27|       -3|
|   2011|   13|South Carolina         |Clemson                     |          34|          13|       21|
|   2011|   13|Southern California    |UCLA                        |          50|           0|       50|
|   2011|   13|Southern Methodist     |Rice                        |          27|          24|        3|
|   2011|   13|Southern Mississippi   |Memphis                     |          44|           7|       37|
|   2011|   13|Stanford               |Notre Dame                  |          28|          14|       14|
|   2011|   13|Utah State             |Nevada                      |          21|          17|        4|
|   2011|   13|Wake Forest            |Vanderbilt                  |           7|          41|      -34|
|   2011|   13|Virginia               |Virginia Tech               |           0|          38|      -38|
|   2011|   13|Washington             |Washington State            |          38|          21|       17|
|   2011|   13|Western Kentucky       |Troy                        |          41|          18|       23|
|   2011|   13|Wisconsin              |Penn State                  |          45|           7|       38|
|   2011|   14|South Florida          |West Virginia               |          27|          30|       -3|
|   2011|   14|Northern Illinois      |Ohio                        |          23|          20|        3|
|   2011|   14|Oregon                 |UCLA                        |          49|          31|       18|
|   2011|   14|Arkansas State         |Troy                        |          45|          14|       31|
|   2011|   14|Baylor                 |Texas                       |          48|          24|       24|
|   2011|   14|Boise State            |New Mexico                  |          45|           0|       45|
|   2011|   14|Hawaii                 |Brigham Young               |          20|          41|      -21|
|   2011|   14|Cincinnati             |Connecticut                 |          35|          27|        8|
|   2011|   14|Clemson                |Virginia Tech               |          38|          10|       28|
|   2011|   14|Kansas State           |Iowa State                  |          30|          23|        7|
|   2011|   14|Louisiana State        |Georgia                     |          42|          10|       32|
|   2011|   14|Florida Atlantic       |Louisiana-Monroe            |           0|          26|      -26|
|   2011|   14|Nevada                 |Idaho                       |          56|           3|       53|
|   2011|   14|North Texas            |Middle Tennessee State      |          59|           7|       52|
|   2011|   14|Oklahoma State         |Oklahoma                    |          44|          10|       34|
|   2011|   14|Pittsburgh             |Syracuse                    |          33|          20|       13|
|   2011|   14|San Diego State        |Fresno State                |          35|          28|        7|
|   2011|   14|Houston                |Southern Mississippi        |          28|          49|      -21|
|   2011|   14|Texas Christian        |Nevada-Las Vegas            |          56|           9|       47|
|   2011|   14|New Mexico State       |Utah State                  |          21|          24|       -3|
|   2011|   14|Wisconsin              |Michigan State              |          42|          39|        3|
|   2011|   14|Colorado State         |Wyoming                     |          19|          22|       -3|
|   2011|   15|Navy                   |Army                        |          27|          21|        6|
|   2011|   16|Louisiana-Lafayette    |San Diego State             |          32|          30|        2|
|   2011|   16|Ohio                   |Utah State                  |          24|          23|        1|
|   2011|   16|Temple                 |Wyoming                     |          37|          15|       22|
|   2011|   17|Marshall               |Florida International       |          20|          10|       10|
|   2011|   17|Texas Christian        |Louisiana Tech              |          31|          24|        7|
|   2011|   17|Boise State            |Arizona State               |          56|          24|       32|
|   2011|   17|Southern Mississippi   |Nevada                      |          24|          17|        7|
|   2011|   17|Missouri               |North Carolina              |          41|          24|       17|
|   2011|   18|North Carolina State   |Louisville                  |          31|          24|        7|
|   2011|   18|Purdue                 |Western Michigan            |          37|          32|        5|
|   2011|   18|Texas                  |California                  |          21|          10|       11|
|   2011|   18|Toledo                 |Air Force                   |          42|          41|        1|
|   2011|   18|Baylor                 |Washington                  |          67|          56|       11|
|   2011|   18|Florida State          |Notre Dame                  |          18|          14|        4|
|   2011|   18|Brigham Young          |Tulsa                       |          24|          21|        3|
|   2011|   18|Mississippi State      |Wake Forest                 |          23|          17|        6|
|   2011|   18|Oklahoma               |Iowa                        |          31|          14|       17|
|   2011|   18|Rutgers                |Iowa State                  |          27|          13|       14|
|   2011|   18|Auburn                 |Virginia                    |          43|          24|       19|
|   2011|   18|Cincinnati             |Vanderbilt                  |          31|          24|        7|
|   2011|   18|Illinois               |UCLA                        |          20|          14|        6|
|   2011|   18|Texas A&M              |Northwestern                |          33|          22|       11|
|   2011|   18|Utah                   |Georgia Tech                |          30|          27|        3|
|   2011|   19|Florida                |Ohio State                  |          24|          17|        7|
|   2011|   19|Houston                |Penn State                  |          30|          14|       16|
|   2011|   19|Michigan State         |Georgia                     |          33|          30|        3|
|   2011|   19|Oklahoma State         |Stanford                    |          41|          38|        3|
|   2011|   19|Oregon                 |Wisconsin                   |          45|          38|        7|
|   2011|   19|South Carolina         |Nebraska                    |          30|          13|       17|
|   2011|   20|Michigan               |Virginia Tech               |          23|          20|        3|
|   2011|   20|West Virginia          |Clemson                     |          70|          33|       37|
|   2011|   20|Arkansas               |Kansas State                |          29|          16|       13|
|   2011|   20|Southern Methodist     |Pittsburgh                  |          28|           6|       22|
|   2011|   20|Northern Illinois      |Arkansas State              |          38|          20|       18|
|   2011|   20|Alabama                |Louisiana State             |          21|           0|       21|
|   2012|    1|Arizona State          |Northern Arizona            |          63|           6|       57|
|   2012|    1|Ball State             |Eastern Michigan            |          37|          26|       11|
|   2012|    1|Brigham Young          |Washington State            |          30|           6|       24|
|   2012|    1|Akron                  |Central Florida             |          14|          56|      -42|
|   2012|    1|Central Michigan       |Southeast Missouri State    |          38|          27|       11|
|   2012|    1|Connecticut            |Massachusetts               |          37|           0|       37|
|   2012|    1|Idaho                  |Eastern Washington          |           3|          20|      -17|
|   2012|    1|Kent State             |Towson                      |          41|          21|       20|
|   2012|    1|Middle Tennessee State |McNeese State               |          21|          27|       -6|
|   2012|    1|Nevada-Las Vegas       |Minnesota                   |          27|          30|       -3|
|   2012|    1|New Mexico State       |Sacramento State            |          49|          19|       30|
|   2012|    1|Vanderbilt             |South Carolina              |          13|          17|       -4|
|   2012|    1|Rice                   |UCLA                        |          24|          49|      -25|
|   2012|    1|Utah                   |Northern Colorado           |          41|           0|       41|
|   2012|    1|Utah State             |Southern Utah               |          34|           3|       31|
|   2012|    1|Florida Atlantic       |Wagner                      |           7|           3|        4|
|   2012|    1|Michigan State         |Boise State                 |          17|          13|        4|
|   2012|    1|Stanford               |San Jose State              |          20|          17|        3|
|   2012|    1|Temple                 |Villanova                   |          41|          10|       31|
|   2012|    1|Tennessee              |North Carolina State        |          35|          21|       14|
|   2012|    1|Air Force              |Idaho State                 |          49|          21|       28|
|   2012|    1|Alabama                |Michigan                    |          41|          14|       27|
|   2012|    1|Arizona                |Toledo                      |          24|          17|        7|
|   2012|    1|Arkansas               |Jacksonville State          |          49|          24|       25|
|   2012|    1|Clemson                |Auburn                      |          26|          19|        7|
|   2012|    1|Colorado State         |Colorado                    |          22|          17|        5|
|   2012|    1|Duke                   |Florida International       |          46|          26|       20|
|   2012|    1|East Carolina          |Appalachian State           |          35|          13|       22|
|   2012|    1|Florida                |Bowling Green State         |          27|          14|       13|
|   2012|    1|Florida State          |Murray State                |          69|           3|       66|
|   2012|    1|Fresno State           |Weber State                 |          37|          10|       27|
|   2012|    1|Georgia                |Buffalo                     |          45|          23|       22|
|   2012|    1|Illinois               |Western Michigan            |          24|           7|       17|
|   2012|    1|Indiana                |Indiana State               |          24|          17|        7|
|   2012|    1|Northern Illinois      |Iowa                        |          17|          18|       -1|
|   2012|    1|Iowa State             |Tulsa                       |          38|          23|       15|
|   2012|    1|Kansas                 |South Dakota State          |          31|          17|       14|
|   2012|    1|Kansas State           |Missouri State              |          51|           9|       42|
|   2012|    1|Louisiana State        |North Texas                 |          41|          14|       27|
|   2012|    1|Louisiana-Lafayette    |Lamar                       |          40|           0|       40|
|   2012|    1|Maryland               |William & Mary              |           7|           6|        1|
|   2012|    1|Boston College         |Miami (FL)                  |          32|          41|       -9|
|   2012|    1|Mississippi            |Central Arkansas            |          49|          27|       22|
|   2012|    1|Mississippi State      |Jackson State               |          56|           9|       47|
|   2012|    1|Missouri               |Southeastern Louisiana      |          62|          10|       52|
|   2012|    1|Nebraska               |Southern Mississippi        |          49|          20|       29|
|   2012|    1|California             |Nevada                      |          24|          31|       -7|
|   2012|    1|New Mexico             |Southern                    |          66|          21|       45|
|   2012|    1|North Carolina         |Elon                        |          62|           0|       62|
|   2012|    1|Syracuse               |Northwestern                |          41|          42|       -1|
|   2012|    1|Notre Dame             |Navy                        |          50|          10|       40|
|   2012|    1|Penn State             |Ohio                        |          14|          24|      -10|
|   2012|    1|Ohio State             |Miami (OH)                  |          56|          10|       46|
|   2012|    1|Texas-El Paso          |Oklahoma                    |           7|          24|      -17|
|   2012|    1|Oklahoma State         |Savannah State              |          84|           0|       84|
|   2012|    1|Oregon                 |Arkansas State              |          57|          34|       23|
|   2012|    1|Purdue                 |Eastern Kentucky            |          48|           6|       42|
|   2012|    1|Tulane                 |Rutgers                     |          12|          24|      -12|
|   2012|    1|South Florida          |Chattanooga                 |          34|          13|       21|
|   2012|    1|Southern California    |Hawaii                      |          49|          10|       39|
|   2012|    1|Memphis                |Tennessee-Martin            |          17|          20|       -3|
|   2012|    1|Texas                  |Wyoming                     |          37|          17|       20|
|   2012|    1|Houston                |Texas State                 |          13|          30|      -17|
|   2012|    1|Texas Tech             |Northwestern State          |          44|           6|       38|
|   2012|    1|South Alabama          |Texas-San Antonio           |          31|          33|       -2|
|   2012|    1|Alabama-Birmingham     |Troy                        |          29|          39|      -10|
|   2012|    1|Virginia               |Richmond                    |          43|          19|       24|
|   2012|    1|Wake Forest            |Liberty                     |          20|          17|        3|
|   2012|    1|Washington             |San Diego State             |          21|          12|        9|
|   2012|    1|West Virginia          |Marshall                    |          69|          34|       35|
|   2012|    1|Western Kentucky       |Austin Peay                 |          49|          10|       39|
|   2012|    1|Wisconsin              |Northern Iowa               |          26|          21|        5|
|   2012|    1|Pittsburgh             |Youngstown State            |          17|          31|      -14|
|   2012|    1|Baylor                 |Southern Methodist          |          59|          24|       35|
|   2012|    1|Louisville             |Kentucky                    |          32|          14|       18|
|   2012|    1|Virginia Tech          |Georgia Tech                |          20|          17|        3|
|   2012|    2|Cincinnati             |Pittsburgh                  |          34|          10|       24|
|   2012|    2|Utah State             |Utah                        |          27|          20|        7|
|   2012|    2|Alabama                |Western Kentucky            |          35|           0|       35|
|   2012|    2|Arizona                |Oklahoma State              |          59|          38|       21|
|   2012|    2|Arizona State          |Illinois                    |          45|          14|       31|
|   2012|    2|Arkansas State         |Memphis                     |          33|          28|        5|
|   2012|    2|Boston College         |Maine                       |          34|           3|       31|
|   2012|    2|Bowling Green State    |Idaho                       |          21|          13|        8|
|   2012|    2|Brigham Young          |Weber State                 |          45|          13|       32|
|   2012|    2|Buffalo                |Morgan State                |          56|          34|       22|
|   2012|    2|California             |Southern Utah               |          50|          31|       19|
|   2012|    2|Clemson                |Ball State                  |          52|          27|       25|
|   2012|    2|Texas A&M              |Florida                     |          17|          20|       -3|
|   2012|    2|Florida International  |Akron                       |          41|          38|        3|
|   2012|    2|Florida State          |Savannah State              |          55|           0|       55|
|   2012|    2|Missouri               |Georgia                     |          20|          41|      -21|
|   2012|    2|Georgia Tech           |Presbyterian                |          59|           3|       56|
|   2012|    2|Eastern Michigan       |Illinois State              |          14|          31|      -17|
|   2012|    2|Massachusetts          |Indiana                     |           6|          45|      -39|
|   2012|    2|Iowa                   |Iowa State                  |           6|           9|       -3|
|   2012|    2|Kansas State           |Miami (FL)                  |          52|          13|       39|
|   2012|    2|Kentucky               |Kent State                  |          47|          14|       33|
|   2012|    2|Louisiana State        |Washington                  |          41|           3|       38|
|   2012|    2|Houston                |Louisiana Tech              |          49|          56|       -7|
|   2012|    2|Troy                   |Louisiana-Lafayette         |          24|          37|      -13|
|   2012|    2|Arkansas               |Louisiana-Monroe            |          31|          34|       -3|
|   2012|    2|Louisville             |Missouri State              |          35|           7|       28|
|   2012|    2|Marshall               |Western Carolina            |          52|          24|       28|
|   2012|    2|Temple                 |Maryland                    |          27|          36|       -9|
|   2012|    2|Miami (OH)             |Southern Illinois           |          30|          14|       16|
|   2012|    2|Michigan               |Air Force                   |          31|          25|        6|
|   2012|    2|Central Michigan       |Michigan State              |           7|          41|      -34|
|   2012|    2|Middle Tennessee State |Florida Atlantic            |          31|          17|       14|
|   2012|    2|Minnesota              |New Hampshire               |          44|           7|       37|
|   2012|    2|Mississippi            |Texas-El Paso               |          28|          10|       18|
|   2012|    2|Mississippi State      |Auburn                      |          28|          10|       18|
|   2012|    2|Connecticut            |North Carolina State        |           7|          10|       -3|
|   2012|    2|Colorado State         |North Dakota State          |           7|          22|      -15|
|   2012|    2|North Texas            |Texas Southern              |          34|           7|       27|
|   2012|    2|Nevada-Las Vegas       |Northern Arizona            |          14|          17|       -3|
|   2012|    2|Northern Illinois      |Tennessee-Martin            |          35|           7|       28|
|   2012|    2|Northwestern           |Vanderbilt                  |          23|          13|       10|
|   2012|    2|Notre Dame             |Purdue                      |          20|          17|        3|
|   2012|    2|Ohio                   |New Mexico State            |          51|          24|       27|
|   2012|    2|Ohio State             |Central Florida             |          31|          16|       15|
|   2012|    2|Oklahoma               |Florida A&M                 |          69|          13|       56|
|   2012|    2|Oregon                 |Fresno State                |          42|          25|       17|
|   2012|    2|Oregon State           |Wisconsin                   |          10|           7|        3|
|   2012|    2|Kansas                 |Rice                        |          24|          25|       -1|
|   2012|    2|Rutgers                |Howard                      |          26|           0|       26|
|   2012|    2|Colorado               |Sacramento State            |          28|          30|       -2|
|   2012|    2|San Diego State        |Army                        |          42|           7|       35|
|   2012|    2|San Jose State         |California-Davis            |          45|          13|       32|
|   2012|    2|South Alabama          |Nicholls State              |           9|           3|        6|
|   2012|    2|South Carolina         |East Carolina               |          48|          10|       38|
|   2012|    2|Nevada                 |South Florida               |          31|          32|       -1|
|   2012|    2|Syracuse               |Southern California         |          29|          42|      -13|
|   2012|    2|Southern Methodist     |Stephen F. Austin           |          52|           0|       52|
|   2012|    2|Stanford               |Duke                        |          50|          13|       37|
|   2012|    2|Tennessee              |Georgia State               |          51|          13|       38|
|   2012|    2|Texas                  |New Mexico                  |          45|           0|       45|
|   2012|    2|Texas Christian        |Grambling State             |          56|           0|       56|
|   2012|    2|Texas State            |Texas Tech                  |          10|          58|      -48|
|   2012|    2|Texas-San Antonio      |Texas A&M-Commerce          |          27|          16|       11|
|   2012|    2|Wyoming                |Toledo                      |          31|          34|       -3|
|   2012|    2|Tulsa                  |Tulane                      |          45|          10|       35|
|   2012|    2|UCLA                   |Nebraska                    |          36|          30|        6|
|   2012|    2|Virginia               |Penn State                  |          17|          16|        1|
|   2012|    2|Virginia Tech          |Austin Peay                 |          42|           7|       35|
|   2012|    2|Wake Forest            |North Carolina              |          28|          27|        1|
|   2012|    2|Washington State       |Eastern Washington          |          24|          20|        4|
|   2012|    2|Western Michigan       |Eastern Illinois            |          52|          21|       31|
|   2012|    3|South Florida          |Rutgers                     |          13|          23|      -10|
|   2012|    3|Nevada-Las Vegas       |Washington State            |          27|          35|       -8|
|   2012|    3|Akron                  |Morgan State                |          66|           6|       60|
|   2012|    3|Arkansas               |Alabama                     |           0|          52|      -52|
|   2012|    3|Arizona                |South Carolina State        |          56|           0|       56|
|   2012|    3|Auburn                 |Louisiana-Monroe            |          31|          28|        3|
|   2012|    3|Indiana                |Ball State                  |          39|          41|       -2|
|   2012|    3|Baylor                 |Sam Houston State           |          48|          23|       25|
|   2012|    3|Boise State            |Miami (OH)                  |          39|          12|       27|
|   2012|    3|Wyoming                |Cal Poly                    |          22|          24|       -2|
|   2012|    3|Central Florida        |Florida International       |          33|          20|       13|
|   2012|    3|Cincinnati             |Delaware State              |          23|           7|       16|
|   2012|    3|Clemson                |Furman                      |          41|           7|       34|
|   2012|    3|Maryland               |Connecticut                 |          21|          24|       -3|
|   2012|    3|Duke                   |North Carolina Central      |          54|          17|       37|
|   2012|    3|Southern Mississippi   |East Carolina               |          14|          24|      -10|
|   2012|    3|Tennessee              |Florida                     |          20|          37|      -17|
|   2012|    3|Florida State          |Wake Forest                 |          52|           0|       52|
|   2012|    3|Fresno State           |Colorado                    |          69|          14|       55|
|   2012|    3|Georgia                |Florida Atlantic            |          56|          20|       36|
|   2012|    3|Georgia Tech           |Virginia                    |          56|          20|       36|
|   2012|    3|Hawaii                 |Lamar                       |          54|           2|       52|
|   2012|    3|Illinois               |Charleston Southern         |          44|           0|       44|
|   2012|    3|Iowa                   |Northern Iowa               |          27|          16|       11|
|   2012|    3|Iowa State             |Western Illinois            |          37|           3|       34|
|   2012|    3|Kansas State           |North Texas                 |          35|          21|       14|
|   2012|    3|Louisiana State        |Idaho                       |          63|          14|       49|
|   2012|    3|Louisiana Tech         |Rice                        |          56|          37|       19|
|   2012|    3|Louisville             |North Carolina              |          39|          34|        5|
|   2012|    3|Miami (FL)             |Bethune-Cookman             |          38|          10|       28|
|   2012|    3|Michigan               |Massachusetts               |          63|          13|       50|
|   2012|    3|Memphis                |Middle Tennessee State      |          30|          48|      -18|
|   2012|    3|Minnesota              |Western Michigan            |          28|          23|        5|
|   2012|    3|Troy                   |Mississippi State           |          24|          30|       -6|
|   2012|    3|Missouri               |Arizona State               |          24|          20|        4|
|   2012|    3|Nebraska               |Arkansas State              |          42|          13|       29|
|   2012|    3|Nevada                 |Northwestern State          |          45|          34|       11|
|   2012|    3|North Carolina State   |South Alabama               |          31|           7|       24|
|   2012|    3|Army                   |Northern Illinois           |          40|          41|       -1|
|   2012|    3|Northwestern           |Boston College              |          22|          13|        9|
|   2012|    3|Michigan State         |Notre Dame                  |           3|          20|      -17|
|   2012|    3|Marshall               |Ohio                        |          24|          27|       -3|
|   2012|    3|Ohio State             |California                  |          35|          28|        7|
|   2012|    3|Oklahoma State         |Louisiana-Lafayette         |          65|          24|       41|
|   2012|    3|Oregon                 |Tennessee Tech              |          63|          14|       49|
|   2012|    3|Penn State             |Navy                        |          34|           7|       27|
|   2012|    3|Pittsburgh             |Virginia Tech               |          35|          17|       18|
|   2012|    3|Purdue                 |Eastern Michigan            |          54|          16|       38|
|   2012|    3|San Diego State        |North Dakota                |          49|          41|        8|
|   2012|    3|San Jose State         |Colorado State              |          40|          20|       20|
|   2012|    3|South Carolina         |Alabama-Birmingham          |          49|           6|       43|
|   2012|    3|Stanford               |Southern California         |          21|          14|        7|
|   2012|    3|Syracuse               |Stony Brook                 |          28|          17|       11|
|   2012|    3|Mississippi            |Texas                       |          31|          66|      -35|
|   2012|    3|Southern Methodist     |Texas A&M                   |           3|          48|      -45|
|   2012|    3|Kansas                 |Texas Christian             |           6|          20|      -14|
|   2012|    3|Texas Tech             |New Mexico                  |          49|          14|       35|
|   2012|    3|Texas-El Paso          |New Mexico State            |          41|          28|       13|
|   2012|    3|Georgia State          |Texas-San Antonio           |          14|          38|      -24|
|   2012|    3|Toledo                 |Bowling Green State         |          27|          15|       12|
|   2012|    3|Tulsa                  |Nicholls State              |          66|          16|       50|
|   2012|    3|UCLA                   |Houston                     |          37|           6|       31|
|   2012|    3|Utah                   |Brigham Young               |          24|          21|        3|
|   2012|    3|Vanderbilt             |Presbyterian                |          58|           0|       58|
|   2012|    3|Washington             |Portland State              |          52|          13|       39|
|   2012|    3|West Virginia          |James Madison               |          42|          12|       30|
|   2012|    3|Kentucky               |Western Kentucky            |          31|          32|       -1|
|   2012|    3|Wisconsin              |Utah State                  |          16|          14|        2|
|   2012|    4|Buffalo                |Kent State                  |           7|          23|      -16|
|   2012|    4|Boise State            |Brigham Young               |           7|           6|        1|
|   2012|    4|Louisiana-Monroe       |Baylor                      |          42|          47|       -5|
|   2012|    4|Alabama                |Florida Atlantic            |          40|           7|       33|
|   2012|    4|Arizona State          |Utah                        |          37|           7|       30|
|   2012|    4|Arkansas State         |Alcorn State                |          56|           0|       56|
|   2012|    4|Ball State             |South Florida               |          31|          27|        4|
|   2012|    4|Iowa                   |Central Michigan            |          31|          32|       -1|
|   2012|    4|Washington State       |Colorado                    |          34|          35|       -1|
|   2012|    4|Duke                   |Memphis                     |          38|          14|       24|
|   2012|    4|Florida                |Kentucky                    |          38|           0|       38|
|   2012|    4|Florida State          |Clemson                     |          49|          37|       12|
|   2012|    4|Georgia                |Vanderbilt                  |          48|           3|       45|
|   2012|    4|Oklahoma               |Kansas State                |          19|          24|       -5|
|   2012|    4|Auburn                 |Louisiana State             |          10|          12|       -2|
|   2012|    4|Illinois               |Louisiana Tech              |          24|          52|      -28|
|   2012|    4|Florida International  |Louisville                  |          21|          28|       -7|
|   2012|    4|Rice                   |Marshall                    |          51|          54|       -3|
|   2012|    4|Georgia Tech           |Miami (FL)                  |          36|          42|       -6|
|   2012|    4|Miami (OH)             |Massachusetts               |          27|          16|       11|
|   2012|    4|Michigan State         |Eastern Michigan            |          23|           7|       16|
|   2012|    4|Minnesota              |Syracuse                    |          17|          10|        7|
|   2012|    4|Mississippi            |Tulane                      |          39|           0|       39|
|   2012|    4|Mississippi State      |South Alabama               |          30|          10|       20|
|   2012|    4|Navy                   |Virginia Military Institute |          41|           3|       38|
|   2012|    4|Nebraska               |Idaho State                 |          73|           7|       66|
|   2012|    4|Hawaii                 |Nevada                      |          24|          69|      -45|
|   2012|    4|Nevada-Las Vegas       |Air Force                   |          38|          35|        3|
|   2012|    4|New Mexico State       |New Mexico                  |          14|          27|      -13|
|   2012|    4|North Carolina         |East Carolina               |          27|           6|       21|
|   2012|    4|North Carolina State   |Citadel                     |          52|          14|       38|
|   2012|    4|Northern Illinois      |Kansas                      |          30|          23|        7|
|   2012|    4|Northwestern           |South Dakota                |          38|           7|       31|
|   2012|    4|Notre Dame             |Michigan                    |          13|           6|        7|
|   2012|    4|Ohio                   |Norfolk State               |          44|          10|       34|
|   2012|    4|Ohio State             |Alabama-Birmingham          |          29|          15|       14|
|   2012|    4|Oregon                 |Arizona                     |          49|           0|       49|
|   2012|    4|UCLA                   |Oregon State                |          20|          27|       -7|
|   2012|    4|Penn State             |Temple                      |          24|          13|       11|
|   2012|    4|Pittsburgh             |Gardner-Webb                |          55|          10|       45|
|   2012|    4|Arkansas               |Rutgers                     |          26|          35|       -9|
|   2012|    4|San Diego State        |San Jose State              |          34|          38|       -4|
|   2012|    4|South Carolina         |Missouri                    |          31|          10|       21|
|   2012|    4|Southern California    |California                  |          27|           9|       18|
|   2012|    4|Tennessee              |Akron                       |          47|          26|       21|
|   2012|    4|Texas A&M              |South Carolina State        |          70|          14|       56|
|   2012|    4|Texas Christian        |Virginia                    |          27|           7|       20|
|   2012|    4|Texas State            |Stephen F. Austin           |          41|          37|        4|
|   2012|    4|Texas-San Antonio      |Northwestern Oklahoma State |          56|           3|       53|
|   2012|    4|Toledo                 |Coastal Carolina            |          38|          28|       10|
|   2012|    4|North Texas            |Troy                        |           7|          14|       -7|
|   2012|    4|Tulsa                  |Fresno State                |          27|          26|        1|
|   2012|    4|Colorado State         |Utah State                  |          19|          31|      -12|
|   2012|    4|Virginia Tech          |Bowling Green State         |          37|           0|       37|
|   2012|    4|Wake Forest            |Army                        |          49|          37|       12|
|   2012|    4|West Virginia          |Maryland                    |          31|          21|       10|
|   2012|    4|Western Kentucky       |Southern Mississippi        |          42|          17|       25|
|   2012|    4|Western Michigan       |Connecticut                 |          30|          24|        6|
|   2012|    4|Wisconsin              |Texas-El Paso               |          37|          26|       11|
|   2012|    4|Idaho                  |Wyoming                     |          37|          40|       -3|
|   2012|    5|Washington             |Stanford                    |          17|          13|        4|
|   2012|    5|Brigham Young          |Hawaii                      |          47|           0|       47|
|   2012|    5|Air Force              |Colorado State              |          42|          21|       21|
|   2012|    5|Alabama                |Mississippi                 |          33|          14|       19|
|   2012|    5|California             |Arizona State               |          17|          27|      -10|
|   2012|    5|New Mexico             |Boise State                 |          29|          32|       -3|
|   2012|    5|Bowling Green State    |Rhode Island                |          48|           8|       40|
|   2012|    5|Cincinnati             |Virginia Tech               |          27|          24|        3|
|   2012|    5|Boston College         |Clemson                     |          31|          45|      -14|
|   2012|    5|Connecticut            |Buffalo                     |          24|          17|        7|
|   2012|    5|Wake Forest            |Duke                        |          27|          34|       -7|
|   2012|    5|East Carolina          |Texas-El Paso               |          28|          18|       10|
|   2012|    5|South Florida          |Florida State               |          17|          30|      -13|
|   2012|    5|Fresno State           |San Diego State             |          52|          40|       12|
|   2012|    5|Georgia                |Tennessee                   |          51|          44|        7|
|   2012|    5|Houston                |Rice                        |          35|          14|       21|
|   2012|    5|Iowa                   |Minnesota                   |          31|          13|       18|
|   2012|    5|Kent State             |Ball State                  |          45|          43|        2|
|   2012|    5|Louisiana State        |Towson                      |          38|          22|       16|
|   2012|    5|Virginia               |Louisiana Tech              |          38|          44|       -6|
|   2012|    5|Louisiana-Lafayette    |Florida International       |          48|          20|       28|
|   2012|    5|Tulane                 |Louisiana-Monroe            |          10|          63|      -53|
|   2012|    5|Southern Mississippi   |Louisville                  |          17|          21|       -4|
|   2012|    5|Miami (FL)             |North Carolina State        |          44|          37|        7|
|   2012|    5|Akron                  |Miami (OH)                  |          49|          56|       -7|
|   2012|    5|Georgia Tech           |Middle Tennessee State      |          28|          49|      -21|
|   2012|    5|Central Florida        |Missouri                    |          16|          21|       -5|
|   2012|    5|Nebraska               |Wisconsin                   |          30|          27|        3|
|   2012|    5|Texas State            |Nevada                      |          21|          34|      -13|
|   2012|    5|North Carolina         |Idaho                       |          66|           0|       66|
|   2012|    5|Florida Atlantic       |North Texas                 |          14|          20|       -6|
|   2012|    5|Northern Illinois      |Central Michigan            |          55|          24|       31|
|   2012|    5|Northwestern           |Indiana                     |          44|          29|       15|
|   2012|    5|Massachusetts          |Ohio                        |          34|          37|       -3|
|   2012|    5|Michigan State         |Ohio State                  |          16|          17|       -1|
|   2012|    5|Washington State       |Oregon                      |          26|          51|      -25|
|   2012|    5|Arizona                |Oregon State                |          35|          38|       -3|
|   2012|    5|Illinois               |Penn State                  |           7|          35|      -28|
|   2012|    5|Purdue                 |Marshall                    |          51|          41|       10|
|   2012|    5|Navy                   |San Jose State              |           0|          12|      -12|
|   2012|    5|Kentucky               |South Carolina              |          17|          38|      -21|
|   2012|    5|Army                   |Stony Brook                 |           3|          23|      -20|
|   2012|    5|Oklahoma State         |Texas                       |          36|          41|       -5|
|   2012|    5|Texas A&M              |Arkansas                    |          58|          10|       48|
|   2012|    5|Southern Methodist     |Texas Christian             |          16|          24|       -8|
|   2012|    5|Iowa State             |Texas Tech                  |          13|          24|      -11|
|   2012|    5|New Mexico State       |Texas-San Antonio           |          14|          35|      -21|
|   2012|    5|Western Michigan       |Toledo                      |          17|          37|      -20|
|   2012|    5|South Alabama          |Troy                        |          10|          31|      -21|
|   2012|    5|Alabama-Birmingham     |Tulsa                       |          42|          49|       -7|
|   2012|    5|Colorado               |UCLA                        |          14|          42|      -28|
|   2012|    5|Utah State             |Nevada-Las Vegas            |          35|          13|       22|
|   2012|    5|West Virginia          |Baylor                      |          70|          63|        7|
|   2012|    5|Arkansas State         |Western Kentucky            |          13|          26|      -13|
|   2012|    6|Florida International  |Arkansas State              |          20|          34|      -14|
|   2012|    6|Central Florida        |East Carolina               |          40|          20|       20|
|   2012|    6|Utah                   |Southern California         |          28|          38|      -10|
|   2012|    6|Brigham Young          |Utah State                  |           6|           3|        3|
|   2012|    6|Syracuse               |Pittsburgh                  |          14|          13|        1|
|   2012|    6|Alabama-Birmingham     |Southeastern Louisiana      |          52|           3|       49|
|   2012|    6|Auburn                 |Arkansas                    |           7|          24|      -17|
|   2012|    6|Army                   |Boston College              |          34|          31|        3|
|   2012|    6|Southern Mississippi   |Boise State                 |          14|          40|      -26|
|   2012|    6|Akron                  |Bowling Green State         |          10|          24|      -14|
|   2012|    6|California             |UCLA                        |          43|          17|       26|
|   2012|    6|Cincinnati             |Miami (OH)                  |          52|          14|       38|
|   2012|    6|Clemson                |Georgia Tech                |          47|          31|       16|
|   2012|    6|Duke                   |Virginia                    |          42|          17|       25|
|   2012|    6|Florida                |Louisiana State             |          14|           6|        8|
|   2012|    6|Colorado State         |Fresno State                |           7|          28|      -21|
|   2012|    6|Houston                |North Texas                 |          44|          21|       23|
|   2012|    6|Idaho                  |New Mexico State            |          26|          18|        8|
|   2012|    6|Texas Christian        |Iowa State                  |          23|          37|      -14|
|   2012|    6|Kansas State           |Kansas                      |          56|          16|       40|
|   2012|    6|Eastern Michigan       |Kent State                  |          14|          41|      -27|
|   2012|    6|Louisiana Tech         |Nevada-Las Vegas            |          58|          31|       27|
|   2012|    6|Louisiana-Lafayette    |Tulane                      |          41|          13|       28|
|   2012|    6|Middle Tennessee State |Louisiana-Monroe            |          17|          31|      -14|
|   2012|    6|Maryland               |Wake Forest                 |          19|          14|        5|
|   2012|    6|Memphis                |Rice                        |          14|          10|        4|
|   2012|    6|Purdue                 |Michigan                    |          13|          44|      -31|
|   2012|    6|Indiana                |Michigan State              |          27|          31|       -4|
|   2012|    6|Kentucky               |Mississippi State           |          14|          27|      -13|
|   2012|    6|Air Force              |Navy                        |          21|          28|       -7|
|   2012|    6|Nevada                 |Wyoming                     |          35|          28|        7|
|   2012|    6|New Mexico             |Texas State                 |          35|          14|       21|
|   2012|    6|North Carolina         |Virginia Tech               |          48|          34|       14|
|   2012|    6|North Carolina State   |Florida State               |          17|          16|        1|
|   2012|    6|Ball State             |Northern Illinois           |          23|          35|      -12|
|   2012|    6|Notre Dame             |Miami (FL)                  |          41|           3|       38|
|   2012|    6|Ohio                   |Buffalo                     |          38|          31|        7|
|   2012|    6|Ohio State             |Nebraska                    |          63|          38|       25|
|   2012|    6|Texas Tech             |Oklahoma                    |          20|          41|      -21|
|   2012|    6|Oregon                 |Washington                  |          52|          21|       31|
|   2012|    6|Oregon State           |Washington State            |          19|           6|       13|
|   2012|    6|Penn State             |Northwestern                |          39|          28|       11|
|   2012|    6|Rutgers                |Connecticut                 |          19|           3|       16|
|   2012|    6|San Diego State        |Hawaii                      |          52|          14|       38|
|   2012|    6|South Carolina         |Georgia                     |          35|           7|       28|
|   2012|    6|Texas-El Paso          |Southern Methodist          |           0|          17|      -17|
|   2012|    6|Stanford               |Arizona                     |          54|          48|        6|
|   2012|    6|Temple                 |South Florida               |          37|          28|        9|
|   2012|    6|Mississippi            |Texas A&M                   |          27|          30|       -3|
|   2012|    6|Toledo                 |Central Michigan            |          50|          35|       15|
|   2012|    6|Marshall               |Tulsa                       |          38|          45|       -7|
|   2012|    6|Missouri               |Vanderbilt                  |          15|          19|       -4|
|   2012|    6|Texas                  |West Virginia               |          45|          48|       -3|
|   2012|    6|Western Michigan       |Massachusetts               |          52|          14|       38|
|   2012|    6|Wisconsin              |Illinois                    |          31|          14|       17|
|   2012|    7|Colorado               |Arizona State               |          17|          51|      -34|
|   2012|    7|Tulsa                  |Texas-El Paso               |          33|          11|       22|
|   2012|    7|Troy                   |Western Kentucky            |          26|          31|       -5|
|   2012|    7|Central Michigan       |Navy                        |          13|          31|      -18|
|   2012|    7|Wyoming                |Air Force                   |          27|          28|       -1|
|   2012|    7|Missouri               |Alabama                     |          10|          42|      -32|
|   2012|    7|Arkansas               |Kentucky                    |          49|           7|       42|
|   2012|    7|Arkansas State         |South Alabama               |          36|          29|        7|
|   2012|    7|Ball State             |Western Michigan            |          30|          24|        6|
|   2012|    7|Boise State            |Fresno State                |          20|          10|       10|
|   2012|    7|Bowling Green State    |Miami (OH)                  |          37|          12|       25|
|   2012|    7|Washington State       |California                  |          17|          31|      -14|
|   2012|    7|Central Florida        |Southern Mississippi        |          38|          31|        7|
|   2012|    7|Cincinnati             |Fordham                     |          49|          17|       32|
|   2012|    7|East Carolina          |Memphis                     |          41|           7|       34|
|   2012|    7|Vanderbilt             |Florida                     |          17|          31|      -14|
|   2012|    7|Florida State          |Boston College              |          51|           7|       44|
|   2012|    7|Houston                |Alabama-Birmingham          |          39|          17|       22|
|   2012|    7|Michigan State         |Iowa                        |          16|          19|       -3|
|   2012|    7|Iowa State             |Kansas State                |          21|          27|       -6|
|   2012|    7|Army                   |Kent State                  |          17|          31|      -14|
|   2012|    7|Louisiana State        |South Carolina              |          23|          21|        2|
|   2012|    7|Louisiana-Monroe       |Florida Atlantic            |          35|          14|       21|
|   2012|    7|Pittsburgh             |Louisville                  |          35|          45|      -10|
|   2012|    7|Virginia               |Maryland                    |          20|          27|       -7|
|   2012|    7|Michigan               |Illinois                    |          45|           0|       45|
|   2012|    7|Florida International  |Middle Tennessee State      |          30|          34|       -4|
|   2012|    7|Mississippi            |Auburn                      |          41|          20|       21|
|   2012|    7|Mississippi State      |Tennessee                   |          41|          31|       10|
|   2012|    7|Nevada-Las Vegas       |Nevada                      |          37|          42|       -5|
|   2012|    7|Hawaii                 |New Mexico                  |          23|          35|      -12|
|   2012|    7|Miami (FL)             |North Carolina              |          14|          18|       -4|
|   2012|    7|Northern Illinois      |Buffalo                     |          45|           3|       42|
|   2012|    7|Minnesota              |Northwestern                |          13|          21|       -8|
|   2012|    7|Notre Dame             |Stanford                    |          20|          13|        7|
|   2012|    7|Ohio                   |Akron                       |          34|          28|        6|
|   2012|    7|Indiana                |Ohio State                  |          49|          52|       -3|
|   2012|    7|Oklahoma               |Texas                       |          63|          21|       42|
|   2012|    7|Kansas                 |Oklahoma State              |          14|          20|       -6|
|   2012|    7|Brigham Young          |Oregon State                |          24|          42|      -18|
|   2012|    7|Rice                   |Texas-San Antonio           |          34|          14|       20|
|   2012|    7|Rutgers                |Syracuse                    |          23|          15|        8|
|   2012|    7|San Diego State        |Colorado State              |          38|          14|       24|
|   2012|    7|Washington             |Southern California         |          14|          24|      -10|
|   2012|    7|Connecticut            |Temple                      |          14|          17|       -3|
|   2012|    7|Louisiana Tech         |Texas A&M                   |          57|          59|       -2|
|   2012|    7|Baylor                 |Texas Christian             |          21|          49|      -28|
|   2012|    7|Texas State            |Idaho                       |          38|           7|       31|
|   2012|    7|Texas Tech             |West Virginia               |          49|          14|       35|
|   2012|    7|Eastern Michigan       |Toledo                      |          47|          52|       -5|
|   2012|    7|Tulane                 |Southern Methodist          |          27|          26|        1|
|   2012|    7|UCLA                   |Utah                        |          21|          14|        7|
|   2012|    7|San Jose State         |Utah State                  |          27|          49|      -22|
|   2012|    7|Virginia Tech          |Duke                        |          41|          20|       21|
|   2012|    7|Purdue                 |Wisconsin                   |          14|          38|      -24|
|   2012|    8|North Texas            |Louisiana-Lafayette         |          30|          23|        7|
|   2012|    8|Arizona State          |Oregon                      |          21|          43|      -22|
|   2012|    8|Southern Methodist     |Houston                     |          72|          42|       30|
|   2012|    8|Syracuse               |Connecticut                 |          40|          10|       30|
|   2012|    8|Air Force              |New Mexico                  |          28|          23|        5|
|   2012|    8|Tennessee              |Alabama                     |          13|          44|      -31|
|   2012|    8|Arizona                |Washington                  |          52|          17|       35|
|   2012|    8|Central Michigan       |Ball State                  |          30|          41|      -11|
|   2012|    8|Boise State            |Nevada-Las Vegas            |          32|           7|       25|
|   2012|    8|Massachusetts          |Bowling Green State         |           0|          24|      -24|
|   2012|    8|Memphis                |Central Florida             |          17|          35|      -18|
|   2012|    8|Clemson                |Virginia Tech               |          38|          17|       21|
|   2012|    8|Duke                   |North Carolina              |          33|          30|        3|
|   2012|    8|Alabama-Birmingham     |East Carolina               |          35|          42|       -7|
|   2012|    8|Eastern Michigan       |Army                        |          48|          38|       10|
|   2012|    8|Florida                |South Carolina              |          44|          11|       33|
|   2012|    8|Miami (FL)             |Florida State               |          20|          33|      -13|
|   2012|    8|Fresno State           |Wyoming                     |          42|          14|       28|
|   2012|    8|Kentucky               |Georgia                     |          24|          29|       -5|
|   2012|    8|Georgia Tech           |Boston College              |          37|          17|       20|
|   2012|    8|West Virginia          |Kansas State                |          14|          55|      -41|
|   2012|    8|Kent State             |Western Michigan            |          41|          24|       17|
|   2012|    8|Texas A&M              |Louisiana State             |          19|          24|       -5|
|   2012|    8|Louisiana Tech         |Idaho                       |          70|          28|       42|
|   2012|    8|Western Kentucky       |Louisiana-Monroe            |          42|          43|       -1|
|   2012|    8|Louisville             |South Florida               |          27|          25|        2|
|   2012|    8|Southern Mississippi   |Marshall                    |          24|          59|      -35|
|   2012|    8|Michigan               |Michigan State              |          12|          10|        2|
|   2012|    8|Mississippi State      |Middle Tennessee State      |          45|           3|       42|
|   2012|    8|Navy                   |Indiana                     |          31|          30|        1|
|   2012|    8|Northwestern           |Nebraska                    |          28|          29|       -1|
|   2012|    8|Maryland               |North Carolina State        |          18|          20|       -2|
|   2012|    8|Akron                  |Northern Illinois           |           7|          37|      -30|
|   2012|    8|Notre Dame             |Brigham Young               |          17|          14|        3|
|   2012|    8|Ohio State             |Purdue                      |          29|          22|        7|
|   2012|    8|Oklahoma               |Kansas                      |          52|           7|       45|
|   2012|    8|Oklahoma State         |Iowa State                  |          31|          10|       21|
|   2012|    8|Oregon State           |Utah                        |          21|           7|       14|
|   2012|    8|Iowa                   |Penn State                  |          14|          38|      -24|
|   2012|    8|Buffalo                |Pittsburgh                  |           6|          20|      -14|
|   2012|    8|Temple                 |Rutgers                     |          10|          35|      -25|
|   2012|    8|Nevada                 |San Diego State             |          38|          39|       -1|
|   2012|    8|Texas-San Antonio      |San Jose State              |          24|          52|      -28|
|   2012|    8|South Alabama          |Florida Atlantic            |          37|          34|        3|
|   2012|    8|Southern California    |Colorado                    |          50|           6|       44|
|   2012|    8|California             |Stanford                    |           3|          21|      -18|
|   2012|    8|Texas                  |Baylor                      |          56|          50|        6|
|   2012|    8|Texas Christian        |Texas Tech                  |          53|          56|       -3|
|   2012|    8|Texas-El Paso          |Tulane                      |          24|          20|        4|
|   2012|    8|Toledo                 |Cincinnati                  |          29|          23|        6|
|   2012|    8|Troy                   |Florida International       |          38|          37|        1|
|   2012|    8|Tulsa                  |Rice                        |          28|          24|        4|
|   2012|    8|Utah State             |New Mexico State            |          41|           7|       34|
|   2012|    8|Vanderbilt             |Auburn                      |          17|          13|        4|
|   2012|    8|Virginia               |Wake Forest                 |          10|          16|       -6|
|   2012|    8|Wisconsin              |Minnesota                   |          38|          13|       25|
|   2012|    9|Louisiana-Lafayette    |Arkansas State              |          27|          50|      -23|
|   2012|    9|Wake Forest            |Clemson                     |          13|          42|      -29|
|   2012|    9|Air Force              |Nevada                      |          48|          31|       17|
|   2012|    9|Louisville             |Cincinnati                  |          34|          31|        3|
|   2012|    9|Alabama                |Mississippi State           |          38|           7|       31|
|   2012|    9|Arizona                |Southern California         |          39|          36|        3|
|   2012|    9|Army                   |Ball State                  |          22|          30|       -8|
|   2012|    9|Wyoming                |Boise State                 |          14|          45|      -31|
|   2012|    9|Boston College         |Maryland                    |          20|          17|        3|
|   2012|    9|Bowling Green State    |Eastern Michigan            |          24|           3|       21|
|   2012|    9|Georgia Tech           |Brigham Young               |          17|          41|      -24|
|   2012|    9|Marshall               |Central Florida             |          17|          54|      -37|
|   2012|    9|Central Michigan       |Akron                       |          35|          14|       21|
|   2012|    9|Colorado State         |Hawaii                      |          42|          27|       15|
|   2012|    9|Florida Atlantic       |Troy                        |          34|          27|        7|
|   2012|    9|Florida State          |Duke                        |          48|           7|       41|
|   2012|    9|New Mexico             |Fresno State                |          32|          49|      -17|
|   2012|    9|Georgia                |Florida                     |          17|           9|        8|
|   2012|    9|Houston                |Texas-El Paso               |          45|          35|       10|
|   2012|    9|Illinois               |Indiana                     |          17|          31|      -14|
|   2012|    9|Iowa State             |Baylor                      |          35|          21|       14|
|   2012|    9|Kansas State           |Texas Tech                  |          55|          24|       31|
|   2012|    9|Rutgers                |Kent State                  |          23|          35|      -12|
|   2012|    9|New Mexico State       |Louisiana Tech              |          14|          28|      -14|
|   2012|    9|Louisiana-Monroe       |South Alabama               |          38|          24|       14|
|   2012|    9|Miami (OH)             |Ohio                        |          23|          20|        3|
|   2012|    9|Wisconsin              |Michigan State              |          13|          16|       -3|
|   2012|    9|Middle Tennessee State |North Texas                 |          38|          21|       17|
|   2012|    9|Minnesota              |Purdue                      |          44|          28|       16|
|   2012|    9|Arkansas               |Mississippi                 |          27|          30|       -3|
|   2012|    9|Missouri               |Kentucky                    |          33|          10|       23|
|   2012|    9|East Carolina          |Navy                        |          28|          56|      -28|
|   2012|    9|Nebraska               |Michigan                    |          23|           9|       14|
|   2012|    9|North Carolina         |North Carolina State        |          43|          35|        8|
|   2012|    9|Western Michigan       |Northern Illinois           |          34|          48|      -14|
|   2012|    9|Northwestern           |Iowa                        |          28|          17|       11|
|   2012|    9|Oklahoma               |Notre Dame                  |          13|          30|      -17|
|   2012|    9|Penn State             |Ohio State                  |          23|          35|      -12|
|   2012|    9|Oklahoma State         |Texas Christian             |          36|          14|       22|
|   2012|    9|Oregon                 |Colorado                    |          70|          14|       56|
|   2012|    9|Pittsburgh             |Temple                      |          47|          17|       30|
|   2012|    9|Rice                   |Southern Mississippi        |          44|          17|       27|
|   2012|    9|San Diego State        |Nevada-Las Vegas            |          24|          13|       11|
|   2012|    9|San Jose State         |Texas State                 |          31|          20|       11|
|   2012|    9|South Carolina         |Tennessee                   |          38|          35|        3|
|   2012|    9|Southern Methodist     |Memphis                     |          44|          13|       31|
|   2012|    9|Stanford               |Washington State            |          24|          17|        7|
|   2012|    9|South Florida          |Syracuse                    |          36|          37|       -1|
|   2012|    9|Kansas                 |Texas                       |          17|          21|       -4|
|   2012|    9|Auburn                 |Texas A&M                   |          21|          63|      -42|
|   2012|    9|Buffalo                |Toledo                      |          20|          25|       -5|
|   2012|    9|Tulane                 |Alabama-Birmingham          |          55|          45|       10|
|   2012|    9|Arizona State          |UCLA                        |          43|          45|       -2|
|   2012|    9|Utah                   |California                  |          49|          27|       22|
|   2012|    9|Texas-San Antonio      |Utah State                  |          17|          48|      -31|
|   2012|    9|Vanderbilt             |Massachusetts               |          49|           7|       42|
|   2012|    9|Washington             |Oregon State                |          20|          17|        3|
|   2012|    9|Florida International  |Western Kentucky            |           6|          14|       -8|
|   2012|   10|Miami (FL)             |Virginia Tech               |          30|          12|       18|
|   2012|   10|Western Kentucky       |Middle Tennessee State      |          29|          34|       -5|
|   2012|   10|Ohio                   |Eastern Michigan            |          45|          14|       31|
|   2012|   10|California             |Washington                  |          13|          21|       -8|
|   2012|   10|Louisiana State        |Alabama                     |          17|          21|       -4|
|   2012|   10|Southern Mississippi   |Alabama-Birmingham          |          19|          27|       -8|
|   2012|   10|Arkansas               |Tulsa                       |          19|          15|        4|
|   2012|   10|North Texas            |Arkansas State              |          19|          37|      -18|
|   2012|   10|Army                   |Air Force                   |          41|          21|       20|
|   2012|   10|Auburn                 |New Mexico State            |          42|           7|       35|
|   2012|   10|Baylor                 |Kansas                      |          41|          14|       27|
|   2012|   10|Buffalo                |Miami (OH)                  |          27|          24|        3|
|   2012|   10|Central Florida        |Southern Methodist          |          42|          17|       25|
|   2012|   10|Cincinnati             |Syracuse                    |          35|          24|       11|
|   2012|   10|Duke                   |Clemson                     |          20|          56|      -36|
|   2012|   10|East Carolina          |Houston                     |          48|          28|       20|
|   2012|   10|Florida                |Missouri                    |          14|           7|        7|
|   2012|   10|South Alabama          |Florida International       |          20|          28|       -8|
|   2012|   10|Fresno State           |Hawaii                      |          45|          10|       35|
|   2012|   10|Georgia                |Mississippi                 |          37|          10|       27|
|   2012|   10|Maryland               |Georgia Tech                |          13|          33|      -20|
|   2012|   10|Indiana                |Iowa                        |          24|          21|        3|
|   2012|   10|Kansas State           |Oklahoma State              |          44|          30|       14|
|   2012|   10|Kent State             |Akron                       |          35|          24|       11|
|   2012|   10|Louisiana Tech         |Texas-San Antonio           |          51|          27|       24|
|   2012|   10|Louisiana-Monroe       |Louisiana-Lafayette         |          24|          40|      -16|
|   2012|   10|Louisville             |Temple                      |          45|          17|       28|
|   2012|   10|Marshall               |Memphis                     |          38|          28|       10|
|   2012|   10|Minnesota              |Michigan                    |          13|          35|      -22|
|   2012|   10|Navy                   |Florida Atlantic            |          24|          17|        7|
|   2012|   10|Michigan State         |Nebraska                    |          24|          28|       -4|
|   2012|   10|Nevada-Las Vegas       |New Mexico                  |          35|           7|       28|
|   2012|   10|Northern Illinois      |Massachusetts               |          63|           0|       63|
|   2012|   10|Notre Dame             |Pittsburgh                  |          29|          26|        3|
|   2012|   10|Ohio State             |Illinois                    |          52|          22|       30|
|   2012|   10|Iowa State             |Oklahoma                    |          20|          35|      -15|
|   2012|   10|Southern California    |Oregon                      |          51|          62|      -11|
|   2012|   10|Oregon State           |Arizona State               |          36|          26|       10|
|   2012|   10|Purdue                 |Penn State                  |           9|          34|      -25|
|   2012|   10|Tulane                 |Rice                        |          47|          49|       -2|
|   2012|   10|Boise State            |San Diego State             |          19|          21|       -2|
|   2012|   10|Idaho                  |San Jose State              |          13|          42|      -29|
|   2012|   10|South Florida          |Connecticut                 |          13|           6|        7|
|   2012|   10|Colorado               |Stanford                    |           0|          48|      -48|
|   2012|   10|Tennessee              |Troy                        |          55|          48|        7|
|   2012|   10|Texas Tech             |Texas                       |          22|          31|       -9|
|   2012|   10|Mississippi State      |Texas A&M                   |          13|          38|      -25|
|   2012|   10|West Virginia          |Texas Christian             |          38|          39|       -1|
|   2012|   10|UCLA                   |Arizona                     |          66|          10|       56|
|   2012|   10|Utah                   |Washington State            |          49|           6|       43|
|   2012|   10|Utah State             |Texas State                 |          38|           7|       31|
|   2012|   10|Kentucky               |Vanderbilt                  |           0|          40|      -40|
|   2012|   10|North Carolina State   |Virginia                    |           6|          33|      -27|
|   2012|   10|Wake Forest            |Boston College              |          28|          14|       14|
|   2012|   10|Central Michigan       |Western Michigan            |          31|          42|      -11|
|   2012|   10|Wyoming                |Colorado State              |          45|          31|       14|
|   2012|   11|Toledo                 |Ball State                  |          27|          34|       -7|
|   2012|   11|Ohio                   |Bowling Green State         |          14|          26|      -12|
|   2012|   11|Arkansas State         |Louisiana-Monroe            |          45|          23|       22|
|   2012|   11|Virginia Tech          |Florida State               |          22|          28|       -6|
|   2012|   11|Connecticut            |Pittsburgh                  |          24|          17|        7|
|   2012|   11|Alabama-Birmingham     |Marshall                    |          38|          31|        7|
|   2012|   11|Arizona                |Colorado                    |          56|          31|       25|
|   2012|   11|Hawaii                 |Boise State                 |          14|          49|      -35|
|   2012|   11|Brigham Young          |Idaho                       |          52|          13|       39|
|   2012|   11|Buffalo                |Western Michigan            |          29|          24|        5|
|   2012|   11|Texas-El Paso          |Central Florida             |          24|          31|       -7|
|   2012|   11|Eastern Michigan       |Central Michigan            |          31|          34|       -3|
|   2012|   11|Temple                 |Cincinnati                  |          10|          34|      -24|
|   2012|   11|Clemson                |Maryland                    |          45|          10|       35|
|   2012|   11|Colorado State         |Nevada-Las Vegas            |          33|          11|       22|
|   2012|   11|Florida                |Louisiana-Lafayette         |          27|          20|        7|
|   2012|   11|Western Kentucky       |Florida Atlantic            |          28|          37|       -9|
|   2012|   11|Nevada                 |Fresno State                |          36|          52|      -16|
|   2012|   11|Auburn                 |Georgia                     |           0|          38|      -38|
|   2012|   11|North Carolina         |Georgia Tech                |          50|          68|      -18|
|   2012|   11|Texas Christian        |Kansas State                |          10|          23|      -13|
|   2012|   11|Miami (OH)             |Kent State                  |          32|          48|      -16|
|   2012|   11|Louisiana State        |Mississippi State           |          37|          17|       20|
|   2012|   11|Texas State            |Louisiana Tech              |          55|          62|       -7|
|   2012|   11|Akron                  |Massachusetts               |          14|          22|       -8|
|   2012|   11|Memphis                |Tulane                      |          37|          23|       14|
|   2012|   11|Michigan               |Northwestern                |          38|          31|        7|
|   2012|   11|Illinois               |Minnesota                   |           3|          17|      -14|
|   2012|   11|Tennessee              |Missouri                    |          48|          51|       -3|
|   2012|   11|Nebraska               |Penn State                  |          32|          23|        9|
|   2012|   11|North Carolina State   |Wake Forest                 |          37|           6|       31|
|   2012|   11|North Texas            |South Alabama               |          24|          14|       10|
|   2012|   11|Boston College         |Notre Dame                  |           6|          21|      -15|
|   2012|   11|Oklahoma               |Baylor                      |          42|          34|        8|
|   2012|   11|Oklahoma State         |West Virginia               |          55|          34|       21|
|   2012|   11|California             |Oregon                      |          17|          59|      -42|
|   2012|   11|Iowa                   |Purdue                      |          24|          27|       -3|
|   2012|   11|Rutgers                |Army                        |          28|           7|       21|
|   2012|   11|San Diego State        |Air Force                   |          28|           9|       19|
|   2012|   11|New Mexico State       |San Jose State              |           7|          47|      -40|
|   2012|   11|South Carolina         |Arkansas                    |          38|          20|       18|
|   2012|   11|Southern California    |Arizona State               |          38|          17|       21|
|   2012|   11|Southern Methodist     |Southern Mississippi        |          34|           6|       28|
|   2012|   11|Stanford               |Oregon State                |          27|          23|        4|
|   2012|   11|Syracuse               |Louisville                  |          45|          26|       19|
|   2012|   11|Texas                  |Iowa State                  |          33|           7|       26|
|   2012|   11|Alabama                |Texas A&M                   |          24|          29|       -5|
|   2012|   11|Texas Tech             |Kansas                      |          41|          34|        7|
|   2012|   11|Texas-San Antonio      |McNeese State               |          31|          24|        7|
|   2012|   11|Troy                   |Navy                        |          41|          31|       10|
|   2012|   11|Houston                |Tulsa                       |           7|          41|      -34|
|   2012|   11|Washington State       |UCLA                        |          36|          44|       -8|
|   2012|   11|Mississippi            |Vanderbilt                  |          26|          27|       -1|
|   2012|   11|Virginia               |Miami (FL)                  |          41|          40|        1|
|   2012|   11|Washington             |Utah                        |          34|          15|       19|
|   2012|   11|Indiana                |Wisconsin                   |          14|          62|      -48|
|   2012|   11|New Mexico             |Wyoming                     |          23|          28|       -5|
|   2012|   12|Ball State             |Ohio                        |          52|          27|       25|
|   2012|   12|Northern Illinois      |Toledo                      |          31|          24|        7|
|   2012|   12|Virginia               |North Carolina              |          13|          37|      -24|
|   2012|   12|Air Force              |Hawaii                      |          21|           7|       14|
|   2012|   12|Florida Atlantic       |Florida International       |          24|          34|      -10|
|   2012|   12|Alabama                |Western Carolina            |          49|           0|       49|
|   2012|   12|Utah                   |Arizona                     |          24|          34|      -10|
|   2012|   12|Arizona State          |Washington State            |          46|           7|       39|
|   2012|   12|Troy                   |Arkansas State              |          34|          41|       -7|
|   2012|   12|Auburn                 |Alabama A&M                 |          51|           7|       44|
|   2012|   12|Baylor                 |Kansas State                |          52|          24|       28|
|   2012|   12|Boise State            |Colorado State              |          42|          14|       28|
|   2012|   12|Massachusetts          |Buffalo                     |          19|          29|      -10|
|   2012|   12|Central Michigan       |Miami (OH)                  |          30|          16|       14|
|   2012|   12|Clemson                |North Carolina State        |          62|          48|       14|
|   2012|   12|Tulane                 |East Carolina               |          23|          28|       -5|
|   2012|   12|Western Michigan       |Eastern Michigan            |          23|          29|       -6|
|   2012|   12|Florida                |Jacksonville State          |          23|           0|       23|
|   2012|   12|Maryland               |Florida State               |          14|          41|      -27|
|   2012|   12|Georgia                |Georgia Southern            |          45|          14|       31|
|   2012|   12|Georgia Tech           |Duke                        |          42|          24|       18|
|   2012|   12|Kansas                 |Iowa State                  |          23|          51|      -28|
|   2012|   12|Bowling Green State    |Kent State                  |          24|          31|       -7|
|   2012|   12|Kentucky               |Samford                     |          34|           3|       31|
|   2012|   12|Louisiana State        |Mississippi                 |          41|          35|        6|
|   2012|   12|Louisiana-Lafayette    |Western Kentucky            |          31|          27|        4|
|   2012|   12|Louisiana-Monroe       |North Texas                 |          42|          16|       26|
|   2012|   12|Marshall               |Houston                     |          44|          41|        3|
|   2012|   12|Alabama-Birmingham     |Memphis                     |           9|          46|      -37|
|   2012|   12|Miami (FL)             |South Florida               |          40|           9|       31|
|   2012|   12|Michigan               |Iowa                        |          42|          17|       25|
|   2012|   12|South Alabama          |Middle Tennessee State      |          12|          20|       -8|
|   2012|   12|Mississippi State      |Arkansas                    |          45|          14|       31|
|   2012|   12|Navy                   |Texas State                 |          21|          10|       11|
|   2012|   12|Nebraska               |Minnesota                   |          38|          14|       24|
|   2012|   12|New Mexico             |Nevada                      |          24|          31|       -7|
|   2012|   12|Michigan State         |Northwestern                |          20|          23|       -3|
|   2012|   12|Notre Dame             |Wake Forest                 |          38|           0|       38|
|   2012|   12|Wisconsin              |Ohio State                  |          14|          21|       -7|
|   2012|   12|West Virginia          |Oklahoma                    |          49|          50|       -1|
|   2012|   12|Oklahoma State         |Texas Tech                  |          59|          21|       38|
|   2012|   12|Oregon State           |California                  |          62|          14|       48|
|   2012|   12|Penn State             |Indiana                     |          45|          22|       23|
|   2012|   12|Illinois               |Purdue                      |          17|          20|       -3|
|   2012|   12|Rice                   |Southern Methodist          |          36|          20|       16|
|   2012|   12|Cincinnati             |Rutgers                     |           3|          10|       -7|
|   2012|   12|San Jose State         |Brigham Young               |          20|          14|        6|
|   2012|   12|South Carolina         |Wofford                     |          24|           7|       17|
|   2012|   12|Oregon                 |Stanford                    |          14|          17|       -3|
|   2012|   12|Missouri               |Syracuse                    |          27|          31|       -4|
|   2012|   12|Army                   |Temple                      |          32|          63|      -31|
|   2012|   12|Texas A&M              |Sam Houston State           |          47|          28|       19|
|   2012|   12|Southern Mississippi   |Texas-El Paso               |          33|          34|       -1|
|   2012|   12|Idaho                  |Texas-San Antonio           |          27|          34|       -7|
|   2012|   12|Tulsa                  |Central Florida             |          23|          21|        2|
|   2012|   12|UCLA                   |Southern California         |          38|          28|       10|
|   2012|   12|Louisiana Tech         |Utah State                  |          41|          48|       -7|
|   2012|   12|Vanderbilt             |Tennessee                   |          41|          18|       23|
|   2012|   12|Boston College         |Virginia Tech               |          23|          30|       -7|
|   2012|   12|Colorado               |Washington                  |           3|          38|      -35|
|   2012|   12|Nevada-Las Vegas       |Wyoming                     |          23|          28|       -5|
|   2012|   13|Toledo                 |Akron                       |          35|          23|       12|
|   2012|   13|Texas                  |Texas Christian             |          13|          20|       -7|
|   2012|   13|Arizona                |Arizona State               |          34|          41|       -7|
|   2012|   13|Miami (OH)             |Ball State                  |          24|          31|       -7|
|   2012|   13|Bowling Green State    |Buffalo                     |          21|           7|       14|
|   2012|   13|Massachusetts          |Central Michigan            |          21|          42|      -21|
|   2012|   13|Cincinnati             |South Florida               |          27|          10|       17|
|   2012|   13|East Carolina          |Marshall                    |          65|          59|        6|
|   2012|   13|Kent State             |Ohio                        |          28|           6|       22|
|   2012|   13|Arkansas               |Louisiana State             |          13|          20|       -7|
|   2012|   13|Iowa                   |Nebraska                    |           7|          13|       -6|
|   2012|   13|Eastern Michigan       |Northern Illinois           |           7|          49|      -42|
|   2012|   13|Temple                 |Syracuse                    |          20|          38|      -18|
|   2012|   13|Colorado               |Utah                        |          35|          42|       -7|
|   2012|   13|Washington State       |Washington                  |          31|          28|        3|
|   2012|   13|Iowa State             |West Virginia               |          24|          31|       -7|
|   2012|   13|Alabama                |Auburn                      |          49|           0|       49|
|   2012|   13|Baylor                 |Texas Tech                  |          52|          45|        7|
|   2012|   13|New Mexico State       |Brigham Young               |          14|          50|      -36|
|   2012|   13|Central Florida        |Alabama-Birmingham          |          49|          24|       25|
|   2012|   13|Colorado State         |New Mexico                  |          24|          20|        4|
|   2012|   13|Louisville             |Connecticut                 |          20|          23|       -3|
|   2012|   13|Florida State          |Florida                     |          26|          37|      -11|
|   2012|   13|Fresno State           |Air Force                   |          48|          15|       33|
|   2012|   13|Georgia                |Georgia Tech                |          42|          10|       32|
|   2012|   13|Hawaii                 |Nevada-Las Vegas            |          48|          10|       38|
|   2012|   13|Houston                |Tulane                      |          40|          17|       23|
|   2012|   13|Louisiana-Lafayette    |South Alabama               |          52|          30|       22|
|   2012|   13|Florida International  |Louisiana-Monroe            |          17|          23|       -6|
|   2012|   13|Memphis                |Southern Mississippi        |          42|          24|       18|
|   2012|   13|Duke                   |Miami (FL)                  |          45|          52|       -7|
|   2012|   13|Minnesota              |Michigan State              |          10|          26|      -16|
|   2012|   13|Middle Tennessee State |Troy                        |          24|          21|        3|
|   2012|   13|Mississippi            |Mississippi State           |          41|          24|       17|
|   2012|   13|North Carolina         |Maryland                    |          45|          38|        7|
|   2012|   13|North Carolina State   |Boston College              |          27|          10|       17|
|   2012|   13|Northwestern           |Illinois                    |          50|          14|       36|
|   2012|   13|Southern California    |Notre Dame                  |          13|          22|       -9|
|   2012|   13|Ohio State             |Michigan                    |          26|          21|        5|
|   2012|   13|Oklahoma               |Oklahoma State              |          51|          48|        3|
|   2012|   13|Oregon State           |Oregon                      |          24|          48|      -24|
|   2012|   13|Penn State             |Wisconsin                   |          24|          21|        3|
|   2012|   13|Pittsburgh             |Rutgers                     |          27|           6|       21|
|   2012|   13|Purdue                 |Indiana                     |          56|          35|       21|
|   2012|   13|Texas-El Paso          |Rice                        |          24|          33|       -9|
|   2012|   13|Wyoming                |San Diego State             |          28|          42|      -14|
|   2012|   13|San Jose State         |Louisiana Tech              |          52|          43|        9|
|   2012|   13|Clemson                |South Carolina              |          17|          27|      -10|
|   2012|   13|Southern Methodist     |Tulsa                       |          35|          27|        8|
|   2012|   13|UCLA                   |Stanford                    |          17|          35|      -18|
|   2012|   13|Tennessee              |Kentucky                    |          37|          17|       20|
|   2012|   13|Texas A&M              |Missouri                    |          59|          29|       30|
|   2012|   13|Texas-San Antonio      |Texas State                 |          38|          31|        7|
|   2012|   13|Utah State             |Idaho                       |          45|           9|       36|
|   2012|   13|Wake Forest            |Vanderbilt                  |          21|          55|      -34|
|   2012|   13|Virginia Tech          |Virginia                    |          17|          14|        3|
|   2012|   13|Western Kentucky       |North Texas                 |          25|          24|        1|
|   2012|   14|Rutgers                |Louisville                  |          17|          20|       -3|
|   2012|   14|Northern Illinois      |Kent State                  |          44|          37|        7|
|   2012|   14|Stanford               |UCLA                        |          27|          24|        3|
|   2012|   14|Alabama                |Georgia                     |          32|          28|        4|
|   2012|   14|Arkansas State         |Middle Tennessee State      |          45|           0|       45|
|   2012|   14|Baylor                 |Oklahoma State              |          41|          34|        7|
|   2012|   14|Nevada                 |Boise State                 |          21|          27|       -6|
|   2012|   14|Connecticut            |Cincinnati                  |          17|          34|      -17|
|   2012|   14|Florida State          |Georgia Tech                |          21|          15|        6|
|   2012|   14|Hawaii                 |South Alabama               |          23|           7|       16|
|   2012|   14|Kansas State           |Texas                       |          42|          24|       18|
|   2012|   14|Florida Atlantic       |Louisiana-Lafayette         |          21|          35|      -14|
|   2012|   14|Texas Christian        |Oklahoma                    |          17|          24|       -7|
|   2012|   14|Oregon State           |Nicholls State              |          77|           3|       74|
|   2012|   14|South Florida          |Pittsburgh                  |           3|          27|      -24|
|   2012|   14|Texas State            |New Mexico State            |          66|          28|       38|
|   2012|   14|Tulsa                  |Central Florida             |          33|          27|        6|
|   2012|   14|West Virginia          |Kansas                      |          59|          10|       49|
|   2012|   14|Wisconsin              |Nebraska                    |          70|          31|       39|
|   2012|   15|Navy                   |Army                        |          17|          13|        4|
|   2012|   16|Arizona                |Nevada                      |          49|          48|        1|
|   2012|   16|Utah State             |Toledo                      |          41|          15|       26|
|   2012|   17|San Diego State        |Brigham Young               |           6|          23|      -17|
|   2012|   17|Central Florida        |Ball State                  |          38|          17|       21|
|   2012|   17|Boise State            |Washington                  |          28|          26|        2|
|   2012|   17|Louisiana-Lafayette    |East Carolina               |          43|          34|        9|
|   2012|   17|Southern Methodist     |Fresno State                |          43|          10|       33|
|   2012|   18|Central Michigan       |Western Kentucky            |          24|          21|        3|
|   2012|   18|Baylor                 |UCLA                        |          49|          26|       23|
|   2012|   18|Cincinnati             |Duke                        |          48|          34|       14|
|   2012|   18|San Jose State         |Bowling Green State         |          29|          20|        9|
|   2012|   18|Ohio                   |Louisiana-Monroe            |          45|          14|       31|
|   2012|   18|Texas Tech             |Minnesota                   |          34|          31|        3|
|   2012|   18|Virginia Tech          |Rutgers                     |          13|          10|        3|
|   2012|   18|Arizona State          |Navy                        |          62|          28|       34|
|   2012|   18|Michigan State         |Texas Christian             |          17|          16|        1|
|   2012|   18|Rice                   |Air Force                   |          33|          14|       19|
|   2012|   18|Syracuse               |West Virginia               |          38|          14|       24|
|   2012|   18|Texas                  |Oregon State                |          31|          27|        4|
|   2012|   19|Clemson                |Louisiana State             |          25|          24|        1|
|   2012|   19|Georgia Tech           |Southern California         |          21|           7|       14|
|   2012|   19|Tulsa                  |Iowa State                  |          31|          17|       14|
|   2012|   19|Vanderbilt             |North Carolina State        |          38|          24|       14|
|   2012|   20|Florida State          |Northern Illinois           |          31|          10|       21|
|   2012|   20|Georgia                |Nebraska                    |          45|          31|       14|
|   2012|   20|Northwestern           |Mississippi State           |          34|          20|       14|
|   2012|   20|Oklahoma State         |Purdue                      |          58|          14|       44|
|   2012|   20|South Carolina         |Michigan                    |          33|          28|        5|
|   2012|   20|Stanford               |Wisconsin                   |          20|          14|        6|
|   2012|   20|Louisville             |Florida                     |          33|          23|       10|
|   2012|   20|Oregon                 |Kansas State                |          35|          17|       18|
|   2012|   20|Texas A&M              |Oklahoma                    |          41|          13|       28|
|   2012|   20|Mississippi            |Pittsburgh                  |          38|          17|       21|
|   2012|   20|Arkansas State         |Kent State                  |          17|          13|        4|
|   2012|   20|Alabama                |Notre Dame                  |          42|          14|       28|
|   2013|    1|Ball State             |Illinois State              |          51|          28|       23|
|   2013|    1|Bowling Green State    |Tulsa                       |          34|           7|       27|
|   2013|    1|Central Florida        |Akron                       |          38|           7|       31|
|   2013|    1|Fresno State           |Rutgers                     |          52|          51|        1|
|   2013|    1|Indiana                |Indiana State               |          73|          35|       38|
|   2013|    1|Kent State             |Liberty                     |          17|          10|        7|
|   2013|    1|Middle Tennessee State |Western Carolina            |          45|          24|       21|
|   2013|    1|Minnesota              |Nevada-Las Vegas            |          51|          23|       28|
|   2013|    1|Vanderbilt             |Mississippi                 |          35|          39|       -4|
|   2013|    1|San Jose State         |Sacramento State            |          24|           0|       24|
|   2013|    1|South Carolina         |North Carolina              |          27|          10|       17|
|   2013|    1|Hawaii                 |Southern California         |          13|          30|      -17|
|   2013|    1|South Alabama          |Southern Utah               |          21|          22|       -1|
|   2013|    1|Connecticut            |Towson                      |          18|          33|      -15|
|   2013|    1|Tulane                 |Jackson State               |          34|           7|       27|
|   2013|    1|Utah                   |Utah State                  |          30|          26|        4|
|   2013|    1|Wake Forest            |Presbyterian                |          31|           7|       24|
|   2013|    1|Arizona                |Northern Arizona            |          35|           0|       35|
|   2013|    1|Army                   |Morgan State                |          28|          12|       16|
|   2013|    1|Houston                |Southern                    |          62|          13|       49|
|   2013|    1|Miami (FL)             |Florida Atlantic            |          34|           6|       28|
|   2013|    1|Michigan State         |Western Michigan            |          26|          13|       13|
|   2013|    1|Kansas State           |North Dakota State          |          21|          24|       -3|
|   2013|    1|Georgia State          |Samford                     |          21|          31|      -10|
|   2013|    1|Southern Methodist     |Texas Tech                  |          23|          41|      -18|
|   2013|    1|Air Force              |Colgate                     |          38|          13|       25|
|   2013|    1|Alabama                |Virginia Tech               |          35|          10|       25|
|   2013|    1|Arkansas               |Louisiana-Lafayette         |          34|          14|       20|
|   2013|    1|Arkansas State         |Arkansas-Pine Bluff         |          62|          11|       51|
|   2013|    1|Auburn                 |Washington State            |          31|          24|        7|
|   2013|    1|Baylor                 |Wofford                     |          69|           3|       66|
|   2013|    1|Boston College         |Villanova                   |          24|          14|       10|
|   2013|    1|Cincinnati             |Purdue                      |          42|           7|       35|
|   2013|    1|Clemson                |Georgia                     |          38|          35|        3|
|   2013|    1|Duke                   |North Carolina Central      |          45|           0|       45|
|   2013|    1|East Carolina          |Old Dominion                |          52|          38|       14|
|   2013|    1|San Diego State        |Eastern Illinois            |          19|          40|      -21|
|   2013|    1|Eastern Michigan       |Howard                      |          34|          24|       10|
|   2013|    1|Oregon State           |Eastern Washington          |          46|          49|       -3|
|   2013|    1|Florida                |Toledo                      |          24|           6|       18|
|   2013|    1|Georgia Tech           |Elon                        |          70|           0|       70|
|   2013|    1|Illinois               |Southern Illinois           |          42|          34|        8|
|   2013|    1|Louisiana State        |Texas Christian             |          37|          27|       10|
|   2013|    1|Marshall               |Miami (OH)                  |          52|          14|       38|
|   2013|    1|Maryland               |Florida International       |          43|          10|       33|
|   2013|    1|South Florida          |McNeese State               |          21|          53|      -32|
|   2013|    1|Michigan               |Central Michigan            |          59|           9|       50|
|   2013|    1|Missouri               |Murray State                |          58|          14|       44|
|   2013|    1|Nebraska               |Wyoming                     |          37|          34|        3|
|   2013|    1|North Carolina State   |Louisiana Tech              |          40|          14|       26|
|   2013|    1|North Texas            |Idaho                       |          40|           6|       34|
|   2013|    1|Iowa                   |Northern Illinois           |          27|          30|       -3|
|   2013|    1|Iowa State             |Northern Iowa               |          20|          28|       -8|
|   2013|    1|California             |Northwestern                |          30|          44|      -14|
|   2013|    1|Notre Dame             |Temple                      |          28|           6|       22|
|   2013|    1|Ohio State             |Buffalo                     |          40|          20|       20|
|   2013|    1|Oklahoma               |Louisiana-Monroe            |          34|           0|       34|
|   2013|    1|Oklahoma State         |Mississippi State           |          21|           3|       18|
|   2013|    1|Oregon                 |Nicholls State              |          66|           3|       63|
|   2013|    1|Penn State             |Syracuse                    |          23|          17|        6|
|   2013|    1|Tennessee              |Austin Peay                 |          45|           0|       45|
|   2013|    1|Texas                  |New Mexico State            |          56|           7|       49|
|   2013|    1|Texas A&M              |Rice                        |          52|          31|       21|
|   2013|    1|Southern Mississippi   |Texas State                 |          15|          22|       -7|
|   2013|    1|New Mexico             |Texas-San Antonio           |          13|          21|       -8|
|   2013|    1|Troy                   |Alabama-Birmingham          |          34|          31|        3|
|   2013|    1|UCLA                   |Nevada                      |          58|          20|       38|
|   2013|    1|Virginia               |Brigham Young               |          19|          16|        3|
|   2013|    1|Washington             |Boise State                 |          38|           6|       32|
|   2013|    1|West Virginia          |William & Mary              |          24|          17|        7|
|   2013|    1|Western Kentucky       |Kentucky                    |          35|          26|        9|
|   2013|    1|Wisconsin              |Massachusetts               |          45|           0|       45|
|   2013|    1|Colorado               |Colorado State              |          41|          27|       14|
|   2013|    1|Louisville             |Ohio                        |          49|           7|       42|
|   2013|    1|Pittsburgh             |Florida State               |          13|          41|      -28|
|   2013|    2|Arizona State          |Sacramento State            |          55|           0|       55|
|   2013|    2|East Carolina          |Florida Atlantic            |          31|          13|       18|
|   2013|    2|Boston College         |Wake Forest                 |          24|          10|       14|
|   2013|    2|Florida International  |Central Florida             |           0|          38|      -38|
|   2013|    2|Akron                  |James Madison               |          35|          33|        2|
|   2013|    2|Nevada-Las Vegas       |Arizona                     |          13|          58|      -45|
|   2013|    2|Arkansas               |Samford                     |          31|          21|       10|
|   2013|    2|Auburn                 |Arkansas State              |          38|           9|       29|
|   2013|    2|Ball State             |Army                        |          40|          14|       26|
|   2013|    2|Baylor                 |Buffalo                     |          70|          13|       57|
|   2013|    2|Boise State            |Tennessee-Martin            |          63|          14|       49|
|   2013|    2|Kent State             |Bowling Green State         |          22|          41|      -19|
|   2013|    2|Brigham Young          |Texas                       |          40|          21|       19|
|   2013|    2|California             |Portland State              |          37|          30|        7|
|   2013|    2|Central Michigan       |New Hampshire               |          24|          21|        3|
|   2013|    2|Georgia State          |Chattanooga                 |          14|          42|      -28|
|   2013|    2|Clemson                |South Carolina State        |          52|          13|       39|
|   2013|    2|Colorado               |Central Arkansas            |          38|          24|       14|
|   2013|    2|Memphis                |Duke                        |          14|          28|      -14|
|   2013|    2|Fresno State           |Cal Poly                    |          41|          25|       16|
|   2013|    2|Georgia                |South Carolina              |          41|          30|       11|
|   2013|    2|Temple                 |Houston                     |          13|          22|       -9|
|   2013|    2|Illinois               |Cincinnati                  |          45|          17|       28|
|   2013|    2|Iowa                   |Missouri State              |          28|          14|       14|
|   2013|    2|Kansas                 |South Dakota                |          31|          14|       17|
|   2013|    2|Kansas State           |Louisiana-Lafayette         |          48|          27|       21|
|   2013|    2|Kentucky               |Miami (OH)                  |          41|           7|       34|
|   2013|    2|Louisiana State        |Alabama-Birmingham          |          56|          17|       39|
|   2013|    2|Louisiana Tech         |Lamar                       |          27|          14|       13|
|   2013|    2|Louisiana-Monroe       |Grambling State             |          48|          10|       38|
|   2013|    2|Louisville             |Eastern Kentucky            |          44|           7|       37|
|   2013|    2|Massachusetts          |Maine                       |          14|          24|      -10|
|   2013|    2|Marshall               |Gardner-Webb                |          55|           0|       55|
|   2013|    2|Maryland               |Old Dominion                |          47|          10|       37|
|   2013|    2|Miami (FL)             |Florida                     |          21|          16|        5|
|   2013|    2|Michigan               |Notre Dame                  |          41|          30|       11|
|   2013|    2|Michigan State         |South Florida               |          21|           6|       15|
|   2013|    2|New Mexico State       |Minnesota                   |          21|          44|      -23|
|   2013|    2|Mississippi            |Southeast Missouri State    |          31|          13|       18|
|   2013|    2|Mississippi State      |Alcorn State                |          51|           7|       44|
|   2013|    2|Missouri               |Toledo                      |          38|          23|       15|
|   2013|    2|Indiana                |Navy                        |          35|          41|       -6|
|   2013|    2|Nebraska               |Southern Mississippi        |          56|          13|       43|
|   2013|    2|Nevada                 |California-Davis            |          36|           7|       29|
|   2013|    2|Texas-El Paso          |New Mexico                  |          35|          42|       -7|
|   2013|    2|Western Michigan       |Nicholls State              |          23|          27|       -4|
|   2013|    2|North Carolina         |Middle Tennessee State      |          40|          20|       20|
|   2013|    2|North Carolina State   |Richmond                    |          23|          21|        2|
|   2013|    2|Northwestern           |Syracuse                    |          48|          27|       21|
|   2013|    2|Ohio                   |North Texas                 |          27|          21|        6|
|   2013|    2|Ohio State             |San Diego State             |          42|           7|       35|
|   2013|    2|Oklahoma               |West Virginia               |          16|           7|        9|
|   2013|    2|Texas-San Antonio      |Oklahoma State              |          35|          56|      -21|
|   2013|    2|Virginia               |Oregon                      |          10|          59|      -49|
|   2013|    2|Oregon State           |Hawaii                      |          33|          14|       19|
|   2013|    2|Penn State             |Eastern Michigan            |          45|           7|       38|
|   2013|    2|Purdue                 |Indiana State               |          20|          14|        6|
|   2013|    2|Rutgers                |Norfolk State               |          38|           0|       38|
|   2013|    2|Tulane                 |South Alabama               |          39|          41|       -2|
|   2013|    2|Southern Methodist     |Montana State               |          31|          30|        1|
|   2013|    2|Stanford               |San Jose State              |          34|          13|       21|
|   2013|    2|Tennessee              |Western Kentucky            |          52|          20|       32|
|   2013|    2|Texas A&M              |Sam Houston State           |          65|          28|       37|
|   2013|    2|Texas Christian        |Southeastern Louisiana      |          38|          17|       21|
|   2013|    2|Texas State            |Prairie View A&M            |          28|           3|       25|
|   2013|    2|Texas Tech             |Stephen F. Austin           |          61|          13|       48|
|   2013|    2|Troy                   |Savannah State              |          66|           3|       63|
|   2013|    2|Tulsa                  |Colorado State              |          30|          27|        3|
|   2013|    2|Utah                   |Weber State                 |          70|           7|       63|
|   2013|    2|Air Force              |Utah State                  |          20|          52|      -32|
|   2013|    2|Vanderbilt             |Austin Peay                 |          38|           3|       35|
|   2013|    2|Virginia Tech          |Western Carolina            |          45|           3|       42|
|   2013|    2|Southern California    |Washington State            |           7|          10|       -3|
|   2013|    2|Wisconsin              |Tennessee Tech              |          48|           0|       48|
|   2013|    2|Wyoming                |Idaho                       |          42|          10|       32|
|   2013|    3|Arkansas State         |Troy                        |          41|          34|        7|
|   2013|    3|Texas Tech             |Texas Christian             |          20|          10|       10|
|   2013|    3|Louisiana Tech         |Tulane                      |          15|          24|       -9|
|   2013|    3|Boise State            |Air Force                   |          42|          20|       22|
|   2013|    3|Texas A&M              |Alabama                     |          42|          49|       -7|
|   2013|    3|Arizona                |Texas-San Antonio           |          38|          13|       25|
|   2013|    3|Arizona State          |Wisconsin                   |          32|          30|        2|
|   2013|    3|Arkansas               |Southern Mississippi        |          24|           3|       21|
|   2013|    3|Auburn                 |Mississippi State           |          24|          20|        4|
|   2013|    3|Florida International  |Bethune-Cookman             |          13|          34|      -21|
|   2013|    3|Buffalo                |Stony Brook                 |          26|          23|        3|
|   2013|    3|Penn State             |Central Florida             |          31|          34|       -3|
|   2013|    3|Cincinnati             |Northwestern State          |          66|           9|       57|
|   2013|    3|Colorado State         |Cal Poly                    |          34|          17|       17|
|   2013|    3|South Florida          |Florida Atlantic            |          10|          28|      -18|
|   2013|    3|Florida State          |Nevada                      |          62|           7|       55|
|   2013|    3|Temple                 |Fordham                     |          29|          30|       -1|
|   2013|    3|Duke                   |Georgia Tech                |          14|          38|      -24|
|   2013|    3|Indiana                |Bowling Green State         |          42|          10|       32|
|   2013|    3|Iowa State             |Iowa                        |          21|          27|       -6|
|   2013|    3|Kansas State           |Massachusetts               |          37|           7|       30|
|   2013|    3|Louisiana State        |Kent State                  |          45|          13|       32|
|   2013|    3|Louisiana-Lafayette    |Nicholls State              |          70|           7|       63|
|   2013|    3|Wake Forest            |Louisiana-Monroe            |          19|          21|       -2|
|   2013|    3|Kentucky               |Louisville                  |          13|          27|      -14|
|   2013|    3|Connecticut            |Maryland                    |          21|          32|      -11|
|   2013|    3|Michigan               |Akron                       |          28|          24|        4|
|   2013|    3|Michigan State         |Youngstown State            |          55|          17|       38|
|   2013|    3|Middle Tennessee State |Memphis                     |          17|          15|        2|
|   2013|    3|Minnesota              |Western Illinois            |          29|          12|       17|
|   2013|    3|Texas                  |Mississippi                 |          23|          44|      -21|
|   2013|    3|Navy                   |Delaware                    |          51|           7|       44|
|   2013|    3|Nevada-Las Vegas       |Central Michigan            |          31|          21|       10|
|   2013|    3|North Texas            |Ball State                  |          34|          27|        7|
|   2013|    3|Idaho                  |Northern Illinois           |          35|          45|      -10|
|   2013|    3|Northwestern           |Western Michigan            |          38|          17|       21|
|   2013|    3|Purdue                 |Notre Dame                  |          24|          31|       -7|
|   2013|    3|Ohio                   |Marshall                    |          34|          31|        3|
|   2013|    3|California             |Ohio State                  |          34|          52|      -18|
|   2013|    3|Oklahoma               |Tulsa                       |          51|          20|       31|
|   2013|    3|Oklahoma State         |Lamar                       |          59|           3|       56|
|   2013|    3|Oregon                 |Tennessee                   |          59|          14|       45|
|   2013|    3|Utah                   |Oregon State                |          48|          51|       -3|
|   2013|    3|Pittsburgh             |New Mexico                  |          49|          27|       22|
|   2013|    3|Rice                   |Kansas                      |          23|          14|        9|
|   2013|    3|Rutgers                |Eastern Michigan            |          28|          10|       18|
|   2013|    3|South Alabama          |Western Kentucky            |          31|          24|        7|
|   2013|    3|South Carolina         |Vanderbilt                  |          35|          25|       10|
|   2013|    3|Southern California    |Boston College              |          35|           7|       28|
|   2013|    3|Army                   |Stanford                    |          20|          34|      -14|
|   2013|    3|Syracuse               |Wagner                      |          54|           0|       54|
|   2013|    3|New Mexico State       |Texas-El Paso               |          21|          42|      -21|
|   2013|    3|Toledo                 |Eastern Washington          |          33|          21|       12|
|   2013|    3|Nebraska               |UCLA                        |          21|          41|      -20|
|   2013|    3|Utah State             |Weber State                 |          70|           6|       64|
|   2013|    3|East Carolina          |Virginia Tech               |          10|          15|       -5|
|   2013|    3|Washington             |Illinois                    |          34|          24|       10|
|   2013|    3|Washington State       |Southern Utah               |          48|          10|       38|
|   2013|    3|West Virginia          |Georgia State               |          41|           7|       34|
|   2013|    3|Wyoming                |Northern Colorado           |          35|           7|       28|
|   2013|    4|North Carolina State   |Clemson                     |          14|          26|      -12|
|   2013|    4|Fresno State           |Boise State                 |          41|          40|        1|
|   2013|    4|Alabama                |Colorado State              |          31|           6|       25|
|   2013|    4|Alabama-Birmingham     |Northwestern State          |          52|          28|       24|
|   2013|    4|Eastern Michigan       |Ball State                  |          20|          51|      -31|
|   2013|    4|Baylor                 |Louisiana-Monroe            |          70|           7|       63|
|   2013|    4|Bowling Green State    |Murray State                |          48|           7|       41|
|   2013|    4|Miami (OH)             |Cincinnati                  |           0|          14|      -14|
|   2013|    4|Florida                |Tennessee                   |          31|          17|       14|
|   2013|    4|Florida State          |Bethune-Cookman             |          54|           6|       48|
|   2013|    4|Georgia                |North Texas                 |          45|          21|       24|
|   2013|    4|Georgia Tech           |North Carolina              |          28|          20|        8|
|   2013|    4|Houston                |Rice                        |          31|          26|        5|
|   2013|    4|Iowa                   |Western Michigan            |          59|           3|       56|
|   2013|    4|Georgia State          |Jacksonville State          |          26|          32|       -6|
|   2013|    4|Kansas                 |Louisiana Tech              |          13|          10|        3|
|   2013|    4|Louisiana State        |Auburn                      |          35|          21|       14|
|   2013|    4|Akron                  |Louisiana-Lafayette         |          30|          35|       -5|
|   2013|    4|Louisville             |Florida International       |          72|           0|       72|
|   2013|    4|Maryland               |West Virginia               |          37|           0|       37|
|   2013|    4|Memphis                |Arkansas State              |          31|           7|       24|
|   2013|    4|Miami (FL)             |Savannah State              |          77|           7|       70|
|   2013|    4|Connecticut            |Michigan                    |          21|          24|       -3|
|   2013|    4|Florida Atlantic       |Middle Tennessee State      |          35|          42|       -7|
|   2013|    4|Minnesota              |San Jose State              |          43|          24|       19|
|   2013|    4|Mississippi State      |Troy                        |          62|           7|       55|
|   2013|    4|Indiana                |Missouri                    |          28|          45|      -17|
|   2013|    4|Nebraska               |South Dakota State          |          59|          20|       39|
|   2013|    4|Nevada                 |Hawaii                      |          31|           9|       22|
|   2013|    4|Nevada-Las Vegas       |Western Illinois            |          38|           7|       31|
|   2013|    4|Northern Illinois      |Eastern Illinois            |          43|          39|        4|
|   2013|    4|Northwestern           |Maine                       |          35|          21|       14|
|   2013|    4|Notre Dame             |Michigan State              |          17|          13|        4|
|   2013|    4|Ohio                   |Austin Peay                 |          38|           0|       38|
|   2013|    4|Ohio State             |Florida A&M                 |          76|           0|       76|
|   2013|    4|San Diego State        |Oregon State                |          30|          34|       -4|
|   2013|    4|Penn State             |Kent State                  |          34|           0|       34|
|   2013|    4|Duke                   |Pittsburgh                  |          55|          58|       -3|
|   2013|    4|Rutgers                |Arkansas                    |          28|          24|        4|
|   2013|    4|Southern California    |Utah State                  |          17|          14|        3|
|   2013|    4|Stanford               |Arizona State               |          42|          28|       14|
|   2013|    4|Syracuse               |Tulane                      |          52|          17|       35|
|   2013|    4|Texas                  |Kansas State                |          31|          21|       10|
|   2013|    4|Texas A&M              |Southern Methodist          |          42|          13|       29|
|   2013|    4|Texas Tech             |Texas State                 |          33|           7|       26|
|   2013|    4|Texas-El Paso          |Texas-San Antonio           |          13|          32|      -19|
|   2013|    4|Central Michigan       |Toledo                      |          17|          38|      -21|
|   2013|    4|UCLA                   |New Mexico State            |          59|          13|       46|
|   2013|    4|Brigham Young          |Utah                        |          13|          20|       -7|
|   2013|    4|Massachusetts          |Vanderbilt                  |           7|          24|      -17|
|   2013|    4|Virginia               |Virginia Military Institute |          49|           0|       49|
|   2013|    4|Virginia Tech          |Marshall                    |          29|          21|        8|
|   2013|    4|Army                   |Wake Forest                 |          11|          25|      -14|
|   2013|    4|Washington             |Idaho State                 |          56|           0|       56|
|   2013|    4|Washington State       |Idaho                       |          42|           0|       42|
|   2013|    4|Western Kentucky       |Morgan State                |          58|          17|       41|
|   2013|    4|Wisconsin              |Purdue                      |          41|          10|       31|
|   2013|    4|Air Force              |Wyoming                     |          23|          56|      -33|
|   2013|    5|Tulsa                  |Iowa State                  |          21|          38|      -17|
|   2013|    5|Georgia Tech           |Virginia Tech               |          10|          17|       -7|
|   2013|    5|Brigham Young          |Middle Tennessee State      |          37|          10|       27|
|   2013|    5|San Jose State         |Utah State                  |          12|          40|      -28|
|   2013|    5|Alabama                |Mississippi                 |          25|           0|       25|
|   2013|    5|Arizona State          |Southern California         |          62|          41|       21|
|   2013|    5|Army                   |Louisiana Tech              |          35|          16|       19|
|   2013|    5|Ball State             |Toledo                      |          31|          24|        7|
|   2013|    5|Boise State            |Southern Mississippi        |          60|           7|       53|
|   2013|    5|Bowling Green State    |Akron                       |          31|          14|       17|
|   2013|    5|Buffalo                |Connecticut                 |          41|          12|       29|
|   2013|    5|Clemson                |Wake Forest                 |          56|           7|       49|
|   2013|    5|Colorado State         |Texas-El Paso               |          59|          42|       17|
|   2013|    5|Duke                   |Troy                        |          38|          31|        7|
|   2013|    5|North Carolina         |East Carolina               |          31|          55|      -24|
|   2013|    5|Kentucky               |Florida                     |           7|          24|      -17|
|   2013|    5|Boston College         |Florida State               |          34|          48|      -14|
|   2013|    5|Hawaii                 |Fresno State                |          37|          42|       -5|
|   2013|    5|Georgia                |Louisiana State             |          44|          41|        3|
|   2013|    5|Texas-San Antonio      |Houston                     |          28|          59|      -31|
|   2013|    5|Idaho                  |Temple                      |          26|          24|        2|
|   2013|    5|Illinois               |Miami (OH)                  |          50|          14|       36|
|   2013|    5|Minnesota              |Iowa                        |           7|          23|      -16|
|   2013|    5|Western Michigan       |Kent State                  |          14|          32|      -18|
|   2013|    5|South Florida          |Miami (FL)                  |          21|          49|      -28|
|   2013|    5|Missouri               |Arkansas State              |          41|          19|       22|
|   2013|    5|Nevada                 |Air Force                   |          45|          42|        3|
|   2013|    5|New Mexico             |Nevada-Las Vegas            |          42|          56|      -14|
|   2013|    5|North Carolina State   |Central Michigan            |          48|          14|       34|
|   2013|    5|Purdue                 |Northern Illinois           |          24|          55|      -31|
|   2013|    5|Ohio State             |Wisconsin                   |          31|          24|        7|
|   2013|    5|Notre Dame             |Oklahoma                    |          21|          35|      -14|
|   2013|    5|Oregon                 |California                  |          55|          16|       39|
|   2013|    5|Oregon State           |Colorado                    |          44|          17|       27|
|   2013|    5|Pittsburgh             |Virginia                    |          14|           3|       11|
|   2013|    5|Rice                   |Florida Atlantic            |          18|          14|        4|
|   2013|    5|New Mexico State       |San Diego State             |          16|          26|      -10|
|   2013|    5|Central Florida        |South Carolina              |          25|          28|       -3|
|   2013|    5|Stanford               |Washington State            |          55|          17|       38|
|   2013|    5|Tennessee              |South Alabama               |          31|          24|        7|
|   2013|    5|Arkansas               |Texas A&M                   |          33|          45|      -12|
|   2013|    5|Texas Christian        |Southern Methodist          |          48|          17|       31|
|   2013|    5|Texas State            |Wyoming                     |          42|          21|       21|
|   2013|    5|Louisiana-Monroe       |Tulane                      |          14|          31|      -17|
|   2013|    5|Vanderbilt             |Alabama-Birmingham          |          52|          24|       28|
|   2013|    5|Washington             |Arizona                     |          31|          13|       18|
|   2013|    5|West Virginia          |Oklahoma State              |          30|          21|        9|
|   2013|    5|Western Kentucky       |Navy                        |          19|           7|       12|
|   2013|    6|Iowa State             |Texas                       |          30|          31|       -1|
|   2013|    6|Utah                   |UCLA                        |          27|          34|       -7|
|   2013|    6|Louisiana-Monroe       |Western Kentucky            |          10|          31|      -21|
|   2013|    6|Utah State             |Brigham Young               |          14|          31|      -17|
|   2013|    6|San Diego State        |Nevada                      |          51|          44|        7|
|   2013|    6|Alabama                |Georgia State               |          45|           3|       42|
|   2013|    6|Auburn                 |Mississippi                 |          30|          22|        8|
|   2013|    6|Virginia               |Ball State                  |          27|          48|      -21|
|   2013|    6|Baylor                 |West Virginia               |          73|          42|       31|
|   2013|    6|Boston College         |Army                        |          48|          27|       21|
|   2013|    6|Bowling Green State    |Massachusetts               |          28|           7|       21|
|   2013|    6|Buffalo                |Eastern Michigan            |          42|          14|       28|
|   2013|    6|Memphis                |Central Florida             |          17|          24|       -7|
|   2013|    6|Miami (OH)             |Central Michigan            |           9|          21|      -12|
|   2013|    6|Syracuse               |Clemson                     |          14|          49|      -35|
|   2013|    6|Middle Tennessee State |East Carolina               |          17|          24|       -7|
|   2013|    6|Florida                |Arkansas                    |          30|          10|       20|
|   2013|    6|Alabama-Birmingham     |Florida Atlantic            |          23|          37|      -14|
|   2013|    6|Southern Mississippi   |Florida International       |          23|          24|       -1|
|   2013|    6|Florida State          |Maryland                    |          63|           0|       63|
|   2013|    6|Idaho                  |Fresno State                |          14|          61|      -47|
|   2013|    6|Tennessee              |Georgia                     |          31|          34|       -3|
|   2013|    6|Indiana                |Penn State                  |          44|          24|       20|
|   2013|    6|Mississippi State      |Louisiana State             |          26|          59|      -33|
|   2013|    6|Texas-El Paso          |Louisiana Tech              |          35|          38|       -3|
|   2013|    6|Louisiana-Lafayette    |Texas State                 |          48|          24|       24|
|   2013|    6|Temple                 |Louisville                  |           7|          30|      -23|
|   2013|    6|Marshall               |Texas-San Antonio           |          34|          10|       24|
|   2013|    6|Miami (FL)             |Georgia Tech                |          45|          30|       15|
|   2013|    6|Michigan               |Minnesota                   |          42|          13|       29|
|   2013|    6|Iowa                   |Michigan State              |          14|          26|      -12|
|   2013|    6|Vanderbilt             |Missouri                    |          28|          51|      -23|
|   2013|    6|Navy                   |Air Force                   |          28|          10|       18|
|   2013|    6|Nebraska               |Illinois                    |          39|          19|       20|
|   2013|    6|New Mexico             |New Mexico State            |          66|          17|       49|
|   2013|    6|Kent State             |Northern Illinois           |          24|          38|      -14|
|   2013|    6|Notre Dame             |Arizona State               |          37|          34|        3|
|   2013|    6|Akron                  |Ohio                        |           3|          43|      -40|
|   2013|    6|Northwestern           |Ohio State                  |          30|          40|      -10|
|   2013|    6|Oklahoma               |Texas Christian             |          20|          17|        3|
|   2013|    6|Oklahoma State         |Kansas State                |          33|          29|        4|
|   2013|    6|Colorado               |Oregon                      |          16|          57|      -41|
|   2013|    6|Tulsa                  |Rice                        |          27|          30|       -3|
|   2013|    6|Southern Methodist     |Rutgers                     |          52|          55|       -3|
|   2013|    6|Hawaii                 |San Jose State              |          27|          37|      -10|
|   2013|    6|South Carolina         |Kentucky                    |          35|          28|        7|
|   2013|    6|South Florida          |Cincinnati                  |          26|          20|        6|
|   2013|    6|Stanford               |Washington                  |          31|          28|        3|
|   2013|    6|Kansas                 |Texas Tech                  |          16|          54|      -38|
|   2013|    6|Toledo                 |Western Michigan            |          47|          20|       27|
|   2013|    6|Troy                   |South Alabama               |          34|          33|        1|
|   2013|    6|Tulane                 |North Texas                 |          24|          21|        3|
|   2013|    6|Virginia Tech          |North Carolina              |          27|          17|       10|
|   2013|    6|Wake Forest            |North Carolina State        |          28|          13|       15|
|   2013|    6|California             |Washington State            |          22|          44|      -22|
|   2013|    7|Louisville             |Rutgers                     |          24|          10|       14|
|   2013|    7|Air Force              |San Diego State             |          20|          27|       -7|
|   2013|    7|Southern California    |Arizona                     |          38|          31|        7|
|   2013|    7|Cincinnati             |Temple                      |          38|          20|       18|
|   2013|    7|Kentucky               |Alabama                     |           7|          48|      -41|
|   2013|    7|Florida International  |Alabama-Birmingham          |          24|          27|       -3|
|   2013|    7|Arizona State          |Colorado                    |          54|          13|       41|
|   2013|    7|Arkansas State         |Idaho                       |          48|          24|       24|
|   2013|    7|Army                   |Eastern Michigan            |          50|          25|       25|
|   2013|    7|Auburn                 |Western Carolina            |          62|           3|       59|
|   2013|    7|Ball State             |Kent State                  |          27|          24|        3|
|   2013|    7|Kansas State           |Baylor                      |          25|          35|      -10|
|   2013|    7|Utah State             |Boise State                 |          23|          34|      -11|
|   2013|    7|Brigham Young          |Georgia Tech                |          38|          20|       18|
|   2013|    7|Western Michigan       |Buffalo                     |           0|          33|      -33|
|   2013|    7|Ohio                   |Central Michigan            |          23|          26|       -3|
|   2013|    7|Clemson                |Boston College              |          24|          14|       10|
|   2013|    7|Duke                   |Navy                        |          35|           7|       28|
|   2013|    7|Houston                |Memphis                     |          25|          15|       10|
|   2013|    7|Louisiana State        |Florida                     |          17|           6|       11|
|   2013|    7|Texas State            |Louisiana-Monroe            |          14|          21|       -7|
|   2013|    7|Florida Atlantic       |Marshall                    |          23|          24|       -1|
|   2013|    7|Maryland               |Virginia                    |          27|          26|        1|
|   2013|    7|Massachusetts          |Miami (OH)                  |          17|          10|        7|
|   2013|    7|Michigan State         |Indiana                     |          42|          28|       14|
|   2013|    7|Mississippi State      |Bowling Green State         |          21|          20|        1|
|   2013|    7|Georgia                |Missouri                    |          26|          41|      -15|
|   2013|    7|Purdue                 |Nebraska                    |           7|          44|      -37|
|   2013|    7|Nevada-Las Vegas       |Hawaii                      |          39|          37|        2|
|   2013|    7|North Texas            |Middle Tennessee State      |          34|           7|       27|
|   2013|    7|Northern Illinois      |Akron                       |          27|          20|        7|
|   2013|    7|Washington             |Oregon                      |          24|          45|      -21|
|   2013|    7|Washington State       |Oregon State                |          24|          52|      -28|
|   2013|    7|Penn State             |Michigan                    |          43|          40|        3|
|   2013|    7|Texas-San Antonio      |Rice                        |          21|          27|       -6|
|   2013|    7|Colorado State         |San Jose State              |          27|          34|       -7|
|   2013|    7|Arkansas               |South Carolina              |           7|          52|      -45|
|   2013|    7|Connecticut            |South Florida               |          10|          13|       -3|
|   2013|    7|North Carolina State   |Syracuse                    |          10|          24|      -14|
|   2013|    7|Texas                  |Oklahoma                    |          36|          20|       16|
|   2013|    7|Mississippi            |Texas A&M                   |          38|          41|       -3|
|   2013|    7|Texas Christian        |Kansas                      |          27|          17|       10|
|   2013|    7|Texas Tech             |Iowa State                  |          42|          35|        7|
|   2013|    7|Georgia State          |Troy                        |          28|          35|       -7|
|   2013|    7|Tulane                 |East Carolina               |          36|          33|        3|
|   2013|    7|Texas-El Paso          |Tulsa                       |          20|          34|      -14|
|   2013|    7|UCLA                   |California                  |          37|          10|       27|
|   2013|    7|Utah                   |Stanford                    |          27|          21|        6|
|   2013|    7|Virginia Tech          |Pittsburgh                  |          19|           9|       10|
|   2013|    7|Wisconsin              |Northwestern                |          35|           6|       29|
|   2013|    7|Wyoming                |New Mexico                  |          38|          31|        7|
|   2013|    8|Western Kentucky       |Louisiana-Lafayette         |          20|          37|      -17|
|   2013|    8|North Carolina         |Miami (FL)                  |          23|          27|       -4|
|   2013|    8|Louisville             |Central Florida             |          35|          38|       -3|
|   2013|    8|Miami (OH)             |Akron                       |          17|          24|       -7|
|   2013|    8|Alabama                |Arkansas                    |          52|           0|       52|
|   2013|    8|Arizona                |Utah                        |          35|          24|       11|
|   2013|    8|Arizona State          |Washington                  |          53|          24|       29|
|   2013|    8|Texas A&M              |Auburn                      |          41|          45|       -4|
|   2013|    8|Western Michigan       |Ball State                  |          17|          38|      -21|
|   2013|    8|Baylor                 |Iowa State                  |          71|           7|       64|
|   2013|    8|Boise State            |Nevada                      |          34|          17|       17|
|   2013|    8|Brigham Young          |Houston                     |          47|          46|        1|
|   2013|    8|Buffalo                |Massachusetts               |          32|           3|       29|
|   2013|    8|Cincinnati             |Connecticut                 |          41|          16|       25|
|   2013|    8|Colorado               |Charleston Southern         |          43|          10|       33|
|   2013|    8|Wyoming                |Colorado State              |          22|          52|      -30|
|   2013|    8|Virginia               |Duke                        |          22|          35|      -13|
|   2013|    8|East Carolina          |Southern Mississippi        |          55|          14|       41|
|   2013|    8|Clemson                |Florida State               |          14|          51|      -37|
|   2013|    8|Fresno State           |Nevada-Las Vegas            |          38|          14|       24|
|   2013|    8|Georgia Tech           |Syracuse                    |          56|           0|       56|
|   2013|    8|Michigan               |Indiana                     |          63|          47|       16|
|   2013|    8|Michigan State         |Purdue                      |          14|           0|       14|
|   2013|    8|Northwestern           |Minnesota                   |          17|          20|       -3|
|   2013|    8|Mississippi            |Louisiana State             |          27|          24|        3|
|   2013|    8|Missouri               |Florida                     |          36|          17|       19|
|   2013|    8|Louisiana Tech         |North Texas                 |          13|          28|      -15|
|   2013|    8|Central Michigan       |Northern Illinois           |          17|          38|      -21|
|   2013|    8|Notre Dame             |Southern California         |          14|          10|        4|
|   2013|    8|Eastern Michigan       |Ohio                        |          28|          56|      -28|
|   2013|    8|Ohio State             |Iowa                        |          34|          24|       10|
|   2013|    8|Kansas                 |Oklahoma                    |          19|          34|      -15|
|   2013|    8|Oklahoma State         |Texas Christian             |          24|          10|       14|
|   2013|    8|Oregon                 |Washington State            |          62|          38|       24|
|   2013|    8|California             |Oregon State                |          17|          49|      -32|
|   2013|    8|Pittsburgh             |Old Dominion                |          35|          24|       11|
|   2013|    8|New Mexico State       |Rice                        |          19|          45|      -26|
|   2013|    8|South Alabama          |Kent State                  |          38|          21|       17|
|   2013|    8|Memphis                |Southern Methodist          |          29|          34|       -5|
|   2013|    8|Stanford               |UCLA                        |          24|          10|       14|
|   2013|    8|Temple                 |Army                        |          33|          14|       19|
|   2013|    8|Tennessee              |South Carolina              |          23|          21|        2|
|   2013|    8|Texas State            |Georgia State               |          24|          17|        7|
|   2013|    8|West Virginia          |Texas Tech                  |          27|          37|      -10|
|   2013|    8|Toledo                 |Navy                        |          45|          44|        1|
|   2013|    8|New Mexico             |Utah State                  |          10|          45|      -35|
|   2013|    8|Vanderbilt             |Georgia                     |          31|          27|        4|
|   2013|    8|Wake Forest            |Maryland                    |          34|          10|       24|
|   2013|    8|Illinois               |Wisconsin                   |          32|          56|      -24|
|   2013|    9|Arkansas State         |Louisiana-Lafayette         |           7|          23|      -16|
|   2013|    9|Middle Tennessee State |Marshall                    |          51|          49|        2|
|   2013|    9|Mississippi State      |Kentucky                    |          28|          22|        6|
|   2013|    9|Brigham Young          |Boise State                 |          37|          20|       17|
|   2013|    9|Alabama                |Tennessee                   |          45|          10|       35|
|   2013|    9|Colorado               |Arizona                     |          20|          44|      -24|
|   2013|    9|Auburn                 |Florida Atlantic            |          45|          10|       35|
|   2013|    9|Akron                  |Ball State                  |          24|          42|      -18|
|   2013|    9|Kansas                 |Baylor                      |          14|          59|      -45|
|   2013|    9|Kent State             |Buffalo                     |          21|          41|      -20|
|   2013|    9|Central Florida        |Connecticut                 |          62|          17|       45|
|   2013|    9|Maryland               |Clemson                     |          27|          40|      -13|
|   2013|    9|Hawaii                 |Colorado State              |          28|          35|       -7|
|   2013|    9|Virginia Tech          |Duke                        |          10|          13|       -3|
|   2013|    9|Florida State          |North Carolina State        |          49|          17|       32|
|   2013|    9|San Diego State        |Fresno State                |          28|          35|       -7|
|   2013|    9|Virginia               |Georgia Tech                |          25|          35|      -10|
|   2013|    9|Rutgers                |Houston                     |          14|          49|      -35|
|   2013|    9|Iowa                   |Northwestern                |          17|          10|        7|
|   2013|    9|Kansas State           |West Virginia               |          35|          12|       23|
|   2013|    9|Louisiana State        |Furman                      |          48|          16|       32|
|   2013|    9|Florida International  |Louisiana Tech              |           7|          23|      -16|
|   2013|    9|Louisiana-Monroe       |Georgia State               |          38|          10|       28|
|   2013|    9|South Florida          |Louisville                  |           3|          34|      -31|
|   2013|    9|Miami (FL)             |Wake Forest                 |          24|          21|        3|
|   2013|    9|Illinois               |Michigan State              |           3|          42|      -39|
|   2013|    9|Minnesota              |Nebraska                    |          34|          23|       11|
|   2013|    9|Mississippi            |Idaho                       |          59|          14|       45|
|   2013|    9|Navy                   |Pittsburgh                  |          24|          21|        3|
|   2013|    9|Nevada                 |Nevada-Las Vegas            |          22|          27|       -5|
|   2013|    9|New Mexico State       |Abilene Christian           |          34|          29|        5|
|   2013|    9|North Carolina         |Boston College              |          34|          10|       24|
|   2013|    9|Southern Mississippi   |North Texas                 |          14|          55|      -41|
|   2013|    9|Northern Illinois      |Eastern Michigan            |          59|          20|       39|
|   2013|    9|Air Force              |Notre Dame                  |          10|          45|      -35|
|   2013|    9|Ohio                   |Miami (OH)                  |          41|          16|       25|
|   2013|    9|Ohio State             |Penn State                  |          63|          14|       49|
|   2013|    9|Oklahoma               |Texas Tech                  |          38|          30|        8|
|   2013|    9|Iowa State             |Oklahoma State              |          27|          58|      -31|
|   2013|    9|Oregon                 |UCLA                        |          42|          14|       28|
|   2013|    9|Rice                   |Texas-El Paso               |          45|           7|       38|
|   2013|    9|San Jose State         |Wyoming                     |          51|          44|        7|
|   2013|    9|Missouri               |South Carolina              |          24|          27|       -3|
|   2013|    9|Southern California    |Utah                        |          19|           3|       16|
|   2013|    9|Southern Methodist     |Temple                      |          59|          49|       10|
|   2013|    9|Oregon State           |Stanford                    |          12|          20|       -8|
|   2013|    9|Texas Christian        |Texas                       |           7|          30|      -23|
|   2013|    9|Texas A&M              |Vanderbilt                  |          56|          24|       32|
|   2013|    9|Texas State            |South Alabama               |          33|          31|        2|
|   2013|    9|Texas-San Antonio      |Alabama-Birmingham          |          52|          31|       21|
|   2013|    9|Bowling Green State    |Toledo                      |          25|          28|       -3|
|   2013|    9|Western Kentucky       |Troy                        |          26|          32|       -6|
|   2013|    9|Tulane                 |Tulsa                       |          14|           7|        7|
|   2013|    9|Washington             |California                  |          41|          17|       24|
|   2013|    9|Massachusetts          |Western Michigan            |          30|          31|       -1|
|   2013|   10|Memphis                |Cincinnati                  |          21|          34|      -13|
|   2013|   10|Washington State       |Arizona State               |          21|          55|      -34|
|   2013|   10|Houston                |South Florida               |          35|          23|       12|
|   2013|   10|Troy                   |Louisiana-Monroe            |          37|          49|      -12|
|   2013|   10|North Texas            |Rice                        |          28|          16|       12|
|   2013|   10|Oregon State           |Southern California         |          14|          31|      -17|
|   2013|   10|Air Force              |Army                        |          42|          28|       14|
|   2013|   10|Akron                  |Kent State                  |          16|           7|        9|
|   2013|   10|California             |Arizona                     |          28|          33|       -5|
|   2013|   10|South Alabama          |Arkansas State              |          16|          17|       -1|
|   2013|   10|Arkansas               |Auburn                      |          17|          35|      -18|
|   2013|   10|Colorado State         |Boise State                 |          30|          42|      -12|
|   2013|   10|Boston College         |Virginia Tech               |          34|          27|        7|
|   2013|   10|Virginia               |Clemson                     |          10|          59|      -49|
|   2013|   10|Florida International  |East Carolina               |          13|          34|      -21|
|   2013|   10|Florida Atlantic       |Tulane                      |          34|          17|       17|
|   2013|   10|Florida State          |Miami (FL)                  |          41|          14|       27|
|   2013|   10|Fresno State           |Nevada                      |          41|          23|       18|
|   2013|   10|Georgia                |Florida                     |          23|          20|        3|
|   2013|   10|Georgia Tech           |Pittsburgh                  |          21|          10|       11|
|   2013|   10|Kansas State           |Iowa State                  |          41|           7|       34|
|   2013|   10|Kentucky               |Alabama State               |          48|          14|       34|
|   2013|   10|Louisiana-Lafayette    |New Mexico State            |          49|          35|       14|
|   2013|   10|Marshall               |Southern Mississippi        |          61|          13|       48|
|   2013|   10|Michigan State         |Michigan                    |          29|           6|       23|
|   2013|   10|Alabama-Birmingham     |Middle Tennessee State      |          21|          24|       -3|
|   2013|   10|Indiana                |Minnesota                   |          39|          42|       -3|
|   2013|   10|Missouri               |Tennessee                   |          31|           3|       28|
|   2013|   10|Nebraska               |Northwestern                |          27|          24|        3|
|   2013|   10|North Carolina State   |North Carolina              |          19|          27|       -8|
|   2013|   10|Massachusetts          |Northern Illinois           |          19|          63|      -44|
|   2013|   10|Notre Dame             |Navy                        |          38|          34|        4|
|   2013|   10|Purdue                 |Ohio State                  |           0|          56|      -56|
|   2013|   10|Texas Tech             |Oklahoma State              |          34|          52|      -18|
|   2013|   10|Penn State             |Illinois                    |          24|          17|        7|
|   2013|   10|Rutgers                |Temple                      |          23|          20|        3|
|   2013|   10|San Diego State        |New Mexico                  |          35|          30|        5|
|   2013|   10|Nevada-Las Vegas       |San Jose State              |          24|          34|      -10|
|   2013|   10|South Carolina         |Mississippi State           |          34|          16|       18|
|   2013|   10|Syracuse               |Wake Forest                 |          13|           0|       13|
|   2013|   10|Texas                  |Kansas                      |          35|          13|       22|
|   2013|   10|Texas A&M              |Texas-El Paso               |          57|           7|       50|
|   2013|   10|Idaho                  |Texas State                 |          21|          37|      -16|
|   2013|   10|Tulsa                  |Texas-San Antonio           |          15|          34|      -19|
|   2013|   10|Toledo                 |Eastern Michigan            |          55|          16|       39|
|   2013|   10|UCLA                   |Colorado                    |          45|          23|       22|
|   2013|   10|Utah State             |Hawaii                      |          47|          10|       37|
|   2013|   10|Texas Christian        |West Virginia               |          27|          30|       -3|
|   2013|   10|Georgia State          |Western Kentucky            |          28|          44|      -16|
|   2013|   10|Iowa                   |Wisconsin                   |           9|          28|      -19|
|   2013|   11|Miami (OH)             |Bowling Green State         |           3|          45|      -42|
|   2013|   11|Buffalo                |Ohio                        |          30|           3|       27|
|   2013|   11|Ball State             |Central Michigan            |          44|          24|       20|
|   2013|   11|Baylor                 |Oklahoma                    |          41|          12|       29|
|   2013|   11|Louisiana-Lafayette    |Troy                        |          41|          36|        5|
|   2013|   11|Stanford               |Oregon                      |          26|          20|        6|
|   2013|   11|Connecticut            |Louisville                  |          10|          31|      -21|
|   2013|   11|New Mexico             |Air Force                   |          45|          37|        8|
|   2013|   11|Alabama                |Louisiana State             |          38|          17|       21|
|   2013|   11|Utah                   |Arizona State               |          19|          20|       -1|
|   2013|   11|Louisiana-Monroe       |Arkansas State              |          14|          42|      -28|
|   2013|   11|Tennessee              |Auburn                      |          23|          55|      -32|
|   2013|   11|New Mexico State       |Boston College              |          34|          48|      -14|
|   2013|   11|Central Florida        |Houston                     |          19|          14|        5|
|   2013|   11|Cincinnati             |Southern Methodist          |          28|          25|        3|
|   2013|   11|Colorado State         |Nevada                      |          38|          17|       21|
|   2013|   11|Duke                   |North Carolina State        |          38|          20|       18|
|   2013|   11|East Carolina          |Tulsa                       |          58|          24|       34|
|   2013|   11|Eastern Michigan       |Western Michigan            |          35|          32|        3|
|   2013|   11|Wake Forest            |Florida State               |           3|          59|      -56|
|   2013|   11|Wyoming                |Fresno State                |          10|          48|      -38|
|   2013|   11|Georgia                |Appalachian State           |          45|           6|       39|
|   2013|   11|Indiana                |Illinois                    |          52|          35|       17|
|   2013|   11|Purdue                 |Iowa                        |          14|          38|      -24|
|   2013|   11|Texas Tech             |Kansas State                |          26|          49|      -23|
|   2013|   11|Louisiana Tech         |Southern Mississippi        |          36|          13|       23|
|   2013|   11|Marshall               |Alabama-Birmingham          |          56|          14|       42|
|   2013|   11|Memphis                |Tennessee-Martin            |          21|           6|       15|
|   2013|   11|Middle Tennessee State |Florida International       |          48|           0|       48|
|   2013|   11|Minnesota              |Penn State                  |          24|          10|       14|
|   2013|   11|Mississippi            |Arkansas                    |          34|          24|       10|
|   2013|   11|Kentucky               |Missouri                    |          17|          48|      -31|
|   2013|   11|Navy                   |Hawaii                      |          42|          28|       14|
|   2013|   11|Michigan               |Nebraska                    |          13|          17|       -4|
|   2013|   11|North Carolina         |Virginia                    |          45|          14|       31|
|   2013|   11|North Texas            |Texas-El Paso               |          41|           7|       34|
|   2013|   11|Oklahoma State         |Kansas                      |          42|           6|       36|
|   2013|   11|Idaho                  |Old Dominion                |          38|          59|      -21|
|   2013|   11|Pittsburgh             |Notre Dame                  |          28|          21|        7|
|   2013|   11|San Jose State         |San Diego State             |          30|          34|       -4|
|   2013|   11|California             |Southern California         |          28|          62|      -34|
|   2013|   11|Maryland               |Syracuse                    |           3|          20|      -17|
|   2013|   11|West Virginia          |Texas                       |          40|          47|       -7|
|   2013|   11|Texas A&M              |Mississippi State           |          51|          41|       10|
|   2013|   11|Iowa State             |Texas Christian             |          17|          21|       -4|
|   2013|   11|Texas-San Antonio      |Tulane                      |          10|           7|        3|
|   2013|   11|Arizona                |UCLA                        |          26|          31|       -5|
|   2013|   11|Nevada-Las Vegas       |Utah State                  |          24|          28|       -4|
|   2013|   11|Florida                |Vanderbilt                  |          17|          34|      -17|
|   2013|   11|Miami (FL)             |Virginia Tech               |          24|          42|      -18|
|   2013|   11|Washington             |Colorado                    |          59|           7|       52|
|   2013|   11|Army                   |Western Kentucky            |          17|          21|       -4|
|   2013|   11|Wisconsin              |Brigham Young               |          27|          17|       10|
|   2013|   12|Bowling Green State    |Ohio                        |          49|           0|       49|
|   2013|   12|Toledo                 |Buffalo                     |          51|          41|       10|
|   2013|   12|Kent State             |Miami (OH)                  |          24|           6|       18|
|   2013|   12|Northern Illinois      |Ball State                  |          48|          27|       21|
|   2013|   12|Clemson                |Georgia Tech                |          55|          31|       24|
|   2013|   12|Tulsa                  |Marshall                    |          34|          45|      -11|
|   2013|   12|UCLA                   |Washington                  |          41|          31|       10|
|   2013|   12|Akron                  |Massachusetts               |          14|          13|        1|
|   2013|   12|Mississippi State      |Alabama                     |           7|          20|      -13|
|   2013|   12|Arizona State          |Oregon State                |          30|          17|       13|
|   2013|   12|Arkansas State         |Texas State                 |          38|          21|       17|
|   2013|   12|Auburn                 |Georgia                     |          43|          38|        5|
|   2013|   12|Baylor                 |Texas Tech                  |          63|          34|       29|
|   2013|   12|Boise State            |Wyoming                     |          48|           7|       41|
|   2013|   12|Boston College         |North Carolina State        |          38|          21|       17|
|   2013|   12|Brigham Young          |Idaho State                 |          59|          13|       46|
|   2013|   12|Temple                 |Central Florida             |          36|          39|       -3|
|   2013|   12|Western Michigan       |Central Michigan            |          22|          27|       -5|
|   2013|   12|Rutgers                |Cincinnati                  |          17|          52|      -35|
|   2013|   12|Colorado               |California                  |          41|          24|       17|
|   2013|   12|New Mexico             |Colorado State              |          42|          66|      -24|
|   2013|   12|Duke                   |Miami (FL)                  |          48|          30|       18|
|   2013|   12|East Carolina          |Alabama-Birmingham          |          63|          14|       49|
|   2013|   12|Southern Mississippi   |Florida Atlantic            |           7|          41|      -34|
|   2013|   12|Florida State          |Syracuse                    |          59|           3|       56|
|   2013|   12|Kansas                 |West Virginia               |          31|          19|       12|
|   2013|   12|Kansas State           |Texas Christian             |          33|          31|        2|
|   2013|   12|Georgia State          |Louisiana-Lafayette         |          21|          35|      -14|
|   2013|   12|Louisville             |Houston                     |          20|          13|        7|
|   2013|   12|Virginia Tech          |Maryland                    |          24|          27|       -3|
|   2013|   12|South Florida          |Memphis                     |          10|          23|      -13|
|   2013|   12|Northwestern           |Michigan                    |          19|          27|       -8|
|   2013|   12|Nebraska               |Michigan State              |          28|          41|      -13|
|   2013|   12|Mississippi            |Troy                        |          51|          21|       30|
|   2013|   12|Navy                   |South Alabama               |          42|          14|       28|
|   2013|   12|Nevada                 |San Jose State              |          38|          16|       22|
|   2013|   12|Pittsburgh             |North Carolina              |          27|          34|       -7|
|   2013|   12|Illinois               |Ohio State                  |          35|          60|      -25|
|   2013|   12|Oklahoma               |Iowa State                  |          48|          10|       38|
|   2013|   12|Texas                  |Oklahoma State              |          13|          38|      -25|
|   2013|   12|Oregon                 |Utah                        |          44|          21|       23|
|   2013|   12|Penn State             |Purdue                      |          45|          21|       24|
|   2013|   12|Rice                   |Louisiana Tech              |          52|          14|       38|
|   2013|   12|Hawaii                 |San Diego State             |          21|          28|       -7|
|   2013|   12|South Carolina         |Florida                     |          19|          14|        5|
|   2013|   12|Southern California    |Stanford                    |          20|          17|        3|
|   2013|   12|Southern Methodist     |Connecticut                 |          38|          21|       17|
|   2013|   12|Texas-El Paso          |Florida International       |          33|          10|       23|
|   2013|   12|Vanderbilt             |Kentucky                    |          22|           6|       16|
|   2013|   12|Arizona                |Washington State            |          17|          24|       -7|
|   2013|   12|Wisconsin              |Indiana                     |          51|           3|       48|
|   2013|   13|Miami (OH)             |Buffalo                     |           7|          44|      -37|
|   2013|   13|Ohio                   |Kent State                  |          13|          44|      -31|
|   2013|   13|Toledo                 |Northern Illinois           |          17|          35|      -18|
|   2013|   13|Central Florida        |Rutgers                     |          41|          17|       24|
|   2013|   13|Air Force              |Nevada-Las Vegas            |          21|          41|      -20|
|   2013|   13|Alabama-Birmingham     |Rice                        |          34|          37|       -3|
|   2013|   13|San Jose State         |Navy                        |          52|          58|       -6|
|   2013|   13|Alabama                |Chattanooga                 |          49|           0|       49|
|   2013|   13|Arizona                |Oregon                      |          42|          16|       26|
|   2013|   13|UCLA                   |Arizona State               |          33|          38|       -5|
|   2013|   13|Arkansas State         |Georgia State               |          35|          33|        2|
|   2013|   13|Maryland               |Boston College              |          26|          29|       -3|
|   2013|   13|Eastern Michigan       |Bowling Green State         |           7|          58|      -51|
|   2013|   13|Central Michigan       |Massachusetts               |          37|           0|       37|
|   2013|   13|Cincinnati             |Houston                     |          24|          17|        7|
|   2013|   13|Clemson                |Citadel                     |          52|           6|       46|
|   2013|   13|Temple                 |Connecticut                 |          21|          28|       -7|
|   2013|   13|Wake Forest            |Duke                        |          21|          28|       -7|
|   2013|   13|North Carolina State   |East Carolina               |          28|          42|      -14|
|   2013|   13|Florida Atlantic       |New Mexico State            |          55|          10|       45|
|   2013|   13|Florida State          |Idaho                       |          80|          14|       66|
|   2013|   13|Fresno State           |New Mexico                  |          69|          28|       41|
|   2013|   13|Georgia                |Kentucky                    |          59|          17|       42|
|   2013|   13|Florida                |Georgia Southern            |          20|          26|       -6|
|   2013|   13|Georgia Tech           |Alabama A&M                 |          66|           7|       59|
|   2013|   13|Purdue                 |Illinois                    |          16|          20|       -4|
|   2013|   13|Iowa                   |Michigan                    |          24|          21|        3|
|   2013|   13|Iowa State             |Kansas                      |          34|           0|       34|
|   2013|   13|Louisiana State        |Texas A&M                   |          34|          10|       24|
|   2013|   13|Louisville             |Memphis                     |          24|          17|        7|
|   2013|   13|Florida International  |Marshall                    |          10|          48|      -38|
|   2013|   13|Miami (FL)             |Virginia                    |          45|          26|       19|
|   2013|   13|Northwestern           |Michigan State              |           6|          30|      -24|
|   2013|   13|Southern Mississippi   |Middle Tennessee State      |          21|          42|      -21|
|   2013|   13|Arkansas               |Mississippi State           |          17|          24|       -7|
|   2013|   13|Mississippi            |Missouri                    |          10|          24|      -14|
|   2013|   13|Penn State             |Nebraska                    |          20|          23|       -3|
|   2013|   13|North Carolina         |Old Dominion                |          80|          20|       60|
|   2013|   13|Notre Dame             |Brigham Young               |          23|          13|       10|
|   2013|   13|Ohio State             |Indiana                     |          42|          14|       28|
|   2013|   13|Kansas State           |Oklahoma                    |          31|          41|      -10|
|   2013|   13|Oklahoma State         |Baylor                      |          49|          17|       32|
|   2013|   13|Syracuse               |Pittsburgh                  |          16|          17|       -1|
|   2013|   13|San Diego State        |Boise State                 |          34|          31|        3|
|   2013|   13|South Alabama          |Louisiana-Monroe            |          36|          14|       22|
|   2013|   13|South Carolina         |Coastal Carolina            |          70|          10|       60|
|   2013|   13|Colorado               |Southern California         |          29|          47|      -18|
|   2013|   13|South Florida          |Southern Methodist          |           6|          16|      -10|
|   2013|   13|Stanford               |California                  |          63|          13|       50|
|   2013|   13|North Texas            |Texas-San Antonio           |          13|          21|       -8|
|   2013|   13|Tulane                 |Texas-El Paso               |          45|           3|       42|
|   2013|   13|Louisiana Tech         |Tulsa                       |          14|          24|      -10|
|   2013|   13|Utah State             |Colorado State              |          13|           0|       13|
|   2013|   13|Tennessee              |Vanderbilt                  |          10|          14|       -4|
|   2013|   13|Oregon State           |Washington                  |          27|          69|      -42|
|   2013|   13|Washington State       |Utah                        |          49|          37|       12|
|   2013|   13|Texas State            |Western Kentucky            |           7|          38|      -31|
|   2013|   13|Minnesota              |Wisconsin                   |           7|          20|      -13|
|   2013|   13|Wyoming                |Hawaii                      |          59|          56|        3|
|   2013|   14|Northern Illinois      |Western Michigan            |          33|          14|       19|
|   2013|   14|Mississippi State      |Mississippi                 |          17|          10|        7|
|   2013|   14|Texas                  |Texas Tech                  |          41|          16|       25|
|   2013|   14|Akron                  |Toledo                      |          31|          29|        2|
|   2013|   14|Ball State             |Miami (OH)                  |          55|          14|       41|
|   2013|   14|Bowling Green State    |Buffalo                     |          24|           7|       17|
|   2013|   14|Central Florida        |South Florida               |          23|          20|        3|
|   2013|   14|Central Michigan       |Eastern Michigan            |          42|          10|       32|
|   2013|   14|Florida Atlantic       |Florida International       |          21|           6|       15|
|   2013|   14|Houston                |Southern Methodist          |          34|           0|       34|
|   2013|   14|Nebraska               |Iowa                        |          17|          38|      -21|
|   2013|   14|Louisiana State        |Arkansas                    |          31|          27|        4|
|   2013|   14|Marshall               |East Carolina               |          59|          28|       31|
|   2013|   14|Pittsburgh             |Miami (FL)                  |          31|          41|      -10|
|   2013|   14|Ohio                   |Massachusetts               |          51|          23|       28|
|   2013|   14|Oregon                 |Oregon State                |          36|          35|        1|
|   2013|   14|San Jose State         |Fresno State                |          62|          52|       10|
|   2013|   14|Troy                   |Texas State                 |          42|          28|       14|
|   2013|   14|Washington             |Washington State            |          27|          17|       10|
|   2013|   14|Arizona State          |Arizona                     |          58|          21|       37|
|   2013|   14|Auburn                 |Alabama                     |          34|          28|        6|
|   2013|   14|Texas Christian        |Baylor                      |          38|          41|       -3|
|   2013|   14|Boise State            |New Mexico                  |          45|          17|       28|
|   2013|   14|Nevada                 |Brigham Young               |          23|          28|       -5|
|   2013|   14|Colorado State         |Air Force                   |          58|          13|       45|
|   2013|   14|Connecticut            |Rutgers                     |          28|          17|       11|
|   2013|   14|North Carolina         |Duke                        |          25|          27|       -2|
|   2013|   14|Florida                |Florida State               |           7|          37|      -30|
|   2013|   14|Georgia Tech           |Georgia                     |          34|          41|       -7|
|   2013|   14|Hawaii                 |Army                        |          49|          42|        7|
|   2013|   14|Indiana                |Purdue                      |          56|          36|       20|
|   2013|   14|West Virginia          |Iowa State                  |          44|          52|       -8|
|   2013|   14|Kansas                 |Kansas State                |          10|          31|      -21|
|   2013|   14|Louisiana-Lafayette    |Louisiana-Monroe            |          28|          31|       -3|
|   2013|   14|North Carolina State   |Maryland                    |          21|          41|      -20|
|   2013|   14|Michigan State         |Minnesota                   |          14|           3|       11|
|   2013|   14|Middle Tennessee State |Texas-El Paso               |          48|          17|       31|
|   2013|   14|Missouri               |Texas A&M                   |          28|          21|        7|
|   2013|   14|Nevada-Las Vegas       |San Diego State             |          45|          19|       26|
|   2013|   14|New Mexico State       |Idaho                       |          24|          16|        8|
|   2013|   14|Tulsa                  |North Texas                 |          10|          42|      -32|
|   2013|   14|Illinois               |Northwestern                |          34|          37|       -3|
|   2013|   14|Michigan               |Ohio State                  |          41|          42|       -1|
|   2013|   14|Wisconsin              |Penn State                  |          24|          31|       -7|
|   2013|   14|Rice                   |Tulane                      |          17|          13|        4|
|   2013|   14|Georgia State          |South Alabama               |          17|          38|      -21|
|   2013|   14|South Carolina         |Clemson                     |          31|          17|       14|
|   2013|   14|Alabama-Birmingham     |Southern Mississippi        |          27|          62|      -35|
|   2013|   14|Stanford               |Notre Dame                  |          27|          20|        7|
|   2013|   14|Syracuse               |Boston College              |          34|          31|        3|
|   2013|   14|Memphis                |Temple                      |          21|          41|      -20|
|   2013|   14|Kentucky               |Tennessee                   |          14|          27|      -13|
|   2013|   14|Texas-San Antonio      |Louisiana Tech              |          30|          10|       20|
|   2013|   14|Southern California    |UCLA                        |          14|          35|      -21|
|   2013|   14|Utah                   |Colorado                    |          24|          17|        7|
|   2013|   14|Utah State             |Wyoming                     |          35|           7|       28|
|   2013|   14|Vanderbilt             |Wake Forest                 |          23|          21|        2|
|   2013|   14|Virginia               |Virginia Tech               |           6|          16|      -10|
|   2013|   14|Western Kentucky       |Arkansas State              |          34|          31|        3|
|   2013|   15|Cincinnati             |Louisville                  |          24|          31|       -7|
|   2013|   15|Bowling Green State    |Northern Illinois           |          47|          27|       20|
|   2013|   15|Auburn                 |Missouri                    |          59|          42|       17|
|   2013|   15|Baylor                 |Texas                       |          30|          10|       20|
|   2013|   15|Southern Methodist     |Central Florida             |          13|          17|       -4|
|   2013|   15|Connecticut            |Memphis                     |          45|          10|       35|
|   2013|   15|Florida State          |Duke                        |          45|           7|       38|
|   2013|   15|Fresno State           |Utah State                  |          24|          17|        7|
|   2013|   15|Michigan State         |Ohio State                  |          34|          24|       10|
|   2013|   15|Oklahoma State         |Oklahoma                    |          24|          33|       -9|
|   2013|   15|Rice                   |Marshall                    |          41|          24|       17|
|   2013|   15|Rutgers                |South Florida               |          31|           6|       25|
|   2013|   15|South Alabama          |Louisiana-Lafayette         |          30|           8|       22|
|   2013|   15|Arizona State          |Stanford                    |          14|          38|      -24|
|   2013|   16|Navy                   |Army                        |          34|           7|       27|
|   2013|   17|Colorado State         |Washington State            |          48|          45|        3|
|   2013|   17|Louisiana-Lafayette    |Tulane                      |          24|          21|        3|
|   2013|   17|San Diego State        |Buffalo                     |          49|          24|       25|
|   2013|   17|Southern California    |Fresno State                |          45|          20|       25|
|   2013|   17|East Carolina          |Ohio                        |          37|          20|       17|
|   2013|   18|Oregon State           |Boise State                 |          38|          23|       15|
|   2013|   18|Pittsburgh             |Bowling Green State         |          30|          27|        3|
|   2013|   18|Utah State             |Northern Illinois           |          21|          14|        7|
|   2013|   18|Marshall               |Maryland                    |          31|          20|       11|
|   2013|   18|Syracuse               |Minnesota                   |          21|          17|        4|
|   2013|   18|Washington             |Brigham Young               |          31|          16|       15|
|   2013|   18|Kansas State           |Michigan                    |          31|          14|       17|
|   2013|   18|Louisville             |Miami (FL)                  |          36|           9|       27|
|   2013|   18|North Carolina         |Cincinnati                  |          39|          17|       22|
|   2013|   18|Notre Dame             |Rutgers                     |          29|          16|       13|
|   2013|   19|Mississippi            |Georgia Tech                |          25|          17|        8|
|   2013|   19|Navy                   |Middle Tennessee State      |          24|           6|       18|
|   2013|   19|Oregon                 |Texas                       |          30|           7|       23|
|   2013|   19|Texas Tech             |Arizona State               |          37|          23|       14|
|   2013|   20|Arizona                |Boston College              |          42|          19|       23|
|   2013|   20|Mississippi State      |Rice                        |          44|           7|       37|
|   2013|   20|Texas A&M              |Duke                        |          52|          48|        4|
|   2013|   20|UCLA                   |Virginia Tech               |          42|          12|       30|
|   2013|   20|Central Florida        |Baylor                      |          52|          42|       10|
|   2013|   20|Louisiana State        |Iowa                        |          21|          14|        7|
|   2013|   20|Michigan State         |Stanford                    |          24|          20|        4|
|   2013|   20|Nebraska               |Georgia                     |          24|          19|        5|
|   2013|   20|North Texas            |Nevada-Las Vegas            |          36|          14|       22|
|   2013|   20|South Carolina         |Wisconsin                   |          34|          24|       10|
|   2013|   20|Oklahoma               |Alabama                     |          45|          31|       14|
|   2013|   20|Clemson                |Ohio State                  |          40|          35|        5|
|   2013|   20|Missouri               |Oklahoma State              |          41|          31|       10|
|   2013|   20|Vanderbilt             |Houston                     |          41|          24|       17|
|   2013|   20|Arkansas State         |Ball State                  |          23|          20|        3|
|   2013|   20|Florida State          |Auburn                      |          34|          31|        3|
|   2014|    1|Georgia State          |Abilene Christian           |          38|          37|        1|
|   2014|    1|Akron                  |Howard                      |          41|           0|       41|
|   2014|    1|Arizona State          |Weber State                 |          45|          14|       31|
|   2014|    1|Central Michigan       |Chattanooga                 |          20|          16|        4|
|   2014|    1|Louisiana-Monroe       |Wake Forest                 |          17|          10|        7|
|   2014|    1|Minnesota              |Eastern Illinois            |          42|          20|       22|
|   2014|    1|Mississippi            |Boise State                 |          35|          13|       22|
|   2014|    1|New Mexico State       |Cal Poly                    |          28|          10|       18|
|   2014|    1|Northern Illinois      |Presbyterian                |          55|           3|       52|
|   2014|    1|Rutgers                |Washington State            |          41|          38|        3|
|   2014|    1|San Jose State         |North Dakota                |          42|          10|       32|
|   2014|    1|Vanderbilt             |Temple                      |           7|          37|      -30|
|   2014|    1|South Carolina         |Texas A&M                   |          28|          52|      -24|
|   2014|    1|Tulsa                  |Tulane                      |          38|          31|        7|
|   2014|    1|Utah                   |Idaho State                 |          56|          14|       42|
|   2014|    1|Arizona                |Nevada-Las Vegas            |          58|          13|       45|
|   2014|    1|Connecticut            |Brigham Young               |          10|          35|      -25|
|   2014|    1|Colorado State         |Colorado                    |          31|          17|       14|
|   2014|    1|Michigan State         |Jacksonville State          |          45|           7|       38|
|   2014|    1|Syracuse               |Villanova                   |          27|          26|        1|
|   2014|    1|Houston                |Texas-San Antonio           |           7|          27|      -20|
|   2014|    1|Western Kentucky       |Bowling Green State         |          59|          31|       28|
|   2014|    1|Air Force              |Nicholls State              |          44|          16|       28|
|   2014|    1|Alabama                |West Virginia               |          33|          23|       10|
|   2014|    1|Alabama-Birmingham     |Troy                        |          48|          10|       38|
|   2014|    1|Arkansas State         |Montana State               |          37|          10|       27|
|   2014|    1|Auburn                 |Arkansas                    |          45|          21|       24|
|   2014|    1|Ball State             |Colgate                     |          30|          10|       20|
|   2014|    1|Florida International  |Bethune-Cookman             |          12|          14|       -2|
|   2014|    1|Boston College         |Massachusetts               |          30|           7|       23|
|   2014|    1|Buffalo                |Duquesne                    |          38|          28|       10|
|   2014|    1|Northwestern           |California                  |          24|          31|       -7|
|   2014|    1|Duke                   |Elon                        |          52|          13|       39|
|   2014|    1|East Carolina          |North Carolina Central      |          52|           7|       45|
|   2014|    1|Eastern Michigan       |Morgan State                |          31|          28|        3|
|   2014|    1|Florida State          |Oklahoma State              |          37|          31|        6|
|   2014|    1|Georgia                |Clemson                     |          45|          21|       24|
|   2014|    1|Georgia Tech           |Wofford                     |          38|          19|       19|
|   2014|    1|Illinois               |Youngstown State            |          28|          17|       11|
|   2014|    1|Indiana                |Indiana State               |          28|          10|       18|
|   2014|    1|Iowa                   |Northern Iowa               |          31|          23|        8|
|   2014|    1|Kansas State           |Stephen F. Austin           |          55|          16|       39|
|   2014|    1|Kentucky               |Tennessee-Martin            |          59|          14|       45|
|   2014|    1|Louisiana State        |Wisconsin                   |          28|          24|        4|
|   2014|    1|Louisiana-Lafayette    |Southern                    |          45|           6|       39|
|   2014|    1|Miami (OH)             |Marshall                    |          27|          42|      -15|
|   2014|    1|Maryland               |James Madison               |          52|           7|       45|
|   2014|    1|Memphis                |Austin Peay                 |          63|           0|       63|
|   2014|    1|Michigan               |Appalachian State           |          52|          14|       38|
|   2014|    1|Middle Tennessee State |Savannah State              |          61|           7|       54|
|   2014|    1|Mississippi State      |Southern Mississippi        |          49|           0|       49|
|   2014|    1|Missouri               |South Dakota State          |          38|          18|       20|
|   2014|    1|Nebraska               |Florida Atlantic            |          55|           7|       48|
|   2014|    1|Nevada                 |Southern Utah               |          28|          19|        9|
|   2014|    1|North Carolina         |Liberty                     |          56|          29|       27|
|   2014|    1|North Carolina State   |Georgia Southern            |          24|          23|        1|
|   2014|    1|Iowa State             |North Dakota State          |          14|          34|      -20|
|   2014|    1|Notre Dame             |Rice                        |          48|          17|       31|
|   2014|    1|Kent State             |Ohio                        |          14|          17|       -3|
|   2014|    1|Ohio State             |Navy                        |          34|          17|       17|
|   2014|    1|Oklahoma               |Louisiana Tech              |          48|          16|       32|
|   2014|    1|Old Dominion           |Hampton                     |          41|          28|       13|
|   2014|    1|Oregon                 |South Dakota                |          62|          13|       49|
|   2014|    1|Oregon State           |Portland State              |          29|          14|       15|
|   2014|    1|Penn State             |Central Florida             |          26|          24|        2|
|   2014|    1|Pittsburgh             |Delaware                    |          62|           0|       62|
|   2014|    1|Purdue                 |Western Michigan            |          43|          34|        9|
|   2014|    1|San Diego State        |Northern Arizona            |          38|           7|       31|
|   2014|    1|South Florida          |Western Carolina            |          36|          31|        5|
|   2014|    1|Southern California    |Fresno State                |          52|          13|       39|
|   2014|    1|Stanford               |California-Davis            |          45|           0|       45|
|   2014|    1|Texas                  |North Texas                 |          38|           7|       31|
|   2014|    1|Texas Christian        |Samford                     |          48|          14|       34|
|   2014|    1|Texas State            |Arkansas-Pine Bluff         |          65|           0|       65|
|   2014|    1|Texas Tech             |Central Arkansas            |          42|          35|        7|
|   2014|    1|New Mexico             |Texas-El Paso               |          24|          31|       -7|
|   2014|    1|Toledo                 |New Hampshire               |          54|          20|       34|
|   2014|    1|Virginia               |UCLA                        |          20|          28|       -8|
|   2014|    1|Virginia Tech          |William & Mary              |          34|           9|       25|
|   2014|    1|Hawaii                 |Washington                  |          16|          17|       -1|
|   2014|    1|Wyoming                |Montana                     |          17|          12|        5|
|   2014|    1|Baylor                 |Southern Methodist          |          45|           0|       45|
|   2014|    1|Tennessee              |Utah State                  |          38|           7|       31|
|   2014|    1|Louisville             |Miami (FL)                  |          31|          13|       18|
|   2014|    2|Texas-San Antonio      |Arizona                     |          23|          26|       -3|
|   2014|    2|Nevada                 |Washington State            |          24|          13|       11|
|   2014|    2|Boston College         |Pittsburgh                  |          20|          30|      -10|
|   2014|    2|Alabama                |Florida Atlantic            |          41|           0|       41|
|   2014|    2|Appalachian State      |Campbell                    |          66|           0|       66|
|   2014|    2|New Mexico             |Arizona State               |          23|          58|      -35|
|   2014|    2|Arkansas               |Nicholls State              |          73|           7|       66|
|   2014|    2|Army                   |Buffalo                     |          47|          39|        8|
|   2014|    2|Auburn                 |San Jose State              |          59|          13|       46|
|   2014|    2|Baylor                 |Northwestern State          |          70|           6|       64|
|   2014|    2|Boise State            |Colorado State              |          37|          24|       13|
|   2014|    2|Bowling Green State    |Virginia Military Institute |          48|           7|       41|
|   2014|    2|Texas                  |Brigham Young               |           7|          41|      -34|
|   2014|    2|California             |Sacramento State            |          55|          14|       41|
|   2014|    2|Purdue                 |Central Michigan            |          17|          38|      -21|
|   2014|    2|Clemson                |South Carolina State        |          73|           7|       66|
|   2014|    2|Colorado               |Massachusetts               |          41|          38|        3|
|   2014|    2|Connecticut            |Stony Brook                 |          19|          16|        3|
|   2014|    2|Troy                   |Duke                        |          17|          34|      -17|
|   2014|    2|Miami (OH)             |Eastern Kentucky            |          10|          17|       -7|
|   2014|    2|Florida                |Eastern Michigan            |          65|           0|       65|
|   2014|    2|Florida International  |Wagner                      |          34|           3|       31|
|   2014|    2|Florida State          |Citadel                     |          37|          12|       25|
|   2014|    2|Georgia Southern       |Savannah State              |          83|           9|       74|
|   2014|    2|Tulane                 |Georgia Tech                |          21|          38|      -17|
|   2014|    2|Houston                |Grambling State             |          47|           0|       47|
|   2014|    2|Illinois               |Western Kentucky            |          42|          34|        8|
|   2014|    2|Iowa                   |Ball State                  |          17|          13|        4|
|   2014|    2|Kansas                 |Southeast Missouri State    |          34|          28|        6|
|   2014|    2|Iowa State             |Kansas State                |          28|          32|       -4|
|   2014|    2|Kentucky               |Ohio                        |          20|           3|       17|
|   2014|    2|Louisiana State        |Sam Houston State           |          56|           0|       56|
|   2014|    2|Louisiana-Lafayette    |Louisiana Tech              |          20|          48|      -28|
|   2014|    2|Louisiana-Monroe       |Idaho                       |          38|          31|        7|
|   2014|    2|Louisville             |Murray State                |          66|          21|       45|
|   2014|    2|Marshall               |Rhode Island                |          48|           7|       41|
|   2014|    2|South Florida          |Maryland                    |          17|          24|       -7|
|   2014|    2|Miami (FL)             |Florida A&M                 |          41|           7|       34|
|   2014|    2|Minnesota              |Middle Tennessee State      |          35|          24|       11|
|   2014|    2|Mississippi            |Vanderbilt                  |          41|           3|       38|
|   2014|    2|Mississippi State      |Alabama-Birmingham          |          47|          34|       13|
|   2014|    2|Toledo                 |Missouri                    |          24|          49|      -25|
|   2014|    2|Temple                 |Navy                        |          24|          31|       -7|
|   2014|    2|Nebraska               |McNeese State               |          31|          24|        7|
|   2014|    2|Nevada-Las Vegas       |Northern Colorado           |          13|          12|        1|
|   2014|    2|Georgia State          |New Mexico State            |          31|          34|       -3|
|   2014|    2|North Carolina         |San Diego State             |          31|          27|        4|
|   2014|    2|North Carolina State   |Old Dominion                |          46|          34|       12|
|   2014|    2|North Texas            |Southern Methodist          |          43|           6|       37|
|   2014|    2|Northwestern           |Northern Illinois           |          15|          23|       -8|
|   2014|    2|Notre Dame             |Michigan                    |          31|           0|       31|
|   2014|    2|Tulsa                  |Oklahoma                    |           7|          52|      -45|
|   2014|    2|Oklahoma State         |Missouri State              |          40|          23|       17|
|   2014|    2|Oregon                 |Michigan State              |          46|          27|       19|
|   2014|    2|Hawaii                 |Oregon State                |          30|          38|       -8|
|   2014|    2|Penn State             |Akron                       |          21|           3|       18|
|   2014|    2|Rutgers                |Howard                      |          38|          25|       13|
|   2014|    2|Kent State             |South Alabama               |          13|          23|      -10|
|   2014|    2|South Carolina         |East Carolina               |          33|          23|       10|
|   2014|    2|Stanford               |Southern California         |          10|          13|       -3|
|   2014|    2|Southern Mississippi   |Alcorn State                |          26|          20|        6|
|   2014|    2|Tennessee              |Arkansas State              |          34|          19|       15|
|   2014|    2|Texas A&M              |Lamar                       |          73|           3|       70|
|   2014|    2|Texas-El Paso          |Texas Tech                  |          26|          30|       -4|
|   2014|    2|UCLA                   |Memphis                     |          42|          35|        7|
|   2014|    2|Utah                   |Fresno State                |          59|          27|       32|
|   2014|    2|Utah State             |Idaho State                 |          40|          20|       20|
|   2014|    2|Virginia               |Richmond                    |          45|          13|       32|
|   2014|    2|Ohio State             |Virginia Tech               |          21|          35|      -14|
|   2014|    2|Wake Forest            |Gardner-Webb                |          23|           7|       16|
|   2014|    2|Washington             |Eastern Washington          |          59|          52|        7|
|   2014|    2|West Virginia          |Towson                      |          54|           0|       54|
|   2014|    2|Wisconsin              |Western Illinois            |          37|           3|       34|
|   2014|    2|Wyoming                |Air Force                   |          17|          13|        4|
|   2014|    3|Brigham Young          |Houston                     |          33|          25|        8|
|   2014|    3|North Texas            |Louisiana Tech              |          21|          42|      -21|
|   2014|    3|Buffalo                |Baylor                      |          21|          63|      -42|
|   2014|    3|Cincinnati             |Toledo                      |          58|          34|       24|
|   2014|    3|Troy                   |Abilene Christian           |          35|          38|       -3|
|   2014|    3|Georgia State          |Air Force                   |          38|          48|      -10|
|   2014|    3|Alabama                |Southern Mississippi        |          52|          12|       40|
|   2014|    3|Alabama-Birmingham     |Alabama A&M                 |          41|          14|       27|
|   2014|    3|Arizona                |Nevada                      |          35|          28|        7|
|   2014|    3|Colorado               |Arizona State               |          24|          38|      -14|
|   2014|    3|Texas Tech             |Arkansas                    |          28|          49|      -21|
|   2014|    3|Connecticut            |Boise State                 |          21|          38|      -17|
|   2014|    3|Boston College         |Southern California         |          37|          31|        6|
|   2014|    3|Bowling Green State    |Indiana                     |          45|          42|        3|
|   2014|    3|Colorado State         |California-Davis            |          49|          21|       28|
|   2014|    3|Duke                   |Kansas                      |          41|           3|       38|
|   2014|    3|Virginia Tech          |East Carolina               |          21|          28|       -7|
|   2014|    3|Florida                |Kentucky                    |          36|          30|        6|
|   2014|    3|Florida Atlantic       |Tulsa                       |          50|          21|       29|
|   2014|    3|Georgia Tech           |Georgia Southern            |          42|          38|        4|
|   2014|    3|Hawaii                 |Northern Iowa               |          27|          24|        3|
|   2014|    3|Ball State             |Indiana State               |          20|          27|       -7|
|   2014|    3|Iowa                   |Iowa State                  |          17|          20|       -3|
|   2014|    3|Louisiana State        |Louisiana-Monroe            |          31|           0|       31|
|   2014|    3|Marshall               |Ohio                        |          44|          14|       30|
|   2014|    3|Miami (FL)             |Arkansas State              |          41|          20|       21|
|   2014|    3|Michigan               |Miami (OH)                  |          34|          10|       24|
|   2014|    3|Middle Tennessee State |Western Kentucky            |          50|          47|        3|
|   2014|    3|Mississippi            |Louisiana-Lafayette         |          56|          15|       41|
|   2014|    3|South Alabama          |Mississippi State           |           3|          35|      -32|
|   2014|    3|Missouri               |Central Florida             |          38|          10|       28|
|   2014|    3|Texas State            |Navy                        |          21|          35|      -14|
|   2014|    3|Fresno State           |Nebraska                    |          19|          55|      -36|
|   2014|    3|South Florida          |North Carolina State        |          17|          49|      -32|
|   2014|    3|Nevada-Las Vegas       |Northern Illinois           |          34|          48|      -14|
|   2014|    3|Notre Dame             |Purdue                      |          30|          14|       16|
|   2014|    3|Ohio State             |Kent State                  |          66|           0|       66|
|   2014|    3|Oklahoma               |Tennessee                   |          34|          10|       24|
|   2014|    3|Oklahoma State         |Texas-San Antonio           |          43|          13|       30|
|   2014|    3|Old Dominion           |Eastern Michigan            |          17|           3|       14|
|   2014|    3|Oregon                 |Wyoming                     |          48|          14|       34|
|   2014|    3|Rutgers                |Penn State                  |          10|          13|       -3|
|   2014|    3|Florida International  |Pittsburgh                  |          25|          42|      -17|
|   2014|    3|South Carolina         |Georgia                     |          38|          35|        3|
|   2014|    3|Stanford               |Army                        |          35|           0|       35|
|   2014|    3|Central Michigan       |Syracuse                    |           3|          40|      -37|
|   2014|    3|Texas A&M              |Rice                        |          38|          10|       28|
|   2014|    3|Texas Christian        |Minnesota                   |          30|           7|       23|
|   2014|    3|Texas-El Paso          |New Mexico State            |          42|          24|       18|
|   2014|    3|Tulane                 |Southeastern Louisiana      |          35|          20|       15|
|   2014|    3|UCLA                   |Texas                       |          20|          17|        3|
|   2014|    3|Utah State             |Wake Forest                 |          36|          24|       12|
|   2014|    3|Vanderbilt             |Massachusetts               |          34|          31|        3|
|   2014|    3|Virginia               |Louisville                  |          23|          21|        2|
|   2014|    3|Washington             |Illinois                    |          44|          19|       25|
|   2014|    3|Washington State       |Portland State              |          59|          21|       38|
|   2014|    3|Maryland               |West Virginia               |          37|          40|       -3|
|   2014|    3|Idaho                  |Western Michigan            |          33|          45|      -12|
|   2014|    4|Kansas State           |Auburn                      |          14|          20|       -6|
|   2014|    4|South Florida          |Connecticut                 |          17|          14|        3|
|   2014|    4|Alabama                |Florida                     |          42|          21|       21|
|   2014|    4|Arizona                |California                  |          49|          45|        4|
|   2014|    4|Arkansas               |Northern Illinois           |          52|          14|       38|
|   2014|    4|Arkansas State         |Utah State                  |          21|          14|        7|
|   2014|    4|Boise State            |Louisiana-Lafayette         |          34|           9|       25|
|   2014|    4|Boston College         |Maine                       |          40|          10|       30|
|   2014|    4|Brigham Young          |Virginia                    |          41|          33|        8|
|   2014|    4|Buffalo                |Norfolk State               |          36|           7|       29|
|   2014|    4|Central Florida        |Bethune-Cookman             |          41|           7|       34|
|   2014|    4|Cincinnati             |Miami (OH)                  |          31|          24|        7|
|   2014|    4|Colorado               |Hawaii                      |          21|          12|        9|
|   2014|    4|Duke                   |Tulane                      |          47|          13|       34|
|   2014|    4|East Carolina          |North Carolina              |          70|          41|       29|
|   2014|    4|Florida State          |Clemson                     |          23|          17|        6|
|   2014|    4|Fresno State           |Southern Utah               |          56|          16|       40|
|   2014|    4|Georgia                |Troy                        |          66|           0|       66|
|   2014|    4|South Alabama          |Georgia Southern            |           6|          28|      -22|
|   2014|    4|Virginia Tech          |Georgia Tech                |          24|          27|       -3|
|   2014|    4|Houston                |Nevada-Las Vegas            |          47|          14|       33|
|   2014|    4|Illinois               |Texas State                 |          42|          35|        7|
|   2014|    4|Missouri               |Indiana                     |          27|          31|       -4|
|   2014|    4|Pittsburgh             |Iowa                        |          20|          24|       -4|
|   2014|    4|Kansas                 |Central Michigan            |          24|          10|       14|
|   2014|    4|Florida International  |Louisville                  |           3|          34|      -31|
|   2014|    4|Akron                  |Marshall                    |          17|          48|      -31|
|   2014|    4|Syracuse               |Maryland                    |          20|          34|      -14|
|   2014|    4|Memphis                |Middle Tennessee State      |          36|          17|       19|
|   2014|    4|Michigan State         |Eastern Michigan            |          73|          14|       59|
|   2014|    4|Minnesota              |San Jose State              |          24|           7|       17|
|   2014|    4|Louisiana State        |Mississippi State           |          29|          34|       -5|
|   2014|    4|Nebraska               |Miami (FL)                  |          41|          31|       10|
|   2014|    4|New Mexico State       |New Mexico                  |          35|          38|       -3|
|   2014|    4|North Carolina State   |Presbyterian                |          42|           0|       42|
|   2014|    4|North Texas            |Nicholls State              |          77|           3|       74|
|   2014|    4|Northwestern           |Western Illinois            |          24|           7|       17|
|   2014|    4|Louisiana Tech         |Northwestern State          |          27|          30|       -3|
|   2014|    4|Ohio                   |Idaho                       |          36|          24|       12|
|   2014|    4|West Virginia          |Oklahoma                    |          33|          45|      -12|
|   2014|    4|Rice                   |Old Dominion                |          42|          45|       -3|
|   2014|    4|Washington State       |Oregon                      |          31|          38|       -7|
|   2014|    4|Oregon State           |San Diego State             |          28|           7|       21|
|   2014|    4|Penn State             |Massachusetts               |          48|           7|       41|
|   2014|    4|Purdue                 |Southern Illinois           |          35|          13|       22|
|   2014|    4|Navy                   |Rutgers                     |          24|          31|       -7|
|   2014|    4|Vanderbilt             |South Carolina              |          34|          48|      -14|
|   2014|    4|Southern Mississippi   |Appalachian State           |          21|          20|        1|
|   2014|    4|Temple                 |Delaware State              |          59|           0|       59|
|   2014|    4|Southern Methodist     |Texas A&M                   |           6|          58|      -52|
|   2014|    4|Toledo                 |Ball State                  |          34|          23|       11|
|   2014|    4|Michigan               |Utah                        |          10|          26|      -16|
|   2014|    4|Wake Forest            |Army                        |          24|          21|        3|
|   2014|    4|Washington             |Georgia State               |          45|          14|       31|
|   2014|    4|Western Michigan       |Murray State                |          45|          14|       31|
|   2014|    4|Wisconsin              |Bowling Green State         |          68|          17|       51|
|   2014|    4|Wyoming                |Florida Atlantic            |          20|          19|        1|
|   2014|    5|Georgia Southern       |Appalachian State           |          34|          14|       20|
|   2014|    5|Oklahoma State         |Texas Tech                  |          45|          35|       10|
|   2014|    5|Arizona State          |UCLA                        |          27|          62|      -35|
|   2014|    5|New Mexico             |Fresno State                |          24|          35|      -11|
|   2014|    5|Old Dominion           |Middle Tennessee State      |          28|          41|      -13|
|   2014|    5|Air Force              |Boise State                 |          28|          14|       14|
|   2014|    5|Pittsburgh             |Akron                       |          10|          21|      -11|
|   2014|    5|Auburn                 |Louisiana Tech              |          45|          17|       28|
|   2014|    5|Iowa State             |Baylor                      |          28|          49|      -21|
|   2014|    5|Massachusetts          |Bowling Green State         |          42|          47|       -5|
|   2014|    5|Buffalo                |Miami (OH)                  |          35|          27|        8|
|   2014|    5|California             |Colorado                    |          59|          56|        3|
|   2014|    5|Clemson                |North Carolina              |          50|          35|       15|
|   2014|    5|Boston College         |Colorado State              |          21|          24|       -3|
|   2014|    5|Florida Atlantic       |Texas-San Antonio           |          41|          37|        4|
|   2014|    5|Alabama-Birmingham     |Florida International       |          20|          34|      -14|
|   2014|    5|North Carolina State   |Florida State               |          41|          56|      -15|
|   2014|    5|Georgia                |Tennessee                   |          35|          32|        3|
|   2014|    5|Purdue                 |Iowa                        |          10|          24|      -14|
|   2014|    5|Kansas State           |Texas-El Paso               |          58|          28|       30|
|   2014|    5|Kentucky               |Vanderbilt                  |          17|           7|       10|
|   2014|    5|Louisiana State        |New Mexico State            |          63|           7|       56|
|   2014|    5|Louisiana-Monroe       |Troy                        |          22|          20|        2|
|   2014|    5|Louisville             |Wake Forest                 |          20|          10|       10|
|   2014|    5|Indiana                |Maryland                    |          15|          37|      -22|
|   2014|    5|Miami (FL)             |Duke                        |          22|          10|       12|
|   2014|    5|Michigan State         |Wyoming                     |          56|          14|       42|
|   2014|    5|Michigan               |Minnesota                   |          14|          30|      -16|
|   2014|    5|Mississippi            |Memphis                     |          24|           3|       21|
|   2014|    5|South Carolina         |Missouri                    |          20|          21|       -1|
|   2014|    5|Nebraska               |Illinois                    |          45|          14|       31|
|   2014|    5|San Jose State         |Nevada                      |          10|          21|      -11|
|   2014|    5|Penn State             |Northwestern                |           6|          29|      -23|
|   2014|    5|Notre Dame             |Syracuse                    |          31|          15|       16|
|   2014|    5|Ohio                   |Eastern Illinois            |          34|          19|       15|
|   2014|    5|Ohio State             |Cincinnati                  |          50|          28|       22|
|   2014|    5|Southern Mississippi   |Rice                        |          23|          41|      -18|
|   2014|    5|Rutgers                |Tulane                      |          31|           6|       25|
|   2014|    5|San Diego State        |Nevada-Las Vegas            |          34|          17|       17|
|   2014|    5|Idaho                  |South Alabama               |          10|          34|      -24|
|   2014|    5|Southern California    |Oregon State                |          35|          10|       25|
|   2014|    5|Washington             |Stanford                    |          13|          20|       -7|
|   2014|    5|Connecticut            |Temple                      |          10|          36|      -26|
|   2014|    5|Kansas                 |Texas                       |           0|          23|      -23|
|   2014|    5|Texas A&M              |Arkansas                    |          35|          28|        7|
|   2014|    5|Southern Methodist     |Texas Christian             |           0|          56|      -56|
|   2014|    5|Tulsa                  |Texas State                 |          34|          37|       -3|
|   2014|    5|Toledo                 |Central Michigan            |          42|          28|       14|
|   2014|    5|Virginia               |Kent State                  |          45|          13|       32|
|   2014|    5|Virginia Tech          |Western Michigan            |          35|          17|       18|
|   2014|    5|Utah                   |Washington State            |          27|          28|       -1|
|   2014|    5|Navy                   |Western Kentucky            |          27|          36|       -9|
|   2014|    5|Wisconsin              |South Florida               |          27|          10|       17|
|   2014|    5|Yale                   |Army                        |          49|          43|        6|
|   2014|    6|Oregon                 |Arizona                     |          24|          31|       -7|
|   2014|    6|Houston                |Central Florida             |          12|          17|       -5|
|   2014|    6|Florida International  |Florida Atlantic            |          38|          10|       28|
|   2014|    6|Fresno State           |San Diego State             |          24|          13|       11|
|   2014|    6|Syracuse               |Louisville                  |           6|          28|      -22|
|   2014|    6|Brigham Young          |Utah State                  |          20|          35|      -15|
|   2014|    6|Air Force              |Navy                        |          30|          21|        9|
|   2014|    6|Akron                  |Eastern Michigan            |          31|           6|       25|
|   2014|    6|Western Kentucky       |Alabama-Birmingham          |          39|          42|       -3|
|   2014|    6|Southern California    |Arizona State               |          34|          38|       -4|
|   2014|    6|Arkansas State         |Louisiana-Monroe            |          28|          14|       14|
|   2014|    6|Army                   |Ball State                  |          33|          24|        9|
|   2014|    6|Auburn                 |Louisiana State             |          41|           7|       34|
|   2014|    6|Texas                  |Baylor                      |           7|          28|      -21|
|   2014|    6|Nevada                 |Boise State                 |          46|          51|       -5|
|   2014|    6|Bowling Green State    |Buffalo                     |          36|          35|        1|
|   2014|    6|Washington State       |California                  |          59|          60|       -1|
|   2014|    6|Central Michigan       |Ohio                        |          28|          10|       18|
|   2014|    6|Clemson                |North Carolina State        |          41|           0|       41|
|   2014|    6|Colorado State         |Tulsa                       |          42|          17|       25|
|   2014|    6|East Carolina          |Southern Methodist          |          45|          24|       21|
|   2014|    6|Tennessee              |Florida                     |           9|          10|       -1|
|   2014|    6|Florida State          |Wake Forest                 |          43|           3|       40|
|   2014|    6|Georgia                |Vanderbilt                  |          44|          17|       27|
|   2014|    6|New Mexico State       |Georgia Southern            |          28|          36|       -8|
|   2014|    6|Georgia Tech           |Miami (FL)                  |          28|          17|       11|
|   2014|    6|Indiana                |North Texas                 |          49|          24|       25|
|   2014|    6|Kansas State           |Texas Tech                  |          45|          13|       32|
|   2014|    6|Kentucky               |South Carolina              |          45|          38|        7|
|   2014|    6|Louisiana Tech         |Texas-El Paso               |          55|           3|       52|
|   2014|    6|Louisiana-Lafayette    |Georgia State               |          34|          31|        3|
|   2014|    6|Old Dominion           |Marshall                    |          14|          56|      -42|
|   2014|    6|Memphis                |Cincinnati                  |          41|          14|       27|
|   2014|    6|Miami (OH)             |Massachusetts               |          42|          41|        1|
|   2014|    6|Michigan State         |Nebraska                    |          27|          22|        5|
|   2014|    6|Middle Tennessee State |Southern Mississippi        |          37|          31|        6|
|   2014|    6|Mississippi            |Alabama                     |          23|          17|        6|
|   2014|    6|Mississippi State      |Texas A&M                   |          48|          31|       17|
|   2014|    6|Texas-San Antonio      |New Mexico                  |           9|          21|      -12|
|   2014|    6|Northern Illinois      |Kent State                  |          17|          14|        3|
|   2014|    6|Northwestern           |Wisconsin                   |          20|          14|        6|
|   2014|    6|Notre Dame             |Stanford                    |          17|          14|        3|
|   2014|    6|Maryland               |Ohio State                  |          24|          52|      -28|
|   2014|    6|Oklahoma State         |Iowa State                  |          37|          20|       17|
|   2014|    6|Colorado               |Oregon State                |          31|          36|       -5|
|   2014|    6|Illinois               |Purdue                      |          27|          38|      -11|
|   2014|    6|Rice                   |Hawaii                      |          28|          14|       14|
|   2014|    6|Rutgers                |Michigan                    |          26|          24|        2|
|   2014|    6|San Jose State         |Nevada-Las Vegas            |          33|          10|       23|
|   2014|    6|Appalachian State      |South Alabama               |          21|          47|      -26|
|   2014|    6|Texas Christian        |Oklahoma                    |          37|          33|        4|
|   2014|    6|Texas State            |Idaho                       |          35|          30|        5|
|   2014|    6|Western Michigan       |Toledo                      |          19|          20|       -1|
|   2014|    6|UCLA                   |Utah                        |          28|          30|       -2|
|   2014|    6|Virginia               |Pittsburgh                  |          24|          19|        5|
|   2014|    6|North Carolina         |Virginia Tech               |          17|          34|      -17|
|   2014|    6|West Virginia          |Kansas                      |          33|          14|       19|
|   2014|    7|Central Florida        |Brigham Young               |          31|          24|        7|
|   2014|    7|Nevada-Las Vegas       |Fresno State                |          30|          27|        3|
|   2014|    7|New Mexico             |San Diego State             |          14|          24|      -10|
|   2014|    7|Stanford               |Washington State            |          34|          17|       17|
|   2014|    7|Akron                  |Miami (OH)                  |          29|          19|       10|
|   2014|    7|Arkansas               |Alabama                     |          13|          14|       -1|
|   2014|    7|Alabama-Birmingham     |North Texas                 |          56|          21|       35|
|   2014|    7|Georgia State          |Arkansas State              |          10|          52|      -42|
|   2014|    7|Baylor                 |Texas Christian             |          61|          58|        3|
|   2014|    7|North Carolina State   |Boston College              |          14|          30|      -16|
|   2014|    7|Ohio                   |Bowling Green State         |          13|          31|      -18|
|   2014|    7|Northern Illinois      |Central Michigan            |          17|          34|      -17|
|   2014|    7|Clemson                |Louisville                  |          23|          17|        6|
|   2014|    7|Nevada                 |Colorado State              |          24|          31|       -7|
|   2014|    7|Georgia Tech           |Duke                        |          25|          31|       -6|
|   2014|    7|South Florida          |East Carolina               |          17|          28|      -11|
|   2014|    7|Eastern Michigan       |Buffalo                     |          37|          27|       10|
|   2014|    7|Syracuse               |Florida State               |          20|          38|      -18|
|   2014|    7|Missouri               |Georgia                     |           0|          34|      -34|
|   2014|    7|Georgia Southern       |Idaho                       |          47|          24|       23|
|   2014|    7|Hawaii                 |Wyoming                     |          38|          28|       10|
|   2014|    7|Memphis                |Houston                     |          24|          28|       -4|
|   2014|    7|Iowa                   |Indiana                     |          45|          29|       16|
|   2014|    7|Iowa State             |Toledo                      |          37|          30|        7|
|   2014|    7|Kentucky               |Louisiana-Monroe            |          48|          14|       34|
|   2014|    7|Appalachian State      |Liberty                     |          48|          55|       -7|
|   2014|    7|Florida                |Louisiana State             |          27|          30|       -3|
|   2014|    7|Marshall               |Middle Tennessee State      |          49|          24|       25|
|   2014|    7|Kent State             |Massachusetts               |          17|          40|      -23|
|   2014|    7|Miami (FL)             |Cincinnati                  |          55|          34|       21|
|   2014|    7|Michigan               |Penn State                  |          18|          13|        5|
|   2014|    7|Purdue                 |Michigan State              |          31|          45|      -14|
|   2014|    7|Minnesota              |Northwestern                |          24|          17|        7|
|   2014|    7|Texas A&M              |Mississippi                 |          20|          35|      -15|
|   2014|    7|Mississippi State      |Auburn                      |          38|          23|       15|
|   2014|    7|Navy                   |Virginia Military Institute |          51|          14|       37|
|   2014|    7|Notre Dame             |North Carolina              |          50|          43|        7|
|   2014|    7|Oklahoma               |Texas                       |          31|          26|        5|
|   2014|    7|Kansas                 |Oklahoma State              |          20|          27|       -7|
|   2014|    7|UCLA                   |Oregon                      |          30|          42|      -12|
|   2014|    7|Army                   |Rice                        |          21|          41|      -20|
|   2014|    7|Arizona                |Southern California         |          26|          28|       -2|
|   2014|    7|Temple                 |Tulsa                       |          35|          24|       11|
|   2014|    7|Tennessee              |Chattanooga                 |          45|          10|       35|
|   2014|    7|Texas-El Paso          |Old Dominion                |          42|          35|        7|
|   2014|    7|Texas-San Antonio      |Florida International       |          16|          13|        3|
|   2014|    7|Troy                   |New Mexico State            |          41|          24|       17|
|   2014|    7|Tulane                 |Connecticut                 |          12|           3|        9|
|   2014|    7|Utah State             |Air Force                   |          34|          16|       18|
|   2014|    7|Vanderbilt             |Charleston Southern         |          21|          20|        1|
|   2014|    7|California             |Washington                  |           7|          31|      -24|
|   2014|    7|Texas Tech             |West Virginia               |          34|          37|       -3|
|   2014|    7|Ball State             |Western Michigan            |          38|          42|       -4|
|   2014|    7|Wisconsin              |Illinois                    |          38|          28|       10|
|   2014|    8|Texas State            |Louisiana-Lafayette         |          10|          34|      -24|
|   2014|    8|Pittsburgh             |Virginia Tech               |          21|          16|        5|
|   2014|    8|Oregon State           |Utah                        |          23|          29|       -6|
|   2014|    8|Boise State            |Fresno State                |          37|          27|       10|
|   2014|    8|Houston                |Temple                      |          31|          10|       21|
|   2014|    8|Air Force              |New Mexico                  |          35|          31|        4|
|   2014|    8|Alabama                |Texas A&M                   |          59|           0|       59|
|   2014|    8|Troy                   |Appalachian State           |          14|          53|      -39|
|   2014|    8|Arizona State          |Stanford                    |          26|          10|       16|
|   2014|    8|Central Michigan       |Ball State                  |          29|          32|       -3|
|   2014|    8|Central Florida        |Tulane                      |          20|          13|        7|
|   2014|    8|Southern Methodist     |Cincinnati                  |           3|          41|      -38|
|   2014|    8|Boston College         |Clemson                     |          13|          17|       -4|
|   2014|    8|Colorado State         |Utah State                  |          16|          13|        3|
|   2014|    8|Duke                   |Virginia                    |          20|          13|        7|
|   2014|    8|Florida Atlantic       |Western Kentucky            |          45|          38|        7|
|   2014|    8|Florida State          |Notre Dame                  |          31|          27|        4|
|   2014|    8|Georgia                |Arkansas                    |          45|          32|       13|
|   2014|    8|Idaho                  |New Mexico State            |          29|          17|       12|
|   2014|    8|Oklahoma               |Kansas State                |          30|          31|       -1|
|   2014|    8|Kent State             |Army                        |          39|          17|       22|
|   2014|    8|Louisiana State        |Kentucky                    |          41|           3|       38|
|   2014|    8|Louisiana Tech         |Texas-San Antonio           |          27|          20|        7|
|   2014|    8|Louisville             |North Carolina State        |          30|          18|       12|
|   2014|    8|Florida International  |Marshall                    |          13|          45|      -32|
|   2014|    8|Maryland               |Iowa                        |          38|          31|        7|
|   2014|    8|Massachusetts          |Eastern Michigan            |          36|          14|       22|
|   2014|    8|Indiana                |Michigan State              |          17|          56|      -39|
|   2014|    8|Middle Tennessee State |Alabama-Birmingham          |          34|          22|       12|
|   2014|    8|Minnesota              |Purdue                      |          39|          38|        1|
|   2014|    8|Mississippi            |Tennessee                   |          34|           3|       31|
|   2014|    8|Florida                |Missouri                    |          13|          42|      -29|
|   2014|    8|Northwestern           |Nebraska                    |          17|          38|      -21|
|   2014|    8|Brigham Young          |Nevada                      |          35|          42|       -7|
|   2014|    8|North Carolina         |Georgia Tech                |          48|          43|        5|
|   2014|    8|Northern Illinois      |Miami (OH)                  |          51|          41|       10|
|   2014|    8|Ohio                   |Akron                       |          23|          20|        3|
|   2014|    8|Ohio State             |Rutgers                     |          56|          17|       39|
|   2014|    8|Oregon                 |Washington                  |          45|          20|       25|
|   2014|    8|San Diego State        |Hawaii                      |          20|          10|       10|
|   2014|    8|Wyoming                |San Jose State              |          20|          27|       -7|
|   2014|    8|South Alabama          |Georgia State               |          30|          27|        3|
|   2014|    8|South Carolina         |Furman                      |          41|          10|       31|
|   2014|    8|Tulsa                  |South Florida               |          30|          38|       -8|
|   2014|    8|Southern California    |Colorado                    |          56|          28|       28|
|   2014|    8|North Texas            |Southern Mississippi        |          20|          30|      -10|
|   2014|    8|Wake Forest            |Syracuse                    |           7|          30|      -23|
|   2014|    8|Texas                  |Iowa State                  |          48|          45|        3|
|   2014|    8|Texas Christian        |Oklahoma State              |          42|           9|       33|
|   2014|    8|Texas Tech             |Kansas                      |          34|          21|       13|
|   2014|    8|California             |UCLA                        |          34|          36|       -2|
|   2014|    8|West Virginia          |Baylor                      |          41|          27|       14|
|   2014|    8|Bowling Green State    |Western Michigan            |          14|          26|      -12|
|   2014|    9|Louisiana-Lafayette    |Arkansas State              |          55|          40|       15|
|   2014|    9|East Carolina          |Connecticut                 |          31|          21|       10|
|   2014|    9|Virginia Tech          |Miami (FL)                  |           6|          30|      -24|
|   2014|    9|Boise State            |Brigham Young               |          55|          30|       25|
|   2014|    9|Cincinnati             |South Florida               |          34|          17|       17|
|   2014|    9|Oregon                 |California                  |          59|          41|       18|
|   2014|    9|South Alabama          |Troy                        |          27|          13|       14|
|   2014|    9|Tennessee              |Alabama                     |          20|          34|      -14|
|   2014|    9|Washington State       |Arizona                     |          37|          59|      -22|
|   2014|    9|Washington             |Arizona State               |          10|          24|      -14|
|   2014|    9|Arkansas               |Alabama-Birmingham          |          45|          17|       28|
|   2014|    9|Auburn                 |South Carolina              |          42|          35|        7|
|   2014|    9|Ball State             |Akron                       |          35|          21|       14|
|   2014|    9|Wake Forest            |Boston College              |          17|          23|       -6|
|   2014|    9|Central Florida        |Temple                      |          34|          14|       20|
|   2014|    9|Buffalo                |Central Michigan            |          14|          20|       -6|
|   2014|    9|Clemson                |Syracuse                    |          16|           6|       10|
|   2014|    9|Colorado State         |Wyoming                     |          45|          31|       14|
|   2014|    9|Georgia State          |Georgia Southern            |          31|          69|      -38|
|   2014|    9|Pittsburgh             |Georgia Tech                |          28|          56|      -28|
|   2014|    9|Illinois               |Minnesota                   |          28|          24|        4|
|   2014|    9|Kansas State           |Texas                       |          23|           0|       23|
|   2014|    9|Louisiana State        |Mississippi                 |          10|           7|        3|
|   2014|    9|Southern Mississippi   |Louisiana Tech              |          20|          31|      -11|
|   2014|    9|Marshall               |Florida Atlantic            |          35|          16|       19|
|   2014|    9|Southern Methodist     |Memphis                     |          10|          48|      -38|
|   2014|    9|Miami (OH)             |Kent State                  |          10|           3|        7|
|   2014|    9|Michigan State         |Michigan                    |          35|          11|       24|
|   2014|    9|Kentucky               |Mississippi State           |          31|          45|      -14|
|   2014|    9|Missouri               |Vanderbilt                  |          24|          14|       10|
|   2014|    9|Navy                   |San Jose State              |          41|          31|       10|
|   2014|    9|Nebraska               |Rutgers                     |          42|          24|       18|
|   2014|    9|Hawaii                 |Nevada                      |          18|          26|       -8|
|   2014|    9|Virginia               |North Carolina              |          27|          28|       -1|
|   2014|    9|Eastern Michigan       |Northern Illinois           |          17|          28|      -11|
|   2014|    9|Penn State             |Ohio State                  |          24|          31|       -7|
|   2014|    9|Rice                   |North Texas                 |          41|          21|       20|
|   2014|    9|Stanford               |Oregon State                |          38|          14|       24|
|   2014|    9|Texas Christian        |Texas Tech                  |          82|          27|       55|
|   2014|    9|Louisiana-Monroe       |Texas State                 |          18|          22|       -4|
|   2014|    9|Texas-San Antonio      |Texas-El Paso               |           0|          34|      -34|
|   2014|    9|Toledo                 |Massachusetts               |          42|          35|        7|
|   2014|    9|Colorado               |UCLA                        |          37|          40|       -3|
|   2014|    9|Utah                   |Southern California         |          24|          21|        3|
|   2014|    9|Utah State             |Nevada-Las Vegas            |          34|          20|       14|
|   2014|    9|Oklahoma State         |West Virginia               |          10|          34|      -24|
|   2014|    9|Western Kentucky       |Old Dominion                |          66|          51|       15|
|   2014|    9|Western Michigan       |Ohio                        |          42|          21|       21|
|   2014|    9|Wisconsin              |Maryland                    |          52|           7|       45|
|   2014|   10|Louisville             |Florida State               |          31|          42|      -11|
|   2014|   10|Georgia Southern       |Troy                        |          42|          10|       32|
|   2014|   10|Tulane                 |Cincinnati                  |          14|          38|      -24|
|   2014|   10|Memphis                |Tulsa                       |          40|          20|       20|
|   2014|   10|Army                   |Air Force                   |           6|          23|      -17|
|   2014|   10|Florida Atlantic       |Alabama-Birmingham          |          28|          31|       -3|
|   2014|   10|Appalachian State      |Georgia State               |          44|           0|       44|
|   2014|   10|Arizona State          |Utah                        |          19|          16|        3|
|   2014|   10|Idaho                  |Arkansas State              |          28|          44|      -16|
|   2014|   10|Mississippi            |Auburn                      |          31|          35|       -4|
|   2014|   10|Baylor                 |Kansas                      |          60|          14|       46|
|   2014|   10|Virginia Tech          |Boston College              |          31|          33|       -2|
|   2014|   10|Middle Tennessee State |Brigham Young               |           7|          27|      -20|
|   2014|   10|Oregon State           |California                  |          31|          45|      -14|
|   2014|   10|Eastern Michigan       |Central Michigan            |           7|          38|      -31|
|   2014|   10|San Jose State         |Colorado State              |          31|          38|       -7|
|   2014|   10|Connecticut            |Central Florida             |          37|          29|        8|
|   2014|   10|Pittsburgh             |Duke                        |          48|          51|       -3|
|   2014|   10|Florida                |Georgia                     |          38|          20|       18|
|   2014|   10|Georgia Tech           |Virginia                    |          35|          10|       25|
|   2014|   10|South Florida          |Houston                     |           3|          27|      -24|
|   2014|   10|Iowa                   |Northwestern                |          48|           7|       41|
|   2014|   10|Kansas State           |Oklahoma State              |          48|          14|       34|
|   2014|   10|Louisiana Tech         |Western Kentucky            |          59|          10|       49|
|   2014|   10|Louisiana-Lafayette    |South Alabama               |          19|           9|       10|
|   2014|   10|Penn State             |Maryland                    |          19|          20|       -1|
|   2014|   10|Miami (FL)             |North Carolina              |          47|          20|       27|
|   2014|   10|Michigan               |Indiana                     |          34|          10|       24|
|   2014|   10|Mississippi State      |Arkansas                    |          17|          10|        7|
|   2014|   10|Missouri               |Kentucky                    |          20|          10|       10|
|   2014|   10|Nebraska               |Purdue                      |          35|          14|       21|
|   2014|   10|Nevada                 |San Diego State             |          30|          14|       16|
|   2014|   10|Nevada-Las Vegas       |New Mexico                  |          28|          31|       -3|
|   2014|   10|Syracuse               |North Carolina State        |          17|          24|       -7|
|   2014|   10|Notre Dame             |Navy                        |          49|          39|       10|
|   2014|   10|Ohio State             |Illinois                    |          55|          14|       41|
|   2014|   10|Iowa State             |Oklahoma                    |          14|          59|      -45|
|   2014|   10|Oregon                 |Stanford                    |          45|          16|       29|
|   2014|   10|Florida International  |Rice                        |          17|          31|      -14|
|   2014|   10|Washington State       |Southern California         |          17|          44|      -27|
|   2014|   10|Temple                 |East Carolina               |          20|          10|       10|
|   2014|   10|South Carolina         |Tennessee                   |          42|          45|       -3|
|   2014|   10|Texas Tech             |Texas                       |          13|          34|      -21|
|   2014|   10|Texas A&M              |Louisiana-Monroe            |          21|          16|        5|
|   2014|   10|West Virginia          |Texas Christian             |          30|          31|       -1|
|   2014|   10|New Mexico State       |Texas State                 |          29|          37|       -8|
|   2014|   10|Texas-El Paso          |Southern Mississippi        |          35|          14|       21|
|   2014|   10|UCLA                   |Arizona                     |          17|           7|       10|
|   2014|   10|Hawaii                 |Utah State                  |          14|          35|      -21|
|   2014|   10|Vanderbilt             |Old Dominion                |          42|          28|       14|
|   2014|   10|Colorado               |Washington                  |          23|          38|      -15|
|   2014|   10|Miami (OH)             |Western Michigan            |          10|          41|      -31|
|   2014|   10|Rutgers                |Wisconsin                   |           0|          37|      -37|
|   2014|   10|Fresno State           |Wyoming                     |          17|          45|      -28|
|   2014|   11|Akron                  |Bowling Green State         |          10|          27|      -17|
|   2014|   11|Kent State             |Toledo                      |          20|          30|      -10|
|   2014|   11|Ball State             |Northern Illinois           |          21|          35|      -14|
|   2014|   11|Ohio                   |Buffalo                     |          37|          14|       23|
|   2014|   11|Wake Forest            |Clemson                     |          20|          34|      -14|
|   2014|   11|Temple                 |Memphis                     |          13|          16|       -3|
|   2014|   11|Wyoming                |Utah State                  |           3|          20|      -17|
|   2014|   11|Nevada-Las Vegas       |Air Force                   |          21|          48|      -27|
|   2014|   11|Louisiana State        |Alabama                     |          13|          20|       -7|
|   2014|   11|Appalachian State      |Louisiana-Monroe            |          31|          29|        2|
|   2014|   11|Arizona                |Colorado                    |          38|          20|       18|
|   2014|   11|Arizona State          |Notre Dame                  |          55|          31|       24|
|   2014|   11|Arkansas State         |South Alabama               |          45|          10|       35|
|   2014|   11|Army                   |Connecticut                 |          35|          21|       14|
|   2014|   11|Oklahoma               |Baylor                      |          14|          48|      -34|
|   2014|   11|New Mexico             |Boise State                 |          49|          60|      -11|
|   2014|   11|Colorado State         |Hawaii                      |          49|          22|       27|
|   2014|   11|Syracuse               |Duke                        |          10|          27|      -17|
|   2014|   11|Vanderbilt             |Florida                     |          10|          34|      -24|
|   2014|   11|Florida State          |Virginia                    |          34|          20|       14|
|   2014|   11|Fresno State           |San Jose State              |          38|          24|       14|
|   2014|   11|Kentucky               |Georgia                     |          31|          63|      -32|
|   2014|   11|Texas State            |Georgia Southern            |          25|          28|       -3|
|   2014|   11|North Carolina State   |Georgia Tech                |          23|          56|      -33|
|   2014|   11|Kansas                 |Iowa State                  |          34|          14|       20|
|   2014|   11|Alabama-Birmingham     |Louisiana Tech              |          24|          40|      -16|
|   2014|   11|New Mexico State       |Louisiana-Lafayette         |          16|          44|      -28|
|   2014|   11|Boston College         |Louisville                  |          19|          38|      -19|
|   2014|   11|Southern Mississippi   |Marshall                    |          17|          63|      -46|
|   2014|   11|Northwestern           |Michigan                    |           9|          10|       -1|
|   2014|   11|Minnesota              |Iowa                        |          51|          14|       37|
|   2014|   11|Mississippi            |Presbyterian                |          48|           0|       48|
|   2014|   11|Mississippi State      |Tennessee-Martin            |          45|          16|       29|
|   2014|   11|North Texas            |Florida Atlantic            |          31|          10|       21|
|   2014|   11|Michigan State         |Ohio State                  |          37|          49|      -12|
|   2014|   11|Old Dominion           |Florida International       |          38|          35|        3|
|   2014|   11|Utah                   |Oregon                      |          27|          51|      -24|
|   2014|   11|Indiana                |Penn State                  |           7|          13|       -6|
|   2014|   11|Rice                   |Texas-San Antonio           |          17|           7|       10|
|   2014|   11|San Diego State        |Idaho                       |          35|          21|       14|
|   2014|   11|Texas                  |West Virginia               |          33|          16|       17|
|   2014|   11|Auburn                 |Texas A&M                   |          38|          41|       -3|
|   2014|   11|Texas Christian        |Kansas State                |          41|          20|       21|
|   2014|   11|Troy                   |Georgia State               |          45|          21|       24|
|   2014|   11|Houston                |Tulane                      |          24|          31|       -7|
|   2014|   11|Tulsa                  |Southern Methodist          |          38|          28|       10|
|   2014|   11|Washington             |UCLA                        |          30|          44|      -14|
|   2014|   11|Oregon State           |Washington State            |          32|          39|       -7|
|   2014|   11|Western Kentucky       |Texas-El Paso               |          35|          27|        8|
|   2014|   11|Purdue                 |Wisconsin                   |          16|          34|      -18|
|   2014|   12|Buffalo                |Akron                       |          55|          24|       31|
|   2014|   12|Northern Illinois      |Toledo                      |          27|          24|        3|
|   2014|   12|Bowling Green State    |Kent State                  |          30|          20|       10|
|   2014|   12|Massachusetts          |Ball State                  |          24|          10|       14|
|   2014|   12|Cincinnati             |East Carolina               |          54|          46|        8|
|   2014|   12|Southern California    |California                  |          38|          30|        8|
|   2014|   12|Texas-San Antonio      |Southern Mississippi        |          12|          10|        2|
|   2014|   12|Central Florida        |Tulsa                       |          31|           7|       24|
|   2014|   12|Air Force              |Nevada                      |          45|          38|        7|
|   2014|   12|Alabama                |Mississippi State           |          25|          20|        5|
|   2014|   12|Arkansas State         |Appalachian State           |          32|          37|       -5|
|   2014|   12|Arizona                |Washington                  |          27|          26|        1|
|   2014|   12|Arkansas               |Louisiana State             |          17|           0|       17|
|   2014|   12|Boise State            |San Diego State             |          38|          29|        9|
|   2014|   12|Brigham Young          |Nevada-Las Vegas            |          42|          23|       19|
|   2014|   12|Central Michigan       |Miami (OH)                  |          34|          27|        7|
|   2014|   12|Florida International  |Middle Tennessee State      |          38|          28|       10|
|   2014|   12|Miami (FL)             |Florida State               |          26|          30|       -4|
|   2014|   12|Georgia                |Auburn                      |          34|           7|       27|
|   2014|   12|Georgia Tech           |Clemson                     |          28|           6|       22|
|   2014|   12|San Jose State         |Hawaii                      |           0|          13|      -13|
|   2014|   12|Illinois               |Iowa                        |          14|          30|      -16|
|   2014|   12|Louisiana-Monroe       |Louisiana-Lafayette         |          27|          34|       -7|
|   2014|   12|Marshall               |Rice                        |          41|          14|       27|
|   2014|   12|Tulane                 |Memphis                     |           7|          38|      -31|
|   2014|   12|Maryland               |Michigan State              |          15|          37|      -22|
|   2014|   12|Texas A&M              |Missouri                    |          27|          34|       -7|
|   2014|   12|Navy                   |Georgia Southern            |          52|          19|       33|
|   2014|   12|North Carolina         |Pittsburgh                  |          40|          35|        5|
|   2014|   12|North Carolina State   |Wake Forest                 |          42|          13|       29|
|   2014|   12|Notre Dame             |Northwestern                |          40|          43|       -3|
|   2014|   12|Notre Dame             |Northwestern                |          40|          43|       -3|
|   2014|   12|Notre Dame             |Northwestern                |          40|          43|       -3|
|   2014|   12|Notre Dame             |Northwestern                |          40|          43|       -3|
|   2014|   12|Minnesota              |Ohio State                  |          24|          31|       -7|
|   2014|   12|Texas Tech             |Oklahoma                    |          30|          42|      -12|
|   2014|   12|Oregon State           |Arizona State               |          35|          27|        8|
|   2014|   12|Penn State             |Temple                      |          30|          13|       17|
|   2014|   12|Rutgers                |Indiana                     |          45|          23|       22|
|   2014|   12|South Alabama          |Texas State                 |          24|          20|        4|
|   2014|   12|Florida                |South Carolina              |          20|          23|       -3|
|   2014|   12|Southern Methodist     |South Florida               |          13|          14|       -1|
|   2014|   12|Tennessee              |Kentucky                    |          50|          16|       34|
|   2014|   12|Oklahoma State         |Texas                       |           7|          28|      -21|
|   2014|   12|Kansas                 |Texas Christian             |          30|          34|       -4|
|   2014|   12|Texas-El Paso          |North Texas                 |          35|          17|       18|
|   2014|   12|Idaho                  |Troy                        |          17|          34|      -17|
|   2014|   12|Stanford               |Utah                        |          17|          20|       -3|
|   2014|   12|Utah State             |New Mexico                  |          28|          21|        7|
|   2014|   12|Duke                   |Virginia Tech               |          16|          17|       -1|
|   2014|   12|Western Kentucky       |Army                        |          52|          24|       28|
|   2014|   12|Western Michigan       |Eastern Michigan            |          51|           7|       44|
|   2014|   12|Wisconsin              |Nebraska                    |          59|          24|       35|
|   2014|   13|Akron                  |Massachusetts               |          30|           6|       24|
|   2014|   13|Ohio                   |Northern Illinois           |          14|          21|       -7|
|   2014|   13|Toledo                 |Bowling Green State         |          27|          20|        7|
|   2014|   13|West Virginia          |Kansas State                |          20|          26|       -6|
|   2014|   13|Duke                   |North Carolina              |          20|          45|      -25|
|   2014|   13|Texas State            |Arkansas State              |          45|          27|       18|
|   2014|   13|Rice                   |Texas-El Paso               |          31|          13|       18|
|   2014|   13|San Diego State        |Air Force                   |          30|          14|       16|
|   2014|   13|Utah State             |San Jose State              |          41|           7|       34|
|   2014|   13|Alabama                |Western Carolina            |          48|          14|       34|
|   2014|   13|Louisiana-Lafayette    |Appalachian State           |          16|          35|      -19|
|   2014|   13|Utah                   |Arizona                     |          10|          42|      -32|
|   2014|   13|Arizona State          |Washington State            |          52|          31|       21|
|   2014|   13|Arkansas               |Mississippi                 |          30|           0|       30|
|   2014|   13|Army                   |Fordham                     |          42|          31|       11|
|   2014|   13|Auburn                 |Samford                     |          31|           7|       24|
|   2014|   13|Ball State             |Eastern Michigan            |          45|          30|       15|
|   2014|   13|Baylor                 |Oklahoma State              |          49|          28|       21|
|   2014|   13|Wyoming                |Boise State                 |          14|          63|      -49|
|   2014|   13|Brigham Young          |Savannah State              |          64|           0|       64|
|   2014|   13|Central Florida        |Southern Methodist          |          53|           7|       46|
|   2014|   13|Connecticut            |Cincinnati                  |           0|          41|      -41|
|   2014|   13|Clemson                |Georgia State               |          28|           0|       28|
|   2014|   13|Colorado State         |New Mexico                  |          58|          20|       38|
|   2014|   13|East Carolina          |Tulane                      |          34|           6|       28|
|   2014|   13|Florida                |Eastern Kentucky            |          52|           3|       49|
|   2014|   13|Florida State          |Boston College              |          20|          17|        3|
|   2014|   13|Nevada                 |Fresno State                |          20|          40|      -20|
|   2014|   13|Georgia                |Charleston Southern         |          55|           9|       46|
|   2014|   13|Hawaii                 |Nevada-Las Vegas            |          37|          35|        2|
|   2014|   13|Houston                |Tulsa                       |          38|          28|       10|
|   2014|   13|Illinois               |Penn State                  |          16|          14|        2|
|   2014|   13|New Mexico State       |Louisiana-Monroe            |          17|          30|      -13|
|   2014|   13|Notre Dame             |Louisville                  |          28|          31|       -3|
|   2014|   13|Alabama-Birmingham     |Marshall                    |          18|          23|       -5|
|   2014|   13|Michigan               |Maryland                    |          16|          23|       -7|
|   2014|   13|Memphis                |South Florida               |          31|          20|       11|
|   2014|   13|Michigan State         |Rutgers                     |          45|           3|       42|
|   2014|   13|Middle Tennessee State |Florida Atlantic            |          35|          34|        1|
|   2014|   13|Nebraska               |Minnesota                   |          24|          28|       -4|
|   2014|   13|Mississippi State      |Vanderbilt                  |          51|           0|       51|
|   2014|   13|Tennessee              |Missouri                    |          21|          29|       -8|
|   2014|   13|North Texas            |Florida International       |          17|          14|        3|
|   2014|   13|Purdue                 |Northwestern                |          14|          38|      -24|
|   2014|   13|Ohio State             |Indiana                     |          42|          27|       15|
|   2014|   13|Oklahoma               |Kansas                      |          44|           7|       37|
|   2014|   13|Old Dominion           |Louisiana Tech              |          30|          27|        3|
|   2014|   13|Oregon                 |Colorado                    |          44|          10|       34|
|   2014|   13|Pittsburgh             |Syracuse                    |          30|           7|       23|
|   2014|   13|South Carolina         |South Alabama               |          37|          12|       25|
|   2014|   13|California             |Stanford                    |          17|          38|      -21|
|   2014|   13|Iowa State             |Texas Tech                  |          31|          34|       -3|
|   2014|   13|UCLA                   |Southern California         |          38|          20|       18|
|   2014|   13|Virginia               |Miami (FL)                  |          30|          13|       17|
|   2014|   13|Wake Forest            |Virginia Tech               |           6|           3|        3|
|   2014|   13|Washington             |Oregon State                |          37|          13|       24|
|   2014|   13|Western Kentucky       |Texas-San Antonio           |          45|           7|       38|
|   2014|   13|Central Michigan       |Western Michigan            |          20|          32|      -12|
|   2014|   13|Iowa                   |Wisconsin                   |          24|          26|       -2|
|   2014|   14|Miami (OH)             |Ohio                        |          21|          24|       -3|
|   2014|   14|Texas A&M              |Louisiana State             |          17|          23|       -6|
|   2014|   14|Texas                  |Texas Christian             |          10|          48|      -38|
|   2014|   14|Air Force              |Colorado State              |          27|          24|        3|
|   2014|   14|Arizona                |Arizona State               |          42|          35|        7|
|   2014|   14|Bowling Green State    |Ball State                  |          24|          41|      -17|
|   2014|   14|Massachusetts          |Buffalo                     |          21|          41|      -20|
|   2014|   14|South Florida          |Central Florida             |           0|          16|      -16|
|   2014|   14|Tulsa                  |East Carolina               |          32|          49|      -17|
|   2014|   14|Southern Methodist     |Houston                     |           9|          35|      -26|
|   2014|   14|Kent State             |Akron                       |          27|          24|        3|
|   2014|   14|Missouri               |Arkansas                    |          21|          14|        7|
|   2014|   14|South Alabama          |Navy                        |          40|          42|       -2|
|   2014|   14|Iowa                   |Nebraska                    |          34|          37|       -3|
|   2014|   14|Western Michigan       |Northern Illinois           |          21|          31|      -10|
|   2014|   14|UCLA                   |Stanford                    |          10|          31|      -21|
|   2014|   14|Eastern Michigan       |Toledo                      |          16|          52|      -36|
|   2014|   14|Virginia Tech          |Virginia                    |          24|          20|        4|
|   2014|   14|Marshall               |Western Kentucky            |          66|          67|       -1|
|   2014|   14|Alabama                |Auburn                      |          55|          44|       11|
|   2014|   14|Southern Mississippi   |Alabama-Birmingham          |          24|          45|      -21|
|   2014|   14|Appalachian State      |Idaho                       |          45|          28|       17|
|   2014|   14|Arkansas State         |New Mexico State            |          68|          35|       33|
|   2014|   14|Baylor                 |Texas Tech                  |          48|          46|        2|
|   2014|   14|Boise State            |Utah State                  |          50|          19|       31|
|   2014|   14|Boston College         |Syracuse                    |          28|           7|       21|
|   2014|   14|California             |Brigham Young               |          35|          42|       -7|
|   2014|   14|Temple                 |Cincinnati                  |           6|          14|       -8|
|   2014|   14|Clemson                |South Carolina              |          35|          17|       18|
|   2014|   14|Duke                   |Wake Forest                 |          41|          21|       20|
|   2014|   14|Florida State          |Florida                     |          24|          19|        5|
|   2014|   14|Fresno State           |Hawaii                      |          28|          21|        7|
|   2014|   14|Georgia Southern       |Louisiana-Monroe            |          22|          16|        6|
|   2014|   14|Georgia                |Georgia Tech                |          24|          30|       -6|
|   2014|   14|Northwestern           |Illinois                    |          33|          47|      -14|
|   2014|   14|Indiana                |Purdue                      |          23|          16|        7|
|   2014|   14|Kansas State           |Kansas                      |          51|          13|       38|
|   2014|   14|Louisiana Tech         |Rice                        |          76|          31|       45|
|   2014|   14|Troy                   |Louisiana-Lafayette         |          23|          42|      -19|
|   2014|   14|Louisville             |Kentucky                    |          44|          40|        4|
|   2014|   14|Memphis                |Connecticut                 |          41|          10|       31|
|   2014|   14|Penn State             |Michigan State              |          10|          34|      -24|
|   2014|   14|Mississippi            |Mississippi State           |          31|          17|       14|
|   2014|   14|Nevada-Las Vegas       |Nevada                      |          27|          49|      -22|
|   2014|   14|New Mexico             |Wyoming                     |          36|          30|        6|
|   2014|   14|North Carolina         |North Carolina State        |           7|          35|      -28|
|   2014|   14|Ohio State             |Michigan                    |          42|          28|       14|
|   2014|   14|Florida Atlantic       |Old Dominion                |          28|          31|       -3|
|   2014|   14|Oregon State           |Oregon                      |          19|          47|      -28|
|   2014|   14|Miami (FL)             |Pittsburgh                  |          23|          35|      -12|
|   2014|   14|Maryland               |Rutgers                     |          38|          41|       -3|
|   2014|   14|San Diego State        |San Jose State              |          38|           7|       31|
|   2014|   14|Southern California    |Notre Dame                  |          49|          14|       35|
|   2014|   14|Vanderbilt             |Tennessee                   |          17|          24|       -7|
|   2014|   14|Georgia State          |Texas State                 |          31|          54|      -23|
|   2014|   14|Texas-El Paso          |Middle Tennessee State      |          24|          21|        3|
|   2014|   14|Texas-San Antonio      |North Texas                 |          34|          27|        7|
|   2014|   14|Colorado               |Utah                        |          34|          38|       -4|
|   2014|   14|Washington State       |Washington                  |          13|          31|      -18|
|   2014|   14|Iowa State             |West Virginia               |          24|          37|      -13|
|   2014|   14|Wisconsin              |Minnesota                   |          34|          24|       10|
|   2014|   15|East Carolina          |Central Florida             |          30|          32|       -2|
|   2014|   15|Northern Illinois      |Bowling Green State         |          51|          17|       34|
|   2014|   15|Oregon                 |Arizona                     |          51|          13|       38|
|   2014|   15|Alabama                |Missouri                    |          42|          13|       29|
|   2014|   15|Baylor                 |Kansas State                |          38|          27|       11|
|   2014|   15|Boise State            |Fresno State                |          28|          14|       14|
|   2014|   15|Cincinnati             |Houston                     |          38|          31|        7|
|   2014|   15|Florida State          |Georgia Tech                |          37|          35|        2|
|   2014|   15|Marshall               |Louisiana Tech              |          26|          23|        3|
|   2014|   15|Ohio State             |Wisconsin                   |          59|           0|       59|
|   2014|   15|Oklahoma               |Oklahoma State              |          35|          38|       -3|
|   2014|   15|Connecticut            |Southern Methodist          |          20|          27|       -7|
|   2014|   15|Tulane                 |Temple                      |           3|          10|       -7|
|   2014|   15|Texas Christian        |Iowa State                  |          55|           3|       52|
|   2014|   16|Navy                   |Army                        |          17|          10|        7|
|   2014|   17|Air Force              |Western Michigan            |          38|          24|       14|
|   2014|   17|Bowling Green State    |South Alabama               |          33|          28|        5|
|   2014|   17|Louisiana-Lafayette    |Nevada                      |          16|           3|       13|
|   2014|   17|Utah                   |Colorado State              |          45|          10|       35|
|   2014|   17|Utah State             |Texas-El Paso               |          21|           6|       15|
|   2014|   17|Memphis                |Brigham Young               |          55|          48|        7|
|   2014|   18|Marshall               |Northern Illinois           |          52|          23|       29|
|   2014|   18|Navy                   |San Diego State             |          17|          16|        1|
|   2014|   18|Rice                   |Fresno State                |          30|           6|       24|
|   2014|   18|Western Kentucky       |Central Michigan            |          49|          48|        1|
|   2014|   18|Louisiana Tech         |Illinois                    |          35|          18|       17|
|   2014|   18|North Carolina State   |Central Florida             |          34|          27|        7|
|   2014|   18|Rutgers                |North Carolina              |          40|          21|       19|
|   2014|   18|Arizona State          |Duke                        |          36|          31|        5|
|   2014|   18|Penn State             |Boston College              |          31|          30|        1|
|   2014|   18|South Carolina         |Miami (FL)                  |          24|          21|        3|
|   2014|   18|Southern California    |Nebraska                    |          45|          42|        3|
|   2014|   18|Virginia Tech          |Cincinnati                  |          33|          17|       16|
|   2014|   19|Arkansas               |Texas                       |          31|           7|       24|
|   2014|   19|Clemson                |Oklahoma                    |          40|           6|       34|
|   2014|   19|Texas A&M              |West Virginia               |          45|          37|        8|
|   2014|   20|Georgia                |Louisville                  |          37|          14|       23|
|   2014|   20|Notre Dame             |Louisiana State             |          31|          28|        3|
|   2014|   20|Stanford               |Maryland                    |          45|          21|       24|
|   2014|   20|Boise State            |Arizona                     |          38|          30|        8|
|   2014|   20|Georgia Tech           |Mississippi State           |          49|          34|       15|
|   2014|   20|Texas Christian        |Mississippi                 |          42|           3|       39|
|   2014|   20|Michigan State         |Baylor                      |          42|          41|        1|
|   2014|   20|Missouri               |Minnesota                   |          33|          17|       16|
|   2014|   20|Ohio State             |Alabama                     |          42|          35|        7|
|   2014|   20|Oregon                 |Florida State               |          59|          20|       39|
|   2014|   20|Wisconsin              |Auburn                      |          34|          31|        3|
|   2014|   20|Houston                |Pittsburgh                  |          35|          34|        1|
|   2014|   20|Oklahoma State         |Washington                  |          30|          22|        8|
|   2014|   20|Tennessee              |Iowa                        |          45|          28|       17|
|   2014|   20|UCLA                   |Kansas State                |          40|          35|        5|
|   2014|   20|Florida                |East Carolina               |          28|          20|        8|
|   2014|   20|Toledo                 |Arkansas State              |          63|          44|       19|
|   2014|   21|Ohio State             |Oregon                      |          42|          20|       22|
|   2015|    1|Arizona                |Texas-San Antonio           |          42|          32|       10|
|   2015|    1|Ball State             |Virginia Military Institute |          48|          36|       12|
|   2015|    1|Connecticut            |Villanova                   |          20|          15|        5|
|   2015|    1|Tulane                 |Duke                        |           7|          37|      -30|
|   2015|    1|Central Florida        |Florida International       |          14|          15|       -1|
|   2015|    1|Fresno State           |Abilene Christian           |          34|          13|       21|
|   2015|    1|Georgia Tech           |Alcorn State                |          69|           6|       63|
|   2015|    1|Hawaii                 |Colorado                    |          28|          20|        8|
|   2015|    1|Nevada                 |California-Davis            |          31|          17|       14|
|   2015|    1|Idaho                  |Ohio                        |          28|          45|      -17|
|   2015|    1|Central Michigan       |Oklahoma State              |          13|          24|      -11|
|   2015|    1|San Jose State         |New Hampshire               |          43|          13|       30|
|   2015|    1|South Carolina         |North Carolina              |          17|          13|        4|
|   2015|    1|Minnesota              |Texas Christian             |          17|          23|       -6|
|   2015|    1|Utah                   |Michigan                    |          24|          17|        7|
|   2015|    1|Utah State             |Southern Utah               |          12|           9|        3|
|   2015|    1|Wake Forest            |Elon                        |          41|           3|       38|
|   2015|    1|Vanderbilt             |Western Kentucky            |          12|          14|       -2|
|   2015|    1|Southern Methodist     |Baylor                      |          21|          56|      -35|
|   2015|    1|Boise State            |Washington                  |          16|          13|        3|
|   2015|    1|Georgia State          |Charlotte                   |          20|          23|       -3|
|   2015|    1|Army                   |Fordham                     |          35|          37|       -2|
|   2015|    1|Western Michigan       |Michigan State              |          24|          37|      -13|
|   2015|    1|Oregon State           |Weber State                 |          26|           7|       19|
|   2015|    1|Syracuse               |Rhode Island                |          47|           0|       47|
|   2015|    1|Air Force              |Morgan State                |          63|           7|       56|
|   2015|    1|Alabama                |Wisconsin                   |          35|          17|       18|
|   2015|    1|Appalachian State      |Howard                      |          49|           0|       49|
|   2015|    1|Arkansas               |Texas-El Paso               |          48|          13|       35|
|   2015|    1|Auburn                 |Louisville                  |          31|          24|        7|
|   2015|    1|Boston College         |Maine                       |          24|           3|       21|
|   2015|    1|Nebraska               |Brigham Young               |          28|          33|       -5|
|   2015|    1|Buffalo                |Albany                      |          51|          14|       37|
|   2015|    1|California             |Grambling State             |          73|          14|       59|
|   2015|    1|Cincinnati             |Alabama A&M                 |          52|          10|       42|
|   2015|    1|Clemson                |Wofford                     |          49|          10|       39|
|   2015|    1|Colorado State         |Savannah State              |          65|          13|       52|
|   2015|    1|East Carolina          |Towson                      |          28|          20|        8|
|   2015|    1|Florida                |New Mexico State            |          61|          13|       48|
|   2015|    1|Florida State          |Texas State                 |          59|          16|       43|
|   2015|    1|Georgia                |Louisiana-Monroe            |          51|          14|       37|
|   2015|    1|Houston                |Tennessee Tech              |          52|          24|       28|
|   2015|    1|Illinois               |Kent State                  |          52|           3|       49|
|   2015|    1|Indiana                |Southern Illinois           |          48|          47|        1|
|   2015|    1|Iowa                   |Illinois State              |          31|          14|       17|
|   2015|    1|Iowa State             |Northern Iowa               |          31|           7|       24|
|   2015|    1|Kansas State           |South Dakota                |          34|           0|       34|
|   2015|    1|Kentucky               |Louisiana-Lafayette         |          40|          33|        7|
|   2015|    1|Louisiana Tech         |Southern                    |          62|          15|       47|
|   2015|    1|Maryland               |Richmond                    |          50|          21|       29|
|   2015|    1|Memphis                |Missouri State              |          63|           7|       56|
|   2015|    1|Miami (FL)             |Bethune-Cookman             |          45|           0|       45|
|   2015|    1|Miami (OH)             |Presbyterian                |          26|           7|       19|
|   2015|    1|Middle Tennessee State |Jackson State               |          70|          14|       56|
|   2015|    1|Mississippi            |Tennessee-Martin            |          76|           3|       73|
|   2015|    1|Southern Mississippi   |Mississippi State           |          16|          34|      -18|
|   2015|    1|Missouri               |Southeast Missouri State    |          34|           3|       31|
|   2015|    1|Navy                   |Colgate                     |          48|          10|       38|
|   2015|    1|New Mexico             |Mississippi Valley State    |          66|           0|       66|
|   2015|    1|North Carolina State   |Troy                        |          49|          21|       28|
|   2015|    1|Wyoming                |North Dakota                |          13|          24|      -11|
|   2015|    1|Northern Illinois      |Nevada-Las Vegas            |          38|          30|        8|
|   2015|    1|Northwestern           |Stanford                    |          16|           6|       10|
|   2015|    1|Notre Dame             |Texas                       |          38|           3|       35|
|   2015|    1|Oklahoma               |Akron                       |          41|           3|       38|
|   2015|    1|Eastern Michigan       |Old Dominion                |          34|          38|       -4|
|   2015|    1|Oregon                 |Eastern Washington          |          61|          42|       19|
|   2015|    1|Pittsburgh             |Youngstown State            |          45|          37|        8|
|   2015|    1|Washington State       |Portland State              |          17|          24|       -7|
|   2015|    1|Rice                   |Wagner                      |          56|          16|       40|
|   2015|    1|Rutgers                |Norfolk State               |          63|          13|       50|
|   2015|    1|San Diego State        |San Diego                   |          37|           3|       34|
|   2015|    1|South Alabama          |Gardner-Webb                |          33|          23|       10|
|   2015|    1|Kansas                 |South Dakota State          |          38|          41|       -3|
|   2015|    1|South Florida          |Florida A&M                 |          51|           3|       48|
|   2015|    1|Southern California    |Arkansas State              |          55|           6|       49|
|   2015|    1|Temple                 |Penn State                  |          27|          10|       17|
|   2015|    1|Tennessee              |Bowling Green State         |          59|          30|       29|
|   2015|    1|Texas A&M              |Arizona State               |          38|          17|       21|
|   2015|    1|Texas Tech             |Sam Houston State           |          59|          45|       14|
|   2015|    1|Tulsa                  |Florida Atlantic            |          47|          44|        3|
|   2015|    1|UCLA                   |Virginia                    |          34|          16|       18|
|   2015|    1|West Virginia          |Georgia Southern            |          44|           0|       44|
|   2015|    1|Marshall               |Purdue                      |          41|          31|       10|
|   2015|    1|Virginia Tech          |Ohio State                  |          24|          42|      -18|
|   2015|    2|Western Kentucky       |Louisiana Tech              |          41|          38|        3|
|   2015|    2|Florida Atlantic       |Miami (FL)                  |          20|          44|      -24|
|   2015|    2|Utah                   |Utah State                  |          24|          14|       10|
|   2015|    2|Air Force              |San Jose State              |          37|          16|       21|
|   2015|    2|Alabama                |Middle Tennessee State      |          37|          10|       27|
|   2015|    2|Nevada                 |Arizona                     |          20|          44|      -24|
|   2015|    2|Arizona State          |Cal Poly                    |          35|          21|       14|
|   2015|    2|Auburn                 |Jacksonville State          |          27|          20|        7|
|   2015|    2|Baylor                 |Lamar                       |          66|          31|       35|
|   2015|    2|Boston College         |Howard                      |          76|           0|       76|
|   2015|    2|Maryland               |Bowling Green State         |          27|          48|      -21|
|   2015|    2|Brigham Young          |Boise State                 |          35|          24|       11|
|   2015|    2|California             |San Diego State             |          35|           7|       28|
|   2015|    2|Central Michigan       |Monmouth                    |          31|          10|       21|
|   2015|    2|Charlotte              |Presbyterian                |          34|          10|       24|
|   2015|    2|Clemson                |Appalachian State           |          41|          10|       31|
|   2015|    2|Colorado               |Massachusetts               |          48|          14|       34|
|   2015|    2|Connecticut            |Army                        |          22|          17|        5|
|   2015|    2|Duke                   |North Carolina Central      |          55|           0|       55|
|   2015|    2|Wyoming                |Eastern Michigan            |          29|          48|      -19|
|   2015|    2|Florida                |East Carolina               |          31|          24|        7|
|   2015|    2|Florida State          |South Florida               |          34|          14|       20|
|   2015|    2|Vanderbilt             |Georgia                     |          14|          31|      -17|
|   2015|    2|Georgia Southern       |Western Michigan            |          43|          17|       26|
|   2015|    2|New Mexico State       |Georgia State               |          32|          34|       -2|
|   2015|    2|Georgia Tech           |Tulane                      |          65|          10|       55|
|   2015|    2|Louisville             |Houston                     |          31|          34|       -3|
|   2015|    2|Illinois               |Western Illinois            |          44|           0|       44|
|   2015|    2|Indiana                |Florida International       |          36|          22|       14|
|   2015|    2|Iowa State             |Iowa                        |          17|          31|      -14|
|   2015|    2|Texas-San Antonio      |Kansas State                |           3|          30|      -27|
|   2015|    2|Kent State             |Delaware State              |          45|          13|       32|
|   2015|    2|South Carolina         |Kentucky                    |          22|          26|       -4|
|   2015|    2|Mississippi State      |Louisiana State             |          19|          21|       -2|
|   2015|    2|Louisiana-Lafayette    |Northwestern State          |          44|          17|       27|
|   2015|    2|Louisiana-Monroe       |Nicholls State              |          47|           0|       47|
|   2015|    2|Kansas                 |Memphis                     |          23|          55|      -32|
|   2015|    2|Michigan               |Oregon State                |          35|           7|       28|
|   2015|    2|Michigan State         |Oregon                      |          31|          28|        3|
|   2015|    2|Colorado State         |Minnesota                   |          20|          23|       -3|
|   2015|    2|Mississippi            |Fresno State                |          73|          21|       52|
|   2015|    2|Arkansas State         |Missouri                    |          20|          27|       -7|
|   2015|    2|Nebraska               |South Alabama               |          48|           9|       39|
|   2015|    2|North Carolina         |North Carolina A&T          |          53|          14|       39|
|   2015|    2|North Carolina State   |Eastern Kentucky            |          35|           0|       35|
|   2015|    2|Northern Illinois      |Murray State                |          57|          26|       31|
|   2015|    2|Northwestern           |Eastern Illinois            |          41|           0|       41|
|   2015|    2|Virginia               |Notre Dame                  |          27|          34|       -7|
|   2015|    2|Ohio                   |Marshall                    |          21|          10|       11|
|   2015|    2|Ohio State             |Hawaii                      |          38|           0|       38|
|   2015|    2|Tennessee              |Oklahoma                    |          24|          31|       -7|
|   2015|    2|Oklahoma State         |Central Arkansas            |          32|           8|       24|
|   2015|    2|Old Dominion           |Norfolk State               |          24|          10|       14|
|   2015|    2|Penn State             |Buffalo                     |          27|          14|       13|
|   2015|    2|Akron                  |Pittsburgh                  |           7|          24|      -17|
|   2015|    2|Purdue                 |Indiana State               |          38|          14|       24|
|   2015|    2|Southern California    |Idaho                       |          59|           9|       50|
|   2015|    2|Southern Methodist     |North Texas                 |          31|          13|       18|
|   2015|    2|Southern Mississippi   |Austin Peay                 |          52|           6|       46|
|   2015|    2|Stanford               |Central Florida             |          31|           7|       24|
|   2015|    2|Syracuse               |Wake Forest                 |          30|          17|       13|
|   2015|    2|Cincinnati             |Temple                      |          26|          34|       -8|
|   2015|    2|Texas                  |Rice                        |          42|          28|       14|
|   2015|    2|Texas A&M              |Ball State                  |          56|          23|       33|
|   2015|    2|Texas Christian        |Stephen F. Austin           |          70|           7|       63|
|   2015|    2|Texas State            |Prairie View A&M            |          63|          24|       39|
|   2015|    2|Texas Tech             |Texas-El Paso               |          69|          20|       49|
|   2015|    2|Toledo                 |Arkansas                    |          16|          12|        4|
|   2015|    2|Troy                   |Charleston Southern         |          44|          16|       28|
|   2015|    2|New Mexico             |Tulsa                       |          21|          40|      -19|
|   2015|    2|Nevada-Las Vegas       |UCLA                        |           3|          37|      -34|
|   2015|    2|Virginia Tech          |Furman                      |          42|           3|       39|
|   2015|    2|Washington             |Sacramento State            |          49|           0|       49|
|   2015|    2|Rutgers                |Washington State            |          34|          37|       -3|
|   2015|    2|West Virginia          |Liberty                     |          41|          17|       24|
|   2015|    2|Wisconsin              |Miami (OH)                  |          58|           0|       58|
|   2015|    3|Louisville             |Clemson                     |          17|          20|       -3|
|   2015|    3|Arizona State          |New Mexico                  |          34|          10|       24|
|   2015|    3|Boise State            |Idaho State                 |          52|           0|       52|
|   2015|    3|Boston College         |Florida State               |           0|          14|      -14|
|   2015|    3|Akron                  |Savannah State              |          52|           9|       43|
|   2015|    3|Arizona                |Northern Arizona            |          77|          13|       64|
|   2015|    3|Arkansas State         |Missouri State              |          70|           7|       63|
|   2015|    3|Eastern Michigan       |Ball State                  |          17|          28|      -11|
|   2015|    3|Florida Atlantic       |Buffalo                     |          15|          33|      -18|
|   2015|    3|Texas                  |California                  |          44|          45|       -1|
|   2015|    3|Miami (OH)             |Cincinnati                  |          33|          37|       -4|
|   2015|    3|Colorado               |Colorado State              |          27|          24|        3|
|   2015|    3|Kentucky               |Florida                     |           9|          14|       -5|
|   2015|    3|Florida International  |North Carolina Central      |          39|          14|       25|
|   2015|    3|Central Florida        |Furman                      |          15|          16|       -1|
|   2015|    3|Georgia                |South Carolina              |          52|          20|       32|
|   2015|    3|Georgia Southern       |Citadel                     |          48|          13|       35|
|   2015|    3|Hawaii                 |California-Davis            |          47|          27|       20|
|   2015|    3|Idaho                  |Wofford                     |          41|          38|        3|
|   2015|    3|Indiana                |Western Kentucky            |          38|          35|        3|
|   2015|    3|Iowa                   |Pittsburgh                  |          27|          24|        3|
|   2015|    3|Kansas State           |Louisiana Tech              |          39|          33|        6|
|   2015|    3|Louisiana State        |Auburn                      |          45|          21|       24|
|   2015|    3|Marshall               |Norfolk State               |          45|           7|       38|
|   2015|    3|Maryland               |South Florida               |          35|          17|       18|
|   2015|    3|Bowling Green State    |Memphis                     |          41|          44|       -3|
|   2015|    3|Miami (FL)             |Nebraska                    |          36|          33|        3|
|   2015|    3|Michigan               |Nevada-Las Vegas            |          28|           7|       21|
|   2015|    3|Michigan State         |Air Force                   |          35|          21|       14|
|   2015|    3|Middle Tennessee State |Charlotte                   |          73|          14|       59|
|   2015|    3|Minnesota              |Kent State                  |          10|           7|        3|
|   2015|    3|Alabama                |Mississippi                 |          37|          43|       -6|
|   2015|    3|Mississippi State      |Northwestern State          |          62|          13|       49|
|   2015|    3|Missouri               |Connecticut                 |           9|           6|        3|
|   2015|    3|Navy                   |East Carolina               |          45|          21|       24|
|   2015|    3|North Carolina         |Illinois                    |          48|          14|       34|
|   2015|    3|Old Dominion           |North Carolina State        |          14|          38|      -24|
|   2015|    3|Duke                   |Northwestern                |          10|          19|       -9|
|   2015|    3|Notre Dame             |Georgia Tech                |          30|          22|        8|
|   2015|    3|Ohio                   |Southeastern Louisiana      |          35|          14|       21|
|   2015|    3|Ohio State             |Northern Illinois           |          20|          13|        7|
|   2015|    3|Oklahoma               |Tulsa                       |          52|          38|       14|
|   2015|    3|Oklahoma State         |Texas-San Antonio           |          69|          14|       55|
|   2015|    3|Oregon                 |Georgia State               |          61|          28|       33|
|   2015|    3|Oregon State           |San Jose State              |          35|          21|       14|
|   2015|    3|Penn State             |Rutgers                     |          28|           3|       25|
|   2015|    3|North Texas            |Rice                        |          24|          38|      -14|
|   2015|    3|San Diego State        |South Alabama               |          27|          34|       -7|
|   2015|    3|Texas State            |Southern Mississippi        |          50|          56|       -6|
|   2015|    3|Southern California    |Stanford                    |          31|          41|      -10|
|   2015|    3|Syracuse               |Central Michigan            |          30|          27|        3|
|   2015|    3|Massachusetts          |Temple                      |          23|          25|       -2|
|   2015|    3|Tennessee              |Western Carolina            |          55|          10|       45|
|   2015|    3|Texas A&M              |Nevada                      |          44|          27|       17|
|   2015|    3|Texas Christian        |Southern Methodist          |          56|          37|       19|
|   2015|    3|Arkansas               |Texas Tech                  |          24|          35|      -11|
|   2015|    3|New Mexico State       |Texas-El Paso               |          47|          50|       -3|
|   2015|    3|Toledo                 |Iowa State                  |          30|          23|        7|
|   2015|    3|Tulane                 |Maine                       |          38|           7|       31|
|   2015|    3|UCLA                   |Brigham Young               |          24|          23|        1|
|   2015|    3|Fresno State           |Utah                        |          24|          45|      -21|
|   2015|    3|Vanderbilt             |Austin Peay                 |          47|           7|       40|
|   2015|    3|Virginia               |William & Mary              |          35|          29|        6|
|   2015|    3|Purdue                 |Virginia Tech               |          24|          51|      -27|
|   2015|    3|Army                   |Wake Forest                 |          14|          17|       -3|
|   2015|    3|Washington             |Utah State                  |          31|          17|       14|
|   2015|    3|Washington State       |Wyoming                     |          31|          14|       17|
|   2015|    3|Western Michigan       |Murray State                |          52|          20|       32|
|   2015|    3|Wisconsin              |Troy                        |          28|           3|       25|
|   2015|    4|Memphis                |Cincinnati                  |          53|          46|        7|
|   2015|    4|Virginia               |Boise State                 |          14|          56|      -42|
|   2015|    4|Oregon State           |Stanford                    |          24|          42|      -18|
|   2015|    4|Louisiana-Lafayette    |Akron                       |          14|          35|      -21|
|   2015|    4|Alabama                |Louisiana-Monroe            |          34|           0|       34|
|   2015|    4|Old Dominion           |Appalachian State           |           0|          49|      -49|
|   2015|    4|Eastern Michigan       |Army                        |          36|          58|      -22|
|   2015|    4|Baylor                 |Rice                        |          70|          17|       53|
|   2015|    4|Boston College         |Northern Illinois           |          17|          14|        3|
|   2015|    4|Purdue                 |Bowling Green State         |          28|          35|       -7|
|   2015|    4|Washington             |California                  |          24|          30|       -6|
|   2015|    4|Colorado               |Nicholls State              |          48|           0|       48|
|   2015|    4|Texas-San Antonio      |Colorado State              |          31|          33|       -2|
|   2015|    4|Duke                   |Georgia Tech                |          34|          20|       14|
|   2015|    4|East Carolina          |Virginia Tech               |          35|          28|        7|
|   2015|    4|Florida                |Tennessee                   |          28|          27|        1|
|   2015|    4|Charlotte              |Florida Atlantic            |           7|          17|      -10|
|   2015|    4|Georgia                |Southern                    |          48|           6|       42|
|   2015|    4|Idaho                  |Georgia Southern            |          20|          44|      -24|
|   2015|    4|Houston                |Texas State                 |          59|          14|       45|
|   2015|    4|Illinois               |Middle Tennessee State      |          27|          25|        2|
|   2015|    4|Wake Forest            |Indiana                     |          24|          31|       -7|
|   2015|    4|Iowa                   |North Texas                 |          62|          16|       46|
|   2015|    4|Southern Methodist     |James Madison               |          45|          48|       -3|
|   2015|    4|Kentucky               |Missouri                    |          21|          13|        8|
|   2015|    4|Syracuse               |Louisiana State             |          24|          34|      -10|
|   2015|    4|Louisiana Tech         |Florida International       |          27|          17|       10|
|   2015|    4|Louisville             |Samford                     |          45|           3|       42|
|   2015|    4|Kent State             |Marshall                    |          29|          36|       -7|
|   2015|    4|Michigan               |Brigham Young               |          31|           0|       31|
|   2015|    4|Michigan State         |Central Michigan            |          30|          10|       20|
|   2015|    4|Minnesota              |Ohio                        |          27|          24|        3|
|   2015|    4|Mississippi            |Vanderbilt                  |          27|          16|       11|
|   2015|    4|Auburn                 |Mississippi State           |           9|          17|       -8|
|   2015|    4|Connecticut            |Navy                        |          18|          28|      -10|
|   2015|    4|Nebraska               |Southern Mississippi        |          36|          28|        8|
|   2015|    4|Buffalo                |Nevada                      |          21|          24|       -3|
|   2015|    4|Nevada-Las Vegas       |Idaho State                 |          80|           8|       72|
|   2015|    4|Wyoming                |New Mexico                  |          28|          38|      -10|
|   2015|    4|North Carolina         |Delaware                    |          41|          14|       27|
|   2015|    4|South Alabama          |North Carolina State        |          13|          63|      -50|
|   2015|    4|Northwestern           |Ball State                  |          24|          19|        5|
|   2015|    4|Notre Dame             |Massachusetts               |          62|          27|       35|
|   2015|    4|Ohio State             |Western Michigan            |          38|          12|       26|
|   2015|    4|Texas                  |Oklahoma State              |          27|          30|       -3|
|   2015|    4|Penn State             |San Diego State             |          37|          21|       16|
|   2015|    4|Rutgers                |Kansas                      |          27|          14|       13|
|   2015|    4|San Jose State         |Fresno State                |          49|          23|       26|
|   2015|    4|South Carolina         |Central Florida             |          31|          14|       17|
|   2015|    4|Arizona State          |Southern California         |          14|          42|      -28|
|   2015|    4|Texas A&M              |Arkansas                    |          28|          21|        7|
|   2015|    4|Texas Tech             |Texas Christian             |          52|          55|       -3|
|   2015|    4|Texas-El Paso          |Incarnate Word              |          27|          17|       10|
|   2015|    4|Toledo                 |Arkansas State              |          37|           7|       30|
|   2015|    4|Arizona                |UCLA                        |          30|          56|      -26|
|   2015|    4|Oregon                 |Utah                        |          20|          62|      -42|
|   2015|    4|West Virginia          |Maryland                    |          45|           6|       39|
|   2015|    4|Western Kentucky       |Miami (OH)                  |          56|          14|       42|
|   2015|    4|Wisconsin              |Hawaii                      |          28|           0|       28|
|   2015|    5|Cincinnati             |Miami (FL)                  |          34|          23|       11|
|   2015|    5|Brigham Young          |Connecticut                 |          30|          13|       17|
|   2015|    5|South Florida          |Memphis                     |          17|          24|       -7|
|   2015|    5|Charlotte              |Temple                      |           3|          37|      -34|
|   2015|    5|Georgia                |Alabama                     |          10|          38|      -28|
|   2015|    5|Appalachian State      |Wyoming                     |          31|          13|       18|
|   2015|    5|UCLA                   |Arizona State               |          23|          38|      -15|
|   2015|    5|Tennessee              |Arkansas                    |          20|          24|       -4|
|   2015|    5|Arkansas State         |Idaho                       |          49|          35|       14|
|   2015|    5|Auburn                 |San Jose State              |          35|          21|       14|
|   2015|    5|Baylor                 |Texas Tech                  |          63|          35|       28|
|   2015|    5|Boise State            |Hawaii                      |          55|           0|       55|
|   2015|    5|Buffalo                |Bowling Green State         |          22|          28|       -6|
|   2015|    5|California             |Washington State            |          34|          28|        6|
|   2015|    5|Central Michigan       |Northern Illinois           |          29|          19|       10|
|   2015|    5|Clemson                |Notre Dame                  |          24|          22|        2|
|   2015|    5|Duke                   |Boston College              |           9|           7|        2|
|   2015|    5|Southern Methodist     |East Carolina               |          23|          49|      -26|
|   2015|    5|Florida                |Mississippi                 |          38|          10|       28|
|   2015|    5|Wake Forest            |Florida State               |          16|          24|       -8|
|   2015|    5|Louisiana-Monroe       |Georgia Southern            |          31|          51|      -20|
|   2015|    5|Tulsa                  |Houston                     |          24|          38|      -14|
|   2015|    5|Illinois               |Nebraska                    |          14|          13|        1|
|   2015|    5|Wisconsin              |Iowa                        |           6|          10|       -4|
|   2015|    5|Iowa State             |Kansas                      |          38|          13|       25|
|   2015|    5|Kent State             |Miami (OH)                  |          20|          14|        6|
|   2015|    5|Kentucky               |Eastern Kentucky            |          34|          27|        7|
|   2015|    5|Georgia State          |Liberty                     |          33|          41|       -8|
|   2015|    5|Louisiana State        |Eastern Michigan            |          44|          22|       22|
|   2015|    5|Louisiana Tech         |Louisiana-Lafayette         |          43|          14|       29|
|   2015|    5|North Carolina State   |Louisville                  |          13|          20|       -7|
|   2015|    5|Marshall               |Old Dominion                |          27|           7|       20|
|   2015|    5|Massachusetts          |Florida International       |          24|          14|       10|
|   2015|    5|Maryland               |Michigan                    |           0|          28|      -28|
|   2015|    5|Michigan State         |Purdue                      |          24|          21|        3|
|   2015|    5|Missouri               |South Carolina              |          24|          10|       14|
|   2015|    5|Navy                   |Air Force                   |          33|          11|       22|
|   2015|    5|Nevada                 |Nevada-Las Vegas            |          17|          23|       -6|
|   2015|    5|New Mexico             |New Mexico State            |          38|          29|        9|
|   2015|    5|Georgia Tech           |North Carolina              |          31|          38|       -7|
|   2015|    5|Northwestern           |Minnesota                   |          27|           0|       27|
|   2015|    5|Akron                  |Ohio                        |          12|          14|       -2|
|   2015|    5|Indiana                |Ohio State                  |          27|          34|       -7|
|   2015|    5|Oklahoma               |West Virginia               |          44|          24|       20|
|   2015|    5|Oklahoma State         |Kansas State                |          36|          34|        2|
|   2015|    5|Colorado               |Oregon                      |          24|          41|      -17|
|   2015|    5|Penn State             |Army                        |          20|          14|        6|
|   2015|    5|Virginia Tech          |Pittsburgh                  |          13|          17|       -4|
|   2015|    5|San Diego State        |Fresno State                |          21|           7|       14|
|   2015|    5|Troy                   |South Alabama               |          18|          24|       -6|
|   2015|    5|Southern Mississippi   |North Texas                 |          49|          14|       35|
|   2015|    5|Stanford               |Arizona                     |          55|          17|       38|
|   2015|    5|Texas A&M              |Mississippi State           |          30|          17|       13|
|   2015|    5|Texas Christian        |Texas                       |          50|           7|       43|
|   2015|    5|Texas-El Paso          |Texas-San Antonio           |           6|          25|      -19|
|   2015|    5|Ball State             |Toledo                      |          10|          24|      -14|
|   2015|    5|Tulane                 |Central Florida             |          45|          31|       14|
|   2015|    5|Utah State             |Colorado State              |          33|          18|       15|
|   2015|    5|Middle Tennessee State |Vanderbilt                  |          13|          17|       -4|
|   2015|    5|Rice                   |Western Kentucky            |          10|          49|      -39|
|   2015|    6|Houston                |Southern Methodist          |          49|          28|       21|
|   2015|    6|Southern California    |Washington                  |          12|          17|       -5|
|   2015|    6|Marshall               |Southern Mississippi        |          31|          10|       21|
|   2015|    6|Virginia Tech          |North Carolina State        |          28|          13|       15|
|   2015|    6|Air Force              |Wyoming                     |          31|          17|       14|
|   2015|    6|Eastern Michigan       |Akron                       |          21|          47|      -26|
|   2015|    6|Alabama                |Arkansas                    |          27|          14|       13|
|   2015|    6|Georgia State          |Appalachian State           |           3|          37|      -34|
|   2015|    6|Arizona                |Oregon State                |          44|           7|       37|
|   2015|    6|Arizona State          |Colorado                    |          48|          23|       25|
|   2015|    6|Kansas                 |Baylor                      |           7|          66|      -59|
|   2015|    6|Colorado State         |Boise State                 |          10|          41|      -31|
|   2015|    6|Bowling Green State    |Massachusetts               |          62|          38|       24|
|   2015|    6|Brigham Young          |East Carolina               |          45|          38|        7|
|   2015|    6|Clemson                |Georgia Tech                |          43|          24|       19|
|   2015|    6|Central Florida        |Connecticut                 |          13|          40|      -27|
|   2015|    6|Army                   |Duke                        |           3|          44|      -41|
|   2015|    6|Missouri               |Florida                     |           3|          21|      -18|
|   2015|    6|Florida International  |Texas-El Paso               |          52|          12|       40|
|   2015|    6|Florida State          |Miami (FL)                  |          29|          24|        5|
|   2015|    6|Iowa                   |Illinois                    |          29|          20|        9|
|   2015|    6|South Carolina         |Louisiana State             |          24|          45|      -21|
|   2015|    6|Texas-San Antonio      |Louisiana Tech              |          31|          34|       -3|
|   2015|    6|Louisiana-Lafayette    |Texas State                 |          49|          27|       22|
|   2015|    6|Michigan               |Northwestern                |          38|           0|       38|
|   2015|    6|Rutgers                |Michigan State              |          24|          31|       -7|
|   2015|    6|Purdue                 |Minnesota                   |          13|          41|      -28|
|   2015|    6|Mississippi            |New Mexico State            |          52|           3|       49|
|   2015|    6|Mississippi State      |Troy                        |          45|          17|       28|
|   2015|    6|Nevada                 |New Mexico                  |          35|          17|       18|
|   2015|    6|Northern Illinois      |Ball State                  |          59|          41|       18|
|   2015|    6|Notre Dame             |Navy                        |          41|          24|       17|
|   2015|    6|Ohio                   |Miami (OH)                  |          34|           3|       31|
|   2015|    6|Ohio State             |Maryland                    |          49|          28|       21|
|   2015|    6|West Virginia          |Oklahoma State              |          26|          33|       -7|
|   2015|    6|Penn State             |Indiana                     |          29|           7|       22|
|   2015|    6|Pittsburgh             |Virginia                    |          26|          19|        7|
|   2015|    6|North Texas            |Portland State              |           7|          66|      -59|
|   2015|    6|Florida Atlantic       |Rice                        |          26|          27|       -1|
|   2015|    6|Hawaii                 |San Diego State             |          14|          28|      -14|
|   2015|    6|Nevada-Las Vegas       |San Jose State              |          27|          33|       -6|
|   2015|    6|South Florida          |Syracuse                    |          45|          24|       21|
|   2015|    6|Temple                 |Tulane                      |          49|          10|       39|
|   2015|    6|Tennessee              |Georgia                     |          38|          31|        7|
|   2015|    6|Texas                  |Oklahoma                    |          24|          17|        7|
|   2015|    6|Kansas State           |Texas Christian             |          45|          52|       -7|
|   2015|    6|Texas Tech             |Iowa State                  |          66|          31|       35|
|   2015|    6|Toledo                 |Kent State                  |          38|           7|       31|
|   2015|    6|Tulsa                  |Louisiana-Monroe            |          34|          24|       10|
|   2015|    6|Utah                   |California                  |          30|          24|        6|
|   2015|    6|Fresno State           |Utah State                  |          14|          56|      -42|
|   2015|    6|Boston College         |Wake Forest                 |           0|           3|       -3|
|   2015|    6|Oregon                 |Washington State            |          38|          45|       -7|
|   2015|    6|Western Kentucky       |Middle Tennessee State      |          58|          28|       30|
|   2015|    6|Western Michigan       |Central Michigan            |          41|          39|        2|
|   2015|    6|Nebraska               |Wisconsin                   |          21|          23|       -2|
|   2015|    7|South Alabama          |Arkansas State              |          31|          49|      -18|
|   2015|    7|Kentucky               |Auburn                      |          27|          30|       -3|
|   2015|    7|Stanford               |UCLA                        |          56|          35|       21|
|   2015|    7|North Texas            |Western Kentucky            |          28|          55|      -27|
|   2015|    7|Brigham Young          |Cincinnati                  |          38|          24|       14|
|   2015|    7|Fresno State           |Nevada-Las Vegas            |          31|          28|        3|
|   2015|    7|Tulane                 |Houston                     |           7|          42|      -35|
|   2015|    7|Utah State             |Boise State                 |          52|          26|       26|
|   2015|    7|Texas A&M              |Alabama                     |          23|          41|      -18|
|   2015|    7|Louisiana-Monroe       |Appalachian State           |          14|          59|      -45|
|   2015|    7|Colorado               |Arizona                     |          31|          38|       -7|
|   2015|    7|Army                   |Bucknell                    |          21|          14|        7|
|   2015|    7|Baylor                 |West Virginia               |          62|          38|       24|
|   2015|    7|Bowling Green State    |Akron                       |          59|          10|       49|
|   2015|    7|Central Michigan       |Buffalo                     |          51|          14|       37|
|   2015|    7|Clemson                |Boston College              |          34|          17|       17|
|   2015|    7|Colorado State         |Air Force                   |          38|          23|       15|
|   2015|    7|East Carolina          |Tulsa                       |          30|          17|       13|
|   2015|    7|Florida State          |Louisville                  |          41|          21|       20|
|   2015|    7|Georgia                |Missouri                    |           9|           6|        3|
|   2015|    7|Georgia Southern       |New Mexico State            |          56|          26|       30|
|   2015|    7|Ball State             |Georgia State               |          19|          31|      -12|
|   2015|    7|Troy                   |Idaho                       |          16|          19|       -3|
|   2015|    7|Northwestern           |Iowa                        |          10|          40|      -30|
|   2015|    7|Massachusetts          |Kent State                  |          10|          15|       -5|
|   2015|    7|Louisiana State        |Florida                     |          35|          28|        7|
|   2015|    7|Florida Atlantic       |Marshall                    |          17|          33|      -16|
|   2015|    7|Memphis                |Mississippi                 |          37|          24|       13|
|   2015|    7|Miami (FL)             |Virginia Tech               |          30|          20|       10|
|   2015|    7|Michigan               |Michigan State              |          23|          27|       -4|
|   2015|    7|Middle Tennessee State |Florida International       |          42|          34|        8|
|   2015|    7|Mississippi State      |Louisiana Tech              |          45|          20|       25|
|   2015|    7|Minnesota              |Nebraska                    |          25|          48|      -23|
|   2015|    7|New Mexico             |Hawaii                      |          28|          27|        1|
|   2015|    7|North Carolina         |Wake Forest                 |          50|          14|       36|
|   2015|    7|Miami (OH)             |Northern Illinois           |          12|          45|      -33|
|   2015|    7|Notre Dame             |Southern California         |          41|          31|       10|
|   2015|    7|Ohio State             |Penn State                  |          38|          10|       28|
|   2015|    7|Kansas State           |Oklahoma                    |           0|          55|      -55|
|   2015|    7|Old Dominion           |Charlotte                   |          37|          34|        3|
|   2015|    7|Washington             |Oregon                      |          20|          26|       -6|
|   2015|    7|Georgia Tech           |Pittsburgh                  |          28|          31|       -3|
|   2015|    7|Indiana                |Rutgers                     |          52|          55|       -3|
|   2015|    7|San Jose State         |San Diego State             |           7|          30|      -23|
|   2015|    7|South Carolina         |Vanderbilt                  |          19|          10|        9|
|   2015|    7|Connecticut            |South Florida               |          20|          28|       -8|
|   2015|    7|Southern Mississippi   |Texas-San Antonio           |          32|          10|       22|
|   2015|    7|Temple                 |Central Florida             |          30|          16|       14|
|   2015|    7|Iowa State             |Texas Christian             |          21|          45|      -24|
|   2015|    7|Kansas                 |Texas Tech                  |          20|          30|      -10|
|   2015|    7|Toledo                 |Eastern Michigan            |          63|          20|       43|
|   2015|    7|Utah                   |Arizona State               |          34|          18|       16|
|   2015|    7|Virginia               |Syracuse                    |          44|          38|        6|
|   2015|    7|Washington State       |Oregon State                |          52|          31|       21|
|   2015|    7|Ohio                   |Western Michigan            |          14|          49|      -35|
|   2015|    7|Wisconsin              |Purdue                      |          24|           7|       17|
|   2015|    7|Wyoming                |Nevada                      |          28|          21|        7|
|   2015|    8|Arkansas State         |Louisiana-Lafayette         |          37|          27|       10|
|   2015|    8|Appalachian State      |Georgia Southern            |          31|          13|       18|
|   2015|    8|East Carolina          |Temple                      |          14|          24|      -10|
|   2015|    8|UCLA                   |California                  |          40|          24|       16|
|   2015|    8|Tulsa                  |Memphis                     |          42|          66|      -24|
|   2015|    8|San Diego State        |Utah State                  |          48|          14|       34|
|   2015|    8|Air Force              |Fresno State                |          42|          14|       28|
|   2015|    8|Alabama                |Tennessee                   |          19|          14|        5|
|   2015|    8|Arkansas               |Auburn                      |          54|          46|        8|
|   2015|    8|Baylor                 |Iowa State                  |          45|          27|       18|
|   2015|    8|Boise State            |Wyoming                     |          34|          14|       20|
|   2015|    8|Kent State             |Bowling Green State         |           0|          48|      -48|
|   2015|    8|Brigham Young          |Wagner                      |          70|           6|       64|
|   2015|    8|Buffalo                |Ohio                        |          41|          17|       24|
|   2015|    8|Ball State             |Central Michigan            |          21|          23|       -2|
|   2015|    8|Cincinnati             |Connecticut                 |          37|          13|       24|
|   2015|    8|Miami (FL)             |Clemson                     |           0|          58|      -58|
|   2015|    8|Oregon State           |Colorado                    |          13|          17|       -4|
|   2015|    8|Virginia Tech          |Duke                        |          43|          45|       -2|
|   2015|    8|Florida International  |Old Dominion                |          41|          12|       29|
|   2015|    8|Georgia Tech           |Florida State               |          22|          16|        6|
|   2015|    8|Central Florida        |Houston                     |          10|          59|      -49|
|   2015|    8|Idaho                  |Louisiana-Monroe            |          27|          13|       14|
|   2015|    8|Louisiana State        |Western Kentucky            |          48|          20|       28|
|   2015|    8|Louisiana Tech         |Middle Tennessee State      |          45|          16|       29|
|   2015|    8|Louisville             |Boston College              |          17|          14|        3|
|   2015|    8|Marshall               |North Texas                 |          30|          13|       17|
|   2015|    8|Michigan State         |Indiana                     |          52|          26|       26|
|   2015|    8|Mississippi            |Texas A&M                   |          23|           3|       20|
|   2015|    8|Mississippi State      |Kentucky                    |          42|          16|       26|
|   2015|    8|Navy                   |Tulane                      |          31|          14|       17|
|   2015|    8|Nevada                 |Hawaii                      |          30|          20|       10|
|   2015|    8|North Carolina         |Virginia                    |          26|          13|       13|
|   2015|    8|Wake Forest            |North Carolina State        |          17|          35|      -18|
|   2015|    8|Northern Illinois      |Eastern Michigan            |          49|          21|       28|
|   2015|    8|Nebraska               |Northwestern                |          28|          30|       -2|
|   2015|    8|Rutgers                |Ohio State                  |           7|          49|      -42|
|   2015|    8|Oklahoma               |Texas Tech                  |          63|          27|       36|
|   2015|    8|Oklahoma State         |Kansas                      |          58|          10|       48|
|   2015|    8|Penn State             |Maryland                    |          31|          30|        1|
|   2015|    8|Syracuse               |Pittsburgh                  |          20|          23|       -3|
|   2015|    8|Rice                   |Army                        |          38|          31|        7|
|   2015|    8|San Jose State         |New Mexico                  |          31|          21|       10|
|   2015|    8|South Florida          |Southern Methodist          |          38|          14|       24|
|   2015|    8|Southern California    |Utah                        |          42|          24|       18|
|   2015|    8|Charlotte              |Southern Mississippi        |          10|          44|      -34|
|   2015|    8|Stanford               |Washington                  |          31|          14|       17|
|   2015|    8|Texas                  |Kansas State                |          23|           9|       14|
|   2015|    8|Texas State            |South Alabama               |          36|          18|       18|
|   2015|    8|Texas-El Paso          |Florida Atlantic            |          27|          17|       10|
|   2015|    8|Massachusetts          |Toledo                      |          35|          51|      -16|
|   2015|    8|New Mexico State       |Troy                        |           7|          52|      -45|
|   2015|    8|Vanderbilt             |Missouri                    |          10|           3|        7|
|   2015|    8|Arizona                |Washington State            |          42|          45|       -3|
|   2015|    8|Western Michigan       |Miami (OH)                  |          35|          13|       22|
|   2015|    8|Illinois               |Wisconsin                   |          13|          24|      -11|
|   2015|    9|Miami (OH)             |Buffalo                     |          24|          29|       -5|
|   2015|    9|Georgia Southern       |Texas State                 |          37|          13|       24|
|   2015|    9|Pittsburgh             |North Carolina              |          19|          26|       -7|
|   2015|    9|Arizona State          |Oregon                      |          55|          61|       -6|
|   2015|    9|Texas Christian        |West Virginia               |          40|          10|       30|
|   2015|    9|Eastern Michigan       |Western Michigan            |          28|          58|      -30|
|   2015|    9|Connecticut            |East Carolina               |          31|          13|       18|
|   2015|    9|Rice                   |Louisiana Tech              |          17|          42|      -25|
|   2015|    9|Wake Forest            |Louisville                  |          19|          20|       -1|
|   2015|    9|Utah State             |Wyoming                     |          58|          27|       31|
|   2015|    9|Hawaii                 |Air Force                   |           7|          58|      -51|
|   2015|    9|Appalachian State      |Troy                        |          44|          41|        3|
|   2015|    9|Arkansas               |Tennessee-Martin            |          63|          28|       35|
|   2015|    9|Arkansas State         |Georgia State               |          48|          34|       14|
|   2015|    9|Ball State             |Massachusetts               |          20|          10|       10|
|   2015|    9|Nevada-Las Vegas       |Boise State                 |          27|          55|      -28|
|   2015|    9|Akron                  |Central Michigan            |           6|          14|       -8|
|   2015|    9|Cincinnati             |Central Florida             |          52|           7|       45|
|   2015|    9|North Carolina State   |Clemson                     |          41|          56|      -15|
|   2015|    9|Florida                |Georgia                     |          27|           3|       24|
|   2015|    9|Florida Atlantic       |Florida International       |          31|          17|       14|
|   2015|    9|Florida State          |Syracuse                    |          45|          21|       24|
|   2015|    9|Houston                |Vanderbilt                  |          34|           0|       34|
|   2015|    9|Iowa                   |Maryland                    |          31|          15|       16|
|   2015|    9|Iowa State             |Texas                       |          24|           0|       24|
|   2015|    9|Louisiana-Lafayette    |Louisiana-Monroe            |          30|          24|        6|
|   2015|    9|Charlotte              |Marshall                    |          10|          34|      -24|
|   2015|    9|Memphis                |Tulane                      |          41|          13|       28|
|   2015|    9|Duke                   |Miami (FL)                  |          27|          30|       -3|
|   2015|    9|Minnesota              |Michigan                    |          26|          29|       -3|
|   2015|    9|Auburn                 |Mississippi                 |          19|          27|       -8|
|   2015|    9|Navy                   |South Florida               |          29|          17|       12|
|   2015|    9|New Mexico State       |Idaho                       |          55|          48|        7|
|   2015|    9|North Texas            |Texas-San Antonio           |          30|          23|        7|
|   2015|    9|Temple                 |Notre Dame                  |          20|          24|       -4|
|   2015|    9|Kansas                 |Oklahoma                    |           7|          62|      -55|
|   2015|    9|Texas Tech             |Oklahoma State              |          53|          70|      -17|
|   2015|    9|Penn State             |Illinois                    |          39|           0|       39|
|   2015|    9|Purdue                 |Nebraska                    |          55|          45|       10|
|   2015|    9|Colorado State         |San Diego State             |          17|          41|      -24|
|   2015|    9|California             |Southern California         |          21|          27|       -6|
|   2015|    9|Southern Mississippi   |Texas-El Paso               |          34|          13|       21|
|   2015|    9|Washington State       |Stanford                    |          28|          30|       -2|
|   2015|    9|Kentucky               |Tennessee                   |          21|          52|      -31|
|   2015|    9|Texas A&M              |South Carolina              |          35|          28|        7|
|   2015|    9|Southern Methodist     |Tulsa                       |          31|          40|       -9|
|   2015|    9|UCLA                   |Colorado                    |          35|          31|        4|
|   2015|    9|Utah                   |Oregon State                |          27|          12|       15|
|   2015|    9|Virginia               |Georgia Tech                |          27|          21|        6|
|   2015|    9|Boston College         |Virginia Tech               |          10|          26|      -16|
|   2015|    9|Washington             |Arizona                     |          49|           3|       46|
|   2015|    9|Old Dominion           |Western Kentucky            |          30|          55|      -25|
|   2015|    9|Wisconsin              |Rutgers                     |          48|          10|       38|
|   2015|   10|Toledo                 |Northern Illinois           |          27|          32|       -5|
|   2015|   10|Bowling Green State    |Ohio                        |          62|          24|       38|
|   2015|   10|Appalachian State      |Arkansas State              |          27|          40|      -13|
|   2015|   10|Kansas State           |Baylor                      |          24|          31|       -7|
|   2015|   10|Kent State             |Buffalo                     |          17|          18|       -1|
|   2015|   10|Missouri               |Mississippi State           |          13|          31|      -18|
|   2015|   10|Fresno State           |Nevada                      |          16|          30|      -14|
|   2015|   10|Western Michigan       |Ball State                  |          54|           7|       47|
|   2015|   10|San Jose State         |Brigham Young               |          16|          17|       -1|
|   2015|   10|Southern Methodist     |Temple                      |          40|          60|      -20|
|   2015|   10|Texas-El Paso          |Rice                        |          24|          21|        3|
|   2015|   10|Air Force              |Army                        |          20|           3|       17|
|   2015|   10|Massachusetts          |Akron                       |          13|          17|       -4|
|   2015|   10|Alabama                |Louisiana State             |          30|          16|       14|
|   2015|   10|Mississippi            |Arkansas                    |          52|          53|       -1|
|   2015|   10|Texas A&M              |Auburn                      |          10|          26|      -16|
|   2015|   10|Clemson                |Florida State               |          23|          13|       10|
|   2015|   10|Wyoming                |Colorado State              |           7|          26|      -19|
|   2015|   10|Tulane                 |Connecticut                 |           3|           7|       -4|
|   2015|   10|Florida                |Vanderbilt                  |           9|           7|        2|
|   2015|   10|Florida International  |Charlotte                   |          48|          31|       17|
|   2015|   10|Georgia                |Kentucky                    |          27|           3|       24|
|   2015|   10|Houston                |Cincinnati                  |          33|          30|        3|
|   2015|   10|Purdue                 |Illinois                    |          14|          48|      -34|
|   2015|   10|Indiana                |Iowa                        |          27|          35|       -8|
|   2015|   10|Louisiana Tech         |North Texas                 |          56|          13|       43|
|   2015|   10|Georgia State          |Louisiana-Lafayette         |          21|          23|       -2|
|   2015|   10|Louisville             |Syracuse                    |          41|          17|       24|
|   2015|   10|Miami (FL)             |Virginia                    |          27|          21|        6|
|   2015|   10|Miami (OH)             |Eastern Michigan            |          28|          13|       15|
|   2015|   10|Michigan               |Rutgers                     |          49|          16|       33|
|   2015|   10|Middle Tennessee State |Marshall                    |          27|          24|        3|
|   2015|   10|Memphis                |Navy                        |          20|          45|      -25|
|   2015|   10|Nebraska               |Michigan State              |          39|          38|        1|
|   2015|   10|Nevada-Las Vegas       |Hawaii                      |          41|          21|       20|
|   2015|   10|New Mexico             |Utah State                  |          14|          13|        1|
|   2015|   10|Texas State            |New Mexico State            |          21|          31|      -10|
|   2015|   10|North Carolina         |Duke                        |          66|          31|       35|
|   2015|   10|Boston College         |North Carolina State        |           8|          24|      -16|
|   2015|   10|Northwestern           |Penn State                  |          23|          21|        2|
|   2015|   10|Pittsburgh             |Notre Dame                  |          30|          42|      -12|
|   2015|   10|Ohio State             |Minnesota                   |          28|          14|       14|
|   2015|   10|Oklahoma               |Iowa State                  |          52|          16|       36|
|   2015|   10|Oklahoma State         |Texas Christian             |          49|          29|       20|
|   2015|   10|Texas-San Antonio      |Old Dominion                |          31|          36|       -5|
|   2015|   10|Oregon                 |California                  |          44|          28|       16|
|   2015|   10|South Alabama          |Idaho                       |          52|          45|        7|
|   2015|   10|East Carolina          |South Florida               |          17|          22|       -5|
|   2015|   10|Southern California    |Arizona                     |          38|          30|        8|
|   2015|   10|Colorado               |Stanford                    |          10|          42|      -32|
|   2015|   10|Tennessee              |South Carolina              |          27|          24|        3|
|   2015|   10|Texas                  |Kansas                      |          59|          20|       39|
|   2015|   10|Troy                   |Louisiana-Monroe            |          51|          14|       37|
|   2015|   10|Tulsa                  |Central Florida             |          45|          30|       15|
|   2015|   10|Oregon State           |UCLA                        |           0|          41|      -41|
|   2015|   10|Washington             |Utah                        |          23|          34|      -11|
|   2015|   10|Washington State       |Arizona State               |          38|          24|       14|
|   2015|   10|West Virginia          |Texas Tech                  |          31|          26|        5|
|   2015|   10|Western Kentucky       |Florida Atlantic            |          35|          19|       16|
|   2015|   10|Maryland               |Wisconsin                   |          24|          31|       -7|
|   2015|   11|Ohio                   |Kent State                  |          27|           0|       27|
|   2015|   11|Central Michigan       |Toledo                      |          23|          28|       -5|
|   2015|   11|Western Michigan       |Bowling Green State         |          27|          41|      -14|
|   2015|   11|Buffalo                |Northern Illinois           |          30|          41|      -11|
|   2015|   11|South Alabama          |Louisiana-Lafayette         |          32|          25|        7|
|   2015|   11|Georgia Tech           |Virginia Tech               |          21|          23|       -2|
|   2015|   11|Colorado               |Southern California         |          24|          27|       -3|
|   2015|   11|Air Force              |Utah State                  |          35|          28|        7|
|   2015|   11|Miami (OH)             |Akron                       |          28|          37|       -9|
|   2015|   11|Mississippi State      |Alabama                     |           6|          31|      -25|
|   2015|   11|Idaho                  |Appalachian State           |          20|          47|      -27|
|   2015|   11|Arizona                |Utah                        |          37|          30|        7|
|   2015|   11|Arizona State          |Washington                  |          27|          17|       10|
|   2015|   11|Louisiana State        |Arkansas                    |          14|          31|      -17|
|   2015|   11|Louisiana-Monroe       |Arkansas State              |          21|          59|      -38|
|   2015|   11|California             |Oregon State                |          54|          24|       30|
|   2015|   11|Cincinnati             |Tulsa                       |          49|          38|       11|
|   2015|   11|Syracuse               |Clemson                     |          27|          37|      -10|
|   2015|   11|Colorado State         |Nevada-Las Vegas            |          49|          35|       14|
|   2015|   11|South Carolina         |Florida                     |          14|          24|      -10|
|   2015|   11|Florida State          |North Carolina State        |          34|          17|       17|
|   2015|   11|Hawaii                 |Fresno State                |          14|          42|      -28|
|   2015|   11|Auburn                 |Georgia                     |          13|          20|       -7|
|   2015|   11|Troy                   |Georgia Southern            |          10|          45|      -35|
|   2015|   11|Texas State            |Georgia State               |          19|          41|      -22|
|   2015|   11|Houston                |Memphis                     |          35|          34|        1|
|   2015|   11|Iowa                   |Minnesota                   |          40|          35|        5|
|   2015|   11|Louisville             |Virginia                    |          38|          31|        7|
|   2015|   11|Marshall               |Florida International       |          52|           0|       52|
|   2015|   11|Eastern Michigan       |Massachusetts               |          17|          28|      -11|
|   2015|   11|Indiana                |Michigan                    |          41|          48|       -7|
|   2015|   11|Michigan State         |Maryland                    |          24|           7|       17|
|   2015|   11|Florida Atlantic       |Middle Tennessee State      |          17|          24|       -7|
|   2015|   11|Missouri               |Brigham Young               |          20|          16|        4|
|   2015|   11|Navy                   |Southern Methodist          |          55|          14|       41|
|   2015|   11|Rutgers                |Nebraska                    |          14|          31|      -17|
|   2015|   11|Nevada                 |San Jose State              |          37|          34|        3|
|   2015|   11|Boise State            |New Mexico                  |          24|          31|       -7|
|   2015|   11|North Carolina         |Miami (FL)                  |          59|          21|       38|
|   2015|   11|Northwestern           |Purdue                      |          21|          14|        7|
|   2015|   11|Notre Dame             |Wake Forest                 |          28|           7|       21|
|   2015|   11|Illinois               |Ohio State                  |           3|          28|      -25|
|   2015|   11|Baylor                 |Oklahoma                    |          34|          44|      -10|
|   2015|   11|Iowa State             |Oklahoma State              |          31|          35|       -4|
|   2015|   11|Old Dominion           |Texas-El Paso               |          31|          21|       10|
|   2015|   11|Stanford               |Oregon                      |          36|          38|       -2|
|   2015|   11|Duke                   |Pittsburgh                  |          13|          31|      -18|
|   2015|   11|San Diego State        |Wyoming                     |          38|           3|       35|
|   2015|   11|South Florida          |Temple                      |          44|          23|       21|
|   2015|   11|Rice                   |Southern Mississippi        |          10|          65|      -55|
|   2015|   11|Tennessee              |North Texas                 |          24|           0|       24|
|   2015|   11|Texas A&M              |Western Carolina            |          41|          17|       24|
|   2015|   11|Texas Christian        |Kansas                      |          23|          17|        6|
|   2015|   11|Texas Tech             |Kansas State                |          59|          44|       15|
|   2015|   11|Charlotte              |Texas-San Antonio           |          27|          30|       -3|
|   2015|   11|Army                   |Tulane                      |          31|          34|       -3|
|   2015|   11|Vanderbilt             |Kentucky                    |          21|          17|        4|
|   2015|   11|UCLA                   |Washington State            |          27|          31|       -4|
|   2015|   11|West Virginia          |Texas                       |          38|          20|       18|
|   2015|   12|Ohio                   |Ball State                  |          48|          31|       17|
|   2015|   12|Bowling Green State    |Toledo                      |          28|          44|      -16|
|   2015|   12|Kent State             |Central Michigan            |          14|          27|      -13|
|   2015|   12|Northern Illinois      |Western Michigan            |          27|          19|        8|
|   2015|   12|Central Florida        |East Carolina               |           7|          44|      -37|
|   2015|   12|Texas State            |Louisiana-Monroe            |          16|           3|       13|
|   2015|   12|Boise State            |Air Force                   |          30|          37|       -7|
|   2015|   12|South Florida          |Cincinnati                  |          65|          27|       38|
|   2015|   12|Akron                  |Buffalo                     |          42|          21|       21|
|   2015|   12|Alabama                |Charleston Southern         |          56|           6|       50|
|   2015|   12|Arizona State          |Arizona                     |          52|          37|       15|
|   2015|   12|Auburn                 |Idaho                       |          56|          34|       22|
|   2015|   12|Oklahoma State         |Baylor                      |          35|          45|      -10|
|   2015|   12|Brigham Young          |Fresno State                |          52|          10|       42|
|   2015|   12|South Carolina         |Citadel                     |          22|          23|       -1|
|   2015|   12|Clemson                |Wake Forest                 |          33|          13|       20|
|   2015|   12|New Mexico             |Colorado State              |          21|          28|       -7|
|   2015|   12|Connecticut            |Houston                     |          20|          17|        3|
|   2015|   12|Florida                |Florida Atlantic            |          20|          14|        6|
|   2015|   12|Florida State          |Chattanooga                 |          52|          13|       39|
|   2015|   12|Georgia                |Georgia Southern            |          23|          17|        6|
|   2015|   12|Georgia State          |South Alabama               |          24|          10|       14|
|   2015|   12|Maryland               |Indiana                     |          28|          47|      -19|
|   2015|   12|Iowa                   |Purdue                      |          40|          20|       20|
|   2015|   12|Kansas State           |Iowa State                  |          38|          35|        3|
|   2015|   12|Kentucky               |Charlotte                   |          58|          10|       48|
|   2015|   12|Texas-El Paso          |Louisiana Tech              |          15|          17|       -2|
|   2015|   12|Miami (FL)             |Georgia Tech                |          38|          21|       17|
|   2015|   12|Massachusetts          |Miami (OH)                  |          13|          20|       -7|
|   2015|   12|Penn State             |Michigan                    |          16|          28|      -12|
|   2015|   12|Ohio State             |Michigan State              |          14|          17|       -3|
|   2015|   12|Middle Tennessee State |North Texas                 |          41|           7|       34|
|   2015|   12|Minnesota              |Illinois                    |          32|          23|        9|
|   2015|   12|Mississippi            |Louisiana State             |          38|          17|       21|
|   2015|   12|Arkansas               |Mississippi State           |          50|          51|       -1|
|   2015|   12|Tulsa                  |Navy                        |          21|          44|      -23|
|   2015|   12|Louisiana-Lafayette    |New Mexico State            |          34|          37|       -3|
|   2015|   12|Virginia Tech          |North Carolina              |          27|          30|       -3|
|   2015|   12|North Carolina State   |Syracuse                    |          42|          29|       13|
|   2015|   12|Wisconsin              |Northwestern                |           7|          13|       -6|
|   2015|   12|Notre Dame             |Boston College              |          19|          16|        3|
|   2015|   12|Oklahoma               |Texas Christian             |          30|          29|        1|
|   2015|   12|Oregon                 |Southern California         |          48|          28|       20|
|   2015|   12|Pittsburgh             |Louisville                  |          45|          34|       11|
|   2015|   12|Army                   |Rutgers                     |          21|          31|      -10|
|   2015|   12|Nevada-Las Vegas       |San Diego State             |          14|          52|      -38|
|   2015|   12|Hawaii                 |San Jose State              |          23|          42|      -19|
|   2015|   12|Southern Methodist     |Tulane                      |          49|          21|       28|
|   2015|   12|Southern Mississippi   |Old Dominion                |          56|          31|       25|
|   2015|   12|Stanford               |California                  |          35|          22|       13|
|   2015|   12|Temple                 |Memphis                     |          31|          12|       19|
|   2015|   12|Missouri               |Tennessee                   |           8|          19|      -11|
|   2015|   12|Vanderbilt             |Texas A&M                   |           0|          25|      -25|
|   2015|   12|Texas-San Antonio      |Rice                        |          34|          24|       10|
|   2015|   12|Utah                   |UCLA                        |           9|          17|       -8|
|   2015|   12|Utah State             |Nevada                      |          31|          27|        4|
|   2015|   12|Virginia               |Duke                        |          42|          34|        8|
|   2015|   12|Oregon State           |Washington                  |           7|          52|      -45|
|   2015|   12|Washington State       |Colorado                    |          27|           3|       24|
|   2015|   12|Kansas                 |West Virginia               |           0|          49|      -49|
|   2015|   12|Florida International  |Western Kentucky            |           7|          63|      -56|
|   2015|   13|Ball State             |Bowling Green State         |          10|          48|      -38|
|   2015|   13|Northern Illinois      |Ohio                        |          21|          26|       -5|
|   2015|   13|Texas                  |Texas Tech                  |          45|          48|       -3|
|   2015|   13|Akron                  |Kent State                  |          20|           0|       20|
|   2015|   13|Arkansas               |Missouri                    |          28|           3|       25|
|   2015|   13|San Jose State         |Boise State                 |          23|          40|      -17|
|   2015|   13|Central Michigan       |Eastern Michigan            |          35|          28|        7|
|   2015|   13|Georgia State          |Troy                        |          31|          21|       10|
|   2015|   13|Houston                |Navy                        |          52|          31|       21|
|   2015|   13|Nebraska               |Iowa                        |          20|          28|       -8|
|   2015|   13|Buffalo                |Massachusetts               |          26|          31|       -5|
|   2015|   13|Pittsburgh             |Miami (FL)                  |          24|          29|       -5|
|   2015|   13|Oregon                 |Oregon State                |          52|          42|       10|
|   2015|   13|Central Florida        |South Florida               |           3|          44|      -41|
|   2015|   13|Texas Christian        |Baylor                      |          28|          21|        7|
|   2015|   13|Tulane                 |Tulsa                       |          34|          45|      -11|
|   2015|   13|Washington             |Washington State            |          45|          10|       35|
|   2015|   13|Western Kentucky       |Marshall                    |          49|          28|       21|
|   2015|   13|Toledo                 |Western Michigan            |          30|          35|       -5|
|   2015|   13|Auburn                 |Alabama                     |          13|          29|      -16|
|   2015|   13|Appalachian State      |Louisiana-Lafayette         |          28|           7|       21|
|   2015|   13|New Mexico State       |Arkansas State              |          28|          52|      -24|
|   2015|   13|Utah State             |Brigham Young               |          28|          51|      -23|
|   2015|   13|California             |Arizona State               |          48|          46|        2|
|   2015|   13|East Carolina          |Cincinnati                  |          16|          19|       -3|
|   2015|   13|South Carolina         |Clemson                     |          32|          37|       -5|
|   2015|   13|Fresno State           |Colorado State              |          31|          34|       -3|
|   2015|   13|Wake Forest            |Duke                        |          21|          27|       -6|
|   2015|   13|Old Dominion           |Florida Atlantic            |          31|          33|       -2|
|   2015|   13|Florida                |Florida State               |           2|          27|      -25|
|   2015|   13|Georgia Tech           |Georgia                     |           7|          13|       -6|
|   2015|   13|Georgia Southern       |South Alabama               |          55|          17|       38|
|   2015|   13|Hawaii                 |Louisiana-Monroe            |          28|          26|        2|
|   2015|   13|Idaho                  |Texas State                 |          38|          31|        7|
|   2015|   13|Purdue                 |Indiana                     |          36|          54|      -18|
|   2015|   13|Kansas                 |Kansas State                |          14|          45|      -31|
|   2015|   13|Louisiana State        |Texas A&M                   |          19|           7|       12|
|   2015|   13|Kentucky               |Louisville                  |          24|          38|      -14|
|   2015|   13|Rutgers                |Maryland                    |          41|          46|       -5|
|   2015|   13|Memphis                |Southern Methodist          |          63|           0|       63|
|   2015|   13|Michigan State         |Penn State                  |          55|          16|       39|
|   2015|   13|Texas-San Antonio      |Middle Tennessee State      |           7|          42|      -35|
|   2015|   13|Mississippi State      |Mississippi                 |          27|          38|      -11|
|   2015|   13|New Mexico             |Air Force                   |          47|          35|       12|
|   2015|   13|North Carolina State   |North Carolina              |          34|          45|      -11|
|   2015|   13|Northwestern           |Illinois                    |          24|          14|       10|
|   2015|   13|Michigan               |Ohio State                  |          13|          42|      -29|
|   2015|   13|Oklahoma State         |Oklahoma                    |          23|          58|      -35|
|   2015|   13|Rice                   |Charlotte                   |          27|           7|       20|
|   2015|   13|San Diego State        |Nevada                      |          31|          14|       17|
|   2015|   13|Southern California    |UCLA                        |          40|          21|       19|
|   2015|   13|Louisiana Tech         |Southern Mississippi        |          24|          58|      -34|
|   2015|   13|Stanford               |Notre Dame                  |          38|          36|        2|
|   2015|   13|Syracuse               |Boston College              |          20|          17|        3|
|   2015|   13|Temple                 |Connecticut                 |          27|           3|       24|
|   2015|   13|Tennessee              |Vanderbilt                  |          53|          28|       25|
|   2015|   13|North Texas            |Texas-El Paso               |          17|          20|       -3|
|   2015|   13|Utah                   |Colorado                    |          20|          14|        6|
|   2015|   13|Virginia               |Virginia Tech               |          20|          23|       -3|
|   2015|   13|West Virginia          |Iowa State                  |          30|           6|       24|
|   2015|   13|Minnesota              |Wisconsin                   |          21|          31|      -10|
|   2015|   13|Wyoming                |Nevada-Las Vegas            |          35|          28|        7|
|   2015|   14|Bowling Green State    |Northern Illinois           |          34|          14|       20|
|   2015|   14|Alabama                |Florida                     |          29|          15|       14|
|   2015|   14|South Alabama          |Appalachian State           |          27|          34|       -7|
|   2015|   14|Arkansas State         |Texas State                 |          55|          17|       38|
|   2015|   14|Clemson                |North Carolina              |          45|          37|        8|
|   2015|   14|Georgia Southern       |Georgia State               |           7|          34|      -27|
|   2015|   14|Houston                |Temple                      |          24|          13|       11|
|   2015|   14|Kansas State           |West Virginia               |          24|          23|        1|
|   2015|   14|Louisiana-Monroe       |New Mexico State            |          42|          35|        7|
|   2015|   14|Michigan State         |Iowa                        |          16|          13|        3|
|   2015|   14|San Diego State        |Air Force                   |          27|          24|        3|
|   2015|   14|Stanford               |Southern California         |          41|          22|       19|
|   2015|   14|Baylor                 |Texas                       |          17|          23|       -6|
|   2015|   14|Louisiana-Lafayette    |Troy                        |          17|          41|      -24|
|   2015|   14|Western Kentucky       |Southern Mississippi        |          45|          28|       17|
|   2015|   15|Navy                   |Army                        |          21|          17|        4|
|   2015|   16|Appalachian State      |Ohio                        |          31|          29|        2|
|   2015|   16|Arizona                |New Mexico                  |          45|          37|        8|
|   2015|   16|Louisiana Tech         |Arkansas State              |          47|          28|       19|
|   2015|   16|San Jose State         |Georgia State               |          27|          16|       11|
|   2015|   16|Utah                   |Brigham Young               |          35|          28|        7|
|   2015|   16|Western Kentucky       |South Florida               |          45|          35|       10|
|   2015|   17|Akron                  |Utah State                  |          23|          21|        2|
|   2015|   17|Toledo                 |Temple                      |          32|          17|       15|
|   2015|   17|Boise State            |Northern Illinois           |          55|           7|       48|
|   2015|   17|Georgia Southern       |Bowling Green State         |          58|          27|       31|
|   2015|   17|San Diego State        |Cincinnati                  |          42|           7|       35|
|   2015|   17|Western Michigan       |Middle Tennessee State      |          45|          31|       14|
|   2015|   17|Duke                   |Indiana                     |          44|          41|        3|
|   2015|   17|Marshall               |Connecticut                 |          16|          10|        6|
|   2015|   17|Nebraska               |UCLA                        |          37|          29|        8|
|   2015|   17|Virginia Tech          |Tulsa                       |          55|          52|        3|
|   2015|   17|Washington             |Southern Mississippi        |          44|          31|       13|
|   2015|   17|Washington State       |Miami (FL)                  |          20|          14|        6|
|   2015|   17|Minnesota              |Central Michigan            |          21|          14|        7|
|   2015|   17|Navy                   |Pittsburgh                  |          44|          28|       16|
|   2015|   18|Baylor                 |North Carolina              |          49|          38|       11|
|   2015|   18|California             |Air Force                   |          55|          36|       19|
|   2015|   18|Louisiana State        |Texas Tech                  |          56|          27|       29|
|   2015|   18|Nevada                 |Colorado State              |          28|          23|        5|
|   2015|   18|Auburn                 |Memphis                     |          31|          10|       21|
|   2015|   18|Louisville             |Texas A&M                   |          27|          21|        6|
|   2015|   18|Mississippi State      |North Carolina State        |          51|          28|       23|
|   2015|   18|Wisconsin              |Southern California         |          23|          21|        2|
|   2015|   18|Alabama                |Michigan State              |          38|           0|       38|
|   2015|   18|Clemson                |Oklahoma                    |          37|          17|       20|
|   2015|   18|Houston                |Florida State               |          38|          24|       14|
|   2015|   18|Michigan               |Florida                     |          41|           7|       34|
|   2015|   18|Mississippi            |Oklahoma State              |          48|          20|       28|
|   2015|   18|Ohio State             |Notre Dame                  |          44|          28|       16|
|   2015|   18|Stanford               |Iowa                        |          45|          16|       29|
|   2015|   18|Tennessee              |Northwestern                |          45|           6|       39|
|   2015|   18|Arkansas               |Kansas State                |          45|          23|       22|
|   2015|   18|Georgia                |Penn State                  |          24|          17|        7|
|   2015|   18|Texas Christian        |Oregon                      |          47|          41|        6|
|   2015|   18|West Virginia          |Arizona State               |          43|          42|        1|
|   2015|   19|Alabama                |Clemson                     |          45|          40|        5|

Now comes the tricky part. I need to turn this long data frame of game
results into a wide format that includes the team names as variable names.
I've written the following helper function to "stack" the teams in each 
game so that we don't have to worry about having two team name columns.


```r
organize_game_results <- function(df, ...){
  home_teams <- df %>%
    select(Season, Game_Num, Home, Home_MOV, ...) %>% 
    mutate(involved = 1) %>% 
    rename(Team = Home)
  away_teams <- df %>%
    select(Season, Game_Num, Away, Home_MOV, ...) %>% 
    mutate(involved = -1) %>% 
    rename(Team = Away)
  return(bind_rows(home_teams, away_teams))
}
```

I'm going to do a bit of R Kung Fu but here is my basic approach:
* ["Nest"](https://blog.rstudio.org/2016/02/02/tidyr-0-4-0/) the stacked team
data so that the data is grouped by Season. Since each season's data is just
stored as a list in a column of the data frame we can use `purrr`'s
`map` function to apply functions to each season of data seperately.
* Use the `spread` function from the `tidyr` package to go from a tall to 
wide format
* Use a linear regression to calculate the Massey Rankings
* Use the `tidy` function from the `broom` package to extract the coefficient
values (which are the actual team rankings)
* Then unnest each season to get one data frame for our results


```r
season_team_values <- results %>%
  organize_game_results %>%
  group_by(Season) %>%
  nest() %>% 
  mutate(regression_df = map(data, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num, data = .)),
         team_values = map(model, tidy)) %>%
  unnest(team_values) %>%
  mutate(Team = str_replace_all(term, fixed("`"), ""))

knitr::kable(head(season_team_values))
```



| Season|term                 |  estimate|  std.error| statistic|   p.value|Team               |
|------:|:--------------------|---------:|----------:|---------:|---------:|:------------------|
|   2005|(Intercept)          |  3.646571|  0.5735598|  6.357787| 0.0000000|(Intercept)        |
|   2005|`Air Force`          | 31.465096| 15.7468260|  1.998187| 0.0461865|Air Force          |
|   2005|Akron                | 27.347303| 15.5560123|  1.757989| 0.0793035|Akron              |
|   2005|Alabama              | 46.604288| 15.6858803|  2.971098| 0.0030966|Alabama            |
|   2005|`Alabama-Birmingham` | 30.838248| 15.8030438|  1.951412| 0.0515135|Alabama-Birmingham |
|   2005|`Appalachian State`  | 22.981138| 18.3074848|  1.255286| 0.2099059|Appalachian State  |

Hopefully that wasn't too complicated

### Problems

If you inspect the results you'll see that there are some areas where we could
improve our results. Here is a list off the top of my head:

1. We don't adjust for blowouts at all
2. We don't adjust for teams who only play 1 or 2 games all season. For
example, Portland State beat two very bad teams in 2015 but those were their
only games so they are rated as one of the best teams in the country. We 
could just consider all teams who only showed up 1-2 times as one team, sort
of as a placeholder for "FCS" teams (the lower division of CFB).
3. We don't regularize our results at all
4. We don't include any team priors from the previous season.

While I certainly believe all of these areas could help improve our results
I'd like a way to know for sure. Since the goal of this post is to determine
how good each team was in a season we can just test that. If our team ratings
accurately predict the results of games between teams then we can assume that
the ratings reflect the true quality of teams.

What we don't want to do is use the same underlying games to both generate and
test the accuracy of the team rankings, then we would be rewarding methods that
overfit to the season results and not the true underlying team quality. So we
will have to generate our team rankings and then test their accuracy on 
different sets of games. I'm going to do this using cross validation which is 
a popular way to test the accuracy of models while avoiding overfitting. 

Let's find the accuracy of this base method we have already used. I'm using 
a helper function called `safe_pred` which you can find in the raw R file I
used to write this post. It basically helps make predictions when a team 
only shows up in the training or test data but not both, which normally
would cause an error. 



```r
K <- 4
cv_results <- results %>%
  mutate(fold_id = sample(1:K, size = n(), replace = T)) %>%
  organize_game_results(fold_id) %>% 
  group_by(Season) %>%
  nest() %>% 
  crossing(fold = 1:K) %>% 
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num - fold_id, data = .)),
         preds = map2(model, test_df, safe_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred)^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error), sd_mse = sd(error))

c(mean(cv_results$mean_mse), mean(cv_results$sd_mse))
```

```
## [1] 263.82627  26.74376
```

So that's our baseline Mean Squared Error.  

Let's try and beat it using the 2nd idea I had. The first is kind of arbitrary
(what do we define as a blowout?) and the fourth we will focus on in the actual
prediction model we are trying to build, so I don't think it's needed yet.



```r
game_list <- results %>%
  mutate(fold_id = sample(1:K, size = n(), replace = T)) %>%
  organize_game_results(fold_id)

bad_teams <- game_list %>%
  count(Season, Team) %>%
  mutate(bad_team = n <= 2)

fcs_fix_results <- game_list %>%
  left_join(bad_teams, by = c("Season", "Team")) %>%
  mutate(Team = ifelse(bad_team, "FCS", Team)) %>%
  select(-n, -bad_team) %>%
  group_by(Season) %>%
  nest() %>% 
  crossing(fold = 1:K) %>% 
  mutate(train = map2(data, fold, function(df, fold_num) filter(df, fold_id != fold_num)),
         test = map2(data, fold, function(df, fold_num) filter(df, fold_id == fold_num)),
         regression_df = map(train, spread, Team, involved, fill = 0),
         test_df = map(test, spread, Team, involved, fill = 0),
         model = map(regression_df, ~ lm(Home_MOV ~ . - Game_Num - fold_id, data = .)),
         preds = map2(model, test_df, safe_pred),
         error = map_dbl(preds, function(df) mean((df$Home_MOV - df$pred)^2))) %>%
  group_by(Season) %>%
  summarize(mean_mse = mean(error))
  
c(mean(fcs_fix_results$mean_mse), mean(fcs_fix_results$sd_mse))
```

```
## Warning in mean.default(fcs_fix_results$sd_mse): argument is not numeric or
## logical: returning NA
```

```
## [1] 264.0257       NA
```

Well that wasn't even an improvement, I guess since by definition those 
teams don't appear too often it really wouldn't make that much of a 
difference. What about using a penalized regression like ridge regression
to reduce the size of the coefficients? Once again we are going to need a
helper function to help us accomplish this. 


```
## Error in eval(expr, envir, enclos): argument "group" is missing, with no default
```

```
## [1] 518.9695  96.5959
```

Wow, that's way worse than the linear regression results.

### Conclussions

So there you have it, we've shown a way to generate ratings for team quality
in a season and shown that those are about as good as we could do using the
limited data we do have. Also I'm pretty proud of getting that grouped cross
validation code to work using nested data frames and the `purrr` package. 
