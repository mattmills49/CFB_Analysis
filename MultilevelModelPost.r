library(arm)
library(dplyr)
library(ggplot2)
library(magrittr)
library(lme4)
library(glmnet)
library(stringr)
library(tidyr)

source("multiplot.r")
source("readin.r")
# multiplot.r is a function found online that will plot multiple ggplots in a grid
# readin.r is my own helper function read in data from multiple folders where I store the CFB Stats data
options(dplyr.width = Inf)

### Read in the Data we Need ###

years <- 2014
plays <- readin("play", years)
# The NCAA treats sacks as runs (dumb) and I am choosing to remove those plays from this analysis
runs <- readin("rush", years) %>% filter(Sack == 0)
players <- readin("player", years)
teams <- readin("team", years)

### Augment Runner Info and get basic stats for runners ###

runs <- left_join(runs, select(plays, Game.Code, Play.Number, Offense.Team.Code, Defense.Team.Code, Down, Distance, Spot), by = c("Game.Code", "Play.Number"))

runner_info <- runs %>% group_by(Player.Code) %>% summarize(YPA = mean(Yards), Runs = n(), TDs = sum(Touchdown))

### Fit Mixed Effects Model for Yards ###

base_lme_model <- lmer(Yards ~ 1 + (1 | Offense.Team.Code) + (1 | Defense.Team.Code) + (1 | Player.Code), data = runs)
lme_values <- coef(base_lme_model)
lme_se <- se.ranef(base_lme_model)
# lme_values is a list that containes the estimates of the random effects coefficients for each different variable. 
# lme_se is the same

### Add estimates of the player's coefficient back into the runner_info ###

runners <- lme_values$Player.Code
runners$Player.Code <- as.numeric(row.names(runners))
names(runners)[1] <- "LME_Value"
runners <- left_join(runners, select(players, Player.Code, Team.Code, Last.Name, First.Name), by = c("Player.Code")) %>% left_join(teams, by = c("Team.Code")) %>% left_join(runner_info, by = c("Player.Code"))

### Get estimates of Offense and Defense ###

offenses <- lme_values$Offense.Team.Code
offenses$Team.Code <- as.numeric(row.names(offenses))
names(offenses)[1] <- "LME_O_Value"
teams <- right_join(teams, offenses, by = c("Team.Code"))

defenses <- lme_values$Defense.Team.Code
defenses$Team.Code <- as.numeric(row.names(defenses))
names(defenses)[1] <- "LME_D_Value"
teams <- right_join(teams, defenses, by = c("Team.Code"))

runner_se <- data_frame(LME_SE = unname(lme_se$Player.Code[, 1]), Player.Code = as.numeric(row.names(lme_se$Player.Code)))

runners <- left_join(runners, runner_se, by = c("Player.Code"))

### Fit Ridge Regression Model ###

# The original ridge regression takes forever to run so I simply reduced the duplicate entries to a weighted regression with the weights as the number of times that observation occured. For example multiple times in a game a running back will run for 4 yards against the same defense, so we just count the number of times that happens.  
weighted_runs <- runs %>% group_by(Yards, Offense.Team.Code, Defense.Team.Code, Player.Code) %>% tally
weighted_runs$Offense.Team.Code %<>% as.factor
weighted_runs$Defense.Team.Code %<>% as.factor
weighted_runs$Player.Code %<>% as.factor
# We can't use the base model.matrix formula because we want to include every dummy variable, we don't want a baseline built in to the intercept. So I had to use the contrasts.arg trick found on stack overflow. 
model_data <- model.matrix(Yards ~ Offense.Team.Code + Defense.Team.Code + Player.Code, data = weighted_runs, contrasts.arg = lapply(weighted_runs[,sapply(weighted_runs,is.factor)], contrasts, contrasts=FALSE))

# The model takes a while so I just load in a saved model for future iterations
# base_rr_model <- glmnet(x = model_data, y = weighted_runs$Yards, weights = weighted_runs$n, alpha = 0)
load("base_rr_model.rdata")
ridge_values <- coef(base_rr_model,s = 8.932324)[,1] # this lambda was found through CV
# These coefficients contain all the estimates for all variables so we need to find the player ones we want
ridge_runners <- data_frame(Runners = names(ridge_values)[str_detect(names(ridge_values), "Player")], Ridge_Value = unname(ridge_values[str_detect(names(ridge_values), "Player")]))
ridge_runners$Player.Code <- as.numeric(str_extract(ridge_runners$Runners, "[0-9]+"))

runner_data <- left_join(runners, select(ridge_runners, -Runners), by = c("Player.Code"))

### Build Plots ###

select(runner_data, Player.Code, LME_Value, YPA) %>% gather(Measure, Value, -Player.Code) %>% ggplot(aes(x = Measure, y = Value)) + geom_violin(fill = "black") + theme_538
select(runner_data, Player.Code, LME_Value, YPA) %>% gather(Measure, Value, -Player.Code) %>% mutate(Measure = factor(Measure, labels = c("Mixed Effects Model Estimate", "Yards per Attempt"))) %>% ggplot(aes(x = Value)) + geom_histogram(binwidth = 2) + facet_wrap(~Measure, ncol = 1) + theme_538 + theme(strip.text = element_text(size = 16)) + ggtitle("Distribution of Runner Estimates") + xlim(-25,25)
select(runner_data, Player.Code, LME_Value, YPA) %>% gather(Measure, Value, -Player.Code) %>% mutate(Measure = factor(Measure, labels = c("Mixed Effects Model Estimate", "Yards per Attempt"))) %>% ggplot(aes(x = Value)) + geom_density(aes(fill = Measure)) + theme_538 + theme(legend.position = "top")

num_ypa <- ggplot(runner_data, aes(x = Runs, y = YPA)) + geom_point(alpha = .6) + xlab("Number of Rushes") + ylab("Yards per Attempt") + ggtitle("Effect of number of runs on YPA") + theme_538
r_ypa <- ggplot(runner_data, aes(Ridge_Value, YPA)) + geom_point(aes(size = Runs), alpha = .6) + xlab("Ridge Regression\nCoefficient Estimates") + ylab("Yards per Attempt") + ggtitle("Ridge Regression Estimates vs Yards per Attmept") + theme_538 + theme(legend.position = c(.75,.25))
num_r <- ggplot(runner_data, aes(Runs, Ridge_Value)) + geom_point(alpha = .6) + xlab("Number of Rushes") + ylab("Coefficient Estimate") + ggtitle("Ridge Regression Coefficient Estimates vs Number of Rushes") + theme_538
num_lme <- ggplot(runner_data, aes(Runs, LME_Value)) + geom_point(alpha = .6) + xlab("Number of Rushes") + ylab("Coefficient Estimate") + ggtitle("Mixed Model Coefficient Estimates vs Number of Rushes") + theme_538
lmm_ypa <- ggplot(runner_data, aes(LME_Value, YPA)) + geom_point(aes(size = Runs), alpha = .6) + xlab("Mixed Effects Model Coefficient Estimates") + ylab("Yards per Attempt") + ggtitle("Mixed Effects Model Estimates vs Yards per Attmept") + theme_538 + theme(legend.position = c(.75, .25))
ggplot(head(arrange(runner_data, desc(LME_Value - 1.96*LME_SE)), 20)) + geom_errorbarh(aes(x = LME_Value, xmin = LME_Value - 1.96*LME_SE, xmax = LME_Value + 1.96*LME_SE, y = reorder(paste0(First.Name, " ", Last.Name), LME_Value - 1.96*LME_SE))) 

top_10_runners <- runners %>% arrange(desc(LME_Value - 2*LME_SE)) %>% head(10) %>% ggplot(aes(x = LME_Value - LME_SE, y = 10:1)) + geom_text(aes(label = paste(First.Name, Last.Name))) + theme_538 + scale_x_continuous(limits = c(7.24, 8.4)) + ylab("") + ggtitle("Top 10 Runners in 2014") + xlab("Multilevel Model Coefficient Estimate") + scale_y_continuous(breaks = 10:1)

top_10_off <- teams %>% arrange(desc(LME_O_Value)) %>% head(10) %>% ggplot(aes(x = LME_O_Value, y = 10:1)) + geom_text()

off_ypa <- runs %>% group_by(Offense.Team.Code) %>% summarize(YPA = mean(Yards)) %>% left_join(teams, by = c("Offense.Team.Code" = "Team.Code")) %>% ggplot(aes(x = LME_O_Value, y = YPA)) + geom_point() + xlab("Multilevel Model Coefficient Estimate") + ylab("Yards per Carry") + ggtitle("Multilevel Model Estimate vs YPC\n   For Offenses") + theme_538
def_ypa <- runs %>% group_by(Defense.Team.Code) %>% summarize(YPA = mean(Yards)) %>% left_join(teams, by = c("Defense.Team.Code" = "Team.Code")) %>% ggplot(aes(x = LME_D_Value, y = YPA)) + geom_point() + xlab("Multilevel Model Coefficient Estimate") + ylab("Yards per Carry") + ggtitle("Multilevel Model Estimate vs YPC\n   For Defenses") + theme_538

