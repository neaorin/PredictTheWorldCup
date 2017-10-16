
# prepare the R environment

library("tidyverse")

library("dplyr") # Data munging functions
library("zoo")   # Feature engineering rolling aggregates

install.packages("data.table")
library("data.table") # Feature engineering

library("ggplot2") # Graphics
library("scales") # For time formatted axis

library("readr") # Reading input files
library("stringr") # String functions

install.packages("Amelia")
library("Amelia") # missing data eval

install.packages("randomForest")
library("randomForest")  # Random forests

install.packages("Metrics")
library(Metrics) # Eval metrics for ML

#load the matches data

if(!file.exists("matches.csv")){
    tryCatch(download.file('https://github.com/neaorin/PredictTheWorldCup/raw/master/input/matches.csv'
                           ,destfile="./matches.csv",method="auto"))
}
                
if(file.exists("matches.csv")) matches_original <- read_csv("matches.csv")

#eliminate any duplicates
matches <- matches_original %>%
  distinct(.keep_all = TRUE, date, team1, team2)

#format datetime
matches$date <- as.POSIXct(strptime(matches$date, "%Y%m%d"), origin="1960-01-01", tz="UTC")

head(matches)
summary(matches)

# what values is our dataset missing?
missmap(matches, main = "Missing values")

# compute some additional values about the games

# generate an id column for future use (joins etc)
matches$match_id = seq.int(nrow(matches))

# randomize team1 and team2 in the dataset to avoid bias in team1 being the home team (more likely to win than team2)
summary(matches$team1Score - matches$team2Score)

set.seed(4342)
matches$switch = runif(nrow(matches), min = 0, max = 1)

summary(matches$switch)

matches <- bind_rows(
  matches %>% filter(switch < 0.5),
  matches %>% filter(switch >= 0.5) %>%
    dplyr::mutate(
      x_team2 = team2,
      team2 = team1,
      team1 = x_team2,
      
      x_team2Text = team2Text,
      team2Text = team1Text,
      team1Text = x_team2Text,

      x_resText = "",
      
      x_team2Score = team2Score,
      team2Score = team1Score,
      team1Score = x_team2Score,
      
      x_team2PenScore = team2PenScore,
      team2PenScore = team1PenScore,
      team1PenScore = x_team2PenScore
    ) %>%
    dplyr::select(
      date, team1, team1Text, team2, team2Text, resText, statText, venue, IdCupSeason, CupName, team1Score, team2Score, team1PenScore, team2PenScore, match_id, switch
    )
    ) %>% 
  arrange(date) %>%
  dplyr::select(-c(switch))

summary(matches$team1Score - matches$team2Score)

# is the game played in a neutral venue
matches$team1Home <- mapply(grepl, pattern=matches$team1Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$team2Home <- mapply(grepl, pattern=matches$team2Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$neutralVenue <- !(matches$team1Home | matches$team2Home)

# text-matching the venue is not 100% accurate.
# some games get TRUE for both team1 and team2 (ex. Congo DR vs Congo)
# in this case, team1 is at home
matches$team2Home[(matches$team1Home == TRUE) & (matches$team2Home == TRUE)] <- FALSE

# game type: Friendly, Qualifier, Final Tournament
matches$friendly <- FALSE
matches$friendly[matches$CupName == "Friendly"] <- TRUE

matches$qualifier <- FALSE
matches$qualifier[matches$CupName %like% "Qual"] <- TRUE

matches$finaltourn <- FALSE
matches$finaltourn[matches$CupName %like% "Final"] <- TRUE
matches$finaltourn[matches$CupName %like% "Confederations Cup"] <- TRUE

# only use official matches (no friendlies)
matches <- matches %>% filter(friendly == FALSE)

# transform the matches table into a team performance table, where each team being 
# involved in a game is a separate observation (row)

teamperf <- bind_rows(
    (matches %>%
    mutate(
      name = team1,
      opponentName = team2,
      homeVenue = team1Home,
      neutralVenue = neutralVenue,
      gs = team1Score,
      ga = team2Score,
      gd = gs - ga,
      w = (team1Score > team2Score),
      l = (team1Score < team2Score),
      d = (team1Score == team2Score),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
    dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, friendly, qualifier, finaltourn))
    ,
    (matches %>%
      mutate(
      name = team2,
      opponentName = team1,
      homeVenue = team2Home,
      neutralVenue = neutralVenue,
      gs = team2Score,
      ga = team1Score,
      gd = gs - ga,
      w = (team1Score < team2Score),
      l = (team1Score > team2Score),
      d = (team1Score == team2Score),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
      dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, friendly, qualifier, finaltourn))
  ) %>%
  arrange(date)

head(teamperf)

# how many international games have been played over the years?
matches %>%
  ggplot(mapping = aes(year(date))) +
    geom_bar(aes(fill=CupName), width=1, color="black") +
    theme(legend.position = "bottom", legend.direction = "vertical")

# how many goals have been scored per game over the years? 
# how about the win margin (goal difference) over the years?
history_indicators <- matches %>%
  group_by(year = year(date)) %>%
  summarize(
    gs_pergame = mean(team1Score + team2Score),
    gd_pergame = mean(abs(team1Score - team2Score))
  ) %>%
  dplyr::select (year, gs_pergame, gd_pergame)

ggplot(history_indicators, mapping = aes(x = year, y = gs_pergame)) +
    geom_point() +
    geom_smooth(method = "loess")

# Out of the teams who have played at least 200 games, what are the winning percentages for each of those teams?
# We use the current point system of 3 points for a win, 1 point for a draw, 0 for a loss.

plot_winpercentage <- function(teamperf, mingames) {
  teamperf %>%
  group_by(name) %>%
  summarize(
    totalgames = n(),
    wins = length(w[w==TRUE]),
    draws = length(d[d==TRUE]),
    winpercentage = (wins*3 + draws) / (totalgames*3)
  ) %>%
  filter(totalgames >= mingames ) %>%
  ggplot(mapping = aes(x = winpercentage, y = totalgames)) +
  geom_point(size = 1.5) + 
  geom_text(aes(label=name), hjust=-.2 , vjust=-.2, size=3) +
  geom_vline(xintercept = .5, linetype = 2, color = "red") +
  expand_limits(x = c(0,1))
} 

plot_winpercentage(teamperf, 200)

# transform old country codes into new ones.
countryCodeMappings <- matrix(c(
  "FRG","GER",
  "TCH","CZE",
  "URS","RUS",
  "SCG","SRB",
  "ZAI","CGO"
  ), ncol=2, byrow = TRUE)

for (i in 1:nrow(countryCodeMappings)) {
  teamperf$name[teamperf$name == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  teamperf$opponentName[teamperf$opponentName == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  
  matches$team1[matches$team1 == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  matches$team2[matches$team2 == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
}

# let's run the win percentage graph again
plot_winpercentage(teamperf, 200)

# how about final tournaments only?
# the number of games played in final tournaments is much smaller than friendlies or qualifiers.

plot_winpercentage(teamperf %>% filter(finaltourn == TRUE), 50)

history_perf <- teamperf %>% 
  mutate(
    periodyearstart = year(date) - year(date) %% 5,
    periodyearend = periodyearstart + 4
  ) %>%
  group_by(name, periodyearstart, periodyearend) %>%
  summarize(
    totalgames = n(),
    wins = length(w[w==TRUE]),
    draws = length(d[d==TRUE]),
    winpercentage = (wins*3 + draws) / (totalgames*3)
  ) %>%
  ungroup() %>%
  filter(totalgames >= 10)

# what is the occurence frequency for match scores?

scorefreq <- matches %>%
  group_by(team1Score, team2Score) %>%
  summarise(
    n = n(),
    freq = n / nrow(matches)
  ) %>%
  ungroup() %>%
  mutate(
    scoretext = paste(team1Score,"-",team2Score)
  ) %>%
  arrange(desc(freq)) 

  head(scorefreq, 20)

  scorefreq %>%
  filter(freq >= 0.001) %>%
  
  ggplot(mapping = aes(x = reorder(scoretext, -freq), y = freq)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Let's calculate some lag indicators for each team which is about to play a game
# we'll take three windows: last 5 games, last 20 games, last 35 games.
# for each window we'll calculate win percentage, loss percentage, draw percentage, and goal differential

lagfn <- function(data, width) {
  return (rollapplyr(data, width = width + 1, FUN = sum, fill = NA, partial=TRUE) - data)
}

lagfn_per <- function(data, width) {
  return (lagfn(data, width) / width)
}

team_features <- teamperf %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(
    last5games_w_per = lagfn_per(w, 5),
    last20games_w_per = lagfn_per(w, 20),
    last35games_w_per = lagfn_per(w, 35),

    last5games_l_per = lagfn_per(l, 5),
    last20games_l_per = lagfn_per(l, 20),
    last35games_l_per = lagfn_per(l, 35),

    last5games_d_per = lagfn_per(d, 5),
    last20games_d_per = lagfn_per(d, 20),
    last35games_d_per = lagfn_per(d, 35),
            
    last5games_gd_per = lagfn_per(gd, 5),
    last20games_gd_per = lagfn_per(gd, 20),
    last35games_gd_per = lagfn_per(gd, 35)
  ) %>%
  dplyr::select (
    match_id, date, name, opponentName, gs, ga,
    w, last5games_w_per, last20games_w_per, last35games_w_per,
    l, last5games_l_per, last20games_l_per, last35games_l_per,
    d, last5games_d_per, last20games_d_per, last35games_d_per,
    gd, last5games_gd_per, last20games_gd_per, last35games_gd_per
          ) %>%
  ungroup()

# we compute a "strength of schedule" indicator to account for each team's quality of opponents faced
opp_strength <- function(match_id, team) {
  return (mean((as.data.frame(match_id) %>% 
                  left_join(teamperf, by = c("match_id")) %>%
                  dplyr::filter(name == team) %>%
                  mutate(periodyearstart = year(date) - year(date) %% 5) %>%
                  left_join(history_perf, by = c("opponentName" = "name", "periodyearstart")) %>%
                  dplyr::select(winpercentage))$winpercentage)
  )
}

team_features <- team_features %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(
    strength_of_schedule = rollapplyr(match_id, width = 20, fill = NA, partial=TRUE, FUN = opp_strength, name[1])
  ) %>%
  ungroup()

# change any missing data to 0 for strength of schedule
team_features$strength_of_schedule[is.na(team_features$strength_of_schedule)] <- 0

head(team_features, n = 50)
summary(team_features)

# fold per-team features into per-match features
match_features <- matches %>%
  left_join(team_features, by=c("match_id", "team1" = "name")) %>%
  left_join(team_features, by=c("match_id", "team2" = "name"), suffix=c(".t1",".t2")) %>%
  dplyr::select(
    date, match_id, team1, team2, team1Home, team2Home, neutralVenue, team1Score, team2Score, friendly, qualifier, finaltourn,
    last5games_w_per.t1,
    last20games_w_per.t1,
    last35games_w_per.t1,
    last5games_l_per.t1,
    last20games_l_per.t1,
    last35games_l_per.t1,
    last5games_d_per.t1,
    last20games_d_per.t1,
    last35games_d_per.t1,
    last5games_gd_per.t1, 
    last20games_gd_per.t1,
    last35games_gd_per.t1,
    last5games_w_per.t2,
    last20games_w_per.t2,
    last35games_w_per.t2,
    last5games_l_per.t2,
    last20games_l_per.t2,
    last35games_l_per.t2,
    last5games_d_per.t2,
    last20games_d_per.t2,
    last35games_d_per.t2,
    last5games_gd_per.t2, 
    last20games_gd_per.t2,
    last35games_gd_per.t2
  )

# add the outcome column (value to train / predict on)
#match_features <- left_join(match_features, scorefreq, by = c("team1Score", "team2Score")) %>%
#  dplyr::select(-c(n,freq,scoretext))
match_features$outcome = match_features$team1Score - match_features$team2Score

# drop all non-interesting columns, and those which should not be supplied for new data (like score)
match_features <- match_features %>%
  dplyr::select(-c(match_id,team1Score,team2Score))

# Model fitting
# just azure ML!

write.csv(match_features, "match_features.csv", row.names = TRUE)

# just upload the match_features.csv file into Azure ML and build your experiment"

# create the training formula 
trainformula <- as.formula(paste('outcome',
                                 paste(names(match_features %>% dplyr::select(-c(date,team1,team2,outcome))),collapse=' + '),
                                 sep=' ~ '))
trainformula

# training and testing datasets

data.train1 <- match_features %>% filter(date >= '1960/1/1' & date < '2010/1/1')
data.test1 <- match_features %>% filter(date >= '2010/1/1')

# Random forest

set.seed(4342)

model.randomForest1 <- randomForest::randomForest(trainformula, data = data.train1, 
                                                  importance = TRUE, ntree = 100)

randomForest::varImpPlot(model.randomForest1)

data.pred.randomForest1 <- predict(model.randomForest1, data.test1, predict.all = TRUE)


# compute evaluation metrics 

metrics.randomForest1.mae <- Metrics::mae(data.test1$outcome, data.pred.randomForest1$aggregate)
metrics.randomForest1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.randomForest1$aggregate)

metrics.condForest1.mae <- Metrics::mae(data.test1$outcome, data.pred.condForest1)
metrics.condForest1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.condForest1)

metrics.nn1.mae <- Metrics::mae(data.test1$outcome, data.pred.nn1)
metrics.nn1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.nn1)

R2 <- function(actual, predicted)
  1 - sum((actual-predicted)^2)/sum((actual-mean(actual))^2)

R2(data.test1$outcome, data.pred.randomForest1$aggregate)
R2(data.test1$outcome, data.pred.condForest1)
R2(data.test1$outcome, data.pred.nn1)
  


# qualified teams - calculate input dataset

#load the qualified teams data

if(!file.exists("qualified.csv")){
  tryCatch(download.file('https://raw.githubusercontent.com/neaorin/PredictTheWorldCup/master/src/TournamentSim/wc2018qualified.csv'
                         ,destfile="./qualified.csv",method="auto"))
}

if(file.exists("qualified.csv")) qualified <- read_csv("qualified.csv")

head(qualified, 32)

data.topredict <- expand.grid(team1 = qualified$name, team2 = qualified$name, stringsAsFactors = FALSE) %>% filter(team1 < team2)

temp <- teamperf %>%
  semi_join(qualified, by = c("name")) %>%
  group_by(name) %>%
  summarise(
    date = max(date)
  )

temp <- team_features %>%
  semi_join(temp, by = c("name", "date"))

data.topredict <- data.topredict %>%
  left_join(temp, by = c("team1" = "name")) %>%
  left_join(temp, by = c("team2" = "name"), suffix = c(".t1", ".t2")) %>%
  dplyr::select(
    team1, team2,
    last5games_w_per.t1,
    last20games_w_per.t1,
    last35games_w_per.t1,
    last5games_l_per.t1,
    last20games_l_per.t1,
    last35games_l_per.t1,
    last5games_d_per.t1,
    last20games_d_per.t1,
    last35games_d_per.t1,
    last5games_gd_per.t1, 
    last20games_gd_per.t1,
    last35games_gd_per.t1,
    last5games_w_per.t2,
    last20games_w_per.t2,
    last35games_w_per.t2,
    last5games_l_per.t2,
    last20games_l_per.t2,
    last35games_l_per.t2,
    last5games_d_per.t2,
    last20games_d_per.t2,
    last35games_d_per.t2,
    last5games_gd_per.t2, 
    last20games_gd_per.t2,
    last35games_gd_per.t2
  ) %>%
  mutate(
    date = as.POSIXct("2018-06-14"), 
    team1Home = (team1 == "RUS"), team2Home = (team2 == "RUS"), neutralVenue = !(team1Home | team2Home), 
    friendly = FALSE, qualifier = FALSE, finaltourn = TRUE
  )

write.csv(data.topredict, "match_features_wc2018.csv",row.names = TRUE)

# ---------- #

match_predictions_wc2018 <- read_csv("match_predictions_wc2018.csv")

temp <- match_predictions_wc2018 %>% dplyr::select(team1, team2, outcome = "Scored Label Mean", sd = "Scored Label Standard Deviation")

write_csv(temp, 'match_predictions_wc2018_static.csv')


data.predicted$sd = apply(data.predicted$individual, c(1), sd)

data.staticpred <- data.topredict %>% 
  dplyr::mutate(
    outcome = data.predicted$aggregate,
    sd = data.predicted$sd
    ) %>%
  dplyr::select(team1, team2, outcome, sd)

head(data.staticpred)

