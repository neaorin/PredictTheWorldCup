
# prepare the R environment

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

install.packages("party")
library(party) # Recursive partitioning

install.packages("https://cran.rstudio.com/bin/windows/contrib/3.4/coin_1.2-1.zip", 
                repos = NULL, type="source") 
library(coin) # Conditional Inference Procedures

install.packages("neuralnet")
library(neuralnet) # Neural networks

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
matches$switch = runif(nrow(temp), min = 0, max = 1)

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
      pen_w = (!is.na(team1PenScore) & (team1PenScore > team2PenScore)),
      pen_l = (!is.na(team2PenScore) & (team1PenScore < team2PenScore)),
      aet = (statText == "AET"),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
    dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, pen_w, pen_l, aet, friendly, qualifier, finaltourn))
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
      pen_w = (!is.na(team2PenScore) & (team1PenScore < team2PenScore)),
      pen_l = (!is.na(team1PenScore) & (team1PenScore > team2PenScore)),
      aet = (statText == "AET"),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
      dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, pen_w, pen_l, aet, friendly, qualifier, finaltourn))
  ) %>%
  arrange(date)

head(teamperf)

# how many international games have been played over the years?
matches %>%
  ggplot(mapping = aes(year(date))) +
    geom_bar(aes(fill=CupName), width=1, color="black") +
    theme(legend.position = "bottom", legend.direction = "vertical")

# how about games for which we don't have the venue recorded?
matches %>%
  filter(is.na(venue)) %>%
  ggplot(mapping = aes(year(date))) +
  geom_bar(aes(fill=CupName), width=1, color="black") +
  theme(legend.position = "bottom", legend.direction = "vertical")

matches %>%
  filter(is.na(venue)) %>%
  group_by(CupName) %>%
  summarise(
    total = n()
  )

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

ggplot(history_indicators, mapping = aes(x = year, y = gd_pergame)) +
  geom_point() +
  geom_smooth(method = "loess")

# Out of the teams who have played at least 300 games, what are the winning percentages for each of those teams?
# We use the current point system of 3 points for a win, 1 point for a draw, 0 for a loss.

plot_winpercentage <- function(teamperf, mingames) {
  teamperf %>%
  group_by(name) %>%
  summarize(
    totalgames = n(),
    wins = length(w[w==TRUE]) + length(pen_w[pen_w==TRUE]),
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

plot_winpercentage(teamperf, 300)

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
plot_winpercentage(teamperf, 300)

# what if we look only at official matches (excluding friendlies)?

plot_winpercentage(teamperf %>% filter(friendly == FALSE), 150)

# how about final tournaments only?
# the number of games played in final tournaments is much smaller than friendlies or qualifiers.

plot_winpercentage(teamperf %>% filter(finaltourn == TRUE), 10)

perf_in_finals <- teamperf %>% 
  filter(finaltourn == TRUE) %>%
  group_by(name) %>%
  summarize(
    totalgames = n(),
    wins = length(w[w==TRUE]) + length(pen_w[pen_w==TRUE]),
    draws = length(d[d==TRUE]),
    winpercentage = (wins*3 + draws) / (totalgames*3)
  ) %>%
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
# for each window we'll calculate some values

lagfn <- function(data, width) {
  return (rollapplyr(data, width = width + 1, FUN = sum, fill = NA, partial=TRUE) - data)
}

lagfn_per <- function(data, width) {
  return (lagfn(data, width) / width)
}

opp_strength <- function(teams) {
  return (mean((perf_in_finals %>% filter(name %in% teams))$winpercentage))
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
    last35games_gd_per = lagfn_per(gd, 35),
    
    strength_of_schedule = rollapplyr(opponentName, width = 21, FUN = opp_strength, fill = NA, partial=TRUE)
  ) %>%
  dplyr::select (
    match_id, date, name, opponentName, gs, ga,
    w, last5games_w_per, last20games_w_per, last35games_w_per,
    l, last5games_l_per, last20games_l_per, last35games_l_per,
    d, last5games_d_per, last20games_d_per, last35games_d_per,
    gd, last5games_gd_per, last20games_gd_per, last35games_gd_per,
    strength_of_schedule
          ) %>%
  ungroup()

# change any missing data to 0 for strength of schedule
team_features$strength_of_schedule[is.na(team_features$strength_of_schedule)] <- 0

head(features, n = 50)
summary(features)

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
    strength_of_schedule.t1,
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
    last35games_gd_per.t2,
    strength_of_schedule.t2
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

# just use the FIFA WC 2014 games for quick testing
match_features %>% filter(date >= "2014-06-12" & date <= "2014-07-13" & finaltourn == TRUE) %>%
  write.csv("match_features_wc2014.csv",row.names = TRUE)

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

data.pred.randomForest1 <- predict(model.randomForest1, data.test1)

# Conditional inference trees

set.seed(4342)

model.condForest1 <- party::cforest(trainformula, data = data.train1, 
                                    controls = party::cforest_unbiased(ntree = 100, mtry = 7))

party::varimp(model.condForest1)

data.pred.condForest1 <- predict(model.condForest1, data.test1, OOB = TRUE, type = "response")


# neural networks
# TODO
# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

# drop all non-numerical features
match_features_scaled <- match_features %>%
  dplyr::select(c(8:32))

maxs <- apply(match_features[c(8:32)], 2, max) 
mins <- apply(match_features[c(8:32)], 2, min)

data.train1.nn.scaled <- as.data.frame(
  scale(data.train1[c(8:32)], center = mins, scale = maxs - mins))
data.test1.nn.scaled <- as.data.frame(
  scale(data.test1[c(8:32)], center = mins, scale = maxs - mins))

formula_nn <- as.formula(paste('outcome',
                               paste(names(match_features)[c(8:31)],collapse=' + '),
                               sep=' ~ '))



formula_nn

model.nn1 <- neuralnet(formula_nn, data = data.train1.nn.scaled, hidden = c(9,3), linear.output = TRUE)

# http://dstath.users.uth.gr/papers/IJRS2009_Stathakis.pdf
# optimal number of hidden nodes in the first hidden layer is: sqrt[(m+2)N] + 2sqrt[N/(m+2)] 
# and in the second hidden layer, the optimal number of hidden nodes is: m*sqrt[N/(m+2)], 
# where N = number of inputs, and m = number of outputs. 

plot(model.nn1)

data.pred.nn1.scaled <- neuralnet::compute(model.nn1, data.test1.nn[c(1:24)])
data.pred.nn1 <- data.pred.nn1.scaled$net.result * (maxs["outcome"] - mins["outcome"]) + mins["outcome"]


# compute evaluation metrics 

metrics.randomForest1.mae <- Metrics::mae(data.test1$outcome, data.pred.randomForest1)
metrics.randomForest1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.randomForest1)

metrics.condForest1.mae <- Metrics::mae(data.test1$outcome, data.pred.condForest1)
metrics.condForest1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.condForest1)

metrics.nn1.mae <- Metrics::mae(data.test1$outcome, data.pred.nn1)
metrics.nn1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.nn1)

R2 <- function(actual, predicted)
  1 - sum((actual-predicted)^2)/sum((actual-mean(actual))^2)

R2(data.test1$outcome, data.pred.randomForest1)
R2(data.test1$outcome, data.pred.condForest1)
R2(data.test1$outcome, data.pred.nn1)
  


# qualified teams - calculate input dataset


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
    strength_of_schedule.t1,
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
    last35games_gd_per.t2,
    strength_of_schedule.t2
  ) %>%
  mutate(
    date = as.POSIXct("2017-01-01"), 
    team1Home = FALSE, team2Home = FALSE, neutralVenue = FALSE, friendly = FALSE, qualifier = FALSE, finaltourn = TRUE
  )

write.csv(data.topredict, "match_features_wc2018.csv",row.names = TRUE)

# ---------- #

match_predictions_wc2018 <- read_csv("match_predictions_wc2018.csv")

temp <- match_predictions_wc2018 %>% dplyr::select(team1, team2, outcome = "Scored Label Mean", sd = "Scored Label Standard Deviation")


temp <- temp %>% group_by(team1) %>% summarise(outcome_m = mean(outcome), sd_m = mean(sd)) %>% arrange(outcome_m)

temp$team1wins <- NA
temp$draw <- NA
temp$team2wins <- NA

gd_draw_threshold <- 0.2745

for (i in 1:nrow(temp)) {
  normd <- rnorm(100, temp[i,]$outcome)
  temp[i,]$team1wins <- 0.01 * length(normd[normd > gd_draw_threshold])
  temp[i,]$team2wins <- 0.01 * length(normd[normd < -gd_draw_threshold])
  temp[i,]$draw <- 0.01 * length(normd[abs(normd) <= gd_draw_threshold])
}

sum(temp$draw) / (sum(temp$team1wins) + sum(temp$team2wins))


# code for transforming population data

library(readr)
API_SP_POP_TOTL_DS2_en_csv_v2 <- read_csv("~/API_SP.POP.TOTL_DS2_en_csv_v2.csv")

cols <- c("Country Name","Country Code","Indicator Name","Indicator Code","pop.1960","pop.1961","pop.1962","pop.1963","pop.1964","pop.1965","pop.1966","pop.1967","pop.1968","pop.1969","pop.1970","pop.1971","pop.1972","pop.1973","pop.1974","pop.1975","pop.1976","pop.1977","pop.1978","pop.1979","pop.1980","pop.1981","pop.1982","pop.1983","pop.1984","pop.1985","pop.1986","pop.1987","pop.1988","pop.1989","pop.1990","pop.1991","pop.1992","pop.1993","pop.1994","pop.1995","pop.1996","pop.1997","pop.1998","pop.1999","pop.2000","pop.2001","pop.2002","pop.2003","pop.2004","pop.2005","pop.2006","pop.2007","pop.2008","pop.2009","pop.2010","pop.2011","pop.2012","pop.2013","pop.2014","pop.2015","pop.2016","X62")
colnames(API_SP_POP_TOTL_DS2_en_csv_v2) <- cols

# transformation
yrnames <- names(API_SP_POP_TOTL_DS2_en_csv_v2 %>% select(c(5:61)))
API_SP_POP_TOTL_DS2_en_csv_v2 <- rbind(API_SP_POP_TOTL_DS2_en_csv_v2, c("England", "ENG", "Population.total", "SP.POP.TOTL"))

population <- stats::reshape(as.data.frame(API_SP_POP_TOTL_DS2_en_csv_v2), idvar = "Country Code",
        varying = as.vector(5:61), direction = "long")

temp <- matches %>%
  mutate (year = year(date)) %>%
  left_join(population, by = c("team1" = "Country Code", "year" = "time")) 


temp <- matches %>%
  distinct(team1, team1Text) %>%
  anti_join(API_SP_POP_TOTL_DS2_en_csv_v2, by = c("team1" = "Country Code")) %>%
  anti_join(API_SP_POP_TOTL_DS2_en_csv_v2, by = c("team1Text" = "Country Name"))
  

