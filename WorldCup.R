
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

# is the game played in a neutral venue
matches$team1Home <- mapply(grepl, pattern=matches$team1Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$team2Home <- mapply(grepl, pattern=matches$team2Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$neutralVenue <- !(matches$team1Home | matches$team2Home)

temp <- matches %>% select(date, team1Text, team2Text, venue, team1Home, team2Home, neutralVenue)

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

# how many goals have been scored per game over the years? 
matches %>%
  group_by(year = year(date)) %>%
  summarize(
    totalgames = n(),
    totalgoals = sum(team1Score + team2Score),
    goalspergame = totalgoals / totalgames
    ) %>%
  ggplot(mapping = aes(x = year, y = goalspergame)) +
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
}

# let's run the win percentage graph again
plot_winpercentage(teamperf, 300)

# what if we look only at official matches (excluding friendlies)?

plot_winpercentage(teamperf %>% filter(friendly == FALSE), 150)

# how about final tournaments only?
# the number of games played in final tournaments is much smaller than friendlies or qualifiers.

plot_winpercentage(teamperf %>% filter(finaltourn == TRUE), 0)

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

features <- teamperf %>%
  arrange(name, date) %>%
  group_by(name) %>%
  mutate(
    last5games_w = lagfn(w, 5),
    last20games_w = lagfn(w, 20),
    last35games_w = lagfn(w, 35),

    last5games_l = lagfn(l, 5),
    last20games_l = lagfn(l, 20),
    last35games_l = lagfn(l, 35),
        
    last5games_gd_per = lagfn_per(gd, 5),
    last20games_gd_per = lagfn_per(gd, 20),
    last35games_gd_per = lagfn_per(gd, 35)
  ) %>%
  dplyr::select (
    match_id, date, name, opponentName, gs, ga,
    w, last5games_w, last20games_w, last35games_w,
    l, last5games_l, last20games_l, last35games_l,
    gd, last5games_gd_per, last20games_gd_per, last35games_gd_per
          ) %>%
  ungroup()

head(features, n = 50)
summary(features)

# calculate the match outcome as a value inside (0,1)
# arrange the possible outcomes so that, the closer the outcome is to either 0 or 1, the more lopsided the win/loss was
# and the closer to 0.5, the more tight the result, and the chance of a draw

scorefreq <- scorefreq %>%
  arrange(
    team1Score - team2Score,
    (team1Score + team2Score) * sign(team1Score - team2Score)
  ) %>%
  within(outcome <- cumsum(freq))

scorefreq %>%
  filter(freq >= 0.001) %>%
  ggplot(mapping = aes(x = reorder(scoretext, outcome), y = outcome)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# outcome is skewed to the left (draw values are between 0.26 and 0.49) because the home team is more likely to win
scorefreq %>% filter(team1Score - team2Score == 0)


# fold per-team features into per-match features
match_features <- matches %>%
  left_join(features, by=c("match_id", "team1" = "name")) %>%
  left_join(features, by=c("match_id", "team2" = "name"), suffix=c(".t1",".t2")) %>%
  dplyr::select(
    date, match_id, team1, team2, team1Home, team2Home, neutralVenue, team1Score, team2Score, friendly, qualifier, finaltourn,
    last5games_w.t1,
    last20games_w.t1,
    last35games_w.t1,
    last5games_l.t1,
    last20games_l.t1,
    last35games_l.t1,
    last5games_gd_per.t1, 
    last20games_gd_per.t1,
    last35games_gd_per.t1,
    last5games_w.t2,
    last20games_w.t2,
    last35games_w.t2,
    last5games_l.t2,
    last20games_l.t2,
    last35games_l.t2,
    last5games_gd_per.t2, 
    last20games_gd_per.t2,
    last35games_gd_per.t2
  )

# add the outcome column (value to train / predict on)
#match_features <- left_join(match_features, scorefreq, by = c("team1Score", "team2Score")) %>%
#  dplyr::select(-c(n,freq,scoretext))
match_features$outcome = match_features$team1Score - match_features$team2Score

# drop all non-feature columns
match_features <- match_features %>%
  dplyr::select(-c(match_id,team1,team2,team1Score,team2Score))

# Model fitting
# just azure ML!

write.csv(match_features, "match_features.csv", row.names = TRUE)

# just upload the match_features.csv file into Azure ML and build your experiment"

# just use the FIFA WC 2014 games for quick testing
match_features %>% filter(date >= "2014-06-12" & date <= "2014-07-13" & finaltourn == TRUE) %>%
  write.csv("match_features_wc2014.csv",row.names = TRUE)

