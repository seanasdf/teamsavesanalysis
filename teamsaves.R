library(tidyverse)
library(Lahman)

#####################################################
########## Read in historical team data #############
#####################################################

savehistory <- filter(Teams, yearID>=1970) %>%
  mutate(rundiff = (R-RA)/G,
         runs_pergame = R/G,
         rallowed_pergame = RA/G) %>%
  select(yearID, teamID, rundiff, runs_pergame, rallowed_pergame, SV) %>%
  setNames(c("year", "team","rundifferential","runs_pergame","rallowed_pergame", "saves"))

#####################################################
########## Read in all Game logs 1970-2016 ########## 
#####################################################

if (!(file.exists("gamelogs.rda"))) {
  #Read in all of the game logs and combine into a single data frame
  gamelogs <- list.files("./gamelog") %>%
  map(function(x) paste("./gamelog/", x, sep="")) %>%
  map(read_csv, col_names=FALSE) %>%
  reduce(rbind)
  
  library(lubridate)
  #pull out only data we care about
  gamelogs <- select(gamelogs,
                     X1, #date
                     X4, #visiting team
                     X7, #home team
                     X10, #visiting team score,
                     X11, #home team score
                     X98, #saving pitcher id
                     X99 #saving pitcher name
  ) %>%
    setNames(c("date","visitor","home","vis_score","home_score","sv_p_id","sv_p_name")) %>%
    mutate(saved = ifelse(!is.na(sv_p_id), 1, 0),
         year = year(ymd(date)),
         winner = ifelse(vis_score>home_score, visitor, home)
    )
  
  save(gamelogs, file="gamelogs.rda")
} else {
  load("gamelogs.rda")
}

#build game log data set from home team's perspective
home_team <- select(gamelogs, date, year, home, home_score, visitor, vis_score, saved, winner) %>%
  rename(team = home, runs_scored = home_score, runs_allowed = vis_score, opponent = visitor) %>%
  mutate(saved = ifelse(saved == 1 & winner==team, 1, 0),
         location = "home")

#build game log data set from visiting team's perspective
visiting_team <- select(gamelogs, date, year, visitor, vis_score, home, home_score, saved, winner) %>%
  rename(team = visitor, runs_scored = vis_score, runs_allowed = home_score, opponent = home) %>%
  mutate(saved = ifelse(saved == 1 & winner==team, 1, 0),
         location = "away")

#combine home and away datasets into one
saves_dataset <- rbind(visiting_team, home_team) %>%
  left_join(savehistory, by=c("year" = "year", "team" = "team")) %>%
  left_join(savehistory, by=c("year" = "year", "opponent" = "team"), suffix = c("_team", "_oppo")) %>%
  filter(!is.na(rundifferential_team) & !is.na(rundifferential_oppo))

blah <- lm(saved ~ runs_pergame_team + rallowed_pergame_team + rundifferential_oppo + runs_pergame_oppo, 
           saves_dataset)