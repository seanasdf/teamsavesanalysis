library(tidyverse)
library(Lahman)

# teamsavehistory <- filter(Teams, yearID>=1973) %>%
#   mutate(rundiff = (R-RA)/G,
#          runs_game = R/G,
#          runsallowed_game = RA/G)


#####################################################
########## Read in all Game logs 1970-2016 ########## 
#####################################################

#Read in all of the game logs and combine into a single data frame
gamelogs <- list.files("./gamelog") %>%
  map(function(x) paste("./gamelog/", x, sep="")) %>%
  map(read_csv, col_names=FALSE) %>%
  reduce(rbind)


#pull out only data we care about
savesdataset <- select(gamelogs,
                       X1, #date
                       X4, #visiting team
                       X7, #home team
                       X10, #visiting team score,
                       X11, #home team score
                       X98, #saving pitcher id
                       X99, #saving pitcher name
)
