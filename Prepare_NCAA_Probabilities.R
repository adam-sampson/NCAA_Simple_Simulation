##########
# Import and prepare NCAA probability data for simulation
# Probabilities are from wacax's kernel submission located
# at https://www.kaggle.com/wacaxx/ncaa-elo-based-submission-2018/code
##########

require(readr)
require(stringr)
require(tidyverse)
# require(data.table)

#---
# Import Data
#---

  # Probabilities are taken from a kagge user kernel located at:
  elo_probs <- read_csv(url("https://www.kaggleusercontent.com/kf/2593684/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..jyN3-8es9ElgAMnGGr7ihQ.bG1HDZ36ahe4kJkt4GqX8-kCG8bm-OHWELhVzFalry3dQ56kSQtmoJcoYW4zPO18aJDFGQEUW9HqXwx14fvRHu79sy34Ek3vThY_wAsO7EdKsmAdOTlf_SpvU7Ahf5Lu1RcSpRy6Xb--MpzKIdFwkw.k2n5KyKDj2BI9T6HevzNJA/EloBenchmark.csv"))
  
  # Team ID to team names were downloaded from the kaggle competition site
  team_names <- read_csv("DataFiles/Teams.csv")
  
  
#---
# Perpare elo_probabilities for simulations and lookups
#---
  elo_probs <- elo_probs %>%
    separate(ID,c("year","team1","team2"))
  
  # For this assignment we only want to deal with 2017 data
  elo_2017 <- subset(elo_probs,year == '2017')
  rm(elo_probs)
  
  # Team names can appear in either the left or right column.
  # Ensure that each team is in the left column for all of its pairings
  elo_2017 <- 
    tibble(team1 = elo_2017$team1, 
           team2 = elo_2017$team2,
           pred = elo_2017$Pred) %>% 
    rbind(tibble(team1 = elo_2017$team2, 
                 team2 = elo_2017$team1,
                 pred = 1 - elo_2017$Pred))
  
  # Each team combination should only appear once. 
  if(sum(duplicated(elo_2017)) > 0) warning("Duplicate team pairs found")
  
  # Set up the left column to have each team to compare to
  elo_matrix_2017 <- tibble(team = unique(elo_2017$team1))
  
  # Create a column for each team and join predictions
  for(team in unique(elo_2017$team1)) {
    elo_matrix_2017 <- elo_matrix_2017 %>%
      full_join(select(subset(elo_2017,team1 == team),-team1), 
                by = c("team" = "team2"))
    # The most recent column needs to be renamed
    colnames(elo_matrix_2017)[length(colnames(elo_matrix_2017))] <- team
  }
  
  rm(team)
  rm(elo_2017)
  
##########
# Use matrix lookup for further simulation in other files with
# source("Prepare_NCAA_Probabilities.R")
##########
