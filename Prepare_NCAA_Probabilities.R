##########
# Prepare NCAA probability data for simulation
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

  # # Probabilities are taken from a kagge user kernel located at:
  # # elo_probs <- read_csv(url("https://www.kaggleusercontent.com/kf/2593684/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..jyN3-8es9ElgAMnGGr7ihQ.bG1HDZ36ahe4kJkt4GqX8-kCG8bm-OHWELhVzFalry3dQ56kSQtmoJcoYW4zPO18aJDFGQEUW9HqXwx14fvRHu79sy34Ek3vThY_wAsO7EdKsmAdOTlf_SpvU7Ahf5Lu1RcSpRy6Xb--MpzKIdFwkw.k2n5KyKDj2BI9T6HevzNJA/EloBenchmark.csv"))
  # elo_probs <- read_csv("DataFiles/EloBenchmark.csv")
  # 
  # # Team ID to team names were downloaded from the kaggle competition site
  # team_names <- read_csv("DataFiles/Teams.csv")

  
#---
# Function that takes the output from the 2018 NCAA Kaggle Competition
# and generates a probability matrix for a specific year
#---
generate_prob.mat <- function(prob_table,year_in) {
  prob_table <- prob_table %>%
    separate(ID,c("year","team1","team2"))
  
  prob_year <- subset(prob_table,year == year_in)
  
  prob_year <- 
    tibble(team1 = prob_year$team1, 
           team2 = prob_year$team2,
           pred = prob_year$Pred) %>% 
    rbind(tibble(team1 = prob_year$team2, 
                 team2 = prob_year$team1,
                 pred = 1 - prob_year$Pred))
  
  # Each team combination should only appear once. 
  if(sum(duplicated(prob_year)) > 0) warning("Duplicate team pairs found")
  
  # Set up the left column to have each team to compare to
  prob_matrix_year <- tibble(team = unique(prob_year$team1))
  
  # Create a column for each team and join predictions
  for(team in unique(prob_year$team1)) {
    prob_matrix_year <- prob_matrix_year %>%
      full_join(select(subset(prob_year,team1 == team),-team1), 
                by = c("team" = "team2"))
    # The most recent column needs to be renamed
    colnames(prob_matrix_year)[length(colnames(prob_matrix_year))] <- team
  }
  
  return(prob_matrix_year)
}

#---
# Function that takes NCAATrouneySeeds and outputs in format needed
#---
parse_tourney_seeds <- function(in_seeds, year_in) {
  out_bracket <- in_seeds %>% 
    filter(Season == year_in) %>%
    select(TeamID)
  return(out_bracket)
}
