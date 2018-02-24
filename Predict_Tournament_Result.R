##########
# This file contains a function to predict the outcome of the 
# NCAA tournament one time. It will return the predicted winner
# of the tournament based on random chance and the probability
# matrix input for each pairwise team result.
##########

require(data.table)
  
#---
# Begin predict_champion(matches.dt,prob.mat.dt) function.
# matches.dt is a file of team IDs organized such that
#   matches.dt[1] plays the first game against matches.dt[2]
#   and the winner of {1,2} plays against {3,4} and so on.
#
# Example bracket: matches <- data.table(teams = c('1112','1116','1124','1137'))
#---

predict_champion <- function(matches.dt, prob.mat.dt) {
  # Make a copy of the input of matches and coerce it to a data.table
  # Also, make each column a team name in the tournament for easy and
  # memory efficient deletion using the data.table methods
  dt <- as.data.table(transpose(matches.dt))
  
  # Make a copy of prob.mat and coerce to data.table for consistency
  prob.mat.dt <- as.data.table(prob.mat.dt)
  
  
  # While there are 2 or more teams in dt, caculate the results of the next round
  while (length(dt)>1) {
    # For each pair in the table of matches, predict the winner of the pair
    # and then remove the loser from the table. Since we are deleting half
    # of the columns as we go, we increment n by 1 and only go through
    # length/2 columns
    for(n in 1:(length(dt)/2)) {
      if(runif(n=1,min=0,max=1) < prob.mat.dt[prob.mat.dt$team == dt[,2][[1]],get(dt[,1][[1]])]){
        # Team 1 wins, so remove column for team 2
        colname <- names(dt)[n+1]
        dt[,(colname) := NULL]
      } else {
        # Team 2 Wins, so remove column for team 1
        colname <- names(dt)[n]
        dt[,(colname) := NULL]
      }
    }
    rm(n,colname) 
  }
  
  # Return the result
  return(dt[[1]])
}


