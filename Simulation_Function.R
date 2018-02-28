



################## TEST PARAMETERS #####################################

    load(file =  "sub.Rdata",  envir = environment())
    #sub <- data.table(id = sub$ID, pred = sub$Pred)
    #save(sub, file = "sub.Rdata")
    # N = 100
    # year = 2017
    # upset_bias = 0
########################################################################
    

#   ____________________________________________________________________________
#   Clean Predictions Data   (use only if needs to be split sub.Rdata is already split)
    
    sub$id <-  strsplit(sub$id, "_")
    sub$season <-    as.integer(sapply(sub$id, "[", 1))
    sub$team_1 <-  as.integer(sapply(sub$id, "[", 2))
    sub$team_2 <- as.integer(sapply(sub$id, "[", 3))
    sub <- sub[,-1]
#   ____________________________________________________________________________
#   Function to run simulation                                              ####
    simulation <- function (probs, N = 1000, year = 2017,  upset_bias = 0) 
    {
        
    load(file =  "all_slots.Rdata",  envir = environment())
    probs <- probs[season == year, ]
    
    #Join slots to the predictions
    n1 <- nrow(probs)
    probs <- merge(probs, all_slots, by = c("season", "team_1", "team_2"))
    
#   Stopifnot(n1 == nrow(probs))
#   Determine seeds
    probs$seed_1_int <- as.integer(substr(probs$seed_1, 2, 3))
    probs$seed_2_int <- as.integer(substr(probs$seed_2, 2, 3))
#   Add some columns for tracking the simulation    
    probs$rand <- stats::runif(.N)
    probs$winner <- ifelse(probs$pred > probs$rand, "team_1", "team_2")
    probs$keep <- 1L
    
#   Add Upset Bias - upset bias will give close games to the underdog   
    if (upset_bias != 0) {
        
        probs$rand <- ifelse(probs$seed_1_int > probs$seed_2_int,  probs$rand - upset_bias, probs$rand - upset_bias)
       
    }
 
#   Run the simulation   
    sims_list <- lapply(1:N, function(x) {
        probs$rand <- stats::runif(.N)
        if (upset_bias != 0) {
            probs$rand <- ifelse(probs$seed_1_int > probs$seed_2_int,  probs$rand - upset_bias, probs$rand - upset_bias)
        }
        
        probs$winner <- ifelse(probs$pred > probs$rand, probs$team_1, probs$team_2)
        internal_sim(probs)
    })
#   Aggregate results   
    sims <- data.table::rbindlist(sims_list)
    data.table::setkeyv(sims, c("slot", "winner"))
    sims <- sims[, list(count = .N), by = c("slot", "winner")]
    sims[, `:=`(count, count + stats::runif(.N)/1e+12)]
    sims$count <- sims$count + (stats::runif(.N)/1e+12)
    
#       
    all_possible_slots_team_1 <- all_slots[season == year, list(slot, winner = team_1)]
    all_possible_slots_team_2 <- all_slots[season == year, list(slot, winner = team_2)]
    all_possible_slots <- unique(rbind(all_possible_slots_team_1, all_possible_slots_team_2))
    
    sims <- merge(all_possible_slots, sims, all.x = TRUE, by = c("slot", "winner"))
    
    sims$count[is.na(sims$count)] <- 0
    sims$prob <- sims$count/N
    sims <- sims[order(count, decreasing = TRUE)]
#   Add Year    
    sims$season <- year
    return(sims)
}





internal_sim <- function (probs) 
{
    all_rounds <- sort(unique(probs$round))
    
#   Evaluate the playin rounds    
    if (all_rounds[1] == 0) {
        r <- 0L
        round_team_1 <- probs[round == r, list(slot = next_slot, 
                                               team_1 = winner, keep_team_1 = 1L)]
        round_team_2 <- probs[round == r, list(slot = next_slot, 
                                               team_2 = winner, keep_team_2 = 1L)]
        probs <- merge(probs, round_team_1, by = c("slot", "team_1"), 
                       all.x = TRUE)
        probs <- merge(probs, round_team_2, by = c("slot", "team_2"), 
                       all.x = TRUE)
        probs[is.na(keep_team_1) & team_1_playedin == (r + 1L) & 
                  round == 1L, `:=`(keep, 0L)]
        probs[is.na(keep_team_2) & team_2_playedin == (r + 1L) & 
                  round == 1L, `:=`(keep, 0L)]
        probs <- probs[keep == 1L, ]
        probs[, `:=`(c("keep_team_1", "keep_team_2"), NULL)]
        all_rounds <- all_rounds[2:length(all_rounds)]
    }
#   Evaluate the regular rounds    
    for (r in 1:5) {
        round_team_1 <- probs[round == r, list(slot = next_slot, 
                                               team_1 = winner, keep_team_1 = 1L)]
        round_team_2 <- probs[round == r, list(slot = next_slot, 
                                               team_2 = winner, keep_team_2 = 1L)]
        probs <- merge(probs, round_team_1, by = c("slot", "team_1"), 
                       all.x = TRUE)
        probs <- merge(probs, round_team_2, by = c("slot", "team_2"), 
                       all.x = TRUE)
        probs[is.na(keep_team_1) & round == (r + 1L), `:=`(keep, 
                                                           0L)]
        probs[is.na(keep_team_2) & round == (r + 1L), `:=`(keep, 
                                                           0L)]
        probs <- probs[keep == 1L, ]
        probs[, `:=`(c("keep_team_1", "keep_team_2"), NULL)]
    }
    probs <- probs[, list(slot, round, team_1, team_2, winner)]
    data.table::setkeyv(probs, "slot")
    return(probs)
}





#   Extract a best bracket from a simulation result
#   Given the results of simTourney, this function pulls out a
#   single tournament result.

#   This function starts with the most likely winner, and then assumes
#   they won all prior games.  It then picks the most likely other team in the
#   championship game, and assumes that team won all prior games.  As such, it
#   works backwards to determine a single result from a simulation.

Create_Bracket <- function(sim){
  
  #Make a deep copy, so we don't update the original data
  dat <- data.table::copy(sim)
  dat[, slot_int := as.integer(slot)]
  
  #Walk backwards from the championship and choose a single tournament outcome

    dat[, prob := prob + stats::runif(.N)/1e12]
    dat[, res := as.integer(prob == max(prob)), by='slot']
    dat <- dat[res == 1,]
    dat[, res := NULL]

  
  return(dat)
}



#   Given an NCAA tournament bracket (a list of slots and who won the game)
#   this function will plot the bracket in a way that can be printed off.
   


Show_Bracket <- function(bracket, add_seed=TRUE, add_prob=TRUE){
  
  load(file =  "seed_positions.Rdata",  envir = environment())
  load(file =  "slot_positions.Rdata",  envir = environment())
  load(file =  "NCAATourneySeeds.Rdata",  envir = environment())
  load(file =  "Teams.Rdata",  envir = environment())
 
   #copy to avoid updating data
  bracket <- data.table::copy(bracket)
  
  #Checks
  year <- sort(unique(bracket$season))
  stopifnot(length(year)==1)
  
  #Subset seeds current year
  tourney_seeds <- tourney_seeds[season == year,]
  
  #Add team names
  data.table::setnames(teams, 'team_id', 'team')
  data.table::setnames(bracket, 'winner', 'team')
  bracket_seeds <- merge(tourney_seeds, teams, by='team', all.x=TRUE)
  bracket <- merge(bracket, teams, by='team', all.x=TRUE)
  
  #Parse seeds
  if(add_seed){
    bracket_seeds[,seed_int := as.integer(substr(seed, 2, 3))]
    bracket <- merge(bracket, bracket_seeds[,list(team, seed_int)], by='team')
    
    bracket_seeds[,team_name := paste0(team_name, '-(', seed_int, ')')]
    bracket[,team_name := paste0(team_name, '-(', seed_int, ')')]
  }
  
  #Add probs
  if(add_prob){
    bracket[,team_name := paste0(team_name, '-(', round(prob, 2), ')')]
  }
  
  #Add printing positions
  bracket_seeds <- merge(bracket_seeds, seed_print_positions, by=c('seed'), all.x=TRUE)
  bracket <- merge(bracket, slot_print_positions, by=c('slot'), all.x=TRUE)
  
  #Setup plot
  x <- seq(0,220,(221/67))
  y <- 0:66
  graphics::plot(x,y,type="l", col.axis="white", col.lab="white", bty="n",axes=F, col="white")
  graphics::segments(0,c(seq(0,30,2),seq(34,64,2)),20,c(seq(0,30,2),seq(34,64,2)))
  graphics::segments(20,c(seq(0,28,4),seq(34,62,4)),20,c(seq(2,30,4),seq(36,64,4)))
  graphics::segments(20,c(seq(1,29,4),seq(35,63,4)),40,c(seq(1,29,4),seq(35,63,4)))
  graphics::segments(40,c(seq(1,25,8),seq(35,59,8)),40,c(seq(5,29,8),seq(39,63,8)))
  graphics::segments(40,c(3,11,19,27,37,45,53,61),60,c(3,11,19,27,37,45,53,61))
  graphics::segments(60,c(3,19,37,53),60,c(11,27,45,61))
  graphics::segments(60,c(7,23,41,57),80,c(7,23,41,57))
  graphics::segments(80,c(7,41),80,c(23,57))
  graphics::segments(80,c(15,49),100,c(15,49))
  graphics::segments(100,c(27,37),120,c(27,37))
  graphics::segments(200,c(seq(0,30,2),seq(34,64,2)),220,c(seq(0,30,2),seq(34,64,2)))
  graphics::segments(200,c(seq(0,28,4),seq(34,62,4)),200,c(seq(2,30,4),seq(36,64,4)))
  graphics::segments(180,c(seq(1,29,4),seq(35,63,4)),200,c(seq(1,29,4),seq(35,63,4)))
  graphics::segments(180,c(seq(1,25,8),seq(35,59,8)),180,c(seq(5,29,8),seq(39,63,8)))
  graphics::segments(160,c(3,11,19,27,37,45,53,61),180,c(3,11,19,27,37,45,53,61))
  graphics::segments(160,c(3,19,37,53),160,c(11,27,45,61))
  graphics::segments(140,c(7,23,41,57),160,c(7,23,41,57))
  graphics::segments(140,c(7,41),140,c(23,57))
  graphics::segments(120,c(15,49),140,c(15,49))
  
  #Print Winner
  winner <- bracket[slot == 'R6CH',]
  graphics::text(winner$x,winner$y,winner$team_name, cex=2.5)
  
  #Print Bracket
  bracket <- bracket[slot != 'R6CH',]
  graphics::text(bracket$x, bracket$y, bracket$team_name,cex=.7)
  
  #Print seeds
  graphics::text(bracket_seeds$x, bracket_seeds$y, bracket_seeds$team_name,cex=.7)
  
  #Return nothing
  return(invisible())
}


#   ____________________________________________________________________________
#   Run the simulation                                                      ####


sim <- simulation( sub, N = 1000, year = 2017,  upset_bias = 0 )
bracket <- Create_Bracket(sim)
Show_Bracket(bracket)

