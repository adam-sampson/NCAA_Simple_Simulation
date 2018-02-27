



################## TEST PARAMETERS #####################################

   probs <-   sample_submission 
    # N = 100
    # year = 2017
    # upset_bias = 0
########################################################################
    

#   ____________________________________________________________________________
#   Clean Predictions Data                                                  ####
    
    probs$id <-  strsplit(probs$id, "_")
    probs$season <-    as.integer(sapply(probs$id, "[", 1))
    probs$team_1 <-  as.integer(sapply(probs$id, "[", 2))
    probs$team_2 <- as.integer(sapply(probs$id, "[", 3))
    probs <- probs[,-1]
#   ____________________________________________________________________________
#   Function to run simulation                                              ####
    simulation <- function (probs, N = 1000, year = 2017,  upset_bias = 0) 
    {
        
    load(file =  "all_slots.Rdata",  envir = environment())
    probs <- probs[season == year, ]
    n1 <- nrow(probs)
    probs <- merge(probs, all_slots, by = c("season", "team_1", "team_2"))
    
    stopifnot(n1 == nrow(probs))
    probs$seed_1_int <- as.integer(substr(probs$seed_1, 2, 3))
    probs$seed_2_int <- as.integer(substr(probs$seed_2, 2, 3))
    probs$rand <- stats::runif(.N)
    probs$winner <- ifelse(probs$pred > probs$rand, "team_1", "team_2")
    probs$keep <- 1L
    
    if (upset_bias != 0) {
        
        probs$rand <- ifelse(probs$seed_1_int > probs$seed_2_int,  probs$rand - upset_bias, probs$rand - upset_bias)
       
    }
 
    
    sims_list <- lapply(1:N, function(x) {
        probs$rand <- stats::runif(.N)
        if (upset_bias != 0) {
            probs$rand <- ifelse(probs$seed_1_int > probs$seed_2_int,  probs$rand - upset_bias, probs$rand - upset_bias)
        }
        
        probs$winner <- ifelse(probs$pred > probs$rand, probs$team_1, probs$team_2)
        internal_sim(probs)
    })
    sims <- data.table::rbindlist(sims_list)
    data.table::setkeyv(sims, c("slot", "winner"))
    sims <- sims[, list(count = .N), by = c("slot", "winner")]
    sims[, `:=`(count, count + stats::runif(.N)/1e+12)]
    sims$count <- sims$count + (stats::runif(.N)/1e+12)
    all_possible_slots_team_1 <- all_slots[season == year, list(slot, 
                                                                winner = team_1)]
    all_possible_slots_team_2 <- all_slots[season == year, list(slot, 
                                                                winner = team_2)]
    all_possible_slots <- unique(rbind(all_possible_slots_team_1, 
                                       all_possible_slots_team_2))
    sims <- merge(all_possible_slots, sims, all.x = TRUE, by = c("slot", 
                                                                 "winner"))
    sims$count[is.na(sims$count)] <- 0
    sims$prob <- sims$count/N
    sims <- sims[order(count, decreasing = TRUE)]
    sims$season <- year
    return(sims)
}





internal_sim <- function (probs) 
{
    all_rounds <- sort(unique(probs$round))
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
simulation( probs, N = 100, year = 2017,  upset_bias = 0 )
