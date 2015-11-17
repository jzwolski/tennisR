#' Makes a prediction based on elo ratings for a given time period.
#' 
#' @param player1 First player in the head to head matchup.
#' @param player2 Second player in the head to head matchup.
#' @param elo A set of elo ratings, in the original elo ratings format.
#' @param games Total number of games played by either player.
#' @param results A subset of match results (the second list returned from either elo ratings functions)
#' @return A probability that player 1 will win the tennis match.
#' @export
Predict <- function(player1, player2, elo, games, results){
  matchup <- data.frame(player1, player2)
  matchup$Day <- NA
  matchup <- matchup[c(3,1,2)]
  
  predict <- predict(elo, matchup, tng=games, gamma=0)
  predict <- data.frame(predict)
  predict.compare <- cbind(matchup, predict)
  predict.compare <- predict.compare[,-1]
  
  for(k in 1:nrow(predict.compare)){
    if(is.na(predict.compare$predict[[k]])==T){
      if(((nrow(results[results$Player==predict.compare$player1[[k]],])+
             nrow(results[results$Opponent==predict.compare$player1[[k]],]))<15) &
           ((nrow(results[results$Player==predict.compare$player2[[k]],])+
               nrow(results[results$Opponent==predict.compare$player2[[k]],]))>=15)){
        predict.compare$predict[[k]] <- 0
      }
      if(((nrow(results[results$Player==predict.compare$player1[[k]],])+
             nrow(results[results$Opponent==predict.compare$player1[[k]],]))>=15) &
           ((nrow(results[results$Player==predict.compare$player2[[k]],])+
               nrow(results[results$Opponent==predict.compare$player2[[k]],]))<15)){
        predict.compare$predict[[k]] <- 1
      }
      if(((nrow(results[results$Player==predict.compare$player1[[k]],])+
             nrow(results[results$Opponent==predict.compare$player1[[k]],]))<15) &
           ((nrow(results[results$Player==predict.compare$player2[[k]],])+
               nrow(results[results$Opponent==predict.compare$player2[[k]],]))<15)){
        predict.compare$predict[[k]] <- 0.5
      }
    }
  }
  probability <- data.frame(predict.compare$player1,predict.compare$player2,predict.compare$predict)
  names(probability)[1] <- "Player1"
  names(probability)[2] <- "Player2"
  names(probability)[3] <- "Probability"
  return(probability)
}