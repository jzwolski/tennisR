#' Makes a prediction based on elo ratings, with no lag, for a given time period.
#' 
#' @param player1 First player in the head to head matchup.
#' @param player2 Second player in the head to head matchup.
#' @param elo A set of elo ratings, in the original elo ratings format.
#' @return A probability that player 1 will win the tennis match.
#' @export
Predictfunction_nolag <- function(player1, player2, elo){
  matchup <- data.frame(player1, player2)
  matchup$Day <- NA
  matchup <- matchup[c(3,1,2)]
  
  predict <- predict(elo, matchup, tng=5, gamma=0)
  predict <- data.frame(predict)
  predict.compare <- cbind(matchup, predict)
  predict.compare <- predict.compare[,-1]
  
  for(k in 1:nrow(predict.compare)){
    if(is.na(predict.compare$predict[[k]])==T){
      if(((nrow(results.subset[results.subset$Player==predict.compare$player1[[k]],])+
             nrow(results.subset[results.subset$Opponent==predict.compare$player1[[k]],]))<15) &
           ((nrow(results.subset[results.subset$Player==predict.compare$player2[[k]],])+
               nrow(results.subset[results.subset$Opponent==predict.compare$player2[[k]],]))>=15)){
        predict.compare$predict[[k]] <- 0
      }
      if(((nrow(results.subset[results.subset$Player==predict.compare$player1[[k]],])+
             nrow(results.subset[results.subset$Opponent==predict.compare$player1[[k]],]))>=15) &
           ((nrow(results.subset[results.subset$Player==predict.compare$player2[[k]],])+
               nrow(results.subset[results.subset$Opponent==predict.compare$player2[[k]],]))<15)){
        predict.compare$predict[[k]] <- 1
      }
      if(((nrow(results.subset[results.subset$Player==predict.compare$player1[[k]],])+
             nrow(results.subset[results.subset$Opponent==predict.compare$player1[[k]],]))<15) &
           ((nrow(results.subset[results.subset$Player==predict.compare$player2[[k]],])+
               nrow(results.subset[results.subset$Opponent==predict.compare$player2[[k]],]))<15)){
        predict.compare$predict[[k]] <- 0.5
      }
    }
  }
}