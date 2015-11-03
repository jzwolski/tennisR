#' Formats a database of match statistics to be able to find elo ratings.
#' 
#' @param x Database of match statistics, in the form of the MatchStats database.
#' @return A dataset of match results formatted to find elo ratings.
#' @export
MatchResultselofunction <- function(x){
  MatchResultselo <- x[c(3,7,8,9)]
  names(MatchResultselo)[4] <- "Result"
  names(MatchResultselo)[1] <- "Day"
  
  for (j in 1:nrow(MatchResultselo))
  {
    if(MatchResultselo$Player[[j]] == MatchResultselo$Result[[j]]){
      MatchResultselo$Result[[j]] <- 1
    } else{
      MatchResultselo$Result[[j]] <- 0
    }
  }
    
  MatchResultselo <- MatchResultselo[which(MatchResultselo$Result==1),]
  
  beginningdate <- as.Date("30.12.2012", "%d.%m.%Y")
  MatchResultselo$Day <- MatchResultselo$Day - beginningdate
  MatchResultselo$Day <- as.numeric(MatchResultselo$Day)
  
  return(MatchResultselo)
}