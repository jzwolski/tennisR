#' Formats an updated MatchStats database to be able to find elo ratings.
#' 
#' @param x Database of updated match statistics, namely the resulting database from the MatchStatsfunction.
#' @return A dataset of updated match results formatted to find elo ratings.
#' @export
MatchResultselofunction <- function(x){
  additionalMatchResultselo <- x[c(3,7,8,9)]
  names(additionalMatchResultselo)[4] <- "Result"
  names(additionalMatchResultselo)[1] <- "Day"
  
  for (j in 1:nrow(additionalMatchResultselo))
  {
    if(additionalMatchResultselo$Player[[j]] == additionalMatchResultselo$Result[[j]]){
      additionalMatchResultselo$Result[[j]] <- 1
    } else{
      additionalMatchResultselo$Result[[j]] <- 0
    }
  }
    
  additionalMatchResultselo <- additionalMatchResultselo[which(additionalMatchResultselo$Result==1),]
  
  beginningdate <- as.Date("30.12.2012", "%d.%m.%Y")
  additionalMatchResultselo$Day <- additionalMatchResultselo$Day - beginningdate
  additionalMatchResultselo$Day <- as.numeric(additionalMatchResultselo$Day)
  
  return(additionalMatchResultselo)
}