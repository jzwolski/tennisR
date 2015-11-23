#' Creates elo ratings for players within a specified time frame, also including matches one month before the specified time frame.
#' 
#' @param date1 The beginning date for ratings, example 2015-03-14.
#' @param date2 The end date for ratings, example 2015-03-14.
#' @param results A data frame where first column is the number of days from a specified date, the second column is Player 1, the third column is Player 2, and the last column is the result, either a 0 or 1.
#' @return A list of elo ratings and match results.
#' @export
Eloratings_status <- function(date1, date2, results){
  date1 <- as.Date(date1, "%Y-%m-%d")
  date2 <- as.Date(date2, "%Y-%m-%d")
  lagdate1 <- date1 %m-% months(1)
  lagdate2 <- date1 - 1
  beginningdate <- as.Date("2012-12-30", "%Y-%m-%d")
  lagdays1 <- lagdate1 - beginningdate
  lagdays2 <- lagdate2 - beginningdate
  results.lagsubset <- results[which((results$Day>=lagdays1)&(results$Day<=lagdays2)),]
  elo.lagsubset <- elo(results.lagsubset, status=NULL, init=0, sort=T)
  elo.lagsubset <- ldply(elo.lagsubset, data.frame)
  elo.lagsubset <- elo.lagsubset[c(2,3)]
  elo.lagsubset <- head(elo.lagsubset,-3)
  
  days1 <- date1 - beginningdate
  days2 <- date2 - beginningdate
  results.subset <- results[which((results$Day>=days1)&(results$Day<=days2)),]
  elo.subset <- elo(results.subset, status=elo.lagsubset, init=0, sort=T)
  fullresults <- rbind(results.lagsubset, results.subset)
  return(list(elo.subset, fullresults))
}