#' Creates elo ratings for players within a specified time frame with a one month lag before the specified time frame.
#' 
#' @param date1 The beginning date for ratings.
#' @param date2 The end date for ratings.
#' @param x A data frame where first column is the number of days from a specified date, the second column is Player 1, the third column is Player 2, and the last column is the result, either a 0 or 1.
#' @return A list of elo ratings.
#' @export
Eloratings_lag <- function(date1, date2, x){
  lagdate1 <- date1 %m-% months(1)
  lagdate2 <- date1 - 1
  lagdays1 <- lagdate1 - beginningdate
  lagdays2 <- lagdate2 - beginningdate
  results.lagsubset <- x[which((x$Day>=lagdays1)&(x$Day<=lagdays2)),]
  elo.lagsubset <- elo(results.lagsubset, status=NULL, init=1, sort=T)
  elo.lagsubset <- ldply(elo.lagsubset, data.frame)
  elo.lagsubset <- elo.lagsubset[c(2,3)]
  elo.lagsubset <- head(elo.lagsubset,-3)
  
  days1 <- date1 - beginningdate
  days2 <- date2 - beginningdate
  results.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(results.subset, status=elo.lagsubset, init=1, sort=T)
  fullresults <- rbind(results.lagsubset, results.subset)
  return(elo.subset)
}