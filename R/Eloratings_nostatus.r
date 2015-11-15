#' Creates elo ratings for players within a specified time frame.
#' 
#' @param date1 The beginning date for ratings, example 2015-03-14.
#' @param date2 The end date for ratings, example 2015-03-14.
#' @param x A data frame where first column is the number of days from a specified date, the second column is Player 1, the third column is Player 2, and the last column is the result, either a 0 or 1.
#' @return A list of elo ratings and match results.
#' @export
Eloratings_nostatus <- function(date1, date2, x){
  date1 <- as.Date(date1, "%Y-%m-%d")
  date2 <- as.Date(date2, "%Y-%m-%d")
  beginningdate <- as.Date("2012-12-30", "%Y-%m-%d")
  days1 <- date1 - beginningdate
  days2 <- date2 - beginningdate
  results.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(results.subset, status=NULL, init=0, sort=T)
  return(list(elo.subset, results.subset))
}