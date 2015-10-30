#' Creates elo ratings for players within a specified time frame without lag.
#' 
#' @param d1 The beginning date for ratings.
#' @param d2 The end date for ratings.
#' @param x A data frame where first column is the number of days from a specified date, the second column is Player 1, the third column is Player 2, and the last column is the result, either a 0 or 1.
#' @return A list of elo ratings.
#' @export
Eloratings_nolag <- function(d1, d2, x){
  days1 <- d1 - beginningdate
  days2 <- d2 - beginningdate
  results.subset <- x[which((x$Day>=days1)&(x$Day<=days2)),]
  elo.subset <- elo(results.subset, status=NULL, init=1, sort=T)
  return(elo.subset)
}