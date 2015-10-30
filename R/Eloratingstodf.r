#' Transforms a list of elo ratings into a data frame.
#' 
#' @param x A list of elo ratings.
#' @return The elo ratings in a data frame.
#' @export
Eloratingstodf <- function(x){
  elo.subset <- ldply(x, data.frame)
  elo.subset <- elo.subset[-c(1,9,10,11)]
  elo.subset <- head(elo.subset,-3)
  return(elo.subset)
}