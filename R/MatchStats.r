#' Tennis Match Statistics Database
#' 
#' Database of match statistics for all ATP250, ATP500, Masters 1000 Series, and Grand Slam
#' Tournaments from 2013 to the 2015 US Open.
#' Includes preliminary information for each match, and match statistics include serving,
#' returning, and overall statistics.
#' 
#' @docType data
#' 
#' @usage data(MatchStats)
#' 
#' @format A data frame with 14654 rows and 46 variables:
#' \describe{
#'    \item{MatchID}{index for each match within each tournament}
#'    \item{Year}{year of the tournament}
#'    \item{Date}{date of the match, as a date variable}
#'    \item{Tournament}{name of the tournament}
#'    \item{Surface}{court surface, either hard, clay, or grass}
#'    \item{Round}{round in the tournament for a specific match}
#'    \item{Player}{player one}
#'    \item{Opponent}{player two}
#'    \item{Winner}{winner of the match}
#'    \item{Aces}{number of total aces}
#'    \item{Aces_Diff}{difference in aces between player one and two}
#'    \item{Double_Faults}{number of total double faults}
#'    \item{Double_Faults_Diff}{difference in double faults between player one and two}
#'    \item{First_Serve_Per}{percentage of total first serves in play}
#'    \item{First_Serve_Per_Diff}{difference in first serve percentage between player one and two}
#'    \item{Total_First_Serves}{total number of first serves}
#'    \item{First_Serve_Points_Won_Per}{percentage of points won on first serve}
#'    \item{First_Serve_Points_Won_Per_Diff}{difference in percentage of points won on first serve between player one and two}
#'    \item{Total_Points_on_First_Serve}{total number of points played when first serve was in play}
#'    \item{Second_Serve_Points_Won_Per}{percentage of points won on second serve}
#'    \item{Second_Serve_Points_Won_Per_Diff}{difference in percentage of points won on second serve between player one and two}
#'    \item{Total_Points_on_Second_Serve}{total number of points played when second serve was in play}
#'    \item{Number_of_Times_Broken}{number of points lost when serving that resulted in a loss of a game}
#'    \item{Break_Points_Faced}{total points played on serve, which if lost, would reesult in a loss of a game}
#'    \item{Break_Points_Saved_Per}{percentage of break points saved when serving}
#'    \item{Break_Points_Saved_Per_Diff}{difference in percentage of break points saved when serving between player one and two}
#'    \item{Service_Games_Played}{total number of games played when serving}
#'    \item{Total_Points_on_Serve}{total number of points played when serving}
#'    \item{Total_Service_Points_Won_Per}{percentage of points won when serving, includes points on first serve and second serve}
#'    \item{Total_Service_Points_Won_Per_Diff}{difference in percentage of points won when serving between player one and two}
#'    \item{Total_First_Serve_Return_Points_Won}{number of points won when returning a first serve in play}
#'    \item{First_Serve_Return_Points_Won_Per}{percentage of points won when returning a first serve in play}
#'    \item{First_Serve_Return_Points_Won_Per_Diff}{difference in percentage of points won when returning a first serve in play between player one and two}
#'    \item{Total_Second_Serve_Return_Points_Won}{number of points won when returning a second serve in play}
#'    \item{Second_Serve_Return_Points_Won_Per}{percentage of points won when returning a second serve in play}
#'    \item{Second_Serve_Return_Points_Won_Per_Diff}{difference in percentage of points won when returning a second serve in play between player one and two}
#'    \item{Break_Points_Converted}{number of points won when returning that resulted in a win of a game}
#'    \item{Break_Points_Converted_Per}{percentage of points won when returning that would result in a win of a game}
#'    \item{Break_Points_Converted_Per_Diff}{difference in percentage of points won when returning that would result in a win of a game between player one and two}
#'    \item{Return_Games_Played}{total number of games played when returning}
#'    \item{Total_Points_Played_Returning}{total number of points played when returning}
#'    \item{Total_Return_Points_Won_Per}{percentage of points won when returning, includes points on first serve and second serve}
#'    \item{Total_Return_Points_Won_Per_Diff}{difference in percentage of points won when returning, includes points on first serve and second serve between player one and two}
#'    \item{Total_Points_Won}{total points won during the match, includes points won on serve and while returning}
#'    \item{Total_Points_Won_Per}{percentage of total points won during the match}
#'    \item{Total_Points_Won_Per_Diff}{difference in percentage of total points won during the match between player one and two}
#'
#' }    
#' 
#' @source \url{http://www.atpworldtour.com/}
"MatchStats"