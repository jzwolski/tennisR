#' Creates a database of match statistics for tournaments.
#'
#' @param website A list of websites from the ATP World Tour.  Use websites of the player draw from the Results Archive page.
#' @return A complete database of match statistics.
#' @export
NewMatchStats <- function(website){
  additionalMatchStats <- NULL
  
  getLinks = function() { 
    links = character() 
    list(a = function(node, ...) { 
      links <<- c(links, xmlGetAttr(node, "href"))
      node 
    }, 
    links = function()links)
  }
  
  htmlToText <- function(input, ...) {
    ###---PACKAGES ---###
    require(RCurl)
    require(XML)
    
    
    ###--- LOCAL FUNCTIONS ---###
    # Determine how to grab html for a single input element
    evaluate_input <- function(input) {    
      # if input is a .html file
      if(file.exists(input)) {
        char.vec <- readLines(input, warn = FALSE)
        return(paste(char.vec, collapse = ""))
      }
      
      # if input is html text
      if(grepl("</html>", input, fixed = TRUE)) return(input)
      
      # if input is a URL, probably should use a regex here instead?
      if(!grepl(" ", input)) {
        # downolad SSL certificate in case of https problem
        if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
        return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
      }
      
      # return NULL if none of the conditions above apply
      return(NULL)
    }
    
    # convert HTML to plain text
    convert_html_to_text <- function(html) {
      doc <- htmlParse(html, asText = TRUE)
      text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
      return(text)
    }
    
    # format text vector into one character string
    collapse_text <- function(txt) {
      return(paste(txt, collapse = " "))
    }
    
    ###--- MAIN ---###
    # STEP 1: Evaluate input
    html.list <- lapply(input, evaluate_input)
    
    # STEP 2: Extract text from HTML
    text.list <- lapply(html.list, convert_html_to_text)
    
    # STEP 3: Return text
    text.vector <- sapply(text.list, collapse_text)
    return(text.vector)
  }
  
  for (j in 1:length(website))
  {
    #Find all links on the page of the draws#
    h1 <- getLinks()
    htmlTreeParse(file = website[j],
                  handlers = h1, useInternalNodes = TRUE)
    #h1$links()
    
    #Subset the links to only links for the match stats#
    files.1 <- h1$links()[str_detect(h1$links(), "match-stats")]
    
    #Add the beginning of the url to get the full web address
    files.2 <- paste("http://www.atpworldtour.com", files.1, sep="")
    firstweb <- substr(files.2, 1, nchar(files.2)-23)
    files.2 <- paste(firstweb, "/match-stats", sep="")
    
    #Pull out text from website
    text <- website[j]
    text <- htmlToText(text)  
    
    #Obtain the beginning and end dates of the tournament  
    date <- gsub(".*([2][0][0-1][0-9].[0-1][0-9].[0-3][0-9] - [2][0][0-1][0-9].[0-1][0-9].[0-3][0-9]).*", "\\1", text)
    begdate <- as.Date(substr(date, 1, 10), "%Y.%m.%d")
    enddate <- as.Date(substr(date, 14, 23), "%Y.%m.%d")
    
    #Determine the length of time between matches
    lengthtourn <- enddate-begdate
    
    draw <- gsub(".*Download PDF","\\1",text)
    draw <- gsub("\r","\\1",draw)
    draw <- gsub("\t","\\1",draw)
    draw <- gsub("\n","\\1",draw)
    draw <- substr(draw,1,10)
    draw <- gsub(" ", "",draw)
    draw <- gsub("R","",draw)
    draw <- as.numeric(draw)
    
    if(draw==32){
      nround <- 5
    }
    if(draw==64){
      nround <- 6
    }
    if(draw==128){
      nround <- 7
    }
    
    betwmatch <- floor(lengthtourn/nround)
    
    #Scrape the website for all the match facts for a given tournament#
    eachtourn = NULL
    for (i in 1:length(files.2))
    {
      table <- getURL(files.2[i])
      table <- readLines(tc <- textConnection(table)); close(tc)
            
      matchtable <- data.frame(matrix(NA, nrow = 2, ncol = 1))
      
      ##Match ID##
      names(matchtable)[1] <- "MatchID"
      matchtable$MatchID <- i
      
      ##Year##
      year <- gsub(".*([2][0][0-1][0-9]).*", "\\1", website[j])
      year <- as.numeric(year)
      matchtable$Year <- year
      
      ##Make the table into a dataframe##
      table <- as.data.frame(table)
      table$table <- as.character(table$table)
      
      ##Tournament##
      Tournament <- which(grepl('section-title',table$table))
      matchtable$Tournament <- substr(table$table[[Tournament[length(Tournament)]+1]],3,nchar(table$table[[Tournament[length(Tournament)]+1]]))
      
      ##Surface##
      Surface <- which(grepl('Court',table$table))
      matchtable$Surface <- substr(table$table[[Surface[length(Surface)]+3]],5,nchar(table$table[[Surface[length(Surface)]+3]]))
      
      ##Round##
      Round <- which(grepl('RoundTitle',table$table))
      matchtable$Round <- substr(table$table[[Round+1]],10,nchar(table$table[[Round+1]]))
      
      ##Date##
      if(draw==32){
        if(matchtable$Round[[1]]=="Round of 32"){
          matchtable$Date <- begdate
        }
        if(matchtable$Round[[1]]=="Round of 16"){
          matchtable$Date <- begdate+betwmatch
        }
        if(matchtable$Round[[1]]=="Quarter-Finals"){
          matchtable$Date <- begdate+(2*betwmatch)
        }
        if(matchtable$Round[[1]]=="Semi-Finals"){
          matchtable$Date <- begdate+(3*betwmatch)
        }
        if(matchtable$Round[[1]]=="Finals"){
          matchtable$Date <- begdate+(4*betwmatch)
        }
      }
      
      if(draw==64){
        if(matchtable$Round[[1]]=="Round of 64"){
          matchtable$Date <- begdate
        }
        if(matchtable$Round[[1]]=="Round of 32"){
          matchtable$Date <- begdate+betwmatch
        }
        if(matchtable$Round[[1]]=="Round of 16"){
          matchtable$Date <- begdate+(2*betwmatch)
        }
        if(matchtable$Round[[1]]=="Quarter-Finals"){
          matchtable$Date <- begdate+(3*betwmatch)
        }
        if(matchtable$Round[[1]]=="Semi-Finals"){
          matchtable$Date <- begdate+(4*betwmatch)
        }
        if(matchtable$Round[[1]]=="Finals"){
          matchtable$Date <- begdate+(5*betwmatch)
        }
      }
      
      if(draw==128){
        if(matchtable$Round[[1]]=="Round of 128"){
          matchtable$Date <- begdate
        }
        if(matchtable$Round[[1]]=="Round of 64"){
          matchtable$Date <- begdate+betwmatch
        }
        if(matchtable$Round[[1]]=="Round of 32"){
          matchtable$Date <- begdate+(2*betwmatch)
        }
        if(matchtable$Round[[1]]=="Round of 16"){
          matchtable$Date <- begdate+(3*betwmatch)
        }
        if(matchtable$Round[[1]]=="Quarter-Finals"){
          matchtable$Date <- begdate+(4*betwmatch)
        }
        if(matchtable$Round[[1]]=="Semi-Finals"){
          matchtable$Date <- begdate+(5*betwmatch)
        }
        if(matchtable$Round[[1]]=="Finals"){
          matchtable$Date <- begdate+(6*betwmatch)
        }
      }
      
      matchtable <- matchtable[c(1,2,6,3,4,5)]
      
      ##Player##
      PlayerNameFirst_Index <- which(grepl('span class="first-name"', table$table))
      PlayerNameLast_Index <- which(grepl('span class="last-name"', table$table))
      
      matchtable$Player <- paste(substr(table$table[[PlayerNameFirst_Index[1]+1]],6,nchar(table$table[[PlayerNameFirst_Index[1]+1]])), substr(table$table[[PlayerNameLast_Index[1]+1]],6,nchar(table$table[[PlayerNameLast_Index[1]+1]])), sep=" ")
      matchtable$Player[[2]] <- paste(substr(table$table[[PlayerNameFirst_Index[2]+1]],6,nchar(table$table[[PlayerNameFirst_Index[2]+1]])), substr(table$table[[PlayerNameLast_Index[2]+1]],6,nchar(table$table[[PlayerNameLast_Index[2]+1]])), sep=" ")
      
      ##Opponent##
      matchtable$Opponent <- matchtable$Player[[2]]
      matchtable$Opponent[[2]] <- matchtable$Player[[1]]
      
      ##Winner##
      matchtable$Winner <- matchtable$Player[[1]]
      
      ##Score##
      #matchtable$Score <- paste()
      
      ##Aces##
      Aces <- table[grep("Aces", table$table), ]    
      matchtable$Aces <- as.numeric(gsub(":","",substr(Aces[1],13,nchar(Aces[1])-3)))
      matchtable$Aces[[2]] <- as.numeric(gsub(":","",substr(Aces[3],13,nchar(Aces[3])-3)))
      
      ##Aces Difference##
      matchtable$Aces_Diff <- matchtable$Aces[[1]] - matchtable$Aces[[2]]
      matchtable$Aces_Diff[[2]] <- matchtable$Aces[[2]] - matchtable$Aces[[1]]
      
      ##Double Faults##
      Double_Faults <- table[grep("DoubleFaults", table$table), ]
      matchtable$Double_Faults <- as.numeric(gsub(":","",substr(Double_Faults[1],21,nchar(Double_Faults[1])-3)))
      matchtable$Double_Faults[[2]] <- as.numeric(gsub(":","",substr(Double_Faults[3],21,nchar(Double_Faults[3])-3)))
      
      ##Double Faults Difference##
      matchtable$Double_Faults_Diff <- matchtable$Double_Faults[[1]] - matchtable$Double_Faults[[2]]
      matchtable$Double_Faults_Diff[[2]] <- matchtable$Double_Faults[[2]] - matchtable$Double_Faults[[1]]
      
      ##First Serve Percentage##
      First_Serve_Per <- table[grep("FirstServePercentage", table$table), ]
      matchtable$First_Serve_Per <- as.numeric(gsub(":","",substr(First_Serve_Per[1],31,nchar(First_Serve_Per[1])-3)))
      matchtable$First_Serve_Per[[2]] <- as.numeric(gsub(":","",substr(First_Serve_Per[3],31,nchar(First_Serve_Per[3])-3)))
      
      ##First Serve Percentage Difference##
      matchtable$First_Serve_Per_Diff <- matchtable$First_Serve_Per[[1]] - matchtable$First_Serve_Per[[2]]
      matchtable$First_Serve_Per_Diff[[2]] <- matchtable$First_Serve_Per[[2]] - matchtable$First_Serve_Per[[1]]
      
      ##Total Number of First Serves##
      Total_First_Serves <- table[grep("FirstServePercentageDivisor", table$table), ]
      matchtable$Total_First_Serves <- as.numeric(gsub(":","",substr(Total_First_Serves[1],38,nchar(Total_First_Serves[1])-3)))
      matchtable$Total_First_Serves[[2]] <- as.numeric(gsub(":","",substr(Total_First_Serves[2],38,nchar(Total_First_Serves[2])-3)))
      
      #First Serve Points Won Percentage##
      First_Serve_Points_Won_Per <- table[grep("FirstServePointsWonPercentage", table$table), ]
      matchtable$First_Serve_Points_Won_Per <- as.numeric(gsub(":","",substr(First_Serve_Points_Won_Per[1],39,nchar(First_Serve_Points_Won_Per[1])-3)))
      matchtable$First_Serve_Points_Won_Per[[2]] <- as.numeric(gsub(":","",substr(First_Serve_Points_Won_Per[2],39,nchar(First_Serve_Points_Won_Per[2])-3)))
      
      ##First Serve Points Won Percentage Difference##
      matchtable$First_Serve_Points_Won_Per_Diff <- matchtable$First_Serve_Points_Won_Per[[1]] - matchtable$First_Serve_Points_Won_Per[[2]]
      matchtable$First_Serve_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Points_Won_Per[[2]] - matchtable$First_Serve_Points_Won_Per[[1]]
      
      ##Total Points Played on First Serve##
      Total_Points_on_First_Serve <- table[grep("FirstServePointsPlayed", table$table), ]
      matchtable$Total_Points_on_First_Serve <- as.numeric(gsub(":","",substr(Total_Points_on_First_Serve[1],32,nchar(Total_Points_on_First_Serve[1])-3)))
      matchtable$Total_Points_on_First_Serve[[2]] <- as.numeric(gsub(":","",substr(Total_Points_on_First_Serve[2],32,nchar(Total_Points_on_First_Serve[2])-3)))
      
      ##Second Serve Points Won Percentage##
      Second_Serve_Points_Won_Per <- table[grep("SecondServePointsWonPercentage", table$table), ]
      matchtable$Second_Serve_Points_Won_Per <- as.numeric(gsub(":","",substr(Second_Serve_Points_Won_Per[1],40,nchar(Second_Serve_Points_Won_Per[1])-3)))
      matchtable$Second_Serve_Points_Won_Per[[2]] <- as.numeric(gsub(":","",substr(Second_Serve_Points_Won_Per[2],40,nchar(Second_Serve_Points_Won_Per[2])-3)))
      
      ##Second Serve Points Won Percentage Difference##
      matchtable$Second_Serve_Points_Won_Per_Diff <- matchtable$Second_Serve_Points_Won_Per[[1]] - matchtable$Second_Serve_Points_Won_Per[[2]]
      matchtable$Second_Serve_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Points_Won_Per[[2]] - matchtable$Second_Serve_Points_Won_Per[[1]]
      
      ##Total Points Played on Second Serve##
      Total_Points_on_Second_Serve <- table[grep("SecondServePointsPlayed", table$table), ]
      matchtable$Total_Points_on_Second_Serve <- as.numeric(gsub(":","",substr(Total_Points_on_Second_Serve[1],33,nchar(Total_Points_on_Second_Serve[1])-3)))
      matchtable$Total_Points_on_Second_Serve[[2]] <- as.numeric(gsub(":","",substr(Total_Points_on_Second_Serve[2],33,nchar(Total_Points_on_Second_Serve[2])-3)))
      
      ##Number of Times Broken##
      Number_of_Times_Broken <- table[grep("NumberOfTimesBroken", table$table), ]
      matchtable$Number_of_Times_Broken <- as.numeric(gsub(":","",substr(Number_of_Times_Broken[1],28,nchar(Number_of_Times_Broken[1])-3)))
      matchtable$Number_of_Times_Broken[[2]] <- as.numeric(gsub(":","",substr(Number_of_Times_Broken[2],28,nchar(Number_of_Times_Broken[2])-3)))
      
      ##Break Points Faced##
      Break_Points_Faced <- table[grep("BreakPointsFacedServing", table$table), ]
      matchtable$Break_Points_Faced <- as.numeric(gsub(":","",substr(Break_Points_Faced[1],32,nchar(Break_Points_Faced[1])-3)))
      matchtable$Break_Points_Faced[[2]] <- as.numeric(gsub(":","",substr(Break_Points_Faced[3],32,nchar(Break_Points_Faced[3])-3)))
      
      ##Break Points Saved Percentage##
      Break_Points_Saved_Per <- table[grep("BreakPointsSavedPercentage", table$table), ]
      matchtable$Break_Points_Saved_Per <- as.numeric(gsub(":","",substr(Break_Points_Saved_Per[1],36,nchar(Break_Points_Saved_Per[1])-3)))
      matchtable$Break_Points_Saved_Per[[2]] <- as.numeric(gsub(":","",substr(Break_Points_Saved_Per[3],36,nchar(Break_Points_Saved_Per[3])-3)))
      
      ##Break Points Saved Percentage Difference##
      matchtable$Break_Points_Saved_Per_Diff <- matchtable$Break_Points_Saved_Per[[1]] - matchtable$Break_Points_Saved_Per[[2]]
      matchtable$Break_Points_Saved_Per_Diff[[2]] <- matchtable$Break_Points_Saved_Per[[2]] - matchtable$Break_Points_Saved_Per[[1]]
      
      ##Service Games Played##
      Service_Games_Played <- table[grep("ServiceGamesPlayed", table$table), ]
      matchtable$Service_Games_Played <- as.numeric(gsub(":","",substr(Service_Games_Played[1],28,nchar(Service_Games_Played[1])-3)))
      matchtable$Service_Games_Played[[2]] <- as.numeric(gsub(":","",substr(Service_Games_Played[3],28,nchar(Service_Games_Played[3])-3)))
      
      ##Total Points Played on Serve##
      Total_Points_on_Serve <- table[grep("TotalServicePointsWonPercentageDivisor", table$table), ]
      matchtable$Total_Points_on_Serve <- as.numeric(gsub(":","",substr(Total_Points_on_Serve[1],48,nchar(Total_Points_on_Serve[1])-3)))
      matchtable$Total_Points_on_Serve[[2]] <- as.numeric(gsub(":","",substr(Total_Points_on_Serve[2],48,nchar(Total_Points_on_Serve[2])-3)))
      
      ##Total Service Points Won Percentage##
      Total_Service_Points_Won_Per <- table[grep("TotalServicePointsWonPercentage", table$table), ]
      matchtable$Total_Service_Points_Won_Per <- as.numeric(gsub(":","",substr(Total_Service_Points_Won_Per[1],41,nchar(Total_Service_Points_Won_Per[1])-3)))
      matchtable$Total_Service_Points_Won_Per[[2]] <- as.numeric(gsub(":","",substr(Total_Service_Points_Won_Per[4],41,nchar(Total_Service_Points_Won_Per[4])-3)))
      
      ##Total Service Points Won Percentage Difference##
      matchtable$Total_Service_Points_Won_Per_Diff <- matchtable$Total_Service_Points_Won_Per[[1]] - matchtable$Total_Service_Points_Won_Per[[2]]
      matchtable$Total_Service_Points_Won_Per_Diff[[2]] <- matchtable$Total_Service_Points_Won_Per[[2]] - matchtable$Total_Service_Points_Won_Per[[1]]
      
      ##Total First Serve Return Points Won##
      Total_First_Serve_Return_Points_Won <- table[grep("FirstServeReturnPointsWon", table$table), ]
      matchtable$Total_First_Serve_Return_Points_Won <- as.numeric(gsub(":","",substr(Total_First_Serve_Return_Points_Won[1],35,nchar(Total_First_Serve_Return_Points_Won[1])-3)))
      matchtable$Total_First_Serve_Return_Points_Won[[2]] <- as.numeric(gsub(":","",substr(Total_First_Serve_Return_Points_Won[3],35,nchar(Total_First_Serve_Return_Points_Won[3])-3)))
      
      ##First Serve Return Points Won Percentage##
      First_Serve_Return_Points_Won_Per <- table[grep("FirstServeReturnPointsWonPercentage", table$table), ]
      matchtable$First_Serve_Return_Points_Won_Per <- round(as.numeric(gsub(":","",substr(First_Serve_Return_Points_Won_Per[1],45,nchar(First_Serve_Return_Points_Won_Per[1])-3))), digits=0)
      matchtable$First_Serve_Return_Points_Won_Per[[2]] <- round(as.numeric(gsub(":","",substr(First_Serve_Return_Points_Won_Per[2],45,nchar(First_Serve_Return_Points_Won_Per[2])-3))), digits=0)
      
      ##First Serve Return Points Won Percentage Difference##
      matchtable$First_Serve_Return_Points_Won_Per_Diff <- matchtable$First_Serve_Return_Points_Won_Per[[1]] - matchtable$First_Serve_Return_Points_Won_Per[[2]]
      matchtable$First_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$First_Serve_Return_Points_Won_Per[[2]] - matchtable$First_Serve_Return_Points_Won_Per[[1]]
      
      ##Total Second Serve Return Points Won##
      Total_Second_Serve_Return_Points_Won <- table[grep("SecondServeReturnPointsWon", table$table), ]
      matchtable$Total_Second_Serve_Return_Points_Won <- as.numeric(gsub(":","",substr(Total_Second_Serve_Return_Points_Won[1],36,nchar(Total_Second_Serve_Return_Points_Won[1])-3)))
      matchtable$Total_Second_Serve_Return_Points_Won[[2]] <- as.numeric(gsub(":","",substr(Total_Second_Serve_Return_Points_Won[3],36,nchar(Total_Second_Serve_Return_Points_Won[3])-3)))
      
      ##Second Serve Return Points Won Percentage##
      Second_Serve_Return_Points_Won_Per <- table[grep("SecondServeReturnPointsWonPercentage", table$table), ]
      matchtable$Second_Serve_Return_Points_Won_Per <- round(as.numeric(gsub(":","",substr(Second_Serve_Return_Points_Won_Per[1],45,nchar(Second_Serve_Return_Points_Won_Per[1])-3))), digits=0)
      matchtable$Second_Serve_Return_Points_Won_Per[[2]] <- round(as.numeric(gsub(":","",substr(Second_Serve_Return_Points_Won_Per[2],45,nchar(Second_Serve_Return_Points_Won_Per[2])-3))), digits=0)
      
      ##Second Serve Return Points Won Percentage Difference##
      matchtable$Second_Serve_Return_Points_Won_Per_Diff <- matchtable$Second_Serve_Return_Points_Won_Per[[1]] - matchtable$Second_Serve_Return_Points_Won_Per[[2]]
      matchtable$Second_Serve_Return_Points_Won_Per_Diff[[2]] <- matchtable$Second_Serve_Return_Points_Won_Per[[2]] - matchtable$Second_Serve_Return_Points_Won_Per[[1]]
      
      ##Total Break Points Converted##
      Break_Points_Converted <- table[grep("BreakPointsConverted", table$table), ]
      matchtable$Break_Points_Converted <- as.numeric(gsub(":","",substr(Break_Points_Converted[1],29,nchar(Break_Points_Converted[1])-3)))
      matchtable$Break_Points_Converted[[2]] <- as.numeric(gsub(":","",substr(Break_Points_Converted[3],29,nchar(Break_Points_Converted[3])-3)))
      
      ##Break Points Converted Percentage##
      Break_Points_Converted_Per <- table[grep("BreakPointsWonPercentage", table$table), ]
      matchtable$Break_Points_Converted_Per <- as.numeric(gsub(":","",substr(Break_Points_Converted_Per[1],34,nchar(Break_Points_Converted_Per[1])-3)))
      matchtable$Break_Points_Converted_Per[[2]] <- as.numeric(gsub(":","",substr(Break_Points_Converted_Per[2],34,nchar(Break_Points_Converted_Per[2])-3)))
      
      ##Break Points Converted Percentage Difference##
      matchtable$Break_Points_Converted_Per_Diff <- matchtable$Break_Points_Converted_Per[[1]] - matchtable$Break_Points_Converted_Per[[2]]
      matchtable$Break_Points_Converted_Per_Diff[[2]] <- matchtable$Break_Points_Converted_Per[[2]] - matchtable$Break_Points_Converted_Per[[1]]
      
      ##Return Games Played##
      Return_Games_Played <- table[grep("ReturnGamesPlayed", table$table), ]
      matchtable$Return_Games_Played <- as.numeric(gsub(":","",substr(Return_Games_Played[1],27,nchar(Return_Games_Played[1])-3)))
      matchtable$Return_Games_Played[[2]] <- as.numeric(gsub(":","",substr(Return_Games_Played[3],27,nchar(Return_Games_Played[3])-3)))
      
      ##Total Points Played Returning##
      Total_Points_Played_Returning <- table[grep("TotalReturnPointsWonPercentageDivisor", table$table), ]
      matchtable$Total_Points_Played_Returning <- as.numeric(gsub(":","",substr(Total_Points_Played_Returning[1],47,nchar(Total_Points_Played_Returning[1])-3)))
      matchtable$Total_Points_Played_Returning[[2]] <- as.numeric(gsub(":","",substr(Total_Points_Played_Returning[2],47,nchar(Total_Points_Played_Returning[2])-3)))
      
      ##Total Return Points Won Percentage##
      Total_Return_Points_Won_Per <- table[grep("TotalReturnPointsWonPercentage", table$table), ]
      matchtable$Total_Return_Points_Won_Per <- as.numeric(gsub(":","",substr(Total_Return_Points_Won_Per[1],40,nchar(Total_Return_Points_Won_Per[1])-3)))
      matchtable$Total_Return_Points_Won_Per[[2]] <- as.numeric(gsub(":","",substr(Total_Return_Points_Won_Per[4],40,nchar(Total_Return_Points_Won_Per[4])-3)))
      
      ##Total Return Points Won Percentage Difference##
      matchtable$Total_Return_Points_Won_Per_Diff <- matchtable$Total_Return_Points_Won_Per[[1]] - matchtable$Total_Return_Points_Won_Per[[2]]
      matchtable$Total_Return_Points_Won_Per_Diff[[2]] <- matchtable$Total_Return_Points_Won_Per[[2]] - matchtable$Total_Return_Points_Won_Per[[1]]
      
      ##Total Points Won##
      Total_Points_Won <- table[grep("TotalPointsWonPercentageDividend", table$table), ]
      matchtable$Total_Points_Won <- as.numeric(gsub(":","",substr(Total_Points_Won[1],42,nchar(Total_Points_Won[1])-3)))
      matchtable$Total_Points_Won[[2]] <- as.numeric(gsub(":","",substr(Total_Points_Won[2],42,nchar(Total_Points_Won[2])-3)))
      
      ##Total Points Won Percentage##
      Total_Points_Won_Per <- table[grep("TotalPointsWonPercentage", table$table), ]
      matchtable$Total_Points_Won_Per <- as.numeric(gsub(":","",substr(Total_Points_Won_Per[1],34,nchar(Total_Points_Won_Per[1])-3)))
      matchtable$Total_Points_Won_Per[[2]] <- as.numeric(gsub(":","",substr(Total_Points_Won_Per[4],34,nchar(Total_Points_Won_Per[4])-3)))
      
      ##Total Points Won Percentage Difference##
      matchtable$Total_Points_Won_Per_Diff <- matchtable$Total_Points_Won_Per[[1]] - matchtable$Total_Points_Won_Per[[2]]
      matchtable$Total_Points_Won_Per_Diff[[2]] <- matchtable$Total_Points_Won_Per[[2]] - matchtable$Total_Points_Won_Per[[1]]
      
      if(i == 1){
        eachtourn <- matchtable
      } else {
        eachtourn <- rbind(eachtourn, matchtable)
      }
    }
    if(draw == 32){
      eachtourn$Round <- factor(eachtourn$Round, levels=c("Round of 32", "Round of 16", "Quarter-Finals",
                                                          "Semi-Finals", "Finals"))
    }
    if(draw == 64){
      eachtourn$Round <- factor(eachtourn$Round, levels=c("Round of 64", "Round of 32", "Round of 16",
                                                          "Quarter-Finals", "Semi-Finals", "Finals"))
    }
    if(draw == 128){
      eachtourn$Round <- factor(eachtourn$Round, levels=c("Round of 128", "Round of 64", "Round of 32",
                                                          "Round of 16", "Quarter-Finals", "Semi-Finals", "Finals"))
    }
    eachtourn <- eachtourn[order(eachtourn$Round),]
    eachtourn$MatchID <- rep(1:length(files.2), each=2)
    if(j == 1){
      additionalMatchStats <- eachtourn
    } else {
      additionalMatchStats <- rbind(additionalMatchStats, eachtourn)
    }
  }
  
  additionalMatchStats <- additionalMatchStats[order(additionalMatchStats$Year, additionalMatchStats$Tournament, xtfrm(additionalMatchStats$Date), additionalMatchStats$MatchID),]
  additionalMatchStats$Player <- as.character(additionalMatchStats$Player)
  additionalMatchStats$Opponent <- as.character(additionalMatchStats$Opponent)
  additionalMatchStats[is.na(additionalMatchStats)] <- 0
  
  for(k in 1:nrow(additionalMatchStats)){
    if(additionalMatchStats$Player[k]=="Pablo And&#250;jar"){
      additionalMatchStats$Player[k]="Pablo Andujar"
    }
    if(additionalMatchStats$Opponent[k]=="Pablo And&#250;jar"){
      additionalMatchStats$Opponent[k]="Pablo Andujar"
    }
    if(additionalMatchStats$Winner[k]=="Pablo And&#250;jar"){
      additionalMatchStats$Winner[k]="Pablo Andujar"
    }
    if(additionalMatchStats$Player[k]=="Nicol&#225;s Almagro"){
      additionalMatchStats$Player[k]="Nicolas Almagro"
    }
    if(additionalMatchStats$Opponent[k]=="Nicol&#225;s Almagro"){
      additionalMatchStats$Opponent[k]="Nicolas Almagro"
    }
    if(additionalMatchStats$Winner[k]=="Nicol&#225;s Almagro"){
      additionalMatchStats$Winner[k]="Nicolas Almagro"
    }
    if(additionalMatchStats$Player[k]=="Mart&#237;n Alund"){
      additionalMatchStats$Player[k]="Martin Alund"
    }
    if(additionalMatchStats$Opponent[k]=="Mart&#237;n Alund"){
      additionalMatchStats$Opponent[k]="Martin Alund"
    }
    if(additionalMatchStats$Winner[k]=="Mart&#237;n Alund"){
      additionalMatchStats$Winner[k]="Martin Alund"
    }
    if(additionalMatchStats$Player[k]=="Guillermo Garc&#237;a-L&#243;pez"){
      additionalMatchStats$Player[k]="Guillermo Garcia-Lopez"
    }
    if(additionalMatchStats$Opponent[k]=="Guillermo Garc&#237;a-L&#243;pez"){
      additionalMatchStats$Opponent[k]="Guillermo Garcia-Lopez"
    }
    if(additionalMatchStats$Winner[k]=="Guillermo Garc&#237;a-L&#243;pez"){
      additionalMatchStats$Winner[k]="Guillermo Garcia-Lopez"
    }
    if(additionalMatchStats$Player[k]=="Feliciano L&#243;pez"){
      additionalMatchStats$Player[k]="Feliciano Lopez"
    }
    if(additionalMatchStats$Opponent[k]=="Feliciano L&#243;pez"){
      additionalMatchStats$Opponent[k]="Feliciano Lopez"
    }
    if(additionalMatchStats$Winner[k]=="Feliciano L&#243;pez"){
      additionalMatchStats$Winner[k]="Feliciano Lopez"
    }
    if(additionalMatchStats$Player[k]=="Juan M&#243;naco"){
      additionalMatchStats$Player[k]="Juan Monaco"
    }
    if(additionalMatchStats$Opponent[k]=="Juan M&#243;naco"){
      additionalMatchStats$Opponent[k]="Juan Monaco"
    }
    if(additionalMatchStats$Winner[k]=="Juan M&#243;naco"){
      additionalMatchStats$Winner[k]="Juan Monaco"
    }
  }
  return(additionalMatchStats)
}