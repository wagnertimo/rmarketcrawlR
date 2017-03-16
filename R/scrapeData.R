#'
#' The scrapeData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main and helper functions to crawl call and auction results data out of
#' @references \url{https://www.regelleistung.net/ext/data/}
#' @references \url{https://www.regelleistung.net/ext/tender/}
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'



#
# HELPER FUNCTIONS FOR CALL DATA
#



scrape_rl_calls <- function(date_from, date_to, uenb_type, rl_type) {

  library(httr)

  url = 'https://www.regelleistung.net/ext/data/';

  payload = list(
     'from' = date_from,
     'to' = date_to,
     'download' = 'true',
     '_download' = 'on',
     'tsoId' = uenb_type,
     'dataType' = rl_type
  );

  postResponse <- POST(url, body = payload, encode = "form", verbose())

  return(content(postResponse, "text"))

}


preprocess_rl_calls <- function(response_content) {

  library(xml2)

  # Preprocess the response data
  #
  # Delete the first 5 rows (unneccessary additional infos)
  # Therefore split first the text
  response_content <-strsplit(response_content, "\n")
  # Now skip/delete the first 5 rows
  response_content <- response_content[[1]][5:length(response_content[[1]])]
  # Paste the char vector back again to a char variable
  response_content <- paste(response_content, sep = "", collapse = "\n")

  return(response_content)

}




#
# HELPER FUNCTIONS FOR AUCTIONS DATA
#

scrape_rl_auctions <- function(date_from, productId) {

  library(httr)

  url = 'https://www.regelleistung.net/ext/tender/';

  payload = list(
    'from' = date_from,
    'productId' = productId
  );

  postResponse <- POST(url, body = payload, encode = "form", verbose())

  return(postResponse)

}


# Crawl the reults table of the auctions to get the auctionIds
getAuctionIds <- function(response) {

  library(XML)

  parsedHtml <- htmlParse(content(response, "text"))

  # Xpath exppression to retrieve all links in the results (tender) table
  # There are three links in all table column containing the auctionId -> just get a unique/distinct link (contains 'details')
  link_elements <- xpathSApply(parsedHtml, "id('tender-table')/tbody/tr/td/a[contains(@href,'details')]/@href")

  auctionIds <- c()

  for(i in 1:length(link_elements)) {

    # Split the link on the slashes '/' and take only the last element, this is the auctionId
    auctionId <- strsplit(link_elements[i], "/")[[1]][length(strsplit(link_elements[i], "/")[[1]])]

    auctionIds <- append(auctionIds, auctionId)
  }

  return(auctionIds)

}


callGETforAuctionResults <- function(auctionId) {

  library(httr)

  url = paste('https://www.regelleistung.net/ext/tender/results/anonymousdownload/',auctionId, sep = "");

  getResponse <- GET(url, verbose())

  return(content(getResponse, "text"))

}




#
# COMMON HELPER FUNCITON (CALL AND AUCTION)
#

# Ignore the Warning message: header and 'col.names' are of different lengths
# This is a strange error it still works
build_df_rl_calls_auctions <- function(response_content, fileName) {

  # Write a temporary csv file out of the preprocessed response data.
  # This whole approach with the temp.csv file allows to process bigger files.
  #
  # Write a temporary csv file from the char variable
  write.csv(response_content, file = fileName, eol = "\n")



  # This if statement builds the data.frame for the operating reserve calls
  if(fileName == "temp.csv") {

    # Read in the temporary csv file
    #
    # Writing the csv file does not remove the "..." parenthesis of the char variable. Furthermore it adds an extra line at the top: "","x" and at the beginning of the second line: "1",
    # Therefore the read in function uses the parameters:
    #     quote = "" (get rid of parenthesis)
    #     skip = 1 (to get rid off the extra line at the beginning)
    df <- read.csv(file = fileName, header = TRUE, sep = ";", dec = ",", na.strings = c("","-"), quote = "", skip = 1)

    # Rename the first date column which has a cryptic name because of the "1",)
    colnames(df)[1] <- "DATUM"

  }
  # This if statement builds the data.frame for the operating reserve auctions
  else if(fileName == "temp2.csv") {

    df <- read.csv(file = "temp2.csv",
                   header = TRUE,
                   sep = ";",
                   dec = ",",
                   na.strings = c("","-"),
                   quote = "",
                   skip = 1,
                   #row.names=NULL,
                   col.names=c("date_from","date_to","product_name","power_price","work_price",
                               "ap_payment_direction","offered_power_MW","called_power_MW","offers_AT", "rr")
    )
    # Workaround to avoid an error -> strange Bug BUT seems to work
    df$rr <- NULL
    # Delete last row -> there is an additional row with a parenthesis and NAs
    df <- df[1:nrow(df)-1,]

  }


  # DELETE temporary files
  #
  invisible(if (file.exists("temp.csv")) file.remove("temp.csv"))
  invisible(if (file.exists("temp2.csv")) file.remove("temp2.csv"))

  return(df)

}




#
# MAIN FUNCTIONS
#


#' @title getOperatingReserveAuctions
#'
#' @description This main function retrieves the operating reserve auction results from \url{https://www.regelleistung.net/ext/tender/}.
#' The data contains all auctions from a given starting date till now.
#'
#'
#' @param date_from the starting date to retrieve all auctions till NOW in the date format DD.MM.YYYY (e.g.'07.03.2017')
#' @param productId PRL (1), SRL (2), MRL (3), sofort abschaltbare Lasten (4), schnell abschaltbare Lasten (5), Primärregelleistung NL (6)
#'
#' @return data.frame with the results of the auctions held from starting date till now
#'
#' @examples
#' getOperatingReserveAuctions('07.03.2017', '2')
#'
#' @export
#'
getOperatingReserveAuctions <- function(date_from, productId) {

  auctionsResponse <- scrape_rl_auctions(date_from, productId)
  auctionIds <- getAuctionIds(auctionsResponse)

  response_content <- callGETforAuctionResults(auctionIds[1])
  df_auctions <- build_df_rl_calls_auctions(response_content, "temp2.csv")

  if(length(auctionIds) > 1) {

    for(j in 2:length(auctionIds)) {

      response_content <- callGETforAuctionResults(auctionIds[j])
      df <- build_df_rl_calls_auctions(response_content, "temp2.csv")

      df_auctions <- rbind(df_auctions, df)
    }
  }

  return(df_auctions)

}




#' @title getOperatingReserveCalls
#'
#' @description This main function retrieves the operating reserve calls from \url{https://www.regelleistung.net/ext/data/}. The time interval can only lay within the same month.
#'
#' @param date_from sets the starting date in format: DD.MM.YYYY
#' @param date_to sets the ending date in format: DD.MM.YYYY
#' @param uenb_type [50Hz (4), TenneT (2), Amprion (3), TransnetBW (1), Netzregelverbund (6), IGCC (11)]
#' @param rl_type [SRL, MRL, RZ_SALDO, REBAP, ZUSATZMASSNAHMEN, NOTHILFE]
#'
#' @return data.frame variable containing the operating reserve call table
#'
#' @examples
#' getOperatingReserveCalls('07.03.2017', '14.03.2017', '4', 'SRL')
#'
#' @export
#'
getOperatingReserveCalls <- function(date_from, date_to, uenb_type, rl_type) {

  # Do the POST request and retrieve the response from the server
  r <- scrape_rl_calls(date_from, date_to, uenb_type, rl_type)
  # Preprocess the response
  p <- preprocess_rl_calls(r)
  # Build up the data.frame
  d <- build_df_rl_calls_auctions(p, "temp.csv")

  return(d)

}








