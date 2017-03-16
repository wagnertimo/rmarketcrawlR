#'
#' The scrapeData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main and helper functions to crawl call and auction data out of
#' @references \url{https://www.regelleistung.net/ext/data/}
#' @references \url{https://www.regelleistung.net/ext/tender/}
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'




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

  r <- POST(url, body = payload, encode = "form", verbose())

}


preprocess_rl_calls <- function(post_response) {

  library(xml2)

  # Preprocess the response data
  #
  # Get the response content as a text
  response_content <- content(post_response, "text")
  # Delete the first 5 rows (unneccessary additional infos)
  # Therefore split first the text
  response_content <-strsplit(response_content, "\n")
  # Now skip/delete the first 5 rows
  response_content <- response_content[[1]][5:length(response_content[[1]])]
  # Paste the char vector back again to a char variable
  response_content <- paste(response_content, sep = "", collapse = "\n")

  return(response_content)

}


build_df_rl_calls <- function(response_content) {

  # Write a temporary csv file out of the preprocessed response data.
  # This whole approach with the temp.csv file allows to process bigger files.
  #
  # Write a temporary csv file from the char variable
  write.csv(response_content, file = "temp.csv", eol = "\n")

  # Read in the temporary csv file
  #
  # Writing the csv file does not remove the "..." parenthesis of the char variable. Furthermore it adds an extra line at the top: "","x" and at the beginning of the second line: "1",
  # Therefore the read in function uses the parameters:
  #     quote = "" (get rid of parenthesis)
  #     skip = 1 (to get rid off the extra line at the beginning)
  df <- read.csv(file = "temp.csv", header = TRUE, sep = ";", dec = ",", na.strings = c("","-"), quote = "", skip = 1)
  # Rename the first date column which has a cryptic name because of the "1",)
  colnames(df)[1] <- "DATUM"

  # DELETE temporary temp.csv
  #
  invisible(if (file.exists("temp.csv")) file.remove("temp.csv"))

  return(df)

}

#' @title getOperatingReserveCalls
#'
#' @description This main functions retrieves the operating reserve calls from @references \url{https://www.regelleistung.net/ext/data/}
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
getOperatingReserveCalls <- function(date_from, date_to, uenb_type, rl_type) {

  # Do the POST request and retrieve the response from the server
  r <- scrape_rl_calls(date_from, date_to, uenb_type, rl_type)
  # Preprocess the response
  p <- preprocess_rl_calls(r)
  # Build up the data.frame
  d <- build_df_rl_calls(p)

  return(d)

}














