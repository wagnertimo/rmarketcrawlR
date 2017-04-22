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

#'---------------------------------------------------------

#'
#' HELPER FUNCTIONS FOR CALL DATA
#'

#' @title scrape_rl_calls
#'
#' @description This function scrapes the reserve calls and returns the POST response in its raw format.
#'
#' @param date_from
#' @param date_to
#' @param uenb_type
#' @param rl_type
#'
#' @return a text content of a POST response. It has to be formatted @seealso preprocess_rl_calls.
#'
#'
#' @export
#'
scrape_rl_calls <- function(date_from, date_to, uenb_type, rl_type) {

  library(httr)

  if(getOption("logging")) print(paste("[INFO]: Called scrape_rl_calls"))

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

#' This function handles the firstly needed preprocessing in the crawling step. It just formats the POST response of @seealso scrape_rl_calls.
#' Further preprocessing is taken by the functions in the @seealso preprocessData.R script.
#'
#' @export
#'
preprocess_rl_calls <- function(response_content) {

  library(xml2)

  if(getOption("logging")) print(paste("[INFO]: Called preprocess_rl_calls"))

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


# This method is used to build a data.frame with monthly date time periods because the calls can only be retrieved within a month.
# Therefore a input time period has to be split in monthly time periods.
#'
#' @export
#'
getDatesArrayOfMonths <- function(d1.start, d1.end) {

  library(lubridate)

  if(getOption("logging")) print(paste("[INFO]: Called getDatesArrayOfMonths"))

  d1.start <- as.Date(d1.start, "%d.%m.%Y")
  d1.end <- as.Date(d1.end, "%d.%m.%Y")

  # calculate the number of monthly date time frames. Be aware of timeframes greater than a year
  length <- (month(d1.end) - month((d1.start)) + 1) + (year(d1.end) - year(d1.start)) * 12

  # init
  d <- d1.start
  dates <- data.frame()

  for(e in 1:length) {

    # Check if the end date was reached
    if(endDateOfTheMonth(d) <= d1.end) {
      # build the monthly timeframe with the start date or start date of the month till the end date of the month
      date <- data.frame(start_date = d, end_date = endDateOfTheMonth(d))

      dates <- rbind(dates, date)
      # Get the next start date of the next month
      d <- startDateOfTheMonth(nextMonthDate(d))

    }
    else{

      date <- data.frame(start_date = d, end_date = d1.end)
      dates <- rbind(dates, date)

      break;
    }
  }

  # Format into german date format
  dates$start_date <- format(dates$start_date, "%d.%m.%Y")
  dates$end_date <- format(dates$end_date, "%d.%m.%Y")

  return(dates)
}

# This helper method returns the end date of the month of a given date
endDateOfTheMonth <- function(date) {
  library(zoo)

  return(as.Date(as.yearmon(date), frac = 1))
}

# This helper method returns the start date of the month of a given date
startDateOfTheMonth <- function(date){
  library(zoo)

  return(as.Date(as.yearmon(date), frac = 0))

}

# This helper method returns the last date of the next month of a given date
nextMonthDate <- function(date){

  library(lubridate)

  month(date) <- month(date) + 1
  day(date) <- days_in_month(month(date))

  return(date)

}


#'---------------------------------------------------------

#
# HELPER FUNCTIONS FOR AUCTIONS DATA
#

scrape_rl_auctions <- function(date_from, productId) {

  library(httr)

  if(getOption("logging")) print(paste("[INFO]: Called scrape_rl_auctions"))

  url = 'https://www.regelleistung.net/ext/tender/';

  payload = list(
    'from' = date_from,
    'productId' = productId
  );

  postResponse <- POST(url, body = payload, encode = "form", verbose())

  return(postResponse)

}


# Crawl the reults table of the auctions to get the auctionIds within the given timeframe. The results of the auction table contain all acutionIds from the given start date till the current date
getAuctionIds <- function(response, date_from, date_to) {

  library(XML)

  if(getOption("logging")) print(paste("[INFO]: Called getAuctionIds"))

  # Calculate the time difference in weeks of the given start and end date. This value sets the needed amount of auctionIds from the response since they are weekly auctions
  tdiff <- floor(as.double(difftime(as.Date(date_to, format = "%d.%m.%Y") ,as.Date(date_from, format = "%d.%m.%Y") , units = c("weeks")))) + 1

  parsedHtml <- htmlParse(content(response, "text"))

  # Xpath exppression to retrieve all links in the results (tender) table
  # There are three links in all table column containing the auctionId -> just get a unique/distinct link (contains 'details')
  link_elements <- xpathSApply(parsedHtml, "id('tender-table')/tbody/tr/td/a[contains(@href,'details')]/@href")

  auctionIds <- c()

  # Stop when last weekly auction specified by the given end date is reached. No need to parse all elements (result lasts till current/latest date)
  for(i in 1:tdiff) {

    # Split the link on the slashes '/' and take only the last element, this is the auctionId
    splitstring <- strsplit(link_elements[i], "/")[[1]]
    auctionId <- splitstring[length(splitstring)]

    auctionIds <- append(auctionIds, auctionId)
  }

  return(auctionIds)

}


callGETforAuctionResults <- function(auctionId) {

  library(httr)

  if(getOption("logging")) print(paste("[INFO]: callGETforAuctionResults - Called for ", auctionId))

  url = paste('https://www.regelleistung.net/ext/tender/results/anonymousdownload/',auctionId, sep = "");

  getResponse <- GET(url, verbose())

  return(content(getResponse, "text"))

}



#'---------------------------------------------------------

#
# COMMON HELPER FUNCITON (CALL AND AUCTION)
#

# Ignore the Warning message: header and 'col.names' are of different lengths
# This is a strange error it still works
#
# # TODO improve this function --> causes arbitrary errors while writing and reading in the temp csv file
#
#'
#' @export
#'
build_df_rl_calls_auctions <- function(response_content, case, fileName) {

  # Write a temporary csv file out of the preprocessed response data.
  # This whole approach with the temp.csv file allows to process bigger files.
  #
  # Write a temporary csv file from the char variable
  write.csv(response_content, file = fileName, eol = "\n")

  # This if statement builds the data.frame for the operating reserve calls
  if(case == "calls") {

    if(getOption("logging")) print(paste("[INFO]: build_rl_calls_auctions - Called for Reserve Calls. Read in file"))

    # Read in the temporary csv file
    #
    # Writing the csv file does not remove the "..." parenthesis of the char variable. Furthermore it adds an extra line at the top: "","x" and at the beginning of the second line: "1",
    # Therefore the read in function uses the parameters:
    #     quote = "" (get rid of parenthesis)
    #     skip = 1 (to get rid off the extra line at the beginning)
    df <- read.csv2(file = fileName, header = TRUE, sep = ";", dec = ",", na.strings = c("","-"), quote = "", skip = 1)

    # Rename the first date column which has a cryptic name because of the "1",)
    colnames(df)[1] <- "DATUM"

    # --> get rid of not needed columns and format variables. BETR..NEG and BETR..POS are numeric values. As factors they caused a problem
    df <- df[, c("DATUM", "UHRZEIT.VON", "BETR..NEG", "BETR..POS")]

    df$DATUM <- as.Date(df$DATUM, "%d.%m.%Y")
    # Change the number style
    df$BETR..NEG <- -as.numeric(formatGermanNumber(df$BETR..NEG))
    df$BETR..POS <- as.numeric(formatGermanNumber(df$BETR..POS))

  }
  # This if statement builds the data.frame for the operating reserve auctions
  else if(case == "auctions") {

    if(getOption("logging")) print(paste("[INFO]: build_rl_calls_auctions - Called for Reserve Auctions. Read in file"))

    df <- read.csv(file = fileName,
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

    df$date_to <- as.Date(df$date_to, "%d.%m.%Y")
    df$date_from <- as.Date(df$date_from, "%d.%m.%Y")

  }


  # DELETE temporary files
  #
  invisible(if (file.exists(fileName)) file.remove(fileName))

  return(df)

}


# This helper method converts a german number into a classical number systems with just a point as a decimal limiter
# German number is defined: commas as decimal limiter and points as thounds seperator (e.g. 133.456.298,0433)
# Classical number is defined: only a point as decimal delimitir (e.g. 133456298.0433)
#
formatGermanNumber <- function(x){
  z <- gsub("[^0-9,.]", "", x)
  z <- gsub("\\.", "", z)
  gsub(",", ".", z)
}


#'---------------------------------------------------------

#
# HELPER FUNCTIONS FOR NEED DATA
#
# Requirement of secondary operating reserve energy (Bedarf an SRL)
# Data Dowload: https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf
# --> 4sec SRL requirement of the Regelnetzverbund since July (07) 2010 monthly data
#
# [http://www.50hertz.com/de/Maerkte/Regelenergie/Regelenergie-Downloadbereich --> no zip and only yearly]
#

# This helper function downloads the zip file and returns a data.frame of a given date code.
# The date code is specified by the year and month e.g. "201612". All the files have a standard naming.
#
# TODO improve this function --> causes arbitrary errors while writing and reading in the temp csv file
#
scrape_rl_need_month <- function(date_code) {

  if(getOption("logging")) print(paste("[INFO]: scrape_rl_need_month - Scrape data for ", date_code))

  # Create a temporary file to store the downloaded zip file in it
  tempFileName <- paste("needs_", date_code, sep = "")

    temp <- tempfile(tempFileName, "./")
  url = paste('https://www.transnetbw.de/files/bis/srlbedarf/', date_code, '_SRL_Bedarf.zip', sep = "");

  if(getOption("logging")) print(paste("[INFO]: scrape_rl_need_month - Download data for ", date_code))

  download.file(url, temp)

  if(getOption("logging"))print(paste("[INFO]: scrape_rl_need_month - Read csv for ", date_code))

  # Unzip in read in the csv file into a data.frame
  #
  # TODO Handle exceptions of 01.2017, 11.2016 and 04.2016 --> at this date codes there is no datacode in the unziped file name just "SRL_Bedarf.csv"
  #       --> Maybe even more month!
  #       --> So do a step by step process
  #           ---> unzip file by getting the the first file. Then get the name of the csv and read it in. Delete all files after use.
  #

  # Get the filename of the zip. It has a strange cryptic ending concatenated, no glue why. But it starts with the "needs_" prefix
  zipF <- list.files("./")[startsWith(list.files("./"), "needs_")]
  # unzip the temp file
  unzip(zipF)

  # delete the temporary file. Not needed anymore after csv is unzipped
  file.remove(zipF)

  # get the csv file name of the unzipped temp file. This step is important because sometimes the filname changes.
  # So no faster process is possible
  csvf <- list.files("./")[endsWith(list.files("./"), ".csv")]

  # Read in the unzipped csv file
  if(getOption("logging")) print(paste("[INFO]: scrape_rl_need_month - Read in csv file for ", date_code))
  dft <-  read.csv(csvf, header = FALSE, sep = ",", dec = ".")

  # delete the csv file
  if(getOption("logging")) print(paste("[INFO]: scrape_rl_need_month - Delete csv file for ", date_code))
  file.remove(csvf)

  # Since there are no headers, include appropriate header names
  colnames(dft) <- c("Date", "Time", "Type", "MW")



  return(dft)

}

# THis is a little helper function to deal with preceeding zeros in the numbers under 10. This is needed to handle the date format of months
# The parameter month is in the string format M (e.g. for august 8 which has to be converted into 08 and for november 11 it stays)
preceedingZerosForMonths <- function(month) {

  if(as.integer(month) < 10) {
    month <- paste("0",month, sep = "")
  }

  return(month)
}

# This function builds up an array containing all the date codes needed to build the whole data.frame.
getDateCodesArray <- function(date_from, date_to) {

  if(getOption("logging")) print(paste("[INFO]: getDateCodesArray"))

  # Init
  dateCodes <- c()
  date_to_month <- strsplit(date_to, "\\.")[[1]][2]
  date_to_year <- strsplit(date_to, "\\.")[[1]][3]
  # Defines the stop criteria for the while loop
  date_to_code <- paste(date_to_year, date_to_month, sep = "");
  date_month <- strsplit(date_from, "\\.")[[1]][2]
  date_year <- strsplit(date_from, "\\.")[[1]][3]

  if(getOption("logging")) print(paste("[INFO]: getDateCodesArray - Building the dateCodes in while loop"))

  # Fill the dateCodes array by counting up the number of months (and year if there is a year change) since the end date (date_to_code) is reached
  repeat{
    # Build up the date code
    dateCode <- paste(date_year, date_month, sep = "")

    if(getOption("logging")) print(paste("[INFO]: Building the dateCode ", dateCode))

    # Append it to the result array
    dateCodes <- c(dateCodes, dateCode)
    # Check if end date is reached
    if(dateCode == date_to_code){
      break
    }
    # Next date
    # Count up to the next month, but be aware of a year change
    if (date_month == "12") {
      date_year <- toString((as.integer(date_year) + 1))
    }
    # Don't forget the preceeding zeros!
    date_month <- preceedingZerosForMonths(toString((as.integer(date_month) + 1) %% 12))
    date_month <- ifelse(date_month == "00","12", date_month)
  }

  return(dateCodes)
}

# This method uses all the date codes within the specified time period to merge the individual data.frames together
buildDataFrameForDateCodes <- function(dateCodes) {

  if(getOption("logging")) print(paste("[INFO]: buildDataFrameForDateCodes - Looping through dateCodes to scrape data"))

  # Init progress bar
  if(getOption("logging")) pb <- txtProgressBar(min = 0, max = length(dateCodes), style = 3)

  # Init
  dfall <- data.frame()
  for(i in 1:length(dateCodes)){

    df <- scrape_rl_need_month(dateCodes[i])
    dfall <- rbind(dfall,df)

    # update progress bar
    if(getOption("logging")) setTxtProgressBar(pb, i)
  }

  # CLose the progress bar
  if(getOption("logging")) close(pb)

  # Change the factor Date variable to an actual Date Type
  dfall$Date <- as.Date(dfall$Date, format = "%Y/%m/%d")

  return(dfall)
}






#' @title mergeCSVFilesOfNeeds
#'
#' @description This method merges the monthly operating reserve needs which had been downloaded and unzip from the site https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/
#'
#' @param year - the year where all 12 monthly data are bind together into one data.frame
#'
#' @return A data.frame of the merged 12 month operating reserve needs (Date, Time, Type, MW)
#'
#' @examples
#' # The 12 month data has already been unziped and stored into ../data/needs
#' df.needs.2016 <- mergeCSVFilesOfNeeds(2016)
#'
#' @export
#'
mergeCSVFilesOfNeeds <- function(year) {

  library(lubridate)

  if(getOption("logging")) print(paste("[INFO]: Called mergeCSVFilesOfNeeds"))

  # inint the merging data.frame
  df <- data.frame()

  for(month in 1:12){

    if(getOption("logging")) print(paste("[INFO]: mergeCSVFilesOfNeeds - Merging for ", month, " ", year))

    # define the path where the file can be found
    path <-  paste("data/needs/", year, preceedingZerosForMonths(month), "_SRL_Bedarf.csv", sep = "")
    # load the data
    mydata = read.csv(path, header = F)
    # set the correct column names
    colnames(mydata) <- c("Date","Time","Type","MW")
    # bind the data row wise
    df <- rbind(df, mydata)
  }

  # format the Date variable
  df$Date <- as.Date(df$Date, format = "%Y/%m/%d")

  if(getOption("logging")) print(paste("[INFO]: mergeCSVFilesOfNeeds - DONE"))

  return(df)
}


#'---------------------------------------------------------

#
# MAIN FUNCTIONS
#


#' @title getOperatingReserveAuctions
#'
#' @description This main function retrieves the operating reserve auction results from \url{https://www.regelleistung.net/ext/tender/}.
#' The data contains all auctions from a given starting date till an end date. Be aware of the weekly data and take care of the latest week. It is already in the data table of the website but there is no downloadable data available.
#'
#'
#' @param date_from the starting date to retrieve all auctions till NOW in the date format DD.MM.YYYY (e.g.'07.03.2017')
#' @param date_to the end date to retrieve all auctions. Format DD.MM.YYYY (e.g.'07.03.2017')
#' @param productId PRL (1), SRL (2), MRL (3), sofort abschaltbare Lasten (4), schnell abschaltbare Lasten (5), Primärregelleistung NL (6)
#'
#' @return data.frame with the results of the auctions held from starting date till now
#'
#' @examples
#' getOperatingReserveAuctions('07.03.2017', '2')
#'
#' @export
#'
getOperatingReserveAuctions <- function(date_from, date_to, productId) {

  if(getOption("logging")) print(paste("[INFO]: Called getOperatingReserveAuctions"))

  # Retrieve all auctions (scrape the auction table) since the the given start date
  auctionsResponse <- scrape_rl_auctions(date_from, productId)
  # Extract the auctionIds from the scraped auction table to retrieve the actual auction data
  auctionIds <- getAuctionIds(auctionsResponse, date_from, date_to)

  if(getOption("logging")) print(paste("[INFO]: getOperatingReserveAuctions - GET request and build of auctions"))

  # Init progress bar // CAUTION --> the length of auctionIds can be longer than needed (retrieves all auctionIds but stops at the input end date)
  if(getOption("logging")) pb <- txtProgressBar(min = 0, max = length(auctionIds), style = 3)

  # Get the first (initial) auction data and add it to the df_auctions data.frame
  response_content <- callGETforAuctionResults(auctionIds[1])
  filename <- paste("temp_auctions_", auctionIds[1], sep = "")
  df_auctions <- build_df_rl_calls_auctions(response_content, "auctions", filename)

  # If only one auctionId (= just auction data of one day) is called, then stop and return the initial auction data
  if(length(auctionIds) > 1) {

    for(j in 2:length(auctionIds)) {

      # Stop if the end date for the auctions is reached
      # TODO FIX BUG.... SEEMS NOT TO STOP --> BETTER TO ALREADY LIMIT THE AUCTION IDS!!!!
      if(df_auctions$date_to < as.Date(date_to, "%d.%m.%Y")) {

        response_content <- callGETforAuctionResults(auctionIds[j])
        # TODO improve this function --> causes arbitrary errors while writing and reading in the temp csv file
        filename <- paste("temp_auctions_", auctionIds[j], sep = "")
        df <- build_df_rl_calls_auctions(response_content, "auctions", filename)

        df_auctions <- rbind(df_auctions, df)

        # update progress bar
        if(getOption("logging")) setTxtProgressBar(pb, j)
      }
      else {
        break;
      }
    }
  }

  # CLose the progress bar
  if(getOption("logging")) close(pb)

  # Delete All temporary files in the data/auctions directory
  #invisible(do.call(file.remove, list(list.files("data/auctions", full.names = TRUE))))

  if(getOption("logging")) print(paste("[INFO]: getOperatingReserveAuctions - DONE"))

  return(df_auctions)


}




#' @title getOperatingReserveCalls
#'
#' @description This main function retrieves the operating reserve calls from \url{https://www.regelleistung.net/ext/data/}.
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

  if(getOption("logging")) print(paste("[INFO]: Called getOperatingReserveCalls"))

  # First split the input timeframe into processable monthly dates
  # Then loop through the monthly timeframes and process like before.
  dates <- getDatesArrayOfMonths(date_from, date_to)

  df <- data.frame()

  # Init progress bar
  if(getOption("logging")) pb <- txtProgressBar(min = 0, max = nrow(dates), style = 3)

  for(e in 1:nrow(dates)) {

    if(getOption("logging")) print(paste("[INFO] getOperatinReserveCalls - POST request for timeframe: ", dates[e,1], " - ", dates[e,2], sep = ""))

    # Do the POST request and retrieve the response from the server
    r <- scrape_rl_calls(dates[e,1], dates[e,2], uenb_type, rl_type)
    # Preprocess the response
    p <- preprocess_rl_calls(r)


    # Build up the data.frame
    # TODO improve this function --> causes arbitrary errors while writing and reading in the temp csv file
    filename <- paste("temp_calls_", e, sep = "")
    d <- build_df_rl_calls_auctions(p, "calls", filename)

    df <- rbind(df, d)

    # update progress bar
    if(getOption("logging")) setTxtProgressBar(pb, e)

  }

  # CLose the progress bar
  if(getOption("logging")) close(pb)
  # Delete All temporary files in the data/calls directory
  #invisible(do.call(file.remove, list(list.files("data/calls", full.names = TRUE))))

  if(getOption("logging")) print(paste("[INFO]: getOperatingReserveCalls - DONE"))

  return(df)
}



#' @title getOperatingReserveNeeds
#'
#' @description This main function retrieves the operating reserve needs from \url{https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf}. The resolution is 4sec. The function can take awhile since it has to download sever MBs of data. The oldest data that can be retrieved is July (07) 2010.
#'
#' @param date_from sets the starting date in format: DD.MM.YYYY
#' @param date_to sets the ending date in format: DD.MM.YYYY
#'
#' @return data.frame variable containing the operating reserve need table for the specified time period
#'
#' @examples
#' getOperatingReserveNeeds("30.12.2015", "02.01.2016")
#'
#' @export
#'
getOperatingReserveNeeds <- function(startDate, endDate) {

  if(getOption("logging")) print(paste("[INFO]: Called getOperatingReserveNeeds"))

  # Extract all the dataCodes to build the whole data.frame by downloading the zip file
  df <- buildDataFrameForDateCodes(getDateCodesArray(startDate, endDate))

  # Delete All temporary files in the data/calls directory
  #invisible(do.call(file.remove, list(list.files("data/needs", full.names = TRUE))))

  if(getOption("logging")) print(paste("[INFO]: getOperatingReserveNeeds - Subset the data.frame"))

  # Subset the whole data.frame to the given time period
  df <- subset(df, Date >= as.Date(startDate, format = "%d.%m.%Y") & Date <= as.Date(endDate, format = "%d.%m.%Y"))

  if(getOption("logging")) print(paste("[INFO]: getOperatingReserveNeeds - DONE"))


  return(df)

}




