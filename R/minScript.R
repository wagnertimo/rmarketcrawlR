#'
#' The minScript
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' This sscript file contains a minimal version with all necessary functions to crawl, preprocess, callculate the approximated 1min operating reserve calls and calculate the marginal workprices.
#' All printings in the console are omitted.
#'
#' TODO: Performance testings: --> parallel computing and maybe substitution of for loops with lapply() functions
#'
#' @references @seealso scrapeData.R
#' @references @seealso preprocessData.R
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'

#' @title setLogging
#'
#' @description This function sets a global options variable called "logging" to TRUE OR FALSE. By default it is FALSE, so no logging is displayed.
#'
#' @param logger - A boolean variable. TRUE for printing out the logging outputs in the console.
#'
#'
#' @export
#'
setLogging <- function(logger) {
  options("logging" = logger)
  ifelse(logger == TRUE, print("Outputs/logs will be displayed!"), print("No console outputs/logs will be displayed!"))
}

# -----------------------------------------------------------------------------------------------------------------------------------------
# MAIN FUNCTION getReserveNeds


#' @title getReserveNeeds
#'
#' @description This function is the production method of @seealso getOperatingReserveNeeds in the @seealso scrapeData.R script. It is without console prints for faster computation and several steps are combined.
#' This main function retrieves the operating reserve needs from \url{https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf}.
#' The resolution is 4sec. The function can take awhile since it has to download sever MBs of data. The oldest data that can be retrieved is July (07) 2010.
#' The method also preprocesses the data in a nice format.
#' Variables in the returned data.frame:  - MW (numeric). The signed power needs for every 4sec. Point as decimal delimiter
#'                                        - DateTime (POSIXct). DateTime object in Y-m-d h:m:s format. Be aware of daylight saving. 4sec time windows.
#'
#' @param startDate sets the starting date. Format (german style): DD.MM.YYYY
#' @param endDate sets the ending date. Format (german style): DD.MM.YYYY
#'
#' @return data.frame variable containing the 4sec operating reserve needs.
#'
#' @examples
#' needs <- getReserveNeeds("30.12.2015", "02.01.2016")
#'
#' @export
#'
getReserveNeeds <- function(startDate, endDate) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getReserveNeeds_", Sys.time(), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  df <- preprocessOperatingReserveNeeds(getOperatingReserveNeeds(startDate, endDate))

  return(df)

}


# -----------------------------------------------------------------------------------------------------------------------------------------
# MAIN FUNCTION getReserveCalls


#' @title getReserveCalls
#'
#' @description This function is the production method of @seealso getOperatingReserveCalls in the @seealso scrapeData.R script. It is without console prints for faster computation and several steps are combined.
#' This main function retrieves the 15min operating reserve calls (not qualified) from \url{https://www.regelleistung.net/ext/data/}.
#' The oldest data that can be retrieved is 2011-06-27.
#' The method also preprocesses the data in a nice format.
#' Variables in the returned data.frame:  - neg_MW (numeric). The signed power calls within the 15min time window. Point as decimal delimiter
#'                                        - pos_MW (numeric). The signed power calls within the 15min time window. Point as decimal delimiter
#'                                        - DateTime (POSIXct). DateTime object in Y-m-d h:m:s format. Be aware of daylight saving. 15min time windows.
#'
#'
#' @param startDate - sets the starting date. Format (german style): DD.MM.YYYY
#' @param endDate - sets the ending date. Format (german style): DD.MM.YYYY
#' @param uenb - 50Hz (4), TenneT (2), Amprion (3), TransnetBW (1), Netzregelverbund (6), IGCC (11)
#' @param rl - SRL, MRL, RZ_SALDO, REBAP, ZUSATZMASSNAHMEN, NOTHILFE
#'
#' @return data.frame variable containing the operating reserve call table
#'
#' @examples
#' # Get the secondary calls for one week of the Netzregelverbund
#' calls <- getReserveCalls('07.03.2017', '14.03.2017', '6', 'SRL')
#'
#' @export
#'
getReserveCalls <- function(startDate, endDate, uenb, rl) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getReserveCalls_", Sys.time(), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  df <- preProcessOperatingReserveCalls(getOperatingReserveCalls(startDate, endDate, uenb, rl))

  return(df)

}



# -----------------------------------------------------------------------------------------------------------------------------------------
# MAIN FUNCTION getReserveAuctions


#' @title getReserveAuctions
#'
#' @description This function is the production method of @seealso getOperatingReserveAuctions in the @seealso scrapeData.R script. It is without console prints for faster computation and several steps are combined.
#' This main function retrieves the operating reserve auction results from \url{https://www.regelleistung.net/ext/tender/}.
#' The data contains all auctions from a given start date till an end date. Be aware of the weekly data and take care of the latest week.
#' Variables in the returned data.frame:  - power_price (numeric). The signed power price of the bid. Point as decimal delimiter.
#'                                        - work_price (numeric). The signed work price of the bid. Point as decimal delimiter. There are also negative prices.
#'                                        - offered_power_MW (numeric). The offered power in MW. Point as decimal delimiter. 5 as the minimal bid. This will be changed someday to smaller increments.
#'                                        - Tarif (char). Contains the value of either "HT", the main tarif or "NT", for the remaining time periods.
#'                                        - Direction (char). Contains the value of either "POS", for positive operating reserve power, or "NEG" for negative power.
#'                                        - date_from (Date). Date object in Y-m-d format. The two date variables build the one week auction time period. This will be changed someday.
#'                                        - date_to (Date). Date object in Y-m-d format. The two date variables build the one week auction time period. This will be changed someday.
#'
#' @param startDate the start date to retrieve all auctions. Format (german style): DD.MM.YYYY (e.g.'07.03.2017')
#' @param endDate the end date to retrieve all auctions. Format (german style): DD.MM.YYYY (e.g.'07.03.2017')
#' @param rl PRL (1), SRL (2), MRL (3), sofort abschaltbare Lasten (4), schnell abschaltbare Lasten (5), Primärregelleistung NL (6)
#'
#' @return data.frame with the results of the auctions held from start date till end date
#'
#' @examples
#' # Get the auction results of the secondary operating reserve power for one week of that specific day
#' auctions <- getReserveAuctions('07.03.2017','07.03.2017', '2')
#'
#' @export
#'
getReserveAuctions <- function(startDate, endDate, rl) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getReserveAuctions_", Sys.time(), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  df <- preprocessOperatingReserveAuctions(getOperatingReserveAuctions(startDate, endDate, rl))

  return(df)

}




#' @title getOneMinuteCalls
#'
#' @description This production function calls the @seealso approximateCallsInRecursion() method in @seealso preprocessData.R script.
#' It calculaates the approximated 1 min calls out of the 4sec reserve needs.
#' Variables in the returned data.frame:  - neg_MW (numeric). The signed power calls within the 15min time window. Point as decimal delimiter
#'                                        - pos_MW (numeric). The signed power calls within the 15min time window. Point as decimal delimiter
#'                                        - approx_1min_call (numeric). The signed, approximated 1min calls. Point as decimal delimiter
#'                                        - DateTime (POSIXct). DateTime object in Y-m-d h:m:s format. Be aware of daylight saving. 1min time windows.
#'
#' @param needs - The preprocessed operating reserve needs (@seealso getReserveNeeds)
#' @param calls - The preprocessed operating reserve calls (@seealso getReserveCalls)
#' @param numCores - OPTIONAL PARAMETER --> PARALLEL COMPUNTING NOT YET FUNCTIONAL
#'
#' @return A complete data.frame with the corrected operating reserve needs to approximate the 1min calls (avg_1min_MW).
#'
#' @examples
#'
#' needs <- getReserveNeeds('07.03.2017', '14.03.2017')
#' calls <- getReserveCalls('07.03.2017', '14.03.2017', '6', 'SRL')
#'
#' OneMinuteCalls <- get1minCalls(needs, calls)
#'
#'
#' @export
#'
getOneMinuteCalls <- function(needs, calls, numCores) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getOneMinuteCalls_", Sys.time(), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  # If the parameter numCores is set then the user wants to use parallel computing
  if(missing(numCores)) {
    res <- approximateCallsInRecursion(needs, calls)
  } else {
    # NOT YET FUNCTIONAL
    res <- parallelCompWrapperForApproximation(needs, calls, numCores)
  }

  # Drop also time zone if used later anytime re-add it @seealso addTimezone in @seealso preprocessData.R scipt file
  drops <- c("cuttedTime", "TZ", "NEG", "POS", "product_name", "Homo_NEG", "Homo_POS", "avg_15min_MW_NEG", "avg_15min_MW_POS")

  res <- res[ , !(names(res) %in% drops)]

  names(res)[names(res)=="avg_1min_MW"] <- "approx_1min_call"

  return(res)

  if(getOption("logging")) loginfo("getOneMinuteCalls - DONE")

}



#' @title getMarginalWorkPrice
#'
#' @description This production function directly calculates the marginal work prices for every minute (based on the approximated 1min calls).
#' If you already should have calculated the 1min approximated calls then use the @seealso calcMarginalWorkPrices in the @seealso preprocessData.R file.
#'
#' @param needs - The preprocessed operating reserve needs (@seealso getReserveNeeds)
#' @param calls - The preprocessed operating reserve calss (@seealso getReserveCalls)
#' @param auctions - The preprocessed operating reserve auctions (@seealso getReserveAuctions)
#' @param numCores - This is an optional parameter. If set then the parallelWrapper is used. Specify the number of processor cores. Do not exceed your number of Cores.
#'
#' @return A complete data.frame with the 1min approximated calls and the corresponding marginal work prices for every minute
#'
#' @examples
#'
#' needs <- getReserveNeeds('07.03.2017', '14.03.2017')
#' calls <- getReserveCalls('07.03.2017', '14.03.2017', '6', 'SRL')
#' # The time period of the auctions must lie within the period of the calls and needs. An auction week goes from monday till sunday.
#' auctions <- getReserveAuctions('06.03.2017', '19.03.2017', '2')
#'
#' df <- getMarginalWorkPrices(needs, calls, auctions)
#'
#'
#' @export
#'
getMarginalWorkPrices <- function(needs, calls, auctions, numCores) {
  library(logging)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  nameLogFile <- paste("getMarginalWorkPrices_", Sys.time(), ".txt", sep="")
  addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  # If the parameter numCores is set then the user wants to use parallel computing
  if(missing(numCores)) {
    df <- calcMarginalWorkPrices(approximateCallsInRecursion(needs, calls), auctions)
  } else {
    df <- parallelCompWrapperForMarginalWorkPrices(approximateCallsInRecursion(needs, calls), auctions, numCores)
  }


  drops <- c("cuttedTime", "TZ", "NEG", "POS", "product_name", "Homo_NEG", "Homo_POS", "avg_15min_MW_NEG", "avg_15min_MW_POS")

  df <- df[ , !(names(df) %in% drops)]

  names(df)[names(df)=="avg_1min_MW"] <- "approx_1min_call"

  if(getOption("logging")) loginfo("getMarginalWorkPrices - DONE")

  return(df)

}



