#'
#' The preprocessData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main and helper functions to preprocess the data which has been scraped @references scrapeData
#' Mainly it approximates the 15min operating reserve calls into a finer resolution of 1min to allow a more realistic represention than the average values.
#'
#' Some useful keyboard shortcuts for package authoring:
#'
#'  Build and Reload Package:  'Cmd + Shift + B'
#'  Check Package:             'Cmd + Shift + E'
#'  Test Package:              'Cmd + Shift + T'
#'



#'
#' HELPER FUNCTIONS
#'

#' @title preProcessOperatingReserveCalls
#'
#' @description This method transforms the original retrieved operating reserve calls into a smaller (less variables) data.frame.
#'
#' @param df.calls - the data.frame containing the original retrieved operating reserve calls
#'
#' @return a modified data.frame containing the DateTime (as POSIXct object), neg_MW (negative num) and pos_MW
#'
#' @export
#'
preProcessOperatingReserveCalls <- function(df.calls) {

  library(lubridate)

  # Build variable DateTime out of Date and Time (as String) and neg_MW (with a negative num) and pos_MW
  # Time takes the value of "UHRZEIT.VON". The seconds are missing so add ":00"
  df.calls$DateTime <- as.POSIXct(paste(dmy(df.calls$DATUM), paste(df.calls$UHRZEIT.VON, ":00", sep = ""), sep=" "), tz = "MET")
  df.calls$pos_MW <- df.calls$BETR..POS
  df.calls$neg_MW <- -df.calls$BETR..NEG
  # HT is Mon - Fri 8 - 20 without bank holiday
  # NT is else
  # Get week day: 1 sunday 2 monday 3 tuesday 4 wednesday ... 7 saturday
  df.calls$Tarif <- ifelse((hour(df.calls$DateTime) >= 8 & hour(df.calls$DateTime) < 20) & (wday(df.calls$DateTime) > 1 & wday(df.calls$DateTime) < 7) & !isGermanHoliday(df.calls$DateTime), "HT", "NT")

  keeps <- c("DateTime", "neg_MW", "pos_MW", "Tarif")
  #keeps <- c("DateTime", "neg_MW", "pos_MW")

  return(df.calls[, keeps])

}




#' @title preprocessOperatingReserveNeeds
#'
#' @description This method transforms the original retrieved operating reserve needs into a nicer data.frame.
#'
#' @param df.needs - the data.frame containing the original retrieved operating reserve needs
#'
#' @return a modified data.frame containing the Type ("RZBedarf"), DateTime (as POSIXct object), the MW and the direction variable (POS or NEG)
#'
#' @export
#'
preprocessOperatingReserveNeeds <- function(df.needs) {

  library(lubridate)

  # Build up a Date-Time object POSIXct for easier handling. Set the timezone to Middle Europe Time
  # format: e.g. 2017-30-12 00:00:04
  df.needs$DateTime <- as.POSIXct(paste(df.needs$Date, df.needs$Time, sep=" "), tz = "MET")

  df.needs$Direction <- ifelse(df.needs$MW < 0, "NEG", "POS")

  # HT is Mon - Fri 8 - 20 without bank holiday
  # NT is else
  # Get week day: 1 sunday 2 monday 3 tuesday 4 wednesday ... 7 saturday
  df.needs$Tarif <- ifelse((hour(df.needs$DateTime) >= 8 & hour(df.needs$DateTime) < 20) & (wday(df.needs$DateTime) > 1 & wday(df.needs$DateTime) < 7) & !isGermanHoliday(df.needs$DateTime), "HT", "NT")

  drops <- c("Date", "Time", "Type")

  return(df.needs[ , !(names(df.needs) %in% drops)])

}


#' This helper method states if the given datetime (POSIXct object) is a german bank holiday
#'
isGermanHoliday <- function(dateTime) {

  library(timeDate)
  library(lubridate)

  bool <- FALSE

  year <- year(dateTime)
  date <- lubridate::date(dateTime)

  # Epiphany(year = year)@Data == date | # 3 Könige
  # EasterSunday(year = year)@Data == date |
  # Pentecost(year = year)@Data == date  | # Pfingstsonntag
  # DEChristmasEve(year = year)@Data == date |
  # DECorpusChristi(year = year)@Data == date  |
  # AllSaints(year = year)@Data == date |



  bool <- ifelse(NewYearsDay(year = year)@Data == date |
                 GoodFriday(year = year)@Data == date |
                 EasterMonday(year = year)@Data == date |
                 LaborDay(year = year)@Data == date  |
                 DEAscension(year = year)@Data == date  |
                 PentecostMonday(year = year)@Data == date  |
                 DEGermanUnity(year = year)@Data == date  |
                 ChristmasDay(year = year)@Data == date |
                 BoxingDay(year = year)@Data == date,
                 TRUE, FALSE)

  return(bool)
}


#' @title aggregateXminAVGMW
#'
#' @description This method averages the data.frame of the operating reserve needs on a x minute time frame (e.g. 1 minute)
#'
#' @param df.needs - the data.frame containing the operating reserve needs (in MW), a Date (as String) and a Time (as String) variable. As well as type ("RZBedarf")
#' @param xmin - a timeframe in minutes to build the average values
#'
#' @return data.frame with the operating reserve needs (in MW), the type ("RZBedarf"), a DateTime (POSIXct object) and the average operating reserve need value of the x min time window
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' aggregateXminAVGMW(df.needs, 1)
#'
#' @export
#'
aggregateXminAVGMW <- function(df.needs, xmin) {

  # Cut the Date-Times into Minutes such that every 4sec observation belongs to a bigger group of minutes
  df.needs$cuttedTime <- cut(df.needs$DateTime, breaks = paste(xmin, "min", sep = " "))
  df.needs$cuttedTime <- as.POSIXct(df.needs$cuttedTime, tz = "MET")

  # Create a data.frame with the mean values of the required MW in operating reserve power for every minute based on the cutted time
  df.needs.avg <- aggregate(x = df.needs$MW,
                            by = list(df.needs$cuttedTime),
                            FUN = mean)
  # Modify/format the new average data.frame
  colnames(df.needs.avg) <- c("cuttedTime", paste("avg_", xmin, "min_MW", sep=""))
  df.needs.avg$cuttedTime <- as.POSIXct(df.needs.avg$cuttedTime, tz = "MET")

  # Merge the average values with the original data.frame by the cuttedTime
  df <- merge(df.needs, df.needs.avg, by = "cuttedTime")

  drops <- c("cuttedTime","Date", "Time")

  return(df[ , !(names(df) %in% drops)])
}




#' @title get15minAVGOff1minAVG
#'
#' @description This method takes a data.frame of 1 min average values of operating reserve needs (@seealso aggregateXminAVGMW) and calculates the average values of a specific time window.
#'
#' @param dataframe - the data.frame with operating reserve needs (already preprocessed)
#' @param xmin - the time window for which the average should be calculated (e.g. 15)
#' @param direction - specifies the positive or negative operating reserve need
#'
#' @return a data.frame with corresponding average values on the given time window ("cuttedTime")
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.needs.preprocessed <- preprocessOperatingReserveNeeds(df.needs)
#' df.avg.15min.neg <- getAVGXmin(df.needs.preprocessed, 15, "NEG")
#'
#' @export
#'
get15minAVGOff1minAVG <- function(dataframe, direction) {

  # Sample down the 1min avg values from the 4sec operating reserve need data
  # Choose only the 4sec DateTime and the 1 min average need
  dataframe <- dataframe[, c("DateTime","avg_1min_MW", "Tarif")]
  # Cut the 4sec DateTime into 1 minute sections
  dataframe$DateTime <- as.POSIXct(cut(dataframe$DateTime, breaks = "1 min"), tz = "MET")
  # Only let 1 of the 15 1 min avg values remaining
  dataframe <- unique(dataframe)

  # Build the 15min blocks
  dataframe$cuttedTime <- cut(dataframe$DateTime, breaks = "15 min")
  dataframe$cuttedTime <- as.POSIXct(dataframe$cuttedTime, tz = "MET")

  # filter the operating reserve power direction
  ifelse(direction == "NEG", dataframe <- dataframe[dataframe$avg_1min_MW < 0,], dataframe <- dataframe[dataframe$avg_1min_MW >= 0,])

  # Create a data.frame with the mean values of the required MW in operating reserve power for every minute based on the cutted time
  dataframe.avg <- aggregate(x = dataframe$avg_1min_MW,
                             by = list(dataframe$cuttedTime),
                             FUN = function(x){sum(x) / 15})
  colnames(dataframe.avg) <- c("cuttedTime", paste("avg_15min_MW_", direction, sep=""))

  return(dataframe.avg)
}



#' @title getNumberOfPosOrNegIn15min
#'
#' @description This method adds the counts of NEG and POS 1min averages for every 15min section.
#'
#' @param dataframe - the data.frame with preprocessed operating reserve needs and calls and their averages
#'
#' @return a data.frame with corresponding counting values for NEG and POS
#'
#' @examples
#' No example! This function is included in the buildCorrectingCallsDF function.
#'
#' @export
#'
getNumberOfPosOrNegIn15min <- function(dataframe) {

  library(dplyr)
  library(tidyr)
  library(magrittr)

  # Cut the Date-Times into 15 Minutes section and format it in a Date (POSIXct) object
  dataframe$cuttedTime <- cut(dataframe$DateTime, breaks = paste("15", "min", sep = " "))
  dataframe$cuttedTime <- as.POSIXct(dataframe$cuttedTime, tz = "MET")

  # Scrape out all the 1min averages of the 4sec data by using unique
  df.temp <- unique(dataframe[, c("cuttedTime","avg_1min_MW")])
  # Add a Directions column for later use. Being able to group
  df.temp$Direction <- as.factor(ifelse(df.temp$avg_1min_MW < 0, "NEG", "POS"))

  # Goal now to count for every 15 minute sections seperatly the NEG and POS 1min averages
  # Use piping statements for convenience. Group by the 15min sections and the NEG and POS and count their NEG and POS appereance.
  test <- df.temp %>%
    group_by(cuttedTime, Direction) %>%
    summarise(n= n())

  # tidyr function spread reshapes that counting table to be able to merge it with the input data.frame
  test <- spread(test, Direction, n)

  r <- merge(dataframe, test, by.x = "cuttedTime", by.y = "cuttedTime")

  # skip the cuttedTime Variable --> not needed anymore
  return(r[, -1])
}



#' @title buildCorrectingCallsDF
#'
#' @description This method builds up the data.frame with all needed variables to correct the operating reserve needs power for approximating the calls.
#'
#' @param df.1min - The data.frame with the 1min average operating reserve needs
#' @param df.15min.neg - The data.frame with the 15min average negative operating reserve needs
#' @param df.15min.pos - The data.frame with the 15min average positive operating reserve needs
#' @param df.15min.calls - The data.frame with the 15min operating reserve calls (both negative and positive. Already preprocessed)
#'
#' @return A complete data.frame with all needed variables for correcting the operating reserve needs and approximating the calls.
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.calls <- getOperatingReserveCalls('30.12.2015', '30.12.2015', '4', 'SRL')
#'
#' buildCorrectingCallsDF(df.needs, df.calls)
#'
#' @export
#'
buildCorrectingCallsDF <- function(df.needs, df.calls) {

  library(dplyr)
  library(magrittr)

  # Calculate the 1min average operating reserve needs out of the 4sec data
  df.1min <- aggregateXminAVGMW(s.needs, 1)

  # Calculate the 15min average operating reserve needs for negative and positive power out of the 1min averages
  df.15min.neg <- get15minAVGOff1minAVG(df.1min, "NEG")
  df.15min.pos <- get15minAVGOff1minAVG(df.1min, "POS")


  # Create common merge variable cuttedTime
  df.1min$cuttedTime <- cut(df.1min$DateTime, breaks = paste("15", "min", sep = " "))
  df.1min$cuttedTime <- as.POSIXct(df.1min$cuttedTime, tz = "MET")

  t.all <- full_join(df.1min, df.15min.neg, by="cuttedTime") %>%
    full_join(df.15min.pos, by="cuttedTime") %>%
    merge(df.calls, by.x = "cuttedTime", by.y = "DateTime")

  drops <- c("cuttedTime", "Tarif.y")
  t.all <- t.all[, !(names(t.all) %in% drops)]
  # Now also add the counters of 1min averages for NEG and POS in every 15min section
  t.all <- getNumberOfPosOrNegIn15min(t.all)

  # Rename the Tarif.x column name which was created in fact of the merge of df.1min and df.15min.calls
  names(t.all)[names(t.all) == 'Tarif.x'] <- 'Tarif'

  #
  # TODO: ADD THE SPECIAL CASE OF HOMOGENITY. LOOK IF CALLS ARE POSITIVE (OR NEGATIVE) BUT NEEDS ARE ALL NEGATIVE (OR POSITIVE)
  #       --> add a new variable 1 or 0 special case (true or false). And if true, add the closes MW
  #

  return(t.all)

}






#'
#' MAIN FUNCTIONS
#'



#' @title approximateOperatingReserveCalls
#'
#' @description THis function approximates a finer 1min resolution of the 15min operating reserve calls. It does this by modifying the values of the operating reserve needs which are available in a 4sec resolution.
#'
#' @param The data.frame of specific 4sec operating reserve needs which is used to approximate the a 1min resolution for the reserve calls.
#' @param The data.frame with the 15min operating reserve calls
#'
#' @return A data.frame which approximates the 1min reserve calls
#'
#' @examples
#' approximateOperatingReserveCalls(operatingReserveNeeds)
#'
#' @export
#'
approxOperatingReserveCalls <- function(reserveNeeds, reserveCalls) {

  # Build up a data.frame with all relevant averages and values to approximate the 1min calls
  r <- buildCorrectingCallsDF(reserveNeeds, reserveCalls)

  #' Correction:
  #'   standard case for POS 1min avg need: (15min call POS MW -  15min avg POS need MW) * count POS / 15
  #'   standard case for NEG 1min avg need: (15min call NEG MW -  15min avg NEG need MW) * count NEG / 15

  # 1. aggregate data.frame of 4 sec resolution in 1 min resolution
  r$cuttedTime <- cut(r$DateTime, breaks = paste("1", "min", sep = " "))
  r$cuttedTime <- as.POSIXct(r$cuttedTime, tz = "MET")

  # Scrape out all the 1min averages of the 4sec data by using unique
  r <- unique(r[, c("cuttedTime","avg_1min_MW", "avg_15min_MW_NEG", "avg_15min_MW_POS", "neg_MW", "pos_MW", "NEG", "POS")])

  # Calculate the corrected operating need value or the new approximated 1min call
  r$Corrected <- ifelse(df.temp$avg_1min_MW < 0, df.temp$avg_1min_MW + ((df.temp$neg_MW - df.temp$avg_15min_MW_NEG) * (15/df.temp$NEG)), df.temp$avg_1min_MW + ((df.temp$pos_M - df.temp$avg_15min_MW_POS) * (15/df.temp$POS)))


  # TODO handle
  # Two special cases:
  # 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative ) calls (in both direction)
  # 2. CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
  #


  # Formatting
  # Rename cuttedTime to DateTime
  names(r)[names(r) == 'cuttedTime'] <- 'DateTime'

  return(r)

}







