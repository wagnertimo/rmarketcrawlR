#'
#' The preprocessData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' @import data.table
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

#'---------------------------------------------------------

#'
#' HELPER FUNCTIONS
#'


#' @title isGermanHoliday
#'
#' @description This method states if the given datetime (POSIXct object) is a german bank holiday. It is used e.g. in @seealso addTarif
#'
#' @param dateTime - A dateTime object
#'
#' @return a boolean value TRUE if the given dateTime is a german holiday or not (FALSE)
#'
#' @export
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





#'---------------------------------------------------------

#'
#' MAIN FUNCTIONS
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
  library(logging)
  library(data.table)
  library(lubridate)
  library(dplyr)
  library(magrittr)

  # Build variable DateTime out of Date and Time (as String) and neg_MW (with a negative num) and pos_MW
  # Time takes the value of "UHRZEIT.VON". The seconds are missing so add ":00"

  # TODO: Handle daylight savings CET and CEST --> Keep them as they are --> two 2 oclock hours
  if(getOption("logging")) loginfo("preProcessOperatingReserveCalls - Formatting POSIXct DateTime object")
  df.calls$DateTime <- as.POSIXct(paste(ymd(df.calls$DATUM), paste(df.calls$UHRZEIT.VON, ":00", sep = ""), sep=" "), tz = "Europe/Berlin")
  # Change, if there is an end of daylight savings in the time period, the two 2 oclock hours into CEST and CET

  # needs data.table library --> sets new colomun names
  setnames(df.calls, "BETR..POS", "pos_MW")
  setnames(df.calls, "BETR..NEG", "neg_MW")

  df.calls <- addTimezone(df.calls)

  keeps <- c("DateTime", "neg_MW", "pos_MW", "TZ")
  #keeps <- c("DateTime", "neg_MW", "pos_MW")

  if(getOption("logging")) loginfo("preProcessOperatingReserveCalls - DONE")

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
  library(logging)
  library(data.table)
  library(dplyr)
  library(magrittr)

  # Build up a Date-Time object POSIXct for easier handling. Set the timezone to Middle Europe Time
  # format: e.g. 2017-30-12 00:00:04

  # TODO: Handle daylight savings CET and CEST
  # Change, if there is an end of daylight savings in the time period, the two 2 oclock hours 2A and 2B into CEST and CET
  if(getOption("logging")) loginfo("preprocessOperatingReserveNeeds - Changing factor variable Time to character")
  df.needs$Time <- as.character(df.needs$Time)

  # Find all rows with Time string variable starting with "2A" or "2B"and mutate its value by adding a "02" and concat it with its own minutes and seconds by splitting the string.
  if(getOption("logging")) loginfo("preprocessOperatingReserveNeeds - Changing character string of daylight saving times")
  df.needs[df.needs$Time %like% "^2A" | df.needs$Time %like% "^2B",] %<>% mutate(Time = paste("02:",
                                                                                              sapply(strsplit(Time, ":"), "[", 2),
                                                                                              ":",
                                                                                              sapply(strsplit(Time, ":"), "[", 3),
                                                                                              sep = ""))

  # This code block is omitted, no differentiation btwn 2hour at CEST and 2hour at CET
  # 2A gets format 02:XX:XX CEST
  # Find all rows with Time string variable starting with "2A" or "2B"and mutate its value by adding a "02" and concat it with its own minutes and seconds by splitting the string.
  # df.needs[df.needs$Time %like% "^2A",] %<>% mutate(Time = paste("02:",
  #                                                     sapply(strsplit(Time, ":"), "[", 2),
  #                                                     ":",
  #                                                     sapply(strsplit(Time, ":"), "[", 3),
  #                                                     sep = ""))
  #
  # df.needs[df.needs$Time %like% "^2B",] %<>% mutate(Time = paste("02:",
  #                                                    sapply(strsplit(Time, ":"), "[", 2),
  #                                                    ":",
  #                                                    sapply(strsplit(Time, ":"), "[", 3),
  #                                                    " +0100",
  #                                                    sep = ""))

  # Then set the POSIXct datetime format
  if(getOption("logging")) loginfo("preprocessOperatingReserveNeeds - Formatting POSIXct datetime")
  df.needs$DateTime <- as.POSIXct(paste(df.needs$Date, df.needs$Time, sep=" "), tz = "Europe/Berlin")

  df.needs <- addTimezone(df.needs)

  # Not sure when and if needed. This variable is redundant
  # df.needs$Direction <- ifelse(df.needs$MW < 0, "NEG", "POS")

  drops <- c("Date", "Time")

  if(getOption("logging")) loginfo("preprocessOperatingReserveNeeds - DONE")

  return(df.needs[ , !(names(df.needs) %in% drops)])

}

# CAUTION!!! ---> IF df spans over several years !!!
# This helper method is used in @seealso preprocessOperatingReserveNeeds and @seealso preprocessOperatingReserveCalls
# It adds the column TZ to the in put data.frame which extracts the time zone (CEST or CET) out of the date
# It also handles the special case of daylight savings such that the second 2am hour gets the time zone CET on the last sunday in october.
addTimezone <- function(df) {
  library(logging)
  library(dplyr)

  if(getOption("logging")) loginfo("addTimezone - Add time zone")

  df$TZ <- format(df[, "DateTime"], format="%Z")

  # CAUTION!!! ---> IF df spans over several years !!!
  year <- as.numeric(unique(format(df$DateTime , "%Y")))
  # Check if in df is the change of daylight saving --> last sunday (1) in october (10) for the given year (format(DateTime, "%Y"))
  # LITTLE HACK with "01:59:59" --> magik server has problems to understand 02:00:00 as CEST. Only than as CEST when before a 1 hour of CEST is called
  fr <- filter(df, DateTime >= as.POSIXct(paste(lastDayOfMonth(1,10,year), "01:59:59", sep = " ")) & DateTime < as.POSIXct(paste(lastDayOfMonth(1,10,year), "03:00:00", sep = " ")))

  if (nrow(fr) > 0){
    # get the rows where the two 2am hours of the daylight saving change in october lay
    rows <- which(df$DateTime >= as.POSIXct(paste(lastDayOfMonth(1,10,year), "01:59:59", sep = " ")) & df$DateTime < as.POSIXct(paste(lastDayOfMonth(1,10,year), "03:00:00", sep = " ")))
    # the first 4 are the "old" (CEST) and the last 4 are the new (CET) 2am hours for the 15min calls --> 4*15 = 60
    # for needs it is the first 60 and last 60 because they are minutely --> so solve it with length of rows
    end <- length(rows)/2 + 1
    df[rows[1:length(rows)/2],]$TZ <- "CEST"
    df[rows[end:length(rows)],]$TZ <- "CET"
  }
  df$TZ <- as.factor(df$TZ)

  return(df)
}

# This function returns a Date object (YYYY-MM-DD) of the last given weekday and month
# E.g. The Last sunday(day = 7) in october (month = 10) in 2016 (year = 2016) is 2016-10-30
# 1 = sunday, 2 = monday, ... 7 = saturday // 1 = january, 2 = February, ... 12 = december
lastDayOfMonth <- function(day, month, year){
  library(lubridate)
  library(zoo)

  lastDate = as.Date(zoo::as.yearmon(paste(year,"-",month,"-01",sep = "")), frac = 1)
  # 1 = sunday , 2 = monday ... 7 saturday
  lastWeekDay = wday(lastDate)
  diff = lastWeekDay - day
  if(diff == 0) {
    return(lastDate)
  }
  else {
    # e.g target sunday = 1 and lastWeekDay monday = 2 --> diff 2 - 1 = 1 --> shift lastDate back 1 (diff) day(s)
    # e.g target sunday = 1 and lastWeekDay tuesday = 3 --> diff 3 - 1 = 2 --> shift lastDate back 2 (diff) day(s)
    # e.g target wednesday = 4 and lastWeekDay tuesday = 3 --> diff 3 - 4 = -1 --> if negative --> 7 - diff = 6 --->shift lastDate back 6 (diff) day(s)
    # e.g target tuesday = 3 and lastWeekDay monday = 2 --> diff 2 - 3 = -1 --> if negative --> 7 - diff = 6 --->shift lastDate back 6 (diff) day(s)
    if(diff < 0) {
      # shift lastDate back by 7 - diff
      shiftback = 7  + diff
    }
    else {
      # diff positive --> shift lastDate back by diff
      shiftback = diff
    }

    return(lastDate - shiftback)
  }
}





#' @title addTarif
#'
#' @description This method adds an additional variable called Tarif to the already preprocessed input data.frame which contains the right DateTime (@seealso preprocessOperatingReserveNeeds or preprocessOperatingReserveCalls). It depends on the helper method @seealso isGermanHoliday
#' @param df - a data.frame which has to be preprocessed and additional information about the tarif is needed. The input data.frame has to have at least a DateTime variable (POSIXct object) with hourly resolution.
#' @return the input data.frame with the additional variable of the tarif
#'
#' @export
#'
addTarif <- function(df) {
  library(logging)
  library(lubridate)
  # HT is Mon - Fri 8 - 20 without bank holiday
  # NT is else
  # Get week day: 1 sunday 2 monday 3 tuesday 4 wednesday ... 7 saturday
  df$Tarif <- ifelse((hour(df$DateTime) >= 8 & hour(df$DateTime) < 20) & (wday(df$DateTime) > 1 & wday(df$DateTime) < 7) & !isGermanHoliday(df$DateTime), "HT", "NT")

  if(getOption("logging")) loginfo("addTarif - DONE")

  return(df)
}




#' @title addDirection
#'
#' @description This method adds an additional variable called Direction to the already preprocessed input data.frame which contains the corrected avg_1min_MW
#' @param df - a data.frame with the corrected avg_1min_MW
#' @return the input data.frame with the additional variable of the Direction
#'
#' @export
#'
addDirection <- function(df) {
  library(logging)

  # Add a Directions column for later use. Being able to group
  df$Direction <- as.factor(ifelse(df$avg_1min_MW < 0, "NEG", "POS"))

  if(getOption("logging")) loginfo("addDirection - DONE")

  return(df)
}




#' @title preprocessOperatingReserveAuctions
#'
#' @description This method transforms the original retrieved operating reserve auctions into a nicer data.frame.
#'
#' @param df.auctions - the data.frame containing the original retrieved operating reserve auctions
#' @param rl - the reserve power product: PRL (1), SRL (2) ... @seealso getReserveAuctions()
#'
#' @return a modified data.frame containing the Type ("RZBedarf"), DateTime (as POSIXct object), the MW and the direction variable (POS or NEG)
#'
#' @export
#'
preprocessOperatingReserveAuctions <- function(df.auctions, rl) {
  library(logging)

  if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions")


  if(rl == "2") {

    if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - Preprocessing for SRL")


    if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - Adding Tarif and Direction variables")
    df.auctions$Tarif <- rapply(strsplit(as.character(df.auctions$product_name), "_"), function(x) x[2])
    df.auctions$Direction <- rapply(strsplit(as.character(df.auctions$product_name), "_"), function(x) x[1])

    # Set the direction/sign of the work price --> ANBIETER_AN_NETZ signals a negative work price
    # CAUTION!!! till last week of (including) 2014-12-29 the declaration is "Anbieter an Netz" then from 2015-01-05 "ANBIETER_AN_NETZ"
    df.auctions$work_price <- ifelse(df.auctions$ap_payment_direction == "ANBIETER_AN_NETZ" | df.auctions$ap_payment_direction == "Anbieter an Netz", -df.auctions$work_price, df.auctions$work_price)

  }
  else if(rl == "3") {

    if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - Preprocessing for MRL")

    # Set the direction/sign of the work price --> ANBIETER_AN_NETZ signals a negative work price
    # CAUTION!!! till last week of (including) 2014-12-29 the declaration is "Anbieter an Netz" then from 2015-01-05 "ANBIETER_AN_NETZ"
    df.auctions$work_price <- ifelse(df.auctions$ap_payment_direction == "ANBIETER_AN_NETZ" | df.auctions$ap_payment_direction == "Anbieter an Netz", -df.auctions$work_price, df.auctions$work_price)

  }

  # drop some useless columns --> USER DECIDES IF USELESS
  #drops <- c("offers_AT", "called_power_MW", "ap_payment_direction", "product_name")
  #df.auctions = df.auctions[ , !(names(df.auctions) %in% drops)]

  return(df.auctions)

  if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - DONE")

}
















