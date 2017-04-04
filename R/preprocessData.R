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

  # Build variable DateTime out of Date and Time (as String) and neg_MW (with a negative num) and pos_MW
  # Time takes the value of "UHRZEIT.VON". The seconds are missing so add ":00"
  df.calls$DateTime <- as.POSIXct(paste(dmy(df.calls$DATUM), paste(df.calls$UHRZEIT.VON, ":00", sep = ""), sep=" "), tz = "MET")
  df.calls$pos_MW <- df.calls$BETR..POS
  df.calls$neg_MW <- -df.calls$BETR..NEG

  keeps <- c("DateTime", "neg_MW", "pos_MW")
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

  # Build up a Date-Time object POSIXct for easier handling. Set the timezone to Middle Europe Time
  # format: e.g. 2017-30-12 00:00:04
  df.needs$DateTime <- as.POSIXct(paste(df.needs$Date, df.needs$Time, sep=" "), tz = "MET")

  # Not sure when and if needed. This variable is redundant
  # df.needs$Direction <- ifelse(df.needs$MW < 0, "NEG", "POS")

  drops <- c("Date", "Time", "Type")

  return(df.needs[ , !(names(df.needs) %in% drops)])

}




#' @title addTarif
#'
#' @description This method adds an additional variable called Tarif to the already preprocessed input data.frame which contains the right DateTime (@seealso preprocessOperatingReserveNeeds or preprocessOperatingReserveCalls)
#' @param df - a data.frame which has to be preprocessed and additional information about the tarif is needed
#' @return the input data.frame with the additional variable of the tarif
#'
#' @export
#'
addTarif <- function(df) {

  library(lubridate)

  # HT is Mon - Fri 8 - 20 without bank holiday
  # NT is else
  # Get week day: 1 sunday 2 monday 3 tuesday 4 wednesday ... 7 saturday
  df$Tarif <- ifelse((hour(df$DateTime) >= 8 & hour(df$DateTime) < 20) & (wday(df$DateTime) > 1 & wday(df$DateTime) < 7) & !isGermanHoliday(df$DateTime), "HT", "NT")

  return(df)
}



#' This helper method states if the given datetime (POSIXct object) is a german bank holiday. It is used in @seealso addTarif
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
#' @description This method averages the data.frame of the operating reserve needs on a X minute time frame (e.g. 1 minute) from the input needs (4sec resolution).
#'
#' @param df.needs - the data.frame containing the operating reserve needs (in MW), a Date (as String) and a Time (as String) variable. As well as type ("RZBedarf")
#' @param xmin - a timeframe in minutes to build the average values
#'
#' @return data.frame with the Xmin (normally 1min) DateTime (POSIXct object) and the average operating reserve need value of the Xmin time window
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' aggregateXminAVGMW(df.needs, 1)
#'
#' @export
#'
aggregateXminAVGMW <- function(df.needs, xmin) {

  print(paste("[INFO]: aggregateXminAVGMW - Cut time"))

  # Cut the Date-Times into Minutes such that every 4sec observation belongs to a bigger group of minutes
  df.needs$cuttedTime <- cut(df.needs$DateTime, breaks = paste(xmin, "min", sep = " "))
  df.needs$cuttedTime <- as.POSIXct(df.needs$cuttedTime, tz = "MET")

  print(paste("[INFO]: aggregateXminAVGMW - Calculate average"))

  # Create a data.frame with the mean values of the required MW in operating reserve power for every minute based on the cutted time
  df.needs.avg <- aggregate(x = df.needs$MW,
                            by = list(df.needs$cuttedTime),
                            FUN = mean)
  # Modify/format the new average data.frame
  colnames(df.needs.avg) <- c("DateTime", paste("avg_", xmin, "min_MW", sep=""))

  return(df.needs.avg)
}




#' @title get15minAVGOff1minAVG
#'
#' @description This method takes a data.frame of 1 min average values of operating reserve needs (@seealso aggregateXminAVGMW) and calculates the average values of a specific time window.
#'
#' @param dataframe - the data.frame with operating reserve needs (already preprocessed) and cuttedTime variable 15min is passed in (save computation time!)
#' @param xmin - the time window for which the average should be calculated (should be 15)
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
get15minAVGOf1minAVG <- function(dataframe, direction) {

  # print(paste("[INFO]: get15minAVGOf1minAVG for direction ", direction, " - Cut time and build 15min blocks"))

  # Sample down the 1min avg values from the 4sec operating reserve need data
  # Choose only the 4sec DateTime and the 1 min average need
  # dataframe <- dataframe[, c("DateTime","avg_1min_MW", "Tarif")]
  # Cut the 4sec DateTime into 1 minute sections
  # dataframe$DateTime <- as.POSIXct(cut(dataframe$DateTime, breaks = "1 min"), tz = "MET")
  # Only let 1 of the 15 1 min avg values remaining
  # dataframe <- unique(dataframe)

  # Build the 15min blocks
  # dataframe$cuttedTime <- cut(dataframe$DateTime, breaks = "15 min")
  # dataframe$cuttedTime <- as.POSIXct(dataframe$cuttedTime, tz = "MET")

  # filter the operating reserve power direction
  ifelse(direction == "NEG", dataframe2 <- dataframe[dataframe$avg_1min_MW < 0,], dataframe2 <- dataframe[dataframe$avg_1min_MW >= 0,])

  # In the case of pure homogenity (only POS direction or only NEG) then the dataframe is empty
  if(nrow(dataframe2) != 0){

    print(paste("[INFO]: get15minAVGOf1minAVG for direction ", direction, " - calculate average"))

    # Create a data.frame with the mean values of the required MW in operating reserve power for every minute based on the cutted time
    df <- aggregate(x = dataframe2$avg_1min_MW,
                               by = list(dataframe2$cuttedTime),
                               FUN = function(x){sum(x) / 15})
    colnames(df) <- c("cuttedTime", paste("avg_15min_MW_", direction, sep=""))

    # Fill up the missing 15min sections with 0s. This can happen if some homogene 15min sections are in dataframe2
    #t <- data.frame(unique(dataframe$cuttedTime), 0)
    #colnames(t) <- c("cuttedTime", paste("avg_15min_MW_", direction, sep=""))
    #print(" T T  : ")
    #print(t)

  }
  else {
    # For pure homogenity fill the dataframe with 0s for the avg value. Number of 0s equals nrows of the input data.frame
    df <- data.frame(unique(dataframe$cuttedTime), 0)
    colnames(df) <- c("cuttedTime", paste("avg_15min_MW_", direction, sep=""))
  }

  #print(df)

  return(df)
}



#' @title getNumberOfPosOrNegIn15min
#'
#' @description This method adds the counts of NEG and POS 1min averages for every 15min section.
#'
#' @param dataframe - THe input data.frame has to have already a cuttedTime variable with 15min sections! (trying to minimize the amount of cut operations due to its computational intensivity)
#'
#' @return a data.frame with corresponding counting values for NEG and POS and adds additionally a Direction variable (NEG, POS)
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

  # print(paste("[INFO]: getNumberOfPosOrNegIn15min - Cut time in 15min blocks"))

  # Cut the Date-Times into 15 Minutes section and format it in a Date (POSIXct) object
  # dataframe$cuttedTime <- cut(dataframe$DateTime, breaks = paste("15", "min", sep = " "))
  # dataframe$cuttedTime <- as.POSIXct(dataframe$cuttedTime, tz = "MET")

  # Scrape out all the 1min averages of the 4sec data by using unique
  # df.temp <- unique(dataframe[, c("cuttedTime","avg_1min_MW")])
  # Add a Directions column for later use. Being able to group
  dataframe$Direction <- as.factor(ifelse(dataframe$avg_1min_MW < 0, "NEG", "POS"))

  print(paste("[INFO]: getNumberOfPosOrNegIn15min - Group by and counting"))

  # Goal now to count for every 15 minute sections seperatly the NEG and POS averages from the 1min data
  # Use piping statements for convenience. Group by the 15min sections and the NEG and POS and count their NEG and POS appereance.
  test <- dataframe %>%
    group_by(cuttedTime, Direction) %>%
    summarise(n= n())

  print(paste("[INFO]: getNumberOfPosOrNegIn15min - Reshaping input data.frame"))

  # tidyr function spread reshapes that counting table to be able to merge it with the input data.frame
  test <- spread(test, Direction, n)
  test[is.na(test)] <- 0

  # If there is no NEG in all the observations --> pure homogenity, then add a zero column for NEG. For POS vice versa.
  invisible(if(!("NEG" %in% colnames(test))) test$NEG <- 0)
  invisible(if(!("POS" %in% colnames(test))) test$POS <- 0)

  print(paste("[INFO]: getNumberOfPosOrNegIn15min - Mmerging with input data.frame"))

  r <- merge(dataframe, test, by = "cuttedTime")
  # If homogenity is the case then there are na entries. Therefore change them to 0
  # Throws arrow if only one (NEG or POS) can be count --> pue Homogenity for all observations(15min sections)
  # r[is.na(r)] <- 0

  # Pass the 15min cuttedTime Variable through. WIll be needed for further operations on the dataset. Saves computation time.
  return(r)
}





#' @title calcHomogenityCorrectness
#'
#' @description Takes as input a combined minutely data.frame with 15min calls and 1 min avg of needs as well as the counts of negative and positive needs within 15min. It then updates the 1min avg need and NEG and POS values based on the homogenity. NOTE: The warnings appear when in normal cases there is no smallest absolute value. The x variable becomes "na". That is the warning
#'
#' @param dataframe - dataframe with 15min neg and pos calls, 1min avg needs, NEG and POS counts and cuttedTime based on 15min.
#'
#' @return An updated dataframe where the NEG and POS counts in the homogenity cases are modified to 14/1 (from 15/0) and the smallest absolute value is updated to 0.
#'
#' @export
#'
calcHomogenityCorrectness <- function(dataframe) {

  #
  # POSSIBLE PROBLEM: WHAT IF MORE EQUAL SMALLEST ABSOLUTE VALUES
  #

  library(dplyr)

  print(paste("[INFO]: calcHomogenityCorrectness - Find the homogenity cases and its smallest absolute value"))

  # Approach:
  # Identify the homogenity cases of the input data.frame --> NEG (POS) count is 15 (only NEG or POS 1min avg needs) and the 15min call is positive (negative).
  h.special <- dataframe[(dataframe$NEG == 15 & dataframe$pos_MW > 0) | (dataframe$POS == 15 & dataframe$neg_MW < 0) ,]

  # h.special is empty if there is no homogenity case. In that case, return the input data.frame with $Homo_NEG = 0and $Homo_POS = 0
  if(nrow(h.special) != 0) {

    # Compute the absolute minimum of the special cases for every 15min section
    # function(x){ifelse(x < 0, max(x), min(x) )} --> does not really work --> x.1 .... x.15 variables created
    # Trying with the formula expression seems to work better
    h.min <- aggregate(avg_1min_MW ~ cuttedTime, h.special, function(x) x[which.min(abs(x))])

    colnames(h.min) <- c("cuttedTime", "x")

    # Merge the original dataframe with the minimum values
    dataframe <- left_join(dataframe, h.min, by = c("cuttedTime" = "cuttedTime"))

    print(paste("[INFO]: calcHomogenityCorrectness - Update the NEG and POS and 1min avg values"))

    # 2. Now update the 1min avg needs value such that the smallest absolute value gets the 0 value and the NEG and POS counts get 14/1
    dataframe$Homo_NEG <- 0
    dataframe$Homo_POS <- 0

    for(i in 1:nrow(dataframe)) {

      # If there is negative Homogenity
      if(dataframe[i,]$NEG == 15 & dataframe[i,]$pos_MW > 0) {

        dataframe[i,]$Homo_NEG <- 1
        dataframe[i,]$NEG <- dataframe[i,]$NEG - 1 # 14 hardcoded also a possibility
        dataframe[i,]$POS <- dataframe[i,]$POS + 1 # 1 hardcoded also a possibility

        # If the minimal absolute value is reached
        if(dataframe[i,]$avg_1min_MW == dataframe[i,]$x) {
          dataframe[i,]$avg_1min_MW <- 0;
        }

      }
      # If there is positive Homogenity
      else if(dataframe[i,]$POS == 15 & dataframe[i,]$neg_MW < 0) {

        dataframe[i,]$Homo_POS <- 1
        dataframe[i,]$POS <- dataframe[i,]$POS - 1 # 14 hardcoded also a possibility
        dataframe[i,]$NEG <- dataframe[i,]$NEG + 1 # 1 hardcoded also a possibility

        # If the minimal absolute value is reached
        if(dataframe[i,]$avg_1min_MW == dataframe[i,]$x) {
          dataframe[i,]$avg_1min_MW <- 0;
        }

      }
    }

    # Get rid of X variable, the smallest absolute value. Not needed anymore
    dataframe <- dataframe[, !(names(dataframe) %in% c("x"))]

  }
  else {

    dataframe$Homo_NEG <-  0
    dataframe$Homo_POS <-  0
  }


  return(dataframe)
}






#'
#' MAIN FUNCTIONS
#'

#' @title approximateCalls
#'
#' @description This method builds the data.frame with all needed variables to correct the operating reserve needs power for approximating the calls.
#'
#' @param reserveNeeds - The preprocessed operating reserve needs (@seealso preprocessOperatingReserveNeeds)
#' @param reserveCalls - The preprocessed operating reserve calls (@seealso preprocessOperatingReserveCalls)
#'
#' @return A complete data.frame with all needed variables for correcting the operating reserve needs and approximating the calls.
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.calls <- getOperatingReserveCalls('30.12.2015', '30.12.2015', '6', 'SRL')
#'
#' reserveCalls <- preProcessOperatingReserveCalls(df.calls)
#' reserveNeeds <- preprocessOperatingReserveNeeds(df.needs)
#'
#' approximateCalls(reserveNeeds, reserveCalls)
#'
#' @export
#'
approximateCalls <- function(reserveNeeds, reserveCalls) {

  library(dplyr)

  print(paste("[INFO]: Called approximateCalls"))

  # Calculate the 1min average operating reserve needs out of the 4sec data
  df.needs.1min <- aggregateXminAVGMW(df.needs, 1)

  print(paste("[INFO]: approximateCalls - Cut the 15min sections"))

  # Join with 15min calls
  # Cut 1min avg needs into 15min for join operation
  df.needs.1min$cuttedTime <- cut(df.needs.1min$DateTime, breaks = paste("15", "min", sep = " "))
  df.needs.1min$cuttedTime <- as.POSIXct(df.needs.1min$cuttedTime, tz = "MET")

  # merge the 1min needs and 15min calls based on the cuttedTime
  t.all <-  merge(df.needs.1min, df.calls, by.x = "cuttedTime", by.y = "DateTime")


  # Add the numbers of negative and positive needs within 15min to the data.frame
  t.all <- getNumberOfPosOrNegIn15min(t.all)

  #print(t.all)

  # Consider 1. special case: Homogenity
  # Now check for homogenity cases and modify the 1min avg values and numbers of NEG and POS
  # If homogenity is the case then set the smallest absolute value of 1min avg need to zero and the 15/0 counts to 14/1. The rest stays the same.


  h.c <- calcHomogenityCorrectness(t.all)

  print(h.c)

  # Now it is time to compute the 15min averages for the needs and make the correction calculation
  # Calculate the 15min average operating reserve needs for negative and positive power out of the 1min averages
  h.c.15min.neg <- get15minAVGOf1minAVG(h.c, "NEG")
  h.c.15min.pos <- get15minAVGOf1minAVG(h.c, "POS")


  # Merge everything together by the passed through cuttedTime variable (cutting time is computational intensive!!)
  # Left Join neccessary. In case of negative (positive) homogene 15min sections the averages of the positive (negative) values are missing NA
  res <- left_join(h.c, h.c.15min.neg, by="cuttedTime")%>%
    left_join(h.c.15min.pos, by = "cuttedTime")
  # set the missing homogene averages to 0
  res[is.na(res)] <- 0


  print(res)

  print(paste("[INFO]: approximateCalls - Calculate the corrected need values"))

  # Init the Correction variable
  res$Corrected <- 0

  for(i in 1:nrow(res)) {

    # Be aware of the case that there is homogenity and the smallest absolute value is zero --> then the if statement (<0) won't be activated.
    # THerefore a special else statement for positive homogenity is needeed
    if((res[i,]$Homo_NEG == 0 & res[i,]$Homo_POS == 0) | (res[i,]$Homo_NEG == 1 & res[i,]$Homo_POS == 0) | (res[i,]$Homo_NEG == 0 & res[i,]$Homo_POS == 1 & res[i,]$avg_1min_MW != 0)) {

      # Calculate the corrected operating need value or the new approximated 1min call
      res[i, ]$Corrected <- ifelse(res[i,]$avg_1min_MW < 0, res[i,]$avg_1min_MW + ((res[i,]$neg_MW - res[i,]$avg_15min_MW_NEG) * (15/res[i,]$NEG)),
                            res[i,]$avg_1min_MW + ((res[i,]$pos_MW - res[i,]$avg_15min_MW_POS) * (15/res[i,]$POS)))


      print(paste("NORMAL CASE: Corrected value for", i, " obs. of ", res[i,]$avg_1min_MW, " is --> ", res[i, ]$Corrected))
    }
    else {
      # a positive homogenity case occured

      # Correction for the special case homogenity: The 0 value marks the smallest absolute value.
      # For positive homogenity, the 0 value is corrected by the opposite avgs and counts (NEG)
      # THis else statement is needed because the ifelse() above doesn#t trigger the NEG calculation because of the < 0 expression
      res[i, ]$Corrected <- if(res[i,]$Homo_POS == 1) res[i,]$neg_MW  * 15

      print(paste("SPECIAL CASE: SMALLEST VALUE REACHED ", i, " - ", res[i,]$avg_1min_MW, " Corrected value: ", res[i, ]$Corrected))

      }
  }

  # TODO handle 2nd special case:
  # CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
  #


  # Formatting
  res <- res[, !(names(res) %in% c("cuttedTime"))]


  return(res)

}


