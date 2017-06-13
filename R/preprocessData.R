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
  library(logging)


  if(getOption("logging")) loginfo(paste("aggregateXminAVGMW - Cut time in ", xmin, "min"))

  # Cut the Date-Times into Minutes such that every 4sec observation belongs to a bigger group of minutes
  df.needs$cuttedTime <- cut(df.needs$DateTime, breaks = paste(xmin, "min", sep = " "))
  df.needs$cuttedTime <- as.POSIXct(df.needs$cuttedTime, tz = "Europe/Berlin")

  if(getOption("logging")) loginfo(paste("aggregateXminAVGMW - Calculate average of ",xmin,"min"))

  # Create a data.frame with the mean values of the required MW in operating reserve power for every minute based on the cutted time
  library(data.table)
  df.needs.avg <- setDT(df.needs)[, lapply(.SD, mean), by=.(cuttedTime, TZ), .SDcols = "MW"]
  setDF(df.needs.avg)


  # Modify/format the new average data.frame
  colnames(df.needs.avg) <- c("DateTime", "TZ", paste("avg_", xmin, "min_MW", sep=""))

  return(df.needs.avg)
}





#' @title get15minAVGOff1minAVG
#'
#' @description This method takes a data.frame of 1 min average values of operating reserve needs (@seealso aggregateXminAVGMW) and calculates the average values of a specific time window.
#'
#' @param dataframe - the data.frame with operating reserve needs (already preprocessed) and cuttedTime variable 15min is passed in (save computation time!)
#' @param direction - specifies the positive or negative operating reserve need
#'
#' @return a data.frame with corresponding average values on the given time window ("cuttedTime")
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.needs.preprocessed <- preprocessOperatingReserveNeeds(df.needs)
#' df.avg.15min.neg <- getAVGXmin(df.needs.preprocessed, "NEG")
#'
#' @export
#'
get15minAVGOf1minAVG <- function(dataframe, direction) {
  library(logging)

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

    if(getOption("logging")) loginfo(paste("get15minAVGOf1minAVG for direction ", direction, " - calculate average"))

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






#' @title get15minAVGs
#'
#' @description This helper method is used in @seealso approxRecursionWith15minChunk. It retrieves a data.frame and returns the same data.frame with added columns for NEG and POS 15min avg of the 1min needs
#'
#' @param dataframe - the data.frame with with operating reserve needs (already preprocessed) and cuttedTime variable 15min is passed in (save computation time!)
#'
#' @return the input data.frame with added columns for 15min neg and pos averages
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.needs.preprocessed <- preprocessOperatingReserveNeeds(df.needs)
#' df.avg.15min.neg <- getAVGXmin(df.needs.preprocessed, "NEG")
#'
#' @export
#'
get15minAVGs <- function(dataframe) {


  # Now it is time to compute the 15min averages for the needs and make the correction calculation
  # Calculate the 15min average operating reserve needs for negative and positive power out of the 1min averages
  h.c.15min.neg <- get15minAVGOf1minAVG(dataframe, "NEG")
  h.c.15min.pos <- get15minAVGOf1minAVG(dataframe, "POS")

  # Merge everything together by the passed through cuttedTime variable (cutting time is computational intensive!!)
  # Left Join neccessary. In case of negative (positive) homogene 15min sections the averages of the positive (negative) values are missing NA
  res <- left_join(dataframe, h.c.15min.neg, by = "cuttedTime")%>%
    left_join(h.c.15min.pos, by = "cuttedTime")
  # set the missing homogene averages to 0
  res[is.na(res)] <- 0

  return(res)
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
  library(logging)
  library(dplyr)
  library(tidyr)
  library(magrittr)

  # print(paste("[INFO]: getNumberOfPosOrNegIn15min - Cut time in 15min blocks"))

  # Cut the Date-Times into 15 Minutes section and format it in a Date (POSIXct) object
  # dataframe$cuttedTime <- cut(dataframe$DateTime, breaks = paste("15", "min", sep = " "))
  # dataframe$cuttedTime <- as.POSIXct(dataframe$cuttedTime, tz = "MET")

  # Scrape out all the 1min averages of the 4sec data by using unique
  # df.temp <- unique(dataframe[, c("cuttedTime","avg_1min_MW")])
  # Add a Directions column for later use. Being able to group --> use numerical value for parallel comp
  dataframe$Direction <- ifelse(dataframe$avg_1min_MW < 0, "NEG", "POS")

  if(getOption("logging")) loginfo(paste("getNumberOfPosOrNegIn15min - ", reformatDateTime(dataframe[1,]$DateTime), " - Group by and counting", sep=""))

  # Goal now to count for every 15 minute sections seperatly the NEG and POS averages from the 1min data
  # Use piping statements for convenience. Group by the 15min sections and the NEG and POS and count their NEG and POS appereance.
  test <- dataframe %>%
    group_by(cuttedTime, Direction) %>%
    summarise(n= n())

  if(getOption("logging")) loginfo(paste("getNumberOfPosOrNegIn15min - ", reformatDateTime(dataframe[1,]$DateTime), " - Reshaping input data.frame"))

  # tidyr function spread reshapes that counting table to be able to merge it with the input data.frame
  test <- spread(test, Direction, n)
  test[is.na(test)] <- 0

  # If there is no NEG in all the observations --> pure homogenity, then add a zero column for NEG. For POS vice versa.
  invisible(if(!("NEG" %in% colnames(test))) test$NEG <- 0)
  invisible(if(!("POS" %in% colnames(test))) test$POS <- 0)

  if(getOption("logging")) loginfo(paste("getNumberOfPosOrNegIn15min - ", reformatDateTime(dataframe[1,]$DateTime), " - Merging with input data.frame"))

  #r <- merge(dataframe, test, by = "cuttedTime")
  # If homogenity is the case then there are na entries. Therefore change them to 0
  # Throws arrow if only one (NEG or POS) can be count --> pue Homogenity for all observations(15min sections)
  # r[is.na(r)] <- 0

  # Pass the 15min cuttedTime Variable through. WIll be needed for further operations on the dataset. Saves computation time.
  #return(r)

  return(test)
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
  library(logging)
  #
  # POSSIBLE PROBLEM: WHAT IF MORE EQUAL SMALLEST ABSOLUTE VALUES
  #

  library(dplyr)

  if(getOption("logging")) loginfo(paste("calcHomogenityCorrectness - ", reformatDateTime(dataframe[1,]$DateTime), " - Find the homogenity cases and its smallest absolute value"))

  # Approach:
  # Identify the homogenity cases of the input data.frame --> NEG (POS) count is 15 (only NEG or POS 1min avg needs) and the 15min call is positive (negative).
  h.special <- dataframe[(dataframe$NEG == 15 & dataframe$pos_MW > 0) | (dataframe$POS == 15 & dataframe$neg_MW < 0) ,]

  # h.special is empty if there is no homogenity case. In that case, return the input data.frame with $Homo_NEG = 0 and $Homo_POS = 0
  if(nrow(h.special) != 0) {

    # Compute the absolute minimum of the special case
    # function(x){ifelse(x < 0, max(x), min(x) )} --> does not really work --> x.1 .... x.15 variables created
    # Trying with the formula expression seems to work better
    h.min <- aggregate(avg_1min_MW ~ cuttedTime, h.special, function(x) x[which.min(abs(x))])

    colnames(h.min) <- c("cuttedTime", "x")

    # Merge the original dataframe with the minimum values
    dataframe <- left_join(dataframe, h.min, by = c("cuttedTime" = "cuttedTime"))

    if(getOption("logging")) loginfo(paste("calcHomogenityCorrectness - ", reformatDateTime(dataframe[1,]$DateTime), " - Update the NEG and POS and 1min avg values"))

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






#' This helping method is called in the @seealso approxRecursionWith15minChunk function to update the avg_1min_MW variable in a recursively manner.
#' Here no extra variable Corrected is added!
#' It takes a data.frame as input with the avg_1min_MW variable and the additional homogenity variables.
#'
#' @export
#'
correctionCalculationForRecursion <- function(res) {
  library(logging)
  if(getOption("logging")) loginfo(paste("correctionCalculationForRecursion - ", reformatDateTime(res[1,]$DateTime), " - Update avg_1min_MW"))

  # Init the Correction variable
  # No extra variable needed for recursion.
  # res$Corrected <- 0

  for(i in 1:nrow(res)) {

    # Be aware of the case that there is homogenity and the smallest absolute value is zero --> then the if statement (<0) won't be activated.
    # THerefore a special else statement for positive homogenity is needeed
    if((res[i,]$Homo_NEG == 0 & res[i,]$Homo_POS == 0) | (res[i,]$Homo_NEG == 1 & res[i,]$Homo_POS == 0) | (res[i,]$Homo_NEG == 0 & res[i,]$Homo_POS == 1 & res[i,]$avg_1min_MW != 0)) {

      # Calculate the corrected operating need value or the new approximated 1min call
      res[i, ]$avg_1min_MW <- ifelse(res[i,]$avg_1min_MW < 0, res[i,]$avg_1min_MW + ((res[i,]$neg_MW - res[i,]$avg_15min_MW_NEG) * (15/res[i,]$NEG)),
                                     res[i,]$avg_1min_MW + ((res[i,]$pos_MW - res[i,]$avg_15min_MW_POS) * (15/res[i,]$POS)))

      # print(paste("NORMAL CASE: Corrected value for", i, " obs. of ", res[i,]$avg_1min_MW, " is --> ", res[i, ]$Corrected))
    }
    else {
      # a positive homogenity case occured --> Correction for the special case homogenity: The 0 value marks the smallest absolute value.
      # This else statement is needed because the ifelse() above doesn#t trigger the NEG calculation because of the < 0 expression
      # For positive homogenity, the 0 value is corrected by the opposite avgs and counts (NEG)
      res[i, ]$avg_1min_MW <- if(res[i,]$Homo_POS == 1) res[i,]$neg_MW  * 15

      # print(paste("SPECIAL CASE: SMALLEST VALUE REACHED ", i, " - ", res[i,]$avg_1min_MW, " Corrected value: ", res[i, ]$Corrected))
    }
  }


  return(res)

}






#' This is a helper method used in @seealso approximateCallsInRecursion
#' It handles the recursion.
#'
#' @param dataframe - a dataframe of a 15min section. Only 15 observations
#'
#' @return a 15min data.frame with the corrected/updated values of avg_1min_MW
#'
#'
approxRecursionWith15minChunk <- function(dataframe) {
  library(logging)
  res <- dataframe
  # Counts the number of recursions which will be logged/printed in the console
  counter <- 0

  # Do and repeat the recursion of counting NEG and POS, correcting for homogenity, calculating the 15min NEG and POS avg of the needs, correct the needs
  repeat{

    counter <- counter + 1
    if(getOption("logging")) loginfo(paste("approxRecursionWith15minChunk - ", reformatDateTime(res[1,]$DateTime), " - Recursion number: ", counter, sep = ""))

    # (re-)set the input data.frame
    df <- res[,!(names(res) %in% c("Corrected", "Direction", "NEG", "POS", "Homo_NEG", "Homo_POS", "avg_15min_MW_NEG", "avg_15min_MW_POS"))]
    # print(df)

    # Add the numbers of negative and positive needs within 15min to the data.frame
    z <- getNumberOfPosOrNegIn15min(df)
    # print(rr)

    # merge with the original data.frame since the getNumberOfPosOrNegIn15min funtion does not combine it with its input
    df <- merge(df, z, by = "cuttedTime")
    # print(df)

    # Handle Homogenity cases. Identify a homogene 15min section and set its smalles absolut value to 0 and reset/update the POS and NEG counters
    h.c <- calcHomogenityCorrectness(df)
    # print(h.c)

    # Calculate the 15min average operating reserve needs for negative and positive power out of the 1min averages. Combine it also already with the input data.frame
    res <- get15minAVGs(h.c)
    # print(res)

    # Compute the corrected needs values for approximating the 1min calls. Implemented correction logic. It updates the avg_1min_MW variable
    res <- correctionCalculationForRecursion(res)
    # print(res)


    #
    # TODO print/log average values of aggregated needs and real 15min calls and number of neg pos
    # --> There was an error --> recursion was in an infinite loop
    #
    min15avgNegNeeds <- get15minAVGOf1minAVG(res, "NEG")$avg_15min_MW_NEG
    min15avgPosNeeds <- get15minAVGOf1minAVG(res, "POS")$avg_15min_MW_POS
    min15CallNeg <- unique(res$neg_MW)
    min15CallPos <- unique(res$pos_MW)
    if(getOption("logging")) logdebug(paste("approxRecursionWith15minChunk - ", reformatDateTime(res[1,]$DateTime), " - 15min NEG avg needs: ", min15avgNegNeeds, " vs. 15min NEG calls: ", min15CallNeg, sep=""))
    if(getOption("logging")) logdebug(paste("approxRecursionWith15minChunk - ", reformatDateTime(res[1,]$DateTime), " - 15min POS avg needs: ", min15avgPosNeeds, " vs. 15min POS calls: ", min15CallPos, sep=""))

    # Check if the stop criteria of the recursion is met.
    # The 15min NEG and POS avg of the needs must equal the corresponding neg and pos 1min calls.
    # Normally it should be that one of the statements of NEG or POS avg is TRUE the other must also be true.
    # This coulld maybe differ when computational errors occur!
    # Use isTRUE(all.equal()) comparison instead of == because of floating point arithemtic
    if(isTRUE(all.equal(min15avgNegNeeds, min15CallNeg)) & isTRUE(all.equal(min15avgPosNeeds, min15CallPos))){

      if(getOption("logging")) loginfo(paste("approxRecursionWith15minChunk - ", reformatDateTime(res[1,]$DateTime), " - Averages equal"))

      # Update the new avg_15min_MW_NEG and avg_15min_MW_POS for the data.frame. Although it is now redundant because it is equal to neg_MW and pos_MW
      res <- get15minAVGs(res[,!(names(res) %in% c("avg_15min_MW_NEG", "avg_15min_MW_POS"))])

      break
    }
  }

  res$num_recursions <- counter
  return(res)

}







#' @title markCrossingZero
#'
#' @description This function adds 2 variables to the input data.frame. pos_CZ and neg_CZ. They have the value 1 if the corrected need crossed the zero line. The artificial zero values in the homogenity case are excluded.
#'
#' @param df - the input data.frame with a cuttedTime variable of 15min sections. The data.frame has to be preprocces and approximated (@seealso approximateCalls)
#'
#' @return The input data.frame with two additional variables (columns) namely pos_CZ and neg_CZ. CZ stands for CrossingZero - the special case.
#'
#' @examples
#' # ... data already crawled and preprocessed
#' df <- approximateCalls(preprocessed.needs, preprocessed.calls)
#'
#' df.with.marked.cz <- markCrossingZero(df)
#'
#' @export
#'
markCrossingZero <- function(df) {
  library(logging)

  if(getOption("logging")) loginfo("markCrossingZero - Loop through input and mark the negative and positive CZ")

  # Here the homogenity case is excluded. The smalelst absolute value is set artificially to zero in a homogene 15min section
  # Init the CrossingZero marking variables for negative (value goes from positive to negative) and positive case (value goes from neg to positive)
  df$neg_CZ <- 0
  df$pos_CZ <- 0

  for(i in 1:nrow(df)) {

    # Mark negative CrossingZero
    if(df[i,]$avg_1min_MW >= 0 & df[i,]$Corrected < 0 & df[i,]$Homo_POS != 1) {
      df[i,]$neg_CZ <- 1
    }
    # Mark positive CrossingZero
    else if(df[i,]$avg_1min_MW < 0 & df[i,]$Corrected > 0) {
      df[i,]$pos_CZ <- 1
    }
  }

  return(df)
}


#' @title countCrossingZeros
#'
#' @description This function needs the helper methods countNegativeCrossingZerosForDF and countPositiveCrossingZerosForDF. It then adds 2 additional variables to the input data.farme. n_neg_CZ and n_pos_CZ For every 15min section. The numbers in the columns represent the total amount of each CrossingZero type within the 15min section.
#'
#' @param df - the input data.frame with a cuttedTime variable of 15min sections. The data.frame has to be preprocces and approximated (@seealso approximateCalls)
#'
#' @return The input data.frame with two additional variables (columns) namely n_negCZ and n_pos_CZ. CZ stands for CrossingZero - the special case.
#'
#' @examples
#' # ... data already crawled and preprocessed
#' df <- approximateCalls(preprocessed.needs, preprocessed.calls)
#'
#' df.with.cz.counts <- countCrossingZeros(df)
#'
#' @export
#'
countCrossingZeros <- function(df) {

  library(dplyr)

  if(getOption("logging")) loginfo("countCrossingZeros - Create the positive and negative counting tables")

  # Init the data.frames such that all 15min sections of the input data.frame are initialized with value 0
  t.pos <- data.frame(unique(df$cuttedTime), 0)
  t.neg <- data.frame(unique(df$cuttedTime), 0)

  colnames(t.neg) <- c("cuttedTime", "n_neg_CZ")
  colnames(t.pos) <- c("cuttedTime", "n_pos_CZ")


  t.pos.n <- df %>%
    group_by(cuttedTime) %>%
    #summarise(n_neg_CZ = countNegativeCrossingZerosForDF(df), n_pos_CZ = countPositiveCrossingZerosForDF(df))
    do(data.frame(n_pos_CZ = countPositiveCrossingZerosForDF(.)))

  # Handle exceptions of no counts.
  if(nrow(t.pos.n) != 0) {
    t.pos <- left_join(t.pos,t.pos.n, by = c("cuttedTime"))[, c("cuttedTime","n")]
    colnames(t.pos) <- c("cuttedTime", "n_pos_CZ")
    t.pos[is.na(t.pos)] <- 0
  }


  t.neg.n <- df %>%
    group_by(cuttedTime) %>%
    #summarise(n_neg_CZ = countNegativeCrossingZerosForDF(df), n_pos_CZ = countPositiveCrossingZerosForDF(df))
    do(data.frame(n_neg_CZ = countNegativeCrossingZerosForDF(.)))

  # Handle exceptions of no counts.
  if(nrow(t.neg.n) != 0) {
    t.neg <- left_join(t.neg,t.neg.n, by = c("cuttedTime"))[, c("cuttedTime","n")]
    t.neg[is.na(t.neg)] <- 0
    colnames(t.neg) <- c("cuttedTime", "n_neg_CZ")
  }

  if(getOption("logging")) loginfo("countCrossingZeros - Merge negative and positive counting tables.")

  # Merge the negative and positive CrossingZero counts into one data.frame to join it with the input data.frame
  t3 <- merge(t.pos,t.neg, by = "cuttedTime")

  if(getOption("logging")) loginfo("countCrossingZeros - Left join the counting tables with the input")

  df <- left_join(df, t3, by = "cuttedTime")

  return(df)
}


#'
#' This helper method is used in the countCrossingZeros function.
#' It counts the corrected needs which were positive and are now negative within the input data.frame.
#'
countNegativeCrossingZerosForDF <- function(df) {
  if(count(df[df$avg_1min_MW >= 0 & df$Corrected < 0 & df$Homo_POS != 1,]) > 0) {
    #print("negative CrossingZero occured")
    count(df[df$avg_1min_MW >= 0 & df$Corrected < 0 & df$Homo_POS != 1,])
  }
}

#'
#' This helper method is used in the countCrossingZeros function.
#' It counts the corrected needs which were negative and are now positive within the input data.frame.
#'
countPositiveCrossingZerosForDF <- function(df) {

  # count the switch from negative to positive
  if(count(df[df$avg_1min_MW < 0 & df$Corrected >= 0,]) > 0){
    #print("positive CrossingZero occured")
    count(df[df$avg_1min_MW < 0 & df$Corrected >= 0,])
  }
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




#' @title imputeMissingCallsWithTSO
#'
#' @description This method replaces missing values for 15 minute calls. It uses the data of the 4 TSOs. The sum equals the values of the Netzregelverbund. If also for the TSO data is missing than it uses the data of the day before.
#' NOTE: If there is a missing negative value there is also a missing positive value (Checked for 2013,2012 and 2011)
#'
#' @param calls - the call data.frame with missing values which has to be imputed
#' @param tso.list - list of the call data of the 4 tso companies (data must be same time period as call data).
#'
#' @return the imputed call data.frame
#'
#' @examples
#' # Get call data with missing values
#' calls = getReserveCalls('01.01.2013', '31.12.2013', '6', 'SRL')
#'
#' # 50Hz (4)
#' calls.2013.4 = getReserveCalls('01.01.2013', '31.12.2013', '4', 'SRL')
#' # TenneT (2)
#' calls.2013.2 = getReserveCalls('01.01.2013', '31.12.2013', '2', 'SRL')
#' # Amprion (3)
#' calls.2013.3 = getReserveCalls('01.01.2013', '31.12.2013', '3', 'SRL')
#' # TransnetBW (1)
#' calls.2013.1 = getReserveCalls('01.01.2013', '31.12.2013', '1', 'SRL')
#' # Build the tso list
#' tso.list <- list(calls.2013.1,calls.2013.2,calls.2013.3,calls.2013.4)
#'
#' # Impute the data
#' imputed.calls <- imputeMissingCallsWithTSO(calls, tso.list)
#'
#' @export
#'
imputeMissingCallsWithTSO <- function(calls, tso.list) {
  library(logging)
  library(dplyr)
  library(reshape2)


  if(getOption("logging")) loginfo("imputeMissingCallsWithTSO - Impute missing values for calls")

  # Get the dates which contain the missing values // if there is a missing negative value there is also a missing positive value (Checked for 2013,2012 and 2011)
  missingdates <- calls[is.na(calls$neg_MW), "DateTime"]
  # Impute NA of tso data (if they have NAs) with they value of the day before
  imputed.tso.list <- impute(tso.list)

  # Sum up the values of the 4 TSOs --> this leads to the value of the Netzregelverbund
  m <- sumUp(imputed.tso.list,missingdates)

  # Replace missing values with the summed TSO values
  calls[calls$DateTime %in% missingdates, "neg_MW"] <- m$neg_MW
  calls[calls$DateTime %in% missingdates, "pos_MW"] <- m$pos_MW

  if(getOption("logging")) loginfo("imputeMissingCallsWithTSO - DONE")

  return(calls)
}


# Helper function for @seealso imputeMissingCallsWithTSO
# insert a list of TSO call data of a year
impute <- function(tso.list){

  resList <- list()
  # look at every TSO data.frame
  for(t in 1:length(tso.list)) {
    df <- tso.list[[t]]
    # Check if TSO has missing values
    if(nrow(df[is.na(df$neg_MW), ]) > 0) {
      df <- imputeMissingValuesOfTSO(df)
    }
    resList[[t]] <- df
  }

  return(resList)
}

# Used in @seealso impute
# Replaces missing values og´f TSO call data with the value of the day before
imputeMissingValuesOfTSO <- function(calls){
  # Get all missing observations
  df <- calls[is.na(calls$neg_MW),]

  for(i in 1:nrow(df)){
    # for every missing value, get the value of the date before --> DateTime in seconds so 24h == 86400sec
    dayBefore <- df[i,"DateTime"] - 86400
    # get the value
    valueNeg <- calls[calls$DateTime == dayBefore, "neg_MW"]
    valuePos <- calls[calls$DateTime == dayBefore, "pos_MW"]

    # Get the missing observation and replace the na with the values
    calls[calls$DateTime == df[i,"DateTime"], "neg_MW"] <- valueNeg
    calls[calls$DateTime == df[i,"DateTime"], "pos_MW"] <- valuePos

  }

  return(calls)
}

# Helper function for @seealso imputeMissingCallsWithTSO
# sums up the data for neg and pos SR of the 4 TSOs for the missing dates
sumUp <- function(tso.list, missingdates) {

  a <- filter(tso.list[[1]], DateTime %in% missingdates)
  b <- filter(tso.list[[2]], DateTime %in% missingdates)
  c <- filter(tso.list[[3]], DateTime %in% missingdates)
  d <- filter(tso.list[[4]], DateTime %in% missingdates) # --> has all values

  rr <- list(a,b,c,d) %>%
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="DateTime"), .)

  # Rename the result columns and drop some useless columns
  colnames(rr) <- c("DateTime", "neg_MW.1", "pos_MW.1", "TZ.1","neg_MW.2", "pos_MW.2", "TZ.1", "neg_MW.3", "pos_MW.3","TZ.3", "neg_MW.4", "pos_MW.4", "TZ")
  rr <- rr[, !(names(rr) %in% c("TZ.1", "TZ.2", "TZ.3"))]

  # count unique na per rows --> if only 2 then only only one TSO has a missing value --> 2013 = always one TSO with NA // 2012 = always one TSO with NA // 2011 = 1 and once 3 TSO with NA
  unique(apply(rr, 1, function(x) sum(is.na(x))))

  # subset rr into two data.frames with negative and positive MW
  rrPos <- rr[, c("DateTime", "TZ", "pos_MW.1", "pos_MW.2", "pos_MW.3", "pos_MW.4")]
  rrNeg <- rr[, c("DateTime", "TZ", "neg_MW.1", "neg_MW.2", "neg_MW.3", "neg_MW.4")]
  # Then melt each subset into long data
  rrPos <- melt(rrPos, id.vars=c("DateTime", "TZ"))
  rrNeg <- melt(rrNeg, id.vars=c("DateTime", "TZ"))
  # Sort by Date to have chronological order and not order by TSO
  rrPos <- arrange(rrPos, DateTime)
  rrNeg <- arrange(rrNeg, DateTime)
  # Rename columns for better access
  colnames(rrPos) <- c("DateTime", "TZ", "TSO", "pos_MW")
  colnames(rrNeg) <- c("DateTime", "TZ", "TSO", "neg_MW")
  # Modify the TSO variable --> just give each TSO a number from 1 - 4
  rrPos$TSO <- substr(rrPos$TSO, nchar(as.character(rrPos$TSO)), nchar(as.character(rrPos$TSO)))
  rrNeg$TSO <- substr(rrNeg$TSO, nchar(as.character(rrNeg$TSO)), nchar(as.character(rrNeg$TSO)))

  # Build a unified data.frame
  agg <- cbind(rrNeg, pos_MW = rrPos$pos_MW)
  # sum neg_MW and pos_MW of the 4 TSO by each DateTime
  agg = aggregate(cbind(neg_MW, pos_MW) ~ DateTime, data = agg, sum, na.rm = TRUE)

  return(agg)

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
#'
#' @return a modified data.frame containing the Type ("RZBedarf"), DateTime (as POSIXct object), the MW and the direction variable (POS or NEG)
#'
#' @export
#'
preprocessOperatingReserveAuctions <- function(df.auctions) {
  library(logging)

  if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - Adding Tarif and Direction variables")
  df.auctions$Tarif <- rapply(strsplit(as.character(df.auctions$product_name), "_"), function(x) x[2])
  df.auctions$Direction <- rapply(strsplit(as.character(df.auctions$product_name), "_"), function(x) x[1])

  # Set the direction/sign of the work price --> ANBIETER_AN_NETZ signals a negative work price
  # CAUTION!!! till last week of (including) 2014-12-29 the declaration is "Anbieter an Netz" then from 2015-01-05 "ANBIETER_AN_NETZ"
  df.auctions$work_price <- ifelse(df.auctions$ap_payment_direction == "ANBIETER_AN_NETZ" | df.auctions$ap_payment_direction == "Anbieter an Netz", -df.auctions$work_price, df.auctions$work_price)


  # ap_payment_direction
  drops <- c("offers_AT", "called_power_MW", "ap_payment_direction", "product_name")

  return(df.auctions[ , !(names(df.auctions) %in% drops)])

  if(getOption("logging")) loginfo("preprocessOperatingReserveAuctions - DONE")

}



#' @title approximateCalls
#'
#' @description This recursive approach handles the special case of CrossingZero. It builds the data.frame with all needed variables to correct the operating reserve needs power for approximating the calls.
#'
#' @param df.needs - The preprocessed operating reserve needs (@seealso preprocessOperatingReserveNeeds)
#' @param df.calls - The preprocessed operating reserve calls (@seealso preprocessOperatingReserveCalls)
#'
#' @return A complete data.frame with the corrected operating reserve needs to approximate the 1min calls (avg_1min_MW).
#'
#' @examples
#' df.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
#' df.calls <- getOperatingReserveCalls('30.12.2015', '30.12.2015', '6', 'SRL')
#'
#' df.calls <- preProcessOperatingReserveCalls(df.calls)
#' df.needs <- preprocessOperatingReserveNeeds(df.needs)
#'
#' approximateCallsInRecursion(df.needs, df.calls)
#'
#'
#' @export
#'
approximateCallsInRecursion <- function(df.needs, df.calls) {
  library(logging)
  library(dplyr)

  # Calculate the 1min average operating reserve needs out of the 4sec data
  df.needs.1min <- aggregateXminAVGMW(df.needs, 1)

  if(getOption("logging")) loginfo("approximateCallsInRecursion - Cut the 15min sections")

  # Join with 15min calls
  # Cut 1min avg needs into 15min for join operation
  df.needs.1min$cuttedTime <- cut(df.needs.1min$DateTime, breaks = paste("15", "min", sep = " "))
  df.needs.1min$cuttedTime <- as.POSIXct(df.needs.1min$cuttedTime, tz = "Europe/Berlin")

  # merge the 1min needs and 15min calls based on the cuttedTime

  # Error in daylight savings --> There are two 2o'clock hours --> so the neg(pos)_MW 15min call has instead of one value, two values!!!
  # one easy possible approach is to build a function which builds group counters for each 15min call and each 225 4sec needs (60*15/4)
  # And then merge by this tag

  # Before merging, TZ needs to be set to a numeric value otherwise the order gets different in a change from CET to CEST (CEST is before CET)
  # the numeric value of 0 or 1 is based on the first observation (weather the time series starts with CEST or CET)
  # CAUTION: --> This leads to different ordering in case the time period goes over two TZ --> e.g. 1.1.2016 - 31.12.2016 --> There is CET-CEST-CET ---> odering will be CET-CET-CEST such that october is the last
  #          --> Order by date again at the end!!!
  df.needs.1min$TZ <- ifelse(df.needs.1min$TZ == df.needs.1min[1, "TZ"], 0, 1)
  df.calls$TZ <- ifelse(df.calls$TZ == df.calls[1, "TZ"], 0, 1)

  t.all = merge(df.needs.1min, df.calls, by.x=c("TZ", "cuttedTime"), by.y=c("TZ", "DateTime"))

  # reformat the data.frame for parallel computing --> DateTime and cuttedTime into numerical value!
  #t.all <- formatForApproximationAndParallelComp(t.all)


  if(getOption("logging")) loginfo("approximateCallsInRecursion - Split the 15min sections")

  #split the data.frame on the timezone and cuttedTime (2) variable into 15min sections
  # creates a list
  p <-  split(t.all, t.all$TZ)
  path <- list()
  for(i in 1:length(p)){
    path <-  append(split(p[[i]], p[[i]]$cuttedTime), path,0)
  }
  # Init the merge data.frame
  res <- data.frame()
  # Init progress bar
  if(getOption("logging")) pb <- txtProgressBar(min = 0, max = length(path), style = 3)

  # Do in parallel
  # For every 15min section do the recursion to correct the 1min avg needs
  for(i in 1:length(path)) {

    if(getOption("logging")) loginfo(paste("approximateCallsInRecursion - Start recursion for section: ", reformatDateTime(min(path[[i]]$DateTime)), " till ", reformatDateTime(max(path[[i]]$DateTime))))

    # ----------- Here starts the recursion ------------------- #
    # Do and repeat the recursion of counting NEG nad POS, correcting for homogenity, calculating the 15min NEG and POS avg of the needs, correct the needs
    correctedData <- approxRecursionWith15minChunk(path[[i]])
    res <- rbind(res, correctedData)
    # update progress bar
    if(getOption("logging")) setTxtProgressBar(pb, i)

  }


  # CLose the progress bar
  if(getOption("logging")) close(pb)

  # Reformat DateTime and cuttedTime
  res$DateTime <- reformatDateTime(res$DateTime)
  res$cuttedTime <- reformatDateTime(res$cuttedTime)

  if(getOption("logging")) loginfo("approximateCallsInRecursion - sort by dates")
  # has to be sorted again due to TZ -> 0,1 change --> if CET-CEST-CET --> the date order gets shuffled
  res <- res %>% arrange(DateTime)


  if(getOption("logging")) loginfo("approximateCallsInRecursion - DONE")

  return(res)

}



# NOT FUNCTIONAL YET!!!
parallelCompWrapperForApproximation <- function(df.needs, df.calls, numCores) {
  library(logging)
  library(dplyr)
  library(foreach)
  library(doParallel)

  # Calculate the 1min average operating reserve needs out of the 4sec data
  df.needs.1min <- aggregateXminAVGMW(df.needs, 1)

  if(getOption("logging")) loginfo("approximateCallsInRecursion - Cut the 15min sections")

  # Join with 15min calls
  # Cut 1min avg needs into 15min for join operation
  df.needs.1min$cuttedTime <- cut(df.needs.1min$DateTime, breaks = paste("15", "min", sep = " "))
  df.needs.1min$cuttedTime <- as.POSIXct(df.needs.1min$cuttedTime, tz = "MET")

  # merge the 1min needs and 15min calls based on the cuttedTime
  t.all <-  merge(df.needs.1min, df.calls, by.x = "cuttedTime", by.y = "DateTime")

  # reformat the data.frame for parallel computing --> DateTime and cuttedTime into numerical value!
  t.all <- formatForApproximationAndParallelComp(t.all)


  if(getOption("logging")) loginfo("approximateCallsInRecursion - Split the 15min sections")

  # split the data.frame on the cuttedTime variable 15min sections
  # creates a list
  path <-  split(t.all, t.all[,1])
  # Init the merge data.frame
  #res <- data.frame()


  # Do in parallel
  # For every 15min section do the recursion to correct the 1min avg needs

  cl <- makeCluster(numCores) #not to overload your computer
  registerDoParallel(cl)

  res <- foreach(i = 1:length(path),
                 .combine = c,
                 .export = c("approxRecursionWith15minChunk","getNumberOfPosOrNegIn15min","calcHomogenityCorrectness","get15minAVGs","correctionCalculationForRecursion", "get15minAVGOf1minAVG"),
                 .packages = c("dplyr","magrittr", "tidyr"),
                 .verbose=TRUE) %dopar% {

    # ----------- Here starts the recursion ------------------- #
    # Do and repeat the recursion of counting NEG nad POS, correcting for homogenity, calculating the 15min NEG and POS avg of the needs, correct the needs
    temp <- approxRecursionWith15minChunk(path[[i]])
    temp
  }

  #stop cluster
  stopCluster(cl)


  # Reformat DateTime and cuttedTime
  res$DateTime <- as.POSIXct(res$DateTime, "%Y-%m-%d %H:%M:%S", origin = "1970-01-01")
  res$cuttedTime <- as.POSIXct(res$cuttedTime, "%Y-%m-%d %H:%M:%S", origin = "1970-01-01")
  attr(res$DateTime, "tzone") <- "Europe/Berlin"
  attr(res$cuttedTime, "tzone") <- "Europe/Berlin"

  if(getOption("logging")) loginfo("approximateCallsInRecursion - DONE")

  return(res)

}


formatForApproximationAndParallelComp <- function(df) {

  df$DateTime <- as.numeric(df$DateTime)
  df$cuttedTime <- as.numeric(df$cuttedTime)
  return(df)
}

# Inut DateTime is a numeric value, target format is "%Y-%m-%d %H:%M:%S" in timezone Berlin
reformatDateTime <- function(dateTime) {
  dt <- as.POSIXct(dateTime, "%Y-%m-%d %H:%M:%S", origin = "1970-01-01")
  attr(dt, "tzone") <- "Europe/Berlin"
  dt
}


#' @title calcMarginalWorkPrices
#'
#' @description This recursive approach handles the special case of CrossingZero. It builds the data.frame with all needed variables to correct the operating reserve needs power for approximating the calls.
#'
#' @param df - The preprocessed and approximated calls (@seealso approximateCallsInRecursion)
#' @param auctions - The preprocessed operating reserve auctions (@seealso getOperatingReserveAuctions and preprocessOperatingReserveAuctions)
#' @param numCores - Specifies the number of cores used for the parallel computation
#'
#' @return A complete data.frame with the 1min approximated calls and the corresponding marginal work prices for every minute
#'
#' @examples
#' sample.auctions <- getOperatingReserveAuctions('28.12.2015', '07.01.2016', '2')
#' sample.needs <- getOperatingReserveNeeds("01.01.2016", "01.01.2016")
#' sample.calls <- getOperatingReserveCalls('01.01.2016', '01.01.2016', '6', 'SRL')
#'
#' s.c <- preProcessOperatingReserveCalls(sample.calls)
#' s.n <- preprocessOperatingReserveNeeds(sample.needs)
#' s.a <- preprocessOperatingReserveAuctions(sample.auctions)
#'
#' df.approx.calls <- approximateCallsInRecursion(s.n, s.c)
#'
#' df <- getMarginalWorkPrices(df.approx.calls, s.a)
#'
#'
#' @export
#'
calcMarginalWorkPrices <- function(approximated.calls, auctions, numCores) {
  library(foreach)
  library(doParallel)
  library(logging)

  if(getOption("logging")) loginfo("parallelCompWrapperForMarginalWorkPrices")
  # Add the Tarif to the calls
  approximated.calls <- addTarif(approximated.calls)
  # Add Direction NEG or POS to the calls
  approximated.calls <- addDirection(approximated.calls)

  # Format the input data.frames --> parallel package only supports numerical variables
  # POSIXct (DateTime), Date (date_from, date_to) and characters (Tarif and Direction) must be converted
  c <- formatApproxCallsForParallelComp(approximated.calls)
  a <- formatAuctionsForParallelComp(auctions)

  # setup parallel backend to use many processors
  #cores = detectCores()
  #cl <- makeCluster(cores[1]-2) #not to overload your computer
  cl <- makeCluster(numCores) #not to overload your computer
  registerDoParallel(cl)

  mwp <- foreach(i = 1:nrow(c), .combine = rbind, .export = c("matchAuctionsWithCalls"), .packages = c("dplyr","magrittr"), .verbose=FALSE) %dopar% {

    temp <- matchAuctionsWithCalls(a, c[i,])
    temp
  }

  #stop cluster
  stopCluster(cl)
  colnames(mwp) <- c("marginal_work_price", "num_orders", "total_num_orders", "fill_quote")

  df <- cbind(approximated.calls, mwp)

  if(getOption("logging")) loginfo("parallelCompWrapperForMarginalWorkPrices - DONE")


  return(df)
}


# Outsourcing the match function. It is used for the parallelWrapper function to calculate the marginal work price
matchAuctionsWithCalls <- function(auction.results, callObj){
  library(dplyr)
  library(magrittr)

  ss <- auction.results %>%
    filter(date_from <= callObj$DateTime & callObj$DateTime <= date_to & callObj$Tarif == Tarif & callObj$Direction == Direction) %>%
    arrange(work_price) %>%
    mutate(cumsum = cumsum(offered_power_MW))

  totalOrders <- nrow(ss) # number of orders which get filled
  # Get the next higher offer otherwise there will be -Inf for 1min calls less than 5MW (since 5MW is the smallest possible offer)
  index <- nrow(filter(ss, cumsum <= abs(callObj$avg_1min_MW))) # + 1 ^= number of orders which get filled
  m <- ss[min(index + 1, nrow(ss)), ]$work_price

  r <- data.frame(m, min(index + 1, nrow(ss)), totalOrders, min(index + 1, nrow(ss))/totalOrders)

  return(r)
}

# returns the minimal input data.frame which the getMarginalWorkPrice function needs
# converts everything to numeric! important for parallel computation
# DateTime origin:1970-01-01
# Tarif: HT = 1, NT = -1
# Direction: NEG = -1, POS = 1
formatApproxCallsForParallelComp <- function(df) {

  # Add the Tarif to the calls
  df <- addTarif(df)
  # Add Direction NEG or POS to the calls
  df <- addDirection(df)
  df <- df[, (names(df) %in% c("DateTime", "avg_1min_MW", "Tarif", "Direction"))]
  df$DateTime <- as.numeric(df$DateTime)
  df$Tarif <- ifelse(df$Tarif == "HT", 1, -1)
  df$Direction <- ifelse(df$Direction == "NEG", -1, 1)

  return(df)

}

# Convert date_from and date_to variable in the auctions data.frame into a POISXct datetime object with hour minute and seconds and then to mumeric value
formatAuctionsForParallelComp <- function(auctions) {
  # always 00:00:00
  auctions$date_from <- as.numeric(as.POSIXct(paste(auctions$date_from, "00:00:00", sep=" "), "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"))
  # always 23:59:00
  auctions$date_to <- as.numeric(as.POSIXct(paste(auctions$date_to, "23:59:00", sep=" "), "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin"))
  auctions$Tarif <- ifelse(auctions$Tarif == "HT", 1, -1)
  auctions$Direction <- ifelse(auctions$Direction == "NEG", -1, 1)

  return(auctions)

}




#' @title getCallProbDataSetOnConditions
#'
#' @description This functions calculates for each price within a given price range the call probability conditioned by specified columns of the input data.frame (e.g. Tarif and Direction). Hereby, the Direction variable is mandatory to correctly calculate the call probability.
#' The conditioned variables have to be calculated before and must be factors or at least factorizable.
#'
#' @param data - the data.frame with the calculated marginal workprices from the approximated 1min calls (@seealso getMarginalWorkPrices)
#' @param numCores - set the number of cores to be used for parallel computation
#' @param price.seq.start - specifies the start price of a price range for which call probabilities should be calculated
#' @param price.seq.end - specifies the end price of a price range for which call probabilities should be calculated
#' @param conditionByColumns - an array of columns/variables of the input data.frame. They condition the porbability calculation. Therefore they have to be factors or at least factorizable. IMPORTANT: !! The variable Direction has to be in it, since for probability calculation the total number (numerator) depends on POS and NEG power (It makes sense to keep Tarif and Direction which define the product type)
#' @param granularity - a factor which specifies the steps of the price range. E.g. a factor of 0.1 leads to decimal incrementions (0, 0.1, 0.2, ..., 1.5, 1.6, 1.7...)
#'
#' @return an array with all the call probabilities of the specified timeperiod and price range.
#'
#' @examples
#' needs <- getOperatingReserveNeeds('01.01.2016', '10.01.2016')
#' calls <- getOperatingReserveCalls('01.01.2016', '10.01.2016', '6', 'SRL')
#' auctions <- getOperatingReserveAuctions('28.12.2015', '10.01.2016', '2')
#'
#' mwork.parallel <- getMarginalWorkPrices(needs,calls,auctions,2)
#' call.probs <- getCallProbDataSet(mwork.parallel, 1, 0, 775, "2016-01-01 00:00:00", "2016-01-01 23:59:59", c("NT", "POS"))
#'
#' # Plot the call probabilities. Therefore create an array with the price range
#' library(ggplot2)
#' price.range <- seq(0, ceiling(max.mwork))
#' qplot(price.range, call.probs, geom="line")
#'
#'
#' @export
#'
getCallProbDataSetOnConditions <- function(data, numCores, price.seq.start, price.seq.end, granularity, conditionByColumns) {
  library(logging)
  library(data.table)
  library(foreach)
  library(doParallel)

  cl <- makeCluster(numCores) #not to overload your computer
  registerDoParallel(cl)

  if(getOption("logging")) loginfo("getCallProbDataSetOnConditions - Start foreach loop")


  end <- (ceiling(price.seq.end) - price.seq.start) * 1/granularity
  # Calculate for each price within the given price range the call probability specified on a time period and product type (Tarif and Direction)
  df <- foreach(i = 0:end,
                .combine = function(x,y) rbindlist(list(x,y), use.names = TRUE, fill = TRUE),
                .export = c("getCallProbForMarginalWorkPrice"),
                .packages = c("dplyr"),
                .verbose=FALSE) %dopar% {

                  temp <- getCallProbForMarginalWorkPrice(data, (price.seq.start + i*granularity), conditionByColumns)
                  temp
                }

  #stop cluster
  stopCluster(cl)

  if(getOption("logging")) loginfo("getCallProbDataSetOnConditions - DONE")

  # convert data.table object back to data.frame
  df <- setDF(df)
  # set the NA values generated by the data.table::rbindlist method --> rbindlist has to be used, otherwise missing combinations get lost
  df[is.na(df)] <- 0

  return(df)
}


#' This is a helper method needed in the @seealso getCallProbDataSet function
#' It retuns the call probability for a given price (mwp) based on the conditions
getCallProbForMarginalWorkPrice <- function(data, mwp, conditionByColumns) {
  library(dplyr)


  # Get the number of work pries which are less than the given price and based on the conditioned subset
  rs.price <- data %>%
    filter(marginal_work_price >= mwp) %>%
    group_by_(.dots = conditionByColumns) %>%
    summarise(n = n())

  # Get the total amount of observations based on the conditions (filter/subset) BUT for both NEG and POS directions
  rs.total2 <- data %>%
    group_by_(.dots = conditionByColumns[-which(conditionByColumns %in% "Direction")]) %>%
    summarise(n = n())

  # join the total numbers and the numbers of the whole condition together in the final result array res2
  rs2 <- left_join(rs.price, rs.total2, by = conditionByColumns[-which(conditionByColumns %in% "Direction")], suffix = c(".price",".total"))

  # Handle Special case: If price is too high and not all directions and tarifs combinations are fullfiled --> set counts to zero and prob (not Inf)
  # Also fill the Direction with the last standing --> ?!?!? What if two and only one missing?
  # rs2[is.na(rs2$n.total), "n.total"] <- 0
  # rs2[is.na(rs2$Direction), "Direction"] <- rs2[!is.na(rs2$Direction), "Direction"]

  # add the price to which the call probability belongs to the result data.frame
  rs2$Price <- mwp
  # calculate the call probability
  rs2$Prob <- ifelse(round(rs2$n.price/rs2$n.total, digits = 4) == Inf, 0, round(rs2$n.price/rs2$n.total, digits = 4))

  # Now build for every combination of the conditions a joint new variable (e.g. from Tarif,Direction and DateClass --> HT_POS_Workday variable/column)
  # Direction varable is a factor --> convert it to a character for further operations
  rs2$Direction <- as.character(rs2$Direction)

  for(i in 1:nrow(rs2)) {
    # concatenate the new Variable based on the combinations of the conditions
    coln <- paste("Prob_", paste(rs2[i,conditionByColumns], collapse = "_"), sep = "")
    # Add the new variable/column to the resulat data.frame with the corresponding probability
    rs2[[coln]] <- rs2[i,]$Prob
  }

  # reformat the data.frame such that only the newly columns and one price row will be returned
  rs2 <- unique(rs2[, !(names(rs2) %in% c(conditionByColumns, "n.price","n.total","Prob") )])

  return(rs2)
}



















