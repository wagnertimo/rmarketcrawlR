
#'
#' Testing script
#'


#'------------------------------------------------------------------------------------------------------

getTheMeanPowerPrice <- function(df, productType, dateFrom) {

  # filter out the austrian offers and get the specific date as well as product type
  df_temp <- filter(df, product_name == productType & date_from == dateFrom & is.na(offers_AT))

  # Sum up the product of power price and called power of all orders in ratio to the overall called power
  return(sum(df_temp$power_price*df_temp$called_power_MW) / sum(df_temp$called_power_MW))

}

getTheMeanPowerPrice(df, 'NEG_NT', '20.03.2017')

#'------------------------------------------------------------------------------------------------------



#'------------------------------------------------------------------------------------------------------
#'
#' TODO: IMPROVE THE CRAWLING FUNCTION. SOMETIMES ERROR OCCURS. SEEMS TO BE A FILE READ/WRITE PROBLEM
#'       ---> MAYBE ANOTHER APPROACH TO NOT WRITE AND READ A TEMP FILE WOULD BE A SOLUTION
#'
#'------------------------------------------------------------------------------------------------------

# Get the operating reserve needs and calls as well as the weekly auctions
# Raw data from regelleistung.net and transnetbw.de (4sec needs)
df.main.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
df.main.calls <- getOperatingReserveCalls('30.12.2015', '30.12.2015', '6', 'SRL')
df.main.auctions <- getOperatingReserveAuctions('23.12.2015', '30.12.2015', '2')

# Preprocess and format the calls and need data
df.main.calls.preprocessed <- preProcessOperatingReserveCalls(df.main.calls)
df.main.needs.preprocessed <- preprocessOperatingReserveNeeds(df.main.needs)



#'------------------------------------------------------------------------------------------------------
#'
#' Two special cases:
#' 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative ) calls (in both direction)
#' 2. CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
#'
#'------------------------------------------------------------------------------------------------------

#'
#' 1. special case Homogenity
#' --> example for negative homogentity (and positive call) "01.01.2016 08:00:00" - "01.01.2016 08:15:00", sample.corrected[481:496,]
#'                                                          "01.01.2016 00:00:00" - "01.01.2016 00:15:00", sample.corrected[1:15,]
#'
#' --> example for positive homogentity (and negative call) "2016-01-01 00:30:00" - "2016-01-01 00:45:00", sample.corrected[30:45,]
#'


# -------------
# DONE
# -------------


#'
#' 2. special case: CrossingZero
#' Occurs e.g. 2016-01-01 12:15:00 -2016-01-01 12:30:00 --> 50th obs of the 15min calls of 01.01.2016
#' --> neg homogenity but second smallest absolute value crosses zero with correction
#'
#' * One rule could be to set all crossing values to 0 --> correction value gets higher because the difference has now to be distributed between less data points --> and this could lead to more and more recursive crossing values
#' * after initial correction recursively adjust the corrections till the 15min average values of the corrected needs and the calls are identical
#'
#'


sample.needs <- getOperatingReserveNeeds("01.01.2016", "01.01.2016")
sample.calls <- getOperatingReserveCalls('01.01.2016', '01.01.2016', '6', 'SRL')

s.c <- preProcessOperatingReserveCalls(sample.calls)
s.n <- preprocessOperatingReserveNeeds(sample.needs)

# Choose a sample. Look in s.c, the 15min calls of the day (e.g. 01.01.2016) and get the observation number
start <- 49 # start observation number of 15min calls (--> 49*15/60 gives the hour of the day)
end <- 50   # end observation number of 15min calls (--> 49*15/60 gives the hour of the day)

# The calculation chooses automatically the corresponding 4sec needs data to the chosen calls observation.
df.needs <- s.n[(((start - 1)*225) + 1):(end*225),]
df.calls <- s.c[start:end,]

# Approximate the calls --> At this time only homogenity as special cases handled. NO ZEROCROSSING till now
r <- approximateCalls(df.needs, df.calls)

# Plot the correction data to compare its result
plotCorrectedNeeds(r)
# Sanity check 15min averages ==? 15min calls
sum(r[r$Corrected < 0 & r$DateTime < "2016-01-01 12:30:00", ]$Corrected) / 15


# Build a function to check if the new, corrected 15min average of the needs is equal to the 15min call
# This function should also count the new number of POS and NEG
# It adds 4 new variables new_POS, new_NEG and 1 or 0 for avg_NEG_eq and avg_POS_eq
# If all other calculations are correct it can only be that the avgs are unequal in a CrossingZero case.
# So normally only new_POS and new_NEG needed, if they are unequal to POS and NEG we have a CrossingZero case

# Analzye the occurence of Case 2 within a longer time period

# Calculate the corrected needs or approximate the calls. Based on the standard case
# r


# DETECT CORSSINGZERO CASES

# Count for every 15min section the number of crossings
r <- countCrossingZeros(r)
# mark negative and positive crossings
r <- markCrossingZero(r)

# subset all negative and positive crossings (crossings of smallest absolute values in a homogen 15min section are not included)
# list all crossings --> datetime (1min), avg_1min_MW, Corrected, neg_CZ, pos_CZ
library(dplyr)

crossings <- select(filter(r, neg_CZ == 1 | pos_CZ == 1), DateTime, avg_1min_MW, Corrected, neg_CZ, pos_CZ, n_neg_CZ, n_pos_CZ)









