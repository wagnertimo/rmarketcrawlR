
#'
#' Testing script to analyze errors and build new functions
#'


#'------------------------------------------------------------------------------------------------------

#' This function is not yet needed
#'

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

sample.needs <- getOperatingReserveNeeds("01.01.2016", "01.01.2016")
sample.calls <- getOperatingReserveCalls('01.01.2016', '01.01.2016', '6', 'SRL')
sample.auctions <- getOperatingReserveAuctions('28.12.2015', '07.01.2016', '2')


s.c <- preProcessOperatingReserveCalls(sample.calls)
s.n <- preprocessOperatingReserveNeeds(sample.needs)
s.a <- preprocessOperatingReserveAuctions(sample.auctions)



#'------------------------------------------------------------------------------------------------------
#'
#' Two special cases:
#' 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative ) calls (in both direction)
#' 2. CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
#'
#'------------------------------------------------------------------------------------------------------

#'
#' 1. special case: Homogenity
#'
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
#'
#' Occurs e.g. 2016-01-01 12:15:00 -2016-01-01 12:30:00 --> 50th obs of the 15min calls of 01.01.2016
#' --> neg homogenity but second smallest absolute value crosses zero with correction
#'
#' * One rule could be to set all crossing values to 0 --> correction value gets higher because the difference has now to be distributed between less data points --> and this could lead to more and more recursive crossing values
#' * after initial correction recursively adjust the corrections till the 15min average values of the corrected needs and the calls are identical
#'
#' Choose the recursive approach:
#' Crossers keep their new value and new counts of NEG and POS and new calculations of avg_15min_MW_NEG and avg_15min_MW_POS are calculated
#' Recursion stops when those two averages match the neg_MW and pos_MW (rounding and/or computational errors) or the new calculated NEG and POS counts are the same as the previous ones
#'
#' test sample:
#' look in code snippet below (DETECT CORSSINGZERO CASES) to identify the crossers and set a sample
#' e.g. start = end 61 --> negative CZ 2 times

# -------------
# DONE
# -------------


# --------------------------------------------------------------------------------
# Sample

# Choose a sample. Look in s.c, the 15min calls of the day (e.g. 01.01.2016) and get the observation number
start <- 1 # start observation number of 15min calls (--> 49*15/60 gives the hour of the day)
end <- 90   # end observation number of 15min calls (--> 49*15/60 gives the hour of the day)

# The calculation chooses automatically the corresponding 4sec needs data to the chosen calls observation.
df.needs <- s.n[(((start - 1)*225) + 1):(end*225),]
df.calls <- s.c[start:end,]


# --------------------------------------------------------------------------------
# Approximate Old

# Approximate the calls --> At this time only homogenity as special cases handled. NO ZEROCROSSING till now
r <- approximateCalls(df.needs, df.calls)


# Sanity checks
# 15min averages ==? 15min calls
sum(r[r$Corrected >= 0 & r$DateTime < "2016-01-01 15:15:00", ]$Corrected) / 15
# Plot the correction data to compare its result
plotCorrectedNeeds(r)


# --------------------------------------------------------------------------------
# Approximate New

# Approximate the calls WITH RECURSION --> At this time all cases are handled (Homogenity and CrossingZero)
t <- approximateCallsInRecursion(df.needs, df.calls)


# Sanity checks
# 15min averages ==? 15min calls
sum(t[t$avg_1min_MW < 0 & t$DateTime < "2016-01-01 15:15:00", ]$avg_1min_MW) / 15
sum(t[t$avg_1min_MW < 0 & t$DateTime >= "2016-01-01 15:15:00", ]$avg_1min_MW) / 15

# This plot does not use Corrected since this variable is omitted in the recursion
plotApproximatedData(t)

# Here a merge with the old approximation function
joint <- merge(r[,!(names(r) %in% c("Corrected"))], select(t, DateTime,Corrected = avg_1min_MW))
plotCorrectedNeeds(joint)
plotCorrectedNeeds(r)



#'------------------------------------------------------------------------------------------------------
#'
#' Testing marginal work price
#'
#'------------------------------------------------------------------------------------------------------

#' marginal work price == the highest bidded work price to which an order (depending on needed MW) gets filled
#'
#' For SRL there are weekly bids. So those bids last only for that weekly timeframe.
#'
#' The matching of the approx. 1min calls with the auctions depends on:
#'      - the timeframe:           the DateTime of the 1min approx call has to fit into the timeframe of the auctions date_from and date_to
#'      - the product:             the 1min approx. call's Tarif has to fit the auction's one --> HT_NEG, NT_NEG, HT_POS, NT_POS
#'      - the corresponding power: the neg. or pos. power needed (minus or plus (matches with NEG or POS Direction of the auction) avg_1min_MW)
#'            - ordered auctions on work price
#'            - summing all auctions till needed power is reached
#'            - last auction's work price == marginal work price for that minute
#'
#'
#' !!! QUESTION !!! --> if needed power is e.g. 1200,783 MW (roundings because of avg 1min calculation)
#'                      --> and last cumulated bid is of power 1200
#'                      --> will the next bid/order get filled with the rest portion of 0,783 or do we ceil the avg_1min_MW????


# Test matching for first 15min sample

sample.app.calls <- t[1:15,]
#s.a

mwork <- getMarginalWorkPrice(sample.app.calls, s.a)






#'-------------------------------------------------------------------------------------------
# DETECT CORSSINGZERO CASES

# Count for every 15min section the number of crossings
# r has to be a data.frame in the type after calling the old approximate Call function with the Corrected colum and Homogenity case
r <- countCrossingZeros(r)
# mark negative and positive crossings
r <- markCrossingZero(r)

# subset all negative and positive crossings (crossings of smallest absolute values in a homogen 15min section are not included)
# list all crossings --> datetime (1min), avg_1min_MW, Corrected, neg_CZ, pos_CZ
library(dplyr)

# subset all negative and positive crossings
crossings.all <- select(filter(r, neg_CZ == 1 | pos_CZ == 1), DateTime, avg_1min_MW, Corrected, neg_CZ, pos_CZ, n_neg_CZ, n_pos_CZ)
# subset only negative CZ
crossings.neg <- select(filter(r, neg_CZ == 1), DateTime, avg_1min_MW, Corrected, neg_CZ, n_neg_CZ)
# subset only positive CZ
crossings.pos <- select(filter(r, pos_CZ == 1), DateTime, avg_1min_MW, Corrected, pos_CZ, n_pos_CZ)

# set the element (observation) of the data.frame which has to be analyzed
d <- crossings.neg[3,]

# Plot a 15min section of the filtered data.frame which contains a CrossingZero element (1min observation)
plotCorrectedNeeds(filter(r, cuttedTime == get15minSection(d$DateTime)))


# This helper method returns a 15min section DateTime object (POSIXct object) to the corresponding input DateTime object (POISXct object)
get15minSection <- function(dateTime){
  library(lubridate)

  m <- lubridate::minute(dateTime)
  minute(dateTime) <- m - (m %% 15)


  return(dateTime)

}

#'----------------------------------------------------------------------------------------------------------

















