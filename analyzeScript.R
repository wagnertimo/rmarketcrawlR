
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
#' Testing the production script file: minScript.R

auctions.2016 <- getReserveAuctions('28.12.2015', '01.01.2017', '2')
calls.2016 <- getReserveCalls('01.01.2016', '31.12.2016', '6', 'SRL')
needs.2016 <- getReserveNeeds('01.01.2016', '31.12.2016')
needs.2 <- getReserveNeeds('01.01.2016', '03.03.2016')

# sample the 2016 data
start <- 1  # start observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)
end <- 960   # end observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)

needs <- needs.2016[(((start - 1)*225) + 1):(end*225),]
calls <- calls.2016[start:end,]
auctions <- auctions.2016



approx.calls <- getOneMinuteCalls(needs,calls)

setLogging(FALSE)
setLogging(TRUE)


# Start 18:05 -> stuck at Cut time -> 18:09 -> calc avg, cut 15min, split 15min sections ... went for run
#mwork <- getMarginalWorkPrices(needs.2016,calls.2016,auctions.2016)

# 10 days (sample of 01.01.2016 - 10.01.2016) parallel for margin calc. with 2 cores --> exe time:  135sec = 2,25min --> ca. 90min
mwork.parallel <- getMarginalWorkPrices(needs,calls,auctions,2)

mwork <- getMarginalWorkPrices(needs,calls,auctions)

# on old laptop 10 days (sample of 01.01.2016 - 10.01.2016) took 13 minutes --> 365/10*13/60 = 8h
# Looks more like one obs one sec --> 365*24*60/60/60 = 146h = 6d
system.time(getMarginalWorkPrices(needs,calls,auctions,2))

# start.time <- Sys.time()
# mwork <- getMarginalWorkPrices(needs,calls,auctions)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

#
# !!! CAUTION THere are -Inf marginal work prices e.g. at 2016-01-01 02:21:00
#




#'------------------------------------------------------------------------------------------------------
#'
#' TODO: IMPROVE THE CRAWLING FUNCTION. SOMETIMES ERROR OCCURS.
#'       ---> Error occurs for operating reserve needs (e.g. on 01.2017, 11.2016 and 04.2016) due to the unziped file name
#'       ---> the date code (e.g. 201701) is missing in the file name so there is just "SRL_Bedarf.csv"
#'
#'       ---> CHANGE CRAWLER FUNCTION: download file, unzip it, change unziped file name to dateCode and then read in csv
#'
#'       DONE
#'
#'------------------------------------------------------------------------------------------------------



# TODO: calls seem to have some NA (or - ) for the variable df.calls$BETR..NEG. That's why everything is NA
#       ---> filter for the NA and find a solution how to handle this
#
#       DONE --> needed to format the BETR..NEG and BETR..POS variable from factor to numeric with converting the german number style
#



#'------------------------------------------------------------------------------------------------------
#'
#' Prepare a sample for 2015
#'
#'------------------------------------------------------------------------------------------------------

df.needs.2015 <- getOperatingReserveNeeds('01.01.2015', '31.12.2015')
df.calls.2015 <- getOperatingReserveCalls('01.01.2015', '31.12.2015', '6', 'SRL')
# Weekly auctions always from monday till sunday!!
df.auctions.2015 <- getOperatingReserveAuctions('29.12.2014', '03.01.2016', '2')



#'------------------------------------------------------------------------------------------------------
#'
#' Prepare a sample for 2016
#'
#'------------------------------------------------------------------------------------------------------

# Crawl the raw data for calls, auctions, needs
df.needs.2016 <- getOperatingReserveNeeds('01.01.2016', '31.12.2016')
df.calls.2016 <- getOperatingReserveCalls('01.01.2016', '31.12.2016', '6', 'SRL')

# TODO: Be aware of the sign for work price!!
df.auctions.2016 <- getOperatingReserveAuctions('28.12.2015', '01.01.2017', '2')

# Preprocess the raw data for calls, auctions, needs
df.prep.calls.2016 <- preProcessOperatingReserveCalls(df.calls.2016)
df.prep.auctions.2016 <- preprocessOperatingReserveAuctions(df.auctions.2016)
df.prep.needs.2016 <- preprocessOperatingReserveNeeds(df.needs.2016)



#'------------------------------------------------------------------------------------------------------
#'
#' Start Operating Reserve Call Approximation
#'
#'------------------------------------------------------------------------------------------------------

# Approximate the calls WITH RECURSION
df.aprx.calls.2016 <- approximateCallsInRecursion(df.prep.needs.2016, df.prep.calls.2016)


#'------------------------------------------------------------------------------------------------------
#'
#' Start Calculating Marginal Work Price
#'
#'------------------------------------------------------------------------------------------------------

mwork <- getMarginalWorkPrices(df.aprx.calls.2016, df.prep.auctions.2016)






#'------------------------------------------------------------------------------------------------------
#'
#' Two special cases:
#' 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative) calls (in both direction)
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
#' Recursive approach:
#' Crossers keep their new value and new counts of NEG and POS and new calculations of avg_15min_MW_NEG and avg_15min_MW_POS are calculated
#' Recursion stops when those two averages match the neg_MW and pos_MW (rounding and/or computational errors)
#'
#' test sample:
#' look in code snippet below (DETECT CORSSINGZERO CASES) to identify the crossers and set a sample
#' e.g. start = end 61 --> negative CZ 2 times
#'


# -------------
# DONE
# -------------


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




# --------------------------------------------------------------------------------
# Sample

s.n <- df.prep.needs.2016
s.c <- df.prep.calls.2016
s.a <- df.prep.auctions.2016

# Choose a sample.
# Look in s.c, the 15min calls of the day (e.g. 01.01.2016) and get the observation number
start <- 1  # start observation number of 15min calls (--> 49*15/60 gives the hour of the day)
end <- 96   # end observation number of 15min calls (--> 49*15/60 gives the hour of the day)

# The calculation chooses automatically the corresponding 4sec needs data to the chosen calls observation.
df.needs <- s.n[(((start - 1)*225) + 1):(end*225),]
df.calls <- s.c[start:end,]


# --------------------------------------------------------------------------------
# Approximate Old --> Only homogenity as a special cases is handled. NO ZEROCROSSING till now

# Approximate the calls
r <- approximateCalls(df.needs, df.calls)

# Sanity checks
# 15min averages ==? 15min calls
sum(r[r$Corrected >= 0 & r$DateTime < "2016-01-01 15:15:00", ]$Corrected) / 15
# Plot the correction data to compare its result
plotCorrectedNeeds(r)


# --------------------------------------------------------------------------------
# Approximate New --> At this time all cases are handled (Homogenity and CrossingZero)

# Approximate the calls WITH RECURSION
t <- approximateCallsInRecursion(df.needs, df.calls)

# Sanity checks
# 15min averages ==? 15min calls
sum(t[t$avg_1min_MW < 0 & t$DateTime < "2016-01-01 15:15:00", ]$avg_1min_MW) / 15
sum(t[t$avg_1min_MW < 0 & t$DateTime >= "2016-01-01 15:15:00", ]$avg_1min_MW) / 15
# This plot does not use the variable Corrected since it is omitted in the recursion
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

sample.app.calls <- t[1:60,]

mwork <- getMarginalWorkPrices(sample.app.calls, s.a)




#'------------------------------------------------------------------------------------------------------
#'
#' Testing: Calculation of call probability for each possible marginal work price in data set (get min (for negative work prices) and max in data.frame)
#'
#'------------------------------------------------------------------------------------------------------


# There are probs. for NEG_NT, NEG_HT and POS_NT, POS_HT
# ---> subset on Tarif and Direction variables


# Get min and max values of the marginal work prices. This sets the boundaries for the probabilities
min.mwork <- min(mwork$marginalWorkPrice) # e.g. 32
max.mwork <- max(mwork$marginalWorkPrice) # e.g. 774.6


# Beginning at a value below the min will lead to 100% call probability !! Be aware of different tarifs

# get the probability vector of the min max sequence
tr <- getCallProbDataSet(mwork, min.mwork, max.mwork)
# save the min max sequence in a vector to plot it against the corresponding probabilities
tr2 <- seq(min.mwork, max.mwork)

library(ggplot2)
qplot(tr2,tr, geom="line")




getCallProbForMarginalWorkPrice <- function(data, mwor) {

  library(dplyr)

  prob <- round(nrow(filter(data, marginalWorkPrice >= mwor))/nrow(data), digits = 2)


  return(prob)
}


getCallProbDataSet <- function(data, min, max) {

  df <- c()
  for(i in seq(min, max)) {
    df[i-min+1] <- getCallProbForMarginalWorkPrice(data, i)
  }

  return(df)
}







