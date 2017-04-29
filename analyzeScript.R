
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

# Old laptop with 2 cores execution time for 10 days (sample of 01.01.2016 - 10.01.2016)
# --> first part: 1min call approximation => ca. 6mins --> for 365 days => ca. 4h
# --> second part: marginal work price computation (parallel comp.) => ca. 1,5min for 365 days => ca. 1h
# estimated total time for 2016 => ca. 5-6h

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


#'------------------------------------------------------------------------------------------------------
#
# !!! CAUTION Infinity loop on 2016-10-30 --> daylight savings
#
#       DONE --> but also check the dst flag of POSIXct object maybe this also works --> http://stackoverflow.com/questions/13156836/character-posixct-conversion-in-r-causes-wrong-timezone-values-on-daylight-savin?noredirect=1&lq=1
#
#   ---> BUT NEW ERROR SEEN --> pos_MW in calls do not format correctly they miss the point as delimiter where a comma was
#
#   ----> DONE
#
#'------------------------------------------------------------------------------------------------------
setLogging(TRUE)

ne <- getReserveNeeds("27.03.2016","27.03.2016")
ne2 <- getReserveNeeds("29.10.2016","29.10.2016")
ne3 <- getReserveNeeds("30.10.2016","30.10.2016")

ca <- getReserveCalls("27.03.2016","27.03.2016", '6', 'SRL')
ca2 <- getReserveCalls("29.10.2016","29.10.2016", '6', 'SRL')
ca3 <- getReserveCalls("30.10.2016","30.10.2016", '6', 'SRL')

a <- getReserveAuctions("24.10.2016","30.10.2016", 2)
a2 <- getReserveAuctions("21.03.2016","27.03.2016", 2)

e <- getOneMinuteCalls(ne, ca)
e3 <- getOneMinuteCalls(ne3, ca3)

m <- getMarginalWorkPrices(ne,ca,a2,2)
m3 <- getMarginalWorkPrices(ne3,ca3,a,2)
m2 <- getMarginalWorkPrices(ne2,ca2,a,2)


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
#df.aprx.calls.2016 <- approximateCallsInRecursion(df.prep.needs.2016, df.prep.calls.2016)


#'------------------------------------------------------------------------------------------------------
#'
#' Start Calculating Marginal Work Price
#'
#'------------------------------------------------------------------------------------------------------

#mwork <- getMarginalWorkPrices(df.aprx.calls.2016, df.prep.auctions.2016)






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

# --------------------
# DONE
# --------------------

#
# !!! CAUTION THere are -Inf marginal work prices e.g. at 2016-01-01 02:21:00
#





#'------------------------------------------------------------------------------------------------------
#'
#' Testing: Calculation of call probability for each possible marginal work price in data set (get min (for negative work prices) and max in data.frame)
#'
#'------------------------------------------------------------------------------------------------------


# There are probs. for NEG_NT, NEG_HT and POS_NT, POS_HT
# ---> subset on Tarif and Direction variables


# Get min and max values of the marginal work prices. This sets the boundaries for the probabilities
min.mwork <- min(mwork.parallel$marginal_work_price) # e.g. 32
max.mwork <- max(mwork.parallel$marginal_work_price) # e.g. 774.6, 5999.97

# Beginning at a value below the min will lead to 100% call probability !! Be aware of different tarifs

library(dplyr)
library(lubridate)

mwork.parallel$DateClass <- ifelse(isWeekendOrHoliday(mwork.parallel$DateTime) == TRUE, "Weekend", "Workday")



isWeekendOrHoliday <- function(dateTime) {
  library(lubridate)
  library(rmarketcrawlR)

  # week days 2 == monday ... 1 == sunday
  return(ifelse(wday(dateTime) == 7 | wday(dateTime) == 1 | isGermanHoliday(dateTime) == TRUE, TRUE, FALSE))
}


# Optimize getCallProbDataSet() function --> user can choose in array c() which columns of input data.frame should be used for the
# conditioned calculation of the call probabilities. IMPORTANT: the columns/variables should be factors or at least factorizable!!!!!!
# E.g: conditionen by Tarif, Direction and Weekend columns
#       --> output will be: columns for each combination (--> NEG_POS_Weekend or HT_NEG_Workday) and rows are the respective call probability with the price
#


# get the probability vector of the min max sequence
tr <- getCallProbDataSetOnConditions(mwork.parallel, 1, 0, 775, conditionByColumns = c("Tarif", "Direction", "DateClass"))

# save the min max sequence in a vector to plot it against the corresponding probabilities
tr2 <- seq(0, 775)

library(ggplot2)

qplot(tr2,tr, geom="line")

# Plot multiple variables (value) against one target variable (key). The target has to be omitted for the values (2:...)
tt <- tr
z <- tt %>%
  # Binds rowwise. Every following column gets bind under the last row. The key variabel (here Price) gets repeated
  gather(key, value, 2:ncol(tt)) %>%
  ggplot(aes(x=Price, y=value, colour=key)) +
  geom_line()




data <- mwork.parallel
mwp <- 775
conditionByColumns <-  c("Tarif", "Direction", "DateClass")

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
# add the price to which the call probability belongs to the result data.frame
rs2$Price <- mwp
# calculate the call probability
rs2$Prob <- round(rs2$n.price/rs2$n.total, digits = 4)

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
















