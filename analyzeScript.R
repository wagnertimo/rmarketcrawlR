
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

auctions.2016 <- getReserveAuctions('01.01.2016', '31.12.2016', '2')
calls.2016 <- getReserveCalls('01.01.2016', '31.12.2016', '6', 'SRL')
needs.2016 <- getReserveNeeds('01.01.2016', '31.12.2016')

auctions <- getReserveAuctions('24.10.2016', '30.10.2016', '2')
calls <- getReserveCalls('30.10.2016', '30.10.2016', '6', 'SRL')
needs <- getReserveNeeds('30.10.2016', '30.10.2016')



approx <- getOneMinuteCalls(needs,calls)



# sample the 2016 data
start <- 1  # start observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)
end <- 672   # end observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)

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


start.time <- Sys.time()
m <- getMarginalWorkPrices(needs,calls,auctions,2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

mwork <- getMarginalWorkPrices(needs,calls,auctions)

# on old laptop 10 days (sample of 01.01.2016 - 10.01.2016) took 13 minutes --> 365/10*13/60 = 8h
# Looks more like one obs one sec --> 365*24*60/60/60 = 146h = 6d

system.time(getMarginalWorkPrices(needs,calls,auctions,2))



# start.time <- Sys.time()
# mwork <- getMarginalWorkPrices(needs,calls,auctions)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken



#' Building a getAllData() function --> it returns a list of yearly based list elements with calls.YYYY, needs.YYYY, auctions.YYYY
#' from 2011-06-27 till 2017-03-31
#'
#' E.g.:
#'
#' list:
#'
#' [[2016]]
#'         [1] calls.2016
#'         [2] needs.2016
#'         [3] auctions.2016
#' [[2015]]
#'         [1] calls.2015
#'         [2] needs.2015
#'         [3] auctions.2015
#'
#' (...)
#'
#' Only for SRL and Netzreglverbund
#'
getAllData <- function(start = "2011-06-27", end = "2017-03-31") {

  start <- format(as.Date(start), "%d.%m.%Y")
  end <- format(as.Date(end), "%d.%m.%Y")

  allData <- list()

  # build the yearly time periods
  year <-
  auctions <- getReserveAuctions(start, paste('31.12.', year, sep=""), '2')
  calls <- getReserveCalls(start, paste('31.12.', year, sep=""), '6', 'SRL')
  needs <- getReserveNeeds(start, paste('31.12.', year, sep=""))


  auctions <- getReserveAuctions('01.01.2015', '31.12.2015', '2')
  calls <- getReserveCalls('01.01.2015', '31.12.2015', '6', 'SRL')
  needs <- getReserveNeeds('01.01.2015', '31.12.2015')

}



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


# -----------------------------------------------------------------------------------------------------
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


# ------------------------------------------------------------------------------------------------------
# TODO: calls seem to have some NA (or - ) for the variable df.calls$BETR..NEG. That's why everything is NA
#       ---> filter for the NA and find a solution how to handle this
#
#       DONE --> needed to format the BETR..NEG and BETR..POS variable from factor to numeric with converting the german number style
#




# -----------------------------------------------------------------------------------------------------
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

# --------------------------------
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


# ------------------------------------------------------------------------------------------
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


#'------------------------------------------------------------------------------------------------------
#'
#' Testing: Calculation of call probability for each possible marginal work price in data set (get min (for negative work prices) and max in data.frame)
#'
#'------------------------------------------------------------------------------------------------------


# There are probs. for NEG_NT, NEG_HT and POS_NT, POS_HT
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
# granularity factor
tr <- getCallProbDataSetOnConditions(m, 1, 0, 100, 0.1, conditionByColumns = c("Tarif", "Direction"))


# seq 0 - 755
# granularity e.g. 0.1 --> 0, 0.1, 0.2, ... 1.0, 1.1, ..., 754.8, 754.8, 755.0 --> n = (755 - 0)*1/granularity + 1




# Plot multiple variables (value) against one target variable (key). The target has to be omitted for the values (2:...)
library(ggplot2)

tt <- tr
plot <- tt %>%
  # Binds rowwise. Every following column gets bind under the last row. The key variabel (here Price) gets repeated
  gather(key, value, 2:ncol(tt)) %>%
  ggplot(aes(x=Price, y=value, colour=key)) +
  geom_line()

plot

m <- filter(mwork.parallel, DateTime >= "2016-01-01 01:00:00" & DateTime < "2016-01-01 15:00:00")
m <- filter(mwork.parallel, DateTime >= "2016-01-05 15:00:00" & DateTime < "2016-01-05 22:00:00")

qplot(m$DateTime,m$marginal_work_price, geom = "line")
qplot(m$DateTime,m$approx_1min_call, geom = "line")

ggplot(m, aes(DateTime)) +
  geom_line(aes(y = marginal_work_price, colour = "marginal work price")) +
  geom_line(aes(y = approx_1min_call, colour = "1min call"))




# Contour Plot with plotly

#'
#' Carpet-Plot „Grenzarbeitspreis“:
#'       X=Tag
#'       Y=Minute des Tages
#'       Grenzarbeitspreis als Farbe (günstigster=grün; teuerster=rot)
#'       --> Clusterung von ähnlichen Abrufen basierend auf Grenzarbeitspreis
#'
#'
#' Idee Graph:
#'        Arbeitspreis fest (z.B. 100 EUR), x=Tag, y=Abrufwahrscheinlichkeit
#'
#' als Carpet-Plot „Abrufwahrscheinlichkeit“:
#'         X = Tag (Minute?)
#'         Y = Abrufwahrscheinlichkeit
#'         Arbeitspreis als Farbe (günstigster=grün; teuerster= rot)
#'         --> Clusterung von Arbeitspreisen mit ähnlicher Abrufwahrscheinlichkeit
#'
#' http://www.netzfrequenzmessung.de/IEWT_Netzfrequenz_Stabilitaet_Paper.pdf, Abb. 11
#'


# Checkout max min of work price of whole data set
range(m$marginal_work_price)

m.mwp.min = min(m$marginal_work_price)
m.mwp.max = max(m$marginal_work_price)


plotMWPTimeSeries(mwp.2016, 20, "POS", 1440)

m3 <- filter(mwp.2016, format(DateTime, "%Y-%m") <= format(as.Date("2016-03-01"), "%Y-%m"))

#'   PLOTLY --> CONTOUR PLOT
week <- filter(mwp.2016, as.Date(format(DateTime, "%Y-%m-%d")) >= as.Date("2016-06-01") & as.Date(format(DateTime, "%Y-%m-%d")) <= as.Date("2016-06-07"))


plotContourCallProb(week, 30, 80, "POS", 1, 60, 2)

pp <- plotContourHourOfDay(mwp.2016, "POS", 60, "contour")
pp

r <- getHighestPriceWithHighestCallProb(m, 0, 80, "POS", 1, 60, 2)

#'
#' TODO Plot hourly total MW power of minutely calls
#'
week$cuttedTime <- cut(week$DateTime, breaks = paste("60", "min", sep = " "))
week$cuttedTime <- as.POSIXct(week$cuttedTime, tz = "Europe/Berlin")
# Get the number of work pries which are less than the given price and based on the conditioned subset
library(dplyr)
library(tidyr)

d <- week %>%
  group_by_(.dots = c("cuttedTime", "Direction")) %>%
  summarise(totalMW = sum(approx_1min_call)) %>%
  spread(Direction, totalMW) %>%
  ggplot(aes(cuttedTime, POS)) +
        geom_line(colour = "blue") +
        labs(x = "Time", y = "Positive Secondary Reserve Calls in MW", title = paste("Hourly Positive Secondary Reserve Calls in MW")) +
        scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))

d



